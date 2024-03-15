use crate::ast::constant::BuiltinConst;
use crate::ast::{
    AstNode, BinaryExpression, CommentAstNode, Constant, ConstantExpression, EndStatement,
    Expression, FunctionCallExpression, FunctionImplementation, GosubStatement, GotoStatement,
    IdentifierExpression, IfStatement, LabelStatement, LetStatement, ParameterSpecifier,
    ParensExpression, PredefinedCallStatement, PredefinedFunctionCallExpression,
    ProcedureCallStatement, ProcedureImplementation, Program, ReturnStatement, Statement,
    UnaryExpression, UnaryOp, VariableDeclarationStatement, VariableSpecifier, VariableType,
    WhileStatement,
};
use crate::executable::{read_file, EntryType, Executable, VariableEntry, VariableNameGenerator};
use crate::tables::{
    FuncOpCode, OpCode, BIN_EXPR, FUNCTION_DEFINITIONS, STATEMENT_DEFINITIONS,
    STATEMENT_SIGNATURE_TABLE, TYPE_NAMES,
};
use std::collections::{HashMap, HashSet};
use std::intrinsics::transmute;
use std::path::PathBuf;
use std::vec;

pub mod reconstruct;
pub mod rename_visitor;

const LAST_STMT: i32 = 0x00e2;

struct FuncL {
    func: i32,
}

#[must_use]
pub fn load_file(file_name: &str) -> Program {
    let mut prg = Program::new();
    prg.file_name = PathBuf::from(file_name);
    let mut d = Decompiler::new(read_file(file_name));
    d.do_pass1();
    d.generate_variable_declarations(&mut prg);
    d.do_pass2(&mut prg, false);

    prg.nodes
        .extend(d.statements.iter().map(|s| AstNode::Statement(s.clone())));

    prg
}

pub struct Decompiler {
    pub executable: Executable,
    cur_stmt: i32,
    akt_proc: i32,

    pass: i32,
    next_label: i32,
    next_func: i32,
    func_flag: i32,
    proc_flag: i32,
    src_ptr: i32,
    symbol: i32,
    valid_buffer: Vec<bool>,
    exp_count: i32,

    pub warnings: Vec<String>,
    pub errors: Vec<String>,
    pub statements: Vec<Statement>,
    label_used: HashMap<i32, i32>,
    func_used: HashMap<i32, FuncL>,
    label_stack: Vec<i32>,
    expr_stack: Vec<Expression>,
}

impl Decompiler {
    pub fn new(executable: Executable) -> Self {
        let mut valid_buffer = Vec::new();
        valid_buffer.resize(executable.code_size as usize / 2, false);
        Decompiler {
            executable,
            cur_stmt: 0,
            akt_proc: 0,
            pass: 0,
            next_label: 0,
            next_func: 0,
            func_flag: 0,
            proc_flag: 0,
            src_ptr: 0,
            symbol: 0,
            valid_buffer,
            exp_count: 0,
            warnings: Vec::new(),
            errors: Vec::new(),
            label_used: HashMap::new(),
            func_used: HashMap::new(),
            label_stack: Vec::new(),
            expr_stack: Vec::new(),
            statements: Vec::new(),
        }
    }

    pub fn output_stmt(&mut self, prg: &mut Program, stmt: Statement) {
        match prg.nodes.last_mut() {
            Some(crate::ast::AstNode::Function(decl)) => {
                if self.func_flag > 0 && decl.id == self.func_flag {
                    decl.get_statements_mut().push(stmt);
                    return;
                }
            }
            Some(crate::ast::AstNode::Procedure(decl)) => {
                if self.proc_flag > 0 && decl.id == self.proc_flag {
                    decl.get_statements_mut().push(stmt);
                    return;
                }
            }
            _ => {}
        }
        self.statements.push(stmt);
    }

    fn fill_valid(&mut self, i: i32, j: i32) {
        for n in i..j {
            self.valid_buffer[n as usize] = true;
        }
    }

    /// Returns the do pass1 of this [`Decompiler`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn do_pass1(&mut self) {
        let mut last_point = 0;
        let mut if_ptr = -5;

        self.cur_stmt = 0;
        self.next_label = 0;
        self.next_func = 0;
        self.src_ptr = 0;
        /*
        for (i, s) in self.executable.source_buffer.iter().enumerate() {
            println!("{}: {}", i, s);
        }*/

        while self.src_ptr != -1 && self.src_ptr <= self.executable.code_size / 2 {
            if self.src_ptr >= self.executable.code_size / 2
                || self.valid_buffer[self.src_ptr as usize]
            {
                self.fill_valid(last_point, self.src_ptr);
            } else {
                let prev_stat = self.cur_stmt;
                if self.src_ptr == if_ptr {
                    self.src_ptr += 2;
                }
                self.cur_stmt = self.executable.source_buffer[self.src_ptr as usize];
                assert!(
                    self.cur_stmt < LAST_STMT
                        && (self.cur_stmt as usize) < STATEMENT_SIGNATURE_TABLE.len()
                        && STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] != 0xaa,
                    "Error: Unknown statement {} at {}.",
                    self.cur_stmt,
                    self.src_ptr
                );
                assert!(
                    self.cur_stmt >= 0 && self.cur_stmt < LAST_STMT,
                    "opCode needs to be 0..226 at {} was {}",
                    self.src_ptr,
                    self.cur_stmt
                );

                let mut trap = false;
                match STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] {
                    0xf6 => unsafe {
                        self.src_ptr += 2;
                        self.akt_proc =
                            self.executable.source_buffer[(self.src_ptr - 1) as usize] - 1;
                        self.executable
                            .variable_declarations
                            .get_mut(&self.akt_proc)
                            .unwrap()
                            .report_variable_usage();
                        let act_dec_start = self
                            .executable
                            .variable_declarations
                            .get(&self.akt_proc)
                            .unwrap()
                            .variable
                            .data
                            .function_value
                            .start_offset as i32;
                        let act_dec_args = self
                            .executable
                            .variable_declarations
                            .get(&self.akt_proc)
                            .unwrap()
                            .variable
                            .data
                            .function_value
                            .parameters;

                        self.funcin(act_dec_start, self.akt_proc);
                        self.pushlabel(act_dec_start);
                        trap = !self.parse_expr(act_dec_args as i32, 0);
                    },
                    0xf7 => {
                        if self.parse_expr(0x01, 0) {
                            self.executable
                                .variable_declarations
                                .get_mut(
                                    &(self.executable.source_buffer[self.src_ptr as usize] - 1),
                                )
                                .unwrap()
                                .report_variable_usage();
                            trap = !self.parse_expr(0x01, 0);
                        } else {
                            trap = true;
                        }
                    }
                    0xf8 => {
                        if self.parse_expr(0x03, 0) {
                            self.executable
                                .variable_declarations
                                .get_mut(
                                    &(self.executable.source_buffer[self.src_ptr as usize] - 1),
                                )
                                .unwrap()
                                .report_variable_usage();
                            self.src_ptr += 1;
                        } else {
                            trap = true;
                        }
                    }
                    0xf9 => {
                        self.executable
                            .variable_declarations
                            .get_mut(
                                &(self.executable.source_buffer[self.src_ptr as usize + 1] - 1),
                            )
                            .unwrap()
                            .report_variable_usage();
                        self.executable
                            .variable_declarations
                            .get_mut(
                                &(self.executable.source_buffer[self.src_ptr as usize + 2] - 1),
                            )
                            .unwrap()
                            .report_variable_usage();
                        self.src_ptr += 3;
                    }
                    0xfe => {
                        self.src_ptr += 1;
                        trap = !self
                            .parse_expr(self.executable.source_buffer[self.src_ptr as usize], 0);
                    }
                    0xfa => {
                        self.executable
                            .variable_declarations
                            .get_mut(
                                &(self.executable.source_buffer[self.src_ptr as usize + 2] - 1),
                            )
                            .unwrap()
                            .report_variable_usage();
                        self.src_ptr += 2;
                        trap = !self.parse_expr(
                            self.executable.source_buffer[self.src_ptr as usize - 1] - 1,
                            0,
                        );
                    }
                    0xfc => {
                        self.src_ptr += 1;
                        trap = !self.parse_expr(
                            self.executable.source_buffer[self.src_ptr as usize] | 0xf00,
                            0,
                        );
                    }
                    0xfd => {
                        self.src_ptr += 1;
                        self.labelin(self.executable.source_buffer[self.src_ptr as usize]);
                        self.src_ptr += 1;
                    }
                    0xff => {
                        if_ptr = self.src_ptr;
                        if self.parse_expr(0x01, 0) {
                            if_ptr = self.set_if_ptr(if_ptr);
                            self.src_ptr += 1;
                        } else {
                            trap = true;
                        }
                    }
                    _ => {
                        trap = !self.parse_expr(
                            (STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0xf0) * 16
                                + (STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0x0f),
                            0,
                        );
                    }
                }
                if !trap {
                    match self.cur_stmt {
                        0x0001 | // END
                        0x002a | // RETURN
                        0x00a9 | // ENDPROC
                        0x00ab | // ENDFUNC
                        0x003a => {
                            self.fill_valid(last_point, self.src_ptr); // STOP
                            last_point = self.src_ptr;
                            if prev_stat == 0x000b {
                                continue;
                            }
                        }
                        0x0029 | 0x000b // GOSUB & IF 
                        => {
                            self.fill_valid(last_point, self.src_ptr);
                            last_point = self.src_ptr;
                            self.pushlabel(self.executable.source_buffer[self.src_ptr as usize - 1]);
                            continue;
                        }
                        0x0007 => {
                            self.fill_valid(last_point, self.src_ptr); // GOTO
                            last_point = self.src_ptr;
                            self.pushlabel(self.executable.source_buffer[self.src_ptr as usize - 1]);
                            if prev_stat == 0x000b {
                                continue;
                            }
                        }
                        _ => {
                            continue;
                        }
                    }
                }
            }
            self.src_ptr = self.poplabel() / 2;
            last_point = self.src_ptr;
        }
        self.pass += 1;
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn generate_variable_declarations(&mut self, prg: &mut Program) {
        self.name_variables();

        if self.symbol == 0 {
            /*                self.output_stmt(prg, Statement::Comment("Source Code:".to_string()));
            self.output_stmt(prg, Statement::Comment("--------------------------------".to_string()));*/
        }

        let mut c_vars = 0;
        let mut c_func = 0;
        let mut c_proc = 0;

        let mut statements = vec![];
        for i in 0..self.executable.max_var {
            let Some(cur_var) = self.executable.variable_declarations.get_mut(&i) else {
                continue;
            };

            if cur_var.get_type() == EntryType::Variable {
                match cur_var.header.variable_type {
                    VariableType::Function => {
                        c_func += 1;
                        cur_var.number = c_func;
                    }
                    VariableType::Procedure => {
                        c_proc += 1;
                        cur_var.number = c_proc;
                    }
                    _ => {
                        c_vars += 1;
                        cur_var.number = c_vars;
                        if self.symbol == 0 {
                            let var_type = TYPE_NAMES[cur_var.header.variable_type as usize];

                            let dims = match cur_var.header.dim {
                                1 => vec![cur_var.header.vector_size],
                                2 => {
                                    vec![cur_var.header.vector_size, cur_var.header.matrix_size]
                                }
                                3 => vec![
                                    cur_var.header.vector_size,
                                    cur_var.header.matrix_size,
                                    cur_var.header.cube_size,
                                ],
                                _ => vec![],
                            };
                            let var_spec = VariableSpecifier::empty(
                                unicase::Ascii::new(cur_var.get_name().clone()),
                                dims,
                            );
                            statements.push(VariableDeclarationStatement::create_empty_statement(
                                var_type,
                                vec![var_spec],
                            ));
                        }
                    }
                }
            }
        }

        for stmt in statements {
            self.output_stmt(prg, stmt);
        }

        if self.symbol == 0 {
            // self.output_stmt(prg, Statement::Comment("--------------------------------".to_string()));
        }
    }

    fn output_func(&mut self, prg: &mut Program, func: i32) {
        let func_name = self
            .executable
            .variable_declarations
            .get(&func)
            .unwrap()
            .get_name()
            .clone();
        let mut func_parameters = vec![];
        unsafe {
            let mut j = self
                .executable
                .variable_declarations
                .get(&func)
                .unwrap()
                .variable
                .data
                .function_value
                .first_var_id as i32;
            for _ in 0..self
                .executable
                .variable_declarations
                .get(&func)
                .unwrap()
                .variable
                .data
                .function_value
                .parameters
            {
                let var_name = format!(
                    "LOC{0:>03}",
                    self.executable
                        .variable_declarations
                        .get(&j)
                        .unwrap()
                        .number
                );

                let var_type = TYPE_NAMES[self
                    .executable
                    .variable_declarations
                    .get(&j)
                    .unwrap()
                    .header
                    .variable_type as usize];
                self.executable
                    .variable_declarations
                    .get_mut(&j)
                    .unwrap()
                    .set_name(var_name.clone());
                func_parameters.push(ParameterSpecifier::empty(
                    false,
                    var_type,
                    VariableSpecifier::empty(unicase::Ascii::new(var_name), vec![]),
                ));
                self.executable
                    .variable_declarations
                    .get_mut(&j)
                    .unwrap()
                    .function_id = func;
                j += 1;
            }
            let func_type = TYPE_NAMES[self
                .executable
                .variable_declarations
                .get(&j)
                .unwrap()
                .header
                .variable_type as usize];
            prg.nodes.push(crate::ast::AstNode::Function(
                FunctionImplementation::empty(
                    func,
                    func_name,
                    func_parameters,
                    func_type,
                    Vec::new(),
                ),
            ));
        }
    }

    fn output_proc(&mut self, prg: &mut Program, proc: i32) {
        let proc_name = self
            .executable
            .variable_declarations
            .get(&proc)
            .unwrap()
            .get_name()
            .clone();
        unsafe {
            let param_flags = self
                .executable
                .variable_declarations
                .get(&proc)
                .unwrap()
                .variable
                .data
                .procedure_value
                .pass_flags;

            let mut proc_parameters = vec![];
            let mut j = self
                .executable
                .variable_declarations
                .get(&proc)
                .unwrap()
                .variable
                .data
                .procedure_value
                .first_var_id as i32;
            for n in 0..self
                .executable
                .variable_declarations
                .get(&proc)
                .unwrap()
                .variable
                .data
                .procedure_value
                .parameters
            {
                let var_name = format!(
                    "LOC{0:>03}",
                    self.executable
                        .variable_declarations
                        .get(&j)
                        .unwrap()
                        .number
                );
                let var_type = TYPE_NAMES[self
                    .executable
                    .variable_declarations
                    .get(&j)
                    .unwrap()
                    .header
                    .variable_type as usize];
                self.executable
                    .variable_declarations
                    .get_mut(&j)
                    .unwrap()
                    .set_name(var_name.clone());
                let is_var = (1 << n) & param_flags != 0;

                proc_parameters.push(ParameterSpecifier::empty(
                    is_var,
                    var_type,
                    VariableSpecifier::empty(unicase::Ascii::new(var_name), vec![]),
                ));
                self.executable
                    .variable_declarations
                    .get_mut(&j)
                    .unwrap()
                    .function_id = proc;
                j += 1;
            }
            prg.nodes.push(crate::ast::AstNode::Procedure(
                ProcedureImplementation::empty(proc, proc_name, proc_parameters, Vec::new()),
            ));
        }
    }

    fn funcin(&mut self, label: i32, func: i32) {
        self.func_used.insert(label, FuncL { func });
    }

    fn funcout(&mut self, prg: &mut Program, label: i32) {
        if let Some(funcl) = self.func_used.get(&label) {
            let func = funcl.func;
            self.func_flag = 0;
            self.proc_flag = 0;
            if self
                .executable
                .variable_declarations
                .get(&func)
                .unwrap()
                .header
                .variable_type
                == VariableType::Function
            {
                self.output_func(prg, func);
                self.func_flag = func;
            } else {
                self.output_proc(prg, func);
                self.proc_flag = func;
            }
            self.dump_locs(prg, func);
        }
    }

    fn get_function_mut(prg: &mut Program, func: i32) -> Option<&mut FunctionImplementation> {
        for f in &mut prg.nodes {
            if let crate::ast::AstNode::Function(decl) = f {
                if decl.id == func {
                    return Some(decl);
                }
            }
        }
        None
    }

    fn get_procedure_mut(prg: &mut Program, func: i32) -> Option<&mut ProcedureImplementation> {
        for f in &mut prg.nodes {
            if let crate::ast::AstNode::Procedure(decl) = f {
                if decl.id == func {
                    return Some(decl);
                }
            }
        }
        None
    }

    fn dump_locs(&mut self, prg: &mut Program, func: i32) {
        let mut i;
        let mx_var;
        let mut j = 0;
        unsafe {
            // StackPtr = 0;
            let cur_var = self.executable.variable_declarations.get(&func).unwrap();
            if cur_var.header.variable_type == VariableType::Function {
                i = cur_var.variable.data.function_value.return_var as i32;
                mx_var = cur_var.variable.data.function_value.return_var as i32
                    + cur_var.variable.data.function_value.local_variables as i32;
            } else {
                i = cur_var.variable.data.procedure_value.first_var_id as i32
                    + cur_var.variable.data.procedure_value.parameters as i32;
                mx_var = cur_var.variable.data.procedure_value.first_var_id as i32
                    + cur_var.variable.data.procedure_value.local_variables as i32;
                //+1;
            }

            while i < mx_var {
                if self
                    .executable
                    .variable_declarations
                    .get(&i)
                    .unwrap()
                    .get_type()
                    == EntryType::Variable
                {
                    self.executable
                        .variable_declarations
                        .get_mut(&i)
                        .unwrap()
                        .function_id = func;
                    if self.symbol == 0 {
                        let var_type = crate::tables::TYPE_NAMES[self
                            .executable
                            .variable_declarations
                            .get(&i)
                            .unwrap()
                            .header
                            .variable_type
                            as usize];
                        let var_name = format!(
                            "LOC{0:>03}",
                            self.executable
                                .variable_declarations
                                .get(&i)
                                .unwrap()
                                .number
                        );
                        self.executable
                            .variable_declarations
                            .get_mut(&i)
                            .unwrap()
                            .set_name(var_name.clone());
                        let cur_var = &self.executable.variable_declarations[&i];

                        if let Some(func) = Decompiler::get_function_mut(prg, func) {
                            let dims = match cur_var.header.dim {
                                1 => vec![cur_var.header.vector_size],
                                2 => vec![cur_var.header.vector_size, cur_var.header.matrix_size],
                                3 => vec![
                                    cur_var.header.vector_size,
                                    cur_var.header.matrix_size,
                                    cur_var.header.cube_size,
                                ],
                                _ => vec![],
                            };
                            func.get_statements_mut().push(
                                VariableDeclarationStatement::create_empty_statement(
                                    var_type,
                                    vec![VariableSpecifier::empty(
                                        unicase::Ascii::new(var_name),
                                        dims,
                                    )],
                                ),
                            );
                        } else if let Some(proc) = Decompiler::get_procedure_mut(prg, func) {
                            let dims = match cur_var.header.dim {
                                1 => vec![cur_var.header.vector_size],
                                2 => vec![cur_var.header.vector_size, cur_var.header.matrix_size],
                                3 => vec![
                                    cur_var.header.vector_size,
                                    cur_var.header.matrix_size,
                                    cur_var.header.cube_size,
                                ],
                                _ => vec![],
                            };
                            proc.get_statements_mut().push(
                                VariableDeclarationStatement::create_empty_statement(
                                    var_type,
                                    vec![VariableSpecifier::empty(
                                        unicase::Ascii::new(var_name),
                                        dims,
                                    )],
                                ),
                            );
                        } else {
                            panic!("function or procedure {func} not found");
                        }
                    }
                }
                i += 1;
                j += 1;
            }
            if j > 0 {
                // self.output.push_str("\n");
            }
        }
    }

    fn labelin(&mut self, label: i32) {
        if self.label_used.contains_key(&label) {
            return;
        }
        self.label_used
            .insert(label, self.label_used.len() as i32 + 1);
    }

    fn labelnr(&mut self, label: i32) -> &i32 {
        match self.label_used.get(&label) {
            Some(x) => x,
            _ => &-1,
        }
    }

    fn labelout(&mut self, prg: &mut Program, label: i32) {
        if let Some(x) = self.label_used.get(&label) {
            let label_stmt = LabelStatement::create_empty_statement(unicase::Ascii::new(format!(
                "LABEL{x:>03}"
            )));
            self.output_stmt(prg, label_stmt);
        }
    }

    fn pushlabel(&mut self, label: i32) {
        self.label_stack.push(label);
    }

    fn poplabel(&mut self) -> i32 {
        if self.label_stack.is_empty() {
            -2
        } else {
            self.label_stack.pop().unwrap()
        }
    }

    fn push_expr(&mut self, str: Expression) {
        self.exp_count += 1;
        self.expr_stack.push(str);
    }

    fn pop_expr(&mut self) -> Option<Expression> {
        if self.expr_stack.is_empty() {
            None
        } else {
            self.exp_count -= 1;
            Some(self.expr_stack.pop().unwrap())
        }
    }

    fn stripper(str: Expression) -> Expression {
        match str {
            Expression::Parens(expr) => Decompiler::stripper(expr.get_expression().clone()),
            Expression::Binary(expr) => BinaryExpression::create_empty_expression(
                expr.get_op(),
                Box::new(Decompiler::stripper(expr.get_left_expression().clone())),
                Box::new(Decompiler::stripper(expr.get_right_expression().clone())),
            ),
            _ => str,
        }
    }

    fn popstrip(&mut self) -> Option<Expression> {
        let expr = self.pop_expr();
        expr.map(Decompiler::stripper)
    }
    fn repl_const(expr: Expression, names: &'static [BuiltinConst]) -> Expression {
        match expr {
            Expression::Const(c) => match c.get_constant_value() {
                Constant::Integer(parse_result) => {
                    let mut i = 0;
                    while i < names.len() && names[i].value != *parse_result {
                        i += 1;
                    }
                    if i < names.len() {
                        return ConstantExpression::create_empty_expression(Constant::Builtin(
                            &names[i],
                        ));
                    }
                    ConstantExpression::create_empty_expression(Constant::Integer(*parse_result))
                }
                _ => Expression::Const(c),
            },
            Expression::Identifier(s) => Expression::Identifier(s),
            Expression::Parens(e) => ParensExpression::create_empty_expression(Box::new(
                Self::repl_const(e.get_expression().clone(), names),
            )),
            Expression::FunctionCall(call_expr) => {
                let mut p2 = vec![];
                for e in call_expr.get_arguments() {
                    p2.push(Self::repl_const(e.clone(), names));
                }

                FunctionCallExpression::create_empty_expression(
                    call_expr.get_identifier().clone(),
                    p2,
                )
            }
            Expression::PredefinedFunctionCall(call_expr) => {
                let mut p2 = vec![];
                for e in call_expr.get_arguments() {
                    p2.push(Self::repl_const(e.clone(), names));
                }

                PredefinedFunctionCallExpression::create_empty_expression(call_expr.get_func(), p2)
            }
            Expression::Unary(expr) => UnaryExpression::create_empty_expression(
                expr.get_op(),
                Box::new(Self::repl_const(expr.get_expression().clone(), names)),
            ),
            Expression::Binary(expr) => BinaryExpression::create_empty_expression(
                expr.get_op(),
                Box::new(Self::repl_const(expr.get_left_expression().clone(), names)),
                Box::new(Self::repl_const(expr.get_right_expression().clone(), names)),
            ),
        }
    }

    fn const_name(&mut self, names: &'static [BuiltinConst]) -> Expression {
        let temp_exr = self.popstrip().unwrap();
        Self::repl_const(temp_exr, names)
    }

    fn trans_exp(&mut self, cur_expr: i32) -> Expression {
        match self.cur_stmt {
            0x00c if cur_expr != 2 => self.popstrip().unwrap(),
            0x00d if cur_expr != 2 => self.popstrip().unwrap(),
            0x00c | 0x00d => self.const_name(&crate::tables::CONSTANT_CONFERENCE_NAMES),

            0x00e if cur_expr != 2 => self.popstrip().unwrap(),
            0x00e => self.const_name(&crate::tables::CONSTANT_NAMES_DISPLAY),

            0x010 => {
                if cur_expr == 3 {
                    self.const_name(&crate::tables::CONSTANT_FACCESS_NAMES)
                } else if cur_expr == 4 {
                    self.const_name(&crate::tables::CONSTANT_OPENFLAGS_NAMES)
                } else {
                    self.popstrip().unwrap()
                }
            }

            0x011 if cur_expr == 3 => self.const_name(&crate::tables::CONSTANT_FACCESS_NAMES),
            0x011 if cur_expr == 4 => self.const_name(&crate::tables::CONSTANT_OPENFLAGS_NAMES),
            // 0x011 => self.popstrip().unwrap(),
            0x012 if cur_expr == 3 => self.const_name(&crate::tables::CONSTANT_FACCESS_NAMES),
            0x012 if cur_expr == 4 => self.const_name(&crate::tables::CONSTANT_OPENFLAGS_NAMES),
            // 0x012 => self.popstrip().unwrap(),
            0x018 => self.const_name(&crate::tables::CONSTANT_LINECOUNT_NAMES),
            0x022 => {
                if cur_expr == 6 {
                    self.const_name(&crate::tables::CONSTANT_1_NAMES)
                } else {
                    self.popstrip().unwrap()
                }
            }
            0x02b => {
                if cur_expr == 5 {
                    self.const_name(&crate::tables::CONSTANT_1_NAMES)
                } else {
                    self.popstrip().unwrap()
                }
            }

            0x039 if cur_expr != 2 => self.popstrip().unwrap(),
            0x039 => self.const_name(&crate::tables::CONSTANT_1_NAMES),

            0x070 if cur_expr != 3 => self.popstrip().unwrap(),
            0x070 => self.const_name(&crate::tables::CONSTANT_SEEK_NAMES),

            0x0cd | 0x0ce if cur_expr != 1 => self.popstrip().unwrap(),
            0x0cd | 0x0ce => self.const_name(&crate::tables::CONSTANT_2_NAMES),

            _ => self.popstrip().unwrap(),
        }
    }

    fn varout(&mut self, var_number: i32) -> Expression {
        let var_nr = var_number - 1;

        let cur_var = self.executable.variable_declarations.get(&var_nr).unwrap();
        if cur_var.get_type().use_name() {
            match cur_var.header.variable_type {
                VariableType::Function | VariableType::Procedure => {
                    return FunctionCallExpression::create_empty_expression(
                        unicase::Ascii::new(self.get_variable_name(cur_var)),
                        vec![],
                    );
                }
                _ => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        self.get_variable_name(cur_var),
                    ));
                }
            }
        }
        unsafe {
            match cur_var.header.variable_type {
                VariableType::Boolean => ConstantExpression::create_empty_expression(
                    Constant::Boolean(cur_var.variable.data.bool_value),
                ),
                VariableType::Integer
                | VariableType::Unsigned
                | VariableType::Byte
                | VariableType::SByte
                | VariableType::Word
                | VariableType::SWord
                | VariableType::Date
                | VariableType::EDate
                | VariableType::DDate
                | VariableType::Money
                | VariableType::Time => ConstantExpression::create_empty_expression(
                    Constant::Integer(cur_var.variable.data.int_value),
                ),
                VariableType::BigStr | VariableType::String => {
                    ConstantExpression::create_empty_expression(Constant::String(
                        cur_var.variable.as_string(),
                    ))
                }
                VariableType::Double => ConstantExpression::create_empty_expression(
                    Constant::Double(cur_var.variable.data.double_value),
                ),
                VariableType::Float => ConstantExpression::create_empty_expression(
                    Constant::Double(cur_var.variable.data.float_value as f64),
                ),
                _ => {
                    log::warn!(
                        "unknown variable type {} at {}",
                        cur_var.header.variable_type,
                        self.src_ptr
                    );
                    ConstantExpression::create_empty_expression(Constant::Integer(
                        cur_var.variable.data.int_value,
                    ))
                }
            }
        }
    }

    fn fnktout(&mut self, func: i32) -> i32 {
        let offset = -func as usize;
        if offset >= FUNCTION_DEFINITIONS.len() {
            log::error!("unknown built in function {} at {}", func, self.src_ptr);
            return -1;
        }

        let func_def = &FUNCTION_DEFINITIONS[(-func) as usize];

        match func_def.args {
            0x10 => {
                if self.exp_count < 1 {
                    return -1;
                }
                let tmp = self.pop_expr().unwrap();
                match func_def.opcode {
                    FuncOpCode::NOT => {
                        self.push_expr(UnaryExpression::create_empty_expression(
                            UnaryOp::Not,
                            Box::new(tmp),
                        ));
                    }
                    FuncOpCode::UMINUS => {
                        self.push_expr(UnaryExpression::create_empty_expression(
                            UnaryOp::Minus,
                            Box::new(tmp),
                        ));
                    }
                    FuncOpCode::UPLUS => {
                        self.push_expr(UnaryExpression::create_empty_expression(
                            UnaryOp::Plus,
                            Box::new(tmp),
                        ));
                    }
                    _ => {
                        self.errors
                            .push(format!("unknown unary function {func} at {}", self.src_ptr));
                        self.push_expr(UnaryExpression::create_empty_expression(
                            UnaryOp::Plus,
                            Box::new(tmp),
                        ));
                    }
                }
                return 0;
            }
            0x11 => {
                if self.exp_count < 2 {
                    if let Some(l_value) = self.pop_expr() {
                        self.warnings.push(format!(
                            "too few argument for built in function {} should be 2 was {} at {} expression on stack: '{}'",
                            func_def.name, self.exp_count, self.src_ptr, l_value
                        ));

                        self.push_expr(l_value);
                    } else {
                        self.warnings.push(format!(
                            "too few argument for built in function {} should be 2 was {} at {} (empty stack)",
                            func_def.name, self.exp_count, self.src_ptr
                        ));
                    }
                    return -1;
                }
                let r_value = self.pop_expr().unwrap();
                let l_value = self.pop_expr().unwrap();

                let binop = BIN_EXPR[offset];

                self.push_expr(ParensExpression::create_empty_expression(Box::new(
                    BinaryExpression::create_empty_expression(
                        binop,
                        Box::new(l_value),
                        Box::new(r_value),
                    ),
                )));

                return 0;
            }
            _ => {}
        }

        if self.exp_count < i32::from(func_def.args) {
            log::error!(
                "too few argument for built in function {} should be {} was {} at {}",
                func_def.name,
                func_def.args,
                self.exp_count,
                self.src_ptr
            );
            return -1;
        }
        let mut parameters = Vec::new();

        for _ in 0..func_def.args {
            parameters.push(self.pop_expr().unwrap());
        }
        parameters.reverse();
        self.push_expr(PredefinedFunctionCallExpression::create_empty_expression(
            func_def, parameters,
        ));
        0
    }

    fn dimexpr(&mut self, dims: i32) -> i32 {
        let mut cur_dim = 0;
        let temp_expr = self.exp_count;

        loop {
            if cur_dim == dims {
                break;
            }
            cur_dim += 1;

            self.exp_count = 0;
            let mut tmp_func = 0;
            self.src_ptr += 1;

            while (self.src_ptr as usize) < self.executable.source_buffer.len()
                && self.executable.source_buffer[self.src_ptr as usize] != 0
            {
                unsafe {
                    let curvar = self.executable.source_buffer[self.src_ptr as usize];
                    if curvar >= 0 && curvar <= self.executable.max_var {
                        let var_idx = &(curvar - 1);
                        if self
                            .executable
                            .variable_declarations
                            .get(var_idx)
                            .unwrap()
                            .header
                            .variable_type
                            == VariableType::Function
                        {
                            self.pushlabel(
                                self.executable
                                    .variable_declarations
                                    .get(var_idx)
                                    .unwrap()
                                    .variable
                                    .data
                                    .function_value
                                    .start_offset as i32,
                            );
                            self.executable
                                .variable_declarations
                                .get_mut(
                                    &(self.executable.source_buffer[self.src_ptr as usize - 1]),
                                )
                                .unwrap()
                                .report_variable_usage();
                            self.funcin(
                                self.executable
                                    .variable_declarations
                                    .get(var_idx)
                                    .unwrap()
                                    .variable
                                    .data
                                    .function_value
                                    .start_offset as i32,
                                *var_idx,
                            );
                            if self.pass == 1 {
                                let temp_str2 = self.pop_expr().unwrap();
                                let tmp = self.pop_expr();
                                if let Some(e) = tmp {
                                    let new_expr = match e {
                                        Expression::FunctionCall(expr) => {
                                            let mut v = expr.get_arguments().clone();
                                            v.push(temp_str2);
                                            FunctionCallExpression::create_empty_expression(
                                                expr.get_identifier().clone(),
                                                v,
                                            )
                                        }
                                        _ => FunctionCallExpression::create_empty_expression(
                                            unicase::Ascii::new(e.to_string()),
                                            vec![temp_str2],
                                        ),
                                    };
                                    self.push_expr(new_expr);
                                } else {
                                    self.push_expr(temp_str2);
                                }
                            }
                            self.src_ptr += 1;
                            if !self.parse_expr(
                                self.executable
                                    .variable_declarations
                                    .get(
                                        &(self.executable.source_buffer[self.src_ptr as usize - 1]
                                            - 1),
                                    )
                                    .unwrap()
                                    .variable
                                    .data
                                    .function_value
                                    .parameters as i32,
                                1,
                            ) {
                                return 1;
                            }
                            self.src_ptr -= 1;
                        } else {
                            if self.pass == 1 {
                                let tmp = self
                                    .varout(self.executable.source_buffer[self.src_ptr as usize]);
                                self.push_expr(tmp);
                            }
                            self.src_ptr += 1;
                            if self.executable.source_buffer[self.src_ptr as usize] != 0 {
                                self.executable
                                    .variable_declarations
                                    .get_mut(
                                        &(self.executable.source_buffer[self.src_ptr as usize - 1]
                                            - 1),
                                    )
                                    .unwrap()
                                    .report_variable_usage();
                                if self
                                    .dimexpr(self.executable.source_buffer[self.src_ptr as usize])
                                    != 0
                                {
                                    return 1;
                                }
                            }
                        }
                    } else {
                        let x = self.executable.source_buffer[self.src_ptr as usize];

                        if self.executable.source_buffer[self.src_ptr as usize]
                            < crate::tables::LAST_FUNC
                        {
                            log::error!(
                                "Error: Unknown function {} at {} avoiding...",
                                self.executable.source_buffer[self.src_ptr as usize],
                                self.src_ptr
                            );
                            return 1;
                        }
                        if self.pass == 1 {
                            if self.fnktout(x) != 0 {
                                tmp_func = self.executable.source_buffer[self.src_ptr as usize];
                            } else if tmp_func != 0 && self.fnktout(tmp_func) == 0 {
                                tmp_func = 0;
                            }
                        }
                    }
                    self.src_ptr += 1;
                }
            }

            if self.pass == 1 {
                let temp_str2 = self.pop_expr().unwrap();
                let tmp = self.pop_expr();
                if let Some(e) = tmp {
                    if let Expression::FunctionCall(expr) = e {
                        let mut v = expr.get_arguments().clone();
                        v.push(temp_str2);
                        self.push_expr(FunctionCallExpression::create_empty_expression(
                            expr.get_identifier().clone(),
                            v,
                        ));
                    } else {
                        if e.to_string() == "VAR001" {
                            println!("cur {cur_dim} dims: {dims}");
                            //panic!();
                        }

                        self.push_expr(FunctionCallExpression::create_empty_expression(
                            unicase::Ascii::new(e.to_string()),
                            vec![temp_str2],
                        ));
                    }
                } else {
                    self.push_expr(temp_str2);
                }
            }
        }

        self.exp_count = temp_expr;
        0
    }

    fn parse_expr(&mut self, max_expr: i32, _rec: i32) -> bool {
        let mut cur_expr = 0;
        let temp_expr = self.exp_count;
        self.src_ptr += 1;
        while (max_expr & 0x0ff) != cur_expr {
            cur_expr += 1;

            self.exp_count = 0;
            let mut trash_func = 0;

            while self.executable.source_buffer[self.src_ptr as usize] != 0 {
                if self.executable.source_buffer[self.src_ptr as usize] >= 0
                    && self.executable.source_buffer[self.src_ptr as usize]
                        <= self.executable.max_var
                {
                    if max_expr / 256 == cur_expr || max_expr / 256 == 0x0f {
                        self.executable
                            .variable_declarations
                            .get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1))
                            .unwrap()
                            .report_variable_usage();
                        if self.pass == 1 {
                            let tmp =
                                self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                            self.push_expr(tmp);
                        }
                        self.src_ptr += 1;

                        if self.executable.source_buffer[self.src_ptr as usize] != 0 {
                            if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize])
                                != 0
                            {
                                return false;
                            }
                            if self.pass == 1 {
                                let temp_str2 = self.pop_expr().unwrap();
                                let tmp3 = self.pop_expr();
                                if let Some(t) = tmp3 {
                                    self.push_expr(t);
                                }
                                self.push_expr(temp_str2);
                            }
                        }
                    } else {
                        unsafe {
                            if self.cur_stmt == 0xa8
                                && ((self
                                    .executable
                                    .variable_declarations
                                    .get(&self.akt_proc)
                                    .unwrap()
                                    .variable
                                    .data
                                    .function_value
                                    .return_var
                                    >> (cur_expr - 1))
                                    & 1)
                                    != 0
                            {
                                self.executable
                                    .variable_declarations
                                    .get_mut(
                                        &(self.executable.source_buffer[self.src_ptr as usize] - 1),
                                    )
                                    .unwrap()
                                    .report_variable_usage();
                            }

                            if self
                                .executable
                                .variable_declarations
                                .get(&(self.executable.source_buffer[self.src_ptr as usize] - 1))
                                .unwrap()
                                .header
                                .variable_type
                                == VariableType::Function
                            {
                                self.pushlabel(
                                    self.executable
                                        .variable_declarations
                                        .get(
                                            &(self.executable.source_buffer[self.src_ptr as usize]
                                                - 1),
                                        )
                                        .unwrap()
                                        .variable
                                        .data
                                        .function_value
                                        .start_offset as i32,
                                );
                                self.executable
                                    .variable_declarations
                                    .get_mut(
                                        &(self.executable.source_buffer[self.src_ptr as usize] - 1),
                                    )
                                    .unwrap()
                                    .report_variable_usage();
                                self.funcin(
                                    self.executable
                                        .variable_declarations
                                        .get(
                                            &(self.executable.source_buffer[self.src_ptr as usize]
                                                - 1),
                                        )
                                        .unwrap()
                                        .variable
                                        .data
                                        .function_value
                                        .start_offset as i32,
                                    self.executable.source_buffer[self.src_ptr as usize] - 1,
                                );
                                let mut stack_len = 0;
                                if self.pass == 1 {
                                    let tmp2 = self.varout(
                                        self.executable.source_buffer[self.src_ptr as usize],
                                    );
                                    let tmp3 = self.pop_expr();
                                    if let Some(t) = tmp3 {
                                        self.push_expr(t);
                                    }
                                    self.push_expr(tmp2);
                                    stack_len = self.expr_stack.len();
                                }

                                self.src_ptr += 1;

                                if !self.parse_expr(
                                    self.executable
                                        .variable_declarations
                                        .get(
                                            &(self.executable.source_buffer
                                                [self.src_ptr as usize - 1]
                                                - 1),
                                        )
                                        .unwrap()
                                        .variable
                                        .data
                                        .function_value
                                        .parameters as i32,
                                    1,
                                ) {
                                    return false;
                                }

                                if self.pass == 1 {
                                    let mut params = Vec::new();
                                    while stack_len < self.expr_stack.len() {
                                        params.push(self.pop_expr().unwrap());
                                    }

                                    let func_expr = self.pop_expr().unwrap();
                                    if let Expression::FunctionCall(mut expr) = func_expr {
                                        expr.set_arguments(params);
                                        self.push_expr(Expression::FunctionCall(expr));
                                    } else {
                                        self.warnings.push(format!(
                                            "function call expected but got {func_expr:?}"
                                        ));
                                        self.push_expr(func_expr);
                                    }
                                }
                            } else {
                                if self.pass == 1 {
                                    let tmp = self.varout(
                                        self.executable.source_buffer[self.src_ptr as usize],
                                    );
                                    self.push_expr(tmp);
                                }
                                self.src_ptr += 1;

                                if self.executable.source_buffer[self.src_ptr as usize] != 0 {
                                    self.executable
                                        .variable_declarations
                                        .get_mut(
                                            &(self.executable.source_buffer
                                                [self.src_ptr as usize - 1]
                                                - 1),
                                        )
                                        .unwrap()
                                        .report_variable_usage();
                                    if self.dimexpr(
                                        self.executable.source_buffer[self.src_ptr as usize],
                                    ) != 0
                                    {
                                        return false;
                                    }
                                }
                                self.src_ptr += 1;
                            }
                        }
                    }
                } else {
                    if self.pass == 1 {
                        if self.fnktout(self.executable.source_buffer[self.src_ptr as usize]) != 0 {
                            trash_func = self.executable.source_buffer[self.src_ptr as usize];
                        } else if trash_func != 0 && self.fnktout(trash_func) == 0 {
                            trash_func = 0;
                        }
                    }
                    self.src_ptr += 1;
                }
            }

            self.src_ptr += 1;
            if self.pass == 1 {
                let tmp2 = self.trans_exp(cur_expr);
                let tmp3 = self.pop_expr();
                if let Some(t) = tmp3 {
                    self.push_expr(t);
                }
                self.push_expr(tmp2);
            }
        }
        self.exp_count = temp_expr + 1;
        true
    }

    fn set_if_ptr(&mut self, i: i32) -> i32 {
        let j = self.executable.source_buffer[self.src_ptr as usize] as usize / 2;
        /*    assert!(
            !(j < 2 || j >= self.executable.source_buffer.len() - 1),
            "Error: Invalid IF pointer at {} buffer length is {}",
            self.src_ptr,
            self.executable.source_buffer.len() / 2
        );*/
        if self.executable.source_buffer[j - 2] == 0x0007
            && self.executable.source_buffer[j - 1] / 2 == i
        {
            self.executable.source_buffer[i as usize] = 0;
            self.executable.source_buffer[self.src_ptr as usize] / 2 - 2
        } else {
            i - 5
        }
    }

    fn outputpass2(
        &mut self,
        prg: &mut Program,
        if_while_stack: &mut Vec<OpCode>,
        stmt: Statement,
    ) {
        if if_while_stack.is_empty() {
            self.output_stmt(prg, stmt);
        } else {
            let op = if_while_stack.pop().unwrap();
            if let Some(expr) = self.pop_expr() {
                let expr = Statement::try_boolean_conversion(&expr);
                match op {
                    OpCode::WHILE => {
                        self.output_stmt(
                            prg,
                            WhileStatement::create_empty_statement(Box::new(expr), Box::new(stmt)),
                        );
                    }
                    OpCode::IF => self.output_stmt(
                        prg,
                        IfStatement::create_empty_statement(Box::new(expr), Box::new(stmt)),
                    ),
                    _ => {}
                }
            } else {
                self.errors.push(format!(
                    "Error: Missing expression for {} at {}",
                    op, self.src_ptr
                ));
            }
        }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn do_pass2(&mut self, prg: &mut Program, disassemble: bool) {
        self.cur_stmt = -1;
        self.next_label = 0;
        self.next_func = 0;
        self.src_ptr = 0;
        let mut if_ptr = -1;
        let mut if_while_stack = vec![];
        let mut last_stmt_offset = 0;
        if disassemble {
            println!();
            println!("Code size: {}", self.executable.code_size);

            for (i, x) in self.executable.source_buffer.iter().enumerate() {
                print!("{:04X} ", *x as i16);
                if i > 0 && (i % 16) == 0 {
                    println!();
                }
            }

            println!();

            println!("-----------------------------");

            println!("Offset  # OpCode      Parameters");
        }
        while self.src_ptr < self.executable.code_size / 2 {
            let prev_stat = self.cur_stmt;
            if self.src_ptr == if_ptr {
                self.src_ptr += 2;
            }

            self.funcout(prg, self.src_ptr * 2);
            self.labelout(prg, self.src_ptr * 2);
            if !self.valid_buffer[self.src_ptr as usize] {
                self.src_ptr += 1;
                continue;
            }

            let stmt_ptr = self.src_ptr;
            self.cur_stmt = self.executable.source_buffer[self.src_ptr as usize];
            assert!(
                !(self.cur_stmt > LAST_STMT
                    || STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] == 0xaa),
                "Error: Unknown statement {}, aborting…",
                self.cur_stmt
            );

            if prev_stat < 0 || STATEMENT_SIGNATURE_TABLE[prev_stat as usize] != 0xff {
                // self.output.push_str("    ");
            }

            let stack_begin = self.expr_stack.len();
            match STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] {
                0x00 |
                0xff |
                0xf6  // procedure 
                => {}
                0xf7 => { // dlockg
                    if !self.parse_expr(0x01, 0) {
                        return;
                    }
                    let tmp = self.varout(*self.executable.source_buffer.get(self.src_ptr as usize).unwrap());
                    self.push_expr(tmp);
                    if !self.parse_expr(0x01, 0) {
                        return;
                    }
                }
                0xf8 => { // dcreate
                    if !self.parse_expr(0x03, 0) {
                        return;
                    }
                    let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                    self.push_expr(tmp);
                    self.src_ptr += 1;
                }
                0xf9 => {// sort
                    let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize + 1]);
                    self.push_expr(tmp);
                    let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize + 2]);
                    self.push_expr(tmp);
                    self.src_ptr += 3;
                }
                0xfe => { // variable expressions - Println
                    self.src_ptr += 1;
                    if !self.parse_expr(self.executable.source_buffer[self.src_ptr as usize], 0) {
                        return;
                    }
                }
                0xfa => { // variable expressions, expr 1 is avar
                    let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize + 2]);
                    self.push_expr(tmp);
                    self.src_ptr += 2;
                    if !self.parse_expr(self.executable.source_buffer[self.src_ptr as usize - 1] - 1, 0) {
                        return;
                    }
                }
                0xfc => { // varies var
                    self.src_ptr += 1;
                    if !self.parse_expr(self.executable.source_buffer[self.src_ptr as usize] | 0xf00, 0) {
                        return;
                    }
                }
                0xfd => { // label (Goto)
                    self.src_ptr += 1;
                    let tmp = *self.labelnr(self.executable.source_buffer[self.src_ptr as usize]);
                    self.push_expr(IdentifierExpression::create_empty_expression(unicase::Ascii::new(format!("LABEL{tmp:>03}"))));
                    self.src_ptr += 1;
                }
                _ => {
                    if !self.parse_expr((STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0x0f), 0) {
                        return;
                    }
                }
            }
            let op: OpCode = unsafe { transmute(self.cur_stmt as u8) };
            if disassemble {
                if stmt_ptr > 0 {
                    self.print_disassembler_parameters(
                        last_stmt_offset as usize,
                        stmt_ptr as usize,
                    );
                }
                let op_str = format!("{:?}", op);

                print!("{:>5X}: {:02X} {:<12}", stmt_ptr * 2, self.cur_stmt, op_str);

                last_stmt_offset = stmt_ptr;
            }

            match op {
                OpCode::END => {
                    self.outputpass2(
                        prg,
                        &mut if_while_stack,
                        EndStatement::create_empty_statement(),
                    );
                    self.src_ptr += 1;
                }
                OpCode::RETURN => {
                    self.outputpass2(
                        prg,
                        &mut if_while_stack,
                        ReturnStatement::create_empty_statement(),
                    );
                    self.src_ptr += 1;
                }
                OpCode::GOTO => {
                    let label = self.pop_expr().unwrap().to_string();
                    self.outputpass2(
                        prg,
                        &mut if_while_stack,
                        GotoStatement::create_empty_statement(unicase::Ascii::new(label)),
                    );
                }
                OpCode::GOSUB => {
                    let label = self.pop_expr().unwrap().to_string();
                    self.outputpass2(
                        prg,
                        &mut if_while_stack,
                        GosubStatement::create_empty_statement(unicase::Ascii::new(label)),
                    );
                }
                OpCode::LET => {
                    let mut value = self.pop_expr().unwrap();
                    let variable = self.pop_expr().unwrap();
                    let (identifier, arguments) = match variable {
                        Expression::Identifier(name) => (name.get_identifier().clone(), vec![]),
                        Expression::Const(constant_expression) => {
                            match constant_expression.get_constant_value() {
                                Constant::String(name) => {
                                    (unicase::Ascii::new(name.clone()), vec![])
                                }
                                _ => panic!("can't translate const to let "),
                            }
                        }
                        Expression::FunctionCall(expr) => match expr.get_arguments().len() {
                            0..=3 => (expr.get_identifier().clone(), expr.get_arguments().clone()),
                            _ => panic!("can't translate func call to let"),
                        },
                        _ => panic!("can't translate func call to let"),
                    };

                    if let Some(var) = self.executable.variable_lookup.get(&identifier) {
                        let var = *var as i32;
                        let var = self.executable.variable_declarations.get(&var).unwrap();
                        if var.header.variable_type == VariableType::Boolean {
                            value = Statement::try_boolean_conversion(&value);
                        }
                    }

                    self.outputpass2(
                        prg,
                        &mut if_while_stack,
                        LetStatement::create_empty_statement(
                            identifier,
                            arguments,
                            Box::new(value),
                        ),
                    );
                }
                OpCode::WHILE | OpCode::IF => {
                    if_ptr = self.src_ptr;
                    if !self.parse_expr(0x001, 0) {
                        return;
                    }
                    if_ptr = self.set_if_ptr(if_ptr);
                    self.src_ptr += 1;
                    if_while_stack.push(op);
                }

                OpCode::FPCLR | OpCode::FEND => {
                    self.src_ptr += 1;
                }

                OpCode::PCALL => unsafe {
                    // PCALL
                    self.src_ptr += 2;
                    self.akt_proc = self.executable.source_buffer[self.src_ptr as usize - 1] - 1;
                    let proc_name = self
                        .executable
                        .variable_declarations
                        .get(&self.akt_proc)
                        .unwrap()
                        .get_name()
                        .clone();
                    if !self.parse_expr(
                        self.executable
                            .variable_declarations
                            .get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1))
                            .unwrap()
                            .variable
                            .data
                            .procedure_value
                            .parameters as i32,
                        0,
                    ) {
                        self.output_stmt(
                            prg,
                            ProcedureCallStatement::create_empty_statement(proc_name, vec![]),
                        );
                        return;
                    }
                    let mut params = Vec::new();
                    while !self.expr_stack.is_empty() {
                        params.push(self.pop_expr().unwrap());
                    }

                    params.reverse();
                    self.output_stmt(
                        prg,
                        ProcedureCallStatement::create_empty_statement(proc_name, params),
                    );
                },
                _ => {
                    let mut found = false;
                    for def in &STATEMENT_DEFINITIONS {
                        if def.opcode as i32 == self.cur_stmt {
                            let mut parameters = Vec::new();
                            if def.max_args > 0 {
                                for _ in 0..def.max_args {
                                    if self.expr_stack.len() <= stack_begin {
                                        break;
                                    }
                                    parameters.push(self.pop_expr().unwrap());
                                }
                            }
                            parameters.reverse();
                            self.outputpass2(
                                prg,
                                &mut if_while_stack,
                                PredefinedCallStatement::create_empty_statement(def, parameters),
                            );
                            found = true;
                            if def.max_args <= 0 {
                                self.src_ptr += 1;
                            }
                            break;
                        }
                    }
                    if !found {
                        log::error!("unknown statement opcode: {}", self.cur_stmt);
                        self.src_ptr += 1;
                    }
                }
            }

            match self.cur_stmt {
                0x00a9 | // ENDPROC
                0x00ab  // ENDFUNC
                => { //STOP
                    self.func_flag = 0;
                    self.proc_flag = 0;
                    self.output_stmt(prg, CommentAstNode::create_empty_statement("---------------------------------------".to_string()));
                }
                _ => {}
            }
        }

        if disassemble {
            self.print_disassembler_parameters(
                last_stmt_offset as usize,
                self.executable.source_buffer.len(),
            );
        }
        self.labelout(prg, self.src_ptr * 2);
    }

    fn name_variables(&mut self) {
        let has_user_vars = has_user_variables(
            &self.executable.variable_declarations,
            self.executable.version,
        );
        let mut name_generator: VariableNameGenerator =
            VariableNameGenerator::new(self.executable.version, has_user_vars);

        for i in 0..self.executable.max_var {
            if let Some(res) = self.executable.variable_declarations.get_mut(&i) {
                if res.get_type() == EntryType::FunctionResult {
                    res.set_name(format!("RESULT{i:>03}"));
                    continue;
                }

                let (name, is_user_variable) = name_generator.get_next_name(res);
                if is_user_variable {
                    res.set_type(EntryType::UserVariable);
                }
                res.set_name(name);
            }
        }
        for (i, res) in &self.executable.variable_declarations {
            self.executable
                .variable_lookup
                .insert(unicase::Ascii::new(res.get_name().clone()), *i as usize);
        }
    }

    fn get_variable_name(&self, cur_var: &VariableEntry) -> String {
        if cur_var.get_type() == EntryType::FunctionResult {
            return self
                .executable
                .variable_declarations
                .get(&cur_var.number)
                .unwrap()
                .get_name()
                .clone();
        }
        cur_var.get_name().clone()
    }

    pub fn print_variable_table(&self) {
        println!();
        println!(
            "--- Variable Table ({} variables) ---",
            self.executable.max_var
        );
        println!("   # Type         Flags Role          Name        Value");
        for i in (0..self.executable.max_var).rev() {
            let var = self.executable.variable_declarations.get(&i).unwrap();

            let ts = if var.header.dim > 0 {
                format!("{}({})", var.header.variable_type, var.header.dim)
            } else {
                var.header.variable_type.to_string()
            };

            print!("{:04X} {:<13}", var.header.id, ts);

            let ts = format!("{:?}", var.get_type());
            print!("{}     {:<14}", var.header.flags, ts);
            print!("{:<12}", var.get_name());

            if var.header.variable_type == VariableType::Function {
                unsafe {
                    print!("{:?}", var.variable.data.function_value);
                }
            } else if var.header.variable_type == VariableType::Procedure {
                unsafe {
                    print!("{:?}", var.variable.data.procedure_value);
                }
            } else {
                print!("{}", var.variable);
                if var.header.dim > 0
                    || var.header.vector_size > 0
                    || var.header.matrix_size > 0
                    || var.header.cube_size > 0
                {
                    print!(
                        " ({}, {}, {})",
                        var.header.vector_size, var.header.matrix_size, var.header.cube_size
                    );
                }
            }
            println!();
        }
    }

    fn print_disassembler_parameters(&self, from: usize, to: usize) {
        for (i, x) in self.executable.source_buffer[from + 1..to]
            .iter()
            .enumerate()
        {
            if i > 0 && i % 20 == 0 {
                println!();
                print!("                      ");
            }
            print!("{:<04X} ", *x);
        }
        println!();
    }
}

pub fn has_user_variables(
    variable_declarations: &HashMap<i32, Box<VariableEntry>>,
    version: u16,
) -> bool {
    let has_user_variables = variable_declarations[&0].header.variable_type
        == VariableType::Boolean
        && (variable_declarations[&1].header.variable_type == VariableType::Boolean)
        && (variable_declarations[&2].header.variable_type == VariableType::Boolean)
        && (variable_declarations[&3].header.variable_type == VariableType::Boolean)
        && (variable_declarations[&4].header.variable_type == VariableType::Date)
        && (variable_declarations[&5].header.variable_type == VariableType::Integer)
        && (variable_declarations[&6].header.variable_type == VariableType::Integer)
        && (variable_declarations[&7].header.variable_type == VariableType::Integer)
        && (variable_declarations[&8].header.variable_type == VariableType::String)
        && (variable_declarations[&9].header.variable_type == VariableType::String)
        && (variable_declarations[&10].header.variable_type == VariableType::String)
        && (variable_declarations[&11].header.variable_type == VariableType::String)
        && (variable_declarations[&12].header.variable_type == VariableType::String)
        && (variable_declarations[&13].header.variable_type == VariableType::String)
        && (variable_declarations[&14].header.variable_type == VariableType::String)
        && (variable_declarations[&15].header.variable_type == VariableType::Boolean)
        && (variable_declarations[&16].header.variable_type == VariableType::Boolean)
        && (variable_declarations[&17].header.variable_type == VariableType::Boolean)
        && (variable_declarations[&18].header.variable_type == VariableType::String)
        && (variable_declarations[&19].header.variable_type == VariableType::String)
        && (variable_declarations[&20].header.variable_type == VariableType::String)
        && (variable_declarations[&21].header.variable_type == VariableType::String)
        && (variable_declarations[&22].header.variable_type == VariableType::Date)
        && (variable_declarations[&20].header.vector_size == 5);

    if has_user_variables
        && version >= 300
        && !(variable_declarations[&23].header.variable_type == VariableType::Integer
            && variable_declarations[&23].header.vector_size == 16)
    {
        return false;
    }

    has_user_variables
}
