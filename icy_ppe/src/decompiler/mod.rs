use crate::ast::constant::BuiltinConst;
use crate::ast::{
    BinaryExpression, CommentStatement, Constant, ConstantExpression, EndStatement, Expression,
    FunctionCallExpression, FunctionImplementation, GosubStatement, GotoStatement,
    IdentifierExpression, IfStatement, LabelStatement, LetStatement, ParameterSpecifier,
    ParensExpression, PredefinedCallStatement, ProcedureCallStatement, ProcedureImplementation,
    Program, ReturnStatement, Statement, UnaryExpression, UnaryOp, VariableDeclarationStatement,
    VariableSpecifier, VariableType, WhileStatement,
};
use crate::executable::{read_file, Executable};
use crate::tables::{
    FuncOpCode, OpCode, BIN_EXPR, FUNCTION_DEFINITIONS, STATEMENT_DEFINITIONS,
    STATEMENT_SIGNATURE_TABLE, TYPE_NAMES,
};
use std::collections::{HashMap, HashSet};
use std::intrinsics::transmute;
use std::path::PathBuf;
use std::vec;

pub mod reconstruct;

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
    d.dump_vars(&mut prg);
    d.do_pass2(&mut prg);
    prg
}

pub struct Decompiler {
    pub executable: Executable,
    cur_stmt: i32,
    akt_proc: i32,
    uvar_flag: bool,
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
            uvar_flag: false,
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
        }
    }

    pub fn output_stmt(&mut self, prg: &mut Program, stmt: Statement) {
        for f in &mut prg.implementations {
            match f {
                crate::ast::Implementations::Function(decl) => {
                    if self.func_flag > 0 && decl.id == self.func_flag {
                        decl.get_statements_mut().push(stmt);
                        return;
                    }
                }
                crate::ast::Implementations::Procedure(decl) => {
                    if self.proc_flag > 0 && decl.id == self.proc_flag {
                        decl.get_statements_mut().push(stmt);
                        return;
                    }
                }
                crate::ast::Implementations::Comment(_) => {}
            }
        }
        prg.statements.push(stmt);
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
                    0xf6 => {
                        self.src_ptr += 2;
                        self.akt_proc =
                            self.executable.source_buffer[(self.src_ptr - 1) as usize] - 1;
                        self.executable
                            .variable_declarations
                            .get_mut(&self.akt_proc)
                            .unwrap()
                            .flag = 1;
                        let act_dec_start = self
                            .executable
                            .variable_declarations
                            .get(&self.akt_proc)
                            .unwrap()
                            .function_header
                            .start;
                        let act_dec_args = self
                            .executable
                            .variable_declarations
                            .get(&self.akt_proc)
                            .unwrap()
                            .function_header
                            .args;

                        self.funcin(act_dec_start, self.akt_proc);
                        self.pushlabel(act_dec_start);
                        trap = !self.parse_expr(act_dec_args, 0);
                    }
                    0xf7 => {
                        if self.parse_expr(0x01, 0) {
                            self.executable
                                .variable_declarations
                                .get_mut(
                                    &(self.executable.source_buffer[self.src_ptr as usize] - 1),
                                )
                                .unwrap()
                                .flag = 1;
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
                                .flag = 1;
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
                            .flag = 1;
                        self.executable
                            .variable_declarations
                            .get_mut(
                                &(self.executable.source_buffer[self.src_ptr as usize + 2] - 1),
                            )
                            .unwrap()
                            .flag = 1;
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
                            .flag = 1;
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
    pub fn dump_vars(&mut self, prg: &mut Program) {
        self.uvar_flag = self.executable.variable_declarations[&0]
            .header
            .variable_type
            == VariableType::Boolean
            && (self.executable.variable_declarations[&1]
                .header
                .variable_type
                == VariableType::Boolean)
            && (self.executable.variable_declarations[&2]
                .header
                .variable_type
                == VariableType::Boolean)
            && (self.executable.variable_declarations[&3]
                .header
                .variable_type
                == VariableType::Boolean)
            && (self.executable.variable_declarations[&4]
                .header
                .variable_type
                == VariableType::Date)
            && (self.executable.variable_declarations[&5]
                .header
                .variable_type
                == VariableType::Integer)
            && (self.executable.variable_declarations[&6]
                .header
                .variable_type
                == VariableType::Integer)
            && (self.executable.variable_declarations[&7]
                .header
                .variable_type
                == VariableType::Integer)
            && (self.executable.variable_declarations[&8]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&9]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&10]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&11]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&12]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&13]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&14]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&15]
                .header
                .variable_type
                == VariableType::Boolean)
            && (self.executable.variable_declarations[&16]
                .header
                .variable_type
                == VariableType::Boolean)
            && (self.executable.variable_declarations[&17]
                .header
                .variable_type
                == VariableType::Boolean)
            && (self.executable.variable_declarations[&18]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&19]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&20]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&21]
                .header
                .variable_type
                == VariableType::String)
            && (self.executable.variable_declarations[&22]
                .header
                .variable_type
                == VariableType::Date)
            && (self.executable.variable_declarations[&20]
                .header
                .vector_size
                == 5);

        if self.uvar_flag
            && self.executable.version >= 300
            && !(self.executable.variable_declarations[&23]
                .header
                .variable_type
                == VariableType::Integer
                && self.executable.variable_declarations[&23]
                    .header
                    .vector_size
                    == 16)
        {
            self.uvar_flag = false;
        }

        if self.symbol == 0 {
            /*                self.output_stmt(prg, Statement::Comment("Source Code:".to_string()));
            self.output_stmt(prg, Statement::Comment("--------------------------------".to_string()));*/
        }

        let mut i: i32 = 0;
        let mut c_vars = 0;
        let mut c_const = 0;
        let mut c_func = 0;
        let mut c_proc = 0;

        let mut string_vars = 0;
        let mut int_vars = 0;
        let mut bool_vars = 0;
        let mut money_vars = 0;
        let mut byte_vars = 0;
        let mut time_vars = 0;
        let mut date_vars = 0;
        let mut generic_vars = 0;

        while i < self.executable.max_var {
            if (i > 0x16 || !self.uvar_flag)
                && self
                    .executable
                    .variable_declarations
                    .get_mut(&i)
                    .unwrap()
                    .lflag
                    == 0
            {
                match self
                    .executable
                    .variable_declarations
                    .get_mut(&i)
                    .unwrap()
                    .header
                    .variable_type
                {
                    VariableType::Function => {
                        if self
                            .executable
                            .variable_declarations
                            .get_mut(&i)
                            .unwrap()
                            .flag
                            == 1
                        {
                            c_func += 1;
                            self.executable
                                .variable_declarations
                                .get_mut(&i)
                                .unwrap()
                                .number = c_func;
                            let n = &(self
                                .executable
                                .variable_declarations
                                .get_mut(&i)
                                .unwrap()
                                .function_header
                                .return_var
                                - 1);
                            self.executable
                                .variable_declarations
                                .get_mut(n)
                                .unwrap()
                                .number = c_func;
                        }
                    }
                    VariableType::Procedure => {
                        let cur_var = self.executable.variable_declarations.get_mut(&i).unwrap();
                        if cur_var.flag == 1 {
                            c_proc += 1;
                            cur_var.number = c_proc;
                        }
                    }
                    _ => {
                        let cur_var = self.executable.variable_declarations.get_mut(&i).unwrap();
                        if cur_var.flag == 0 {
                            c_const += 1;
                            cur_var.number = c_const;
                        } else {
                            c_vars += 1;
                            cur_var.number = c_vars;
                            if self.symbol == 0 {
                                let var_type = TYPE_NAMES[cur_var.header.variable_type as usize];
                                let var_name = match cur_var.header.variable_type {
                                    VariableType::String => {
                                        string_vars += 1;
                                        format!("STR{string_vars:>03}")
                                    }
                                    VariableType::BigStr => {
                                        string_vars += 1;
                                        format!("BSTR{string_vars:>03}")
                                    }
                                    VariableType::Integer => {
                                        int_vars += 1;
                                        format!("INT{int_vars:>03}")
                                    }
                                    VariableType::Boolean => {
                                        bool_vars += 1;
                                        format!("BOOL{bool_vars:>03}")
                                    }
                                    VariableType::Byte => {
                                        byte_vars += 1;
                                        format!("BYTE{byte_vars:>03}")
                                    }
                                    VariableType::Money => {
                                        money_vars += 1;
                                        format!("MONEY{money_vars:>03}")
                                    }
                                    VariableType::Time => {
                                        time_vars += 1;
                                        format!("TIME{time_vars:>03}")
                                    }
                                    VariableType::Date => {
                                        date_vars += 1;
                                        format!("DATE{date_vars:>03}")
                                    }
                                    _ => {
                                        generic_vars += 1;
                                        format!("VAR{generic_vars:>03}")
                                    }
                                };
                                cur_var.var_name = var_name.clone();
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

                                self.output_stmt(
                                    prg,
                                    VariableDeclarationStatement::create_empty_statement(
                                        var_type,
                                        vec![VariableSpecifier::empty(
                                            unicase::Ascii::new(var_name),
                                            dims,
                                        )],
                                    ),
                                );
                            }
                        }
                    }
                }
            }
            i += 1;
        }

        if self.symbol == 0 {
            // self.output_stmt(prg, Statement::Comment("--------------------------------".to_string()));
        }
    }

    fn output_func(&mut self, prg: &mut Program, func: i32) {
        let func_name = format!(
            "FUNC{0:>03}",
            self.executable
                .variable_declarations
                .get(&func)
                .unwrap()
                .number
        );
        let mut func_parameters = vec![];

        let mut j = self
            .executable
            .variable_declarations
            .get(&func)
            .unwrap()
            .function_header
            .first_var;
        for _ in 0..self
            .executable
            .variable_declarations
            .get(&func)
            .unwrap()
            .function_header
            .args
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
        prg.implementations
            .push(crate::ast::Implementations::Function(
                FunctionImplementation::empty(
                    func,
                    func_name,
                    func_parameters,
                    func_type,
                    Vec::new(),
                ),
            ));
    }

    fn output_proc(&mut self, prg: &mut Program, proc: i32) {
        let proc_name = format!(
            "PROC{0:>03}",
            self.executable
                .variable_declarations
                .get(&proc)
                .unwrap()
                .number
        );
        let param_flags = self
            .executable
            .variable_declarations
            .get(&proc)
            .unwrap()
            .function_header
            .return_var;

        let mut proc_parameters = vec![];
        let mut j = self
            .executable
            .variable_declarations
            .get(&proc)
            .unwrap()
            .function_header
            .first_var;
        for n in 0..self
            .executable
            .variable_declarations
            .get(&proc)
            .unwrap()
            .function_header
            .args
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
        prg.implementations
            .push(crate::ast::Implementations::Procedure(
                ProcedureImplementation::empty(proc, proc_name, proc_parameters, Vec::new()),
            ));
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
        for f in &mut prg.implementations {
            if let crate::ast::Implementations::Function(decl) = f {
                if decl.id == func {
                    return Some(decl);
                }
            }
        }
        None
    }

    /*

    fn get_procedure_mut(prg: &mut Program, func: i32) -> Option<&mut ProcedureImplementation> {
        prg.procedure_implementations
            .iter_mut()
            .find(|f| f.id == func)
    }*/

    fn dump_locs(&mut self, prg: &mut Program, func: i32) {
        let mut i;
        let mx_var: i32;
        let mut j = 0;
        // StackPtr = 0;
        let cur_var = self.executable.variable_declarations.get(&func).unwrap();
        if cur_var.header.variable_type == VariableType::Function {
            i = cur_var.function_header.return_var;
            mx_var = cur_var.function_header.return_var + cur_var.function_header.total_var;
        } else {
            i = cur_var.function_header.first_var + cur_var.function_header.args;
            mx_var = cur_var.function_header.first_var + cur_var.function_header.total_var;
            //+1;
        }

        while i < mx_var {
            if self.executable.variable_declarations.get(&i).unwrap().flag == 1 {
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
                    let cur_var = &self.executable.variable_declarations[&i];

                    let func = Decompiler::get_function_mut(prg, func).unwrap();

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
                }
            }
            i += 1;
            j += 1;
        }
        if j > 0 {
            // self.output.push_str("\n");
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
        if self.uvar_flag
            && (self.executable.version < 300 && var_nr < 0x17
                || self.executable.version >= 300 && var_nr < 0x18)
        {
            match var_nr {
                0 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_EXPERT".to_string(),
                    ))
                }
                1 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_FSE".to_string(),
                    ))
                }
                2 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_FSEP".to_string(),
                    ))
                }
                3 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_CLS".to_string(),
                    ))
                }
                4 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_EXPDATE".to_string(),
                    ))
                }
                5 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_SEC".to_string(),
                    ))
                }
                6 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_PAGELEN".to_string(),
                    ))
                }
                7 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_EXPSEC".to_string(),
                    ))
                }
                8 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_CITY".to_string(),
                    ))
                }
                9 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_BDPHONE".to_string(),
                    ))
                }
                10 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_HVPHONE".to_string(),
                    ))
                }
                11 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_TRANS".to_string(),
                    ))
                }
                12 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_CMNT1".to_string(),
                    ))
                }
                13 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_CMNT2".to_string(),
                    ))
                }
                14 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_PWD".to_string(),
                    ))
                }
                15 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_SCROLL".to_string(),
                    ))
                }
                16 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_LONGHDR".to_string(),
                    ))
                }
                17 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_DEF79".to_string(),
                    ))
                }
                18 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_ALIAS".to_string(),
                    ))
                }
                19 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_VER".to_string(),
                    ))
                }
                20 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_ADDR".to_string(),
                    ))
                }
                21 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_NOTES".to_string(),
                    ))
                }
                22 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_PWDEXP".to_string(),
                    ))
                }
                23 => {
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        "U_ACCOUNT".to_string(),
                    ))
                }

                // Added in 3.40
                /*              24 => return IdentifierExpression::create_empty_expression("U_SHORTDESC"),
                                25 => return IdentifierExpression::create_empty_expression("U_GENDER"),
                                26 => return IdentifierExpression::create_empty_expression("U_BIRTHDATE"),
                                27 => return IdentifierExpression::create_empty_expression("U_EMAIL"),
                                28 => return IdentifierExpression::create_empty_expression("U_WEB"),
                */
                _ => {
                    return FunctionCallExpression::create_empty_expression(
                        unicase::Ascii::new("????".to_string()),
                        vec![],
                    )
                }
            }
        }

        let cur_var = self.executable.variable_declarations.get(&var_nr).unwrap();
        if cur_var.flag == 1 {
            match cur_var.header.variable_type {
                VariableType::Function => {
                    return FunctionCallExpression::create_empty_expression(
                        unicase::Ascii::new(format!("FUNC{0:>03}", cur_var.number)),
                        vec![],
                    );
                }
                VariableType::Procedure => {
                    return FunctionCallExpression::create_empty_expression(
                        unicase::Ascii::new(format!("PROC{0:>03}", cur_var.number)),
                        vec![],
                    );
                }
                _ => {
                    if self
                        .executable
                        .variable_declarations
                        .get(&var_nr)
                        .unwrap()
                        .fflag
                        == 1
                    {
                        return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                            format!("FUNC{0:>03}", cur_var.number),
                        ));
                    }
                    if self
                        .executable
                        .variable_declarations
                        .get(&var_nr)
                        .unwrap()
                        .lflag
                        == 1
                    {
                        return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                            format!("LOC{0:>03}", cur_var.number),
                        ));
                    }
                    return IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        cur_var.var_name.clone(),
                    ));
                }
            }
        }
        match cur_var.header.variable_type {
            VariableType::Boolean => {
                if cur_var.content == 0 {
                    ConstantExpression::create_empty_expression(Constant::Boolean(false))
                } else {
                    ConstantExpression::create_empty_expression(Constant::Boolean(true))
                }
            }
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
            | VariableType::Time => ConstantExpression::create_empty_expression(Constant::Integer(
                cur_var.content as i32,
            )),
            VariableType::BigStr | VariableType::String => {
                ConstantExpression::create_empty_expression(Constant::String(
                    cur_var.string_value.clone(),
                ))
            }
            VariableType::Double | VariableType::Real => {
                ConstantExpression::create_empty_expression(Constant::Real(cur_var.content as f64))
            }
            _ => ConstantExpression::create_empty_expression(Constant::Integer(0)),
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
        self.push_expr(FunctionCallExpression::create_empty_expression(
            unicase::Ascii::new(func_def.name.to_string()),
            parameters,
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
                                .function_header
                                .start,
                        );
                        self.executable
                            .variable_declarations
                            .get_mut(&(self.executable.source_buffer[self.src_ptr as usize - 1]))
                            .unwrap()
                            .flag = 1;
                        self.funcin(
                            self.executable
                                .variable_declarations
                                .get(var_idx)
                                .unwrap()
                                .function_header
                                .start,
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
                                    &(self.executable.source_buffer[self.src_ptr as usize - 1] - 1),
                                )
                                .unwrap()
                                .function_header
                                .args,
                            1,
                        ) {
                            return 1;
                        }
                        self.src_ptr -= 1;
                    } else {
                        if self.pass == 1 {
                            let tmp =
                                self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                            self.push_expr(tmp);
                        }
                        self.src_ptr += 1;
                        if self.executable.source_buffer[self.src_ptr as usize] != 0 {
                            self.executable
                                .variable_declarations
                                .get_mut(
                                    &(self.executable.source_buffer[self.src_ptr as usize - 1] - 1),
                                )
                                .unwrap()
                                .flag = 1;
                            if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize])
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
                            .flag = 1;
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
                        if self.cur_stmt == 0xa8
                            && ((self
                                .executable
                                .variable_declarations
                                .get(&self.akt_proc)
                                .unwrap()
                                .function_header
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
                                .flag = 1;
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
                                        &(self.executable.source_buffer[self.src_ptr as usize] - 1),
                                    )
                                    .unwrap()
                                    .function_header
                                    .start,
                            );
                            self.executable
                                .variable_declarations
                                .get_mut(
                                    &(self.executable.source_buffer[self.src_ptr as usize] - 1),
                                )
                                .unwrap()
                                .flag = 1;
                            self.funcin(
                                self.executable
                                    .variable_declarations
                                    .get(
                                        &(self.executable.source_buffer[self.src_ptr as usize] - 1),
                                    )
                                    .unwrap()
                                    .function_header
                                    .start,
                                self.executable.source_buffer[self.src_ptr as usize] - 1,
                            );
                            let mut stack_len = 0;
                            if self.pass == 1 {
                                let tmp2 = self
                                    .varout(self.executable.source_buffer[self.src_ptr as usize]);
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
                                        &(self.executable.source_buffer[self.src_ptr as usize - 1]
                                            - 1),
                                    )
                                    .unwrap()
                                    .function_header
                                    .args,
                                1,
                            ) {
                                return false;
                            }

                            if self.pass == 1 {
                                let mut params = Vec::new();
                                while stack_len < self.expr_stack.len() {
                                    params.push(self.pop_expr().unwrap());
                                }

                                let func_name = self.pop_expr().unwrap();

                                match func_name {
                                    Expression::FunctionCall(expr) => {
                                        self.push_expr(Expression::FunctionCall(expr));
                                    }
                                    _ => {
                                        self.push_expr(func_name);
                                    }
                                }
                            }
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
                                    .flag = 1;
                                if self
                                    .dimexpr(self.executable.source_buffer[self.src_ptr as usize])
                                    != 0
                                {
                                    return false;
                                }
                            }
                            self.src_ptr += 1;
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
        assert!(
            !(j < 2 || j >= self.executable.source_buffer.len() - 1),
            "Error: Invalid IF pointer at {} buffer length is {}",
            self.src_ptr,
            self.executable.source_buffer.len() / 2
        );
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
            let expr = self.pop_expr().unwrap();
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
        }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn do_pass2(&mut self, prg: &mut Program) {
        self.cur_stmt = -1;
        self.next_label = 0;
        self.next_func = 0;
        self.src_ptr = 0;
        let mut if_ptr = -1;
        let mut if_while_stack = vec![];

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
                    let value = self.pop_expr().unwrap();
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

                OpCode::PCALL => {
                    // PCALL
                    self.src_ptr += 2;
                    self.akt_proc = self.executable.source_buffer[self.src_ptr as usize - 1] - 1;
                    let proc_name = format!(
                        "PROC{0:>03}",
                        self.executable
                            .variable_declarations
                            .get(&self.akt_proc)
                            .unwrap()
                            .number
                    );
                    if !self.parse_expr(
                        self.executable
                            .variable_declarations
                            .get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1))
                            .unwrap()
                            .function_header
                            .args,
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
                }
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
                    self.output_stmt(prg, CommentStatement::create_empty_statement("---------------------------------------".to_string()));
                }
                _ => {}
            }
        }

        self.labelout(prg, self.src_ptr * 2);
    }
}

#[cfg(test)]
mod tests {
    /*
    fn is_match(output: &str, original: &str) -> bool {
        let mut i = 0;
        let mut j = 0;

        let output = output.as_bytes();
        let original = original.as_bytes();

        while i < output.len() && j < original.len() {
            // skip comments - assume that ';' is not inside a string
            if output[i] == b';' {
                while output[i] != b'\n' {
                    i += 1;
                }
            }

            if output[i] == original[j] {
                i += 1;
                j += 1;
                continue;
            }
            if char::is_whitespace(output[i] as char) {
                i += 1;
                continue;
            }
            if char::is_whitespace(original[j] as char) {
                j += 1;
                continue;
            }
            return false;
        }
        // skip original trailing ws.
        while j < original.len() && char::is_whitespace(original[j] as char) {
            j += 1;
        }
        if j >= original.len() {
            return true;
        }
        //        println!("at {} {}", j, String::from_utf8_lossy(&output[j..]));
        false
    }

    #[test]
    fn test_decompiler() {
        use std::fs::{self};

        let mut data_path = env::current_dir().unwrap();
        data_path.push("test_data");
        //let mut success = 0;
        //let mut skipped = 0;
        for entry in fs::read_dir(data_path).expect("Error reading test_data directory.") {
            let cur_entry = entry.unwrap().path();

            if cur_entry.extension().unwrap() != "ppe" {
                continue;
            }

            let file_name = cur_entry.as_os_str();
            println!(
                "File: {}...",
                cur_entry.file_name().unwrap().to_str().unwrap()
            );
            /*
            if !cur_entry.file_name().unwrap().to_str().unwrap().eq("if_elseif_else_endif_end.ppe")
            {
                // println!("skip.");
                //skipped += 1;
                continue;
            }

            if cur_entry.file_name().unwrap().to_str().unwrap().eq("if_elseif_else_endif_end.ppe")
            {
                // println!("skip.");
                //skipped += 1;
                continue;
            }*/

            let d = crate::decompiler::decompile(file_name.to_str().unwrap(), false, false);
            let source_file = cur_entry.with_extension("pps");
            let orig_text = fs::read_to_string(source_file).unwrap();

            let are_equal = is_match(&d.to_string(), &orig_text);

            if are_equal {
                // println!(" match.");
                //success += 1;
            } else {
                println!(
                    "'{}' not matched…\n{}-----\n{}",
                    cur_entry.file_name().unwrap().to_str().unwrap(),
                    d,
                    orig_text
                );
            }

            assert!(are_equal);
        }
        // println!("successful {} skipped {}", success, skipped);
    }*/
}
