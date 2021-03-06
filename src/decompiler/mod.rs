use std::collections::{HashMap, HashSet};
use std::intrinsics::transmute;
use crate::ast::{Block, Constant, Declaration, Expression, FunctionImplementation, Program, Statement, VariableType, get_var_name};
use crate::executable::{Executable, read_file};
use crate::tables::{BIN_EXPR, FUNCTION_DEFINITIONS, FuncOpCode, OpCode, STATEMENT_DEFINITIONS, STATEMENT_SIGNATURE_TABLE, TYPE_NAMES};

pub mod reconstruct;

const LAST_FUNC: i32 = -286;
const LAST_STMT: i32 = 0x00e2;

struct FuncL
{
    func: i32,
}

#[must_use] pub fn decompile(file_name: &str, to_file: bool, raw: bool) -> Program {
    let mut prg = Program::new();
    let mut d = Decompiler::new(read_file(file_name));
    
    println!("{}.{} detected", d.executable.version / 100, d.executable.version % 100);


    if to_file {
        println!("Pass 1 ...");
    }
    d.do_pass1();
    if to_file {
        println!();
    }
    d.dump_vars(&mut prg);
    if to_file {
        println!("Pass 2 ...");
    }
    d.do_pass2(&mut prg);
    if !raw {
        if to_file {
            println!("Pass 3 ...");
        }
        reconstruct::do_pass3(&mut prg);
        reconstruct::do_pass4(&mut prg);
    }
    if to_file {
        println!();
        println!("Source decompilation complete...");
    }

    let trash_flag = d.trash_flag;
    if trash_flag != 0 {
        d.output_stmt(&mut prg, Statement::Comment("---------------------------------------".to_string()));
        d.output_stmt(&mut prg, Statement::Comment(format!("!!! {} ERROR(S) CAUSED BY PPLC BUGS DETECTED", trash_flag)));
        d.output_stmt(&mut prg, Statement::Comment("PROBLEM: These expressions most probably looked like !0+!0+!0 or similar".to_string()));
        d.output_stmt(&mut prg, Statement::Comment("         before PPLC fucked it up to something like !(!(+0)+0)!0. This may".to_string()));
        d.output_stmt(&mut prg, Statement::Comment("         also apply to - / and * aswell. Also occurs upon use of multiple !'s.".to_string()));
        d.output_stmt(&mut prg, Statement::Comment("         Some smartbrains use this BUG to make programms running different".to_string()));
        d.output_stmt(&mut prg, Statement::Comment("         (or not at all) when being de- and recompiled.".to_string()));
        if to_file {
            println!("{} COMPILER ERROR(S) DETECTED", trash_flag);
        }
    }
    prg
}

#[must_use]pub fn load_file(file_name: &str) -> Program {
    let mut prg = Program::new();
    let mut d = Decompiler::new(read_file(file_name));
    d.do_pass1();
    d.dump_vars(&mut prg);
    d.do_pass2(&mut prg);
    prg
}

struct Decompiler
{
    executable: Executable,
    cur_stmt: i32,
    akt_proc: i32,
    uvar_flag: bool,
    pass: i32,
    next_label: i32,
    next_func: i32,
    func_flag: i32,
    proc_flag: i32,
    src_ptr: i32,
    symbol:
     i32,
    valid_buffer: Vec<bool>,
    exp_count: i32,
    trash_flag: i32,
    label_used: HashMap<i32, i32>,
    func_used: HashMap<i32, FuncL>,
    label_stack: Vec<i32>,
    expr_stack: Vec<Expression>,
}


impl Decompiler {
    fn new(executable: Executable) -> Self {
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
            trash_flag: 0,
            label_used: HashMap::new(),
            func_used: HashMap::new(),
            label_stack: Vec::new(),
            expr_stack: Vec::new(),
        }
    }

    

    pub fn output_stmt(&mut self, prg: &mut Program, stmt: Statement)
    {
        if self.func_flag > 0 {
            for decl in &mut prg.function_implementations {
                if decl.id == self.func_flag {
                    decl.block.statements.push(stmt);
                    return;
                }
            }
        }
        if self.proc_flag > 0 {
            for decl in &mut prg.procedure_implementations {
                if decl.id == self.proc_flag {
                    decl.block.statements.push(stmt);
                    return;
                }
            }
        }
        prg.main_block.statements.push(stmt);
    }

    fn fill_valid(&mut self, i: i32, j: i32) {
        for n in i..j {
            self.valid_buffer[n as usize] = true;
        }
    }

    fn do_pass1(&mut self) {
        let mut last_point = 0;
        let mut if_ptr = -5;

        self.cur_stmt = 0;
        self.next_label = 0;
        self.next_func = 0;
        self.src_ptr = 0;

        while self.src_ptr != -1 && self.src_ptr <= self.executable.code_size / 2 {
            if self.src_ptr >= self.executable.code_size / 2 || self.valid_buffer[self.src_ptr as usize] {
                self.fill_valid(last_point, self.src_ptr);
            } else {
                let prev_stat = self.cur_stmt;
                if self.src_ptr == if_ptr { self.src_ptr += 2; }
                self.cur_stmt = self.executable.source_buffer[self.src_ptr as usize];

                assert!(self.cur_stmt < LAST_STMT && (self.cur_stmt as usize) < STATEMENT_SIGNATURE_TABLE.len() && STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] != 0xaa, "Error: Unknown statement {} at {}.", self.cur_stmt, self.src_ptr);
                assert!(self.cur_stmt >= 0 && self.cur_stmt < LAST_STMT, "opCode needs to be 0..226 at {} was {}", self.src_ptr, self.cur_stmt);
                
                let mut trap = false;
                match STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] {
                    0xf6 => {
                        self.src_ptr += 2;
                        self.akt_proc = self.executable.source_buffer[(self.src_ptr - 1) as usize] - 1;
                        self.executable.variable_declarations.get_mut(&self.akt_proc).unwrap().flag = 1;
                        let act_dec_start = self.executable.variable_declarations.get(&self.akt_proc).unwrap().start;
                        let act_dec_args = self.executable.variable_declarations.get(&self.akt_proc).unwrap().args;

                        self.funcin(act_dec_start, self.akt_proc);
                        self.pushlabel(act_dec_start);
                        trap = !self.parse_expr(act_dec_args, 0);
                    }
                    0xf7 => {
                        if self.parse_expr(0x01, 0) {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            trap = !self.parse_expr(0x01, 0);
                        } else {
                            trap = true;
                        }
                    }
                    0xf8 => {
                        if self.parse_expr(0x03, 0) {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            self.src_ptr += 1;
                        } else {
                            trap = true;
                        }
                    }
                    0xf9 => {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 1] - 1)).unwrap().flag = 1;
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 2] - 1)).unwrap().flag = 1;
                        self.src_ptr += 3;
                    }
                    0xfe => {
                        self.src_ptr += 1;
                        trap = !self.parse_expr(self.executable.source_buffer[self.src_ptr as usize], 0);
                    }
                    0xfa => {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 2] - 1)).unwrap().flag = 1;
                        self.src_ptr += 2;
                        trap = !self.parse_expr(self.executable.source_buffer[self.src_ptr as usize - 1] - 1, 0);
                    }
                    0xfc => {
                        self.src_ptr += 1;
                        trap = !self.parse_expr(self.executable.source_buffer[self.src_ptr as usize] | 0xf00, 0);
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
                        trap = !self.parse_expr((STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0x0f), 0);
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


    fn dump_vars(&mut self, prg: &mut Program) {
        self.uvar_flag =
            self.executable.variable_declarations[&0].variable_type == VariableType::Boolean &&
                (self.executable.variable_declarations[&1].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&2].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&3].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&4].variable_type == VariableType::Date) &&
                (self.executable.variable_declarations[&5].variable_type == VariableType::Integer) &&
                (self.executable.variable_declarations[&6].variable_type == VariableType::Integer) &&
                (self.executable.variable_declarations[&7].variable_type == VariableType::Integer) &&
                (self.executable.variable_declarations[&8].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&9].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&10].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&11].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&12].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&13].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&14].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&15].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&16].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&17].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&18].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&19].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&20].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&21].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&22].variable_type == VariableType::Date) &&
                (self.executable.variable_declarations[&20].vector_size == 5);

        if self.uvar_flag && self.executable.version >= 300 && !(self.executable.variable_declarations[&23].variable_type == VariableType::Integer && self.executable.variable_declarations[&23].vector_size == 16) {
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
            if (i > 0x16 || !self.uvar_flag) && self.executable.variable_declarations.get_mut(&i).unwrap().lflag == 0 {
                match self.executable.variable_declarations.get_mut(&i).unwrap().variable_type {
                    VariableType::Function => {
                        if self.executable.variable_declarations.get_mut(&i).unwrap().flag == 1 {
                            c_func += 1;
                            self.executable.variable_declarations.get_mut(&i).unwrap().number = c_func;
                            let n = &(self.executable.variable_declarations.get_mut(&i).unwrap().return_var - 1);
                            self.executable.variable_declarations.get_mut(n).unwrap().number = c_func;
                        }
                    }
                    VariableType::Procedure => {
                        let mut cur_var = self.executable.variable_declarations.get_mut(&i).unwrap();
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
                                let var_type = TYPE_NAMES[cur_var.variable_type as usize];
                                cur_var.var_name = match cur_var.variable_type {
                                    VariableType::String => { string_vars += 1; format!("STR{0:>03}", string_vars) }
                                    VariableType::Integer => { int_vars += 1; format!("INT{0:>03}", int_vars) }
                                    VariableType::Boolean => { bool_vars += 1; format!("BOOL{0:>03}", bool_vars) }
                                    VariableType::Byte => { byte_vars += 1; format!("BYTE{0:>03}", byte_vars) }
                                    VariableType::Money => { money_vars += 1; format!("MONEY{0:>03}", money_vars) }
                                    VariableType::Time => { time_vars += 1; format!("TIME{0:>03}", time_vars) }
                                    VariableType::Date => { date_vars += 1; format!("DATE{0:>03}", date_vars) }
                                    _ => { generic_vars += 1; format!("VAR{0:>03}", generic_vars) }

                                };
                                match cur_var.dim {
                                    1 => prg.declarations.push(Declaration::create_variable1(var_type, cur_var.var_name.clone(), cur_var.vector_size)),
                                    2 => prg.declarations.push(Declaration::create_variable2(var_type, cur_var.var_name.clone(), cur_var.vector_size, cur_var.matrix_size)),
                                    3 => prg.declarations.push(Declaration::create_variable3(var_type, cur_var.var_name.clone(), cur_var.vector_size, cur_var.matrix_size, cur_var.cube_size)),
                                    _ => prg.declarations.push(Declaration::create_variable(var_type, cur_var.var_name.clone()))
                                }
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
        let func_name = format!("FUNC{0:>03}", self.executable.variable_declarations.get(&func).unwrap().number);
        let mut func_parameters = vec![];

        let mut j = self.executable.variable_declarations.get(&func).unwrap().first_var;
        for _ in 0..self.executable.variable_declarations.get(&func).unwrap().args {
            let var_name = format!("LOC{0:>03}", self.executable.variable_declarations.get(&j).unwrap().number);
            let var_type = TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize];
            func_parameters.push(Declaration::create_variable(var_type, var_name));
            self.executable.variable_declarations.get_mut(&j).unwrap().function_id = func;
            j += 1;
        }
        let func_type = TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize];
        prg.function_implementations.push(FunctionImplementation {
            id: func,
            declaration: Declaration::Function(func_name, func_parameters, func_type),
            block: Block::new(),
            variable_declarations: Vec::new(),
        });
    }

    fn output_proc(&mut self, prg: &mut Program, proc: i32) {
        let proc_name = format!("PROC{0:>03}", self.executable.variable_declarations.get(&proc).unwrap().number);
        let mut proc_parameters = vec![];
        let mut j = self.executable.variable_declarations.get(&proc).unwrap().first_var;
        for _ in 0..self.executable.variable_declarations.get(&proc).unwrap().args {
            let var_name = format!(" LOC{0:>03}", self.executable.variable_declarations.get(&j).unwrap().number);
            let var_type = TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize];
            proc_parameters.push(Declaration::create_variable(var_type, var_name));
            self.executable.variable_declarations.get_mut(&j).unwrap().function_id = proc;
            j += 1;
        }
        prg.procedure_implementations.push(FunctionImplementation {
            id: proc,
            declaration: Declaration::Procedure(proc_name, proc_parameters),
            block: Block::new(),
            variable_declarations: Vec::new(),
        });
    }

    fn funcin(&mut self, label: i32, func: i32) {
        self.func_used.insert(label, FuncL
        {
            func
        });
    }

    fn funcout(&mut self, prg: &mut Program, label: i32) {

        if let Some(funcl) = self.func_used.get(&label) {
            let func = funcl.func;
            self.func_flag = 0;
            self.proc_flag = 0;
            if self.executable.variable_declarations.get(&func).unwrap().variable_type == VariableType::Function {
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
        for f in &mut prg.function_implementations {
            if f.id == func {
                return Some(f);
            }
        }
        for f in &mut prg.procedure_implementations {
            if f.id == func {
                return Some(f);
            }
        }
        None
    }

    fn dump_locs(&mut self, prg: &mut Program, func: i32) {
        let mut i;
        let mx_var: i32;
        let mut j = 0;
        // StackPtr = 0;
        let cur_var = self.executable.variable_declarations.get(&func).unwrap();
        if cur_var.variable_type == VariableType::Function
        {
            i = cur_var.return_var;
            mx_var = cur_var.return_var + cur_var.total_var;
        } else {
            i = cur_var.first_var + cur_var.args;
            mx_var = cur_var.first_var + cur_var.total_var; //+1;
        }

        while i < mx_var
        {
            if self.executable.variable_declarations.get(&i).unwrap().flag == 1
            {
                self.executable.variable_declarations.get_mut(&i).unwrap().function_id = func;
                if self.symbol == 0
                {
                    let var_type = crate::tables::TYPE_NAMES[self.executable.variable_declarations.get(&i).unwrap().variable_type as usize];
                    let var_name = format!("LOC{0:>03}", self.executable.variable_declarations.get(&i).unwrap().number);
                    let cur_var = &self.executable.variable_declarations[&i];

                    let func = Decompiler::get_function_mut(prg, func).unwrap();

                    match cur_var.dim {
                        1 => func.variable_declarations.push(Declaration::create_variable1(var_type, var_name, cur_var.vector_size)),
                        2 => func.variable_declarations.push(Declaration::create_variable2(var_type, var_name, cur_var.vector_size, cur_var.matrix_size)),
                        3 => func.variable_declarations.push(Declaration::create_variable3(var_type, var_name, cur_var.vector_size, cur_var.matrix_size, cur_var.cube_size)),
                        _ => func.variable_declarations.push(Declaration::create_variable(var_type, var_name))
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

    fn labelin(&mut self, label: i32) {
        if self.label_used.contains_key(&label) {
            return;
        }
        self.label_used.insert(label, self.label_used.len() as i32 + 1);
    }

    fn labelnr(&mut self, label: i32) -> &i32 {
        match self.label_used.get(&label) {
            Some(x) => x,
            _ => &-1
        }
    }

    fn labelout(&mut self, prg: &mut Program, label: i32) {
        if let Some(x) = self.label_used.get(&label) {
            let label_stmt = Statement::Label(format!("LABEL{0:>03}", x));
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
        //  println!("push expression: '{:?}'", str);
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
            Expression::Parens(expr) => Decompiler::stripper(*expr),
            Expression::BinaryExpression(op, l, r) => Expression::BinaryExpression(op, Box::new(Decompiler::stripper(*l)), Box::new(Decompiler::stripper(*r))),
            _ => str
        }
    }

    fn popstrip(&mut self) -> Option<Expression> {
        let expr = self.pop_expr();
        expr.map(Decompiler::stripper)
    }
    fn repl_const(&mut self, expr: Expression, vars: &'static [i32], names: &'static [Constant]) -> Expression {
        match expr {
            Expression::Const(c) => {
                match c {
                    Constant::Integer(parse_result) => {
                        let mut i = 0;
                        while vars[i] != -1 && vars[i] != parse_result {
                            i += 1;
                        }
                        if vars[i] != -1 {
                            return Expression::Const(names[i].clone());
                        }
                        Expression::Const(Constant::Integer(parse_result))
                    }
                    _ => Expression::Const(c)
                }
            }
            Expression::Dim1(expr, dim1) => Expression::Dim1(Box::new(self.repl_const(*expr, vars, names)), Box::new(self.repl_const(*dim1, vars, names))),
            Expression::Dim2(expr, dim1, dim2) => Expression::Dim2(Box::new(self.repl_const(*expr, vars, names)), Box::new(self.repl_const(*dim1, vars, names)), Box::new(self.repl_const(*dim2, vars, names))),
            Expression::Dim3(expr, dim1, dim2, dim3) => Expression::Dim3(Box::new(self.repl_const(*expr, vars, names)), Box::new(self.repl_const(*dim1, vars, names)), Box::new(self.repl_const(*dim2, vars, names)), Box::new(self.repl_const(*dim3, vars, names))),
            Expression::Identifier(s) => Expression::Identifier(s),
            Expression::Parens(e) => Expression::Parens(Box::new(self.repl_const(*e, vars, names))),
            Expression::FunctionCall(n, p) => {
                let mut p2 = vec![];
                for e in p {
                    p2.push(self.repl_const(e, vars, names));
                }
                Expression::FunctionCall(n, p2)
            }
            Expression::PredefinedFunctionCall(n, p) => {
                let mut p2 = vec![];
                for e in p {
                    p2.push(self.repl_const(e, vars, names));
                }
                Expression::PredefinedFunctionCall(n, p2)
            }
            Expression::Not(e) => Expression::Not(Box::new(self.repl_const(*e, vars, names))),
            Expression::Minus(e) => Expression::Minus(Box::new(self.repl_const(*e, vars, names))),
            Expression::Plus(e) => Expression::Plus(Box::new(self.repl_const(*e, vars, names))),
            Expression::BinaryExpression(op, l, r) => Expression::BinaryExpression(op, Box::new(self.repl_const(*l, vars, names)), Box::new(self.repl_const(*r, vars, names))),
        }
    }

    fn const_name(&mut self, vars: &'static [i32], names: &'static [Constant]) -> Expression {
        let temp_exr = self.popstrip().unwrap();
        self.repl_const(temp_exr, vars, names)
    }

    fn trans_exp(&mut self, cur_expr: i32) -> Expression {
        match self.cur_stmt {
            0x00c if cur_expr != 2 => self.popstrip().unwrap(),
            0x00c => self.const_name(&crate::tables::CONSTANT_CONFERENCE_OFFSETS, &crate::tables::CONSTANT_CONFERENCE_NAMES),

            0x00d if cur_expr != 2 => self.popstrip().unwrap(),
            0x00d => self.const_name(&crate::tables::CONSTANT_CONFERENCE_OFFSETS, &crate::tables::CONSTANT_CONFERENCE_NAMES),
            
            0x00e if cur_expr != 2 => self.popstrip().unwrap(),
            0x00e => self.const_name(&crate::tables::CONSTANT_NAMES_OFFSETS, &crate::tables::CONSTANT_NAMES_DISPLAY),
            
            0x010 => {
                if cur_expr == 3 {
                    self.const_name(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if cur_expr == 4 {
                    self.const_name(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                } else {
                    self.popstrip().unwrap()
                }
            }
            
            0x011 if cur_expr == 3 => self.const_name(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES),
            0x011 if cur_expr == 4 => self.const_name(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES),
            0x011 => self.popstrip().unwrap(),

            0x012 if cur_expr == 3 => self.const_name(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES),
            0x012 if cur_expr == 4 => self.const_name(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES),
            0x012 => self.popstrip().unwrap(),
            
            0x018 => {
                self.const_name(&crate::tables::CONSTANT_LINECOUNT_OFFSETS, &crate::tables::CONSTANT_LINECOUNT_NAMES)
            }
            0x022 => {
                if cur_expr == 6 {
                    self.const_name(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                } else {
                    self.popstrip().unwrap()
                }
            }
            0x02b => {
                if cur_expr == 5 {
                    self.const_name(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                } else {
                    self.popstrip().unwrap()
                }
            }
            
            0x039 if cur_expr != 2 => self.popstrip().unwrap(),
            0x039 => self.const_name(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES),
            
            0x070 if cur_expr != 3 => self.popstrip().unwrap(),
            0x070 => self.const_name(&crate::tables::CONSTANT_SEEK_OFFSETS, &crate::tables::CONSTANT_SEEK_NAMES),
            
            0x0cd | 0x0ce if cur_expr != 1 => self.popstrip().unwrap(),
            0x0cd | 0x0ce => self.const_name(&crate::tables::CONSTANT_2_OFFSETS, &crate::tables::CONSTANT_2_NAMES),
            
            _ => {
                self.popstrip().unwrap()
            }
        }
    }

    fn varout(&mut self, var_number: i32) -> Expression
    {
        let var_nr = var_number - 1;
        if self.uvar_flag && (self.executable.version < 300 && var_nr < 0x17 || 
                              self.executable.version >= 300 && var_nr < 0x18) {
            match var_nr {
                0 => return Expression::Identifier("U_EXPERT".to_string()),
                1 => return Expression::Identifier("U_FSE".to_string()),
                2 => return Expression::Identifier("U_FSEP".to_string()),
                3 => return Expression::Identifier("U_CLS".to_string()),
                4 => return Expression::Identifier("U_EXPDATE".to_string()),
                5 => return Expression::Identifier("U_SEC".to_string()),
                6 => return Expression::Identifier("U_PAGELEN".to_string()),
                7 => return Expression::Identifier("U_EXPSEC".to_string()),
                8 => return Expression::Identifier("U_CITY".to_string()),
                9 => return Expression::Identifier("U_BDPHONE".to_string()),
                10 => return Expression::Identifier("U_HVPHONE".to_string()),
                11 => return Expression::Identifier("U_TRANS".to_string()),
                12 => return Expression::Identifier("U_CMNT1".to_string()),
                13 => return Expression::Identifier("U_CMNT2".to_string()),
                14 => return Expression::Identifier("U_PWD".to_string()),
                15 => return Expression::Identifier("U_SCROLL".to_string()),
                16 => return Expression::Identifier("U_LONGHDR".to_string()),
                17 => return Expression::Identifier("U_DEF79".to_string()),
                18 => return Expression::Identifier("U_ALIAS".to_string()),
                19 => return Expression::Identifier("U_VER".to_string()),
                20 => return Expression::Identifier("U_ADDR".to_string()),
                21 => return Expression::Identifier("U_NOTES".to_string()),
                22 => return Expression::Identifier("U_PWDEXP".to_string()),
                23 => return Expression::Identifier("U_ACCOUNT".to_string()),
                _ => return Expression::FunctionCall("????".to_string(), vec![])
            }
        }

        let cur_var = self.executable.variable_declarations.get(&var_nr).unwrap();
        if cur_var.flag == 1
        {
            match cur_var.variable_type {
                VariableType::Function => {
                    return Expression::FunctionCall(format!("FUNC{0:>03}", cur_var.number), vec![]);
                }
                VariableType::Procedure => {
                    return Expression::FunctionCall(format!("PROC{0:>03}", cur_var.number), vec![]);
                }
                _ => {
                    if self.executable.variable_declarations.get(&var_nr).unwrap().fflag == 1 {
                        return Expression::Identifier(format!("FUNC{0:>03}", cur_var.number));
                    }
                    if self.executable.variable_declarations.get(&var_nr).unwrap().lflag == 1 {
                        return Expression::Identifier(format!("LOC{0:>03}", cur_var.number));
                    }
                    return Expression::Identifier(cur_var.var_name.clone());
                }
            }
        }
        match cur_var.variable_type {
            VariableType::Boolean => {
                if cur_var.content == 0 {
                    Expression::Const(Constant::Boolean(false))
                } else {
                    Expression::Const(Constant::Boolean(true))
                }
            }
            VariableType::Integer |
            VariableType::Unsigned |
            VariableType::Byte |
            VariableType::SByte |
            VariableType::Word |
            VariableType::SWord |
            VariableType::Date |
            VariableType::EDate |
            VariableType::DDate |
            VariableType::Money |
            VariableType::Time => Expression::Const(Constant::Integer(cur_var.content as i32)),
            VariableType::BigStr | 
            VariableType::String => Expression::Const(Constant::String(cur_var.string_value.clone())),
            VariableType::Double |
            VariableType::Real => Expression::Const(Constant::Real(cur_var.content as f64)),
            _ => Expression::Const(Constant::Integer(0))
        }
    }

    fn fnktout(&mut self, func: i32) -> i32
    {
        let offset = -func as usize;
        if offset >= FUNCTION_DEFINITIONS.len() {
            println!("unknown built in function {} at {}", func, self.src_ptr);
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
                    FuncOpCode::NOT => self.push_expr(Expression::Not(Box::new(tmp))),
                    FuncOpCode::UMINUS => self.push_expr(Expression::Minus(Box::new(tmp))),
                    FuncOpCode::UPLUS => self.push_expr(Expression::Plus(Box::new(tmp))),
                    _ => panic!("{}", format!("unknown unary function {}", func))
                }
                return 0;
            }
            0x11 => {
                if self.exp_count < 2 {
                    return -1;
                }
                let r_value = self.pop_expr().unwrap();
                let l_value = self.pop_expr().unwrap();

                let binop = BIN_EXPR[offset];

                self.push_expr(Expression::Parens(
                    Box::new(Expression::BinaryExpression(binop, Box::new(l_value), Box::new(r_value)))
                ));

                return 0;
            }
            _ => {}
        }

        if self.exp_count < i32::from(func_def.args) {
            println!("too few argument for built in function {} should be {} was {} at {}", func_def.name, func_def.args, self.exp_count, self.src_ptr);
            return -1;
        }
        let mut parameters = Vec::new();

        for _ in 0..func_def.args {
            parameters.push(self.pop_expr().unwrap());
        }
        parameters.reverse();
        self.push_expr(Expression::PredefinedFunctionCall(func_def, parameters));
        0
    }

    fn dimexpr(&mut self, dims: i32) -> i32
    {
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

            while (self.src_ptr as usize) < self.executable.source_buffer.len() && self.executable.source_buffer[self.src_ptr as usize] != 0 {
                let curvar = self.executable.source_buffer[self.src_ptr as usize];
                if curvar >= 0 && curvar <= self.executable.max_var {
                    let var_idx = &(curvar - 1);
                    if self.executable.variable_declarations.get(var_idx).unwrap().variable_type == VariableType::Function {
                        self.pushlabel(self.executable.variable_declarations.get(var_idx).unwrap().start);
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize - 1])).unwrap().flag = 1;
                        self.funcin(self.executable.variable_declarations.get(var_idx).unwrap().start, *var_idx);
                        if self.pass == 1
                        {
                            let temp_str2 = self.pop_expr().unwrap();
                            let tmp = self.pop_expr();
                            if let Some(e) = tmp {
                                match e {
                                    Expression::Dim1(expr, vec_expr) => self.push_expr(Expression::Dim2(expr, vec_expr, Box::new(temp_str2))),
                                    Expression::Dim2(expr, vec_expr, mat_expr) => self.push_expr(Expression::Dim3(expr, vec_expr, mat_expr, Box::new(temp_str2))),
                                    _ => self.push_expr(Expression::Dim1(Box::new(e), Box::new(temp_str2))),
                                }
                            } else {
                                self.push_expr(temp_str2);
                            }
                        }
                        self.src_ptr += 1;
                        if !self.parse_expr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().args, 1) {
                            return 1;
                        }
                        self.src_ptr -= 1;
                    } else {
                        if self.pass == 1 {
                            let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                            self.push_expr(tmp);
                        }
                        self.src_ptr += 1;
                        if self.executable.source_buffer[self.src_ptr as usize] != 0
                        {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().flag = 1;
                            if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize]) != 0 {
                                return 1;
                            }
                        }
                    }
                } else {
                    let x = self.executable.source_buffer[self.src_ptr as usize];

                    if self.executable.source_buffer[self.src_ptr as usize] < LAST_FUNC {
                        println!("Error: Unknown function {} at {} avoiding...", self.executable.source_buffer[self.src_ptr as usize], self.src_ptr);
                        return 1;
                    }
                    if self.pass == 1 {
                        if self.fnktout(x) != 0 {
                            tmp_func = self.executable.source_buffer[self.src_ptr as usize];
                            self.trash_flag = 1;
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
                    match e {
                        Expression::Dim1(expr, vec_expr) => self.push_expr(Expression::Dim2(expr, vec_expr, Box::new(temp_str2))),
                        Expression::Dim2(expr, vec_expr, mat_expr) => self.push_expr(Expression::Dim3(expr, vec_expr, mat_expr, Box::new(temp_str2))),
                        _ => {
                            if e.to_string() == "VAR001"  {
                                println!("cur {} dims: {}", cur_dim, dims);
                                //panic!();
                            }

                            self.push_expr(Expression::Dim1(Box::new(e), Box::new(temp_str2)));
                        }
                    }
                } else {
                    self.push_expr(temp_str2);
                }
            }
        }

        self.exp_count = temp_expr;
        0
    }

    fn parse_expr(&mut self, max_expr: i32, _rec: i32) -> bool
    {
        let mut cur_expr = 0;
        let temp_expr = self.exp_count;

        self.src_ptr += 1;
        while (max_expr & 0x0ff) != cur_expr {
            cur_expr += 1;

            self.exp_count = 0;
            let mut tmp_func = 0;

            while self.executable.source_buffer[self.src_ptr as usize] != 0 {

                if self.executable.source_buffer[self.src_ptr as usize] >= 0 && self.executable.source_buffer[self.src_ptr as usize] <= self.executable.max_var {
                    if max_expr / 256 == cur_expr || max_expr / 256 == 0x0f {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                        if self.pass == 1 {
                            let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                            self.push_expr(tmp);
                        }
                        self.src_ptr += 1;
                        if self.executable.source_buffer[self.src_ptr as usize] != 0
                        {
                            if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize]) != 0 {
                                return false;
                            }
                            if self.pass == 1 {
                                let temp_str2 = self.pop_expr().unwrap();
                                let tmp3 = self.pop_expr();
                                if let Some(t) = tmp3 { self.push_expr(t) }
                                self.push_expr(temp_str2);
                            }
                        }
                    } else {
                        if self.cur_stmt == 0xa8 && ((self.executable.variable_declarations.get(&self.akt_proc).unwrap().return_var >> (cur_expr - 1)) & 1) != 0 {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                        }
                        if self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().variable_type == VariableType::Function {
                            self.pushlabel(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().start);
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            self.funcin(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().start, self.executable.source_buffer[self.src_ptr as usize] - 1);
                            let mut stack_len = 0;
                            if self.pass == 1 {
                                let tmp2 = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                                let tmp3 = self.pop_expr();
                                if let Some(t) = tmp3 { self.push_expr(t) }
                                self.push_expr(tmp2);
                                stack_len = self.expr_stack.len();
                            }

                            self.src_ptr += 1;
                            if !self.parse_expr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().args, 1) {
                                return false;
                            }
                            if self.pass == 1 {
                                let mut params = Vec::new();
                                while stack_len < self.expr_stack.len() {
                                    params.push(self.pop_expr().unwrap());
                                }

                                let func_name = self.pop_expr().unwrap();

                                match func_name {
                                    Expression::FunctionCall(name, _oldparams) => { self.push_expr(Expression::FunctionCall(name, params)); }
                                    _ => { self.push_expr(func_name); }
                                }
                            }
                        } else {
                            if self.pass == 1 {
                                let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                                self.push_expr(tmp);
                            }
                            self.src_ptr += 1;
                            if self.executable.source_buffer[self.src_ptr as usize] != 0 {
                                self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().flag = 1;
                                if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize]) != 0 {
                                    return false;
                                }
                            }
                            self.src_ptr += 1;
                        }
                    }
                } else {
                    if self.pass == 1 {
                        if self.fnktout((self.executable.source_buffer[self.src_ptr as usize]) as i32) != 0 {
                            tmp_func = self.executable.source_buffer[self.src_ptr as usize];
                            self.trash_flag = 1;
                        } else if tmp_func != 0 && self.fnktout(tmp_func) == 0 {
                            tmp_func = 0;
                        }
                    }
                    self.src_ptr += 1;
                }
            }

            self.src_ptr += 1;
            if self.pass == 1 {
                let tmp2 = self.trans_exp(cur_expr);
                let tmp3 = self.pop_expr();
                if let Some(t) = tmp3 { self.push_expr(t) }
                self.push_expr(tmp2);
            }
        }
        self.exp_count = temp_expr + 1;
        true
    }

    fn set_if_ptr(&mut self, i: i32) -> i32
    {
        let j = self.executable.source_buffer[self.src_ptr as usize] as usize / 2;
        if self.executable.source_buffer[j - 2] == 0x0007 && self.executable.source_buffer[j - 1] / 2 == i {
            self.executable.source_buffer[i as usize] = 0;
            self.executable.source_buffer[self.src_ptr as usize] / 2 - 2
        } else {
            i - 5
        }
    }

    fn outputpass2(&mut self, prg: &mut Program, if_while_stack: &mut Vec<OpCode>, stmt: Statement) {
        if if_while_stack.is_empty() {
            self.output_stmt(prg, stmt);
        } else {
            let op = if_while_stack.pop().unwrap();
            let expr = self.pop_expr().unwrap();
            match op {
                OpCode::WHILE => self.output_stmt(prg, Statement::While(Box::new(expr), Box::new(stmt))),
                OpCode::IF => self.output_stmt(prg, Statement::If(Box::new(expr), Box::new(stmt))),
                _ => {}
            }
        }
    }

    fn do_pass2(&mut self, prg: &mut Program) {
        self.cur_stmt = -1;
        self.next_label = 0;
        self.next_func = 0;
        self.src_ptr = 0;
        self.trash_flag = 0;
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
            assert!(!(self.cur_stmt > LAST_STMT || STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] == 0xaa), "Error: Unknown statement {}, aborting???", self.cur_stmt);

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
                    self.push_expr(Expression::Identifier(format!("LABEL{0:>03}", tmp)));
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
                    self.outputpass2(prg, &mut if_while_stack,  Statement::End);
                    self.src_ptr += 1;
                }
                OpCode::RETURN => {
                    self.outputpass2(prg, &mut if_while_stack,  Statement::Return);
                    self.src_ptr += 1;
                }
                OpCode::STOP => {
                    self.outputpass2(prg, &mut if_while_stack,  Statement::Stop);
                    self.src_ptr += 1;
                }
                OpCode::GOTO => {
                    let label = self.pop_expr().unwrap().to_string();
                    self.outputpass2(prg, &mut if_while_stack,  Statement::Goto(label));
                }
                OpCode::GOSUB => {
                    let label = self.pop_expr().unwrap().to_string();
                    self.outputpass2(prg, &mut if_while_stack,  Statement::Gosub(label));
                }
                OpCode::LET => {
                    let value = self.pop_expr().unwrap();
                    let variable = self.pop_expr().unwrap();
                    self.outputpass2(prg, &mut if_while_stack,  Statement::Let(Box::new(variable), Box::new(value)));
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
                OpCode::INC => {
                    let variable = self.pop_expr().unwrap();
                    self.outputpass2(prg, &mut if_while_stack,  Statement::Inc(variable.to_string()));
                }
                OpCode::DEC => {
                    let variable = self.pop_expr().unwrap();
                    self.outputpass2(prg, &mut if_while_stack,  Statement::Dec(variable.to_string()));
                }
                OpCode::FPCLR => {
                    // TODO?
                    //self.outputpass2(prg, &mut if_while_stack,  Statement::EndProc);
                    self.src_ptr += 1;
                }
                OpCode::FEND => {
                    // TODO?
                    //self.outputpass2(prg, &mut if_while_stack,  Statement::EndFunc);
                    self.src_ptr += 1;
                }
                OpCode::PCALL => { // PCALL
                    self.src_ptr += 2;
                    self.akt_proc = self.executable.source_buffer[self.src_ptr as usize - 1] - 1;
                    let proc_name = format!("PROC{0:>03}", self.executable.variable_declarations.get(&self.akt_proc).unwrap().number);
                    if !self.parse_expr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().args, 0) {
                        self.output_stmt(prg, Statement::ProcedureCall(proc_name, vec![]));
                        return;
                    }
                    let mut params = Vec::new();
                    while !self.expr_stack.is_empty() {
                        params.push(self.pop_expr().unwrap());
                    }

                    params.reverse();
                    self.output_stmt(prg, Statement::ProcedureCall(proc_name, params));
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
                            self.outputpass2(prg, &mut if_while_stack, Statement::Call(def, parameters));
                            found = true;
                            if def.max_args <= 0 {
                                self.src_ptr += 1;
                            }
                            break;
                        }
                    }
                    if !found {
                        println!("unknown statement opcode: {}", self.cur_stmt);
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
                    self.output_stmt(prg, Statement::Comment("---------------------------------------".to_string()));
                }
                _ => {}
            }
        }

        self.labelout(prg, self.src_ptr * 2);
    }
}

#[cfg(test)]
mod tests {
    use std::env;

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
        if j >= original.len() { return true; }
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
            println!("File: {}...", cur_entry.file_name().unwrap().to_str().unwrap());
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
                println!("'{}' not matched???", cur_entry.file_name().unwrap().to_str().unwrap());
                print!("{}", d);
                println!("-----");
                print!("{}", orig_text);
            }

            assert!(are_equal);
        }
        // println!("successful {} skipped {}", success, skipped);
    }
}