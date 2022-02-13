use std::collections::HashMap;
use std::intrinsics::transmute;
use crate::executable::*;
use crate::interpreter::{Block, FunctionDeclaration, Program};
use crate::parser::{BinOp, Constant, Declaration, Expression, Statement};
use crate::tables::*;

const LAST_FUNC: i32 = -286;
const LAST_STMT: i32 = 0x00e2;

struct FuncL
{
    label: i32,
    func: i32,
}

pub struct Decompiler
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
    symbol: i32,
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
        for _ in 0..executable.code_size / 2 + 1 {
            valid_buffer.push(false);
        }
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

    pub fn read(file_name: &str) -> Program {
        let mut prg = Program::new();
        let mut d = Decompiler::new(read_file(file_name));

        /*        d.output_stmt(&mut prg, Statement::Comment("---------------------------------------".to_string()));
                d.output_stmt(&mut prg, Statement::Comment("PCBoard programming language decompiler".to_string()));
                d.output_stmt(&mut prg, Statement::Comment("---------------------------------------".to_string()));*/

        println!("Pass 1 ...");
        d.do_pass1();
        println!();
        d.dump_vars(&mut prg);
        println!("Pass 2 ...");
        d.do_pass2(&mut prg);
        println!("Pass 3 ...");
        Decompiler::do_pass3(&mut prg);
        println!();
        println!("Source decompilation complete...");

        let trash_flag = d.trash_flag;
        if trash_flag != 0 {
            d.output_stmt(&mut prg, Statement::Comment("---------------------------------------".to_string()));
            d.output_stmt(&mut prg, Statement::Comment(format!("!!! {} ERROR(S) CAUSED BY PPLC BUGS DETECTED", trash_flag)));
            d.output_stmt(&mut prg, Statement::Comment("PROBLEM: These expressions most probably looked like !0+!0+!0 or similar".to_string()));
            d.output_stmt(&mut prg, Statement::Comment("         before PPLC fucked it up to something like !(!(+0)+0)!0. This may".to_string()));
            d.output_stmt(&mut prg, Statement::Comment("         also apply to - / and * aswell. Also occurs upon use of multiple !'s.".to_string()));
            d.output_stmt(&mut prg, Statement::Comment("         Some smartbrains use this BUG to make programms running different".to_string()));
            d.output_stmt(&mut prg, Statement::Comment("         (or not at all) when being de- and recompiled.".to_string()));
            println!("{} COMPILER ERROR(S) DETECTED", trash_flag);
        }

        prg
    }

    pub fn output_stmt(&mut self, prg: &mut Program, stmt: Statement)
    {
        if self.func_flag > 0 {
            for decl in &mut prg.function_declarations {
                if decl.id == self.func_flag {
                    decl.block.statements.push(stmt);
                    return;
                }
            }
        }
        if self.proc_flag > 0 {
            for decl in &mut prg.procedure_declarations {
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
                if self.cur_stmt > LAST_STMT || (self.cur_stmt as usize) < STATEMENT_SIGNATURE_TABLE.len() && STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] == 0xaa {
                    panic!("Error: Unknown statement {} at {}.", self.cur_stmt, self.src_ptr);
                }
                if self.cur_stmt < 0 || self.cur_stmt > LAST_STMT {
                    panic!("opCode needs to be 0..226 at {} was {}", self.src_ptr, self.cur_stmt);
                }
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
                        trap = self.parse_expr(act_dec_args, 0) != 0;
                    }
                    0xf7 => {
                        if self.parse_expr(0x01, 0) != 0 {
                            trap = true;
                        } else {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            trap = self.parse_expr(0x01, 0) != 0;
                        }
                    }
                    0xf8 => {
                        if self.parse_expr(0x03, 0) != 0 {
                            trap = true;
                        } else {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            self.src_ptr += 1;
                        }
                    }
                    0xf9 => {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 1] - 1)).unwrap().flag = 1;
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 2] - 1)).unwrap().flag = 1;
                        self.src_ptr += 3;
                    }
                    0xfe => {
                        self.src_ptr += 1;
                        trap = self.parse_expr(self.executable.source_buffer[self.src_ptr as usize], 0) != 0;
                    }
                    0xfa => {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 2] - 1)).unwrap().flag = 1;
                        self.src_ptr += 2;
                        trap = self.parse_expr(self.executable.source_buffer[self.src_ptr as usize - 1] - 1, 0) != 0;
                    }
                    0xfc => {
                        self.src_ptr += 1;
                        trap = self.parse_expr(self.executable.source_buffer[self.src_ptr as usize] | 0xf00, 0) != 0;
                    }
                    0xfd => {
                        self.src_ptr += 1;
                        self.labelin(self.executable.source_buffer[self.src_ptr as usize]);
                        self.src_ptr += 1;
                    }
                    0xff => {
                        if_ptr = self.src_ptr;
                        if self.parse_expr(0x01, 0) != 0 {
                            trap = true;
                        } else {
                            if_ptr = self.set_if_ptr(if_ptr);
                            self.src_ptr += 1;
                        }
                    }
                    _ => {
                        trap = self.parse_expr((STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0x0f), 0) != 0;
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
                        0x0029 => {
                            self.fill_valid(last_point, self.src_ptr); // GOSUB
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
                        0x000b => {
                            self.fill_valid(last_point, self.src_ptr); // IF
                            last_point = self.src_ptr;
                            self.pushlabel(self.executable.source_buffer[self.src_ptr as usize - 1]);
                            continue;
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

        if self.uvar_flag && self.executable.version >= 300 {
            if !(self.executable.variable_declarations[&23].variable_type == VariableType::Integer && self.executable.variable_declarations[&23].vector_size == 16) {
                self.uvar_flag = false;
            }
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
            if i > 0x16 || !self.uvar_flag {
                if self.executable.variable_declarations.get_mut(&i).unwrap().lflag == 0 {
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
                                        1 => prg.variable_declarations.push(Declaration::Variable1(var_type, cur_var.var_name.clone(), cur_var.vector_size)),
                                        2 => prg.variable_declarations.push(Declaration::Variable2(var_type, cur_var.var_name.clone(), cur_var.vector_size, cur_var.matrix_size)),
                                        3 => prg.variable_declarations.push(Declaration::Variable3(var_type, cur_var.var_name.clone(), cur_var.vector_size, cur_var.matrix_size, cur_var.cube_size)),
                                        _ => prg.variable_declarations.push(Declaration::Variable(var_type, cur_var.var_name.clone()))
                                    }
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
            func_parameters.push(Declaration::Variable(var_type, var_name));
            self.executable.variable_declarations.get_mut(&j).unwrap().function_id = func;
            j += 1;
        }
        let func_type = TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize];
        prg.function_declarations.push(Box::new(FunctionDeclaration {
            id: func,
            declaration: Declaration::Function(func_name, func_parameters, func_type),
            block: Block::new(),
            variable_declarations: Vec::new(),
        }));
    }

    fn output_proc(&mut self, prg: &mut Program, proc: i32) {
        let proc_name = format!("PROC{0:>03}", self.executable.variable_declarations.get(&proc).unwrap().number);
        let mut proc_parameters = vec![];
        let mut j = self.executable.variable_declarations.get(&proc).unwrap().first_var;
        for _ in 0..self.executable.variable_declarations.get(&proc).unwrap().args {
            let var_name = format!(" LOC{0:>03}", self.executable.variable_declarations.get(&j).unwrap().number);
            let var_type = TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize];
            proc_parameters.push(Declaration::Variable(var_type, var_name));
            self.executable.variable_declarations.get_mut(&j).unwrap().function_id = proc;
            j += 1;
        }
        prg.procedure_declarations.push(Box::new(FunctionDeclaration {
            id: proc,
            declaration: Declaration::Procedure(proc_name, proc_parameters),
            block: Block::new(),
            variable_declarations: Vec::new(),
        }));
    }

    fn funcin(&mut self, label: i32, func: i32) {
        self.func_used.insert(label, FuncL
        {
            label,
            func
        });
    }

    fn funcout(&mut self, prg: &mut Program, label: i32) {
        match self.func_used.get(&label) {
            Some(funcl) => {
                let func = funcl.func;
                let label = funcl.label;
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
            _ => {}
        }
    }
    fn get_function<'a>(&self, prg: &'a mut Program, func: i32) -> Option<&'a mut FunctionDeclaration> {
        for f in &mut prg.function_declarations {
            if f.id == func {
                return Some(&mut **f);
            }
        }
        for f in &mut prg.procedure_declarations {
            if f.id == func {
                return Some(&mut **f);
            }
        }
        return None
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

                    let func = self.get_function(prg, func).unwrap();

                    match cur_var.dim {
                        1 => func.variable_declarations.push(Declaration::Variable1(var_type, var_name, cur_var.vector_size)),
                        2 => func.variable_declarations.push(Declaration::Variable2(var_type, var_name, cur_var.vector_size, cur_var.matrix_size)),
                        3 => func.variable_declarations.push(Declaration::Variable3(var_type, var_name, cur_var.vector_size, cur_var.matrix_size, cur_var.cube_size)),
                        _ => func.variable_declarations.push(Declaration::Variable(var_type, var_name))
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
        match self.label_used.get(&label) {
            Some(x) => {
                let label_stmt = Statement::Label(format!("LABEL{0:>03}", x));
                self.output_stmt(prg, label_stmt);
            }
            _ => {}
        }
    }

    fn pushlabel(&mut self, label: i32) {
        self.label_stack.push(label);
    }

    fn poplabel(&mut self) -> i32 {
        if self.label_stack.len() == 0 {
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
        if self.expr_stack.len() == 0 {
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
        match expr {
            Some(t) => Some(Decompiler::stripper(t)),
            None => None
        }
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
                        return Expression::Const(Constant::Integer(parse_result));
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
            Expression::Not(e) => Expression::Not(Box::new(self.repl_const(*e, vars, names))),
            Expression::Minus(e) => Expression::Minus(Box::new(self.repl_const(*e, vars, names))),
            Expression::BinaryExpression(op, l, r) => Expression::BinaryExpression(op, Box::new(self.repl_const(*l, vars, names)), Box::new(self.repl_const(*r, vars, names))),
        }
    }

    fn const_name(&mut self, vars: &'static [i32], names: &'static [Constant]) -> Expression {
        let temp_exr = self.popstrip().unwrap();
        self.repl_const(temp_exr, vars, names)
    }

    fn trans_exp(&mut self, cur_expr: i32) -> Expression {
        match self.cur_stmt {
            0x00c => {
                if cur_expr != 2 {
                    self.popstrip().unwrap()
                } else {
                    self.const_name(&crate::tables::CONSTANT_CONFERENCE_OFFSETS, &crate::tables::CONSTANT_CONFERENCE_NAMES)
                }
            }
            0x00d => {
                if cur_expr != 2 {
                    self.popstrip().unwrap()
                } else {
                    self.const_name(&crate::tables::CONSTANT_CONFERENCE_OFFSETS, &crate::tables::CONSTANT_CONFERENCE_NAMES)
                }
            }
            0x00e => {
                if cur_expr != 2 {
                    self.popstrip().unwrap()
                } else {
                    self.const_name(&crate::tables::CONSTANT_NAMES_OFFSETS, &crate::tables::CONSTANT_NAMES_DISPLAY)
                }
            }
            0x010 => {
                if cur_expr == 3 {
                    self.const_name(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if cur_expr == 4 {
                    self.const_name(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                } else {
                    self.popstrip().unwrap()
                }
            }
            0x011 => {
                if cur_expr == 3 {
                    self.const_name(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if cur_expr == 4 {
                    self.const_name(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                } else {
                    self.popstrip().unwrap()
                }
            }
            0x012 => {
                if cur_expr == 3 {
                    self.const_name(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if cur_expr == 4 {
                    self.const_name(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                } else {
                    self.popstrip().unwrap()
                }
            }
            0x018 => {
                self.const_name(&crate::tables::CONSTANT_LINECOUNT_OFFSETS, &crate::tables::CONSTANT_LINECOUNT_NAMES)
            }
            0x022 => {
                if cur_expr != 6 {
                    self.popstrip().unwrap()
                } else {
                    self.const_name(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                }
            }
            0x02b => {
                if cur_expr != 5 {
                    self.popstrip().unwrap()
                } else {
                    self.const_name(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                }
            }
            0x039 => {
                if cur_expr != 2 {
                    self.popstrip().unwrap()
                } else {
                    self.const_name(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                }
            }
            0x070 => {
                if cur_expr != 3 {
                    self.popstrip().unwrap()
                } else {
                    self.const_name(&crate::tables::CONSTANT_SEEK_OFFSETS, &crate::tables::CONSTANT_SEEK_NAMES)
                }
            }
            0x0cd | 0x0ce => {
                if cur_expr != 1 {
                    self.popstrip().unwrap()
                } else {
                    self.const_name(&crate::tables::CONSTANT_2_OFFSETS, &crate::tables::CONSTANT_2_NAMES)
                }
            }
            _ => {
                self.popstrip().unwrap()
            }
        }
    }

    fn varout(&mut self, var_number: i32) -> Expression
    {
        let var_nr = var_number - 1;
        if (self.executable.version < 300 && var_nr < 0x17 && self.uvar_flag) || (self.executable.version >= 300 && var_nr < 0x18 && self.uvar_flag) {
            match var_nr {
                0 => return Expression::FunctionCall("U_EXPERT".to_string(), vec![]),
                1 => return Expression::FunctionCall("U_FSE".to_string(), vec![]),
                2 => return Expression::FunctionCall("U_FSEP".to_string(), vec![]),
                3 => return Expression::FunctionCall("U_CLS".to_string(), vec![]),
                4 => return Expression::FunctionCall("U_EXPDATE".to_string(), vec![]),
                5 => return Expression::FunctionCall("U_SEC".to_string(), vec![]),
                6 => return Expression::FunctionCall("U_PAGELEN".to_string(), vec![]),
                7 => return Expression::FunctionCall("U_EXPSEC".to_string(), vec![]),
                8 => return Expression::FunctionCall("U_CITY".to_string(), vec![]),
                9 => return Expression::FunctionCall("U_BDPHONE".to_string(), vec![]),
                10 => return Expression::FunctionCall("U_HVPHONE".to_string(), vec![]),
                11 => return Expression::FunctionCall("U_TRANS".to_string(), vec![]),
                12 => return Expression::FunctionCall("U_CMNT1".to_string(), vec![]),
                13 => return Expression::FunctionCall("U_CMNT2".to_string(), vec![]),
                14 => return Expression::FunctionCall("U_PWD".to_string(), vec![]),
                15 => return Expression::FunctionCall("U_SCROLL".to_string(), vec![]),
                16 => return Expression::FunctionCall("U_LONGHDR".to_string(), vec![]),
                17 => return Expression::FunctionCall("U_DEF79".to_string(), vec![]),
                18 => return Expression::FunctionCall("U_ALIAS".to_string(), vec![]),
                19 => return Expression::FunctionCall("U_VER".to_string(), vec![]),
                20 => return Expression::FunctionCall("U_ADDR".to_string(), vec![]),
                21 => return Expression::FunctionCall("U_NOTES".to_string(), vec![]),
                22 => return Expression::FunctionCall("U_PWDEXP".to_string(), vec![]),
                23 => return Expression::FunctionCall("U_ACCOUNT".to_string(), vec![]),
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
                if cur_var.content != 0 {
                    Expression::Const(Constant::TRUE)
                } else {
                    Expression::Const(Constant::FALSE)
                }
            }
            VariableType::Date => {
                return Expression::Const(Constant::Integer(cur_var.content as i32));
            }
            VariableType::Money => {
                return Expression::Const(Constant::Money(((cur_var.content as f32) / 10.0) as f64));
            }
            VariableType::String => {
                return Expression::Const(Constant::String(cur_var.string_value.clone()));
            }
            VariableType::Time => {
                return Expression::Const(Constant::Integer(cur_var.content as i32));
            }
            VariableType::Double => {
                return Expression::Const(Constant::Double(cur_var.content as f64));
            }
            VariableType::Integer => {
                return Expression::Const(Constant::Integer(cur_var.content as i32));
            }
            _ => {
                return Expression::Const(Constant::String(cur_var.string_value.clone()));
            }
        }
    }

    fn fnktout(&mut self, func: i32) -> i32
    {
        let mut i = 0;
        if (func as usize) < FUNCTION_SIGNATURE_TABLE.len() {
            match FUNCTION_SIGNATURE_TABLE[func as usize] {
                0x10 => {
                    if self.exp_count < 1 {
                        return -1;
                    }
                    let tmp = self.pop_expr().unwrap();
                    match func {
                        15 => self.push_expr(Expression::Not(Box::new(tmp))),
                        2 => self.push_expr(Expression::Minus(Box::new(tmp))),
                        _ => panic!("{}", format!("unknown unary function {}", func))
                    }
                    return 0;
                }
                0x11 => {
                    if self.exp_count < 2 {
                        return -1;
                    }
                    let rvalue = self.pop_expr().unwrap();
                    let lvalue = self.pop_expr().unwrap();

                    let binop = BIN_EXPR[func as usize];

                    self.push_expr(Expression::Parens(
                        Box::new(Expression::BinaryExpression(binop, Box::new(lvalue), Box::new(rvalue)))
                    ));

                    return 0;
                }
                _ => {}
            }
        }

        if (func as usize) >= FUNCTION_SIGNATURE_TABLE.len() || self.exp_count < FUNCTION_SIGNATURE_TABLE[func as usize] {
            println!("unknown func {} at {}", func, self.src_ptr);
            return -1;
        }
        let func_name = if (func as usize) < EXPR_NAMES.len() { EXPR_NAMES[func as usize].to_string() } else { "_TODO_".to_string() };
        let mut parameters = Vec::new();
        while FUNCTION_SIGNATURE_TABLE[func as usize] > i {
            i += 1;
            parameters.push(self.pop_expr().unwrap());
        }
        parameters.reverse();
        self.push_expr(Expression::FunctionCall(func_name, parameters));
        return 0;
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
                                    _ => {
                                        self.push_expr(Expression::Dim1(Box::new(e), Box::new(temp_str2)))
                                    },
                                }
                            } else {
                                self.push_expr(temp_str2);
                            }
                        }
                        self.src_ptr += 1;
                        if self.parse_expr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().args, 1) != 0 {
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
                    let x = -self.executable.source_buffer[self.src_ptr as usize] - 1;

                    if self.executable.source_buffer[self.src_ptr as usize] < LAST_FUNC || (x >= 0 && FUNCTION_SIGNATURE_TABLE[x as usize] == 0xaa) {
                        println!("Error: Unknown function {} at {} avoiding...", self.executable.source_buffer[self.src_ptr as usize], self.src_ptr);
                        return 1;
                    }
                    if self.pass == 1 {
                        if self.fnktout(x) != 0 {
                            tmp_func = self.executable.source_buffer[self.src_ptr as usize];
                            self.trash_flag = 1;
                        } else if tmp_func != 0 && self.fnktout(-tmp_func - 1) == 0 {
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
        return 0;
    }

    fn parse_expr(&mut self, max_expr: i32, _rec: i32) -> i32
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
                                return 1;
                            }
                            if self.pass == 1 {
                                let temp_str2 = self.pop_expr().unwrap();
                                let tmp3 = self.pop_expr();
                                match tmp3 {
                                    Some(t) => self.push_expr(t),
                                    _ => {}
                                }
                                self.push_expr(temp_str2);
                            }
                        }
                    } else {
                        if self.cur_stmt == 0xa8
                        {
                            if ((self.executable.variable_declarations.get(&self.akt_proc).unwrap().return_var >> (cur_expr - 1)) & 1) != 0 {
                                self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            }
                        }
                        if self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().variable_type == VariableType::Function {
                            self.pushlabel(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().start);
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            self.funcin(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().start, self.executable.source_buffer[self.src_ptr as usize] - 1);
                            let mut stack_len = 0;
                            if self.pass == 1 {
                                let tmp2 = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                                let tmp3 = self.pop_expr();
                                match tmp3 {
                                    Some(t) => self.push_expr(t),
                                    _ => {}
                                }
                                self.push_expr(tmp2);
                                stack_len = self.expr_stack.len();
                            }

                            self.src_ptr += 1;
                            if self.parse_expr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().args, 1) != 0 {
                                return 1;
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
                                    return 1;
                                }
                            }
                            self.src_ptr += 1;
                        }
                    }
                } else {
                    let func_idx = (-self.executable.source_buffer[self.src_ptr as usize] - 1) as usize;
                    if func_idx >= FUNCTION_SIGNATURE_TABLE.len() || FUNCTION_SIGNATURE_TABLE[func_idx] == 0xaa {
                        println!("Error: Unknown function {:04X} avoiding...", self.executable.source_buffer[self.src_ptr as usize]);
                        return 1;
                    }

                    if self.pass == 1 {
                        if self.fnktout((-self.executable.source_buffer[self.src_ptr as usize] - 1) as i32) != 0 {
                            tmp_func = self.executable.source_buffer[self.src_ptr as usize];
                            self.trash_flag = 1;
                        } else if tmp_func != 0 && self.fnktout(-tmp_func - 1) == 0 {
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
                match tmp3 {
                    Some(t) => self.push_expr(t),
                    _ => {}
                }
                self.push_expr(tmp2);
            }
        }
        self.exp_count = temp_expr + 1;
        0
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
    /*
        fn out_simple_statement(&mut self, prg : &mut Program, stmt : Statement) {
            self.output_stmt(prg, stmt);
            self.src_ptr += 1;
        }

        fn get_expressions(&mut self) -> Vec<Expression> {
            let mut parameters = vec![];
            while self.string_stack.len() > 0 {
                parameters.push(self.popstr().unwrap());
            }
            parameters.reverse();
            parameters
        }*/

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
            if self.cur_stmt > LAST_STMT || STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] == 0xaa {
                panic!("Error: Unknown statement {}, aborting…", self.cur_stmt);
            }

            if prev_stat < 0 || STATEMENT_SIGNATURE_TABLE[prev_stat as usize] != 0xff {
                // self.output.push_str("    ");
            }

            let stack_begin = self.expr_stack.len();
            match STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] {
                0x00 => {}
                0xf6 => { // procedure
                }
                0xf7 => { // dlockg
                    if self.parse_expr(0x01, 0) != 0 {
                        return;
                    }
                    let tmp = self.varout(*self.executable.source_buffer.get(self.src_ptr as usize).unwrap());
                    self.push_expr(tmp);
                    if self.parse_expr(0x01, 0) != 0 {
                        return;
                    }
                }
                0xf8 => { // dcreate
                    if self.parse_expr(0x03, 0) != 0 {
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
                    if self.parse_expr(self.executable.source_buffer[self.src_ptr as usize], 0) != 0 {
                        return;
                    }
                }
                0xfa => { // variable expressions, expr 1 is avar
                    let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize + 2]);
                    self.push_expr(tmp);
                    self.src_ptr += 2;
                    if self.parse_expr(self.executable.source_buffer[self.src_ptr as usize - 1] - 1, 0) != 0 {
                        return;
                    }
                }
                0xfc => { // varies var
                    self.src_ptr += 1;
                    if self.parse_expr(self.executable.source_buffer[self.src_ptr as usize] | 0xf00, 0) != 0 {
                        return;
                    }
                }
                0xfd => { // label (Goto)
                    self.src_ptr += 1;
                    let tmp = *self.labelnr(self.executable.source_buffer[self.src_ptr as usize]);
                    self.push_expr(Expression::Identifier(format!("LABEL{0:>03}", tmp)));
                    self.src_ptr += 1;
                }
                0xff => {}
                _ => {
                    if self.parse_expr((STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[self.cur_stmt as usize] & 0x0f), 0) != 0 {
                        return;
                    }
                }
            }
            let op: OpCode = unsafe { transmute(self.cur_stmt as u8) };
            match op {
                OpCode::WHILE | OpCode::IF => {
                    if_ptr = self.src_ptr;
                    if self.parse_expr(0x001, 0) != 0 {
                        return;
                    }
                    if_ptr = self.set_if_ptr(if_ptr);
                    self.src_ptr += 1;
                    if_while_stack.push(op);
                }
                OpCode::PCALL => { // PCALL
                    self.src_ptr += 2;
                    self.akt_proc = self.executable.source_buffer[self.src_ptr as usize - 1] - 1;
                    let proc_name = format!("PROC{0:>03}", self.executable.variable_declarations.get(&self.akt_proc).unwrap().number);
                    if self.parse_expr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().args, 0) != 0 {
                        self.output_stmt(prg, Statement::ProcedureCall(proc_name, vec![]));
                        return;
                    }
                    let mut params = Vec::new();
                    while self.expr_stack.len() > 0 {
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

                            if !if_while_stack.is_empty() {
                                let op = if_while_stack.pop().unwrap();
                                let expr = self.pop_expr().unwrap();
                                match op {
                                    OpCode::WHILE => self.output_stmt(prg, Statement::While(Box::new(expr), Box::new(Statement::Call(&def, parameters)))),
                                    OpCode::IF => self.output_stmt(prg, Statement::If(Box::new(expr), Box::new(Statement::Call(&def, parameters)))),
                                    _ => {}
                                }
                            } else {
                                self.output_stmt(prg, Statement::Call(&def, parameters));
                            }
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

    fn do_pass3(prg: &mut Program)
    {
        Decompiler::optimize_block(&mut prg.main_block);
        for fd in &mut prg.function_declarations {
            Decompiler::optimize_block(&mut fd.block);
        }
        for pd in &mut prg.procedure_declarations {
            Decompiler::optimize_block(&mut pd.block);
        }
    }

    fn optimize_block(block: &mut Block)
    {
        Decompiler::scan_for_next(block);
        Decompiler::scan_do_while(block);
    }

    fn mach_for_construct(label: &Statement, if_statement: &Statement) -> Option<(String, String, String, Expression)> // for_label, indexName, breakout_label, to_expr
    {
        let for_label;
        let breakout_label;
        match label {
            Statement::Label(label) => for_label = label,
            _ => return None
        }
        match if_statement {
            Statement::If(expr, s) => {
                match &**s {
                    Statement::Call(def, params) => {
                        if def.opcode != OpCode::GOTO {
                            return None;
                        }
                        // todo: match expression
                        breakout_label = params[0].to_string();
                    }
                    _ => return None
                }

                if let Expression::Not(not_expr) = &**expr {
                    if let Expression::Parens(p_expr) = &**not_expr {
                        if let Expression::BinaryExpression(_op, lvalue, rvalue) = &**p_expr {
                            // TODO: Check _op
                            if let Expression::Parens(p_expr_l) = &**lvalue {
                                if let Expression::Parens(p_expr_r) = &**rvalue {
                                    if let Expression::BinaryExpression(opl, _llvalue, lrvalue) = &**p_expr_l {
                                        if let Expression::BinaryExpression(opr, _rlvalue, rrvalue) = &**p_expr_r {
                                            if *opl != BinOp::Add || *opr != BinOp::Add {
                                                return None;
                                            }

                                            if let Expression::Parens(left_binop) = &**lrvalue {
                                                if let Expression::Parens(right_binop) = &**rrvalue {
                                                    if let Expression::BinaryExpression(opl, llvalue, lrvalue) = &**left_binop {
                                                        if let Expression::BinaryExpression(opr, rlvalue, rrvalue) = &**right_binop {
                                                            if llvalue != rlvalue || *opl != BinOp::Greater || *opr != BinOp::LowerEq || lrvalue != rrvalue {
                                                                return None;
                                                            }
                                                            println!("!!!!!!!!!!! MATCH");

                                                            return Some((for_label.clone(), llvalue.to_string(), breakout_label.clone(), (**lrvalue).clone()));
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => return None
        }

        None
    }

    fn scan_for_next(block: &mut Block)
    {
        // FOR Header:
        // LET VAR001 = [START]
        // :LABEL002
        // IF (!(((1 < 0) + (VAR001 > [END])) & ((1 > 0) + (VAR001 <= [END])))) GOTO LABEL001
        // ...
        // LET VAR001 = VAR001 + [STEP]
        // GOTO LABEL002
        // :LABEL001
        let mut i = 0;
        let mut var_name;
        while i < block.statements.len() {
            let cur = &block.statements[i];
            if let Statement::Call(def, params) = cur {
                if def.opcode != OpCode::LET || params.len() != 2 || i + 3 >= block.statements.len() {
                    i += 1;
                    continue;
                }
                var_name = params[0].to_string();

                let label = &block.statements[i + 1];
                let if_statement = &block.statements[i + 2];
                let m = Decompiler::mach_for_construct(&label, &if_statement);

                if let Some((for_label, index_label, breakout_label, to_expr)) = m {
                    let mut j = i + 1;
                    let mut matching_goto = -1;
                    while j < block.statements.len() {
                        if let Statement::Call(def, params) = &block.statements[j] {
                            if def.opcode == OpCode::GOTO {
                                if let Expression::Identifier(next_label) = &params[0] {
                                    if *next_label == for_label {
                                        if j + 1 >= block.statements.len() { continue; }
                                        if let Statement::Label(next_label) = &block.statements[j + 1] {
                                            if *next_label == breakout_label {
                                                matching_goto = j as i32;
                                                break;
                                            }
                                        }
                                        i += 1;
                                        continue;
                                    }
                                }
                            }
                        }
                        j += 1;
                    }
                    if matching_goto < 0 {
                        i += 1;
                        continue;
                    }

                    let step_expr;
                    match &block.statements[matching_goto as usize - 1] {
                        Statement::Call(def, params) => {
                            if def.opcode != OpCode::LET || params.len() != 2 {
                                i += 1;
                                continue;
                            }
                            // todo: match expression as string
                            if params[0].to_string() != var_name {
                                i += 1;
                                continue;
                            }

                            if let Expression::BinaryExpression(_op, lvalue, rvalue) = &params[1] {
                                if *_op != BinOp::Add { continue; } // always add even if step is negative
                                if let Expression::Identifier(lstr) = &**lvalue {
                                    if *lstr != index_label {
                                        i += 1;
                                        continue;
                                    }
                                }
                                step_expr = (**rvalue).clone();
                            } else {
                                i += 1;
                                continue;
                            }
                        }
                        _ =>  {
                            i += 1;
                            continue;
                        }
                    }

                    let from_expr = Box::new(params[1].clone());
                    let label_cp = breakout_label.clone();
                    // replace with next
                    block.statements[matching_goto as usize] = Statement::Next;
                    block.statements.remove((matching_goto + 1) as usize); // remove breakout_label
                    block.statements.remove((matching_goto - 1) as usize); // remove LET

                    // replace for… next part
                    block.statements[i] = Statement::For(var_name, from_expr, Box::new(to_expr), Box::new(step_expr)); // replace LET
                    block.statements.remove((i + 1) as usize); // remove for_label
                    block.statements.remove((i + 1) as usize); // remove if

                    Decompiler::scan_possible_breaks(&mut block.statements, i, matching_goto as usize - 3, label_cp.as_str());

                    // there needs to be a better way to handle that
                    let mut continue_label = String::new();
                    if let Statement::Label(lbl) = &block.statements[matching_goto as usize - 3] {
                        continue_label = lbl.clone();
                    }

                    if continue_label.len() > 0 {
                        Decompiler::scan_possible_continues(&mut block.statements, i, matching_goto as usize - 3, continue_label.as_str());
                        block.statements.remove((matching_goto - 1) as usize);
                    }
                }
            }
            i += 1;
        }
    }

    fn scan_possible_breaks(block: &mut Vec<Statement>, start : usize, end : usize, break_label : &str)
    {
        for i in start..end {
            match &block[i] {
                Statement::While(cond, stmt) => {
                    if let Statement::Call(def, params) = &**stmt {
                        if params.len() != 1 { continue; }
                        if def.opcode == OpCode::GOTO && params[0].to_string() == break_label {
                            block[i] = Statement::While(Box::new((**cond).clone()), Box::new(Statement::Break));
                        }
                    }
                }
                Statement::If(cond, stmt) => {
                    if let Statement::Call(def, params) = &**stmt {
                        if params.len() != 1 { continue; }
                        if def.opcode == OpCode::GOTO && params[0].to_string() == break_label {
                            block[i] = Statement::If(Box::new((**cond).clone()), Box::new(Statement::Break));
                        }
                    }
                }
                Statement::Call(def, params) => {
                    if params.len() != 1 { continue; }
                    if def.opcode == OpCode::GOTO && params[0].to_string() == break_label {
                        block[i] = Statement::Break;
                    }
                }
                _ => {}
            }
        }
    }

    fn scan_possible_continues(block: &mut Vec<Statement>, start : usize, end : usize, continue_label : &str)
    {
        for i in start..end {
            match &block[i] {
                Statement::While(cond, stmt) => {
                    if let Statement::Call(def, params) = &**stmt {
                        if params.len() != 1 { continue; }
                        if def.opcode == OpCode::GOTO && params[0].to_string() == continue_label {
                            block[i] = Statement::While(Box::new((**cond).clone()), Box::new(Statement::Continue));
                        }
                    }
                }
                Statement::If(cond, stmt) => {
                    if let Statement::Call(def, params) = &**stmt {
                        if params.len() != 1 { continue; }
                        if def.opcode == OpCode::GOTO && params[0].to_string() == continue_label {
                            block[i] = Statement::If(Box::new((**cond).clone()), Box::new(Statement::Continue));
                        }
                    }
                }
                Statement::Call(def, params) => {
                    if params.len() != 1 { continue; }
                    if def.opcode == OpCode::GOTO && params[0].to_string() == continue_label {
                        block[i] = Statement::Continue;
                    }
                }
                _ => {}
            }
        }
    }

    fn scan_do_while(block: &mut Block)
    {
        let mut i = 0;
        while i < block.statements.len() {
            let cur = &block.statements[i];
            if let Statement::Label(label) = cur {
                i += 1;
                if i >= block.statements.len() {
                    break;
                }
                if let Statement::If(exp, stmt) = &block.statements[i] {
                    match &(**stmt) {
                        Statement::Call(def, params) => {
                            if def.opcode != OpCode::GOTO || params.len() != 1 {
                                continue;
                            }
                            if let Expression::Identifier(break_label) = &params[0] {
                                // search matching goto
                                let mut j = i + 1;
                                let mut matching_goto = -1;
                                while j < block.statements.len() {
                                    if let Statement::Call(def, params) = &block.statements[j] {
                                        if def.opcode == OpCode::GOTO {
                                            if let Expression::Identifier(next_label) = &params[0] {
                                                if next_label == label {
                                                    if j + 1 >= block.statements.len() { continue; }
                                                    if let Statement::Label(next_label) = &block.statements[j + 1] {
                                                        if next_label == break_label {
                                                            matching_goto = j as i32;
                                                            break;
                                                        }
                                                    }
                                                    j += 1;
                                                    continue;
                                                }
                                            }
                                        }
                                    }
                                    j += 1;
                                }
                                if matching_goto < 0 {
                                    i += 1;
                                    continue;
                                }
                                let label_cp = break_label.clone();
                                block.statements[i] = Statement::DoWhile(exp.clone());
                                block.statements[matching_goto as usize] = Statement::EndWhile;
                                block.statements.remove((matching_goto + 1) as usize);
                                block.statements.remove((i - 1) as usize);

                                Decompiler::scan_possible_breaks(&mut block.statements, i, matching_goto as usize, label_cp.as_str());
                                // there needs to be a better way to handle that
                                let mut continue_label = String::new();
                                if let Statement::Label(lbl) = &block.statements[matching_goto as usize - 2] {
                                    continue_label = lbl.clone();
                                }

                                if continue_label.len() > 0 {
                                    Decompiler::scan_possible_continues(&mut block.statements, i, matching_goto as usize, continue_label.as_str());
                                    block.statements.remove((matching_goto - 1) as usize);
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }

            i += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env;

    fn is_match(output: &String, original: &String) -> bool {
        let mut i = 0;
        let mut j = 0;

        let output = output.as_bytes();
        let original = original.as_bytes();

        while i < output.len() && j < original.len() {
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
            i += 1;
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
            print!("File: {}…\n", cur_entry.file_name().unwrap().to_str().unwrap());

            if cur_entry.file_name().unwrap().to_str().unwrap().eq("if_elseif_else_endif_end.ppe")
            {
                // println!("skip.");
                //skipped += 1;
                continue;
            }

            let d = crate::decompiler::Decompiler::read(file_name.to_str().unwrap());
            let source_file = cur_entry.with_extension("pps");
            let orig_text = fs::read_to_string(source_file).unwrap();

            let are_equal = is_match(&d.to_string(), &orig_text);

            if !are_equal {
                println!("'{}' not matched…", cur_entry.file_name().unwrap().to_str().unwrap());
                print!("{}", d.to_string());
                println!("-----");
                print!("{}", orig_text);
            } else {
                // println!(" match.");
                //success += 1;
            }

            assert!(are_equal);
        }
        // println!("successful {} skipped {}", success, skipped);
    }
}