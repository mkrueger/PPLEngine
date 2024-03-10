use icy_ppe::{
    ast::{Program, Statement, VariableType, VariableValue},
    crypt::{encode_rle, encrypt},
    tables::{self, get_function_definition, OpCode},
};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilationErrorType {
    #[error("Too many declarations: {0}")]
    TooManyDeclarations(usize),

    #[error("Variable not found: {0}")]
    VariableNotFound(String),
}
pub struct CompilationError {
    pub error: CompilationErrorType,
    pub range: core::ops::Range<usize>,
}

struct VarInfo {
    pub id: usize,
    pub var_type: VariableType,
    pub dims: u8,
    pub vector_size: usize,
    pub matrix_size: usize,
    pub cube_size: usize,

    //  Bit 1 set: tell runtime mod to reinit the var
    pub flags: u8,
}

struct Variable {
    pub info: VarInfo,
    pub var_type: VariableType,
    pub value: VariableValue,
}

#[derive(Debug)]
struct Function {
    pub args: i32,
    pub total_var: i32,
    pub start: i32,
    pub first_var: i32,
    pub return_var: i32,

    pub usages: Vec<usize>,
}
impl Function {
    fn add_usage(&mut self, len: usize) {
        self.usages.push(len);
    }
}

impl Variable {
    fn create_var_header(&self, exe: &Executable, version: u16) -> Vec<u8> {
        let mut buffer = Vec::new();
        buffer.extend(u16::to_le_bytes(self.info.id as u16));

        buffer.push(self.info.dims);

        buffer.extend(u16::to_le_bytes(self.info.vector_size as u16));
        buffer.extend(u16::to_le_bytes(self.info.matrix_size as u16));
        buffer.extend(u16::to_le_bytes(self.info.cube_size as u16));

        buffer.push(self.info.var_type as u8);
        buffer.push(self.info.flags);
        encrypt(&mut buffer, version);
        if self.var_type == VariableType::Procedure || self.var_type == VariableType::Function {
            let VariableValue::String(s) = &self.value else {
                panic!("Invalid value type {:?}", self.value);
            };
            let proc = exe.procedure_declarations.get(s).unwrap();

            buffer.push(0);
            buffer.push(0);
            buffer.push(0);
            buffer.push(0);

            buffer.push(proc.args as u8);
            buffer.push(proc.total_var as u8);
            buffer.extend(u16::to_le_bytes(proc.start as u16));
            buffer.extend(u16::to_le_bytes(proc.first_var as u16));
            buffer.extend(u16::to_le_bytes(proc.return_var as u16));
        } else if self.var_type == VariableType::String {
            let s = if self.info.dims == 0 {
                let VariableValue::String(s) = &self.value else {
                    panic!("Invalid value type {:?}", self.value);
                };
                if s.len() > u16::MAX as usize {
                    panic!("String too long");
                }
                s.clone()
            } else {
                String::new()
            };
            let mut string_buffer: Vec<u8> = Vec::new();

            for c in s.chars() {
                if let Some(b) = icy_ppe::tables::UNICODE_TO_CP437.get(&c) {
                    string_buffer.push(*b);
                } else {
                    string_buffer.push(c as u8);
                }
            }
            string_buffer.push(0);

            buffer.extend_from_slice(&u16::to_le_bytes(string_buffer.len() as u16));
            encrypt(&mut string_buffer, version);
            buffer.extend(string_buffer);
        } else {
            // VTABLE - get's ignored by PCBoard - pure garbage
            buffer.push(0);
            buffer.push(0);

            // variable type
            buffer.push(self.var_type as u8);
            buffer.push(0);

            buffer.extend_from_slice(&u64::to_le_bytes(self.value.get_u64_value()));
        }
        buffer
    }
}
struct LabelInfo {
    pub address: usize,
    pub usages: Vec<usize>,
}

pub struct Executable {
    procedure_declarations: HashMap<String, Function>,

    variable_table: HashMap<String, Variable>,
    script_buffer: Vec<u16>,

    label_table: HashMap<String, LabelInfo>,
    pub errors: Vec<CompilationError>,

    cur_function: String,
    cur_function_id: i32,
}

const ENDPROC: u16 = 0x00a9;
const ENDFUNC: u16 = 0x00ab;
const END: u16 = 1;

impl Executable {
    pub fn new() -> Self {
        Self {
            variable_table: HashMap::new(),
            procedure_declarations: HashMap::new(),
            script_buffer: Vec::new(),
            label_table: HashMap::new(),
            errors: Vec::new(),
            cur_function: String::new(),
            cur_function_id: -1,
        }
    }

    fn add_variable(
        &mut self,
        var_type: VariableType,
        name: &str,
        dims: u8,
        vector_size: usize,
        matrix_size: usize,
        cube_size: usize,
    ) {
        let id = self.variable_table.len() + 1;
        self.variable_table.insert(
            name.to_ascii_uppercase().to_string(),
            Variable {
                info: VarInfo {
                    id,
                    var_type,
                    dims,
                    vector_size,
                    matrix_size,
                    cube_size,
                    flags: 0,
                },
                var_type,
                value: var_type.create_empty_value(),
            },
        );
    }

    fn add_predefined_variable(&mut self, name: &str, val: VariableValue) {
        let id = self.variable_table.len() + 1;
        self.variable_table.insert(
            name.to_ascii_uppercase().to_string(),
            Variable {
                info: VarInfo {
                    id,
                    var_type: val.get_type(),
                    dims: val.get_dimensions(),
                    vector_size: val.get_vector_size(),
                    matrix_size: val.get_matrix_size(),
                    cube_size: val.get_cube_size(),
                    flags: 0,
                },
                var_type: val.get_type(),
                value: val,
            },
        );
    }

    fn initialize_variables(&mut self) {
        self.add_predefined_variable("U_EXPERT", VariableValue::Boolean(false));
        self.add_predefined_variable("U_FSE", VariableValue::Boolean(false));
        self.add_predefined_variable("U_FSEP", VariableValue::Boolean(false));
        self.add_predefined_variable("U_CLS", VariableValue::Boolean(false));

        self.add_predefined_variable("U_EXPDATE", VariableValue::Date(0));

        self.add_predefined_variable("U_SEC", VariableValue::Integer(0));
        self.add_predefined_variable("U_PAGELEN", VariableValue::Integer(0));
        self.add_predefined_variable("U_EXPSEC", VariableValue::Integer(0));

        self.add_predefined_variable("U_CITY", VariableValue::String(String::new()));
        self.add_predefined_variable("U_BDPHONE", VariableValue::String(String::new()));
        self.add_predefined_variable("U_HVPHONE", VariableValue::String(String::new()));
        self.add_predefined_variable("U_TRANS", VariableValue::String(String::new()));
        self.add_predefined_variable("U_CMNT1", VariableValue::String(String::new()));
        self.add_predefined_variable("U_CMNT2", VariableValue::String(String::new()));
        self.add_predefined_variable("U_PWD", VariableValue::String(String::new()));

        self.add_predefined_variable("U_SCROLL", VariableValue::Boolean(false));
        self.add_predefined_variable("U_LONGHDR", VariableValue::Boolean(false));
        self.add_predefined_variable("U_DEF79", VariableValue::Boolean(false));

        self.add_predefined_variable("U_ALIAS", VariableValue::String(String::new()));
        self.add_predefined_variable("U_VER", VariableValue::String(String::new()));
        self.add_predefined_variable(
            "U_ADDR",
            VariableValue::Dim1(
                VariableType::String,
                vec![VariableValue::String(String::new()); 5],
            ),
        );
        self.add_predefined_variable(
            "U_NOTES",
            VariableValue::Dim1(
                VariableType::String,
                vec![VariableValue::String(String::new()); 4],
            ),
        );

        self.add_predefined_variable("U_PWDEXP", VariableValue::Date(0));

        self.add_predefined_variable(
            "U_ACCOUNT",
            VariableValue::Dim1(VariableType::Integer, vec![VariableValue::Integer(0); 16]),
        );

        // 3.40 variables
        self.add_predefined_variable("U_SHORTDESC", VariableValue::Boolean(false));
        self.add_predefined_variable("U_GENDER", VariableValue::String(String::new()));
        self.add_predefined_variable("U_BIRTHDATE", VariableValue::String(String::new()));
        self.add_predefined_variable("U_EMAIL", VariableValue::String(String::new()));
        self.add_predefined_variable("U_WEB", VariableValue::String(String::new()));
    }

    pub fn compile(&mut self, prg: &Program) {
        self.initialize_variables();

        for d in prg.declarations.iter() {
            match d {
                icy_ppe::ast::Declaration::Function(name, params, ret_value) => {
                    self.procedure_declarations.insert(
                        name.to_uppercase().clone(),
                        Function {
                            args: params.len() as i32,
                            total_var: 0,
                            start: 0,
                            first_var: 0,
                            return_var: *ret_value as i32,
                            usages: Vec::new(),
                        },
                    );
                }
                icy_ppe::ast::Declaration::Procedure(name, params) => {
                    self.procedure_declarations.insert(
                        name.to_uppercase().clone(),
                        Function {
                            args: params.len() as i32,
                            total_var: 0,
                            start: 0,
                            first_var: 0,
                            return_var: 0,
                            usages: Vec::new(),
                        },
                    );
                }
                icy_ppe::ast::Declaration::Variable(var_type, vars) => {
                    for v in vars {
                        self.add_variable(
                            *var_type,
                            v.get_name(),
                            v.get_dims(),
                            v.get_vector_size(),
                            v.get_matrix_size(),
                            v.get_cube_size(),
                        );
                    }
                }
            }
        }

        prg.main_block.statements.iter().for_each(|s| {
            self.compile_statement(s);
        });
        self.script_buffer.push(END);

        self.fill_labels();

        if self.script_buffer.last() != Some(&(OpCode::END as u16)) {
            self.script_buffer.push(OpCode::END as u16);
        }

        for p in &prg.procedure_implementations {
            {
                let decl = self
                    .procedure_declarations
                    .get_mut(p.declaration.get_name())
                    .unwrap();
                decl.start = self.script_buffer.len() as i32 * 2;
                let id = self.variable_table.len() as i32 + 1;
                decl.first_var = id;
                self.variable_table.insert(
                    p.declaration.get_name().to_ascii_uppercase().clone(),
                    Variable {
                        info: VarInfo {
                            id: id as usize,
                            var_type: VariableType::Procedure,
                            dims: 0,
                            vector_size: 0,
                            matrix_size: 0,
                            cube_size: 0,
                            flags: 0,
                        },
                        var_type: VariableType::Procedure,
                        value: VariableValue::String(p.declaration.get_name().clone()),
                    },
                );
            }

            p.block.statements.iter().for_each(|s| {
                self.compile_statement(s);
            });
            self.fill_labels();

            self.script_buffer.push(ENDPROC);

            {
                let decl = self
                    .procedure_declarations
                    .get_mut(p.declaration.get_name())
                    .unwrap();
                decl.total_var = self.variable_table.len() as i32 - decl.first_var;
            }
        }

        for p in &prg.function_implementations {
            {
                let decl = self
                    .procedure_declarations
                    .get_mut(p.declaration.get_name())
                    .unwrap();
                decl.start = self.script_buffer.len() as i32 * 2;
                let id = self.variable_table.len() as i32 + 1;
                decl.first_var = id;
                decl.return_var = p.declaration.get_return_vartype();

                self.variable_table.insert(
                    p.declaration.get_name().to_ascii_uppercase().clone(),
                    Variable {
                        info: VarInfo {
                            id: id as usize,
                            var_type: VariableType::Function,
                            dims: 0,
                            vector_size: 0,
                            matrix_size: 0,
                            cube_size: 0,
                            flags: 0,
                        },
                        var_type: VariableType::Function,
                        value: VariableValue::String(p.declaration.get_name().clone()),
                    },
                );
                self.cur_function = p.declaration.get_name().to_ascii_uppercase().clone();
                self.cur_function_id = id;
            }

            p.block.statements.iter().for_each(|s| {
                self.compile_statement(s);
            });
            self.cur_function_id = -1;

            self.fill_labels();
            self.script_buffer.push(ENDFUNC);

            {
                let decl = self
                    .procedure_declarations
                    .get_mut(p.declaration.get_name())
                    .unwrap();
                decl.total_var = self.variable_table.len() as i32 - decl.first_var;
            }
        }
        if self.script_buffer.last() != Some(&(OpCode::END as u16)) {
            self.script_buffer.push(OpCode::END as u16);
        }

        for decl in &self.procedure_declarations {
            for idx in &decl.1.usages {
                self.script_buffer[*idx] = decl.1.first_var as u16;
            }
        }
    }

    fn compile_statement(&mut self, s: &Statement) {
        match s {
            Statement::Call(smt, pars) => {
                let op_code = smt.opcode as u8;
                self.script_buffer.push(op_code as u16);
                if (pars.len() as i8) < smt.min_args || (pars.len() as i8) > smt.max_args {
                    panic!("Invalid number of parameters for {}", smt.name);
                }
                if smt.min_args != smt.max_args {
                    self.script_buffer.push(pars.len() as u16);
                }
                for expr in pars {
                    let expr_buffer = self.compile_expression(expr);
                    self.script_buffer.extend(expr_buffer);
                }
            }

            Statement::End => {
                self.script_buffer.push(OpCode::END as u16);
            }
            Statement::Comment(_) => {
                // ignore
            }
            Statement::Block(block) => {
                block.iter().for_each(|s| self.compile_statement(s));
            }
            Statement::If(cond, stmt) => {
                self.script_buffer.push(OpCode::IF as u16);

                let cond_buffer = self.compile_expression(cond);
                self.script_buffer.extend(cond_buffer);

                // MAGIC?
                self.script_buffer.push(26);

                self.compile_statement(stmt);
            }
            Statement::While(cond, stmt) => {
                self.script_buffer.push(OpCode::WHILE as u16);

                let cond_buffer = self.compile_expression(cond);
                self.script_buffer.extend(cond_buffer);

                // MAGIC?
                self.script_buffer.push(30);

                self.compile_statement(stmt);
            }
            Statement::Return => {
                self.script_buffer.push(OpCode::RETURN as u16);
            }
            Statement::Let(var, expr) => {
                self.script_buffer.push(OpCode::LET as u16);
                let var_name = var.get_name().to_ascii_uppercase();

                if self.cur_function_id >= 0 && self.cur_function == var_name {
                    self.script_buffer.push(self.cur_function_id as u16);
                    self.script_buffer.push(0);
                } else {
                    let Some(decl) = self.variable_table.get(&var_name) else {
                        self.errors.push(CompilationError {
                            error: CompilationErrorType::VariableNotFound(var.get_name().clone()),
                            range: 0..0, // TODO :Range
                        });
                        return;
                    };
                    self.script_buffer.push(decl.info.id as u16);

                    match &**var {
                        icy_ppe::ast::VarInfo::Var0(_) => {
                            self.script_buffer.push(0);
                        }
                        icy_ppe::ast::VarInfo::Var1(_, expr) => {
                            self.script_buffer.push(1);
                            let expr_buffer = self.compile_expression(expr);
                            self.script_buffer.extend(expr_buffer);
                        }
                        icy_ppe::ast::VarInfo::Var2(_, expr1, expr2) => {
                            self.script_buffer.push(2);
                            let expr_buffer = self.compile_expression(expr1);
                            self.script_buffer.extend(expr_buffer);
                            let expr_buffer = self.compile_expression(expr2);
                            self.script_buffer.extend(expr_buffer);
                        }
                        icy_ppe::ast::VarInfo::Var3(_, expr1, expr2, expr3) => {
                            self.script_buffer.push(3);
                            let expr_buffer = self.compile_expression(expr1);
                            self.script_buffer.extend(expr_buffer);
                            let expr_buffer: Vec<u16> = self.compile_expression(expr2);
                            self.script_buffer.extend(expr_buffer);
                            let expr_buffer = self.compile_expression(expr3);
                            self.script_buffer.extend(expr_buffer);
                        }
                    }
                }

                let expr_buffer = self.compile_expression(expr);
                self.script_buffer.extend(expr_buffer);
            }
            Statement::Gosub(label) => {
                self.script_buffer.push(OpCode::GOSUB as u16);
                self.add_label_usage(label, self.script_buffer.len() * 2);
                self.script_buffer.push(0);
            }
            Statement::Goto(label) => {
                self.script_buffer.push(OpCode::GOTO as u16);
                self.add_label_usage(label, self.script_buffer.len() * 2);
                self.script_buffer.push(0);
            }
            Statement::Label(label) => {
                self.add_label_address(label, self.script_buffer.len() * 2);
            }

            Statement::ProcedureCall(name, parameters) => {
                self.script_buffer.push(OpCode::PCALL as u16);

                // will be filled later.
                self.procedure_declarations
                    .get_mut(name)
                    .unwrap()
                    .add_usage(self.script_buffer.len());
                self.script_buffer.push(0);

                for p in parameters {
                    let expr_buffer = self.compile_expression(p);
                    self.script_buffer.extend(expr_buffer);
                }
                self.script_buffer.push(0);
            }

            Statement::Continue => panic!("Continue not allowed in output AST."),
            Statement::Break => panic!("Break not allowed in output AST."),
            Statement::IfThen(_, _, _, _) => panic!("if then not allowed in output AST."),
            Statement::DoWhile(_, _) => panic!("do while not allowed in output AST."),
            Statement::For(_, _, _, _, _) => panic!("for not allowed in output AST."),
            Statement::Select(_, _, _) => panic!("select not allowed in output AST."),
        }
    }

    pub fn create_binary(&self, version: u16) -> Result<Vec<u8>, CompilationErrorType> {
        let mut buffer = Vec::new();
        buffer.extend_from_slice(b"PCBoard Programming Language Executable  ");
        buffer.push(b'0' + (version / 100) as u8);
        buffer.push(b'.');
        let minor = version % 100;
        buffer.push(b'0' + (minor / 10) as u8);
        buffer.push(b'0' + (minor % 10) as u8);
        buffer.extend_from_slice(b"\x0D\x0A\x1A");

        if self.variable_table.len() > 65535 {
            return Err(CompilationErrorType::TooManyDeclarations(
                self.variable_table.len(),
            ));
        }
        buffer.extend_from_slice(&u16::to_le_bytes(self.variable_table.len() as u16));

        let mut vars: Vec<&Variable> = self.variable_table.iter().map(|s| s.1).collect();
        vars.sort_by(|a, b| b.info.id.cmp(&a.info.id));

        for d in &vars {
            let var = d.create_var_header(self, version);
            buffer.extend(var);
        }
        let mut script_buffer = Vec::new();
        for s in &self.script_buffer {
            script_buffer.extend_from_slice(&s.to_le_bytes());
        }
        let mut code_data = encode_rle(&script_buffer);

        buffer.extend_from_slice(&u16::to_le_bytes(self.script_buffer.len() as u16 * 2));
        // in the very unlikely case the rle compressed buffer is larger than the original buffer
        let use_rle = code_data.len() > script_buffer.len() || version < 300;
        if use_rle {
            code_data = script_buffer;
        }

        let mut offset = 0;
        while offset < code_data.len() {
            let chunk_size = 2027;
            let add_byte = use_rle
                && offset + chunk_size < code_data.len()
                && code_data[offset + chunk_size] == 0;

            let end = (offset + chunk_size).min(code_data.len());
            let chunk = &mut code_data[offset..end];

            offset += chunk_size;

            encrypt(chunk, version);
            buffer.extend_from_slice(chunk);

            if add_byte {
                buffer.push(code_data[end]);
                offset += 1;
            }
        }
        Ok(buffer)
    }

    fn compile_expression(&mut self, expr: &icy_ppe::ast::Expression) -> Vec<u16> {
        let mut stack = Vec::new();
        self.comp_expr(&mut stack, expr);
        stack.push(0); // push end of expression
        stack
    }

    fn comp_expr(&mut self, stack: &mut Vec<u16>, expr: &icy_ppe::ast::Expression) {
        match expr {
            icy_ppe::ast::Expression::Identifier(id) => {
                if let Some(decl) = self
                    .variable_table
                    .get(&id.get_identifier().to_ascii_uppercase())
                {
                    stack.push(decl.info.id as u16);
                    stack.push(0);
                } else {
                    self.errors.push(CompilationError {
                        error: CompilationErrorType::VariableNotFound(id.get_identifier().clone()),
                        range: id.get_identifier_token().span.clone(),
                    });
                }
            }
            icy_ppe::ast::Expression::Const(constant) => {
                stack.push(self.lookup_constant(constant.get_constant_value()));
                stack.push(0);
            }
            icy_ppe::ast::Expression::Parens(expr) => {
                self.comp_expr(stack, expr.get_expression());
            }
            icy_ppe::ast::Expression::FunctionCall(expr) => {
                let predef = get_function_definition(expr.get_identifier());
                // TODO: Check parameter signature
                if predef >= 0 {
                    for p in expr.get_arguments() {
                        let mut stack2 = Vec::new();
                        self.comp_expr(&mut stack2, p);
                        stack.extend(stack2);
                    }
                    stack.push(tables::FUNCTION_DEFINITIONS[predef as usize].opcode as u16);
                    return;
                }

                if self
                    .procedure_declarations
                    .contains_key(&expr.get_identifier().to_uppercase())
                {
                    for p in expr.get_arguments() {
                        let expr_buffer = self.compile_expression(p);
                        stack.extend(expr_buffer);
                    }
                    if let Some(decl) = self
                        .procedure_declarations
                        .get_mut(&expr.get_identifier().to_uppercase())
                    {
                        // will be filled later.
                        decl.add_usage(self.script_buffer.len());
                        stack.push(0);
                    }
                } else {
                    if let Some(var) = self
                        .variable_table
                        .get(&expr.get_identifier().to_uppercase())
                    {
                        stack.push(var.info.id as u16);
                        stack.push(var.info.dims as u16);
                    } else {
                        self.errors.push(CompilationError {
                            error: CompilationErrorType::VariableNotFound(
                                expr.get_identifier().clone(),
                            ),
                            range: 0..0, // TODO :Range
                        });
                    }

                    for p in expr.get_arguments() {
                        let expr_buffer = self.compile_expression(p);
                        stack.extend(expr_buffer);
                    }
                }
            }

            icy_ppe::ast::Expression::Unary(expr) => {
                self.comp_expr(stack, expr.get_expression());
                stack.push(expr.get_op() as u16);
            }
            icy_ppe::ast::Expression::Binary(expr) => {
                self.comp_expr(stack, expr.get_left_expression());
                self.comp_expr(stack, expr.get_right_expression());
                stack.push(expr.get_op() as u16);
            }
        }
    }

    fn lookup_constant(&mut self, constant: &icy_ppe::ast::Constant) -> u16 {
        let id = self.variable_table.len() as u16 + 1;
        self.variable_table.insert(
            self.variable_table.len().to_string(),
            Variable {
                info: VarInfo {
                    id: id as usize,
                    var_type: constant.get_var_type(),
                    dims: 0,
                    vector_size: 0,
                    matrix_size: 0,
                    cube_size: 0,
                    flags: 0,
                },
                var_type: constant.get_var_type(),
                value: constant.get_value(),
            },
        );
        id
    }

    fn add_label_address(&mut self, label: &str, len: usize) {
        self.get_label_info(label).address = len;
    }

    fn add_label_usage(&mut self, label: &str, len: usize) {
        self.get_label_info(label).usages.push(len);
    }

    fn get_label_info(&mut self, label: &str) -> &mut LabelInfo {
        let label = label.to_ascii_uppercase();
        if !self.label_table.contains_key(&label) {
            self.label_table.insert(
                label.to_ascii_uppercase().clone(),
                LabelInfo {
                    address: 0,
                    usages: Vec::new(),
                },
            );
        }
        self.label_table.get_mut(&label).unwrap()
    }

    fn fill_labels(&mut self) {
        for info in self.label_table.values() {
            for usage in &info.usages {
                self.script_buffer[*usage / 2] = info.address as u16;
            }
        }
        self.label_table.clear();
    }
}
