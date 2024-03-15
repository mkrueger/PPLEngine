use icy_ppe::{
    ast::{Program, Statement, Variable, VariableData, VariableType, VariableValue},
    crypt::{encode_rle, encrypt},
    executable::FunctionValue,
    parser::lexer::SpannedToken,
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

    #[error("Procedure not found: {0}")]
    ProcedureNotFound(String),

    #[error("Label already defined: {0}")]
    LabelAlreadyDefined(String),

    #[error("Undefined label: {0}")]
    UndefinedLabel(String),

    #[error("Function {0} already defined.")]
    FunctionAlreadyDefined(String),

    #[error("Procedure {0} already defined.")]
    ProcedureAlreadyDefined(String),
}

pub struct CompilationError {
    pub error: CompilationErrorType,
    pub range: core::ops::Range<usize>,
}

#[derive(Error, Debug)]
pub enum CompilationWarningType {
    #[error("Unused label {0}")]
    UnusedLabel(String),
}

pub struct CompilationWarning {
    pub error: CompilationWarningType,
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

struct VariableId {
    pub info: VarInfo,
    pub var_type: VariableType,
    pub value: icy_ppe::ast::Variable,
}

#[derive(Debug)]
struct Function {
    pub id: usize,
    pub header: FunctionValue,

    pub usages: Vec<usize>,
}
impl Function {
    fn add_usage(&mut self, len: usize) {
        self.usages.push(len);
    }
}

impl VariableId {
    fn create_var_header(&self, exe: &PPEOutput, version: u16) -> Vec<u8> {
        let mut buffer = Vec::new();
        buffer.extend(u16::to_le_bytes(self.info.id as u16));
        buffer.push(self.info.dims);
        buffer.extend(u16::to_le_bytes(self.info.vector_size as u16));
        buffer.extend(u16::to_le_bytes(self.info.matrix_size as u16));
        buffer.extend(u16::to_le_bytes(self.info.cube_size as u16));

        buffer.push(self.info.var_type as u8);
        buffer.push(self.info.flags);
        encrypt(&mut buffer, version);

        let b = buffer.len();
        if self.var_type == VariableType::Procedure || self.var_type == VariableType::Function {
            let VariableValue::String(s) = &self.value.generic_data else {
                panic!("Invalid value type {:?}", self.value);
            };
            let proc = exe
                .procedure_declarations
                .get(&unicase::Ascii::new(s.clone()))
                .unwrap();

            buffer.push(0);
            buffer.push(0);
            buffer.push(self.var_type as u8);
            buffer.push(0);
            proc.header.append(&mut buffer);
            encrypt(&mut buffer[b..], version);
        } else if self.var_type == VariableType::String {
            let s = if self.info.dims == 0 {
                let VariableValue::String(s) = &self.value.generic_data else {
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
            encrypt(&mut buffer[b..], version);
        }

        buffer
    }
}
#[derive(Debug)]
struct LabelInfo {
    pub address: Option<(SpannedToken, usize)>,
    pub usages: Vec<(SpannedToken, usize)>,
}

pub struct PPEOutput {
    procedure_declarations: HashMap<unicase::Ascii<String>, Function>,

    variable_id: usize,
    variable_table: Vec<VariableId>,
    variable_lookup: HashMap<unicase::Ascii<String>, usize>,

    script_buffer: Vec<u16>,

    label_table: HashMap<unicase::Ascii<String>, LabelInfo>,
    pub errors: Vec<CompilationError>,
    pub warnings: Vec<CompilationWarning>,

    cur_function: unicase::Ascii<String>,
    cur_function_id: i32,
}

impl PPEOutput {
    pub fn new() -> Self {
        Self {
            variable_table: Vec::new(),
            variable_lookup: HashMap::new(),
            procedure_declarations: HashMap::new(),
            script_buffer: Vec::new(),
            label_table: HashMap::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            cur_function: unicase::Ascii::new(String::new()),
            cur_function_id: -1,
            variable_id: 0,
        }
    }

    fn add_variable(
        &mut self,
        var_type: VariableType,
        name: unicase::Ascii<String>,
        dims: u8,
        vector_size: usize,
        matrix_size: usize,
        cube_size: usize,
    ) {
        let id = self.next_id();
        self.variable_lookup
            .insert(name.clone(), self.variable_table.len());
        self.variable_table.push(VariableId {
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
        });
    }

    fn add_predefined_variable(&mut self, name: &str, val: Variable) {
        let id = self.next_id();
        self.variable_lookup.insert(
            unicase::Ascii::new(name.to_string()),
            self.variable_table.len(),
        );
        self.variable_table.push(VariableId {
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
        });
    }

    fn initialize_variables(&mut self) {
        self.add_predefined_variable("U_EXPERT", Variable::new_bool(false));
        self.add_predefined_variable("U_FSE", Variable::new_bool(false));
        self.add_predefined_variable("U_FSEP", Variable::new_bool(false));
        self.add_predefined_variable("U_CLS", Variable::new_bool(false));
        self.add_predefined_variable(
            "U_EXPDATE",
            Variable::new(VariableType::Date, VariableData::default()),
        );
        self.add_predefined_variable("U_SEC", Variable::new_int(0));
        self.add_predefined_variable("U_PAGELEN", Variable::new_int(0));
        self.add_predefined_variable("U_EXPSEC", Variable::new_int(0));
        self.add_predefined_variable("U_CITY", Variable::new_string(String::new()));
        self.add_predefined_variable("U_BDPHONE", Variable::new_string(String::new()));
        self.add_predefined_variable("U_HVPHONE", Variable::new_string(String::new()));
        self.add_predefined_variable("U_TRANS", Variable::new_string(String::new()));
        self.add_predefined_variable("U_CMNT1", Variable::new_string(String::new()));
        self.add_predefined_variable("U_CMNT2", Variable::new_string(String::new()));
        self.add_predefined_variable("U_PWD", Variable::new_string(String::new()));
        self.add_predefined_variable("U_SCROLL", Variable::new_bool(false));
        self.add_predefined_variable("U_LONGHDR", Variable::new_bool(false));
        self.add_predefined_variable("U_DEF79", Variable::new_bool(false));
        self.add_predefined_variable("U_ALIAS", Variable::new_string(String::new()));
        self.add_predefined_variable("U_VER", Variable::new_string(String::new()));
        self.add_predefined_variable(
            "U_ADDR",
            Variable::new_vector(
                VariableType::String,
                vec![Variable::new_string(String::new()); 5],
            ),
        );
        self.add_predefined_variable(
            "U_NOTES",
            Variable::new_vector(
                VariableType::String,
                vec![Variable::new_string(String::new()); 4],
            ),
        );

        self.add_predefined_variable(
            "U_PWDEXP",
            Variable::new(VariableType::Date, VariableData::default()),
        );

        self.add_predefined_variable(
            "U_ACCOUNT",
            Variable::new_vector(VariableType::Integer, vec![Variable::new_int(0); 16]),
        );

        // 3.40 variables
        /*
        self.add_predefined_variable("U_SHORTDESC", Variable::new_bool(false));
        self.add_predefined_variable("U_GENDER", Variable::new_string(String::new()));
        self.add_predefined_variable("U_BIRTHDATE", Variable::new_string(String::new()));
        self.add_predefined_variable("U_EMAIL", Variable::new_string(String::new()));
        self.add_predefined_variable("U_WEB", Variable::new_string(String::new()));
        */
    }

    pub fn compile(&mut self, prg: &Program, no_user_vars: bool) {
        if !no_user_vars {
            self.initialize_variables();
        }

        for d in &prg.nodes {
            match d {
                icy_ppe::ast::AstNode::Comment(_) => {}
                icy_ppe::ast::AstNode::Function(_func) => {}
                icy_ppe::ast::AstNode::Procedure(_proc) => {}

                icy_ppe::ast::AstNode::FunctionDeclaration(func) => {
                    if self
                        .procedure_declarations
                        .contains_key(func.get_identifier())
                    {
                        self.errors.push(CompilationError {
                            error: CompilationErrorType::FunctionAlreadyDefined(
                                func.get_identifier().to_string(),
                            ),
                            range: func.get_identifier_token().span.clone(),
                        });
                        return;
                    }
                    self.procedure_declarations.insert(
                        func.get_identifier().clone(),
                        Function {
                            id: 0,
                            header: FunctionValue {
                                parameters: func.get_parameters().len() as u8,
                                local_variables: 0,
                                start_offset: 0,
                                first_var_id: 0,
                                return_var: func.get_return_type() as i16,
                            },
                            usages: Vec::new(),
                        },
                    );
                }

                icy_ppe::ast::AstNode::ProcedureDeclaration(proc) => {
                    if self
                        .procedure_declarations
                        .contains_key(proc.get_identifier())
                    {
                        self.errors.push(CompilationError {
                            error: CompilationErrorType::ProcedureAlreadyDefined(
                                proc.get_identifier().to_string(),
                            ),
                            range: proc.get_identifier_token().span.clone(),
                        });
                        return;
                    }
                    self.variable_lookup
                        .insert(proc.get_identifier().clone(), self.variable_table.len());
                    let id: usize = self.next_id();
                    self.variable_table.push(VariableId {
                        info: VarInfo {
                            id,
                            var_type: VariableType::Procedure,
                            dims: 0,
                            vector_size: 0,
                            matrix_size: 0,
                            cube_size: 0,
                            flags: 0,
                        },
                        var_type: VariableType::Procedure,
                        value: Variable::new_string(proc.get_identifier().to_string()),
                    });

                    let mut var_params = 0;
                    for (i, p) in proc.get_parameters().iter().enumerate() {
                        if p.is_var() {
                            var_params |= 1 << i;
                        }
                    }
                    self.procedure_declarations.insert(
                        proc.get_identifier().clone(),
                        Function {
                            id,
                            header: FunctionValue {
                                parameters: proc.get_parameters().len() as u8,
                                local_variables: 0,
                                start_offset: 0,
                                first_var_id: 0,
                                return_var: var_params,
                            },
                            usages: Vec::new(),
                        },
                    );
                }

                icy_ppe::ast::AstNode::Statement(stmt) => {
                    self.compile_statement(stmt);
                }
            }
        }
        self.fill_labels();
        if !self.script_buffer.ends_with(&[OpCode::END as u16]) {
            self.script_buffer.push(OpCode::END as u16);
        }

        for imp in &prg.nodes {
            match imp {
                icy_ppe::ast::AstNode::Comment(_) => {}
                icy_ppe::ast::AstNode::Procedure(p) => {
                    {
                        let decl = self
                            .procedure_declarations
                            .get_mut(p.get_identifier())
                            .unwrap();
                        decl.header.start_offset = self.script_buffer.len() as u16 * 2;
                        decl.header.first_var_id = self.variable_table.len() as i16;
                    }
                    for param in p.get_parameters() {
                        let id = self.next_id();
                        self.variable_lookup.insert(
                            param.get_variable().get_identifier().clone(),
                            self.variable_table.len(),
                        );
                        self.variable_table.push(VariableId {
                            info: VarInfo {
                                id,
                                var_type: param.get_variable_type(),
                                dims: param.get_variable().get_dimensions().len() as u8,
                                vector_size: param.get_variable().get_vector_size(),
                                matrix_size: param.get_variable().get_matrix_size(),
                                cube_size: param.get_variable().get_cube_size(),
                                flags: 0,
                            },
                            var_type: param.get_variable_type(),
                            value: param.get_variable_type().create_empty_value(),
                        });
                    }

                    p.get_statements().iter().for_each(|s| {
                        self.compile_statement(s);
                    });

                    self.fill_labels();

                    self.script_buffer.push(OpCode::FPCLR as u16);
                    self.script_buffer.push(OpCode::END as u16);
                    {
                        let decl = self
                            .procedure_declarations
                            .get_mut(p.get_identifier())
                            .unwrap();

                        for i in decl.header.first_var_id as usize..self.variable_table.len() {
                            self.variable_table[i].info.flags |= 1;
                        }

                        decl.header.local_variables =
                            (self.variable_table.len() as i16 - decl.header.first_var_id) as u8;
                    }
                }
                icy_ppe::ast::AstNode::Function(p) => {
                    {
                        let id = self.next_id();
                        let decl = self
                            .procedure_declarations
                            .get_mut(p.get_identifier())
                            .unwrap();
                        decl.header.start_offset = self.script_buffer.len() as u16 * 2;
                        decl.header.first_var_id = id as i16;
                        self.variable_lookup
                            .insert(p.get_identifier().clone(), self.variable_table.len());
                        self.variable_table.push(VariableId {
                            info: VarInfo {
                                id,
                                var_type: VariableType::Function,
                                dims: 0,
                                vector_size: 0,
                                matrix_size: 0,
                                cube_size: 0,
                                flags: 0,
                            },
                            var_type: VariableType::Function,
                            value: Variable::new_string(p.get_identifier().to_string()),
                        });
                        self.cur_function = p.get_identifier().clone();
                        self.cur_function_id = id as i32;
                    }
                    for param in p.get_parameters() {
                        let id = self.next_id();
                        self.variable_lookup.insert(
                            param.get_variable().get_identifier().clone(),
                            self.variable_table.len(),
                        );
                        self.variable_table.push(VariableId {
                            info: VarInfo {
                                id,
                                var_type: param.get_variable_type(),
                                dims: param.get_variable().get_dimensions().len() as u8,
                                vector_size: param.get_variable().get_vector_size(),
                                matrix_size: param.get_variable().get_matrix_size(),
                                cube_size: param.get_variable().get_cube_size(),
                                flags: 0,
                            },
                            var_type: param.get_variable_type(),
                            value: param.get_variable_type().create_empty_value(),
                        });
                    }
                    p.get_statements().iter().for_each(|s| {
                        self.compile_statement(s);
                    });
                    self.cur_function_id = -1;
                    self.fill_labels();
                    self.script_buffer.push(OpCode::FEND as u16);
                    self.script_buffer.push(OpCode::END as u16);
                    {
                        let decl = self
                            .procedure_declarations
                            .get_mut(p.get_identifier())
                            .unwrap();
                        decl.header.local_variables =
                            (self.variable_table.len() as i16 - decl.header.first_var_id) as u8;
                    }
                }

                _ => {}
            }
        }

        if !self.script_buffer.ends_with(&[OpCode::END as u16]) {
            self.script_buffer.push(OpCode::END as u16);
        }

        for decl in &self.procedure_declarations {
            for idx in &decl.1.usages {
                self.script_buffer[*idx] = decl.1.id as u16;
            }
        }
    }

    fn compile_statement(&mut self, s: &Statement) {
        match s {
            Statement::End(_) => {
                self.script_buffer.push(OpCode::END as u16);
            }
            Statement::Comment(_) => {
                // ignore
            }
            Statement::Block(block_stmt) => {
                block_stmt
                    .get_statements()
                    .iter()
                    .for_each(|s| self.compile_statement(s));
            }
            Statement::If(if_stmt) => {
                self.script_buffer.push(OpCode::IF as u16);

                let cond_buffer = self.compile_expression(if_stmt.get_condition());
                self.script_buffer.extend(cond_buffer);

                // MAGIC?
                self.script_buffer.push(26);

                self.compile_statement(if_stmt.get_statement());
            }
            Statement::While(while_stmt) => {
                self.script_buffer.push(OpCode::WHILE as u16);

                let cond_buffer = self.compile_expression(while_stmt.get_condition());
                self.script_buffer.extend(cond_buffer);

                // MAGIC?
                self.script_buffer.push(30);

                self.compile_statement(while_stmt.get_statement());
            }
            Statement::Return(_) => {
                self.script_buffer.push(OpCode::RETURN as u16);
            }
            Statement::Let(let_smt) => {
                self.script_buffer.push(OpCode::LET as u16);
                let var_name = let_smt.get_identifier();

                if self.cur_function_id >= 0 && self.cur_function == *var_name {
                    self.script_buffer.push(self.cur_function_id as u16);
                    self.script_buffer.push(0);
                } else {
                    let Some(decl_idx) = self.variable_lookup.get(var_name) else {
                        self.errors.push(CompilationError {
                            error: CompilationErrorType::VariableNotFound(
                                let_smt.get_identifier().to_string(),
                            ),
                            range: let_smt.get_identifier_token().span.clone(),
                        });
                        return;
                    };
                    let decl = &self.variable_table[*decl_idx];
                    self.script_buffer.push(decl.info.id as u16);
                    self.script_buffer
                        .push(let_smt.get_arguments().len() as u16);
                    for arg in let_smt.get_arguments() {
                        let expr_buffer = self.compile_expression(arg);
                        self.script_buffer.extend(expr_buffer);
                    }
                }

                let expr_buffer = self.compile_expression(let_smt.get_value_expression());
                self.script_buffer.extend(expr_buffer);
            }
            Statement::Gosub(gosub_stmt) => {
                self.script_buffer.push(OpCode::GOSUB as u16);
                self.add_label_usage(
                    gosub_stmt.get_label(),
                    gosub_stmt.get_label_token().clone(),
                    self.script_buffer.len() * 2,
                );
                self.script_buffer.push(0);
            }
            Statement::Goto(goto_stmt) => {
                self.script_buffer.push(OpCode::GOTO as u16);
                self.add_label_usage(
                    goto_stmt.get_label(),
                    goto_stmt.get_label_token().clone(),
                    self.script_buffer.len() * 2,
                );
                self.script_buffer.push(0);
            }
            Statement::Label(label) => {
                self.add_label_address(
                    label.get_label(),
                    label.get_label_token().clone(),
                    self.script_buffer.len() * 2,
                );
            }
            Statement::PredifinedCall(call_stmt) => {
                let def = call_stmt.get_func();
                let op_code = def.opcode as u8;
                self.script_buffer.push(op_code as u16);
                if (call_stmt.get_arguments().len() as i8) < def.min_args
                    || (call_stmt.get_arguments().len() as i8) > def.max_args
                {
                    panic!("Invalid number of parameters for {}", def.name);
                }
                if def.min_args != def.max_args {
                    self.script_buffer
                        .push(call_stmt.get_arguments().len() as u16);
                }
                for expr in call_stmt.get_arguments() {
                    let expr_buffer = self.compile_expression(expr);
                    self.script_buffer.extend(expr_buffer);
                }
            }
            Statement::Call(call_stmt) => {
                self.script_buffer.push(OpCode::PCALL as u16);

                // will be filled later.
                if let Some(procedure) = self
                    .procedure_declarations
                    .get_mut(call_stmt.get_identifier())
                {
                    procedure.add_usage(self.script_buffer.len());
                    self.script_buffer.push(0);

                    for p in call_stmt.get_arguments() {
                        let expr_buffer = self.compile_expression(p);
                        self.script_buffer.extend(expr_buffer);
                    }
                    self.script_buffer.push(0);
                } else {
                    self.errors.push(CompilationError {
                        error: CompilationErrorType::ProcedureNotFound(
                            call_stmt.get_identifier().to_string(),
                        ),
                        range: 0..0, // TODO :Range
                    });
                }
            }
            Statement::VariableDeclaration(var_decl) => {
                for v in var_decl.get_variables() {
                    self.add_variable(
                        var_decl.get_variable_type(),
                        v.get_identifier().clone(),
                        v.get_dimensions().len() as u8,
                        v.get_vector_size(),
                        v.get_matrix_size(),
                        v.get_cube_size(),
                    );
                }
            }

            Statement::Continue(_) => panic!("Continue not allowed in output AST."),
            Statement::Break(_) => panic!("Break not allowed in output AST."),
            Statement::IfThen(_) => panic!("if then not allowed in output AST."),
            Statement::WhileDo(_) => panic!("do while not allowed in output AST."),
            Statement::For(_) => panic!("for not allowed in output AST."),
            Statement::Select(_) => panic!("select not allowed in output AST."),
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
        for d in self.variable_table.iter().rev() {
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
                if let Some(decl_idx) = self.variable_lookup.get(id.get_identifier()) {
                    let decl = &self.variable_table[*decl_idx];
                    stack.push(decl.info.id as u16);
                    stack.push(0);
                } else {
                    self.errors.push(CompilationError {
                        error: CompilationErrorType::VariableNotFound(
                            id.get_identifier().to_string(),
                        ),
                        range: id.get_identifier_token().span.clone(),
                    });
                }
            }
            icy_ppe::ast::Expression::Const(constant) => {
                stack.push(self.lookup_constant(constant.get_constant_value()) as u16);
                stack.push(0);
            }
            icy_ppe::ast::Expression::Parens(expr) => {
                self.comp_expr(stack, expr.get_expression());
            }
            icy_ppe::ast::Expression::PredefinedFunctionCall(expr) => {
                let predef = get_function_definition(expr.get_identifier());
                // TODO: Check parameter signature
                if predef >= 0 {
                    for p in expr.get_arguments() {
                        let mut stack2 = Vec::new();
                        self.comp_expr(&mut stack2, p);
                        stack.extend(stack2);
                    }
                    stack.push(tables::FUNCTION_DEFINITIONS[predef as usize].opcode as u16);
                }
            }
            icy_ppe::ast::Expression::FunctionCall(expr) => {
                if self
                    .procedure_declarations
                    .contains_key(expr.get_identifier())
                {
                    for p in expr.get_arguments() {
                        let expr_buffer = self.compile_expression(p);
                        stack.extend(expr_buffer);
                    }
                    if let Some(decl) = self.procedure_declarations.get_mut(expr.get_identifier()) {
                        // will be filled later.
                        decl.add_usage(self.script_buffer.len());
                        stack.push(0);
                    }
                } else {
                    if let Some(var_idx) = self.variable_lookup.get(expr.get_identifier()) {
                        let var = &self.variable_table[*var_idx];
                        stack.push(var.info.id as u16);
                        stack.push(var.info.dims as u16);
                    } else {
                        self.errors.push(CompilationError {
                            error: CompilationErrorType::VariableNotFound(
                                expr.get_identifier().to_string(),
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

    fn lookup_constant(&mut self, constant: &icy_ppe::ast::Constant) -> usize {
        let id = self.next_id();
        self.variable_table.push(VariableId {
            info: VarInfo {
                id,
                var_type: constant.get_var_type(),
                dims: 0,
                vector_size: 0,
                matrix_size: 0,
                cube_size: 0,
                flags: 0,
            },
            var_type: constant.get_var_type(),
            value: constant.get_value(),
        });
        id
    }

    fn add_label_address(
        &mut self,
        label: &unicase::Ascii<String>,
        token: SpannedToken,
        len: usize,
    ) {
        if self.get_label_info(label).address.is_some() {
            self.errors.push(CompilationError {
                error: CompilationErrorType::LabelAlreadyDefined(label.to_string()),
                range: token.span.clone(),
            });
            return;
        }

        self.get_label_info(label).address = Some((token, len));
    }

    fn add_label_usage(&mut self, label: &unicase::Ascii<String>, token: SpannedToken, len: usize) {
        self.get_label_info(label).usages.push((token, len));
    }

    fn get_label_info(&mut self, label: &unicase::Ascii<String>) -> &mut LabelInfo {
        if !self.label_table.contains_key(label) {
            self.label_table.insert(
                label.clone(),
                LabelInfo {
                    address: None,
                    usages: Vec::new(),
                },
            );
        }
        self.label_table.get_mut(label).unwrap()
    }

    fn fill_labels(&mut self) {
        for info in self.label_table.values() {
            let Some((address_token, address_pos)) = &info.address else {
                let label = &info.usages.first().unwrap().0;
                self.errors.push(CompilationError {
                    error: CompilationErrorType::UndefinedLabel(label.token.to_string()),
                    range: label.span.clone(),
                });
                continue;
            };

            if info.usages.is_empty() {
                self.warnings.push(CompilationWarning {
                    error: CompilationWarningType::UnusedLabel(address_token.token.to_string()),
                    range: address_token.span.clone(),
                });
                continue;
            }

            for (_token, usage) in &info.usages {
                self.script_buffer[*usage / 2] = *address_pos as u16;
            }
        }
        self.label_table.clear();
    }

    fn next_id(&mut self) -> usize {
        self.variable_id += 1;
        self.variable_id
    }
}
