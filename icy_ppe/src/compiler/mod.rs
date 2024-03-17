pub use ast_transform::*;
pub mod ast_transform;

use std::collections::HashMap;
use thiserror::Error;

use crate::{
    ast::{
        AstNode, BinOp, Constant, Expression, ParameterSpecifier, Program, Statement, UnaryOp,
        Variable, VariableData, VariableType,
    },
    executable::{
        EntryType, Executable, FunctionValue, OpCode, PPECommand, PPEExpr, VarHeader, VariableEntry,
    },
    parser::lexer::SpannedToken,
    tables::{get_function_definition, FuncOpCode, FUNCTION_DEFINITIONS},
};

#[cfg(test)]
pub mod tests;

#[derive(Error, Debug)]
pub enum CompilationErrorType {
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

#[derive(Debug)]
struct LabelInfo {
    pub address: Option<(SpannedToken, usize)>,
    pub usages: Vec<(SpannedToken, usize)>,
}

pub struct PPECompiler {
    procedure_declarations: HashMap<unicase::Ascii<String>, Function>,

    variable_id: usize,
    variable_table: Vec<VariableEntry>,
    variable_lookup: HashMap<unicase::Ascii<String>, usize>,

    script_buffer: Vec<i16>,

    label_table: HashMap<unicase::Ascii<String>, LabelInfo>,
    pub errors: Vec<CompilationError>,
    pub warnings: Vec<CompilationWarning>,

    cur_function: unicase::Ascii<String>,
    cur_function_id: i32,

    commands: Vec<PPECommand>,
}

impl PPECompiler {
    pub fn new() -> Self {
        Self {
            variable_table: Vec::new(),
            variable_lookup: HashMap::new(),
            procedure_declarations: HashMap::new(),
            script_buffer: Vec::new(),
            label_table: HashMap::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            commands: Vec::new(),
            cur_function: unicase::Ascii::new(String::new()),

            cur_function_id: -1,
            variable_id: 0,
        }
    }

    fn add_variable(
        &mut self,
        variable_type: VariableType,
        name: unicase::Ascii<String>,
        dim: u8,
        vector_size: usize,
        matrix_size: usize,
        cube_size: usize,
    ) {
        let id = self.next_id();
        let header = VarHeader {
            id,
            variable_type,
            dim,
            vector_size,
            matrix_size,
            cube_size,
            flags: 0,
        };
        let mut entry = VariableEntry::new(header, variable_type.create_empty_value());
        entry.set_name(name.to_string());
        entry.set_type(EntryType::Variable);
        self.variable_table.push(entry);

        self.variable_lookup.insert(name, self.variable_table.len());
    }

    fn add_predefined_variable(&mut self, name: &str, val: Variable) {
        let id = self.next_id();
        self.variable_lookup.insert(
            unicase::Ascii::new(name.to_string()),
            self.variable_table.len(),
        );
        let header = VarHeader {
            id,
            variable_type: val.get_type(),
            dim: val.get_dimensions(),
            vector_size: val.get_vector_size(),
            matrix_size: val.get_matrix_size(),
            cube_size: val.get_cube_size(),
            flags: 0,
        };
        let mut entry = VariableEntry::new(header, val);
        entry.set_name(name.to_string());
        entry.set_type(EntryType::UserVariable);
        self.variable_table.push(entry);
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

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn compile(&mut self, prg: &Program, no_user_vars: bool) {
        if !no_user_vars {
            self.initialize_variables();
        }

        for d in &prg.nodes {
            match d {
                AstNode::Comment(_) => {}
                AstNode::Function(_func) => {}
                AstNode::Procedure(_proc) => {}

                AstNode::FunctionDeclaration(func) => {
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

                    self.variable_lookup
                        .insert(func.get_identifier().clone(), self.variable_table.len());
                    let id: usize = self.next_id();
                    let header: VarHeader = VarHeader {
                        id,
                        variable_type: VariableType::Function,
                        dim: 0,
                        vector_size: 0,
                        matrix_size: 0,
                        cube_size: 0,
                        flags: 0,
                    };
                    let mut entry =
                        VariableEntry::new(header, VariableType::Function.create_empty_value());
                    entry.set_name(func.get_identifier().to_string());
                    entry.set_type(EntryType::Procedure);
                    self.variable_table.push(entry);

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

                AstNode::ProcedureDeclaration(proc) => {
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
                    let header: VarHeader = VarHeader {
                        id,
                        variable_type: VariableType::Procedure,
                        dim: 0,
                        vector_size: 0,
                        matrix_size: 0,
                        cube_size: 0,
                        flags: 0,
                    };
                    let mut entry =
                        VariableEntry::new(header, VariableType::Procedure.create_empty_value());
                    entry.set_name(proc.get_identifier().to_string());
                    entry.set_type(EntryType::Procedure);
                    self.variable_table.push(entry);

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

                AstNode::Statement(stmt) => {
                    self.compile_statement(stmt);
                }
            }
        }
        self.fill_labels();
        if !self.script_buffer.ends_with(&[OpCode::END as i16]) {
            self.script_buffer.push(OpCode::END as i16);
        }

        for imp in &prg.nodes {
            match imp {
                AstNode::Procedure(p) => {
                    {
                        let decl = self
                            .procedure_declarations
                            .get_mut(p.get_identifier())
                            .unwrap();
                        decl.header.start_offset = self.script_buffer.len() as u16 * 2;
                        decl.header.first_var_id = self.variable_table.len() as i16;
                    }
                    self.add_parameters(p.get_parameters());

                    p.get_statements().iter().for_each(|s| {
                        self.compile_statement(s);
                    });

                    self.fill_labels();

                    self.script_buffer.push(OpCode::FPCLR as i16);
                    self.script_buffer.push(OpCode::END as i16);
                    {
                        let decl = self
                            .procedure_declarations
                            .get_mut(p.get_identifier())
                            .unwrap();

                        for i in decl.header.first_var_id as usize..self.variable_table.len() {
                            self.variable_table[i].header.flags |= 1;
                        }

                        decl.header.local_variables =
                            (self.variable_table.len() as i16 - decl.header.first_var_id) as u8;
                    }
                }
                AstNode::Function(func) => {
                    {
                        let id = self.next_id();
                        let decl = self
                            .procedure_declarations
                            .get_mut(func.get_identifier())
                            .unwrap();
                        decl.header.start_offset = self.script_buffer.len() as u16 * 2;
                        decl.header.first_var_id = id as i16;
                        self.variable_lookup
                            .insert(func.get_identifier().clone(), self.variable_table.len());
                        self.cur_function = func.get_identifier().clone();
                        self.cur_function_id = id as i32;
                    }
                    self.add_parameters(func.get_parameters());

                    let id = self.next_id();

                    let header: VarHeader = VarHeader {
                        id,
                        variable_type: *func.get_return_type(),
                        dim: 0,
                        vector_size: 0,
                        matrix_size: 0,
                        cube_size: 0,
                        flags: 0,
                    };
                    let mut entry =
                        VariableEntry::new(header, func.get_return_type().create_empty_value());
                    entry.set_name(format!("RESULT_FOR_{}", func.get_identifier()));
                    entry.set_type(EntryType::FunctionResult);
                    self.variable_table.push(entry);

                    func.get_statements().iter().for_each(|s| {
                        self.compile_statement(s);
                    });
                    self.cur_function_id = -1;
                    self.fill_labels();
                    self.script_buffer.push(OpCode::FEND as i16);
                    self.script_buffer.push(OpCode::END as i16);
                    {
                        let decl = self
                            .procedure_declarations
                            .get_mut(func.get_identifier())
                            .unwrap();
                        decl.header.local_variables =
                            (self.variable_table.len() as i16 - decl.header.first_var_id) as u8;
                    }
                }

                _ => {}
            }
        }

        if !self.script_buffer.ends_with(&[OpCode::END as i16]) {
            self.script_buffer.push(OpCode::END as i16);
        }

        for decl in &self.procedure_declarations {
            for idx in &decl.1.usages {
                self.script_buffer[*idx] = decl.1.id as i16;
            }
        }
    }

    fn add_parameters(&mut self, parameters: &[ParameterSpecifier]) {
        for param in parameters {
            let id = self.next_id();
            self.variable_lookup.insert(
                param.get_variable().get_identifier().clone(),
                self.variable_table.len(),
            );

            let header: VarHeader = VarHeader {
                id,
                variable_type: param.get_variable_type(),
                dim: param.get_variable().get_dimensions().len() as u8,
                vector_size: param.get_variable().get_vector_size(),
                matrix_size: param.get_variable().get_matrix_size(),
                cube_size: param.get_variable().get_cube_size(),
                flags: 0,
            };
            let mut entry =
                VariableEntry::new(header, param.get_variable_type().create_empty_value());
            entry.set_name(param.get_variable().get_identifier().to_string());
            entry.set_type(crate::executable::EntryType::Parameter);
            self.variable_table.push(entry);
        }
    }

    fn compile_statement(&mut self, s: &Statement) {
        match s {
            Statement::End(_) => {
                self.commands.push(PPECommand::End);
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
                self.script_buffer.push(OpCode::IF as i16);

                let cond_buffer = self.compile_expression(if_stmt.get_condition());
                self.script_buffer.extend(cond_buffer);

                // MAGIC?
                self.script_buffer.push(26);

                self.compile_statement(if_stmt.get_statement());
            }
            Statement::While(while_stmt) => {
                self.script_buffer.push(OpCode::WHILE as i16);

                let cond_buffer = self.compile_expression(while_stmt.get_condition());
                self.script_buffer.extend(cond_buffer);

                // MAGIC?
                self.script_buffer.push(30);

                self.compile_statement(while_stmt.get_statement());
            }
            Statement::Return(_) => {
                self.commands.push(PPECommand::Return);
            }
            Statement::Let(let_smt) => {
                self.script_buffer.push(OpCode::LET as i16);
                let var_name = let_smt.get_identifier();

                if self.cur_function_id >= 0 && self.cur_function == *var_name {
                    self.script_buffer.push(self.cur_function_id as i16);
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
                    self.script_buffer.push(decl.header.id as i16);
                    self.script_buffer
                        .push(let_smt.get_arguments().len() as i16);
                    for arg in let_smt.get_arguments() {
                        let expr_buffer = self.compile_expression(arg);
                        self.script_buffer.extend(expr_buffer);
                    }
                }

                let expr_buffer = self.compile_expression(let_smt.get_value_expression());
                self.script_buffer.extend(expr_buffer);
            }
            Statement::Gosub(gosub_stmt) => {
                self.commands.push(PPECommand::Gosub(0));
                self.add_label_usage(
                    gosub_stmt.get_label(),
                    gosub_stmt.get_label_token().clone(),
                    self.commands.len(),
                );
            }
            Statement::Goto(goto_stmt) => {
                self.commands.push(PPECommand::Goto(0));
                self.add_label_usage(
                    goto_stmt.get_label(),
                    goto_stmt.get_label_token().clone(),
                    self.commands.len(),
                );
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
                self.script_buffer.push(op_code as i16);

                /*
                if (call_stmt.get_arguments().len() as i8) < def.min_args
                    || (call_stmt.get_arguments().len() as i8) > def.max_args
                {
                    panic!("Invalid number of parameters for {}", def.name);
                }
                if def.min_args != def.max_args {
                    self.script_buffer
                        .push(call_stmt.get_arguments().len() as i16);
                    }
                */
                for expr in call_stmt.get_arguments() {
                    let expr_buffer = self.compile_expression(expr);
                    self.script_buffer.extend(expr_buffer);
                }
            }
            Statement::Call(call_stmt) => {
                self.script_buffer.push(OpCode::PCALL as i16);

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

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn create_executable(&self, version: u16) -> Result<Executable, CompilationErrorType> {
        Ok(Executable {
            version,
            variable_table: self.variable_table.clone(),
            script_buffer: self.script_buffer.clone(),
        })
    }

    fn compile_expression(&mut self, expr: &Expression) -> Vec<i16> {
        let mut stack = Vec::new();
        self.comp_expr(expr);
        stack.push(0); // push end of expression
        stack
    }

    fn comp_expr(&mut self, expr: &Expression) -> Option<PPEExpr> {
        match expr {
            Expression::Identifier(id) => {
                if let Some(decl_idx) = self.variable_lookup.get(id.get_identifier()) {
                    let decl = &self.variable_table[*decl_idx];
                    return Some(PPEExpr::Value(decl.header.id));
                }
                self.errors.push(CompilationError {
                    error: CompilationErrorType::VariableNotFound(id.get_identifier().to_string()),
                    range: id.get_identifier_token().span.clone(),
                });
            }
            Expression::Const(constant) => {
                let table_id = self.lookup_constant(constant.get_constant_value());
                return Some(PPEExpr::Value(table_id));
            }
            Expression::Parens(expr) => {
                return self.comp_expr(expr.get_expression());
            }
            Expression::PredefinedFunctionCall(expr) => {
                let predef = get_function_definition(expr.get_identifier());
                // TODO: Check parameter signature
                return Some(PPEExpr::PredefinedFunctionCall(
                    &FUNCTION_DEFINITIONS[predef as usize],
                    expr.get_arguments()
                        .iter()
                        .filter_map(|e| self.comp_expr(e))
                        .collect(),
                ));
            }
            Expression::FunctionCall(_expr) => {
                /*   if self
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
                        stack.push(var.header.id as i16);
                        stack.push(var.header.dim as i16);
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
                }*/
            }

            Expression::Unary(unary_expr) => {
                let Some(expr) = self.comp_expr(unary_expr.get_expression()) else {
                    return None;
                };
                let func = match unary_expr.get_op() {
                    UnaryOp::Plus => FuncOpCode::UPLUS,
                    UnaryOp::Minus => FuncOpCode::UMINUS,
                    UnaryOp::Not => FuncOpCode::NOT,
                };
                let offset = -(func as i32);
                return Some(PPEExpr::PredefinedFunctionCall(
                    &FUNCTION_DEFINITIONS[offset as usize],
                    vec![expr],
                ));
            }
            Expression::Binary(bin_expr) => {
                let Some(left) = self.comp_expr(bin_expr.get_left_expression()) else {
                    return None;
                };
                let Some(right) = self.comp_expr(bin_expr.get_right_expression()) else {
                    return None;
                };
                let func = match bin_expr.get_op() {
                    BinOp::PoW => FuncOpCode::EXP,
                    BinOp::Mul => FuncOpCode::TIMES,
                    BinOp::Div => FuncOpCode::DIVIDE,
                    BinOp::Mod => FuncOpCode::MOD,
                    BinOp::Add => FuncOpCode::PLUS,
                    BinOp::Sub => FuncOpCode::MINUS,
                    BinOp::Eq => FuncOpCode::EQ,
                    BinOp::NotEq => FuncOpCode::NE,
                    BinOp::Lower => FuncOpCode::LT,
                    BinOp::LowerEq => FuncOpCode::LE,
                    BinOp::Greater => FuncOpCode::GT,
                    BinOp::GreaterEq => FuncOpCode::GE,
                    BinOp::And => FuncOpCode::AND,
                    BinOp::Or => FuncOpCode::OR,
                };
                let offset = -(func as i32);
                return Some(PPEExpr::PredefinedFunctionCall(
                    &FUNCTION_DEFINITIONS[offset as usize],
                    vec![left, right],
                ));
            }
        }
        None
    }

    fn lookup_constant(&mut self, constant: &Constant) -> usize {
        let id = self.next_id();
        let header: VarHeader = VarHeader {
            id,
            variable_type: constant.get_var_type(),
            dim: 0,
            vector_size: 0,
            matrix_size: 0,
            cube_size: 0,
            flags: 0,
        };
        let mut entry = VariableEntry::new(header, constant.get_value());
        entry.set_name(format!("CONST_{id}"));
        entry.set_type(EntryType::Constant);
        self.variable_table.push(entry);
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
                self.script_buffer[*usage / 2] = *address_pos as i16;
            }
        }
        self.label_table.clear();
    }

    fn next_id(&mut self) -> usize {
        self.variable_id += 1;
        self.variable_id
    }
}

impl Default for PPECompiler {
    fn default() -> Self {
        Self::new()
    }
}
