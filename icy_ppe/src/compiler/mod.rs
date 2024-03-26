pub use ast_transform::*;
use unicase::Ascii;
pub mod ast_transform;

use std::collections::HashMap;
use thiserror::Error;

use crate::{
    ast::{Ast, AstNode, Constant, Expression, ParameterSpecifier, Statement},
    executable::{
        EntryType, Executable, ExpressionNegator, FunctionValue, GenericVariableData, OpCode,
        PPECommand, PPEExpr, PPEScript, ProcedureValue, TableEntry, VarHeader, VariableTable,
        VariableType, VariableValue, USER_VARIABLES,
    },
    parser::lexer::{Spanned, Token},
};

use self::expr_compiler::ExpressionCompiler;

pub mod expr_compiler;

/*
#[cfg(test)]
pub mod tests;
*/

#[derive(Error, Debug)]
pub enum CompilationErrorType {
    #[error("Label already used ({0})")]
    LabelAlreadyDefined(String),

    #[error("Label not found ({0})")]
    LabelNotFound(String),

    #[error("Variable name already used ({0})")]
    VariableAlreadyDefined(String),

    #[error("Variable not found ({0})")]
    VariableNotFound(String),

    #[error("Procedure not found ({0})")]
    ProcedureNotFound(String),

    #[error("Function not found ({0})")]
    FunctionNotFound(String),

    #[error("SORT arguments should be one (1) dimensional arrays ({0})")]
    SortArgumentDimensionError(u8),

    #[error("Argument should be a variable ({0})")]
    VariableExpected(usize),

    #[error("Can't assign value to.")]
    InvalidLetVariable,

    #[error("Unused variable ({0})")]
    UnusedVariable(String),

    #[error("Unused FUNCTION/PROCEDURE ({0})")]
    UnusedFunction(String),

    #[error("Missing FUNCTION/PROCEDURE definition. ({0})")]
    MissingImplementation(String),

    #[error("FUNCTION return type does not match with declaration ({0})")]
    ReturnTypeMismatch(String),

    #[error("FUNCTION/PROCEDURE parameters not match with declaration ({0})")]
    ParameterMismatch(String),
}

#[derive(Error, Debug)]
pub enum CompilationWarningType {
    #[error("Unused label {0}")]
    UnusedLabel(String),
}

pub struct PPECompiler {
    version: u16,
    variable_id: usize,
    variable_table: VariableTable,
    variable_lookup: HashMap<unicase::Ascii<String>, usize>,
    local_variable_lookup: HashMap<unicase::Ascii<String>, usize>,
    local_lookup: bool,
    cur_offset: usize,

    label_table: Vec<i32>,
    label_lookup_table: HashMap<unicase::Ascii<String>, usize>,

    const_lookup_table: HashMap<(VariableType, u64), usize>,
    string_lookup_table: HashMap<String, usize>,

    cur_function: unicase::Ascii<String>,
    cur_function_id: i32,

    commands: PPEScript,
}

impl PPECompiler {
    pub fn new(version: u16) -> Self {
        Self {
            version,
            variable_table: VariableTable::default(),
            variable_lookup: HashMap::new(),
            local_variable_lookup: HashMap::new(),
            label_table: Vec::new(),
            label_lookup_table: HashMap::new(),
            commands: PPEScript::default(),
            cur_function: unicase::Ascii::new(String::new()),
            cur_offset: 0,
            cur_function_id: -1,
            variable_id: 0,
            const_lookup_table: HashMap::new(),
            string_lookup_table: HashMap::new(),
            local_lookup: false,
        }
    }

    pub fn get_script(&self) -> &PPEScript {
        &self.commands
    }

    fn push_variable(&mut self, name: unicase::Ascii<String>, entry: TableEntry) {
        self.variable_table.push(entry);
        if self.local_lookup {
            self.local_variable_lookup
                .insert(name, self.variable_table.len());
        } else {
            self.variable_lookup.insert(name, self.variable_table.len());
        }
    }

    fn variable_count(&self) -> usize {
        self.variable_table.len()
    }

    fn add_variable(
        &mut self,
        variable_type: VariableType,
        name: &unicase::Ascii<String>,
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
        let entry = TableEntry::new(
            name.to_string(),
            header,
            variable_type.create_empty_value(),
            EntryType::Variable,
        );
        self.push_variable(name.clone(), entry);
    }

    fn add_predefined_variable(&mut self, name: &str, val: VariableValue) {
        let id = self.next_id();
        let header = VarHeader {
            id,
            variable_type: val.get_type(),
            dim: val.get_dimensions(),
            vector_size: val.get_vector_size(),
            matrix_size: val.get_matrix_size(),
            cube_size: val.get_cube_size(),
            flags: 0,
        };
        let entry = TableEntry::new(name, header, val, EntryType::UserVariable);
        self.push_variable(unicase::Ascii::new(name.to_string()), entry);
    }

    fn initialize_user_variables(&mut self) {
        for user_var in USER_VARIABLES.iter() {
            if user_var.version <= self.version {
                self.add_predefined_variable(user_var.name, user_var.value.clone());
            }
        }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn compile(&mut self, prg: &Ast) {
        if prg.require_user_variables {
            self.initialize_user_variables();
        }

        let prg = prg.visit_mut(&mut AstTransformationVisitor::default());

        for d in &prg.nodes {
            match d {
                AstNode::Function(_func) => {}
                AstNode::Procedure(_proc) => {}
                AstNode::FunctionDeclaration(func) => {
                    if self.has_variable(func.get_identifier()) {
                        log::error!("Variable already defined: {}", func.get_identifier());
                        return;
                    }

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

                    let value = FunctionValue {
                        parameters: func.get_parameters().len() as u8,
                        local_variables: 0,
                        start_offset: 0,
                        first_var_id: 0,
                        return_var: func.get_return_type() as i16,
                    };

                    let entry = TableEntry::new(
                        func.get_identifier().to_string(),
                        header,
                        VariableValue::new_function(value),
                        EntryType::Function,
                    );
                    self.push_variable(func.get_identifier().clone(), entry);
                }
                AstNode::ProcedureDeclaration(proc) => {
                    if self.has_variable(proc.get_identifier()) {
                        log::error!("Variable already defined: {}", proc.get_identifier());
                        return;
                    }
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

                    let mut pass_flags = 0;
                    for (i, p) in proc.get_parameters().iter().enumerate() {
                        if p.is_var() {
                            pass_flags |= 1 << i;
                        }
                    }
                    let value = ProcedureValue {
                        parameters: proc.get_parameters().len() as u8,
                        local_variables: 0,
                        start_offset: 0,
                        first_var_id: 0,
                        pass_flags,
                    };

                    let entry = TableEntry::new(
                        proc.get_identifier().to_string(),
                        header,
                        VariableValue::new_procedure(value),
                        EntryType::Procedure,
                    );
                    self.push_variable(proc.get_identifier().clone(), entry);
                }
                AstNode::VariableDeclaration(stmt) => {
                    self.compile_add_statement(stmt);
                }
                AstNode::Main(block) => {
                    for s in block.get_statements() {
                        self.compile_add_statement(s);
                    }
                }
            }
        }

        if self.commands.statements.is_empty()
            || self.commands.statements.last().unwrap().command != PPECommand::End
        {
            self.commands
                .add_statement(&mut self.cur_offset, PPECommand::End);
        }

        self.compile_functions(&prg);
        self.fill_labels();
    }

    fn compile_functions(&mut self, prg: &Ast) {
        for imp in &prg.nodes {
            match imp {
                AstNode::Procedure(proc) => {
                    let Some(idx) = self.lookup_variable_index(proc.get_identifier()) else {
                        log::error!("Procedure not found: {}", proc.get_identifier());
                        continue;
                    };
                    let first = self.variable_count();
                    self.start_parse_function_body();
                    self.add_parameters(proc.get_parameters());

                    {
                        let start_offset = self.cur_offset as u16 * 2;
                        let decl = self.variable_table.get_var_entry_mut(idx);
                        decl.value.data.procedure_value.start_offset = start_offset;
                        decl.value.data.procedure_value.first_var_id = first as i16;
                    }
                    proc.get_statements().iter().for_each(|s| {
                        self.compile_add_statement(s);
                    });
                    self.end_parse_function_body();

                    self.commands
                        .add_statement(&mut self.cur_offset, PPECommand::EndProc);
                    self.commands
                        .add_statement(&mut self.cur_offset, PPECommand::End);

                    unsafe {
                        let decl = self.variable_table.get_var_entry(idx).clone();

                        for i in decl.value.data.procedure_value.first_var_id as usize
                            ..self.variable_count()
                        {
                            if self.variable_table.get_var_entry(i + 1).get_type()
                                == EntryType::Constant
                            {
                                self.variable_table.get_var_entry_mut(i + 1).header.flags |= 1;
                            }
                        }
                        let var_count = self.variable_count() as i16;
                        let decl = self.variable_table.get_var_entry_mut(idx);
                        let locals = (var_count
                            - decl.value.data.function_value.first_var_id
                            - decl.value.data.function_value.parameters as i16)
                            as u8;
                        decl.value.data.function_value.local_variables = locals;
                    }
                }
                AstNode::Function(func) => {
                    let Some(idx) = self.lookup_variable_index(func.get_identifier()) else {
                        log::error!("Function not found: {}", func.get_identifier());
                        continue;
                    };
                    let first = self.variable_count();
                    self.start_parse_function_body();

                    self.add_parameters(func.get_parameters());

                    let id = self.next_id();
                    {
                        let co = self.cur_offset;
                        let decl = self.variable_table.get_var_entry_mut(idx);
                        decl.value.data.function_value.start_offset = co as u16 * 2;
                        decl.value.data.function_value.return_var = id as i16;
                        decl.value.data.function_value.first_var_id = first as i16;
                        self.cur_function = func.get_identifier().clone();
                        self.cur_function_id = id as i32;
                    }

                    let header: VarHeader = VarHeader {
                        id,
                        variable_type: func.get_return_type(),
                        dim: 0,
                        vector_size: 0,
                        matrix_size: 0,
                        cube_size: 0,
                        flags: 0,
                    };
                    let entry = TableEntry::new(
                        format!("#{id} result"),
                        header,
                        func.get_return_type().create_empty_value(),
                        EntryType::FunctionResult,
                    );
                    self.push_variable(Ascii::new(entry.get_name().clone()), entry);

                    func.get_statements().iter().for_each(|s| {
                        self.compile_add_statement(s);
                    });
                    self.end_parse_function_body();

                    self.cur_function_id = -1;
                    self.commands
                        .add_statement(&mut self.cur_offset, PPECommand::EndFunc);
                    self.commands
                        .add_statement(&mut self.cur_offset, PPECommand::End);
                    unsafe {
                        let var_count = self.variable_count() as i16;
                        let decl = self.variable_table.get_var_entry(idx).clone();
                        for i in decl.value.data.procedure_value.first_var_id as usize
                            ..self.variable_count()
                        {
                            if self.variable_table.get_var_entry(i + 1).get_type()
                                == EntryType::Constant
                            {
                                self.variable_table.get_var_entry_mut(i + 1).header.flags |= 1;
                            }
                        }
                        let decl = self.variable_table.get_var_entry_mut(idx);

                        let locals = (var_count
                            - decl.value.data.function_value.first_var_id
                            - decl.value.data.function_value.parameters as i16)
                            as u8;
                        decl.value.data.function_value.local_variables = locals;
                    }
                }
                _ => {}
            }
        }
    }

    fn compile_add_statement(&mut self, stmt: &Statement) {
        if let Statement::Block(block) = stmt {
            for s in block.get_statements() {
                self.compile_add_statement(s);
            }
            return;
        }
        if let Some(stmt) = self.compile_statement(stmt) {
            self.commands.add_statement(&mut self.cur_offset, stmt);
        }
    }

    fn add_parameters(&mut self, parameters: &[ParameterSpecifier]) {
        for param in parameters {
            let id = self.next_id();
            let header: VarHeader = VarHeader {
                id,
                variable_type: param.get_variable_type(),
                dim: param.get_variable().get_dimensions().len() as u8,
                vector_size: param.get_variable().get_vector_size(),
                matrix_size: param.get_variable().get_matrix_size(),
                cube_size: param.get_variable().get_cube_size(),
                flags: 0,
            };
            let entry = TableEntry::new(
                param.get_variable().get_identifier().to_string(),
                header,
                param.get_variable_type().create_empty_value(),
                crate::executable::EntryType::Parameter,
            );
            self.push_variable(param.get_variable().get_identifier().clone(), entry);
        }
    }

    fn compile_statement(&mut self, s: &Statement) -> Option<PPECommand> {
        match s {
            Statement::Return(_) => Some(PPECommand::Return),
            Statement::Comment(_) => None,

            Statement::Gosub(gosub_stmt) => Some(PPECommand::Gosub(
                self.get_label_index(gosub_stmt.get_label()),
            )),
            Statement::Goto(goto_stmt) => Some(PPECommand::Goto(
                self.get_label_index(goto_stmt.get_label()),
            )),
            Statement::Label(label) => {
                self.set_label_offset(label.get_label_token());
                None
            }
            Statement::If(if_stmt) => {
                let Statement::Goto(goto_stmt) = if_stmt.get_statement() else {
                    panic!("Invalid if statement without goto.");
                };

                let cond_buffer = self
                    .comp_expr(if_stmt.get_condition())
                    .visit_mut(&mut ExpressionNegator::default());
                Some(PPECommand::IfNot(
                    Box::new(cond_buffer),
                    self.get_label_index(goto_stmt.get_label()),
                ))
            }

            Statement::Let(let_smt) => {
                let var_name = let_smt.get_identifier();

                let Some(decl_idx) = self.lookup_variable_index(var_name) else {
                    log::error!("Variable not found: {}", var_name);
                    return None;
                };

                let mut decl = self.variable_table.get_var_entry(decl_idx);
                if decl.header.variable_type == VariableType::Function {
                    unsafe {
                        decl = self
                            .variable_table
                            .get_var_entry(decl.value.data.function_value.return_var as usize);
                    }
                }

                if decl.header.dim != let_smt.get_arguments().len() as u8 {
                    log::error!("Invalid dimensions for variable: {}", var_name.to_string());
                    return None;
                }
                let decl_id = decl.header.id;
                let variable = if decl.header.dim == 0 {
                    PPEExpr::Value(decl_id)
                } else {
                    let mut arguments = Vec::new();
                    for arg in let_smt.get_arguments() {
                        let expr_buffer = self.comp_expr(arg);
                        arguments.push(expr_buffer);
                    }
                    PPEExpr::Dim(decl_id, arguments)
                };
                let value = self.comp_expr(let_smt.get_value_expression());

                Some(PPECommand::Let(Box::new(variable), Box::new(value)))
            }
            Statement::PredifinedCall(call_stmt) => {
                let def = call_stmt.get_func();
                let mut arguments = Vec::new();
                for arg in call_stmt.get_arguments() {
                    let expr_buffer = self.comp_expr(arg);
                    arguments.push(expr_buffer);
                }

                Some(PPECommand::PredefinedCall(
                    def.opcode.get_definition(), // to de-alias aliases
                    arguments,
                ))
            }
            Statement::Call(call_stmt) => {
                let Some(decl_idx) = self.lookup_variable_index(call_stmt.get_identifier()) else {
                    log::error!(
                        "Procedure not found: {}",
                        call_stmt.get_identifier().to_string()
                    );
                    return None;
                };
                let mut arguments = Vec::new();
                for arg in call_stmt.get_arguments() {
                    let expr_buffer = self.comp_expr(arg);
                    arguments.push(expr_buffer);
                }

                let decl = self.variable_table.get_var_entry(decl_idx).clone();
                if decl.header.variable_type == VariableType::Procedure {
                    let len = unsafe { decl.value.data.procedure_value.parameters as usize };
                    if !Self::check_arg_count(
                        len,
                        arguments.len(),
                        call_stmt.get_identifier_token(),
                    ) {
                        return None;
                    }
                    Some(PPECommand::ProcedureCall(decl.header.id, arguments))
                } else if decl.header.variable_type == VariableType::Function {
                    let len = unsafe { decl.value.data.function_value.parameters as usize };
                    if !Self::check_arg_count(
                        len,
                        arguments.len(),
                        call_stmt.get_identifier_token(),
                    ) {
                        return None;
                    }

                    Some(PPECommand::PredefinedCall(
                        OpCode::EVAL.get_definition(),
                        vec![PPEExpr::FunctionCall(decl.header.id, arguments)],
                    ))
                } else {
                    log::error!("Invalid call to variable: {}", call_stmt.get_identifier());
                    return None;
                }
            }
            Statement::VariableDeclaration(var_decl) => {
                for v in var_decl.get_variables() {
                    if self.has_variable_defined(v.get_identifier()) {
                        log::error!("Variable already defined: {}", v.get_identifier());
                        continue;
                    }

                    self.add_variable(
                        var_decl.get_variable_type(),
                        v.get_identifier(),
                        v.get_dimensions().len() as u8,
                        v.get_vector_size(),
                        v.get_matrix_size(),
                        v.get_cube_size(),
                    );
                }
                None
            }
            Statement::While(_) => panic!("While not allowed in output AST."),
            Statement::Block(_) => panic!("Block not handled by compile statement."),
            Statement::Continue(_) => panic!("Continue not allowed in output AST."),
            Statement::Break(_) => panic!("Break not allowed in output AST."),
            Statement::IfThen(_) => panic!("if then not allowed in output AST."),
            Statement::WhileDo(_) => panic!("do while not allowed in output AST."),
            Statement::For(_) => panic!("for not allowed in output AST."),
            Statement::Select(_) => panic!("select not allowed in output AST."),
        }
    }

    fn check_arg_count(
        arg_count_expected: usize,
        arg_count: usize,
        identifier_token: &Spanned<Token>,
    ) -> bool {
        if arg_count_expected != arg_count {
            log::error!(
                "Invalid number of parameters for {}: expected {}, got {}",
                identifier_token.token,
                arg_count_expected,
                arg_count
            );
            return false;
        }
        true
    }

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn create_executable(&self, version: u16) -> Result<Executable, CompilationErrorType> {
        let mut variable_table = self.variable_table.clone();
        variable_table.set_version(version);
        Ok(Executable {
            version,
            variable_table,
            script_buffer: self.commands.serialize(),
        })
    }

    fn comp_expr(&mut self, expr: &Expression) -> PPEExpr {
        expr.visit(&mut ExpressionCompiler { compiler: self })
    }

    fn lookup_constant(&mut self, constant: &Constant) -> usize {
        let value = constant.get_value();

        if let GenericVariableData::String(str) = &value.generic_data {
            if let Some(id) = self.string_lookup_table.get(str) {
                return *id;
            }
        } else {
            unsafe {
                let key = (constant.get_var_type(), value.data.u64_value);
                if let Some(id) = self.const_lookup_table.get(&key) {
                    return *id;
                }
            }
        }
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
        let name = format!(
            "CONST_{}",
            self.const_lookup_table.len() + self.string_lookup_table.len() + 1
        );
        let entry = TableEntry::new(name, header, value.clone(), EntryType::Constant);
        self.variable_table.push(entry);
        if let GenericVariableData::String(str) = value.generic_data {
            self.string_lookup_table.insert(str, id);
        } else {
            unsafe {
                let key = (constant.get_var_type(), value.data.u64_value);
                self.const_lookup_table.insert(key, id);
            }
        }
        id
    }

    fn next_id(&mut self) -> usize {
        self.variable_id += 1;
        self.variable_id
    }

    fn get_label_index(&mut self, label: &unicase::Ascii<String>) -> usize {
        if let Some(idx) = self.label_lookup_table.get(label) {
            *idx
        } else {
            let idx = self.label_table.len();
            self.label_lookup_table.insert(label.clone(), idx);
            self.label_table.push(-1);
            idx
        }
    }

    fn set_label_offset(&mut self, label_token: &Spanned<Token>) {
        let Token::Label(identifier) = &label_token.token else {
            log::error!("Invalid label token {:?}", label_token);
            return;
        };
        if let Some(idx) = self.label_lookup_table.get_mut(identifier) {
            if self.label_table[*idx] >= 0 {
                log::error!("Label already defined: {}", identifier);
                return;
            }
            self.label_table[*idx] = self.cur_offset as i32;
        } else {
            let idx = self.label_table.len();
            self.label_lookup_table.insert(identifier.clone(), idx);
            self.label_table.push(self.cur_offset as i32);
        }
    }

    fn start_parse_function_body(&mut self) {
        self.label_lookup_table.clear();
        self.local_variable_lookup.clear();
        self.local_lookup = true;
    }

    fn end_parse_function_body(&mut self) {
        self.local_variable_lookup.clear();
        self.local_lookup = false;
    }

    fn has_variable_defined(&self, get_identifier: &unicase::Ascii<String>) -> bool {
        if self.local_lookup {
            return self.local_variable_lookup.contains_key(get_identifier);
        }
        self.variable_lookup.contains_key(get_identifier)
    }

    fn has_variable(&self, get_identifier: &unicase::Ascii<String>) -> bool {
        self.variable_lookup.contains_key(get_identifier)
            || self.local_variable_lookup.contains_key(get_identifier)
    }

    fn lookup_variable(&self, get_identifier: &unicase::Ascii<String>) -> Option<&TableEntry> {
        if self.local_lookup {
            if let Some(idx) = self.local_variable_lookup.get(get_identifier) {
                return Some(self.variable_table.get_var_entry(*idx));
            }
        }

        if let Some(idx) = self.variable_lookup.get(get_identifier) {
            Some(self.variable_table.get_var_entry(*idx))
        } else {
            None
        }
    }

    fn lookup_variable_index(&self, get_identifier: &unicase::Ascii<String>) -> Option<usize> {
        if self.local_lookup {
            if let Some(c) = self.local_variable_lookup.get(get_identifier) {
                return Some(*c);
            }
        }
        self.variable_lookup.get(get_identifier).copied()
    }

    fn fill_labels(&mut self) {
        let last = (self.commands.statements.len() as i32 - 1) as usize;
        for stmt in &mut self.commands.statements {
            match &mut stmt.command {
                PPECommand::IfNot(_, idx) | PPECommand::Goto(idx) | PPECommand::Gosub(idx) => {
                    let label_idx = self.label_table[*idx];
                    if label_idx < 0 {
                        *idx = last;
                    } else {
                        *idx = label_idx as usize * 2;
                    }
                }
                _ => {}
            }
        }
    }
}
