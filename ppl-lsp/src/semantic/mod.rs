use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use icy_ppe::{
    ast::{
        walk_function_call_expression, walk_function_implementation,
        walk_predefined_call_statement, walk_predefined_function_call_expression,
        walk_procedure_call_statement, walk_procedure_implementation, AstVisitor, CommentAstNode,
        ConstantExpression, FunctionCallExpression, FunctionDeclarationAstNode,
        FunctionImplementation, GosubStatement, GotoStatement, IdentifierExpression,
        LabelStatement, LetStatement, PredefinedCallStatement, PredefinedFunctionCallExpression,
        ProcedureCallStatement, ProcedureDeclarationAstNode, ProcedureImplementation,
        VariableDeclarationStatement,
    },
    compiler::CompilationErrorType,
    executable::{FuncOpCode, OpCode, VarHeader, VariableType},
    parser::{
        self,
        lexer::{Spanned, Token},
        ErrorRepoter,
    },
};

#[cfg(test)]
mod find_references_tests;

#[derive(Debug, Clone, PartialEq)]
pub enum ReferenceType {
    PredefinedFunc(FuncOpCode),
    PredefinedProc(OpCode),
    Label(usize),
    Variable(usize),

    Function(usize),
    Procedure(usize),
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct References {
    pub declaration: Option<Spanned<String>>,
    pub references: Vec<Spanned<String>>,
}

impl References {
    pub fn contains(&self, offset: usize) -> bool {
        for r in &self.references {
            if r.span.contains(&offset) {
                return true;
            }
        }
        if let Some(decl) = &self.declaration {
            decl.span.contains(&offset)
        } else {
            false
        }
    }
}

#[derive(Default)]
pub struct SemanticVisitor {
    _version: u16,

    pub errors: Arc<Mutex<ErrorRepoter>>,
    pub references: Vec<(ReferenceType, References)>,

    // labels
    label_count: usize,
    label_lookup_table: HashMap<unicase::Ascii<String>, usize>,

    // variables
    local_lookup: bool,
    variable_count: usize,
    variable_lookup: HashMap<unicase::Ascii<String>, usize>,
    local_variable_lookup: HashMap<unicase::Ascii<String>, usize>,
    variables: Vec<VarHeader>,

    procedures: usize,

    functions: usize,
}

impl SemanticVisitor {
    pub fn new(version: u16, errors: Arc<Mutex<ErrorRepoter>>) -> Self {
        Self {
            _version: version,
            errors,
            references: Vec::new(),

            label_count: 0,
            label_lookup_table: HashMap::new(),

            local_lookup: false,
            variable_count: 0,
            variable_lookup: HashMap::new(),
            local_variable_lookup: HashMap::new(),
            variables: Vec::new(),

            procedures: 0,
            functions: 0,
        }
    }

    fn set_declaration(
        &mut self,
        reftype: ReferenceType,
        identifier_token: &Spanned<parser::lexer::Token>,
    ) {
        for r in &mut self.references {
            if r.0 == reftype {
                r.1.declaration = Some(Spanned::new(
                    identifier_token.token.to_string(),
                    identifier_token.span.clone(),
                ));
                return;
            }
        }

        self.references.push((
            reftype,
            References {
                declaration: Some(Spanned::new(
                    identifier_token.token.to_string(),
                    identifier_token.span.clone(),
                )),
                references: vec![],
            },
        ));
    }

    fn add_reference(
        &mut self,
        reftype: ReferenceType,
        identifier_token: &Spanned<parser::lexer::Token>,
    ) {
        for r in &mut self.references {
            if r.0 == reftype {
                r.1.references.push(Spanned::new(
                    identifier_token.token.to_string(),
                    identifier_token.span.clone(),
                ));
                return;
            }
        }
        log::info!("Label ref {:?}", identifier_token);

        self.references.push((
            reftype,
            References {
                declaration: None,
                references: vec![Spanned::new(
                    identifier_token.token.to_string(),
                    identifier_token.span.clone(),
                )],
            },
        ));
    }

    fn add_label_usage(&mut self, label_token: &Spanned<Token>) {
        let Token::Identifier(identifier) = &label_token.token else {
            log::error!("Invalid label token {:?}", label_token);
            return;
        };
        let idx = if let Some(idx) = self.label_lookup_table.get_mut(identifier) {
            *idx
        } else {
            self.label_count += 1;
            self.label_lookup_table
                .insert(identifier.clone(), self.label_count);
            self.label_count
        };

        self.add_reference(ReferenceType::Label(idx), label_token);
    }

    fn set_label_declaration(&mut self, label_token: &Spanned<Token>) {
        let Token::Label(identifier) = &label_token.token else {
            log::error!("Invalid label token {:?}", label_token);
            return;
        };
        let idx = if let Some(idx) = self.label_lookup_table.get_mut(identifier) {
            for r in &mut self.references {
                if r.0 == ReferenceType::Label(*idx) && r.1.declaration.is_some() {
                    self.errors.lock().unwrap().report_error(
                        label_token.span.clone(),
                        CompilationErrorType::LabelAlreadyDefined(identifier.to_string()),
                    );
                    return;
                }
            }
            *idx
        } else {
            self.label_count += 1;
            self.label_lookup_table
                .insert(identifier.clone(), self.label_count);
            self.label_count
        };

        log::info!("Label decla {:?} -> {:?}", identifier, label_token.span);
        self.set_declaration(ReferenceType::Label(idx), label_token);
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

    fn has_variable_defined(&self, id: &unicase::Ascii<String>) -> bool {
        if self.local_lookup {
            return self.local_variable_lookup.contains_key(id);
        }
        self.variable_lookup.contains_key(id)
    }

    fn add_variable(
        &mut self,
        variable_type: VariableType,
        identifier: &Spanned<parser::lexer::Token>,
        dim: u8,
        vector_size: usize,
        matrix_size: usize,
        cube_size: usize,
    ) {
        let id = self.variable_count;
        self.variable_count += 1;
        let header = VarHeader {
            id,
            variable_type,
            dim,
            vector_size,
            matrix_size,
            cube_size,
            flags: 0,
        };
        self.variables.push(header);
        self.set_declaration(ReferenceType::Variable(id), identifier);
        if self.local_lookup {
            self.local_variable_lookup
                .insert(unicase::Ascii::new(identifier.token.to_string()), id);
        } else {
            self.variable_lookup
                .insert(unicase::Ascii::new(identifier.token.to_string()), id);
        }
    }

    fn lookup_variable(
        &mut self,
        id: &unicase::Ascii<String>,
    ) -> Option<&mut (ReferenceType, References)> {
        if self.local_lookup {
            if let Some(idx) = self.local_variable_lookup.get(id) {
                return self.references.get_mut(*idx);
            }
        }

        if let Some(idx) = self.variable_lookup.get(id) {
            return self.references.get_mut(*idx);
        }
        None
    }
}

impl AstVisitor<()> for SemanticVisitor {
    fn visit_identifier_expression(&mut self, identifier: &IdentifierExpression) {
        if let Some(decl) = self.lookup_variable(identifier.get_identifier()) {
            decl.1.references.push(Spanned::new(
                identifier.get_identifier().to_string(),
                identifier.get_identifier_token().span.clone(),
            ));
        } else {
            self.errors.lock().unwrap().report_error(
                identifier.get_identifier_token().span.clone(),
                CompilationErrorType::VariableNotFound(identifier.get_identifier().to_string()),
            );
        }
    }

    fn visit_constant_expression(&mut self, _constant: &ConstantExpression) {
        // nothing yet
    }

    fn visit_predefined_function_call_expression(
        &mut self,
        call: &PredefinedFunctionCallExpression,
    ) {
        self.add_reference(
            ReferenceType::PredefinedFunc(call.get_func().opcode),
            call.get_identifier_token(),
        );
        walk_predefined_function_call_expression(self, call);
    }

    fn visit_comment(&mut self, _comment: &CommentAstNode) {
        // nothing yet
    }

    fn visit_predefined_call_statement(&mut self, call: &PredefinedCallStatement) {
        self.add_reference(
            ReferenceType::PredefinedProc(call.get_func().opcode),
            call.get_identifier_token(),
        );
        walk_predefined_call_statement(self, call);
    }

    fn visit_function_call_expression(&mut self, call: &FunctionCallExpression) {
        let mut found = false;
        if let Some((rt, r)) = self.lookup_variable(call.get_identifier()) {
            if matches!(rt, ReferenceType::Function(_)) {
                r.references.push(Spanned::new(
                    call.get_identifier().to_string(),
                    call.get_identifier_token().span.clone(),
                ));
                found = true;
            }
            if matches!(rt, ReferenceType::Variable(_)) {
                r.references.push(Spanned::new(
                    call.get_identifier().to_string(),
                    call.get_identifier_token().span.clone(),
                ));
                found = true;
            }
        }

        if !found {
            self.errors.lock().unwrap().report_error(
                call.get_identifier_token().span.clone(),
                CompilationErrorType::FunctionNotFound(call.get_identifier().to_string()),
            );
        }
        walk_function_call_expression(self, call);
    }

    fn visit_goto_statement(&mut self, goto: &GotoStatement) {
        self.add_label_usage(goto.get_label_token());
    }

    fn visit_gosub_statement(&mut self, gosub: &GosubStatement) {
        self.add_label_usage(gosub.get_label_token());
    }

    fn visit_label_statement(&mut self, label: &LabelStatement) {
        self.set_label_declaration(label.get_label_token());
    }

    fn visit_let_statement(&mut self, let_stmt: &LetStatement) {
        if let Some((_rt, r)) = self.lookup_variable(let_stmt.get_identifier()) {
            r.references.push(Spanned::new(
                let_stmt.get_identifier().to_string(),
                let_stmt.get_identifier_token().span.clone(),
            ));
        } else {
            self.errors.lock().unwrap().report_error(
                let_stmt.get_identifier_token().span.clone(),
                CompilationErrorType::VariableNotFound(let_stmt.get_identifier().to_string()),
            );
        }
        let_stmt.get_value_expression().visit(self);
    }

    fn visit_variable_declaration_statement(&mut self, var_decl: &VariableDeclarationStatement) {
        for v in var_decl.get_variables() {
            if self.has_variable_defined(v.get_identifier()) {
                self.errors.lock().unwrap().report_error(
                    v.get_identifier_token().span.clone(),
                    CompilationErrorType::VariableAlreadyDefined(v.get_identifier().to_string()),
                );
                continue;
            }

            self.add_variable(
                var_decl.get_variable_type(),
                v.get_identifier_token(),
                v.get_dimensions().len() as u8,
                v.get_vector_size(),
                v.get_matrix_size(),
                v.get_cube_size(),
            );
        }
    }

    fn visit_procedure_call_statement(&mut self, call: &ProcedureCallStatement) {
        let mut found = false;
        if let Some((rt, r)) = self.lookup_variable(call.get_identifier()) {
            if matches!(rt, ReferenceType::Procedure(_)) {
                r.references.push(Spanned::new(
                    call.get_identifier().to_string(),
                    call.get_identifier_token().span.clone(),
                ));
                found = true;
            }
        }

        if !found {
            self.errors.lock().unwrap().report_error(
                call.get_identifier_token().span.clone(),
                CompilationErrorType::ProcedureNotFound(call.get_identifier().to_string()),
            );
        }

        walk_procedure_call_statement(self, call);
    }

    fn visit_procedure_declaration(&mut self, proc_decl: &ProcedureDeclarationAstNode) {
        if self.has_variable_defined(proc_decl.get_identifier()) {
            self.errors.lock().unwrap().report_error(
                proc_decl.get_identifier_token().span.clone(),
                CompilationErrorType::VariableAlreadyDefined(
                    proc_decl.get_identifier().to_string(),
                ),
            );
            return;
        }
        self.variable_lookup
            .insert(proc_decl.get_identifier().clone(), self.references.len());
        self.set_declaration(
            ReferenceType::Procedure(self.procedures),
            proc_decl.get_identifier_token(),
        );
        self.procedures += 1;
    }

    fn visit_function_declaration(&mut self, func_decl: &FunctionDeclarationAstNode) {
        if self.has_variable_defined(func_decl.get_identifier()) {
            self.errors.lock().unwrap().report_error(
                func_decl.get_identifier_token().span.clone(),
                CompilationErrorType::VariableAlreadyDefined(
                    func_decl.get_identifier().to_string(),
                ),
            );
            return;
        }
        self.variable_lookup
            .insert(func_decl.get_identifier().clone(), self.references.len());
        self.set_declaration(
            ReferenceType::Function(self.functions),
            func_decl.get_identifier_token(),
        );
        self.functions += 1;
    }

    fn visit_function_implementation(&mut self, function: &FunctionImplementation) {
        if let Some((_rt, r)) = self.lookup_variable(function.get_identifier()) {
            r.references.push(Spanned::new(
                function.get_identifier().to_string(),
                function.get_identifier_token().span.clone(),
            ));
        } else {
            self.errors.lock().unwrap().report_error(
                function.get_identifier_token().span.clone(),
                CompilationErrorType::FunctionNotFound(function.get_identifier().to_string()),
            );
        }

        self.start_parse_function_body();
        walk_function_implementation(self, function);
        self.end_parse_function_body();
    }

    fn visit_procedure_implementation(&mut self, procedure: &ProcedureImplementation) {
        if let Some((_rt, r)) = self.lookup_variable(procedure.get_identifier()) {
            r.references.push(Spanned::new(
                procedure.get_identifier().to_string(),
                procedure.get_identifier_token().span.clone(),
            ));
        } else {
            self.errors.lock().unwrap().report_error(
                procedure.get_identifier_token().span.clone(),
                CompilationErrorType::ProcedureNotFound(procedure.get_identifier().to_string()),
            );
        }

        self.start_parse_function_body();
        walk_procedure_implementation(self, procedure);
        self.end_parse_function_body();
    }
}