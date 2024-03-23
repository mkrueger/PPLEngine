use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{
    ast::AstVisitor,
    compiler::CompilationErrorType,
    executable::{FuncOpCode, OpCode, VarHeader, VariableType},
    parser::{
        lexer::{Spanned, Token},
        ErrorRepoter,
    },
};

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
    version: u16,

    pub errors: Arc<Mutex<ErrorRepoter>>,
    pub references: Vec<(ReferenceType, References)>,

    // labels
    label_count: usize,
    label_lookup_table: HashMap<unicase::Ascii<String>, usize>,

    // variables
    local_lookup: bool,
    variable_count: usize,
    variable_lookup: HashMap<String, usize>,
    local_variable_lookup: HashMap<String, usize>,
    variables: Vec<VarHeader>,

    procedures: usize,
    functions: usize,
}

impl SemanticVisitor {
    pub fn new(version: u16, errors: Arc<Mutex<ErrorRepoter>>) -> Self {
        Self {
            version,
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
        identifier_token: &Spanned<crate::parser::lexer::Token>,
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
        identifier_token: &Spanned<crate::parser::lexer::Token>,
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

    fn has_variable_defined(&self, get_identifier: &String) -> bool {
        if self.local_lookup {
            return self.local_variable_lookup.contains_key(get_identifier);
        }
        self.variable_lookup.contains_key(get_identifier)
    }

    fn add_variable(
        &mut self,
        variable_type: VariableType,
        identifier: &Spanned<crate::parser::lexer::Token>,
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
                .insert(identifier.token.to_string(), id);
        } else {
            self.variable_lookup
                .insert(identifier.token.to_string(), id);
        }
    }

    fn lookup_variable(&self, get_identifier: &String) -> Option<usize> {
        if self.local_lookup {
            if let Some(idx) = self.local_variable_lookup.get(get_identifier) {
                return Some(*idx);
            }
        }

        self.variable_lookup.get(get_identifier).copied()
    }
}

impl AstVisitor<()> for SemanticVisitor {
    fn visit_identifier_expression(&mut self, identifier: &crate::ast::IdentifierExpression) {
        if let Some(decl) = self.lookup_variable(identifier.get_identifier()) {
            self.add_reference(
                ReferenceType::Variable(decl),
                identifier.get_identifier_token(),
            );
        } else {
            log::error!("Variable not found {:?}", identifier.get_identifier_token());
        }
    }

    fn visit_constant_expression(&mut self, _constant: &crate::ast::ConstantExpression) {
        // nothing yet
    }

    fn visit_predefined_function_call_expression(
        &mut self,
        call: &crate::ast::PredefinedFunctionCallExpression,
    ) {
        self.add_reference(
            ReferenceType::PredefinedFunc(call.get_func().opcode),
            call.get_identifier_token(),
        );
    }

    fn visit_comment(&mut self, _comment: &crate::ast::CommentAstNode) {
        // nothing yet
    }

    fn visit_predefined_call_statement(&mut self, call: &crate::ast::PredefinedCallStatement) {
        self.add_reference(
            ReferenceType::PredefinedProc(call.get_func().opcode),
            call.get_identifier_token(),
        );
        crate::ast::walk_predefined_call_statement(self, call);
    }

    fn visit_function_call_expression(&mut self, call: &crate::ast::FunctionCallExpression) {
        if let Some(idx) = self.lookup_variable(call.get_identifier()) {
            self.add_reference(ReferenceType::Function(idx), call.get_identifier_token());
        }
        crate::ast::walk_function_call_expression(self, call);
    }

    fn visit_goto_statement(&mut self, goto: &crate::ast::GotoStatement) {
        self.add_label_usage(goto.get_label_token());
    }

    fn visit_gosub_statement(&mut self, gosub: &crate::ast::GosubStatement) {
        self.add_label_usage(gosub.get_label_token());
    }

    fn visit_label_statement(&mut self, label: &crate::ast::LabelStatement) {
        self.set_label_declaration(label.get_label_token());
    }

    fn visit_variable_declaration_statement(
        &mut self,
        var_decl: &crate::ast::VariableDeclarationStatement,
    ) {
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

    fn visit_procedure_declaration(&mut self, proc_decl: &crate::ast::ProcedureDeclarationAstNode) {
        self.set_declaration(
            ReferenceType::Function(self.procedures),
            proc_decl.get_identifier_token(),
        );
        self.procedures += 1;
    }

    fn visit_function_declaration(&mut self, func_decl: &crate::ast::FunctionDeclarationAstNode) {
        self.set_declaration(
            ReferenceType::Function(self.functions),
            func_decl.get_identifier_token(),
        );
        self.functions += 1;
    }

    fn visit_function_implementation(&mut self, function: &crate::ast::FunctionImplementation) {
        self.start_parse_function_body();
        crate::ast::walk_function_implementation(self, function);
        self.end_parse_function_body();
    }

    fn visit_procedure_implementation(&mut self, procedure: &crate::ast::ProcedureImplementation) {
        self.start_parse_function_body();
        crate::ast::walk_procedure_implementation(self, procedure);
        self.end_parse_function_body();
    }
}
