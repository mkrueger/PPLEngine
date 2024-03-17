use std::collections::HashMap;

use crate::{
    ast::{walk_for_stmt, walk_for_stmt_mut, AstVisitor, Expression},
    executable::OpCode,
};

#[derive(Default)]
pub struct RenameScanVistitor {
    pub rename_map: HashMap<unicase::Ascii<String>, unicase::Ascii<String>>,
    cur_index_var: usize,
    file_names: usize,
}

const INDEX_VARS: [&str; 4] = ["i", "j", "k", "l"];

impl AstVisitor<()> for RenameScanVistitor {
    fn visit_for_statement(&mut self, for_stmt: &crate::ast::ForStatement) {
        let var_name = for_stmt.get_identifier();
        if !self.rename_map.contains_key(var_name) && self.cur_index_var < INDEX_VARS.len() {
            self.rename_map.insert(
                var_name.clone(),
                unicase::Ascii::new(INDEX_VARS[self.cur_index_var].to_string()),
            );
            self.cur_index_var += 1;
        }
        walk_for_stmt(self, for_stmt);
    }

    fn visit_predefined_call_statement(&mut self, call: &crate::ast::PredefinedCallStatement) {
        match &call.get_func().opcode {
            OpCode::FOPEN | OpCode::DELETE | OpCode::DISPFILE => {
                if let Expression::Identifier(id) = &call.get_arguments()[0] {
                    let var_name = id.get_identifier();
                    if !self.rename_map.contains_key(var_name) {
                        self.file_names += 1;
                        self.rename_map.insert(
                            var_name.clone(),
                            unicase::Ascii::new(format!("fileName{}", self.file_names)),
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

use crate::ast::AstVisitorMut;
#[derive(Default)]
pub struct RenameVisitor {
    pub rename_map: HashMap<unicase::Ascii<String>, unicase::Ascii<String>>,
}

impl RenameVisitor {
    pub fn new(rename_map: HashMap<unicase::Ascii<String>, unicase::Ascii<String>>) -> Self {
        Self { rename_map }
    }
}

impl AstVisitorMut<()> for RenameVisitor {
    fn visit_identifier_expression(&mut self, identifier: &mut crate::ast::IdentifierExpression) {
        if let Some(id) = self.rename_map.get(identifier.get_identifier()) {
            identifier.set_identifier(id.clone());
        }
    }

    fn visit_function_call_expression(&mut self, call: &mut crate::ast::FunctionCallExpression) {
        if let Some(id) = self.rename_map.get(call.get_identifier()) {
            call.set_identifier(id.clone());
        }
        crate::ast::walk_function_call_expression_mut(self, call);
    }

    fn visit_for_statement(&mut self, for_stmt: &mut crate::ast::ForStatement) {
        if let Some(id) = self.rename_map.get(for_stmt.get_identifier()) {
            for_stmt.set_identifier(id.clone());
        }
        walk_for_stmt_mut(self, for_stmt);
    }

    fn visit_let_statement(&mut self, let_stmt: &mut crate::ast::LetStatement) {
        if let Some(id) = self.rename_map.get(let_stmt.get_identifier()) {
            let_stmt.set_identifier(id.clone());
        }
        crate::ast::walk_let_stmt_mut(self, let_stmt);
    }

    fn visit_goto_statement(&mut self, goto: &mut crate::ast::GotoStatement) {
        if let Some(id) = self.rename_map.get(goto.get_label()) {
            goto.set_label(id.clone());
        }
    }

    fn visit_gosub_statement(&mut self, goto: &mut crate::ast::GosubStatement) {
        if let Some(id) = self.rename_map.get(goto.get_label()) {
            goto.set_label(id.clone());
        }
    }

    fn visit_label_statement(&mut self, label: &mut crate::ast::LabelStatement) {
        if let Some(id) = self.rename_map.get(label.get_label()) {
            label.set_label(id.clone());
        }
    }

    fn visit_procedure_call_statement(&mut self, call: &mut crate::ast::ProcedureCallStatement) {
        if let Some(id) = self.rename_map.get(call.get_identifier()) {
            call.set_identifier(id.clone());
        }
        crate::ast::walk_procedure_call_statement_mut(self, call);
    }

    fn visit_variable_declaration_statement(
        &mut self,
        var_decl: &mut crate::ast::VariableDeclarationStatement,
    ) {
        for v in var_decl.get_variables_mut() {
            if let Some(id) = self.rename_map.get(v.get_identifier()) {
                v.set_identifier(id.clone());
            }
        }
    }

    fn visit_procedure_declaration(
        &mut self,
        proc_decl: &mut crate::ast::ProcedureDeclarationAstNode,
    ) {
        if let Some(id) = self.rename_map.get(proc_decl.get_identifier()) {
            proc_decl.set_identifier(id.clone());
        }
        for v in proc_decl.get_parameters_mut() {
            if let Some(id) = self.rename_map.get(v.get_variable().get_identifier()) {
                v.get_variable_mut().set_identifier(id.clone());
            }
        }
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: &mut crate::ast::FunctionDeclarationAstNode,
    ) {
        if let Some(id) = self.rename_map.get(func_decl.get_identifier()) {
            func_decl.set_identifier(id.clone());
        }
        for v in func_decl.get_parameters_mut() {
            if let Some(id) = self.rename_map.get(v.get_variable().get_identifier()) {
                v.get_variable_mut().set_identifier(id.clone());
            }
        }
    }

    fn visit_function_implementation(&mut self, function: &mut crate::ast::FunctionImplementation) {
        if let Some(id) = self.rename_map.get(function.get_identifier()) {
            function.set_identifier(id.clone());
        }
        for v in function.get_parameters_mut() {
            if let Some(id) = self.rename_map.get(v.get_variable().get_identifier()) {
                v.get_variable_mut().set_identifier(id.clone());
            }
        }

        crate::ast::walk_function_implementation_mut(self, function);
    }

    fn visit_procedure_implementation(
        &mut self,
        procedure: &mut crate::ast::ProcedureImplementation,
    ) {
        if let Some(id) = self.rename_map.get(procedure.get_identifier()) {
            procedure.set_identifier(id.clone());
        }
        for v in procedure.get_parameters_mut() {
            if let Some(id) = self.rename_map.get(v.get_variable().get_identifier()) {
                v.get_variable_mut().set_identifier(id.clone());
            }
        }

        crate::ast::walk_procedure_implementation_mut(self, procedure);
    }
}
