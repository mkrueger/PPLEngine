use crate::ast::AstVisitorMut;
/*

#[derive(Default)]
pub struct RenameVarsVisitor {}

impl AstVisitorMut<()> for RenameVarsVisitor {
    fn visit_identifier_expression(&mut self, identifier: &mut crate::ast::IdentifierExpression) {
        identifier.set_identifier(identifier.get_identifier().to_ascii_uppercase());
    }

    fn visit_function_call_expression(&mut self, call: &mut crate::ast::FunctionCallExpression) {
        call.set_identifier(call.get_identifier().to_ascii_uppercase());
        crate::ast::walk_function_call_expression_mut(self, call);
    }

    fn visit_let_statement(&mut self, let_stmt: &mut crate::ast::LetStatement) {
        let_stmt.set_identifier(let_stmt.get_identifier().to_ascii_uppercase());
        crate::ast::walk_let_stmt_mut(self, let_stmt);
    }

    fn visit_goto_statement(&mut self, goto: &mut crate::ast::GotoStatement) {
        goto.set_label(goto.get_label().to_ascii_uppercase());
    }

    fn visit_label_statement(&mut self, label: &mut crate::ast::LabelStatement) {
        label.set_label(label.get_label().to_ascii_uppercase());
    }

    fn visit_procedure_call_statement(&mut self, call: &mut crate::ast::ProcedureCallStatement) {
        call.set_identifier(call.get_identifier().to_ascii_uppercase());
        crate::ast::walk_procedure_call_statement_mut(self, call);
    }

    fn visit_variable_declaration_statement(
        &mut self,
        var_decl: &mut crate::ast::VariableDeclarationStatement,
    ) {
        for v in var_decl.get_variables_mut() {
            v.set_identifier(v.get_identifier().to_ascii_uppercase());
        }
    }

    fn visit_procedure_declaration_statement(
        &mut self,
        proc_decl: &mut crate::ast::ProcedureDeclarationStatement,
    ) {
        proc_decl.set_identifier(proc_decl.get_identifier().to_ascii_uppercase());
    }

    fn visit_function_declaration_statement(
        &mut self,
        func_decl: &mut crate::ast::FunctionDeclarationStatement,
    ) {
        func_decl.set_identifier(func_decl.get_identifier().to_ascii_uppercase());
    }

    fn visit_function_implementation(&mut self, function: &mut crate::ast::FunctionImplementation) {
        function.set_identifier(function.get_identifier().to_ascii_uppercase());
        for p in function.get_parameters_mut() {
            let upper = p.get_variable_mut().get_identifier().to_ascii_uppercase();
            p.get_variable_mut().set_identifier(upper);
        }
        crate::ast::walk_function_implementation_mut(self, function);
    }

    fn visit_procedure_implementation(
        &mut self,
        procedure: &mut crate::ast::ProcedureImplementation,
    ) {
        procedure.set_identifier(procedure.get_identifier().to_ascii_uppercase());
        for p in procedure.get_parameters_mut() {
            let upper = p.get_variable_mut().get_identifier().to_ascii_uppercase();
            p.get_variable_mut().set_identifier(upper);
        }
        crate::ast::walk_procedure_implementation_mut(self, procedure);
    }
}
*/
