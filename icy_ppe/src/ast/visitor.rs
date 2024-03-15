use crate::parser::lexer::SpannedToken;

use super::{
    BinaryExpression, BlockStatement, BreakStatement, CommentAstNode, ConstantExpression,
    ContinueStatement, EndStatement, ForStatement, FunctionCallExpression,
    FunctionDeclarationAstNode, FunctionImplementation, GosubStatement, GotoStatement,
    IdentifierExpression, IfStatement, IfThenStatement, LabelStatement, LetStatement,
    ParensExpression, PredefinedCallStatement, PredefinedFunctionCallExpression,
    ProcedureCallStatement, ProcedureDeclarationAstNode, ProcedureImplementation, Program,
    ReturnStatement, SelectStatement, UnaryExpression, VariableDeclarationStatement,
    WhileDoStatement, WhileStatement,
};

#[allow(unused_variables)]
pub trait AstVisitor<T: Default>: Sized {
    // visit expressions
    fn visit_identifier_expression(&mut self, identifier: &IdentifierExpression) -> T {
        T::default()
    }
    fn visit_constant_expression(&mut self, constant: &ConstantExpression) -> T {
        T::default()
    }
    fn visit_binary_expression(&mut self, binary: &BinaryExpression) -> T {
        walk_binary_expression(self, binary);
        T::default()
    }
    fn visit_unary_expression(&mut self, unary: &UnaryExpression) -> T {
        unary.get_expression().visit(self)
    }
    fn visit_predefined_function_call_expression(
        &mut self,
        call: &PredefinedFunctionCallExpression,
    ) -> T {
        walk__predefined_function_call_expression(self, call);
        T::default()
    }
    fn visit_function_call_expression(&mut self, call: &FunctionCallExpression) -> T {
        walk_function_call_expression(self, call);
        T::default()
    }
    fn visit_parens_expression(&mut self, parens: &ParensExpression) -> T {
        parens.get_expression().visit(self)
    }

    // visit statements
    fn visit_comment(&mut self, comment: &CommentAstNode) -> T {
        T::default()
    }
    fn visit_end_statement(&mut self, end: &EndStatement) -> T {
        T::default()
    }
    fn visit_block_statement(&mut self, block: &BlockStatement) -> T {
        walk_block_stmt(self, block);
        T::default()
    }
    fn visit_if_statement(&mut self, if_stmt: &IfStatement) -> T {
        walk_if_stmt(self, if_stmt);
        T::default()
    }
    fn visit_if_then_statement(&mut self, if_then: &IfThenStatement) -> T {
        walk_if_then_stmt(self, if_then);
        T::default()
    }
    fn visit_select_statement(&mut self, select_stmt: &SelectStatement) -> T {
        walk_select_stmt(self, select_stmt);
        T::default()
    }
    fn visit_while_statement(&mut self, while_stmt: &WhileStatement) -> T {
        walk_while_stmt(self, while_stmt);
        T::default()
    }
    fn visit_while_do_statement(&mut self, while_do: &WhileDoStatement) -> T {
        walk_while_do_stmt(self, while_do);
        T::default()
    }
    fn visit_for_statement(&mut self, for_stmt: &ForStatement) -> T {
        walk_for_stmt(self, for_stmt);
        T::default()
    }
    fn visit_break_statement(&mut self, break_stmt: &BreakStatement) -> T {
        T::default()
    }
    fn visit_continue_statement(&mut self, continue_stmt: &ContinueStatement) -> T {
        T::default()
    }
    fn visit_gosub_statement(&mut self, gosub: &GosubStatement) -> T {
        T::default()
    }
    fn visit_return_statement(&mut self, return_stmt: &ReturnStatement) -> T {
        T::default()
    }
    fn visit_let_statement(&mut self, let_stmt: &LetStatement) -> T {
        walk_let_stmt(self, let_stmt);
        T::default()
    }
    fn visit_goto_statement(&mut self, goto: &GotoStatement) -> T {
        T::default()
    }
    fn visit_label_statement(&mut self, label: &LabelStatement) -> T {
        T::default()
    }
    fn visit_procedure_call_statement(&mut self, call: &ProcedureCallStatement) -> T {
        walk_procedure_call_statement(self, call);
        T::default()
    }
    fn visit_predefined_call_statement(&mut self, call: &PredefinedCallStatement) -> T {
        walk_predefined_call_statement(self, call);
        T::default()
    }

    // visit declarations
    fn visit_variable_declaration_statement(
        &mut self,
        var_decl: &VariableDeclarationStatement,
    ) -> T {
        T::default()
    }
    fn visit_procedure_declaration(&mut self, proc_decl: &ProcedureDeclarationAstNode) -> T {
        T::default()
    }
    fn visit_function_declaration(&mut self, func_decl: &FunctionDeclarationAstNode) -> T {
        T::default()
    }

    // visit implementations

    fn visit_function_implementation(&mut self, function: &FunctionImplementation) -> T {
        walk_function_implementation(self, function);
        T::default()
    }

    fn visit_procedure_implementation(&mut self, procedure: &ProcedureImplementation) -> T {
        walk_procedure_implementation(self, procedure);
        T::default()
    }

    fn visit_program(&mut self, program: &Program) -> T {
        walk_program(self, program);
        T::default()
    }
}

pub fn walk_program<T: Default, V: AstVisitor<T>>(visitor: &mut V, program: &Program) {
    for node in &program.nodes {
        node.visit(visitor);
    }
}

pub fn walk_select_stmt<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    select_stmt: &SelectStatement,
) {
    select_stmt.get_expression().visit(visitor);

    for case_block in select_stmt.get_case_blocks() {
        case_block.get_expression().visit(visitor);
        for stmt in case_block.get_statements() {
            stmt.visit(visitor);
        }
    }
    if let Some(else_block) = select_stmt.get_case_else_block() {
        else_block.get_expression().visit(visitor);

        for stmt in else_block.get_statements() {
            stmt.visit(visitor);
        }
    }
}

pub fn walk_while_stmt<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    while_do_stmt: &WhileStatement,
) {
    while_do_stmt.get_condition().visit(visitor);
    while_do_stmt.get_statement().visit(visitor);
}

pub fn walk_while_do_stmt<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    while_do_stmt: &WhileDoStatement,
) {
    while_do_stmt.get_condition().visit(visitor);
    for stmt in while_do_stmt.get_statements() {
        stmt.visit(visitor);
    }
}

pub fn walk_for_stmt<T: Default, V: AstVisitor<T>>(visitor: &mut V, for_stmt: &ForStatement) {
    for_stmt.get_start_expr().visit(visitor);
    for_stmt.get_end_expr().visit(visitor);
    if let Some(step) = for_stmt.get_step_expr() {
        step.visit(visitor);
    }
    for stmt in for_stmt.get_statements() {
        stmt.visit(visitor);
    }
}

pub fn walk_function_implementation<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    function: &FunctionImplementation,
) {
    for stmt in function.get_statements() {
        stmt.visit(visitor);
    }
}

pub fn walk_procedure_implementation<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    procedure: &ProcedureImplementation,
) {
    for stmt in procedure.get_statements() {
        stmt.visit(visitor);
    }
}

pub fn walk_let_stmt<T: Default, V: AstVisitor<T>>(visitor: &mut V, let_stmt: &LetStatement) {
    for arg in let_stmt.get_arguments() {
        arg.visit(visitor);
    }
    let_stmt.get_value_expression().visit(visitor);
}

pub fn walk_binary_expression<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    binary: &BinaryExpression,
) {
    binary.get_left_expression().visit(visitor);
    binary.get_right_expression().visit(visitor);
}

pub fn walk__predefined_function_call_expression<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    call: &PredefinedFunctionCallExpression,
) {
    for arg in call.get_arguments() {
        arg.visit(visitor);
    }
}

pub fn walk_function_call_expression<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    call: &FunctionCallExpression,
) {
    for arg in call.get_arguments() {
        arg.visit(visitor);
    }
}

pub fn walk_predefined_call_statement<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    call: &PredefinedCallStatement,
) {
    for arg in call.get_arguments() {
        arg.visit(visitor);
    }
}

pub fn walk_procedure_call_statement<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    call: &ProcedureCallStatement,
) {
    for arg in call.get_arguments() {
        arg.visit(visitor);
    }
}

pub fn walk_if_then_stmt<T: Default, V: AstVisitor<T>>(visitor: &mut V, if_then: &IfThenStatement) {
    if_then.get_condition().visit(visitor);
    for stmt in if_then.get_statements() {
        stmt.visit(visitor);
    }
    for stmt in if_then.get_else_if_blocks() {
        stmt.get_condition().visit(visitor);
        for s in stmt.get_statements() {
            s.visit(visitor);
        }
    }
    if let Some(else_block) = if_then.get_else_block() {
        for stmt in else_block.get_statements() {
            stmt.visit(visitor);
        }
    }
}

pub fn walk_if_stmt<T: Default, V: AstVisitor<T>>(visitor: &mut V, if_stmt: &IfStatement) {
    if_stmt.get_condition().visit(visitor);
    if_stmt.get_statement().visit(visitor);
}

pub fn walk_block_stmt<T: Default, V: AstVisitor<T>>(visitor: &mut V, block: &BlockStatement) {
    for stmt in block.get_statements() {
        stmt.visit(visitor);
    }
}

#[allow(unused_variables)]
pub trait AstVisitorMut<T: Default>: Sized {
    // visit expressions
    fn visit_identifier_expression(&mut self, identifier: &mut IdentifierExpression) -> T {
        T::default()
    }
    fn visit_constant_expression(&mut self, constant: &mut ConstantExpression) -> T {
        T::default()
    }
    fn visit_binary_expression(&mut self, binary: &mut BinaryExpression) -> T {
        walk_binary_expression_mut(self, binary);
        T::default()
    }
    fn visit_unary_expression(&mut self, unary: &mut UnaryExpression) -> T {
        unary.get_expression_mut().visit_mut(self)
    }

    fn visit_predefined_function_call_expression(
        &mut self,
        call: &mut PredefinedFunctionCallExpression,
    ) -> T {
        walk_predefined_function_call_expression_mut(self, call);
        T::default()
    }

    fn visit_function_call_expression(&mut self, call: &mut FunctionCallExpression) -> T {
        walk_function_call_expression_mut(self, call);
        T::default()
    }
    fn visit_parens_expression(&mut self, parens: &mut ParensExpression) -> T {
        parens.get_expression_mut().visit_mut(self)
    }

    // visit statements
    fn visit_comment(&mut self, comment: &mut CommentAstNode) -> T {
        T::default()
    }
    fn visit_end_statement(&mut self, end: &mut EndStatement) -> T {
        T::default()
    }
    fn visit_block_statement(&mut self, block: &mut BlockStatement) -> T {
        walk_block_stmt_mut(self, block);
        T::default()
    }
    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> T {
        walk_if_stmt_mut(self, if_stmt);
        T::default()
    }
    fn visit_if_then_statement(&mut self, if_then: &mut IfThenStatement) -> T {
        walk_if_then_stmt_mut(self, if_then);
        T::default()
    }
    fn visit_select_statement(&mut self, select_stmt: &mut SelectStatement) -> T {
        walk_select_stmt_mut(self, select_stmt);
        T::default()
    }
    fn visit_while_statement(&mut self, while_stmt: &mut WhileStatement) -> T {
        walk_while_stmt_mut(self, while_stmt);
        T::default()
    }
    fn visit_while_do_statement(&mut self, while_do: &mut WhileDoStatement) -> T {
        walk_while_do_stmt_mut(self, while_do);
        T::default()
    }
    fn visit_for_statement(&mut self, for_stmt: &mut ForStatement) -> T {
        walk_for_stmt_mut(self, for_stmt);
        T::default()
    }
    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> T {
        T::default()
    }
    fn visit_continue_statement(&mut self, continue_stmt: &mut ContinueStatement) -> T {
        T::default()
    }
    fn visit_gosub_statement(&mut self, gosub: &mut GosubStatement) -> T {
        T::default()
    }
    fn visit_return_statement(&mut self, return_stmt: &mut ReturnStatement) -> T {
        T::default()
    }
    fn visit_let_statement(&mut self, let_stmt: &mut LetStatement) -> T {
        walk_let_stmt_mut(self, let_stmt);
        T::default()
    }
    fn visit_goto_statement(&mut self, goto: &mut GotoStatement) -> T {
        T::default()
    }
    fn visit_label_statement(&mut self, label: &mut LabelStatement) -> T {
        T::default()
    }
    fn visit_procedure_call_statement(&mut self, call: &mut ProcedureCallStatement) -> T {
        walk_procedure_call_statement_mut(self, call);
        T::default()
    }
    fn visit_predefined_call_statement(&mut self, call: &mut PredefinedCallStatement) -> T {
        walk_predefined_call_statement_mut(self, call);
        T::default()
    }

    // visit declarations
    fn visit_variable_declaration_statement(
        &mut self,
        var_decl: &mut VariableDeclarationStatement,
    ) -> T {
        T::default()
    }
    fn visit_procedure_declaration(&mut self, proc_decl: &mut ProcedureDeclarationAstNode) -> T {
        T::default()
    }
    fn visit_function_declaration(&mut self, func_decl: &mut FunctionDeclarationAstNode) -> T {
        T::default()
    }

    // visit implementations

    fn visit_comment_implementation(&mut self, comment: &mut SpannedToken) -> T {
        T::default()
    }

    fn visit_function_implementation(&mut self, function: &mut FunctionImplementation) -> T {
        walk_function_implementation_mut(self, function);
        T::default()
    }

    fn visit_procedure_implementation(&mut self, procedure: &mut ProcedureImplementation) -> T {
        walk_procedure_implementation_mut(self, procedure);
        T::default()
    }

    fn visit_program(&mut self, program: &mut Program) -> T {
        walk_program_mut(self, program);
        T::default()
    }
}

pub fn walk_program_mut<T: Default, V: AstVisitorMut<T>>(visitor: &mut V, program: &mut Program) {
    for node in &mut program.nodes {
        node.visit_mut(visitor);
    }
}

pub fn walk_select_stmt_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    select_stmt: &mut SelectStatement,
) {
    select_stmt.get_expression_mut().visit_mut(visitor);

    for case_block in select_stmt.get_case_blocks_mut() {
        case_block.get_expression_mut().visit_mut(visitor);
        for stmt in case_block.get_statements_mut() {
            stmt.visit_mut(visitor);
        }
    }
    if let Some(else_block) = select_stmt.get_case_else_block_mut() {
        else_block.get_expression_mut().visit_mut(visitor);

        for stmt in else_block.get_statements_mut() {
            stmt.visit_mut(visitor);
        }
    }
}

pub fn walk_while_stmt_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    while_do_stmt: &mut WhileStatement,
) {
    while_do_stmt.get_condition_mut().visit_mut(visitor);
    while_do_stmt.get_statement_mut().visit_mut(visitor);
}

pub fn walk_while_do_stmt_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    while_do_stmt: &mut WhileDoStatement,
) {
    while_do_stmt.get_condition_mut().visit_mut(visitor);
    for stmt in while_do_stmt.get_statements_mut() {
        stmt.visit_mut(visitor);
    }
}

pub fn walk_for_stmt_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    for_stmt: &mut ForStatement,
) {
    for_stmt.get_start_expr_mut().visit_mut(visitor);
    for_stmt.get_end_expr_mut().visit_mut(visitor);
    if let Some(step) = for_stmt.get_step_expr_mut() {
        step.visit_mut(visitor);
    }
    for stmt in for_stmt.get_statements_mut() {
        stmt.visit_mut(visitor);
    }
}

pub fn walk_function_implementation_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    function: &mut FunctionImplementation,
) {
    for stmt in function.get_statements_mut() {
        stmt.visit_mut(visitor);
    }
}

pub fn walk_procedure_implementation_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    procedure: &mut ProcedureImplementation,
) {
    for stmt in procedure.get_statements_mut() {
        stmt.visit_mut(visitor);
    }
}

pub fn walk_let_stmt_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    let_stmt: &mut LetStatement,
) {
    for arg in let_stmt.get_arguments_mut() {
        arg.visit_mut(visitor);
    }
    let_stmt.get_value_expression_mut().visit_mut(visitor);
}

pub fn walk_binary_expression_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    binary: &mut BinaryExpression,
) {
    binary.get_left_expression_mut().visit_mut(visitor);
    binary.get_right_expression_mut().visit_mut(visitor);
}

pub fn walk_predefined_function_call_expression_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    call: &mut PredefinedFunctionCallExpression,
) {
    for arg in call.get_arguments_mut() {
        arg.visit_mut(visitor);
    }
}

pub fn walk_function_call_expression_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    call: &mut FunctionCallExpression,
) {
    for arg in call.get_arguments_mut() {
        arg.visit_mut(visitor);
    }
}

pub fn walk_predefined_call_statement_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    call: &mut PredefinedCallStatement,
) {
    for arg in call.get_arguments_mut() {
        arg.visit_mut(visitor);
    }
}

pub fn walk_procedure_call_statement_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    call: &mut ProcedureCallStatement,
) {
    for arg in call.get_arguments_mut() {
        arg.visit_mut(visitor);
    }
}

pub fn walk_if_then_stmt_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    if_then: &mut IfThenStatement,
) {
    if_then.get_condition_mut().visit_mut(visitor);
    for stmt in if_then.get_statements_mut() {
        stmt.visit_mut(visitor);
    }
    for stmt in if_then.get_else_if_blocks_mut() {
        stmt.get_condition_mut().visit_mut(visitor);
        for s in stmt.get_statements_mut() {
            s.visit_mut(visitor);
        }
    }
    if let Some(else_block) = if_then.get_else_block_mut() {
        for stmt in else_block.get_statements_mut() {
            stmt.visit_mut(visitor);
        }
    }
}

pub fn walk_if_stmt_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    if_stmt: &mut IfStatement,
) {
    if_stmt.get_condition_mut().visit_mut(visitor);
    if_stmt.get_statement_mut().visit_mut(visitor);
}

pub fn walk_block_stmt_mut<T: Default, V: AstVisitorMut<T>>(
    visitor: &mut V,
    block: &mut BlockStatement,
) {
    for stmt in block.get_statements_mut() {
        stmt.visit_mut(visitor);
    }
}
