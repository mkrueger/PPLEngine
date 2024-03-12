use super::{
    BinaryExpression, BlockStatement, BreakStatement, CommentStatement, ConstantExpression,
    ContinueStatement, EndStatement, ForStatement, FunctionCallExpression,
    FunctionDeclarationStatement, FunctionImplementation, GosubStatement, GotoStatement,
    IdentifierExpression, IfStatement, IfThenStatement, LabelStatement, LetStatement,
    ParensExpression, PredefinedCallStatement, ProcedureCallStatement,
    ProcedureDeclarationStatement, ProcedureImplementation, ReturnStatement, SelectStatement,
    UnaryExpression, VariableDeclarationStatement, WhileDoStatement, WhileStatement,
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
    fn visit_function_call_expression(&mut self, call: &FunctionCallExpression) -> T {
        walk_function_call_expression(self, call);
        T::default()
    }
    fn visit_parens_expression(&mut self, parens: &ParensExpression) -> T {
        parens.get_expression().visit(self)
    }

    // visit statements
    fn visit_comment_statement(&mut self, comment: &CommentStatement) -> T {
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
    fn visit_select_statement(&mut self, select: &SelectStatement) -> T;
    fn visit_while_statement(&mut self, while_stmt: &WhileStatement) -> T;
    fn visit_while_do_statement(&mut self, while_do: &WhileDoStatement) -> T;
    fn visit_for_statement(&mut self, for_stmt: &ForStatement) -> T;
    fn visit_break_statement(&mut self, break_stmt: &BreakStatement) -> T;
    fn visit_continue_statement(&mut self, continue_stmt: &ContinueStatement) -> T;
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
    fn visit_procedure_declaration_statement(
        &mut self,
        proc_decl: &ProcedureDeclarationStatement,
    ) -> T {
        T::default()
    }
    fn visit_function_declaration_statement(
        &mut self,
        func_decl: &FunctionDeclarationStatement,
    ) -> T {
        T::default()
    }

    // visit implementations
    fn visit_function_implementation(&mut self, function: &FunctionImplementation) -> T {
        walk_function_implementationt(self, function);
        T::default()
    }
    fn visit_procedure_implementation(&mut self, procedure: &ProcedureImplementation) -> T {
        walk_procedure_implementationt(self, procedure);
        T::default()
    }
}

fn walk_function_implementationt<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    function: &FunctionImplementation,
) {
    for stmt in function.get_statements() {
        stmt.visit(visitor);
    }
}

fn walk_procedure_implementationt<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    procedure: &ProcedureImplementation,
) {
    for stmt in procedure.get_statements() {
        stmt.visit(visitor);
    }
}

fn walk_let_stmt<T: Default, V: AstVisitor<T>>(visitor: &mut V, let_stmt: &LetStatement) {
    for arg in let_stmt.get_arguments() {
        arg.visit(visitor);
    }
    let_stmt.get_value_expression().visit(visitor);
}

fn walk_binary_expression<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    binary: &BinaryExpression,
) {
    binary.get_left_expression().visit(visitor);
    binary.get_right_expression().visit(visitor);
}

fn walk_function_call_expression<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    call: &FunctionCallExpression,
) {
    for arg in call.get_arguments() {
        arg.visit(visitor);
    }
}

fn walk_predefined_call_statement<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    call: &PredefinedCallStatement,
) {
    for arg in call.get_arguments() {
        arg.visit(visitor);
    }
}

fn walk_procedure_call_statement<T: Default, V: AstVisitor<T>>(
    visitor: &mut V,
    call: &ProcedureCallStatement,
) {
    for arg in call.get_arguments() {
        arg.visit(visitor);
    }
}

fn walk_if_then_stmt<T: Default, V: AstVisitor<T>>(visitor: &mut V, if_then: &IfThenStatement) {
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

fn walk_if_stmt<T: Default, V: AstVisitor<T>>(visitor: &mut V, if_stmt: &IfStatement) {
    if_stmt.get_condition().visit(visitor);
    if_stmt.get_statement().visit(visitor);
}

fn walk_block_stmt<T: Default, V: AstVisitor<T>>(visitor: &mut V, block: &BlockStatement) {
    for stmt in block.get_statements() {
        stmt.visit(visitor);
    }
}
