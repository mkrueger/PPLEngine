use crate::ast::{AstVisitorMut, BinaryExpression, BlockStatement, Constant, ConstantExpression, Expression, ForStatement, GotoStatement, IdentifierExpression, IfStatement, LabelStatement, LetStatement, SelectStatement, Statement, UnaryExpression};

#[derive(Default)]
pub struct AstTransformationVisitor {
    labels: usize,
    continue_break_labels: Vec<(unicase::Ascii<String>, unicase::Ascii<String>)>,
}

impl AstTransformationVisitor {
    pub fn next_label(&mut self) -> unicase::Ascii<String> {
        let label = unicase::Ascii::new(format!("*(label{}", self.labels));
        self.labels += 1;
        label
    }
}

impl AstVisitorMut for AstTransformationVisitor {
    fn visit_continue_statement(&mut self, _continue_stmt: &crate::ast::ContinueStatement) -> Statement {
        let (continue_label, _) = self.continue_break_labels.last().unwrap();
        GotoStatement::create_empty_statement(continue_label.clone())
    }
    fn visit_break_statement(&mut self, _break_stmt: &crate::ast::BreakStatement) -> Statement {
        let (_, break_label) = self.continue_break_labels.last().unwrap();
        GotoStatement::create_empty_statement(break_label.clone())
    }

    fn visit_if_then_statement(&mut self, if_then: &crate::ast::IfThenStatement) -> Statement {
        let mut statements = Vec::new();
        
        let if_exit_label = self.next_label();
        statements.push(
            IfStatement::create_empty_statement(
                Box::new(UnaryExpression::create_empty_expression(crate::ast::UnaryOp::Not, Box::new(if_then.get_condition().clone()))),
                Box::new(GotoStatement::create_empty_statement(if_exit_label.clone())))
        );
        statements.extend(if_then.get_statements().iter().map(|s| s.visit_mut(self)));
        statements.push(
            GotoStatement::create_empty_statement(if_exit_label.clone())
        );
        for else_if in if_then.get_else_if_blocks() {
            let else_if_exit_label = self.next_label();
            statements.push(
                IfStatement::create_empty_statement(
                    Box::new(UnaryExpression::create_empty_expression(crate::ast::UnaryOp::Not, Box::new(else_if.get_condition().clone()))),
                    Box::new(GotoStatement::create_empty_statement(else_if_exit_label.clone()))
                )
            );
            statements.extend(else_if.get_statements().iter().map(|s| s.visit_mut(self)));
            statements.push(
                GotoStatement::create_empty_statement(if_exit_label.clone())
            );    
            statements.push(
                LabelStatement::create_empty_statement(else_if_exit_label.clone())
            );
        }

        if let Some(else_block) = if_then.get_else_block() {
            statements.extend(else_block.get_statements().iter().map(|s| s.visit_mut(self)));
        }

        statements.push(
            LabelStatement::create_empty_statement(if_exit_label.clone())
        );

        Statement::Block(BlockStatement::empty(statements))
    }

    fn visit_while_do_statement(&mut self, while_do: &crate::ast::WhileDoStatement) -> Statement {
        let mut statements = Vec::new();

        let continue_label = self.next_label();
        let break_label = self.next_label();

        self.continue_break_labels.push((continue_label.clone(), break_label.clone()));
        
        statements.push(
            LabelStatement::create_empty_statement(continue_label.clone())
        );
        statements.push(
            IfStatement::create_empty_statement(
                Box::new(UnaryExpression::create_empty_expression(crate::ast::UnaryOp::Not, Box::new(while_do.get_condition().clone()))),
                Box::new(GotoStatement::create_empty_statement(break_label.clone())))
        );
        statements.extend(while_do.get_statements().iter().map(|s| s.visit_mut(self)));
        statements.push(
            GotoStatement::create_empty_statement(continue_label.clone())
        );    
        statements.push(
            LabelStatement::create_empty_statement(break_label.clone())
        );
        self.continue_break_labels.pop();
        Statement::Block(BlockStatement::empty(statements))
    }

    fn visit_select_statement(&mut self, select_stmt: &SelectStatement) -> Statement {
        let mut statements = Vec::new();

        let expr = Box::new(select_stmt.get_expression().clone());
        let case_exit_label = self.next_label();

        for case_block in select_stmt.get_case_blocks() {
            let next_case_label = self.next_label();
            statements.push(
                IfStatement::create_empty_statement(
                    Box::new(BinaryExpression::create_empty_expression(crate::ast::BinOp::NotEq, expr.clone(), Box::new(case_block.get_expression().clone()))),
                    Box::new(GotoStatement::create_empty_statement(next_case_label.clone())))
            );
            statements.extend(case_block.get_statements().iter().map(|s| s.visit_mut(self)));
            statements.push(
                GotoStatement::create_empty_statement(case_exit_label.clone())
            );
            statements.push(
                LabelStatement::create_empty_statement(next_case_label.clone())
            );
        }

        if let Some(case_block) = select_stmt.get_case_else_block() {
            statements.extend(case_block.get_statements().iter().map(|s| s.visit_mut(self)));
        }

        statements.push(
            LabelStatement::create_empty_statement(case_exit_label.clone())
        );

        Statement::Block(BlockStatement::empty(statements))
    }

    fn visit_for_statement(&mut self, for_stmt: &ForStatement) -> Statement {
        let mut statements = Vec::new();

        let continue_label = self.next_label();
        let break_label = self.next_label();

        let id_expr = Box::new(IdentifierExpression::create_empty_expression(for_stmt.get_identifier().clone()));

        // init variable
        statements.push(
            LetStatement::create_empty_statement(
                for_stmt.get_identifier().clone(),
                Vec::new(),
                Box::new(for_stmt.get_start_expr().visit_mut(self))
            )
        );

        // create loop
        self.continue_break_labels.push((continue_label.clone(), break_label.clone()));
        statements.push(
            LabelStatement::create_empty_statement(continue_label.clone())
        );

        let lower_bound = Box::new(BinaryExpression::create_empty_expression(crate::ast::BinOp::Greater, id_expr.clone(), Box::new(for_stmt.get_end_expr().visit_mut(self))));
        let upper_bound = Box::new(BinaryExpression::create_empty_expression(crate::ast::BinOp::Lower, id_expr.clone(), Box::new(for_stmt.get_end_expr().visit_mut(self))));

        statements.push(
            IfStatement::create_empty_statement(
                Box::new(BinaryExpression::create_empty_expression(crate::ast::BinOp::Or, lower_bound, upper_bound)),
                Box::new(GotoStatement::create_empty_statement(break_label.clone())))
        );

        statements.extend(for_stmt.get_statements().iter().map(|s| s.visit_mut(self)));
        
        // create step & increment
        let increment = if let Some(increment) = for_stmt.get_step_expr() {
            increment.visit_mut(self)
        } else {
            Expression::Const(ConstantExpression::empty(Constant::Integer(1)))
        };
        statements.push(
            LetStatement::create_empty_statement(
                for_stmt.get_identifier().clone(),
                Vec::new(),
                Box::new(BinaryExpression::create_empty_expression(crate::ast::BinOp::Add, id_expr, Box::new(increment)))
            )
        );

        // loop & exit;
        statements.push(
            GotoStatement::create_empty_statement(continue_label.clone())
        );    
        statements.push(
            LabelStatement::create_empty_statement(break_label.clone())
        );
        self.continue_break_labels.pop();
        Statement::Block(BlockStatement::empty(statements))
    }

}
