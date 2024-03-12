use crate::ast::{
    BinOp, BinaryExpression, Constant, ConstantExpression, ElseBlock, ElseIfBlock, GotoStatement,
    IdentifierExpression, IfStatement, IfThenStatement, LabelStatement, LetStatement,
    ParensExpression, Program, Statement, UnaryExpression,
};

pub fn transform_ast(prg: &mut Program) {
    for f in &mut prg.function_implementations {
        transform_block(&mut f.block.statements);
    }
    for f in &mut prg.procedure_implementations {
        transform_block(&mut f.block.statements);
    }
    transform_block(&mut prg.main_block.statements);
}

fn transform_block(statements: &mut Vec<Statement>) {
    translate_select_case(statements);

    let mut i = 0;
    let mut labels = 1;

    while i < statements.len() {
        match &mut statements[i].clone() {
            Statement::Block(block_stmt) => {
                statements.splice(i..=i, block_stmt.get_statements().iter().cloned());
                continue;
            }
            Statement::WhileDo(while_do_stmt) => {
                statements.remove(i);

                let break_label = format!("label{labels}");
                labels += 1;
                let continue_label = format!("label{labels}");
                labels += 1;
                let loop_label = format!("label{labels}");
                labels += 1;

                statements.insert(
                    i,
                    LabelStatement::create_empty_statement(break_label.clone()),
                );
                statements.insert(i, GotoStatement::create_empty_statement(loop_label.clone()));
                statements.insert(
                    i,
                    LabelStatement::create_empty_statement(continue_label.clone()),
                );

                scan_possible_breaks(while_do_stmt.get_statements_mut(), &break_label);
                scan_possible_continues(while_do_stmt.get_statements_mut(), &continue_label);

                statements.splice(i..i, while_do_stmt.get_statements().iter().cloned());

                // block
                statements.insert(
                    i,
                    IfStatement::create_empty_statement(
                        Box::new(UnaryExpression::create_empty_expression(
                            crate::ast::UnaryOp::Not,
                            Box::new(ParensExpression::create_empty_expression(Box::new(
                                while_do_stmt.get_condition().clone(),
                            ))),
                        )),
                        Box::new(GotoStatement::create_empty_statement(break_label.clone())),
                    ),
                );

                statements.insert(i, LabelStatement::create_empty_statement(loop_label));
                continue;
            }

            Statement::For(for_stmt) => {
                statements.remove(i);

                let break_label = format!("label{labels}");
                labels += 1;
                let continue_label = format!("label{labels}");
                labels += 1;
                let loop_label = format!("label{labels}");
                labels += 1;

                statements.insert(
                    i,
                    LabelStatement::create_empty_statement(break_label.clone()),
                );
                statements.insert(i, GotoStatement::create_empty_statement(loop_label.clone()));
                let step = *for_stmt.get_step_expr().clone().unwrap_or(Box::new(
                    ConstantExpression::create_empty_expression(Constant::Integer(1)),
                ));
                statements.insert(
                    i,
                    LetStatement::create_empty_statement(
                        for_stmt.get_identifier().clone(),
                        Vec::new(),
                        Box::new(BinaryExpression::create_empty_expression(
                            BinOp::Add,
                            Box::new(IdentifierExpression::create_empty_expression(
                                for_stmt.get_identifier().clone(),
                            )),
                            Box::new(step.clone()),
                        )),
                    ),
                );
                statements.insert(
                    i,
                    LabelStatement::create_empty_statement(continue_label.clone()),
                );

                scan_possible_breaks(for_stmt.get_statements_mut(), &break_label);
                scan_possible_continues(for_stmt.get_statements_mut(), &continue_label);
                statements.splice(i..i, for_stmt.get_statements().iter().cloned());

                statements.insert(
                    i,
                    IfStatement::create_empty_statement(
                        Box::new(UnaryExpression::create_empty_expression(
                            crate::ast::UnaryOp::Not,
                            Box::new(ParensExpression::create_empty_expression(Box::new(
                                BinaryExpression::create_empty_expression(
                                    BinOp::Or,
                                    Box::new(ParensExpression::create_empty_expression(Box::new(
                                        BinaryExpression::create_empty_expression(
                                            BinOp::And,
                                            Box::new(ParensExpression::create_empty_expression(
                                                Box::new(BinaryExpression::create_empty_expression(
                                                    BinOp::Lower,
                                                    Box::new(step.clone()),
                                                    Box::new(
                                                        ConstantExpression::create_empty_expression(
                                                            Constant::Integer(0),
                                                        ),
                                                    ),
                                                )),
                                            )),
                                            Box::new(ParensExpression::create_empty_expression(
                                                Box::new(BinaryExpression::create_empty_expression(
                                                    BinOp::GreaterEq,
                                                    Box::new(IdentifierExpression::create_empty_expression(for_stmt.get_identifier().clone())),
                                                    Box::new(for_stmt.get_end_expr().clone()),
                                                )),
                                            )),
                                        ),
                                    ))),
                                    Box::new(ParensExpression::create_empty_expression(Box::new(
                                        BinaryExpression::create_empty_expression(
                                            BinOp::And,
                                            Box::new(ParensExpression::create_empty_expression(
                                                Box::new(BinaryExpression::create_empty_expression(
                                                    BinOp::GreaterEq,
                                                    Box::new(step.clone()),
                                                    Box::new(
                                                        ConstantExpression::create_empty_expression(
                                                            Constant::Integer(0),
                                                        ),
                                                    ),
                                                )),
                                            )),
                                            Box::new(ParensExpression::create_empty_expression(
                                                Box::new(BinaryExpression::create_empty_expression(
                                                    BinOp::LowerEq,
                                                    Box::new(IdentifierExpression::create_empty_expression(for_stmt.get_identifier().clone())),
                                                    Box::new(for_stmt.get_end_expr().clone()),
                                                )),
                                            )),
                                        ),
                                    ))),
                                ),
                            ))),
                        )),
                        Box::new(GotoStatement::create_empty_statement(break_label.clone())),
                    ),
                );

                statements.insert(i, LabelStatement::create_empty_statement(loop_label));

                statements.insert(
                    i,
                    LetStatement::create_empty_statement(
                        for_stmt.get_identifier().clone(),
                        Vec::new(),
                        Box::new(for_stmt.get_start_expr().clone()),
                    ),
                );
            }

            Statement::IfThen(if_then_stmt) => {
                statements.remove(i);
                let mut if_exit_label = format!("label{labels}");
                labels += 1;
                let else_exit_label = format!("label{labels}");
                labels += 1;

                statements.insert(
                    i,
                    LabelStatement::create_empty_statement(else_exit_label.clone()),
                );

                if let Some(else_stmts) = if_then_stmt.get_else_block() {
                    statements.splice(i..i, else_stmts.get_statements().iter().cloned());
                }

                if !if_then_stmt.get_else_if_blocks().is_empty() {
                    for n in (0..if_then_stmt.get_else_if_blocks().len()).rev() {
                        if n == if_then_stmt.get_else_if_blocks().len() - 1
                            && if_then_stmt.get_else_block().is_none()
                        {
                            if_exit_label = else_exit_label.clone();
                        }

                        let ef = &if_then_stmt.get_else_if_blocks()[n];
                        if n != if_then_stmt.get_else_if_blocks().len() - 1
                            || if_then_stmt.get_else_block().is_some()
                        {
                            statements.insert(
                                i,
                                LabelStatement::create_empty_statement(if_exit_label.clone()),
                            );
                            statements.insert(
                                i,
                                GotoStatement::create_empty_statement(else_exit_label.clone()),
                            );
                        }

                        statements.splice(i..i, ef.get_statements().iter().cloned());

                        statements.insert(
                            i,
                            IfStatement::create_empty_statement(
                                Box::new(UnaryExpression::create_empty_expression(
                                    crate::ast::UnaryOp::Not,
                                    Box::new(ParensExpression::create_empty_expression(Box::new(
                                        ef.get_condition().clone(),
                                    ))),
                                )),
                                Box::new(GotoStatement::create_empty_statement(
                                    if_exit_label.clone(),
                                )),
                            ),
                        );

                        if_exit_label = format!("label{labels}");
                        labels += 1;
                    }
                }

                statements.insert(
                    i,
                    LabelStatement::create_empty_statement(if_exit_label.clone()),
                );
                if if_then_stmt.get_else_block().is_some()
                    || !if_then_stmt.get_else_if_blocks().is_empty()
                {
                    statements.insert(
                        i,
                        GotoStatement::create_empty_statement(else_exit_label.clone()),
                    );
                }
                statements.splice(i..i, if_then_stmt.get_statements().iter().cloned());
                statements.insert(
                    i,
                    IfStatement::create_empty_statement(
                        Box::new(UnaryExpression::create_empty_expression(
                            crate::ast::UnaryOp::Not,
                            Box::new(ParensExpression::create_empty_expression(Box::new(
                                if_then_stmt.get_condition().clone(),
                            ))),
                        )),
                        Box::new(GotoStatement::create_empty_statement(if_exit_label.clone())),
                    ),
                );
            }
            _ => {}
        }

        i += 1;
    }

    crate::decompiler::reconstruct::strip_unused_labels(statements);
}

fn translate_select_case(statements: &mut [Statement]) {
    let mut i = 0;

    while i < statements.len() {
        if let Statement::Select(select_stmt) = &statements[i] {
            let if_statements = select_stmt
                .get_case_else_block()
                .as_ref()
                .map(|blocks| ElseBlock::empty(blocks.get_statements().clone()));

            statements[i] = IfThenStatement::create_empty_statement(
                Box::new(BinaryExpression::create_empty_expression(
                    BinOp::Eq,
                    Box::new(select_stmt.get_expr().clone()),
                    Box::new(select_stmt.get_case_blocks()[0].get_expr().clone()),
                )),
                select_stmt.get_case_blocks()[0].get_statements().clone(),
                select_stmt.get_case_blocks()[1..]
                    .iter()
                    .map(|block| {
                        ElseIfBlock::empty(
                            Box::new(BinaryExpression::create_empty_expression(
                                BinOp::Eq,
                                Box::new(select_stmt.get_expr().clone()),
                                Box::new(block.get_expr().clone()),
                            )),
                            block.get_statements().clone(),
                        )
                    })
                    .collect(),
                if_statements,
            );
        }
        i += 1;
    }
}

fn scan_possible_breaks(block: &mut [Statement], break_label: impl Into<String>) {
    let break_label = break_label.into();
    for cur_stmt in block {
        match cur_stmt {
            Statement::If(if_stmt) => {
                if let Statement::Break(_) = if_stmt.get_statement() {
                    if_stmt.set_statement(Box::new(GotoStatement::create_empty_statement(
                        break_label.clone(),
                    )));
                }
            }
            Statement::Break(_) => {
                *cur_stmt = GotoStatement::create_empty_statement(break_label.clone());
            }
            _ => {}
        }
    }
}

fn scan_possible_continues(block: &mut [Statement], break_label: impl Into<String>) {
    let break_label = break_label.into();
    for cur_stmt in block {
        match cur_stmt {
            Statement::If(if_stmt) => {
                if let Statement::Continue(_) = if_stmt.get_statement() {
                    if_stmt.set_statement(Box::new(GotoStatement::create_empty_statement(
                        break_label.clone(),
                    )));
                }
            }
            Statement::Continue(_) => {
                *cur_stmt = GotoStatement::create_empty_statement(break_label.clone());
            }
            _ => {}
        }
    }
}
