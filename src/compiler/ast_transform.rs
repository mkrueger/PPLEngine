use crate::ast::{Program, Statement, Expression, BinOp, Constant};

pub fn transform_ast(prg: &mut Program)
{
    for f in &mut prg.function_implementations {
        transform_block(&mut f.block.statements);
    }
    for f in &mut prg.procedure_implementations {
        transform_block(&mut f.block.statements);
    }
    transform_block(&mut prg.main_block.statements);
}

fn transform_block(statements: &mut Vec<Statement>)  {
    let mut i = 0;
    let mut labels = 1;

    while i < statements.len() {
        match &mut statements[i].clone() {
            Statement::Block(stmts) => {
                statements.splice(i..=i, stmts.iter().cloned());
                continue;
            }
            Statement::DoWhile(cond, stmts) => {
                statements.remove(i);

                let break_label = format!("label{}", labels);
                labels += 1;
                let continue_label = format!("label{}", labels);
                labels += 1;
                let loop_label = format!("label{}", labels);
                labels += 1;

                statements.insert(i, Statement::Label(break_label.clone()));
                statements.insert(i, Statement::Goto(loop_label.clone()));
                statements.insert(i, Statement::Label(continue_label.clone()));
                
                statements.splice(i..i, stmts.iter().cloned());

                scan_possible_breaks( stmts, &break_label);
                scan_possible_continues( stmts, &continue_label);
                // block
                statements.insert(i, Statement::If(
                    Box::new(Expression::Not(Box::new(Expression::Parens(cond.clone())))),
                    Box::new(Statement::Goto(break_label.clone()))
                ));

                statements.insert(i, Statement::Label(loop_label));
                continue;
            }

            Statement::For(var_name, from, to, opt_step, stmts) => {
                statements.remove(i);

                let break_label = format!("label{}", labels);
                labels += 1;
                let continue_label = format!("label{}", labels);
                labels += 1;
                let loop_label = format!("label{}", labels);
                labels += 1;

                statements.insert(i, Statement::Label(break_label.clone()));
                statements.insert(i, Statement::Goto(loop_label.clone()));
                let step = *opt_step.clone().unwrap_or(Box::new(Expression::Const(Constant::Integer(1))));
                statements.insert(i, Statement::Let(var_name.clone(), Box::new(Expression::BinaryExpression(BinOp::Add, var_name.clone(), Box::new(step.clone())))));
                statements.insert(i, Statement::Label(continue_label.clone()));
                
                scan_possible_breaks( stmts, &break_label);
                scan_possible_continues( stmts, &continue_label);
                statements.splice(i..i, stmts.iter().cloned());

                statements.insert(i, Statement::If(
                    Box::new(Expression::Not(
                        Box::new(Expression::Parens(Box::new(Expression::BinaryExpression(
                            BinOp::Or, 
                            Box::new(Expression::Parens(Box::new(Expression::BinaryExpression(
                                BinOp::And, 
                                Box::new(Expression::Parens(Box::new(Expression::BinaryExpression(BinOp::Lower, 
                                    Box::new(step.clone()), 
                                    Box::new(Expression::Const(Constant::Integer(0)))
                                )))),
                                Box::new(Expression::Parens(Box::new(Expression::BinaryExpression(BinOp::GreaterEq, 
                                    var_name.clone(), 
                                    to.clone()
                                ))))
                            )))),
                            Box::new(Expression::Parens(Box::new(Expression::BinaryExpression(
                                BinOp::And, 
                                Box::new(Expression::Parens(Box::new(Expression::BinaryExpression(BinOp::GreaterEq, 
                                    Box::new(step.clone()), 
                                    Box::new(Expression::Const(Constant::Integer(0)))
                                )))),
                                Box::new(Expression::Parens(Box::new(Expression::BinaryExpression(BinOp::LowerEq, 
                                    var_name.clone(), 
                                    to.clone()
                                ))))
                            ))))
                        ))))
                    )),

                    Box::new(Statement::Goto(break_label.clone()))
                ));

                statements.insert(i, Statement::Label(loop_label));
                statements.insert(i, Statement::Let(var_name.clone(), from.clone()));

            }
            
            Statement::IfThen(cond, stmts, opt_else_if, opt_else) => {
                statements.remove(i);
                let mut if_exit_label = format!("label{}", labels);
                labels += 1;
                let else_exit_label = format!("label{}", labels);
                labels += 1;
                statements.insert(i, Statement::Label(else_exit_label.clone()));

                if let Some(else_stmts) = opt_else {
                    statements.splice(i..i, else_stmts.iter().cloned());
                }

                if let Some(else_if) = opt_else_if {
                    for n in 0..else_if.len() {
                        let ef = &else_if[n];
                        if n != else_if.len() - 1 || opt_else.is_some()  {
                            statements.insert(i, Statement::Label(if_exit_label.clone()));
                            statements.insert(i, Statement::Goto(else_exit_label.clone()));
                        }

                        statements.splice(i..i, ef.block.iter().cloned());

                        statements.insert(i, Statement::If(
                            Box::new(Expression::Not(Box::new(Expression::Parens(ef.cond.clone())))),
                            Box::new(Statement::Goto(if_exit_label.clone()))
                        ));

                        if_exit_label = format!("label{}", labels);
                        labels += 1;
                    }
                }

                statements.insert(i, Statement::Label(if_exit_label.clone()));
                if opt_else.is_some()  || opt_else_if.is_some() {
                    statements.insert(i, Statement::Goto(else_exit_label.clone()));
                }
                statements.splice(i..i, stmts.iter().cloned());

                statements.insert(i, Statement::If(
                    Box::new(Expression::Not(Box::new(Expression::Parens(cond.clone())))),
                    Box::new(Statement::Goto(if_exit_label.clone()))
                ));

            }
            _ => {}
        }

        i += 1;
    }

    crate::decompiler::reconstruct::strip_unused_labels(statements);

}

fn scan_possible_breaks(block: &mut [Statement], break_label: &String) {
    for cur_stmt in block {
        match cur_stmt {
            Statement::If(_, stmt) => {
                if let Statement::Break = &**stmt {
                    *stmt = Box::new(Statement::Goto(break_label.clone()));
                }
            }
            Statement::Break => {
                *cur_stmt = Statement::Goto(break_label.clone());
            }
            _ => {}
        }
    }
}

fn scan_possible_continues(block: &mut [Statement], break_label: &String) {
    for cur_stmt in block {
        match cur_stmt {
            Statement::If(_, stmt) => {
                if let Statement::Continue = &**stmt {
                    *stmt = Box::new(Statement::Goto(break_label.clone()));
                }
            }
            Statement::Continue => {
                *cur_stmt = Statement::Goto(break_label.clone());
            }
            _ => {}
        }
    }
}
