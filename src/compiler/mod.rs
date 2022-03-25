use crate::ast::{Program, Statement, Expression};

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
                    Box::new(Expression::Not(cond.clone())),
                    Box::new(Statement::Goto(break_label.clone()))
                ));

                statements.insert(i, Statement::Label(loop_label));

                

                continue;

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
            Statement::If(cond, stmt) => {
                if let Statement::Break = &**stmt {
                    *cur_stmt = Statement::Goto(break_label.clone());
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
            Statement::If(cond, stmt) => {
                if let Statement::Continue = &**stmt {
                    *cur_stmt = Statement::Goto(break_label.clone());
                }
            }
            Statement::Continue => {
                *cur_stmt = Statement::Goto(break_label.clone());
            }
            _ => {}
        }
    }
}
