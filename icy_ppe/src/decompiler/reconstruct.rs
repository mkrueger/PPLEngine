use crate::ast::{
    BinOp, BreakStatement, CaseBlock, ContinueStatement, ElseBlock, ElseIfBlock, ForStatement,
    IdentifierExpression, IfStatement, IfThenStatement, Implementations, SelectStatement,
    UnaryExpression, WhileDoStatement,
};

use super::{Expression, HashMap, HashSet, Program, Statement};

pub fn do_pass3(prg: &mut Program) {
    optimize_block(&mut prg.statements);
    for fd in &mut prg.implementations {
        match fd {
            Implementations::Function(fd) => {
                optimize_block(fd.get_statements_mut());
            }
            Implementations::Procedure(pd) => {
                optimize_block(pd.get_statements_mut());
            }
            Implementations::Comment(_) => {}
        }
    }
}

fn optimize_loops(statements: &mut Vec<Statement>) {
    scan_for_next(statements);
    scan_do_while(statements);
}

fn optimize_ifs(statements: &mut Vec<Statement>) {
    scan_if_else(statements);
    scan_if(statements);
}

fn optimize_block(statements: &mut Vec<Statement>) {
    optimize_loops(statements);
    optimize_ifs(statements);
    scan_select_statements(statements);

    strip_unused_labels(statements);
}

fn get_label_index(statements: &[Statement], from: i32, to: i32, label: &String) -> Option<usize> {
    for j in from..to {
        if let Statement::Label(next_label) = &statements[j as usize] {
            if next_label.get_label() == label {
                return Some(j as usize);
            }
        }
    }
    None
}

fn get_first(s: &[Statement]) -> Option<&Statement> {
    if s.is_empty() {
        return None;
    }
    if let Statement::If(_) = s[0] {
        return Some(&s[0]);
    }
    if s.len() < 2 {
        return None;
    }

    Some(&s[1])
}

fn scan_select_statements(statements: &mut [Statement]) {
    let mut i = 0;
    while i < statements.len() {
        if let Statement::IfThen(if_then_stmt) = statements[i].clone() {
            if if_then_stmt.get_else_block().is_none() {
                i += 1;
                continue;
            }
            let Expression::Binary(bin_expr) =
                Statement::try_boolean_conversion(if_then_stmt.get_condition())
            else {
                i += 1;
                continue;
            };
            if bin_expr.get_op() != BinOp::Eq {
                i += 1;
                continue;
            }

            let mut skip = false;
            for if_else_block in if_then_stmt.get_else_if_blocks() {
                let Expression::Binary(bin_expr2) =
                    Statement::try_boolean_conversion(if_else_block.get_condition())
                else {
                    skip = true;
                    break;
                };
                if bin_expr2.get_op() != BinOp::Eq {
                    skip = true;
                    break;
                }
                if bin_expr.get_left_expression() != bin_expr2.get_left_expression() {
                    skip = true;
                    break;
                }
            }

            if skip {
                i += 1;
                continue;
            }

            let mut case_blocks = Vec::new();

            if !if_then_stmt.get_statements().is_empty() {
                case_blocks.push(CaseBlock::empty(
                    Box::new(bin_expr.get_right_expression().clone()),
                    if_then_stmt.get_statements().clone(),
                ));
            }

            for if_else_block in if_then_stmt.get_else_if_blocks() {
                let Expression::Binary(bin_expr2) =
                    Statement::try_boolean_conversion(if_else_block.get_condition())
                else {
                    i += 1;
                    continue;
                };

                if bin_expr2.get_op() != BinOp::Eq {
                    i += 1;
                    continue;
                }
                case_blocks.push(CaseBlock::empty(
                    Box::new(bin_expr2.get_right_expression().clone()),
                    if_else_block.get_statements().clone(),
                ));
            }
            let case_else_block = if_then_stmt.get_else_block().as_ref().map(|else_block| {
                CaseBlock::empty(
                    Box::new(IdentifierExpression::create_empty_expression("Else")),
                    else_block.get_statements().clone(),
                )
            });
            statements[i] = SelectStatement::create_empty_statement(
                Box::new(bin_expr.get_left_expression().clone()),
                case_blocks,
                case_else_block,
            );
        }
        i += 1;
    }
}

fn scan_if_else(statements: &mut Vec<Statement>) {
    let mut i = 0;
    while i < statements.len() {
        if let Statement::If(if_stmt) = statements[i].clone() {
            if let Statement::Goto(else_label) = if_stmt.get_statement() {
                let else_label_index = get_label_index(
                    statements,
                    i as i32 + 1,
                    statements.len() as i32,
                    else_label.get_label(),
                );
                if else_label_index.is_none() {
                    i += 1;
                    continue;
                }

                let endif_label;
                let endif_label_index = if let Statement::Goto(end_label) =
                    &statements[else_label_index.unwrap() - 1]
                {
                    endif_label = end_label.get_label().clone();
                    get_label_index(
                        statements,
                        else_label_index.unwrap() as i32 + 1,
                        statements.len() as i32,
                        end_label.get_label(),
                    )
                } else {
                    endif_label = String::new();
                    None
                };

                if endif_label_index.is_none() {
                    i += 1;
                    continue;
                }

                let endif_label_index = endif_label_index.unwrap();

                let mut elseif_blocks = Vec::new();
                let mut else_block = None;
                let if_block;

                if let Some(else_label_index) = else_label_index {
                    let mut else_block2: Vec<Statement> = statements
                        .drain(else_label_index..endif_label_index)
                        .collect();

                    if_block = statements.drain((i + 1)..(else_label_index - 1)).collect();

                    while let Some(Statement::If(if_stmt)) = get_first(&else_block2) {
                        if let Statement::Goto(label) = if_stmt.get_statement() {
                            let label_idx = if *label.get_label() == endif_label {
                                // last elseif case.
                                else_block2.len() as usize
                            } else {
                                let label_idx = get_label_index(
                                    &else_block2,
                                    1,
                                    else_block2.len() as i32,
                                    label.get_label(),
                                );
                                if label_idx.is_none() {
                                    break;
                                }
                                let mut label_idx = label_idx.unwrap();

                                if let Statement::Goto(label2) = &else_block2[label_idx - 1] {
                                    if *endif_label != *label2.get_label() {
                                        break;
                                    }
                                    label_idx -= 1;
                                }
                                label_idx
                            };
                            let cond = Box::new(UnaryExpression::create_empty_expression(
                                crate::ast::UnaryOp::Not,
                                Box::new(if_stmt.get_condition().clone()),
                            ));
                            let mut block: Vec<Statement> =
                                else_block2.drain(0..label_idx).collect();

                            let a = if let Statement::Label(_) = block[0] {
                                1
                            } else {
                                0
                            };
                            block.remove(a);

                            elseif_blocks.push(ElseIfBlock::empty(cond, block));

                            if !else_block2.is_empty() {
                                if let Statement::Goto(label2) = &else_block2[0] {
                                    if *endif_label == *label2.get_label() {
                                        else_block2.remove(0);
                                    }
                                }
                            }
                        } else {
                            break;
                        }
                    }

                    if !else_block2.is_empty() {
                        optimize_loops(&mut else_block2);
                        optimize_ifs(&mut else_block2);
                        else_block = Some(ElseBlock::empty(else_block2));
                    }
                } else {
                    if_block = statements.drain((i + 1)..(endif_label_index - 1)).collect();
                }

                for block in &mut elseif_blocks {
                    optimize_loops(block.get_statements_mut());
                    optimize_ifs(block.get_statements_mut());
                }

                statements[i] = IfThenStatement::create_empty_statement(
                    Box::new(UnaryExpression::create_empty_expression(
                        crate::ast::UnaryOp::Not,
                        Box::new(if_stmt.get_condition().clone()),
                    )),
                    if_block,
                    elseif_blocks,
                    else_block,
                ); // replace if with if…then

                statements.remove(i + 1); // remove goto
                                          // do not remove labels they may be needed to analyze other constructs
            }
        }

        i += 1;
    }
}

fn scan_if(statements: &mut Vec<Statement>) {
    let mut i = 0;
    while i < statements.len() {
        if let Statement::If(if_stmt) = statements[i].clone() {
            if let Statement::Goto(endif_label) = if_stmt.get_statement() {
                let endif_label_index = get_label_index(
                    statements,
                    i as i32 + 1,
                    statements.len() as i32,
                    endif_label.get_label(),
                );
                if endif_label_index.is_none() {
                    i += 1;
                    continue;
                }

                let mut statements2 = statements
                    .drain((i + 1)..(endif_label_index.unwrap() as usize))
                    .collect();
                optimize_loops(&mut statements2);
                optimize_ifs(&mut statements2);

                statements[i] = IfThenStatement::create_empty_statement(
                    Box::new(UnaryExpression::create_empty_expression(
                        crate::ast::UnaryOp::Not,
                        Box::new(if_stmt.get_condition().clone()),
                    )),
                    statements2,
                    Vec::new(),
                    None,
                );
                // replace if with if…then
                // do not remove labels they may be needed to analyze other constructs
            }
        }

        i += 1;
    }
}

fn mach_for_construct(
    label: &Statement,
    if_statement: &Statement,
) -> Option<(String, String, String, Expression)> // for_label, indexName, breakout_label, to_expr
{
    let breakout_label;
    let Statement::Label(for_label) = label else {
        return None;
    };

    match if_statement {
        Statement::If(if_stmt) => {
            match if_stmt.get_statement() {
                Statement::Goto(goto_stmt) => {
                    // todo: match expression
                    breakout_label = goto_stmt.get_label();
                }
                _ => return None,
            }

            if let Expression::Unary(not_expr) = if_stmt.get_condition() {
                if not_expr.get_op() == crate::ast::UnaryOp::Not {
                    if let Expression::Parens(p_expr) = not_expr.get_expression() {
                        if let Expression::Binary(bin_op) = p_expr.get_expression() {
                            // TODO: Check _op
                            if let Expression::Parens(p_expr_l) = bin_op.get_left_expression() {
                                if let Expression::Parens(p_expr_r) = bin_op.get_right_expression()
                                {
                                    if let Expression::Binary(lbin_op) = p_expr_l.get_expression() {
                                        if let Expression::Binary(rbin_op) =
                                            p_expr_r.get_expression()
                                        {
                                            // TODO: Check _op

                                            /*     if *opl != BinOp::Add || *opr != BinOp::Add {
                                                return None;
                                            }*/

                                            if let Expression::Parens(left_binop) =
                                                lbin_op.get_right_expression()
                                            {
                                                if let Expression::Parens(right_binop) =
                                                    rbin_op.get_right_expression()
                                                {
                                                    if let Expression::Binary(bin_op1) =
                                                        left_binop.get_expression()
                                                    {
                                                        if let Expression::Binary(bin_op2) =
                                                            right_binop.get_expression()
                                                        {
                                                            // TODO: Check _op
                                                            if bin_op1.get_left_expression() != bin_op2.get_right_expression()/*|| *opl != BinOp::Greater || *opr != BinOp::LowerEq */|| bin_op1.get_right_expression() != bin_op2.get_right_expression()
                                                            {
                                                                return None;
                                                            }
                                                            return Some((
                                                                for_label.get_label().clone(),
                                                                bin_op1
                                                                    .get_left_expression()
                                                                    .to_string(),
                                                                breakout_label.clone(),
                                                                bin_op1
                                                                    .get_right_expression()
                                                                    .clone(),
                                                            ));
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        _ => return None,
    }

    None
}

fn scan_for_next(statements: &mut Vec<Statement>) {
    // FOR Header:
    // LET VAR001 = [START]
    // :LABEL002
    // IF (!(((1 < 0) + (VAR001 > [END])) & ((1 > 0) + (VAR001 <= [END])))) GOTO LABEL001
    // ...
    // LET VAR001 = VAR001 + [STEP]
    // GOTO LABEL002
    // :LABEL001

    if statements.len() < 2 {
        return;
    }
    let mut i = 0;
    while i < statements.len() - 2 {
        if let Statement::Let(outer_let) = &statements[i] {
            let label = &statements[i + 1];
            let if_statement = &statements[i + 2];
            let m = mach_for_construct(label, if_statement);

            if let Some((for_label, index_label, breakout_label, to_expr)) = m {
                let mut j = i + 1;
                let mut matching_goto = -1;
                while j < statements.len() {
                    if let Statement::Goto(next_label) = &statements[j] {
                        if *next_label.get_label() == for_label {
                            if j + 1 >= statements.len() {
                                continue;
                            }
                            if let Statement::Label(next_label) = &statements[j + 1] {
                                if *next_label.get_label() == breakout_label {
                                    matching_goto = j as i32;
                                    break;
                                }
                            }
                            i += 1;
                            continue;
                        }
                    }
                    j += 1;
                }
                if matching_goto < 0 {
                    i += 1;
                    continue;
                }

                let step_expr;
                if let Statement::Let(inner_let) = &statements[matching_goto as usize - 1] {
                    // todo: match expression as string
                    if inner_let.get_identifier() != outer_let.get_identifier() {
                        i += 1;
                        continue;
                    }

                    if let Expression::Binary(bin_expr) = inner_let.get_value_expression() {
                        if bin_expr.get_op() != BinOp::Add {
                            continue;
                        } // always add even if step is negative
                        if let Expression::Identifier(lstr) = bin_expr.get_left_expression() {
                            if *lstr.get_identifier() != index_label {
                                i += 1;
                                continue;
                            }
                        }
                        step_expr = bin_expr.get_right_expression().clone();
                    } else {
                        i += 1;
                        continue;
                    }
                } else {
                    i += 1;
                    continue;
                }

                let from_expr = Box::new(outer_let.get_value_expression().clone());
                let var_name = outer_let.get_identifier().clone();

                statements.remove((matching_goto - 1) as usize); // remove LET
                statements.remove((matching_goto - 1) as usize); // remove matching goto
                statements.remove(i + 1); // remove for_label
                statements.remove(i + 1); // remove if

                let mut statements2 = statements
                    .drain((i + 1)..(matching_goto as usize - 3))
                    .collect();
                optimize_loops(&mut statements2);
                scan_possible_breaks(&mut statements2, &breakout_label);
                // there needs to be a better way to handle that
                if !statements2.is_empty() {
                    let mut continue_label = String::new();
                    if let Statement::Label(lbl) = &statements2.last().unwrap() {
                        continue_label = lbl.get_label().clone();
                    }
                    if !continue_label.is_empty() {
                        scan_possible_continues(&mut statements2, continue_label.as_str());
                    }
                }
                optimize_ifs(&mut statements2);

                if step_expr.to_string() == "1" {
                    statements[i] = ForStatement::create_empty_statement(
                        var_name,
                        from_expr,
                        Box::new(to_expr),
                        None,
                        statements2,
                    );
                } else {
                    statements[i] = ForStatement::create_empty_statement(
                        var_name,
                        from_expr,
                        Box::new(to_expr),
                        Some(Box::new(step_expr)),
                        statements2,
                    );
                }
                continue;
            }
        }
        i += 1;
    }
}

fn scan_possible_breaks(block: &mut [Statement], break_label: &str) {
    for cur_stmt in block {
        match cur_stmt {
            Statement::If(if_stmt) => {
                if let Statement::Goto(label) = if_stmt.get_statement() {
                    if label.get_label() == break_label {
                        *cur_stmt = IfStatement::create_empty_statement(
                            Box::new(if_stmt.get_condition().clone()),
                            Box::new(BreakStatement::create_empty_statement()),
                        );
                    }
                }
            }
            Statement::Goto(label) => {
                if label.get_label() == break_label {
                    *cur_stmt = BreakStatement::create_empty_statement();
                }
            }
            _ => {}
        }
    }
}

#[allow(clippy::needless_range_loop)]
fn scan_possible_continues(block: &mut [Statement], continue_label: &str) {
    for cur_stmt in block {
        match cur_stmt {
            Statement::If(if_stmt) => {
                if let Statement::Goto(label) = if_stmt.get_statement() {
                    if label.get_label() == continue_label {
                        *cur_stmt = IfStatement::create_empty_statement(
                            Box::new(if_stmt.get_condition().clone()),
                            Box::new(ContinueStatement::create_empty_statement()),
                        );
                    }
                }
            }
            Statement::Goto(label) => {
                if *label.get_label() == continue_label {
                    *cur_stmt = ContinueStatement::create_empty_statement();
                }
            }
            _ => {}
        }
    }
}

fn scan_do_while(statements: &mut Vec<Statement>) {
    scan_do_while2(statements);

    let mut i = 0;
    'main: while i < statements.len() {
        let cur = &statements[i];
        if let Statement::Label(label) = cur {
            i += 1;
            if i >= statements.len() {
                break;
            }
            if let Statement::If(if_stmt) = statements[i].clone() {
                if let Statement::Goto(break_label) = if_stmt.get_statement() {
                    // search matching goto
                    let mut j = i + 1;
                    let mut matching_goto = -1;
                    while j < statements.len() {
                        if let Statement::Goto(next_label) = &statements[j] {
                            if next_label.get_label() == label.get_label() {
                                if j + 1 >= statements.len() {
                                    i += 1;
                                    continue 'main;
                                }
                                if let Statement::Label(next_label) = &statements[j + 1] {
                                    if next_label.get_label() == break_label.get_label() {
                                        matching_goto = j as i32;
                                        break;
                                    }
                                }
                                j += 1;
                                continue;
                            }
                        }
                        j += 1;
                    }
                    if matching_goto < 0 {
                        i += 1;
                        continue;
                    }
                    let label_cp = break_label.get_label().clone();

                    let mut statements2 = statements
                        .drain((i + 1)..(matching_goto as usize))
                        .collect();
                    optimize_loops(&mut statements2);

                    scan_possible_breaks(&mut statements2, label_cp.as_str());
                    // there needs to be a better way to handle that
                    let mut continue_label = String::new();
                    if let Statement::Label(lbl) = &statements2.last().unwrap() {
                        continue_label = lbl.get_label().clone();
                    }
                    if !continue_label.is_empty() {
                        scan_possible_continues(&mut statements2, continue_label.as_str());
                    }
                    optimize_ifs(&mut statements2);

                    statements[i] = WhileDoStatement::create_empty_statement(
                        Box::new(UnaryExpression::create_empty_expression(
                            crate::ast::UnaryOp::Not,
                            Box::new(if_stmt.get_condition().clone()),
                        )),
                        statements2,
                    );
                    statements.remove(i + 1);
                    statements.remove(i - 1);

                    i = 0;
                    continue;
                }
            }
        }
        i += 1;
    }
}

fn scan_do_while2(_statements: &mut [Statement]) {
    /*
        let mut i = 0;

        while i < statements.len() {
            let cur = &statements[i];
            if let Statement::Label(while_label) = cur {
                i += 1;
                if i >= statements.len() {
                    break;
                }

                // search matching goto
                let mut j = i + 1;

                let mut matching_goto = 0;
                while j < statements.len() {
                    if let Statement::Goto(check_label) = &statements[j] {
                        if j + 1 >= statements.len() {
                            break;
                        }
                        if check_label == while_label {
                            matching_goto = j;
                            break;
                        }
                    }
                    j += 1;
                }
                if matching_goto <= i {
                    i += 1;
                    continue;
                }
                // test for break label
                if let Statement::If(exp, stmt) = statements[matching_goto - 1].clone() {
                    if let Statement::Goto(break_label) = &*stmt {
                        if let Statement::Label(next_label) = &statements[matching_goto + 1] {
                            // check break label
                            if next_label != break_label {
                                i += 1;
                                continue;
                            }
                            let label_cp = break_label.clone();
                            let mut block: Vec<Statement> =
                                statements.drain((i + 1)..=matching_goto).collect();
                            block.pop(); // pop goto start of while
                            block.pop(); // pop goto start of while
                            optimize_loops(&mut block);
                            scan_possible_breaks(&mut block, label_cp.as_str());
                            // there needs to be a better way to handle that
                            let mut continue_label = String::new();
                            if let Some(Statement::Label(lbl)) = &block.last() {
                                continue_label = lbl.clone();
                            }
                            if !continue_label.is_empty() {
                                scan_possible_continues(&mut block, continue_label.as_str());
                            }
                            optimize_ifs(&mut block);
                            statements[i] = Statement::DoWhile(
                                Box::new(Expression::UnaryExpression(
                                    crate::ast::UnaryOp::Not,
                                    exp.clone(),
                                )),
                                block,
                            );
                            i = 0;
                            continue;
                        }
                    }
                }
            }
            i += 1;
        }

    */
}

fn gather_labels(stmt: &Statement, used_labels: &mut HashSet<String>) {
    match stmt {
        Statement::If(if_stmt) => {
            gather_labels(if_stmt.get_statement(), used_labels);
        }
        Statement::While(while_stmt) => {
            gather_labels(while_stmt.get_statement(), used_labels);
        }
        Statement::IfThen(if_then_stmt) => {
            for stmt in if_then_stmt.get_statements() {
                gather_labels(stmt, used_labels);
            }
            for block in if_then_stmt.get_else_if_blocks() {
                for stmt in block.get_statements() {
                    gather_labels(stmt, used_labels);
                }
            }
            if let Some(stmts) = if_then_stmt.get_else_block() {
                for stmt in stmts.get_statements() {
                    gather_labels(stmt, used_labels);
                }
            }
        }
        Statement::Select(select_stmt) => {
            for block in select_stmt.get_case_blocks() {
                for stmt in block.get_statements() {
                    gather_labels(stmt, used_labels);
                }
            }
            if let Some(stmts) = select_stmt.get_case_else_block() {
                for stmt in stmts.get_statements() {
                    gather_labels(stmt, used_labels);
                }
            }
        }
        Statement::Block(block_stmt) => {
            for stmt in block_stmt.get_statements() {
                gather_labels(stmt, used_labels);
            }
        }
        Statement::WhileDo(while_do_stmt) => {
            for stmt in while_do_stmt.get_statements() {
                gather_labels(stmt, used_labels);
            }
        }
        Statement::For(for_stmt) => {
            for stmt in for_stmt.get_statements() {
                gather_labels(stmt, used_labels);
            }
        }
        Statement::Goto(goto_stmt) => {
            used_labels.insert(goto_stmt.get_label().clone());
        }
        Statement::Gosub(gosub_stmt) => {
            used_labels.insert(gosub_stmt.get_label().clone());
        }
        _ => {}
    }
}

pub fn strip_unused_labels(statements: &mut Vec<Statement>) {
    let mut used_labels = HashSet::new();
    let mut i = 0;
    while i < statements.len() {
        gather_labels(&statements[i], &mut used_labels);
        i += 1;
    }
    strip_unused_labels2(statements, &used_labels);
}

fn strip_unused_labels2(statements: &mut Vec<Statement>, used_labels: &HashSet<String>) {
    let mut i = 0;
    while i < statements.len() {
        if let Statement::Label(label) = &statements[i] {
            if !used_labels.contains(label.get_label()) {
                statements.remove(i);
                continue;
            }
        }

        match &mut statements[i] {
            Statement::IfThen(if_then_stmt) => {
                strip_unused_labels2(if_then_stmt.get_statements_mut(), used_labels);
                for else_if_block in if_then_stmt.get_else_if_blocks_mut() {
                    strip_unused_labels2(else_if_block.get_statements_mut(), used_labels);
                }
                if let Some(else_block) = if_then_stmt.get_else_block_mut() {
                    strip_unused_labels2(else_block.get_statements_mut(), used_labels);
                }
            }
            Statement::Select(case_stmt) => {
                for block in case_stmt.get_case_blocks_mut() {
                    strip_unused_labels2(block.get_statements_mut(), used_labels);
                }
                if let Some(stmts) = case_stmt.get_case_else_block_mut() {
                    strip_unused_labels2(stmts.get_statements_mut(), used_labels);
                }
            }
            Statement::Block(block_stmt) => {
                strip_unused_labels2(block_stmt.get_statements_mut(), used_labels);
            }
            Statement::WhileDo(while_do_stmt) => {
                strip_unused_labels2(while_do_stmt.get_statements_mut(), used_labels);
            }
            Statement::For(for_stmt) => {
                strip_unused_labels2(for_stmt.get_statements_mut(), used_labels);
            }
            _ => {}
        }
        i += 1;
    }
}

pub fn do_pass4(prg: &mut Program) {
    rename_variables(&mut prg.statements);
    for fd in &mut prg.implementations {
        match fd {
            Implementations::Function(fd) => {
                rename_variables(fd.get_statements_mut());
            }
            Implementations::Procedure(pd) => {
                rename_variables(pd.get_statements_mut());
            }
            Implementations::Comment(_) => {}
        }
    }
}

const _INDEX_VARS: [&str; 4] = ["i", "j", "k", "l"];

fn scan_replace_vars(
    _stmt: &Statement,
    _rename_map: &mut HashMap<String, String>,
    _index: &mut i32,
    _file_names: &mut i32,
) {
    /*
        match stmt {
            Statement::For(v, _, _, _, stmts) => {
                let var_name = get_var_name(v);
                if !rename_map.contains_key(&var_name) && *index < INDEX_VARS.len() as i32 {
                    rename_map.insert(var_name, INDEX_VARS[*index as usize].to_string());

                    *index += 1;
                }
                for s in stmts {
                    scan_replace_vars(s, rename_map, index, file_names);
                }
            }

            Statement::DoWhile(_, stmts) | Statement::Block(stmts) => {
                for s in stmts {
                    scan_replace_vars(s, rename_map, index, file_names);
                }
            }
            Statement::IfThen(_, stmts, else_ifs, else_stmts) => {
                for s in stmts {
                    scan_replace_vars(s, rename_map, index, file_names);
                }
                for block in else_ifs {
                    for s in &block.block {
                        scan_replace_vars(s, rename_map, index, file_names);
                    }
                }
                if let Some(stmts) = else_stmts {
                    for s in stmts {
                        scan_replace_vars(s, rename_map, index, file_names);
                    }
                }
            }

            Statement::If(_, s) | Statement::While(_, s) => {
                scan_replace_vars(s, rename_map, index, file_names);
            }
            Statement::Call(def, parameters) => match &def.opcode {
                OpCode::FOPEN => {
                    let var_name = get_var_name(&parameters[1]);
                    rename_map.entry(var_name).or_insert_with(|| {
                        *file_names += 1;
                        format!("fileName{file_names}")
                    });
                }
                OpCode::DELETE => {
                    let var_name = get_var_name(&parameters[0]);
                    rename_map.entry(var_name).or_insert_with(|| {
                        *file_names += 1;
                        format!("fileName{file_names}")
                    });
                }
                OpCode::DISPFILE => {
                    let var_name = get_var_name(&parameters[0]);
                    rename_map.entry(var_name).or_insert_with(|| {
                        *file_names += 1;
                        format!("fileName{file_names}")
                    });
                }
                _ => {}
            },
            _ => {}
        }
    */
}

fn rename_variables(block: &mut Vec<Statement>) {
    let mut rename_map = HashMap::new();
    let mut index = 0;
    let mut file_names = 0;

    for stmt in block.iter() {
        scan_replace_vars(stmt, &mut rename_map, &mut index, &mut file_names);
    }

    for stmt in block {
        replace_in_statement(stmt, &rename_map);
    }
}

fn replace_in_statement(stmt: &mut Statement, rename_map: &HashMap<String, String>) {
    match stmt {
        Statement::If(if_stmt) => {
            replace_in_expression(if_stmt.get_condition_mut(), rename_map);
            replace_in_statement(if_stmt.get_statement_mut(), rename_map);
        }
        Statement::While(while_stmt) => {
            replace_in_expression(while_stmt.get_condition_mut(), rename_map);
            replace_in_statement(while_stmt.get_statement_mut(), rename_map);
        }
        Statement::WhileDo(while_do_stmt) => {
            replace_in_expression(while_do_stmt.get_condition_mut(), rename_map);
            for s in while_do_stmt.get_statements_mut() {
                replace_in_statement(s, rename_map);
            }
        }
        Statement::Block(block_stmt) => {
            for s in block_stmt.get_statements_mut() {
                replace_in_statement(s, rename_map);
            }
        }
        Statement::IfThen(if_then_stmt) => {
            replace_in_expression(if_then_stmt.get_condition_mut(), rename_map);
            for s in if_then_stmt.get_statements_mut() {
                replace_in_statement(s, rename_map);
            }
            for else_if_block in if_then_stmt.get_else_if_blocks_mut() {
                for s in else_if_block.get_statements_mut() {
                    replace_in_statement(s, rename_map);
                }
            }
            if let Some(else_block) = if_then_stmt.get_else_block_mut() {
                for s in else_block.get_statements_mut() {
                    replace_in_statement(s, rename_map);
                }
            }
        }

        Statement::For(for_stmt) => {
            if rename_map.contains_key(for_stmt.get_identifier()) {
                for_stmt.set_identifier(rename_map.get(for_stmt.get_identifier()).unwrap());
            }
            replace_in_expression(for_stmt.get_start_expr_mut(), rename_map);
            replace_in_expression(for_stmt.get_end_expr_mut(), rename_map);

            for stmt in for_stmt.get_statements_mut() {
                replace_in_statement(stmt, rename_map);
            }
        }
        Statement::Let(let_stmt) => {
            if let Some(name) = rename_map.get(&let_stmt.get_identifier().to_ascii_uppercase()) {
                let_stmt.set_identifier(name.clone());
            }
            for arg in let_stmt.get_arguments_mut() {
                replace_in_expression(arg, rename_map);
            }
            replace_in_expression(let_stmt.get_value_expression_mut(), rename_map);
        }
        Statement::Call(call_stmt) => {
            for p in call_stmt.get_arguments_mut() {
                replace_in_expression(p, rename_map);
            }
        }
        _ => {}
    }
}

fn replace_in_expression(repl_expr: &mut Expression, rename_map: &HashMap<String, String>) {
    match repl_expr {
        Expression::Identifier(id) => {
            if rename_map.contains_key(id.get_identifier()) {
                *id = IdentifierExpression::empty(rename_map.get(id.get_identifier()).unwrap());
            }
        }
        Expression::Parens(expr) => {
            replace_in_expression(expr.get_expression_mut(), rename_map);
        }
        Expression::Unary(expr) => {
            replace_in_expression(expr.get_expression_mut(), rename_map);
        }

        Expression::Binary(expr) => {
            replace_in_expression(expr.get_left_expressionmut(), rename_map);
            replace_in_expression(expr.get_right_expressionmut(), rename_map);
        }

        Expression::FunctionCall(expr) => {
            if rename_map.contains_key(expr.get_identifier()) {
                expr.set_identifier(rename_map.get(expr.get_identifier()).unwrap().to_string());
            }
            for p in expr.get_arguments_mut() {
                replace_in_expression(p, rename_map);
            }
        }
        Expression::Const(_) => {}
    }
}
