use super::*;

pub fn do_pass3(prg: &mut Program)
{
    optimize_block(&mut prg.main_block);
    for fd in &mut prg.function_declarations {
        optimize_block(&mut fd.block);
    }
    for pd in &mut prg.procedure_declarations {
        optimize_block(&mut pd.block);
    }
}

fn optimize_block(block: &mut Block)
{
    scan_for_next(block);
    scan_do_while(block);
    scan_break_continue(block);
    scan_if_else(block);
    scan_if(block);
    strip_unused_labels(block);
    scan_elseif(block);
    strip_unused_labels(block);
}

fn get_label_index(block: &Block, from : i32, to : i32, label: &String) -> Option<usize>
{
    for j in from..to {
        if let Statement::Label(next_label) = &block.statements[j as usize] {
            if next_label == label {
                return Some(j as usize);
            }
        }
    }
    None
}

/// Check if no next, endwhile or endif is between from…to that is not starting inside from…to
fn is_balanced(block: &Block, from : usize, to : usize) -> bool
{
    let mut if_nesting = 0;
    let mut for_nesting = 0;
    let mut while_nesting = 0;

    for i in from..to {
        match &block.statements[i] {
            Statement::IfThen(_cond) => { if_nesting += 1 },
            Statement::EndIf => {
                if_nesting -= 1;
                if if_nesting < 0 {
                    return false;
                }
            }
            Statement::DoWhile(_cond) => { while_nesting += 1 },
            Statement::EndWhile => {
                while_nesting -= 1;
                if while_nesting < 0 {
                    return false;
                }
            }
            Statement::For(_iter, _from, _to, _step) => { for_nesting += 1 },
            Statement::Next => {
                for_nesting -= 1;
                if for_nesting < 0 {
                    return false;
                }
            }
            _ => {}
        }
    }

    true
}


fn scan_elseif(block: &mut Block)
{
    let mut i = 0;
    while i < block.statements.len() - 1 {
        if let Statement::Else = &block.statements[i] {
            match &block.statements[i + 1] {
                Statement::IfThen(cond) => {
                    let mut nested = 1;
                    for j in (i + 2)..(block.statements.len() - 1) {
                        match  &block.statements[j] {
                            Statement::IfThen(_cond) => { nested += 1 },
                            Statement::EndIf => {
                                nested -= 1;
                                if nested <= 0 {
                                    if !is_balanced(block, i, j - 1) { break ; }
                                    if let Statement::EndIf = &block.statements[j + 1] { // next elsif block
                                        block.statements[i] = Statement::ElseIf(Box::new((**cond).clone())); // replace 1st ELSE
                                        block.statements.remove((i + 1) as usize); // remove if
                                        block.statements.remove(j as usize); // remove 2nd EndIf
                                        i = 0;
                                    } else if let Statement::Goto(_label) = &block.statements[j - 1]  {
                                        // TODO: Check goto label needs to move out of if...then construct

                                        block.statements[i] = Statement::ElseIf(Box::new((**cond).clone())); // replace 1st ELSE
                                        block.statements[j] = Statement::Else; // Replace Endif with ELSE
                                        block.statements.remove((j - 1) as usize); // remove goto
                                        block.statements.remove((i + 1) as usize); // remove if
                                    }
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                },
                Statement::If(cond, stmt) => { // may be an end if without else block 
                    if let Statement::Goto(label) = &**stmt  {
                        if let Some(label_index) = get_label_index(block, i as i32 + 1, block.statements.len() as i32, &label) {
                            if let Statement::EndIf = block.statements[label_index - 1] {
                                // todo check if the EndIf matches the if block
                                block.statements[i] = Statement::ElseIf(Box::new(Expression::Not(Box::new((**cond).clone())))); // replace ELSE
                                block.statements.remove((i + 1) as usize); // remove if
                            }
                        }
                    }
                },
                _ => {}
            }
        }
        i += 1;
    }
}

fn scan_if(block: &mut Block)
{
    for i in 0..block.statements.len() {
        if let Statement::If(cond, stmt) = &block.statements[i] {
            if let Statement::Goto(endif_label) = &**stmt {
                let endif_label_index = get_label_index(block, i as i32 + 1, block.statements.len() as i32, endif_label);
                if let None = endif_label_index { continue; }
                if !is_balanced(block, i + 1, endif_label_index.unwrap()) { continue ; }
                block.statements[i] = Statement::IfThen(Box::new(Expression::Not(Box::new((**cond).clone())))); // replace if with if…then
                // do not remove labels they may be needed to analyze other constructs
                block.statements.insert(endif_label_index.unwrap(), Statement::EndIf); // insert Endif before label
            }
        }
    }
}

fn scan_if_else(block: &mut Block)
{
    for i in 0..block.statements.len() {
        if let Statement::If(cond, stmt) = &block.statements[i] {
            if let Statement::Goto(else_label) = &**stmt {
                let else_label_index = get_label_index(block, i as i32 + 1, block.statements.len() as i32, else_label);
                if let None = else_label_index { continue; }
                if let Statement::Goto(endif_label) = &block.statements[else_label_index.unwrap() - 1] {
                    let endif_label_index = get_label_index(block, else_label_index.unwrap() as i32 + 1, block.statements.len() as i32, endif_label);
                    if let None = endif_label_index { continue; }
                    if !is_balanced(block, i + 1, endif_label_index.unwrap() - 1) { continue ; }

                    block.statements[i] = Statement::IfThen(Box::new(Expression::Not(Box::new((**cond).clone())))); // replace if with if…then
                    block.statements[else_label_index.unwrap() - 1] = Statement::Else;   // replace goto with Else
                    block.statements.insert(endif_label_index.unwrap(), Statement::EndIf); // insert Endif before label

                    // do not remove labels they may be needed to analyze other constructs
                }
            }
        }
    }
}

fn mach_for_construct(label: &Statement, if_statement: &Statement) -> Option<(String, String, String, Expression)> // for_label, indexName, breakout_label, to_expr
{
    let for_label;
    let breakout_label;
    match label {
        Statement::Label(label) => for_label = label,
        _ => return None
    }
    match if_statement {
        Statement::If(expr, s) => {
            match &**s {
                Statement::Goto(label) => {
                    // todo: match expression
                    breakout_label = label;
                }
                _ => return None
            }

            if let Expression::Not(not_expr) = &**expr {
                if let Expression::Parens(p_expr) = &**not_expr {
                    if let Expression::BinaryExpression(_op, lvalue, rvalue) = &**p_expr {
                        // TODO: Check _op
                        if let Expression::Parens(p_expr_l) = &**lvalue {
                            if let Expression::Parens(p_expr_r) = &**rvalue {
                                if let Expression::BinaryExpression(_opl, _llvalue, lrvalue) = &**p_expr_l {
                                    if let Expression::BinaryExpression(_opr, _rlvalue, rrvalue) = &**p_expr_r {
                                        // TODO: Check _op

                                        /*     if *opl != BinOp::Add || *opr != BinOp::Add {
                                                 return None;
                                             }*/

                                        if let Expression::Parens(left_binop) = &**lrvalue {
                                            if let Expression::Parens(right_binop) = &**rrvalue {
                                                if let Expression::BinaryExpression(_opl, llvalue, lrvalue) = &**left_binop {
                                                    if let Expression::BinaryExpression(_opr, rlvalue, rrvalue) = &**right_binop {
                                                        // TODO: Check _op
                                                        if llvalue != rlvalue/*|| *opl != BinOp::Greater || *opr != BinOp::LowerEq */|| lrvalue != rrvalue {
                                                            return None;
                                                        }
                                                        return Some((for_label.clone(), llvalue.to_string(), breakout_label.clone(), (**lrvalue).clone()));
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
        _ => return None
    }

    None
}

fn scan_for_next(block: &mut Block)
{
    // FOR Header:
    // LET VAR001 = [START]
    // :LABEL002
    // IF (!(((1 < 0) + (VAR001 > [END])) & ((1 > 0) + (VAR001 <= [END])))) GOTO LABEL001
    // ...
    // LET VAR001 = VAR001 + [STEP]
    // GOTO LABEL002
    // :LABEL001
    let mut i = 0;
    while i < block.statements.len() - 2 {
        if let Statement::Let(var_name, expr) = &block.statements[i] {
            let label = &block.statements[i + 1];
            let if_statement = &block.statements[i + 2];
            let m = mach_for_construct(&label, &if_statement);

            if let Some((for_label, index_label, breakout_label, to_expr)) = m {
                let mut j = i + 1;
                let mut matching_goto = -1;
                while j < block.statements.len() {
                    if let Statement::Goto(next_label) = &block.statements[j] {
                        if *next_label == for_label {
                            if j + 1 >= block.statements.len() { continue; }
                            if let Statement::Label(next_label) = &block.statements[j + 1] {
                                if *next_label == breakout_label {
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
                match &block.statements[matching_goto as usize - 1] {
                    Statement::Let(var2, expr) => {
                        // todo: match expression as string
                        if var2 != var_name {
                            i += 1;
                            continue;
                        }

                        if let Expression::BinaryExpression(_op, lvalue, rvalue) = &**expr {
                            if *_op != BinOp::Add { continue; } // always add even if step is negative
                            if let Expression::Identifier(lstr) = &**lvalue {
                                if *lstr != index_label {
                                    i += 1;
                                    continue;
                                }
                            }
                            step_expr = (**rvalue).clone();
                        } else {
                            i += 1;
                            continue;
                        }
                    }
                    _ =>  {
                        i += 1;
                        continue;
                    }
                }

                let from_expr = Box::new(*expr.clone());
                let var_name = (*var_name).clone();
                // replace with next
                block.statements[matching_goto as usize] = Statement::Next;

                // replace for… next part
                block.statements[i] = Statement::For(var_name, from_expr, Box::new(to_expr), Box::new(step_expr)); // replace LET
                block.statements.remove((matching_goto - 1) as usize); // remove LET
                block.statements.remove((i + 1) as usize); // remove for_label
                block.statements.remove((i + 1) as usize); // remove if
                continue;
            }
        }
        i += 1;
    }
}

fn scan_possible_breaks(block: &mut Vec<Statement>, start : usize, end : usize, break_label : &str)
{
    for i in start..end {
        match &block[i] {
            Statement::While(cond, stmt) => {
                if let Statement::Goto(label) = &**stmt {
                    if label == break_label {
                        block[i] = Statement::While(Box::new((**cond).clone()), Box::new(Statement::Break));
                    }
                }
            }
            Statement::If(cond, stmt) => {
                if let Statement::Goto(label) = &**stmt {
                    if label == break_label {
                        block[i] = Statement::If(Box::new((**cond).clone()), Box::new(Statement::Break));
                    }
                }
            }
            Statement::Goto(label) => {
                if label == break_label {
                    block[i] = Statement::Break;
                }
            }
            _ => {}
        }
    }
}

fn scan_possible_continues(block: &mut Vec<Statement>, start : usize, end : usize, continue_label : &str)
{
    for i in start..end {
        match &block[i] {
            Statement::While(cond, stmt) => {
                if let Statement::Goto(label) = &**stmt {
                    if label == continue_label {
                        block[i] = Statement::While(Box::new((**cond).clone()), Box::new(Statement::Continue));
                    }
                }
            }
            Statement::If(cond, stmt) => {
                if let Statement::Goto(label) = &**stmt {
                    if label == continue_label {
                        block[i] = Statement::If(Box::new((**cond).clone()), Box::new(Statement::Continue));
                    }
                }
            }
            Statement::Goto(label) => {
                if label.to_string() == continue_label {
                    block[i] = Statement::Continue;
                }
            }
            _ => {}
        }
    }
}

fn scan_do_while(block: &mut Block)
{
    let mut i = 0;
    while i < block.statements.len() {
        let cur = &block.statements[i];
        if let Statement::Label(label) = cur {
            i += 1;
            if i >= block.statements.len() {
                break;
            }
            if let Statement::If(exp, stmt) = &block.statements[i] {
                match &(**stmt) {
                    Statement::Goto(break_label) => {
                        // search matching goto
                        let mut j = i + 1;
                        let mut matching_goto = -1;
                        while j < block.statements.len() {
                            if let Statement::Goto(next_label) = &block.statements[j] {
                                if next_label == label {
                                    if j + 1 >= block.statements.len() { continue; }
                                    if let Statement::Label(next_label) = &block.statements[j + 1] {
                                        if next_label == break_label {
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
                        //let label_cp = break_label.clone();

                        block.statements[i] = Statement::DoWhile(Box::new(Expression::Not(exp.clone())));
                        block.statements[matching_goto as usize] = Statement::EndWhile;
                        // block.statements.remove((matching_goto + 1) as usize); // breakout label
                        block.statements.remove((i - 1) as usize);

                        /*
                        scan_possible_breaks(&mut block.statements, i, matching_goto as usize, label_cp.as_str());
                        // there needs to be a better way to handle that
                        let mut continue_label = String::new();
                        if let Statement::Label(lbl) = &block.statements[matching_goto as usize - 2] {
                            continue_label = lbl.clone();
                        }

                        if continue_label.len() > 0 {
                            scan_possible_continues(&mut block.statements, i, matching_goto as usize, continue_label.as_str());
                            block.statements.remove((matching_goto - 1) as usize);
                        }
                        */
                        i = 0;
                        continue;
                    }
                    _ => {}
                }
            }
        }

        i += 1;
    }
}

fn scan_break_continue(block: &mut Block)
{
    let mut i = 0;
    while i < block.statements.len() {
        let mut start : i32 = -1;
        let mut end : i32 = -1;
        let mut nested = 0;

        match &block.statements[i] {
            Statement::DoWhile(_cond) => {
                for j in (i + 1)..block.statements.len() {
                    match  &block.statements[j] {
                        Statement::DoWhile(_cond) => { nested += 1 },
                        Statement::EndWhile => {
                            nested -= 1;
                            if nested < 0 {
                                let l1 = match &block.statements[j as usize - 1] {
                                    Statement::Label(continue_label) => continue_label.clone(),
                                    _ => String::new()
                                };
                                let l2 = match &block.statements[j as usize + 1] {
                                    Statement::Label(break_label) => break_label.clone(),
                                    _ => String::new()
                                };

                                if start < 0 {
                                    if !l1.is_empty() {
                                        scan_possible_continues(&mut block.statements, i + 1, j as usize, l1.as_str());
                                    }
                                    if !l2.is_empty() {
                                        scan_possible_breaks(&mut block.statements, i + 1, j as usize, l2.as_str());
                                    }
                                } else {
                                    if !l1.is_empty() {
                                        scan_possible_continues(&mut block.statements, i + 1, start as usize, l1.as_str());
                                        scan_possible_continues(&mut block.statements, end as usize, j as usize, l1.as_str());
                                    }
                                    if !l2.is_empty() {
                                        scan_possible_breaks(&mut block.statements, i, start as usize, l2.as_str());
                                        scan_possible_breaks(&mut block.statements, end as usize, j as usize, l2.as_str());
                                    }
                                }
                                break;
                            }
                        }
                        _ => {}
                    }
                }
            }
            Statement::For(_label, _from, _to, _step) => {
                for j in (i + 1)..block.statements.len() {
                    match  &block.statements[j] {
                        Statement::For(_label, _from, _to, _step) => {
                            if nested == 0 {
                                start = j as i32;
                            }
                            nested += 1;
                        },
                        Statement::Next => {
                            nested -= 1;
                            if nested < 0 {
                                let l1 = match &block.statements[j as usize - 1] {
                                    Statement::Label(continue_label) => continue_label.clone(),
                                    _ => String::new()
                                };
                                let l2 = match &block.statements[j as usize + 1] {
                                    Statement::Label(break_label) => break_label.clone(),
                                    _ => String::new()
                                };

                                if start < 0 {
                                    if !l1.is_empty() {
                                        scan_possible_continues(&mut block.statements, i + 1, j as usize, l1.as_str());
                                    }
                                    if !l2.is_empty() {
                                        scan_possible_breaks(&mut block.statements, i + 1, j as usize, l2.as_str());
                                    }
                                } else {
                                    if !l1.is_empty() {
                                        scan_possible_continues(&mut block.statements, i + 1, start as usize, l1.as_str());
                                        scan_possible_continues(&mut block.statements, end as usize, j as usize, l1.as_str());
                                    }
                                    if !l2.is_empty() {
                                        scan_possible_breaks(&mut block.statements, i, start as usize, l2.as_str());
                                        scan_possible_breaks(&mut block.statements, end as usize, j as usize, l2.as_str());
                                    }
                                }
                                break;
                            }
                            end = j as i32 + 1;
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        i += 1;
    }
}

fn gather_labels(stmt: &Statement, used_labels: &mut HashSet<String>)
{
    match stmt {
        Statement::If(_cond, stmt) => { gather_labels(stmt, used_labels); }
        Statement::While(_cond, stmt) => { gather_labels(stmt, used_labels); }
        Statement::Goto(label) => {
            used_labels.insert(label.clone());
         }
         Statement::Gosub(label) => {
            used_labels.insert(label.clone());
         }
          _ => {}
    }
}

fn strip_unused_labels(block: &mut Block)
{
    let mut used_labels = HashSet::new();
    for stmt in &block.statements {
        gather_labels(stmt, &mut used_labels);
    }
    let mut i = 0;
    while i < block.statements.len() {
        if let Statement::Label(label) = &block.statements[i] {
            if !used_labels.contains(label) {
                block.statements.remove(i as usize);
                continue;
            }
        }
        i += 1;
    }
}