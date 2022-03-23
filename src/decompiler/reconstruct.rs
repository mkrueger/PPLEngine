use super::{Block, Declaration, Expression, HashMap, HashSet, OpCode, Program, Statement, get_var_name};

pub fn do_pass3(prg: &mut Program) {
    optimize_block(&mut prg.main_block);
    for fd in &mut prg.function_declarations {
        optimize_block(&mut fd.block);
    }
    for pd in &mut prg.procedure_declarations {
        optimize_block(&mut pd.block);
    }
}

fn optimize_block(block: &mut Block) {
    // scan_for_next(block);
    // scan_do_while(block);
    // scan_break_continue(block);
    // scan_if_else(block);
    // scan_if(block);
    strip_unused_labels(block);
   // scan_elseif(block);
    strip_unused_labels(block);
}

fn get_label_index(block: &Block, from: i32, to: i32, label: &String) -> Option<usize> {
    for j in from..to {
        if let Statement::Label(next_label) = &block.statements[j as usize] {
            if next_label == label {
                return Some(j as usize);
            }
        }
    }
    None
}
/*
fn scan_elseif(block: &mut Block) {
    let mut i = 0;
    while i < block.statements.len() - 1 {
        if let Statement::Else = &block.statements[i] {
            match &block.statements[i + 1] {
                /*Statement::IfThen(cond) => {
                    let mut nested = 1;
                    for j in (i + 2)..(block.statements.len() - 1) {
                        match &block.statements[j] {
                            Statement::IfThen(_cond) => nested += 1,
                            Statement::EndIf => {
                                nested -= 1;
                                if nested <= 0 {
                                    if !is_balanced(block, i, j - 1) {
                                        break;
                                    }
                                    if let Statement::EndIf = &block.statements[j + 1] {
                                        // next elsif block
                                        block.statements[i] =
                                            Statement::ElseIf(Box::new((**cond).clone())); // replace 1st ELSE
                                        block.statements.remove((i + 1) as usize); // remove if
                                        block.statements.remove(j as usize); // remove 2nd EndIf
                                        i = 0;
                                    } else if let Statement::Goto(_label) = &block.statements[j - 1]
                                    {
                                        // TODO: Check goto label needs to move out of if...then construct
                                        block.statements[i] =
                                            Statement::ElseIf(Box::new((**cond).clone())); // replace 1st ELSE
                                        block.statements[j] = Statement::Else; // Replace Endif with ELSE
                                        block.statements.remove((j - 1) as usize); // remove goto
                                        block.statements.remove((i + 1) as usize);
                                        // remove if
                                    }
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                }
                Statement::If(cond, stmt) => {
                    // may be an end if without else block
                    if let Statement::Goto(label) = &**stmt {
                        if let Some(label_index) = get_label_index(
                            block,
                            i as i32 + 1,
                            block.statements.len() as i32,
                            label,
                        ) {
                            if let Statement::EndIf = block.statements[label_index - 1] {
                                // todo check if the EndIf matches the if block
                                block.statements[i] = Statement::ElseIf(Box::new(Expression::Not(
                                    Box::new((**cond).clone()),
                                ))); // replace ELSE
                                block.statements.remove((i + 1) as usize); // remove if
                            }
                        }
                    }
                }*/
                _ => {}
            }
        }
        i += 1;
    }
} */
/* 
fn scan_if(block: &mut Block) {
    for i in 0..block.statements.len() {
        if let Statement::If(cond, stmt) = &block.statements[i] {
            if let Statement::Goto(endif_label) = &**stmt {
                let endif_label_index = get_label_index(
                    block,
                    i as i32 + 1,
                    block.statements.len() as i32,
                    endif_label,
                );
                if endif_label_index.is_none() {
                    continue;
                }
                if !is_balanced(block, i + 1, endif_label_index.unwrap()) {
                    continue;
                }
                block.statements[i] =
                    Statement::IfThen(Box::new(Expression::Not(Box::new((**cond).clone())))); // replace if with if…then
                                                                                              // do not remove labels they may be needed to analyze other constructs
                block
                    .statements
                    .insert(endif_label_index.unwrap(), Statement::EndIf); // insert Endif before label
            }
        }
    }
}

fn scan_if_else(block: &mut Block) {
    for i in 0..block.statements.len() {
        if let Statement::If(cond, stmt) = &block.statements[i] {
            if let Statement::Goto(else_label) = &**stmt {
                let else_label_index = get_label_index(
                    block,
                    i as i32 + 1,
                    block.statements.len() as i32,
                    else_label,
                );
                if else_label_index == None {
                    continue;
                }
                if let Statement::Goto(endif_label) =
                    &block.statements[else_label_index.unwrap() - 1]
                {
                    let endif_label_index = get_label_index(
                        block,
                        else_label_index.unwrap() as i32 + 1,
                        block.statements.len() as i32,
                        endif_label,
                    );
                    if endif_label_index.is_none() {
                        continue;
                    }
                    if !is_balanced(block, i + 1, endif_label_index.unwrap() - 1) {
                        continue;
                    }

                    block.statements[i] =
                        Statement::IfThen(Box::new(Expression::Not(Box::new((**cond).clone())))); // replace if with if…then
                    block.statements[else_label_index.unwrap() - 1] = Statement::Else; // replace goto with Else
                    block
                        .statements
                        .insert(endif_label_index.unwrap(), Statement::EndIf); // insert Endif before label

                    // do not remove labels they may be needed to analyze other constructs
                }
            }
        }
    }
}
*/
fn mach_for_construct(
    label: &Statement,
    if_statement: &Statement,
) -> Option<(String, String, String, Expression)> // for_label, indexName, breakout_label, to_expr
{
    let breakout_label;
    let for_label = match label {
        Statement::Label(label) => label,
        _ => return None,
    };

    match if_statement {
        Statement::If(expr, s) => {
            match &**s {
                Statement::Goto(label) => {
                    // todo: match expression
                    breakout_label = label;
                }
                _ => return None,
            }

            if let Expression::Not(not_expr) = &**expr {
                if let Expression::Parens(p_expr) = &**not_expr {
                    if let Expression::BinaryExpression(_op, lvalue, rvalue) = &**p_expr {
                        // TODO: Check _op
                        if let Expression::Parens(p_expr_l) = &**lvalue {
                            if let Expression::Parens(p_expr_r) = &**rvalue {
                                if let Expression::BinaryExpression(_opl, _llvalue, lrvalue) =
                                    &**p_expr_l
                                {
                                    if let Expression::BinaryExpression(_opr, _rlvalue, rrvalue) =
                                        &**p_expr_r
                                    {
                                        // TODO: Check _op

                                        /*     if *opl != BinOp::Add || *opr != BinOp::Add {
                                            return None;
                                        }*/

                                        if let Expression::Parens(left_binop) = &**lrvalue {
                                            if let Expression::Parens(right_binop) = &**rrvalue {
                                                if let Expression::BinaryExpression(
                                                    _opl,
                                                    llvalue,
                                                    lrvalue,
                                                ) = &**left_binop
                                                {
                                                    if let Expression::BinaryExpression(
                                                        _opr,
                                                        rlvalue,
                                                        rrvalue,
                                                    ) = &**right_binop
                                                    {
                                                        // TODO: Check _op
                                                        if llvalue != rlvalue/*|| *opl != BinOp::Greater || *opr != BinOp::LowerEq */|| lrvalue != rrvalue
                                                        {
                                                            return None;
                                                        }
                                                        return Some((
                                                            for_label.clone(),
                                                            llvalue.to_string(),
                                                            breakout_label.clone(),
                                                            (**lrvalue).clone(),
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
        _ => return None,
    }

    None
}
/*
fn scan_for_next(block: &mut Block) {
    // FOR Header:
    // LET VAR001 = [START]
    // :LABEL002
    // IF (!(((1 < 0) + (VAR001 > [END])) & ((1 > 0) + (VAR001 <= [END])))) GOTO LABEL001
    // ...
    // LET VAR001 = VAR001 + [STEP]
    // GOTO LABEL002
    // :LABEL001

    if block.statements.len() < 2 {
        return;
    }
    let mut i = 0;
    while i < block.statements.len() - 2 {
        if let Statement::Let(var_name, expr) = &block.statements[i] {
            let label = &block.statements[i + 1];
            let if_statement = &block.statements[i + 2];
            let m = mach_for_construct(label, if_statement);

            if let Some((for_label, index_label, breakout_label, to_expr)) = m {
                let mut j = i + 1;
                let mut matching_goto = -1;
                while j < block.statements.len() {
                    if let Statement::Goto(next_label) = &block.statements[j] {
                        if *next_label == for_label {
                            if j + 1 >= block.statements.len() {
                                continue;
                            }
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
                if let Statement::Let(var2, expr) = &block.statements[matching_goto as usize - 1] {
                    // todo: match expression as string
                    if var2 != var_name {
                        i += 1;
                        continue;
                    }

                    if let Expression::BinaryExpression(op, lvalue, rvalue) = &**expr {
                        if *op != BinOp::Add {
                            continue;
                        } // always add even if step is negative
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
                } else {
                    i += 1;
                    continue;
                }

                let from_expr = Box::new(*expr.clone());
                let var_name = (*var_name).clone();
                // replace with next
                block.statements[matching_goto as usize] = Statement::Next;

                // replace for… next part
                if step_expr.to_string() == "1" {
                    block.statements[i] =
                        Statement::For(var_name, from_expr, Box::new(to_expr), None);
                // replace LET
                } else {
                    block.statements[i] = Statement::For(
                        var_name,
                        from_expr,
                        Box::new(to_expr),
                        Some(Box::new(step_expr)),
                    ); // replace LET
                }
                block.statements.remove((matching_goto - 1) as usize); // remove LET
                block.statements.remove((i + 1) as usize); // remove for_label
                block.statements.remove((i + 1) as usize); // remove if
                continue;
            }
        }
        i += 1;
    }
}
*/
fn scan_possible_breaks(block: &mut [Statement], start: usize, end: usize, break_label: &str) {
    for cur_stmt in &mut block[start..end] {
        match cur_stmt {
            Statement::While(cond, stmt) => {
                if let Statement::Goto(label) = &**stmt {
                    if label == break_label {
                        *cur_stmt = Statement::While(
                            Box::new((**cond).clone()),
                            Box::new(Statement::Break),
                        );
                    }
                }
            }
            Statement::If(cond, stmt) => {
                if let Statement::Goto(label) = &**stmt {
                    if label == break_label {
                        *cur_stmt =
                            Statement::If(Box::new((**cond).clone()), Box::new(Statement::Break));
                    }
                }
            }
            Statement::Goto(label) => {
                if label == break_label {
                    *cur_stmt = Statement::Break;
                }
            }
            _ => {}
        }
    }
}

#[allow(clippy::needless_range_loop)]
fn scan_possible_continues(
    block: &mut [Statement],
    start: usize,
    end: usize,
    continue_label: &str,
) {
    for i in start..end {
        match &block[i] {
            Statement::While(cond, stmt) => {
                if let Statement::Goto(label) = &**stmt {
                    if label == continue_label {
                        block[i] = Statement::While(
                            Box::new((**cond).clone()),
                            Box::new(Statement::Continue),
                        );
                    }
                }
            }
            Statement::If(cond, stmt) => {
                if let Statement::Goto(label) = &**stmt {
                    if label == continue_label {
                        block[i] = Statement::If(
                            Box::new((**cond).clone()),
                            Box::new(Statement::Continue),
                        );
                    }
                }
            }
            Statement::Goto(label) => {
                if *label == continue_label {
                    block[i] = Statement::Continue;
                }
            }
            _ => {}
        }
    }
}
/*
fn scan_do_while(block: &mut Block) {
    let mut i = 0;
    while i < block.statements.len() {
        let cur = &block.statements[i];
        if let Statement::Label(label) = cur {
            i += 1;
            if i >= block.statements.len() {
                break;
            }
            if let Statement::If(exp, stmt) = &block.statements[i] {
                if let Statement::Goto(break_label) = &(**stmt) {
                    // search matching goto
                    let mut j = i + 1;
                    let mut matching_goto = -1;
                    while j < block.statements.len() {
                        if let Statement::Goto(next_label) = &block.statements[j] {
                            if next_label == label {
                                if j + 1 >= block.statements.len() {
                                    continue;
                                }
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

                    block.statements[i] =
                        Statement::DoWhile(Box::new(Expression::Not(exp.clone())));
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
            }
        }

        i += 1;
    }
}

fn scan_break_continue(block: &mut Block) {
    let mut i = 0;
    while i < block.statements.len() {
        let mut start: i32 = -1;
        let mut end: i32 = -1;
        let mut nested = 0;

        match &block.statements[i] {
            /*
            Statement::DoWhile(_cond) => {
                for j in (i + 1)..block.statements.len() {
                    match &block.statements[j] {
                        Statement::DoWhile(_cond) => nested += 1,
                        Statement::EndWhile => {
                            nested -= 1;
                            if nested < 0 {
                                let l1 = match &block.statements[j as usize - 1] {
                                    Statement::Label(continue_label) => continue_label.clone(),
                                    _ => String::new(),
                                };
                                let l2 = match &block.statements[j as usize + 1] {
                                    Statement::Label(break_label) => break_label.clone(),
                                    _ => String::new(),
                                };

                                if start < 0 {
                                    if !l1.is_empty() {
                                        scan_possible_continues(
                                            &mut block.statements,
                                            i + 1,
                                            j as usize,
                                            l1.as_str(),
                                        );
                                    }
                                    if !l2.is_empty() {
                                        scan_possible_breaks(
                                            &mut block.statements,
                                            i + 1,
                                            j as usize,
                                            l2.as_str(),
                                        );
                                    }
                                } else {
                                    if !l1.is_empty() {
                                        scan_possible_continues(
                                            &mut block.statements,
                                            i + 1,
                                            start as usize,
                                            l1.as_str(),
                                        );
                                        scan_possible_continues(
                                            &mut block.statements,
                                            end as usize,
                                            j as usize,
                                            l1.as_str(),
                                        );
                                    }
                                    if !l2.is_empty() {
                                        scan_possible_breaks(
                                            &mut block.statements,
                                            i,
                                            start as usize,
                                            l2.as_str(),
                                        );
                                        scan_possible_breaks(
                                            &mut block.statements,
                                            end as usize,
                                            j as usize,
                                            l2.as_str(),
                                        );
                                    }
                                }
                                break;
                            }
                        }
                        _ => {}
                    }
                }
            }
            */
            Statement::For(_label, _from, _to, _step) => {
                for j in (i + 1)..block.statements.len() {
                    match &block.statements[j] {
                        Statement::For(_label, _from, _to, _step) => {
                            if nested == 0 {
                                start = j as i32;
                            }
                            nested += 1;
                        }
                        Statement::Next => {
                            nested -= 1;
                            if nested < 0 {
                                let l1 = match &block.statements[j as usize - 1] {
                                    Statement::Label(continue_label) => continue_label.clone(),
                                    _ => String::new(),
                                };
                                let l2 = match &block.statements[j as usize + 1] {
                                    Statement::Label(break_label) => break_label.clone(),
                                    _ => String::new(),
                                };

                                if start < 0 {
                                    if !l1.is_empty() {
                                        scan_possible_continues(
                                            &mut block.statements,
                                            i + 1,
                                            j as usize,
                                            l1.as_str(),
                                        );
                                    }
                                    if !l2.is_empty() {
                                        scan_possible_breaks(
                                            &mut block.statements,
                                            i + 1,
                                            j as usize,
                                            l2.as_str(),
                                        );
                                    }
                                } else {
                                    if !l1.is_empty() {
                                        scan_possible_continues(
                                            &mut block.statements,
                                            i + 1,
                                            start as usize,
                                            l1.as_str(),
                                        );
                                        scan_possible_continues(
                                            &mut block.statements,
                                            end as usize,
                                            j as usize,
                                            l1.as_str(),
                                        );
                                    }
                                    if !l2.is_empty() {
                                        scan_possible_breaks(
                                            &mut block.statements,
                                            i,
                                            start as usize,
                                            l2.as_str(),
                                        );
                                        scan_possible_breaks(
                                            &mut block.statements,
                                            end as usize,
                                            j as usize,
                                            l2.as_str(),
                                        );
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
*/
fn gather_labels(stmt: &Statement, used_labels: &mut HashSet<String>) {
    match stmt {
        Statement::If(_cond, stmt) => {
            gather_labels(stmt, used_labels);
        }
        Statement::While(_cond, stmt) => {
            gather_labels(stmt, used_labels);
        }
        Statement::Goto(label) | Statement::Gosub(label) => {
            used_labels.insert(label.clone());
        }
        _ => {}
    }
}

fn strip_unused_labels(block: &mut Block) {
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

pub fn do_pass4(prg: &mut Program) {
    rename_variables(&mut prg.main_block, &mut prg.declarations);
    for fd in &mut prg.function_declarations {
        rename_variables(&mut fd.block, &mut fd.variable_declarations);
    }
    for pd in &mut prg.procedure_declarations {
        rename_variables(&mut pd.block, &mut pd.variable_declarations);
    }
}

const INDEX_VARS: [&str; 4] = ["i", "j", "k", "l"];

fn scan_replace_vars(
    stmt: &Statement,
    rename_map: &mut HashMap<String, String>,
    index: &mut i32,
    file_names: &mut i32,
) {
    match stmt {
        Statement::For(v, _, _, _, _) => {
            let var_name = get_var_name(&*v);
            if !rename_map.contains_key(&var_name) && *index < INDEX_VARS.len() as i32 {
                rename_map.insert(var_name, INDEX_VARS[*index as usize].to_string());

                *index += 1;
            }
        }
        Statement::If(_, s) |
        Statement::While(_, s) => scan_replace_vars(s, rename_map, index, file_names),
        Statement::Call(def, parameters) => match &def.opcode {
            OpCode::FOPEN => {
                let var_name = get_var_name(&parameters[1]);
                rename_map.entry(var_name).or_insert_with(|| {
                    *file_names += 1;
                    format!("fileName{}", file_names)
                });
            }
            OpCode::DELETE => {
                let var_name = get_var_name(&parameters[0]);
                rename_map.entry(var_name).or_insert_with(|| {
                    *file_names += 1;
                    format!("fileName{}", file_names)
                });
            }
            OpCode::DISPFILE => {
                let var_name = get_var_name(&parameters[0]);
                rename_map.entry(var_name).or_insert_with(|| {
                    *file_names += 1;
                    format!("fileName{}", file_names)
                });
            }
            _ => {}
        },
        _ => {}
    }
}

fn rename_variables(block: &mut Block, declarations: &mut Vec<Declaration>) {
    let mut rename_map = HashMap::new();
    let mut index = 0;
    let mut file_names = 0;

    for stmt in &block.statements {
        scan_replace_vars(stmt, &mut rename_map, &mut index, &mut file_names);
    }
/* 
    for decl in declarations {
        match decl {
            Declaration::Variable(_, name) |
            Declaration::Variable1(_, name, _) |
            Declaration::Variable2(_, name, _, _) |
            Declaration::Variable3(_, name, _, _, _) => {
                if rename_map.contains_key(name) {
                    *name = rename_map.get(name).unwrap().to_string();
                }
            }
            _ => {}
        }
    }
*/
    for stmt in &mut block.statements {
        replace_in_statement(stmt, &rename_map);
    }
}

fn replace_in_statement(stmt: &mut Statement, rename_map: &HashMap<String, String>) {
    match stmt {
        Statement::While(expr, stmt2) | Statement::If(expr, stmt2) => {
            replace_in_expression(expr, rename_map);
            replace_in_statement(stmt2, rename_map);
        }
       // Statement::IfThen(expr) |
       // Statement::ElseIf(expr) |
       // Statement::DoWhile(expr) => replace_in_expression(expr, rename_map),
        Statement::For(expr1, expr2, expr3, _, _ ) => {
            replace_in_expression(expr1, rename_map);
            replace_in_expression(expr2, rename_map);
            replace_in_expression(expr3, rename_map);
        }
        Statement::Let(expr, rvalue) => {
            replace_in_expression(expr, rename_map);
            replace_in_expression(rvalue, rename_map);
        }
        Statement::ProcedureCall(_, parameters) |
        Statement::Call(_, parameters) => {
            for p in parameters {
                replace_in_expression(p, rename_map);
            }
        }
        _ => {}
    }
}

fn replace_in_expression(repl_expr: &mut Expression, rename_map: &HashMap<String, String>) {
    match repl_expr {
        Expression::Identifier(id) => {
            if rename_map.contains_key(id) {
                *id = rename_map.get(id).unwrap().to_string();
            }
        }
        Expression::Parens(expr) |
        Expression::Not(expr) |
        Expression::Minus(expr) => replace_in_expression(expr, rename_map),        Expression::FunctionCall(_, parameters) => {
            for p in parameters {
                replace_in_expression(p, rename_map);
            }
        }

        Expression::BinaryExpression(_, l_value, r_value) => {
            replace_in_expression(l_value, rename_map);
            replace_in_expression(r_value, rename_map);
        }
        Expression::Dim1(expr1, expr2) => {
            replace_in_expression(expr1, rename_map);
            replace_in_expression(expr2, rename_map);
        }
        Expression::Dim2(expr1, expr2, expr3) => {
            replace_in_expression(expr1, rename_map);
            replace_in_expression(expr2, rename_map);
            replace_in_expression(expr3, rename_map);
        }
        Expression::Dim3(expr1, expr2, expr3, expr4) => {
            replace_in_expression(expr1, rename_map);
            replace_in_expression(expr2, rename_map);
            replace_in_expression(expr3, rename_map);
            replace_in_expression(expr4, rename_map);
        }
        _ => {}
    }
}
