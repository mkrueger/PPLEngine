use crate::ast::{BinOp, ElseIfBlock, VarInfo};

use super::{Block, Declaration, Expression, HashMap, HashSet, OpCode, Program, Statement, get_var_name};

pub fn do_pass3(prg: &mut Program) {
    optimize_block(&mut prg.main_block.statements);
    for fd in &mut prg.function_implementations {
        optimize_block(&mut fd.block.statements);
    }
    for pd in &mut prg.procedure_implementations {
        optimize_block(&mut pd.block.statements);
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
    strip_unused_labels(statements);
}

fn get_label_index(statements: &Vec<Statement>, from: i32, to: i32, label: &String) -> Option<usize> {
    for j in from..to {
        if let Statement::Label(next_label) = &statements[j as usize] {
            if next_label == label {
                return Some(j as usize);
            }
        }
    }
    None
}

fn get_first(s: &Vec<Statement>) -> Option<&Statement> {

    if s.is_empty() { return None; }
    if let Statement::If(_, _) = s[0] { return Some(&s[0]); }
    if s.len() < 2 { return None; }

    Some(&s[1])
}

fn scan_if_else(statements: &mut Vec<Statement>) {
    let mut i = 0;
    while i < statements.len() {
        if let Statement::If(cond, stmt) = statements[i].clone() {
            if let Statement::Goto(else_label) = &*stmt {
                let else_label_index = get_label_index(
                    statements,
                    i as i32 + 1,
                    statements.len() as i32,
                    else_label,
                );
                if else_label_index.is_none() { i += 1; continue; }

                let endif_label;
                let endif_label_index = if let Statement::Goto(end_label) = &statements[else_label_index.unwrap() - 1]
                {
                    endif_label = end_label.clone();
                    get_label_index(
                        statements,
                        else_label_index.unwrap() as i32 + 1,
                        statements.len() as i32,
                        end_label,
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

                let mut elseif_blocks = None;
                let mut else_block = None;
                let if_block;

                if let Some(else_label_index) = else_label_index {
                    if_block = statements.drain((i + 1)..(else_label_index - 1 )).collect();

                    let mut elseif_blocks2 = Vec::new();

                    let mut else_block2: Vec<Statement> = statements.drain((else_label_index)..(endif_label_index)).collect();

                    while let Some(Statement::If(cond, stmt)) = get_first(&else_block2)  {
                        if let Statement::Goto(label) = &**stmt {
                            let label_idx = get_label_index(
                                &else_block2,
                                1,
                                else_block2.len() as i32,
                                label,
                            );
                            if label_idx.is_none() { break ;}
                            let mut label_idx = label_idx.unwrap();

                            if let Statement::Goto(label2) = &else_block2[label_idx - 1] {
                                if *endif_label != *label2 {
                                    break;
                                }
                                label_idx -= 1;
                            }

                            let cond = Box::new(Expression::Not(Box::new(*cond.clone())));
                            let mut block: Vec<Statement> = else_block2.drain(0..label_idx).collect();
                            
                            let a = if let Statement::Label(_) = block[0] {
                                1 
                            } else { 0 } ;
                            block.remove(a);


                            elseif_blocks2.push(ElseIfBlock {
                                cond,
                                block
                            });
                            
                            if let Statement::Goto(label2) = &else_block2[0]  {
                                if *endif_label == *label2 {
                                    else_block2.remove(0);
                                }
                            }
                        } else {
                            break;
                        }
                    }
                    if !elseif_blocks2.is_empty() {
                        elseif_blocks = Some(elseif_blocks2);
                    }
                    
                    if !else_block2.is_empty() {
                        optimize_loops( &mut else_block2);
                        optimize_ifs( &mut else_block2);
                        else_block = Some(else_block2);
                    }
                } else {
                    if_block = statements.drain((i + 1)..(endif_label_index - 1 )).collect();
                }


                if let Some(else_block) = &mut elseif_blocks { 
                    for block in else_block {
                        optimize_loops( &mut block.block);
                        optimize_ifs( &mut block.block);
                    }
                }

                statements[i] = Statement::IfThen(
                    Box::new(Expression::Not(Box::new((*cond).clone()))),
                    if_block,
                    elseif_blocks,
                    else_block
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

        if let Statement::If(cond, stmt) = statements[i].clone() {
            if let Statement::Goto(endif_label) = &*stmt {
                let endif_label_index = get_label_index(
                    statements,
                    i as i32 + 1,
                    statements.len() as i32,
                    endif_label,
                );
                if endif_label_index.is_none() {
                    i += 1;
                    continue;
                }

                let mut statements2 = statements.drain((i + 1)..(endif_label_index.unwrap() as usize)).collect();
                optimize_loops( &mut statements2);
                optimize_ifs( &mut statements2);

                statements[i] =
                    Statement::IfThen(
                        Box::new(Expression::Not(cond)),
                        statements2,
                        None,
                        None
                    ); // replace if with if…then
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
        if let Statement::Let(var_name, expr) = &statements[i] {
            let label = &statements[i + 1];
            let if_statement = &statements[i + 2];
            let m = mach_for_construct(label, if_statement);

            if let Some((for_label, index_label, breakout_label, to_expr)) = m {
                let mut j = i + 1;
                let mut matching_goto = -1;
                while j < statements.len() {
                    if let Statement::Goto(next_label) = &statements[j] {
                        if *next_label == for_label {
                            if j + 1 >= statements.len() {
                                continue;
                            }
                            if let Statement::Label(next_label) = &statements[j + 1] {
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
                if let Statement::Let(var2, expr) = &statements[matching_goto as usize - 1] {
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
                
                statements.remove((matching_goto - 1) as usize); // remove LET
                statements.remove((matching_goto - 1) as usize); // remove matching goto
                statements.remove((i + 1) as usize); // remove for_label
                statements.remove((i + 1) as usize); // remove if

                let mut statements2 = statements.drain((i + 1)..(matching_goto as usize - 3)).collect();
                optimize_loops( &mut statements2);
                scan_possible_breaks(&mut statements2, &breakout_label);
                // there needs to be a better way to handle that
                if statements2.len() > 0 {
                    let mut continue_label = String::new();
                    if let Statement::Label(lbl) = &statements2.last().unwrap() {
                        continue_label = lbl.clone();
                    }
                    if continue_label.len() > 0 {
                        scan_possible_continues(&mut statements2, continue_label.as_str());
                    }
                }
                optimize_ifs( &mut statements2);

                if step_expr.to_string() == "1" {
                    statements[i] = Statement::For(Box::new(var_name.as_expr()), from_expr, Box::new(to_expr), None, statements2);
                } else {
                    statements[i] = Statement::For(
                        Box::new(var_name.as_expr()),
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
    continue_label: &str,
) {
    for cur_stmt in block {
        match cur_stmt {
            Statement::If(cond, stmt) => {
                if let Statement::Goto(label) = &**stmt {
                    if label == continue_label {
                        *cur_stmt =
                            Statement::If(Box::new((**cond).clone()), Box::new(Statement::Continue));
                    }
                }
            }
            Statement::Goto(label) => {
                if *label == continue_label {
                    *cur_stmt = Statement::Continue;
                }
            }
            _ => {}
        }
    }
}

fn scan_do_while(statements: &mut Vec<Statement>) {
    let mut i = 0;
    'main: while i < statements.len() {
        let cur = &statements[i];
        if let Statement::Label(label) = cur {
            i += 1;
            if i >= statements.len() {
                break;
            }
            if let Statement::If(exp, stmt) = statements[i].clone() {
                if let Statement::Goto(break_label) = &*stmt {
                    // search matching goto
                    let mut j = i + 1;
                    let mut matching_goto = -1;
                    while j < statements.len() {
                        if let Statement::Goto(next_label) = &statements[j] {
                            if next_label == label {
                                if j + 1 >= statements.len() {
                                    i += 1;
                                    continue 'main;
                                }
                                if let Statement::Label(next_label) = &statements[j + 1] {
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
                    let label_cp = break_label.clone();

                    let mut statements2 = statements.drain((i + 1)..(matching_goto as usize)).collect();
                    optimize_loops( &mut statements2);

                    scan_possible_breaks(&mut statements2, label_cp.as_str());
                    // there needs to be a better way to handle that
                    let mut continue_label = String::new();
                    if let Statement::Label(lbl) = &statements2.last().unwrap() {
                        continue_label = lbl.clone();
                    }
                    if continue_label.len() > 0 {
                        scan_possible_continues(&mut statements2, continue_label.as_str());
                    }
                    optimize_ifs( &mut statements2);

                    statements[i] = Statement::DoWhile(Box::new(Expression::Not(exp.clone())), statements2);
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

fn gather_labels(stmt: &Statement, used_labels: &mut HashSet<String>) {
    match stmt {
        Statement::If(_, stmt) => {
            gather_labels(stmt, used_labels);
        }
        Statement::IfThen(_, stmts, else_ifs,  else_stmts) => {
            for stmt in stmts {
                gather_labels(stmt, used_labels);
            }
            if let Some(blocks) = else_ifs {
                for block in blocks {
                    for stmt in &block.block {
                        gather_labels(stmt, used_labels);
                    }
                }
            }
            if let Some(stmts) = else_stmts {
                for stmt in stmts {
                    gather_labels(stmt, used_labels);
                }
            }
        }
        Statement::DoWhile(_, stmts) => {
            for stmt in stmts {
                gather_labels(stmt, used_labels);
            }
        }
        Statement::Block(stmts) |
        Statement::For(_, _, _, _, stmts) => {
            for stmt in stmts {
                gather_labels(stmt, used_labels);
            }
        }
        Statement::While(_, stmt) => {
            gather_labels(stmt, used_labels);
        }
        Statement::Goto(label) | Statement::Gosub(label) => {
            used_labels.insert(label.clone());
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
            if !used_labels.contains(label) {
                statements.remove(i as usize);
                continue;
            }
        }

        match &mut statements[i] {
            Statement::IfThen(_, stmts, else_ifs,  else_stmts) => {

                strip_unused_labels2(stmts, used_labels);
                if let Some(blocks) = else_ifs {
                    for block in blocks {
                        strip_unused_labels2(&mut block.block, used_labels);
                    }
                }
                if let Some(stmts) = else_stmts {
                    strip_unused_labels2(stmts, used_labels);
                }
            }
            Statement::DoWhile(_, stmts) |
            Statement::Block(stmts) |
            Statement::For(_, _, _, _, stmts) => {
                strip_unused_labels2(stmts, used_labels);
            }
            _ => {}
        }
        i += 1;
    }
}

pub fn do_pass4(prg: &mut Program) {
    rename_variables(&mut prg.main_block, &mut prg.declarations);
    for fd in &mut prg.function_implementations {
        rename_variables(&mut fd.block, &mut fd.variable_declarations);
    }
    for pd in &mut prg.procedure_implementations {
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
        Statement::For(v, _, _, _, stmts) => {
            let var_name = get_var_name(&*v);
            if !rename_map.contains_key(&var_name) && *index < INDEX_VARS.len() as i32 {
                rename_map.insert(var_name, INDEX_VARS[*index as usize].to_string());

                *index += 1;
            }
            for s in stmts {
                scan_replace_vars(s, rename_map, index, file_names);
            }
        }

        Statement::DoWhile(_, stmts) |
        Statement::Block(stmts)  => {
            for s in stmts {
                scan_replace_vars(s, rename_map, index, file_names);
            }
        }
        Statement::IfThen(_, stmts, else_ifs,  else_stmts) => {
            for s in stmts {
                scan_replace_vars(s, rename_map, index, file_names);
            }
            if let Some(blocks) = else_ifs {
                for block in blocks {
                    for s in &block.block {
                        scan_replace_vars(s, rename_map, index, file_names);
                    }
                }
            }
            if let Some(stmts) = else_stmts {
                for s in stmts {
                    scan_replace_vars(s, rename_map, index, file_names);
                }
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

    for decl in declarations {
        match decl {
            Declaration::Variable(_, infos)=> {
                for info in infos { 
                    let name = info.get_name();
                    if let Some(v) = rename_map.get(name) {
                        info.rename(v.clone());
                    }
                }
            }
            _ => {}
        }
    }

    for stmt in &mut block.statements {
        replace_in_statement(stmt, &rename_map);
    }
}

fn replace_in_statement(stmt: &mut Statement, rename_map: &HashMap<String, String>) {
    match stmt {
        Statement::While(expr, stmt2) | 
        Statement::If(expr, stmt2) => {
            replace_in_expression(expr, rename_map);
            replace_in_statement(stmt2, rename_map);
        }

        Statement::DoWhile(cond, stmts) => {
            replace_in_expression(cond, rename_map);
            for s in stmts {
                replace_in_statement(s, rename_map);
            }
        }
        Statement::Block(stmts)  => {
            for s in stmts {
                replace_in_statement(s, rename_map);
            }
        }
        Statement::IfThen(cond, stmts, else_ifs,  else_stmts) => {
            replace_in_expression(cond, rename_map);
            for s in stmts {
                replace_in_statement(s, rename_map);
            }
            if let Some(blocks) = else_ifs {
                for block in blocks {
                    for s in &mut block.block {
                        replace_in_statement(s, rename_map);
                    }
                }
            }
            if let Some(stmts) = else_stmts {
                for s in stmts {
                    replace_in_statement(s, rename_map);
                }
            }
        }

        Statement::For(expr1, expr2, expr3, _, stmts) => {
            replace_in_expression(expr1, rename_map);
            replace_in_expression(expr2, rename_map);
            replace_in_expression(expr3, rename_map);

            for stmt in stmts { 
                replace_in_statement(stmt, rename_map);
            }
        }
        Statement::Let(expr, rvalue) => {
            replace_in_varinfo(expr, rename_map);
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

fn replace_in_varinfo(repl_expr: &mut VarInfo, rename_map: &HashMap<String, String>) {
    match repl_expr {
        VarInfo::Var0(name) |
        VarInfo::Var1(name, _) |
        VarInfo::Var2(name, _, _) |
        VarInfo::Var3(name, _, _, _) => {
            if rename_map.contains_key(name) {
                *name = rename_map.get(name).unwrap().to_string();
            }
        }
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
