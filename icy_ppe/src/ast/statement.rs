use crate::{
    output_keyword_indented,
    tables::{StatementDefinition, PPL_TRUE},
};

use super::{
    Constant, ConstantExpression, Expression, ProgramContext, UnaryOp, VarInfo, VariableType,
};
use crate::output_keyword;

#[derive(Debug, Clone, PartialEq)]
pub struct ElseIfBlock {
    pub cond: Box<Expression>,
    pub block: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Comment(String),
    End,
    Block(Vec<Statement>),
    While(Box<Expression>, Box<Statement>),
    Select(Box<Expression>, Vec<ElseIfBlock>, Option<Vec<Statement>>),
    If(Box<Expression>, Box<Statement>),
    IfThen(
        Box<Expression>,
        Vec<Statement>,
        Vec<ElseIfBlock>,
        Option<Vec<Statement>>,
    ),
    DoWhile(Box<Expression>, Vec<Statement>),
    For(
        Box<Expression>,
        Box<Expression>,
        Box<Expression>,
        Option<Box<Expression>>,
        Vec<Statement>,
    ),
    Break,
    Continue,
    Gosub(String),
    Return,
    Let(Box<VarInfo>, Box<Expression>),
    Goto(String),
    Label(String),
    ProcedureCall(String, Vec<Expression>),
    Call(&'static StatementDefinition<'static>, Vec<Expression>),
}

pub fn get_var_name(expr: &Expression) -> String {
    if let Expression::FunctionCall(call_expr) = expr {
        call_expr.get_identifier().clone()
    } else {
        expr.to_string()
    }
}

impl Statement {
    pub fn param_list_to_string(l: &Vec<Expression>) -> String {
        let mut res = String::new();
        for expr in l {
            if !res.is_empty() {
                res.push_str(", ");
            }
            res.push_str(expr.to_string().as_str());
        }
        res
    }

    pub fn try_boolean_conversion(expr: &Expression) -> Expression {
        match expr {
            Expression::Const(c) => match c.get_constant_value() {
                Constant::Integer(i) => {
                    if *i == PPL_TRUE {
                        ConstantExpression::create_empty_expression(Constant::Boolean(true))
                    } else {
                        ConstantExpression::create_empty_expression(Constant::Boolean(false))
                    }
                }
                _ => expr.clone(),
            },
            Expression::Parens(expr) => Statement::try_boolean_conversion(expr.get_expression()),
            Expression::Unary(op, notexpr) => {
                if !matches!(op, UnaryOp::Not) {
                    return expr.clone();
                }

                match &**notexpr {
                    Expression::Const(c) => match c.get_constant_value() {
                        Constant::Boolean(b) => {
                            Expression::Const(ConstantExpression::empty(Constant::Boolean(!b)))
                        }
                        _ => expr.clone(),
                    },
                    Expression::Unary(op, notexpr) => {
                        if matches!(op, UnaryOp::Not) {
                            return Statement::try_boolean_conversion(notexpr);
                        }
                        expr.clone()
                    }
                    _ => expr.clone(),
                }
            }
            _ => expr.clone(),
        }
    }

    pub fn get_indent(indent: i32) -> String {
        let mut res = String::new();
        for _ in 0..indent {
            res.push_str("    ");
        }
        res
    }

    fn output_stmts(prg: &dyn ProgramContext, stmts: &Vec<Statement>, indent: i32) -> String {
        let mut res = String::new();
        for stmt in stmts {
            let (str, ind, modifier) = stmt.to_string(prg, indent);
            res.push_str(Statement::get_indent(ind + modifier).as_str());
            res.push_str(&str);
            res.push('\n');
        }

        //   Statement::ElseIf(cond) => (format!("{} ({}) {}", output_keyword("ElseIf"), Statement::out_bool_func(cond), output_keyword("Then")), indent, -1),

        res
    }

    fn output_if_stmts(
        prg: &dyn ProgramContext,
        stmts: &Vec<Statement>,
        else_if_blocks: &Vec<ElseIfBlock>,
        else_block: &Option<Vec<Statement>>,
        indent: i32,
    ) -> String {
        let mut res = String::new();

        res.push_str(&Statement::output_stmts(prg, stmts, indent));

        for else_if_block in else_if_blocks {
            res.push_str(&format!(
                "{} ({}) {}",
                output_keyword_indented(indent - 1, "ElseIf"),
                Statement::out_bool_func(&else_if_block.cond),
                output_keyword("Then")
            ));
            res.push('\n');
            res.push_str(&Statement::output_stmts(prg, &else_if_block.block, indent));
        }
        if let Some(else_block) = else_block {
            if !else_block.is_empty() {
                res.push_str(&output_keyword_indented(indent - 1, "Else"));
                res.push('\n');
                res.push_str(&Statement::output_stmts(prg, else_block, indent));
            }
        }
        res
    }

    fn output_case_blocks(
        prg: &dyn ProgramContext,
        case_blocks: &Vec<ElseIfBlock>,
        else_block: &Option<Vec<Statement>>,
        indent: i32,
    ) -> String {
        let mut res = String::new();

        for case_block in case_blocks {
            res.push_str(&format!(
                "{} {}",
                output_keyword_indented(indent - 1, "Case"),
                &case_block.cond
            ));
            res.push('\n');
            res.push_str(&Statement::output_stmts(prg, &case_block.block, indent));
        }

        if let Some(else_block) = else_block {
            if !else_block.is_empty() {
                res.push_str(&output_keyword_indented(indent - 1, "Case Else"));
                res.push('\n');
                res.push_str(&Statement::output_stmts(prg, else_block, indent));
            }
        }
        res
    }

    pub fn strip_outer_parens(exp: &Expression) -> &Expression {
        if let Expression::Parens(pexpr) = exp {
            pexpr.get_expression()
        } else {
            exp
        }
    }

    pub fn out_bool_func(expr: &Expression) -> String {
        Statement::strip_outer_parens(&Statement::try_boolean_conversion(
            Statement::strip_outer_parens(expr),
        ))
        .to_string()
    }

    pub fn to_string(&self, prg: &dyn ProgramContext, indent: i32) -> (String, i32, i32) // (str, indent, cur_line_inden_tmodifier)
    {
        match self {
            Statement::Comment(str) => (format!(";{str}"), indent, 0),
            Statement::Block(stmts) => (
                format!(
                    "{}\n{}{}",
                    output_keyword("Begin"),
                    Statement::output_stmts(prg, stmts, indent + 1),
                    output_keyword_indented(indent, "End")
                ),
                indent,
                0,
            ),
            Statement::While(cond, stmt) => (
                format!(
                    "{} ({}) {}",
                    output_keyword("While"),
                    Statement::out_bool_func(cond),
                    stmt.to_string(prg, 0).0
                ),
                indent,
                0,
            ),
            Statement::If(cond, stmt) => (
                format!(
                    "{} ({}) {}",
                    output_keyword("If"),
                    Statement::out_bool_func(cond),
                    stmt.to_string(prg, 0).0
                ),
                indent,
                0,
            ),
            Statement::IfThen(cond, stmts, else_if_blocks, else_block) => (
                format!(
                    "{} ({}) {}\n{}{}",
                    output_keyword("If"),
                    Statement::out_bool_func(cond),
                    output_keyword("Then"),
                    Statement::output_if_stmts(prg, stmts, else_if_blocks, else_block, indent + 1),
                    output_keyword_indented(indent, "EndIf")
                ),
                indent,
                0,
            ),

            Statement::Select(cond, case_blocks, else_block) => (
                format!(
                    "{} ({})\n{}{}",
                    output_keyword("Select Case"),
                    cond,
                    Statement::output_case_blocks(prg, case_blocks, else_block, indent + 1),
                    output_keyword_indented(indent, "EndSelect")
                ),
                indent,
                0,
            ),
            Statement::DoWhile(cond, stmts) => (
                format!(
                    "{} ({}) {}\n{}{}",
                    output_keyword("While"),
                    Statement::out_bool_func(cond),
                    output_keyword("Do"),
                    Statement::output_stmts(prg, stmts, indent + 1),
                    output_keyword_indented(indent, "EndWhile")
                ),
                indent,
                0,
            ),
            Statement::Break => (output_keyword("Break"), indent, 0),
            Statement::Continue => (output_keyword("Continue"), indent, 0),
            Statement::End => (output_keyword("End"), indent, 0),
            Statement::Gosub(label) => {
                (format!("{} {}", output_keyword("GoSub"), label), indent, 0)
            }
            Statement::Return => (output_keyword("Return"), indent, 0),
            Statement::Let(var, expr) => {
                let expected_type = prg.get_var_type(var.get_name());
                let expr2 = if expected_type == VariableType::Boolean {
                    Statement::try_boolean_conversion(expr)
                } else {
                    (**expr).clone()
                };
                (format!("{var} = {expr2}"), indent, 0)
            }
            Statement::Goto(label) => (format!("{} {}", output_keyword("GoTo"), label), indent, 0),
            Statement::For(var_name, from, to, step, stmts) => {
                let var_name = &get_var_name(var_name);
                if let Some(s) = step {
                    (
                        format!(
                            "{} {} = {} {} {} {} {}\n{}{}",
                            output_keyword("For"),
                            var_name,
                            from,
                            output_keyword("To"),
                            to,
                            output_keyword("Step"),
                            s,
                            Statement::output_stmts(prg, stmts, indent + 1),
                            output_keyword_indented(indent, "Next")
                        ),
                        indent,
                        0,
                    )
                } else {
                    (
                        format!(
                            "{} {} = {} {} {}\n{}{}",
                            output_keyword("For"),
                            var_name,
                            from,
                            output_keyword("To"),
                            to,
                            Statement::output_stmts(prg, stmts, indent + 1),
                            output_keyword_indented(indent, "Next")
                        ),
                        indent,
                        0,
                    )
                }
            }
            Statement::Label(str) => (
                format!("\n{}:{}", Statement::get_indent(indent - 1), str),
                indent,
                -1,
            ),
            Statement::ProcedureCall(name, params) => (
                format!("{}({})", name, Statement::param_list_to_string(params)),
                indent,
                0,
            ),
            Statement::Call(def, params) => {
                if params.is_empty() {
                    (output_keyword(def.name), indent, 0)
                } else {
                    (
                        format!(
                            "{} {}",
                            output_keyword(def.name),
                            Statement::param_list_to_string(params)
                        ),
                        indent,
                        0,
                    )
                }
            }
        }
    }
}
