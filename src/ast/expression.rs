use std::fmt;

use crate::{tables::FunctionDefinition, output_keyword};

use super::{Constant, Statement};

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinOp {
    PoW,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Eq,
    NotEq,
    Lower,
    LowerEq,
    Greater,
    GreaterEq,
    And,
    Or,
}


impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::PoW => write!(f, "^"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Eq => write!(f, "="),
            BinOp::NotEq => write!(f, "<>"),
            BinOp::Lower => write!(f, "<"),
            BinOp::LowerEq => write!(f, "<="),
            BinOp::Greater => write!(f, ">"),
            BinOp::GreaterEq => write!(f, ">="),
            BinOp::And => write!(f, "&"),
            BinOp::Or => write!(f, "|")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression
{
    Identifier(String),
    Const(Constant),
    Parens(Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    PredefinedFunctionCall(&'static FunctionDefinition<'static>, Vec<Expression>),
    Not(Box<Expression>),
    Minus(Box<Expression>),
    Plus(Box<Expression>),
    BinaryExpression(BinOp, Box<Expression>, Box<Expression>),
    Dim1(Box<Expression>, Box<Expression>),
    Dim2(Box<Expression>, Box<Expression>, Box<Expression>),
    Dim3(Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{}", id),
            Expression::Const(c) => write!(f, "{}", c),
            Expression::Parens(expr) => write!(f, "({})", **expr),
            Expression::Not(expr) => write!(f, "!{}", **expr),
            Expression::Minus(expr) => write!(f, "-{}", **expr),
            Expression::Plus(expr) => write!(f, "+{}", **expr),
            Expression::BinaryExpression(op, l_expr, r_expr) => write!(f, "{} {} {}", **l_expr, op, **r_expr),
            Expression::FunctionCall(name, params) => write!(f, "{}({})", name, Statement::param_list_to_string(params)),
            Expression::PredefinedFunctionCall(name, params) => write!(f, "{}({})", output_keyword(name.name), Statement::param_list_to_string(params)),
            Expression::Dim1(expr, vec) => write!(f, "{}({})", **expr, **vec),
            Expression::Dim2(expr, vec, mat) => write!(f, "{}({}, {})", **expr, **vec, **mat),
            Expression::Dim3(expr, vec, mat, cub) => write!(f, "{}({}, {}, {})", **expr, **vec, **mat, **cub),

        }
    }
}
