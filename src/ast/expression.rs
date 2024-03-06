use std::fmt;

use crate::{output_keyword, tables::FunctionDefinition};

use super::{Constant, Statement};

#[repr(i16)]
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinOp {
    PoW = -4,
    Mul = -5,
    Div = -6,
    Mod = -7,
    Add = -8,
    Sub = -9,
    Eq = -10,
    NotEq = -11,
    Lower = -12,
    LowerEq = -13,
    Greater = -14,
    GreaterEq = -15,
    And = -17,
    Or = -18,
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
            BinOp::Or => write!(f, "|"),
        }
    }
}

#[repr(i16)]
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnaryOp {
    Plus = -2,
    Minus = -3,
    Not = -16,
}
impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnaryOp::Plus => write!(f, "+"),
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(String),
    Const(Constant),
    Parens(Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    PredefinedFunctionCall(&'static FunctionDefinition<'static>, Vec<Expression>),
    UnaryExpression(UnaryOp, Box<Expression>),
    BinaryExpression(BinOp, Box<Expression>, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{id}"),
            Expression::Const(c) => write!(f, "{c}"),
            Expression::Parens(expr) => write!(f, "({})", **expr),
            Expression::UnaryExpression(op, expr) => {
                write!(f, "{}{}", op, **expr)
            }
            Expression::BinaryExpression(op, l_expr, r_expr) => {
                write!(f, "{} {} {}", **l_expr, op, **r_expr)
            }
            Expression::FunctionCall(name, params) => {
                write!(f, "{}({})", name, Statement::param_list_to_string(params))
            }
            Expression::PredefinedFunctionCall(name, params) => write!(
                f,
                "{}({})",
                output_keyword(name.name),
                Statement::param_list_to_string(params)
            ),
        }
    }
}
