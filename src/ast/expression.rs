use crate::{tables::FunctionDefinition, output_keyword};

use super::*;

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

impl BinOp
{
    pub fn to_string(&self) -> String
    {
        match self {
            BinOp::PoW => "^".to_string(),
            BinOp::Mul => "*".to_string(),
            BinOp::Div => "/".to_string(),
            BinOp::Mod => "%".to_string(),
            BinOp::Add => "+".to_string(),
            BinOp::Sub => "-".to_string(),
            BinOp::Eq => "=".to_string(),
            BinOp::NotEq => "<>".to_string(),
            BinOp::Lower => "<".to_string(),
            BinOp::LowerEq => "<=".to_string(),
            BinOp::Greater => ">".to_string(),
            BinOp::GreaterEq => ">=".to_string(),
            BinOp::And => "&".to_string(),
            BinOp::Or => "|".to_string()
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

impl Expression
{
    pub fn to_string(&self) -> String
    {
        match self {
            Expression::Identifier(id) => id.to_string(),
            Expression::Const(c) => c.to_string(),
            Expression::Parens(expr) => format!("({})", (**expr).to_string()),
            Expression::Not(expr) => format!("!{}", (**expr).to_string()),
            Expression::Minus(expr) => format!("-{}", (**expr).to_string()),
            Expression::Plus(expr) => format!("+{}", (**expr).to_string()),
            Expression::BinaryExpression(op, lexpr, rexpr) => format!("{} {} {}", (**lexpr).to_string(), op.to_string(), (**rexpr).to_string()),
            Expression::FunctionCall(name, params) => format!("{}({})", name, Statement::param_list_to_string(params)),
            Expression::PredefinedFunctionCall(name, params) => format!("{}({})", output_keyword(name.name), Statement::param_list_to_string(params)),
            Expression::Dim1(expr, vec) => format!("{}({})", (**expr).to_string(), (**vec).to_string()),
            Expression::Dim2(expr, vec, mat) => format!("{}({}, {})", (**expr).to_string(), (**vec).to_string(), (**mat).to_string()),
            Expression::Dim3(expr, vec, mat, cub) => format!("{}({}, {}, {})", (**expr).to_string(), (**vec).to_string(), (**mat).to_string(), (**cub).to_string()),

        }
    }
}
