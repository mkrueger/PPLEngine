use std::fmt;

use super::Constant;
use crate::parser::tokens::{SpannedToken, Token};

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
    Identifier(IdentifierExpression),
    Const(ConstantExpression),
    Parens(ParensExpression),
    FunctionCall(FunctionCallExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{id}"),
            Expression::Const(c) => write!(f, "{c}"),
            Expression::Parens(expr) => write!(f, "{expr}"),
            Expression::Unary(expr) => {
                write!(f, "{expr}")
            }
            Expression::Binary(expr) => {
                write!(f, "{expr}")
            }
            Expression::FunctionCall(expr) => {
                write!(f, "{expr}")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierExpression {
    identifier_token: SpannedToken,
}

impl IdentifierExpression {
    pub fn new(identifier_token: SpannedToken) -> Self {
        Self { identifier_token }
    }

    pub fn empty(identifier: impl Into<String>) -> Self {
        Self {
            identifier_token: SpannedToken::create_empty(Token::Identifier(identifier.into())),
        }
    }

    pub fn get_identifier_token(&self) -> &SpannedToken {
        &self.identifier_token
    }

    /// Returns a reference to the get identifier of this [`IdentifierExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_identifier(&self) -> &String {
        if let Token::Identifier(id) = &self.identifier_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub(crate) fn create_empty_expression(identifier: impl Into<String>) -> Expression {
        Expression::Identifier(IdentifierExpression::empty(identifier))
    }
}

impl fmt::Display for IdentifierExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.get_identifier())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstantExpression {
    constant_token: SpannedToken,
    constant_value: Constant,
}

impl ConstantExpression {
    pub fn new(constant_token: SpannedToken, constant_value: Constant) -> Self {
        Self {
            constant_token,
            constant_value,
        }
    }

    pub fn empty(constant_value: Constant) -> Self {
        Self {
            constant_token: SpannedToken::create_empty(Token::Comment),
            constant_value,
        }
    }

    pub fn get_constant_token(&self) -> &SpannedToken {
        &self.constant_token
    }

    pub fn get_constant_value(&self) -> &Constant {
        &self.constant_value
    }

    pub(crate) fn create_empty_expression(constant_value: Constant) -> Expression {
        Expression::Const(ConstantExpression::empty(constant_value))
    }
}

impl fmt::Display for ConstantExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.get_constant_value())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParensExpression {
    lpar_token: SpannedToken,
    expression: Box<Expression>,
    rpar_token: SpannedToken,
}

impl ParensExpression {
    pub fn new(
        leftpar_token: SpannedToken,
        expression: Box<Expression>,
        rightpar_token: SpannedToken,
    ) -> Self {
        Self {
            lpar_token: leftpar_token,
            expression,
            rpar_token: rightpar_token,
        }
    }

    pub fn empty(expression: Box<Expression>) -> Self {
        Self {
            lpar_token: SpannedToken::create_empty(Token::LPar),
            expression,
            rpar_token: SpannedToken::create_empty(Token::RPar),
        }
    }

    pub fn get_lpar_token_token(&self) -> &SpannedToken {
        &self.lpar_token
    }

    pub fn get_expression(&self) -> &Expression {
        &self.expression
    }

    pub fn get_expression_mut(&mut self) -> &mut Expression {
        &mut self.expression
    }

    pub fn get_rpar_token_token(&self) -> &SpannedToken {
        &self.rpar_token
    }

    pub(crate) fn create_empty_expression(constant_value: Box<Expression>) -> Expression {
        Expression::Parens(ParensExpression::empty(constant_value))
    }
}

impl fmt::Display for ParensExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.get_expression())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCallExpression {
    identifier_token: SpannedToken,
    lpar_token: SpannedToken,
    arguments: Vec<Expression>,
    rpar_token: SpannedToken,
}

impl FunctionCallExpression {
    pub fn new(
        identifier_token: SpannedToken,
        leftpar_token: SpannedToken,
        arguments: Vec<Expression>,
        rightpar_token: SpannedToken,
    ) -> Self {
        Self {
            identifier_token,
            lpar_token: leftpar_token,
            arguments,
            rpar_token: rightpar_token,
        }
    }

    pub fn empty(identifier: impl Into<String>, arguments: Vec<Expression>) -> Self {
        Self {
            identifier_token: SpannedToken::create_empty(Token::Identifier(identifier.into())),
            lpar_token: SpannedToken::create_empty(Token::LPar),
            arguments,
            rpar_token: SpannedToken::create_empty(Token::RPar),
        }
    }

    pub fn get_identifier_token(&self) -> &SpannedToken {
        &self.identifier_token
    }
    pub fn get_lpar_token_token(&self) -> &SpannedToken {
        &self.lpar_token
    }

    pub fn get_arguments(&self) -> &Vec<Expression> {
        &self.arguments
    }

    pub fn get_arguments_mut(&mut self) -> &mut Vec<Expression> {
        &mut self.arguments
    }

    pub fn get_rpar_token_token(&self) -> &SpannedToken {
        &self.rpar_token
    }

    /// Returns a reference to the get identifier of this [`IdentifierExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_identifier(&self) -> &String {
        if let Token::Identifier(id) = &self.identifier_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub(crate) fn create_empty_expression(
        identifier: impl Into<String>,
        arguments: Vec<Expression>,
    ) -> Expression {
        let identifier = identifier.into();
        Expression::FunctionCall(FunctionCallExpression::empty(identifier, arguments))
    }

    pub(crate) fn set_identifier(&mut self, identifier: String) {
        self.identifier_token.token = Token::Identifier(identifier);
    }
}

impl fmt::Display for FunctionCallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.get_identifier())?;
        for (i, arg) in self.get_arguments().iter().enumerate() {
            write!(f, "{arg}")?;
            if i < self.get_arguments().len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpression {
    op_token: SpannedToken,
    expression: Box<Expression>,
}

impl UnaryExpression {
    pub fn new(op_token: SpannedToken, expression: Box<Expression>) -> Self {
        Self {
            op_token,
            expression,
        }
    }

    pub fn empty(op: UnaryOp, expression: Box<Expression>) -> Self {
        Self {
            op_token: SpannedToken::create_empty(match op {
                UnaryOp::Plus => Token::Add,
                UnaryOp::Minus => Token::Sub,
                UnaryOp::Not => Token::Not,
            }),
            expression,
        }
    }

    /// Returns the get op of this [`UnaryExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_op(&self) -> UnaryOp {
        match &self.op_token.token {
            Token::Add => UnaryOp::Plus,
            Token::Sub => UnaryOp::Minus,
            Token::Not => UnaryOp::Not,
            _ => panic!("Expected unary operator"),
        }
    }

    pub fn get_op_token(&self) -> &SpannedToken {
        &self.op_token
    }

    pub fn get_expression(&self) -> &Expression {
        &self.expression
    }

    pub fn get_expression_mut(&mut self) -> &mut Expression {
        &mut self.expression
    }

    pub(crate) fn create_empty_expression(op: UnaryOp, expression: Box<Expression>) -> Expression {
        Expression::Unary(UnaryExpression::empty(op, expression))
    }
}

impl fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.get_op(), self.get_expression())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpression {
    left_expression: Box<Expression>,
    op_token: SpannedToken,
    right_expression: Box<Expression>,
}

impl BinaryExpression {
    pub fn new(
        left_expression: Box<Expression>,
        op_token: SpannedToken,
        right_expression: Box<Expression>,
    ) -> Self {
        Self {
            left_expression,
            op_token,
            right_expression,
        }
    }

    pub fn empty(
        left_expression: Box<Expression>,
        op: BinOp,
        right_expression: Box<Expression>,
    ) -> Self {
        Self {
            left_expression,
            op_token: SpannedToken::create_empty(match op {
                BinOp::PoW => Token::PoW,
                BinOp::Mul => Token::Mul,
                BinOp::Div => Token::Div,
                BinOp::Mod => Token::Mod,
                BinOp::Add => Token::Add,
                BinOp::Sub => Token::Sub,
                BinOp::Eq => Token::Eq,
                BinOp::NotEq => Token::NotEq,
                BinOp::Lower => Token::Lower,
                BinOp::LowerEq => Token::LowerEq,
                BinOp::Greater => Token::Greater,
                BinOp::GreaterEq => Token::GreaterEq,
                BinOp::And => Token::And,
                BinOp::Or => Token::Or,
            }),
            right_expression,
        }
    }

    /// Returns the get op of this [`UnaryExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_op(&self) -> BinOp {
        match &self.op_token.token {
            Token::PoW => BinOp::PoW,
            Token::Mul => BinOp::Mul,
            Token::Div => BinOp::Div,
            Token::Mod => BinOp::Mod,
            Token::Add => BinOp::Add,
            Token::Sub => BinOp::Sub,
            Token::Eq => BinOp::Eq,
            Token::NotEq => BinOp::NotEq,
            Token::Lower => BinOp::Lower,
            Token::LowerEq => BinOp::LowerEq,
            Token::Greater => BinOp::Greater,
            Token::GreaterEq => BinOp::GreaterEq,
            Token::And => BinOp::And,
            Token::Or => BinOp::Or,
            _ => panic!("Expected binary operator"),
        }
    }

    pub fn get_left_expression(&self) -> &Expression {
        &self.left_expression
    }

    pub fn get_left_expressionmut(&mut self) -> &mut Expression {
        &mut self.left_expression
    }

    pub fn get_op_token(&self) -> &SpannedToken {
        &self.op_token
    }

    pub fn get_right_expression(&self) -> &Expression {
        &self.right_expression
    }

    pub fn get_right_expressionmut(&mut self) -> &mut Expression {
        &mut self.right_expression
    }

    pub(crate) fn create_empty_expression(
        op: BinOp,
        left_expression: Box<Expression>,
        right_expression: Box<Expression>,
    ) -> Expression {
        Expression::Binary(BinaryExpression::empty(
            left_expression,
            op,
            right_expression,
        ))
    }
}

impl fmt::Display for BinaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.get_left_expression(),
            self.get_op(),
            self.get_right_expression()
        )
    }
}
