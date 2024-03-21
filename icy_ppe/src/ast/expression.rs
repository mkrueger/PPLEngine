use std::fmt;

use super::{AstVisitor, AstVisitorMut, Constant};
use crate::{
    executable::FunctionDefinition,
    parser::lexer::{SpannedToken, Token},
};

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
#[derive(Debug, PartialEq, Copy, Clone, Eq)]
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
    PredefinedFunctionCall(PredefinedFunctionCallExpression),
    FunctionCall(FunctionCallExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
}

impl Expression {
    pub fn get_span(&self) -> core::ops::Range<usize> {
        match self {
            Expression::Identifier(i) => i.get_identifier_token().span.clone(),
            Expression::Const(c) => c.get_constant_token().span.clone(),
            Expression::Parens(p) => {
                p.get_lpar_token_token().span.start..p.get_rpar_token_token().span.end
            }
            Expression::PredefinedFunctionCall(pc) => {
                pc.get_identifier_token().span.start..pc.get_rpar_token_token().span.end
            }
            Expression::FunctionCall(fc) => {
                fc.get_identifier_token().span.start..fc.get_rpar_token_token().span.end
            }
            Expression::Unary(u) => u.get_op_token().span.start..u.get_expression().get_span().end,
            Expression::Binary(b) => {
                b.get_left_expression().get_span().start..b.get_right_expression().get_span().end
            }
        }
    }

    pub fn visit<T: Default, V: AstVisitor<T>>(&self, visitor: &mut V) -> T {
        match self {
            Expression::Identifier(expr) => visitor.visit_identifier_expression(expr),
            Expression::Const(expr) => visitor.visit_constant_expression(expr),
            Expression::Parens(expr) => visitor.visit_parens_expression(expr),
            Expression::PredefinedFunctionCall(expr) => {
                visitor.visit_predefined_function_call_expression(expr)
            }
            Expression::FunctionCall(expr) => visitor.visit_function_call_expression(expr),
            Expression::Unary(expr) => visitor.visit_unary_expression(expr),
            Expression::Binary(expr) => visitor.visit_binary_expression(expr),
        }
    }

    #[must_use]
    pub fn visit_mut<V: AstVisitorMut>(&self, visitor: &mut V) -> Self {
        match self {
            Expression::Identifier(expr) => visitor.visit_identifier_expression(expr),
            Expression::Const(expr) => visitor.visit_constant_expression(expr),
            Expression::Parens(expr) => visitor.visit_parens_expression(expr),
            Expression::PredefinedFunctionCall(expr) => {
                visitor.visit_predefined_function_call_expression(expr)
            }
            Expression::FunctionCall(expr) => visitor.visit_function_call_expression(expr),
            Expression::Unary(expr) => visitor.visit_unary_expression(expr),
            Expression::Binary(expr) => visitor.visit_binary_expression(expr),
        }
    }

    pub fn is_similar(&self, check: &Expression) -> bool {
        match (self, check) {
            (Expression::Identifier(i1), Expression::Identifier(i2)) => {
                i1.get_identifier() == i2.get_identifier()
            }
            (Expression::Const(c1), Expression::Const(c2)) => {
                c1.constant_value == c2.constant_value
            }
            (Expression::Parens(e1), Expression::Parens(e2)) => {
                e1.get_expression().is_similar(e2.get_expression())
            }
            (Expression::PredefinedFunctionCall(f1), Expression::PredefinedFunctionCall(f2)) => {
                f1.get_identifier() == f2.get_identifier()
                    && f1
                        .get_arguments()
                        .iter()
                        .zip(f2.get_arguments().iter())
                        .all(|(a, b)| a.is_similar(b))
            }
            (Expression::FunctionCall(f1), Expression::FunctionCall(f2)) => {
                f1.get_identifier() == f2.get_identifier()
                    && f1
                        .get_arguments()
                        .iter()
                        .zip(f2.get_arguments().iter())
                        .all(|(a, b)| a.is_similar(b))
            }
            (Expression::Unary(expr1), Expression::Unary(expr2)) => {
                expr1.get_op() == expr2.get_op()
                    && expr1.get_expression().is_similar(expr2.get_expression())
            }
            (Expression::Binary(expr1), Expression::Binary(expr2)) => {
                expr1.get_op() == expr2.get_op()
                    && expr1
                        .get_left_expression()
                        .is_similar(expr2.get_left_expression())
                    && expr1
                        .get_right_expression()
                        .is_similar(expr2.get_right_expression())
            }
            _ => false,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut output_visitor = crate::ast::output_visitor::OutputVisitor::default();
        self.visit(&mut output_visitor);
        write!(f, "{}", output_visitor.output)
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

    pub fn empty(identifier: unicase::Ascii<String>) -> Self {
        Self {
            identifier_token: SpannedToken::create_empty(Token::Identifier(identifier)),
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
    pub fn get_identifier(&self) -> &unicase::Ascii<String> {
        if let Token::Identifier(id) = &self.identifier_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub fn set_identifier(&mut self, new_id: unicase::Ascii<String>) {
        if let Token::Identifier(id) = &mut self.identifier_token.token {
            *id = new_id;
        }
    }

    pub(crate) fn create_empty_expression(identifier: unicase::Ascii<String>) -> Expression {
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
            constant_token: SpannedToken::create_empty(Token::Identifier(unicase::Ascii::new(
                String::new(),
            ))),
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
        expression: Expression,
        rightpar_token: SpannedToken,
    ) -> Self {
        Self {
            lpar_token: leftpar_token,
            expression: Box::new(expression),
            rpar_token: rightpar_token,
        }
    }

    pub fn empty(expression: Expression) -> Self {
        Self {
            lpar_token: SpannedToken::create_empty(Token::LPar),
            expression: Box::new(expression),
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

    pub(crate) fn create_empty_expression(constant_value: Expression) -> Expression {
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

    pub fn empty(identifier: unicase::Ascii<String>, arguments: Vec<Expression>) -> Self {
        Self {
            identifier_token: SpannedToken::create_empty(Token::Identifier(identifier)),
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

    pub(crate) fn set_arguments(&mut self, arguments: Vec<Expression>) {
        self.arguments = arguments;
    }

    pub fn get_rpar_token_token(&self) -> &SpannedToken {
        &self.rpar_token
    }

    /// Returns a reference to the get identifier of this [`IdentifierExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_identifier(&self) -> &unicase::Ascii<String> {
        if let Token::Identifier(id) = &self.identifier_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub(crate) fn create_empty_expression(
        identifier: unicase::Ascii<String>,
        arguments: Vec<Expression>,
    ) -> Expression {
        Expression::FunctionCall(FunctionCallExpression::empty(identifier, arguments))
    }

    pub fn set_identifier(&mut self, identifier: unicase::Ascii<String>) {
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
pub struct PredefinedFunctionCallExpression {
    identifier_token: SpannedToken,
    func: &'static FunctionDefinition,

    lpar_token: SpannedToken,
    arguments: Vec<Expression>,
    rpar_token: SpannedToken,
}

impl PredefinedFunctionCallExpression {
    pub fn new(
        identifier_token: SpannedToken,
        func: &'static FunctionDefinition,
        leftpar_token: SpannedToken,
        arguments: Vec<Expression>,
        rightpar_token: SpannedToken,
    ) -> Self {
        Self {
            identifier_token,
            func,
            lpar_token: leftpar_token,
            arguments,
            rpar_token: rightpar_token,
        }
    }

    pub fn empty(func: &'static FunctionDefinition, arguments: Vec<Expression>) -> Self {
        Self {
            identifier_token: SpannedToken::create_empty(Token::Identifier(unicase::Ascii::new(
                func.name.to_string(),
            ))),
            func,
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
    pub fn get_identifier(&self) -> &unicase::Ascii<String> {
        if let Token::Identifier(id) = &self.identifier_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub(crate) fn create_empty_expression(
        func: &'static FunctionDefinition,
        arguments: Vec<Expression>,
    ) -> Expression {
        Expression::PredefinedFunctionCall(PredefinedFunctionCallExpression::empty(func, arguments))
    }

    pub fn get_func(&self) -> &'static FunctionDefinition {
        self.func
    }
}

impl fmt::Display for PredefinedFunctionCallExpression {
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
    pub fn new(op_token: SpannedToken, expression: Expression) -> Self {
        Self {
            op_token,
            expression: Box::new(expression),
        }
    }

    pub fn empty(op: UnaryOp, expression: Expression) -> Self {
        Self {
            op_token: SpannedToken::create_empty(match op {
                UnaryOp::Plus => Token::Add,
                UnaryOp::Minus => Token::Sub,
                UnaryOp::Not => Token::Not,
            }),
            expression: Box::new(expression),
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

    pub(crate) fn create_empty_expression(op: UnaryOp, expression: Expression) -> Expression {
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
        left_expression: Expression,
        op_token: SpannedToken,
        right_expression: Expression,
    ) -> Self {
        Self {
            left_expression: Box::new(left_expression),
            op_token,
            right_expression: Box::new(right_expression),
        }
    }

    pub fn empty(left_expression: Expression, op: BinOp, right_expression: Expression) -> Self {
        Self {
            left_expression: Box::new(left_expression),
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
            right_expression: Box::new(right_expression),
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

    pub fn get_left_expression_mut(&mut self) -> &mut Expression {
        &mut self.left_expression
    }

    pub fn get_op_token(&self) -> &SpannedToken {
        &self.op_token
    }

    pub fn get_right_expression(&self) -> &Expression {
        &self.right_expression
    }

    pub fn get_right_expression_mut(&mut self) -> &mut Expression {
        &mut self.right_expression
    }

    pub(crate) fn create_empty_expression(
        op: BinOp,
        left_expression: Expression,
        right_expression: Expression,
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
