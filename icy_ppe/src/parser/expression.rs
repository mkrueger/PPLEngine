use super::{lexer::Token, Parser};
use crate::{
    ast::{
        BinOp, BinaryExpression, ConstantExpression, Expression, FunctionCallExpression,
        IdentifierExpression, ParensExpression, PredefinedFunctionCallExpression, UnaryExpression,
    },
    executable::{get_function_definition, FUNCTION_DEFINITIONS},
    parser::{Error, ParserError, ParserErrorType},
};

impl Parser {
    pub fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_bool()
    }
    fn parse_bool(&mut self) -> Option<Expression> {
        // it's correct on the upper level - it's very unusual
        if self.get_cur_token() == Some(Token::Not) {
            self.next_token();
            let Some(expr) = self.parse_bool() else {
                return None;
            };
            return Some(UnaryExpression::create_empty_expression(
                crate::ast::UnaryOp::Not,
                expr,
            ));
        }

        let Some(mut expr) = self.parse_comparison() else {
            return None;
        };
        while self.get_cur_token() == Some(Token::Or) || self.get_cur_token() == Some(Token::And) {
            let op = match self.get_cur_token() {
                Some(Token::Or) => BinOp::Or,
                Some(Token::And) => BinOp::And,
                _ => {
                    self.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::UnexpectedError,
                        range: self.save_token_span(),
                    }));
                    return None;
                }
            };
            self.next_token();
            let right = self.parse_comparison();
            if let Some(e) = right {
                expr = BinaryExpression::create_empty_expression(op, expr, e);
            } else {
                return None;
            }
        }
        Some(expr)
    }

    fn parse_comparison(&mut self) -> Option<Expression> {
        let Some(mut expr) = self.parse_term() else {
            return None;
        };
        while self.get_cur_token() == Some(Token::Greater)
            || self.get_cur_token() == Some(Token::GreaterEq)
            || self.get_cur_token() == Some(Token::Lower)
            || self.get_cur_token() == Some(Token::LowerEq)
            || self.get_cur_token() == Some(Token::Eq)
            || self.get_cur_token() == Some(Token::NotEq)
        {
            let op = match self.get_cur_token() {
                Some(Token::Greater) => BinOp::Greater,
                Some(Token::GreaterEq) => BinOp::GreaterEq,
                Some(Token::Lower) => BinOp::Lower,
                Some(Token::LowerEq) => BinOp::LowerEq,
                Some(Token::Eq) => BinOp::Eq,
                Some(Token::NotEq) => BinOp::NotEq,
                _ => {
                    self.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::UnexpectedError,
                        range: self.save_token_span(),
                    }));
                    return None;
                }
            };
            self.next_token();

            let right = self.parse_term();
            if let Some(e) = right {
                expr = BinaryExpression::create_empty_expression(op, expr, e);
            } else {
                return None;
            }
        }

        Some(expr)
    }

    fn parse_term(&mut self) -> Option<Expression> {
        let Some(mut expr) = self.parse_factor() else {
            return None;
        };
        while self.get_cur_token() == Some(Token::Add) || self.get_cur_token() == Some(Token::Sub) {
            let op = match self.get_cur_token() {
                Some(Token::Add) => BinOp::Add,
                Some(Token::Sub) => BinOp::Sub,
                _ => {
                    self.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::UnexpectedError,
                        range: self.save_token_span(),
                    }));
                    return None;
                }
            };
            self.next_token();
            let right = self.parse_factor();
            if let Some(e) = right {
                expr = BinaryExpression::create_empty_expression(op, expr, e);
            } else {
                return None;
            }
        }

        Some(expr)
    }

    fn parse_factor(&mut self) -> Option<Expression> {
        let Some(mut expr) = self.parse_pow() else {
            return None;
        };
        while self.get_cur_token() == Some(Token::Mul)
            || self.get_cur_token() == Some(Token::Div)
            || self.get_cur_token() == Some(Token::Mod)
        {
            let op = match self.get_cur_token() {
                Some(Token::Mul) => BinOp::Mul,
                Some(Token::Div) => BinOp::Div,
                Some(Token::Mod) => BinOp::Mod,
                _ => {
                    self.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::UnexpectedError,
                        range: self.save_token_span(),
                    }));
                    return None;
                }
            };
            self.next_token();

            let right = self.parse_pow();
            if let Some(e) = right {
                expr = BinaryExpression::create_empty_expression(op, expr, e);
            } else {
                return None;
            }
        }
        Some(expr)
    }

    fn parse_pow(&mut self) -> Option<Expression> {
        let Some(mut expr) = self.parse_unary() else {
            return None;
        };
        while self.get_cur_token() == Some(Token::PoW) {
            self.next_token();
            let right = self.parse_unary();
            if let Some(e) = right {
                expr = BinaryExpression::create_empty_expression(BinOp::PoW, expr, e);
            } else {
                return None;
            }
        }
        Some(expr)
    }

    fn parse_unary(&mut self) -> Option<Expression> {
        if self.get_cur_token() == Some(Token::Add) {
            self.next_token();
            let expr = self.parse_unary();
            if let Some(e) = expr {
                return Some(UnaryExpression::create_empty_expression(
                    crate::ast::UnaryOp::Plus,
                    e,
                ));
            }
        }
        if self.get_cur_token() == Some(Token::Sub) {
            self.next_token();
            let expr = self.parse_unary();
            if let Some(e) = expr {
                return Some(UnaryExpression::create_empty_expression(
                    crate::ast::UnaryOp::Minus,
                    e,
                ));
            }
        }
        if self.get_cur_token() == Some(Token::Not) {
            self.next_token();
            let expr = self.parse_unary();
            if let Some(e) = expr {
                return Some(UnaryExpression::create_empty_expression(
                    crate::ast::UnaryOp::Not,
                    e,
                ));
            }
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Option<Expression> {
        let Some(t) = self.cur_token.clone() else {
            return None;
        };

        match &t.token {
            Token::Const(c) => {
                self.next_token();
                Some(Expression::Const(ConstantExpression::new(
                    t.clone(),
                    c.clone(),
                )))
            }
            Token::Identifier(id) => {
                let identifier_token = self.save_spannedtoken();
                self.next_token();
                if self.get_cur_token() == Some(Token::LPar) {
                    let leftpar_token = self.save_spannedtoken();

                    self.next_token();
                    let mut arguments = Vec::new();

                    while self.get_cur_token() != Some(Token::RPar) {
                        let Some(value) = self.parse_expression() else {
                            self.errors.push(Error::ParserError(ParserError {
                                error: ParserErrorType::InvalidToken(self.save_token()),
                                range: self.save_token_span(),
                            }));
                            return None;
                        };
                        arguments.push(value);
                        if self.get_cur_token() == Some(Token::Comma) {
                            self.next_token();
                            continue;
                        }

                        if self.get_cur_token() != Some(Token::RPar)
                            && self.get_cur_token() != Some(Token::Comma)
                        {
                            break;
                        }
                    }

                    if self.get_cur_token() != Some(Token::RPar) {
                        self.errors.push(Error::ParserError(ParserError {
                            error: ParserErrorType::MissingCloseParens(self.save_token()),
                            range: self.save_token_span(),
                        }));
                        return None;
                    }
                    let rightpar_token = self.save_spannedtoken();

                    self.next_token();

                    let predef = get_function_definition(id);
                    if predef >= 0 {
                        return Some(Expression::PredefinedFunctionCall(
                            PredefinedFunctionCallExpression::new(
                                identifier_token,
                                &FUNCTION_DEFINITIONS[predef as usize],
                                leftpar_token,
                                arguments,
                                rightpar_token,
                            ),
                        ));
                    }
                    return Some(Expression::FunctionCall(FunctionCallExpression::new(
                        identifier_token,
                        leftpar_token,
                        arguments,
                        rightpar_token,
                    )));
                }

                Some(Expression::Identifier(IdentifierExpression::new(
                    identifier_token,
                )))
            }
            Token::LPar => {
                self.next_token();
                let Some(expr) = self.parse_expression() else {
                    self.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::ExpressionExpected(self.save_token()),
                        range: self.save_token_span(),
                    }));
                    return None;
                };
                let ret = ParensExpression::create_empty_expression(expr);
                if self.get_cur_token() != Some(Token::RPar) {
                    self.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::MissingCloseParens(self.save_token()),
                        range: self.save_token_span(),
                    }));
                    return None;
                }
                self.next_token();
                Some(ret)
            }
            _ => None,
        }
    }
}
