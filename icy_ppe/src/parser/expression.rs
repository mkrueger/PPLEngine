use super::{tokens::Token, Tokenizer};
use crate::{
    ast::{
        BinOp, BinaryExpression, ConstantExpression, Expression, FunctionCallExpression,
        IdentifierExpression, ParensExpression, UnaryExpression,
    },
    parser::{Error, ParserError, ParserErrorType},
};

impl<'a> Tokenizer<'a> {
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
                Box::new(expr),
            ));
        }

        let Some(mut expr) = self.parse_comparison() else {
            return None;
        };
        while self.get_cur_token() == Some(Token::Or) || self.get_cur_token() == Some(Token::And) {
            let op = match self.get_cur_token() {
                Some(Token::Or) => BinOp::Or,
                Some(Token::And) => BinOp::And,
                _ => panic!(),
            };
            self.next_token();
            let right = self.parse_comparison();
            if let Some(e) = right {
                expr = BinaryExpression::create_empty_expression(op, Box::new(expr), Box::new(e));
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
                _ => panic!(),
            };
            self.next_token();

            let right = self.parse_term();
            if let Some(e) = right {
                expr = BinaryExpression::create_empty_expression(op, Box::new(expr), Box::new(e));
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
                _ => panic!(),
            };
            self.next_token();
            let right = self.parse_factor();
            if let Some(e) = right {
                expr = BinaryExpression::create_empty_expression(op, Box::new(expr), Box::new(e));
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
                _ => panic!(),
            };
            self.next_token();

            let right = self.parse_pow();
            if let Some(e) = right {
                expr = BinaryExpression::create_empty_expression(op, Box::new(expr), Box::new(e));
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
                expr = BinaryExpression::create_empty_expression(
                    BinOp::PoW,
                    Box::new(expr),
                    Box::new(e),
                );
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
                    Box::new(e),
                ));
            }
        }
        if self.get_cur_token() == Some(Token::Sub) {
            self.next_token();
            let expr = self.parse_unary();
            if let Some(e) = expr {
                return Some(UnaryExpression::create_empty_expression(
                    crate::ast::UnaryOp::Minus,
                    Box::new(e),
                ));
            }
        }
        if self.get_cur_token() == Some(Token::Not) {
            self.next_token();
            let expr = self.parse_unary();
            if let Some(e) = expr {
                return Some(UnaryExpression::create_empty_expression(
                    crate::ast::UnaryOp::Not,
                    Box::new(e),
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
                let ct = self.cur_token.as_ref().unwrap().clone();
                self.next_token();
                if self.get_cur_token() == Some(Token::LPar) {
                    self.next_token();
                    let mut params = Vec::new();

                    while self.get_cur_token() != Some(Token::RPar) {
                        let Some(value) = self.parse_expression() else {
                            self.errors.push(Error::ParserError(ParserError {
                                error: ParserErrorType::InvalidToken(
                                    self.cur_token.as_ref().unwrap().token.clone(),
                                ),
                                range: self.cur_token.as_ref().unwrap().span.clone(),
                            }));
                            return None;
                        };
                        params.push(value);
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
                            error: ParserErrorType::MissingCloseParens(
                                self.cur_token.as_ref().unwrap().token.clone(),
                            ),
                            range: self.cur_token.as_ref().unwrap().span.clone(),
                        }));
                    }

                    self.next_token();
                    return Some(FunctionCallExpression::create_empty_expression(
                        id.clone(),
                        params,
                    ));
                }

                Some(Expression::Identifier(IdentifierExpression::new(ct)))
            }
            Token::LPar => {
                self.next_token();
                let Some(expr) = self.parse_expression() else {
                    return None;
                };
                let ret = ParensExpression::create_empty_expression(Box::new(expr));
                assert!(
                    !(self.get_cur_token() != Some(Token::RPar)),
                    "unclosed parens."
                );
                self.next_token();
                Some(ret)
            }
            _ => None,
        }
    }
}
#[cfg(test)]
mod tests {
    /*
        use crate::{
            ast::{BinOp, Constant, Expression},
            parser::Tokenizer,
            tables::{get_function_definition, FUNCTION_DEFINITIONS},
        };
        fn parse_expression(src: &str) -> Expression {
            let mut tokenizer = Tokenizer::new(src);
            tokenizer.next_token();
            tokenizer.parse_expression().unwrap()
        }

        #[test]
        fn test_parse_parens() {
            assert_eq!(
                Expression::Parens(Box::new(Expression::Const(Constant::Integer(5)))),
                parse_expression("(5)")
            );
        }

        #[test]
        fn test_unary_expressions() {
            assert_eq!(
                Expression::UnaryExpression(
                    crate::ast::UnaryOp::Not,
                    Box::new(Expression::Const(Constant::Builtin(
                        &crate::ast::constant::BuiltinConst::FALSE
                    )))
                ),
                parse_expression("!FALSE")
            );

            assert_eq!(
                Expression::UnaryExpression(
                    crate::ast::UnaryOp::Minus,
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("-5")
            );

            assert_eq!(
                Expression::UnaryExpression(
                    crate::ast::UnaryOp::Plus,
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("+5")
            );
        }

        #[test]
        fn test_binary_expressions() {
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::PoW,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2^5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::PoW,
                    Box::new(Expression::Const(Constant::Integer(1))),
                    Box::new(Expression::Const(Constant::Integer(3)))
                ),
                parse_expression("1**3")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::Mul,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2*5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::Div,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2/5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::Mod,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2%5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::Add,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2+5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::Sub,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2-5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::Eq,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2 = 5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::NotEq,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2 <> 5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::Lower,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2 < 5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::LowerEq,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2 <= 5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::Greater,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2 > 5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::GreaterEq,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2 >= 5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::And,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2 & 5")
            );
            assert_eq!(
                Expression::BinaryExpression(
                    BinOp::Or,
                    Box::new(Expression::Const(Constant::Integer(2))),
                    Box::new(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("2 | 5")
            );
        }

        #[test]
        fn test_parse_expression() {
            assert_eq!(
                Expression::Const(Constant::Money(42.42)),
                parse_expression("$42.42")
            );

            assert_eq!(
                Expression::PredefinedFunctionCall(
                    &FUNCTION_DEFINITIONS[get_function_definition("ABORT") as usize],
                    Vec::new()
                ),
                parse_expression("ABORT()")
            );
            assert_eq!(
                Expression::PredefinedFunctionCall(
                    &FUNCTION_DEFINITIONS[get_function_definition("ABS") as usize],
                    vec!(Expression::Const(Constant::Integer(5)))
                ),
                parse_expression("ABS(5)")
            );
        }

    */
}
