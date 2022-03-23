use crate::{ast::{Expression, BinOp}, tables::FUNCTION_DEFINITIONS};
use super::tokens::{Tokenizer, Token};

impl Tokenizer {
    pub fn parse_expression(&mut self) -> Expression {
        self.parse_bool()
    }
    fn parse_bool(&mut self) -> Expression {
        // it's correct on the upper level - it's very unusual
        if self.cur_token == Some(Token::Not) {
            self.next_token();
            return Expression::Not(Box::new(self.parse_bool()));
        }

        let mut expr = self.parse_comparison();
        while self.cur_token == Some(Token::Or) || self.cur_token == Some(Token::And) {
            let op = match self.cur_token {
                Some(Token::Or) => BinOp::Or,
                Some(Token::And) => BinOp::And,
                _ => panic!()
            };
            self.next_token();
            let right = self.parse_comparison();
            expr = Expression::BinaryExpression(op, Box::new(expr), Box::new(right));
        }
        expr
    }

    fn parse_comparison(&mut self) -> Expression {
        let mut expr = self.parse_term();
        while self.cur_token == Some(Token::Greater) || self.cur_token == Some(Token::GreaterEq) || self.cur_token == Some(Token::Lower) || self.cur_token == Some(Token::LowerEq) || self.cur_token == Some(Token::Eq) || self.cur_token == Some(Token::NotEq) {
            let op = match self.cur_token {
                Some(Token::Greater) => BinOp::Greater,
                Some(Token::GreaterEq) => BinOp::GreaterEq,
                Some(Token::Lower) => BinOp::Lower,
                Some(Token::LowerEq) => BinOp::LowerEq,
                Some(Token::Eq) => BinOp::Eq,
                Some(Token::NotEq) => BinOp::NotEq,
                _ => panic!()
            };
            self.next_token();

            let right = self.parse_term();
            expr = Expression::BinaryExpression(op, Box::new(expr), Box::new(right));
        }

        expr
    }

    fn parse_term(&mut self) -> Expression {
        let mut expr = self.parse_factor();
        while self.cur_token == Some(Token::Add) || self.cur_token == Some(Token::Sub) {
            let op = match self.cur_token {
                Some(Token::Add) => BinOp::Add,
                Some(Token::Sub) => BinOp::Sub,
                _ => panic!()
            };
            self.next_token();
            let right = self.parse_factor();
            expr = Expression::BinaryExpression(op, Box::new(expr), Box::new(right));
        }

        expr
    }

    fn parse_factor(&mut self) -> Expression {
        let mut expr = self.parse_pow();
        while self.cur_token == Some(Token::Mul) || self.cur_token == Some(Token::Div) || self.cur_token == Some(Token::Mod) {
            let op = match self.cur_token {
                Some(Token::Mul) => BinOp::Mul,
                Some(Token::Div) => BinOp::Div,
                Some(Token::Mod) => BinOp::Mod,
                _ => panic!()
            };
            self.next_token();
            let right = self.parse_pow();
            expr = Expression::BinaryExpression(op, Box::new(expr), Box::new(right));
        }
        expr
    }

    fn parse_pow(&mut self) -> Expression {
        let mut expr = self.parse_unary();
        while self.cur_token == Some(Token::PoW) {
            self.next_token();
            let right = self.parse_unary();
            expr = Expression::BinaryExpression(BinOp::PoW, Box::new(expr), Box::new(right));
        }
        expr
    }

    fn parse_unary(&mut self) -> Expression {
        if self.cur_token == Some(Token::Add) {
            self.next_token();
            return Expression::Plus(Box::new(self.parse_unary()));
        }
        if self.cur_token == Some(Token::Sub) {
            self.next_token();
            return Expression::Minus(Box::new(self.parse_unary()));
        }
        
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Expression {
        let t = self.cur_token.clone();
        match t{
            Some(Token::Const(c)) => { 
                self.next_token();
                Expression::Const(c.clone())
            },
            Some(Token::Identifier(id)) => {
                self.next_token();

                if self.cur_token == Some(Token::LPar) {
                    self.next_token();
                    let mut params = Vec::new();

                    while self.cur_token != Some(Token::RPar) {
                        params.push(self.parse_expression());
                        if self.cur_token == Some(Token::Comma) {
                            self.next_token();
                        }
                    }
                    if self.cur_token != Some(Token::RPar) {
                        panic!("missing closing parens");
                    }
                    self.next_token();

                    let predef = crate::tables::get_function_definition(&id);
                    // TODO: Check parameter signature

                    if predef >= 0 {
                        return Expression::PredefinedFunctionCall(&FUNCTION_DEFINITIONS[predef as usize], params);
                    } else {
                        return Expression::FunctionCall(id, params);
                    }
                }

                Expression::Identifier(id)
            },
            Some(Token::LPar) => { 
                self.next_token();
                let ret = Expression::Parens(Box::new(self.parse_expression()));
                if self.cur_token != Some(Token::RPar) {
                    panic!("unclosed parens.");
                }
                self.next_token();
                ret
            }
            _ => { panic!("invalid primary token {:?}", self.cur_token); }
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::{parser::tokens::{Tokenizer}, ast::{Constant, Expression, BinOp}, tables::{PPL_FALSE, get_function_definition, FUNCTION_DEFINITIONS}};
    fn parse_expression(str: &str) -> Expression {
        let mut tokenizer = Tokenizer::new(str);
        tokenizer.next_token();
        tokenizer.parse_expression()
    }

    #[test]
    fn test_parse_expression() {
        assert_eq!(Expression::Const(Constant::Money(42.42)), parse_expression("$42.42"));
        assert_eq!(Expression::Parens(Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("(5)"));
        assert_eq!(Expression::Not(Box::new(Expression::Const(Constant::Integer(PPL_FALSE)))), parse_expression("!FALSE"));
        assert_eq!(Expression::PredefinedFunctionCall(&FUNCTION_DEFINITIONS[get_function_definition("ABORT") as usize], Vec::new()), parse_expression("ABORT()"));
        assert_eq!(Expression::PredefinedFunctionCall(&FUNCTION_DEFINITIONS[get_function_definition("ABS") as usize], vec!(Expression::Const(Constant::Integer(5)))), parse_expression("ABS(5)"));
        assert_eq!(Expression::BinaryExpression(BinOp::PoW, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2^5"));
        assert_eq!(Expression::BinaryExpression(BinOp::Mul, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2*5"));
        assert_eq!(Expression::BinaryExpression(BinOp::Div, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2/5"));
        assert_eq!(Expression::BinaryExpression(BinOp::Mod, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2%5"));
        assert_eq!(Expression::BinaryExpression(BinOp::Add, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2+5"));
        assert_eq!(Expression::BinaryExpression(BinOp::Sub, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2-5"));
        assert_eq!(Expression::BinaryExpression(BinOp::Eq, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2 = 5"));
        assert_eq!(Expression::BinaryExpression(BinOp::NotEq, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2 <> 5"));
        assert_eq!(Expression::BinaryExpression(BinOp::Lower, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2 < 5"));
        assert_eq!(Expression::BinaryExpression(BinOp::LowerEq, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2 <= 5"));
        assert_eq!(Expression::BinaryExpression(BinOp::Greater, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2 > 5"));
        assert_eq!(Expression::BinaryExpression(BinOp::GreaterEq, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2 >= 5"));
        assert_eq!(Expression::BinaryExpression(BinOp::And, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2 & 5"));
        assert_eq!(Expression::BinaryExpression(BinOp::Or, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))), parse_expression("2 | 5"));
    }
}