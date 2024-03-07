use super::{
    tokens::{Token, Tokenizer},
    ParserInput, Span, Spanned,
};
use crate::{
    ast::{BinOp, Expression},
    tables::FUNCTION_DEFINITIONS,
};
use chumsky::prelude::*;

impl Tokenizer {
    pub fn parse_expression(&mut self) -> Expression {
        self.parse_bool()
    }
    fn parse_bool(&mut self) -> Expression {
        // it's correct on the upper level - it's very unusual
        if self.cur_token == Some(Token::Not) {
            self.next_token();
            return Expression::UnaryExpression(
                crate::ast::UnaryOp::Not,
                Box::new(self.parse_bool()),
            );
        }

        let mut expr = self.parse_comparison();
        while self.cur_token == Some(Token::Or) || self.cur_token == Some(Token::And) {
            let op = match self.cur_token {
                Some(Token::Or) => BinOp::Or,
                Some(Token::And) => BinOp::And,
                _ => panic!(),
            };
            self.next_token();
            let right = self.parse_comparison();
            expr = Expression::BinaryExpression(op, Box::new(expr), Box::new(right));
        }
        expr
    }

    fn parse_comparison(&mut self) -> Expression {
        let mut expr = self.parse_term();
        while self.cur_token == Some(Token::Greater)
            || self.cur_token == Some(Token::GreaterEq)
            || self.cur_token == Some(Token::Lower)
            || self.cur_token == Some(Token::LowerEq)
            || self.cur_token == Some(Token::Eq)
            || self.cur_token == Some(Token::NotEq)
        {
            let op = match self.cur_token {
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
                _ => panic!(),
            };
            self.next_token();
            let right = self.parse_factor();
            expr = Expression::BinaryExpression(op, Box::new(expr), Box::new(right));
        }

        expr
    }

    fn parse_factor(&mut self) -> Expression {
        let mut expr = self.parse_pow();
        while self.cur_token == Some(Token::Mul)
            || self.cur_token == Some(Token::Div)
            || self.cur_token == Some(Token::Mod)
        {
            let op = match self.cur_token {
                Some(Token::Mul) => BinOp::Mul,
                Some(Token::Div) => BinOp::Div,
                Some(Token::Mod) => BinOp::Mod,
                _ => panic!(),
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
            return Expression::UnaryExpression(
                crate::ast::UnaryOp::Plus,
                Box::new(self.parse_unary()),
            );
        }
        if self.cur_token == Some(Token::Sub) {
            self.next_token();
            return Expression::UnaryExpression(
                crate::ast::UnaryOp::Minus,
                Box::new(self.parse_unary()),
            );
        }
        if self.cur_token == Some(Token::Not) {
            self.next_token();
            return Expression::UnaryExpression(
                crate::ast::UnaryOp::Not,
                Box::new(self.parse_unary()),
            );
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Expression {
        let t = self.cur_token.clone();
        match t {
            Some(Token::Const(c)) => {
                self.next_token();
                Expression::Const(c.clone())
            }
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
                    assert!(
                        !(self.cur_token != Some(Token::RPar)),
                        "missing closing parens"
                    );
                    self.next_token();

                    let predef = crate::tables::get_function_definition(&id);
                    // TODO: Check parameter signature

                    if predef >= 0 {
                        return Expression::PredefinedFunctionCall(
                            &FUNCTION_DEFINITIONS[predef as usize],
                            params,
                        );
                    }
                    return Expression::FunctionCall(id, params);
                }

                Expression::Identifier(id)
            }
            Some(Token::LPar) => {
                self.next_token();
                let ret = Expression::Parens(Box::new(self.parse_expression()));
                assert!(!(self.cur_token != Some(Token::RPar)), "unclosed parens.");
                self.next_token();
                ret
            }
            _ => {
                panic!("invalid primary token {:?}", self.cur_token);
            }
        }
    }
}

pub fn expression_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens>,
    Spanned<Expression>,
    extra::Err<Rich<'tokens, Token, Span>>,
> + Clone {
    let expr = recursive(|expr| {
        let func = select! {
            Token::Identifier(c) => Expression::Identifier(c),
        }
        .then(
            expr.clone()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LPar), just(Token::RPar)),
        )
        .map(|(f, params)| {
            let id = f.to_string();
            let predef = crate::tables::get_function_definition(&id);
            // TODO: Check parameter signature

            if predef >= 0 {
                return Expression::PredefinedFunctionCall(
                    &FUNCTION_DEFINITIONS[predef as usize],
                    params,
                );
            }
            return Expression::FunctionCall(id, params);
        });

        let atom = func
            .or(select! {
                Token::Const(c) => Expression::Const(c),
            })
            .or(expr
                .delimited_by(just(Token::LPar), just(Token::RPar))
                .map(|a| Expression::Parens(Box::new(a))));

        let unary = just(Token::Not)
            .map(|_| crate::ast::UnaryOp::Not)
            .or(just(Token::Add).map(|_| crate::ast::UnaryOp::Plus))
            .or(just(Token::Sub).map(|_| crate::ast::UnaryOp::Minus))
            .repeated()
            .foldr(atom, |op, e| Expression::UnaryExpression(op, Box::new(e)));

        let pow = unary.clone().foldl(
            just(Token::PoW)
                .map(|_| crate::ast::BinOp::PoW)
                .then(unary)
                .repeated(),
            |lhs, (op, rhs)| Expression::BinaryExpression(op, Box::new(lhs), Box::new(rhs)),
        );

        let factor = pow.clone().foldl(
            choice((
                just(Token::Mul).map(|_| crate::ast::BinOp::Mul),
                just(Token::Div).map(|_| crate::ast::BinOp::Div),
                just(Token::Mod).map(|_| crate::ast::BinOp::Mod),
            ))
            .then(pow)
            .repeated(),
            |lhs, (op, rhs)| Expression::BinaryExpression(op, Box::new(lhs), Box::new(rhs)),
        );

        let term = factor.clone().foldl(
            choice((
                just(Token::Add).map(|_| crate::ast::BinOp::Add),
                just(Token::Sub).map(|_| crate::ast::BinOp::Sub),
            ))
            .then(factor)
            .repeated(),
            |lhs, (op, rhs)| Expression::BinaryExpression(op, Box::new(lhs), Box::new(rhs)),
        );

        let comparison = term.clone().foldl(
            choice((
                just(Token::Greater).map(|_| crate::ast::BinOp::Greater),
                just(Token::GreaterEq).map(|_| crate::ast::BinOp::GreaterEq),
                just(Token::Lower).map(|_| crate::ast::BinOp::Lower),
                just(Token::LowerEq).map(|_| crate::ast::BinOp::LowerEq),
                just(Token::Eq).map(|_| crate::ast::BinOp::Eq),
                just(Token::NotEq).map(|_| crate::ast::BinOp::NotEq),
            ))
            .then(term)
            .repeated(),
            |lhs, (op, rhs)| Expression::BinaryExpression(op, Box::new(lhs), Box::new(rhs)),
        );

        let bool_expr = comparison.clone().foldl(
            choice((
                just(Token::And).map(|_| crate::ast::BinOp::And),
                just(Token::Or).map(|_| crate::ast::BinOp::Or),
            ))
            .then(comparison)
            .repeated(),
            |lhs, (op, rhs)| Expression::BinaryExpression(op, Box::new(lhs), Box::new(rhs)),
        );

        let not_expr = just(Token::Not)
            .map(|_| crate::ast::UnaryOp::Not)
            .repeated()
            .foldr(bool_expr, |op, e| {
                Expression::UnaryExpression(op, Box::new(e))
            });

        not_expr
    });

    expr.map_with(|tok, e| (tok, e.span()))
        .recover_with(skip_then_retry_until(any().ignored(), end()))
}

#[cfg(test)]
mod tests {
    use chumsky::{input::Input, Parser};

    use crate::{
        ast::{BinOp, Constant, Expression},
        parser::{expression::expression_parser, tokens::lexer},
        tables::{get_function_definition, FUNCTION_DEFINITIONS, PPL_FALSE},
    };
    fn parse_expression(src: &str) -> Expression {
        println!("Parsing expression: {src}");
        let res = lexer().parse(src);
        if res.errors().len() > 0 {
            println!("{} lexer errors", res.errors().len());
        }
        for e in res.errors() {
            println!("Error: {e:?}");
        }
        assert_eq!(0, res.errors().len());

        let (tokens, mut errs) = res.into_output_errors();
        let tokens = tokens.unwrap();
        let expr =
            expression_parser().parse(tokens.as_slice().spanned((src.len()..src.len()).into()));
        let (expr, _span) = expr.output().unwrap();
        expr.clone()
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
                Box::new(Expression::Const(Constant::Integer(PPL_FALSE)))
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
}
