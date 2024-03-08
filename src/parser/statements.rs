use crate::{
    ast::{ElseIfBlock, Expression, Statement, VarInfo},
    tables::STATEMENT_DEFINITIONS,
};
use chumsky::{input::ValueInput, prelude::*};

use super::{expression::expression_parser, tokens::Token, Tokenizer};

impl<'a> Tokenizer<'a> {
    pub fn skip_eol(&mut self) {}

    fn parse_while(&mut self) -> Statement {
        assert!(
            !(self.cur_token != Some(Token::LPar)),
            "'(' expected got: {:?}",
            self.cur_token
        );
        self.next_token();
        let cond = self.parse_expression();

        assert!(
            !(self.cur_token != Some(Token::RPar)),
            "')' expected got: {:?}",
            self.cur_token
        );
        self.next_token();

        if self.cur_token == Some(Token::Identifier("DO".to_string())) {
            self.next_token();
            let mut statements = Vec::new();
            self.skip_eol();
            while self.cur_token != Some(Token::Identifier("ENDWHILE".to_string())) {
                statements.push(self.parse_statement());
                self.skip_eol();
            }
            self.next_token(); // skip ENDWHILE
            Statement::DoWhile(Box::new(cond), statements)
        } else {
            Statement::While(Box::new(cond), Box::new(self.parse_statement()))
        }
    }

    fn parse_block(&mut self) -> Statement {
        self.skip_eol();
        let mut statements = Vec::new();
        while self.cur_token != Some(Token::Identifier("END".to_string())) {
            statements.push(self.parse_statement());
            self.skip_eol();
        }
        self.next_token();
        Statement::Block(statements)
    }

    fn parse_for(&mut self) -> Statement {
        let var = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
            self.next_token();

            Expression::Identifier(id)
        } else {
            panic!("identifier expected got: {:?}", self.cur_token)
        };

        assert!(
            !(self.cur_token != Some(Token::Eq)),
            "'=' expected got: {:?}",
            self.cur_token
        );
        self.next_token();
        let from = self.parse_expression();

        assert!(
            !(self.cur_token != Some(Token::Identifier("TO".to_string()))),
            "'TO' expected got: {:?}",
            self.cur_token
        );
        self.next_token();

        let to = self.parse_expression();

        let step = if self.cur_token == Some(Token::Identifier("STEP".to_string())) {
            self.next_token();
            Some(Box::new(self.parse_expression()))
        } else {
            None
        };

        let mut statements = Vec::new();
        self.skip_eol();
        while self.cur_token != Some(Token::Identifier("NEXT".to_string())) {
            statements.push(self.parse_statement());
            self.skip_eol();
        }
        self.next_token(); // skip NEXT

        Statement::For(
            Box::new(var),
            Box::new(from),
            Box::new(to),
            step,
            statements,
        )
    }

    fn parse_if(&mut self) -> Statement {
        assert!(
            !(self.cur_token != Some(Token::LPar)),
            "'(' expected got: {:?}",
            self.cur_token
        );
        self.next_token();
        let cond = self.parse_expression();

        assert!(
            !(self.cur_token != Some(Token::RPar)),
            "')' expected got: {:?}",
            self.cur_token
        );
        self.next_token();

        if self.cur_token != Some(Token::Identifier("THEN".to_string())) {
            self.skip_eol();
            return Statement::If(Box::new(cond), Box::new(self.parse_statement()));
        }
        self.next_token();
        let mut statements = Vec::new();
        self.skip_eol();
        while self.cur_token != Some(Token::Identifier("ENDIF".to_string()))
            && self.cur_token != Some(Token::Identifier("ELSE".to_string()))
            && self.cur_token != Some(Token::Identifier("ELSEIF".to_string()))
        {
            assert!(self.cur_token.is_some(), "unexpected eol");
            statements.push(self.parse_statement());
            self.skip_eol();
        }

        let else_if_blocks = if self.cur_token == Some(Token::Identifier("ELSEIF".to_string())) {
            let mut blocks = Vec::new();

            while self.cur_token == Some(Token::Identifier("ELSEIF".to_string())) {
                self.next_token();

                assert!(
                    !(self.cur_token != Some(Token::LPar)),
                    "'(' expected got: {:?}",
                    self.cur_token
                );
                self.next_token();
                let cond = self.parse_expression();

                assert!(
                    !(self.cur_token != Some(Token::RPar)),
                    "')' expected got: {:?}",
                    self.cur_token
                );
                self.next_token();

                let mut statements = Vec::new();
                if self.cur_token == Some(Token::Identifier("THEN".to_string())) {
                    self.next_token();
                    self.skip_eol();
                    while self.cur_token != Some(Token::Identifier("ELSEIF".to_string()))
                        && self.cur_token != Some(Token::Identifier("ELSE".to_string()))
                        && self.cur_token != Some(Token::Identifier("ENDIF".to_string()))
                    {
                        statements.push(self.parse_statement());
                        self.skip_eol();
                    }
                } else {
                    statements.push(self.parse_statement());
                    self.skip_eol();
                }

                blocks.push(ElseIfBlock {
                    cond: Box::new(cond),
                    block: statements,
                });
            }
            Some(blocks)
        } else {
            None
        };

        let else_block = if self.cur_token == Some(Token::Identifier("ELSE".to_string())) {
            self.next_token();
            let mut statements = Vec::new();
            self.skip_eol();
            while self.cur_token != Some(Token::Identifier("ENDIF".to_string())) {
                statements.push(self.parse_statement());
                self.skip_eol();
            }
            Some(statements)
        } else {
            None
        };

        self.next_token();

        Statement::IfThen(Box::new(cond), statements, else_if_blocks, else_block)
    }

    pub fn parse_statement(&mut self) -> Statement {
        match self.cur_token.clone() {
            Some(Token::End) => return Statement::End,
            Some(Token::Begin) => return self.parse_block(),
            Some(Token::While) => return self.parse_while(),
            Some(Token::If) => return self.parse_if(),
            Some(Token::For) => return self.parse_for(),
            Some(Token::Let) => {
                let id = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                    self.next_token();
                    id
                } else {
                    panic!("no id found");
                };
                if self.cur_token == Some(Token::Eq) {
                    self.next_token();
                    let right = self.parse_expression();
                    return Statement::Let(Box::new(VarInfo::Var0(id)), Box::new(right));
                }
                panic!("error parsing let statement");
            }
            Some(Token::Break) => return Statement::Break,
            Some(Token::Continue) => return Statement::Continue,
            Some(Token::Return) => return Statement::Return,
            Some(Token::Gosub) => {
                if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                    self.next_token();
                    return Statement::Gosub(id);
                }
                panic!("gosub expected a label, got: {:?}", self.cur_token);
            }
            Some(Token::Goto) => {
                if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                    self.next_token();
                    return Statement::Goto(id);
                }
                panic!("goto expected a label, got: {:?}", self.cur_token);
            }

            Some(Token::Identifier(id)) => {
                self.next_token();
                for def in &STATEMENT_DEFINITIONS {
                    if def.name.to_uppercase() == id {
                        let mut params = Vec::new();
                        while self.cur_token.is_some() {
                            if params.len() as i8 >= def.max_args {
                                break;
                            }
                            params.push(self.parse_expression());

                            if self.cur_token.is_none() {
                                break;
                            }
                            if self.cur_token == Some(Token::Comma) {
                                self.next_token();
                            } else {
                                break;
                            }
                        }

                        assert!(
                            (params.len() as i8) >= def.min_args,
                            "{} has too few arguments {} [{}:{}]",
                            def.name,
                            params.len(),
                            def.min_args,
                            def.max_args
                        );
                        assert!(
                            (params.len() as i8) <= def.max_args,
                            "{} has too many arguments {} [{}:{}]",
                            def.name,
                            params.len(),
                            def.min_args,
                            def.max_args
                        );
                        return Statement::Call(def, params);
                    }
                }

                if self.cur_token == Some(Token::Eq) {
                    self.next_token();
                    let right = self.parse_expression();
                    return Statement::Let(Box::new(VarInfo::Var0(id)), Box::new(right));
                }

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
                    if self.cur_token == Some(Token::Eq) {
                        self.next_token();
                        let right = self.parse_expression();
                        if params.len() == 1 {
                            return Statement::Let(
                                Box::new(VarInfo::Var1(id, params[0].clone())),
                                Box::new(right),
                            );
                        }
                        if params.len() == 2 {
                            return Statement::Let(
                                Box::new(VarInfo::Var2(id, params[0].clone(), params[1].clone())),
                                Box::new(right),
                            );
                        }
                        if params.len() == 3 {
                            return Statement::Let(
                                Box::new(VarInfo::Var3(
                                    id,
                                    params[0].clone(),
                                    params[1].clone(),
                                    params[2].clone(),
                                )),
                                Box::new(right),
                            );
                        }
                        panic!("too many dimensions: {}", params.len());
                    }

                    return Statement::ProcedureCall(id, params);
                }

                panic!("invalid identifier {:?}", self.cur_token);
            }

            Some(Token::Label(id)) => {
                self.next_token();
                return Statement::Label(id);
            }

            _ => {
                panic!("error unexpected token {:?}", self.cur_token);
            }
        }
    }
}

pub fn statement_parser<'a, I>(
) -> impl Parser<'a, I, Statement, extra::Err<Rich<'a, Token>>> + Clone
where
    I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
    let stmt = recursive(|stmt| {
        let expr = expression_parser();

        let ident = select! { Token::Identifier(ident) => ident };

        let let_stmt = just(Token::Let)
            .or_not()
            .then(ident)
            .map(|(_let, id)| id)
            .then_ignore(just(Token::Eq))
            .then(expr)
            .map(|(lhs, rhs)| Statement::Let(Box::new(VarInfo::Var0(lhs)), Box::new(rhs)));

        let goto_stmt = just(Token::Goto)
            .then(ident)
            .map(|(_, rhs)| Statement::Goto(rhs));
        let gosub_stmt = just(Token::Gosub)
            .then(ident)
            .map(|(_, rhs)| Statement::Gosub(rhs));

        let label_stmt = select! { Token::Label(ident) => ident }.map(Statement::Label);

        let proc_call = select! {
            Token::Identifier(c) => Expression::Identifier(c),
        }
        .then(
            expression_parser()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LPar), just(Token::RPar)),
        )
        .map(|(f, params)| {
            let id = f.to_string();
            Statement::ProcedureCall(id, params)
        });

        let call = select! { Token::Identifier(ident) => ident }
            .then(
                expression_parser()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .or_not(),
            )
            .map(|(def, args)| {
                let name = def.to_uppercase();
                let def = STATEMENT_DEFINITIONS
                    .iter()
                    .find(|&def| def.name.to_uppercase() == name)
                    .unwrap();
                let mut params = Vec::new();
                if let Some(args) = args {
                    assert!(
                        (args.len() as i8) >= def.min_args,
                        "{} has too few arguments {} [{}:{}]",
                        def.name,
                        args.len(),
                        def.min_args,
                        def.max_args
                    );
                    assert!(
                        (args.len() as i8) <= def.max_args,
                        "{} has too many arguments {} [{}:{}]",
                        def.name,
                        args.len(),
                        def.min_args,
                        def.max_args
                    );
                    for arg in args {
                        params.push(arg);
                    }
                }
                Statement::Call(def, params)
            });

        let if_stmt = just(Token::If)
            .then(expression_parser().delimited_by(just(Token::LPar), just(Token::RPar))) /*
            .then(choice((
                just(Token::Then)
                .then(stmt.clone().map(|(_, stmt)| stmt).repeated().collect::<Vec<_>>().then(just(Token::EndIf)))
                .map(|(cond, (then_stmt, epxr))| Statement::IfThen(Box::new(Expression::Const(crate::ast::Constant::Integer(0))), then_stmt, None, None))
                ,
            )))*/
            .then(stmt.clone())
            .map(
                |((_, cond), then_stmt)| Statement::If(Box::new(cond), Box::new(then_stmt)), /*match stmt {
                                                                                                 Statement::If(_, then_stmt) => Statement::If(Box::new(cond.0), then_stmt),
                                                                                                 Statement::IfThen(_, then_stmt, else_if, else_stmt) => Statement::IfThen(Box::new(cond.0), then_stmt, else_if, else_stmt),
                                                                                                 stmt => stmt
                                                                                             }}*/
            );

        let_stmt
            .or(goto_stmt)
            .or(gosub_stmt)
            .or(label_stmt)
            .or(just(Token::End).to(Statement::End))
            .or(just(Token::Return).to(Statement::Return))
            .or(just(Token::Break).to(Statement::Break))
            .or(just(Token::Continue).to(Statement::Continue))
            .or(proc_call)
            .or(call)
            .or(if_stmt)
    });
    /*
    stmt.map_with(|tok, e| (tok, e.span()))
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        */

    stmt
}

#[cfg(test)]
mod tests {
    use chumsky::{
        input::{Input, Stream},
        Parser,
    };
    use logos::Logos;

    use crate::{
        ast::{Constant, ElseIfBlock, Expression, Statement, VarInfo},
        parser::{statements::statement_parser, tokens::Token},
        tables::{StatementDefinition, PPL_FALSE, PPL_TRUE, STATEMENT_DEFINITIONS},
    };

    fn parse_statement(src: &str) -> Statement {
        println!("Parsing statement: {src}");
        let token_iter = Token::lexer(src)
            .spanned()
            // Convert logos errors into tokens. We want parsing to be recoverable and not fail at the lexing stage, so
            // we have a dedicated `Token::Error` variant that represents a token error that was previously encountered
            .map(|(tok, span)| match tok {
                // Turn the `Range<usize>` spans logos gives us into chumsky's `SimpleSpan` via `Into`, because it's easier
                // to work with
                Ok(tok) => (tok, span.into()),
                Err(()) => (Token::Comment, span.into()),
            });
        let token_stream = Stream::from_iter(token_iter)
            // Tell chumsky to split the (Token, SimpleSpan) stream into its parts so that it can handle the spans for us
            // This involves giving chumsky an 'end of input' span: we just use a zero-width span at the end of the string
            .spanned((src.len()..src.len()).into());

        let stmt = statement_parser().parse(token_stream);

        if stmt.errors().len() > 0 {
            println!("{} lexer errors", stmt.errors().len());
        }
        for e in stmt.errors() {
            println!("Error: {e:?}");
        }

        stmt.output().unwrap().clone()
    }

    fn get_statement_definition(name: &str) -> Option<&'static StatementDefinition> {
        let name = name.to_uppercase();
        STATEMENT_DEFINITIONS
            .iter()
            .find(|&def| def.name.to_uppercase() == name)
    }

    #[test]
    fn test_let() {
        assert_eq!(
            Statement::Let(
                Box::new(VarInfo::Var0("foo_bar".to_string())),
                Box::new(Expression::Const(Constant::Integer(1)))
            ),
            parse_statement("foo_bar=1")
        );

        assert_eq!(
            Statement::Let(
                Box::new(VarInfo::Var0("FOO".to_string())),
                Box::new(Expression::Const(Constant::Integer(PPL_FALSE)))
            ),
            parse_statement("LET FOO = FALSE")
        );
    }

    #[test]
    fn test_parse_statement() {
        assert_eq!(
            Statement::Call(
                get_statement_definition("ADJTIME").unwrap(),
                vec![Expression::Const(Constant::Integer(1))]
            ),
            parse_statement("ADJTIME 1")
        );
        assert_eq!(
            Statement::Call(
                get_statement_definition("ANSIPOS").unwrap(),
                vec![
                    Expression::Const(Constant::Integer(1)),
                    Expression::Const(Constant::Integer(2))
                ]
            ),
            parse_statement("ANSIPOS 1, 2")
        );
        assert_eq!(
            Statement::Call(
                get_statement_definition("BROADCAST").unwrap(),
                vec![
                    Expression::Const(Constant::Integer(1)),
                    Expression::Const(Constant::Integer(2)),
                    Expression::Const(Constant::Integer(3))
                ]
            ),
            parse_statement("BROADCAST 1, 2, 3")
        );
        assert_eq!(
            Statement::Call(get_statement_definition("BYE").unwrap(), vec![]),
            parse_statement("BYE")
        );
        assert_eq!(
            Statement::Call(get_statement_definition("PRINTLN").unwrap(), vec![]),
            parse_statement("PRINTLN")
        );
        assert_eq!(
            Statement::Call(
                get_statement_definition("PRINTLN").unwrap(),
                vec![Expression::Const(Constant::String(
                    "Hello World".to_string()
                ))]
            ),
            parse_statement("PRINTLN \"Hello World\"")
        );
    }

    #[test]
    fn test_parse_hello_world() {
        // check_statements(";This is a comment\nPRINT \"Hello World\"\n\t\n\n", vec![Statement::Call(get_statement_definition("PRINT").unwrap(), vec![Expression::Const(Constant::String("Hello World".to_string()))])]);
    }

    #[test]
    fn test_gotogosub() {
        assert_eq!(
            Statement::Goto("LABEL1".to_string()),
            parse_statement("GOTO LABEL1")
        );

        assert_eq!(
            Statement::Gosub("LABEL2".to_string()),
            parse_statement("GOSUB LABEL2")
        );
        assert_eq!(
            Statement::Label("LABEL1".to_string()),
            parse_statement(":LABEL1")
        );
    }

    #[test]
    fn test_incdec() {
        assert_eq!(
            Statement::Call(
                get_statement_definition("INC").unwrap(),
                vec![Expression::Identifier("VAR1".to_string()),]
            ),
            parse_statement("INC VAR1")
        );
        assert_eq!(
            Statement::Call(
                get_statement_definition("DEC").unwrap(),
                vec![Expression::Identifier("VAR2".to_string()),]
            ),
            parse_statement("DEC VAR2")
        );
    }

    #[test]
    fn test_parse_simple_noncalls() {
        assert_eq!(Statement::End, parse_statement("End ; Predifined End"));

        assert_eq!(Statement::Break, parse_statement("BREAK"));

        assert_eq!(Statement::Continue, parse_statement("CONTINUE"));

        assert_eq!(Statement::Return, parse_statement("RETURN"));
    }

    #[test]
    fn test_procedure_calls() {
        assert_eq!(
            Statement::ProcedureCall("PROC".to_string(), Vec::new()),
            parse_statement("PROC()")
        );
        assert_eq!(
            Statement::ProcedureCall(
                "PROC".to_string(),
                vec![Expression::Const(Constant::Integer(PPL_TRUE))]
            ),
            parse_statement("PROC(TRUE)")
        );
        assert_eq!(
            Statement::ProcedureCall(
                "PROC".to_string(),
                vec![
                    Expression::Const(Constant::Integer(5)),
                    Expression::Const(Constant::Integer(7))
                ]
            ),
            parse_statement("PROC(5, 7)")
        );
    }

    #[test]
    fn test_parse_if() {
        assert_eq!(
            Statement::If(
                Box::new(Expression::Const(Constant::Integer(PPL_FALSE))),
                Box::new(Statement::End)
            ),
            parse_statement("IF (FALSE) END")
        );
        let print_hello = parse_statement("PRINT \"Hello Word\"");
        let print_5 = parse_statement("PRINT 5");
        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Integer(PPL_TRUE))),
                vec![print_hello.clone()],
                None,
                None
            ),
            parse_statement("IF (TRUE) THEN PRINT \"Hello Word\" ENDIF")
        );

        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Integer(PPL_TRUE))),
                vec![print_hello.clone()],
                Some(vec![ElseIfBlock {
                    cond: Box::new(Expression::Const(Constant::Integer(PPL_TRUE))),
                    block: vec![print_5]
                }]),
                None
            ),
            parse_statement("IF (TRUE) THEN PRINT \"Hello Word\" ELSEIF (TRUE) THEN PRINT 5 ENDIF")
        );

        let print_5 = parse_statement("PRINT 5");

        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Integer(PPL_FALSE))),
                Vec::new(),
                None,
                Some(vec![print_5])
            ),
            parse_statement("IF (FALSE) THEN ELSE PRINT 5 ENDIF")
        );
    }

    #[test]
    fn test_parse_while() {
        assert_eq!(
            Statement::While(
                Box::new(Expression::Const(Constant::Integer(PPL_FALSE))),
                Box::new(Statement::End)
            ),
            parse_statement("WHILE (FALSE) END")
        );
        assert_eq!(
            Statement::DoWhile(
                Box::new(Expression::Const(Constant::Integer(PPL_TRUE))),
                Vec::new()
            ),
            parse_statement("WHILE (TRUE) DO ENDWHILE")
        );
    }

    #[test]
    fn test_for_next() {
        assert_eq!(
            Statement::For(
                Box::new(Expression::Identifier("I".to_string())),
                Box::new(Expression::Const(Constant::Integer(1))),
                Box::new(Expression::Const(Constant::Integer(10))),
                None,
                Vec::new()
            ),
            parse_statement("FOR i = 1 TO 10 NEXT")
        );
        assert_eq!(
            Statement::For(
                Box::new(Expression::Identifier("I".to_string())),
                Box::new(Expression::Const(Constant::Integer(1))),
                Box::new(Expression::Const(Constant::Integer(10))),
                Some(Box::new(Expression::Const(Constant::Integer(5)))),
                Vec::new()
            ),
            parse_statement("FOR i = 1 TO 10 STEP 5 NEXT")
        );
        assert_eq!(
            Statement::For(
                Box::new(Expression::Identifier("I".to_string())),
                Box::new(Expression::Const(Constant::Integer(1))),
                Box::new(Expression::Const(Constant::Integer(10))),
                Some(Box::new(Expression::UnaryExpression(
                    crate::ast::UnaryOp::Minus,
                    Box::new(Expression::Const(Constant::Integer(1)))
                ))),
                Vec::new()
            ),
            parse_statement("FOR i = 1 TO 10 STEP -1 NEXT")
        );
    }

    #[test]
    fn test_parse_block() {
        assert_eq!(Statement::Block(Vec::new()), parse_statement("BEGIN END"));
    }
}
