use crate::{
    ast::{Constant, ElseIfBlock, Expression, Statement, VarInfo},
    parser::{ParserError, ParserErrorType},
    tables::STATEMENT_DEFINITIONS,
};

use super::{tokens::Token, Tokenizer};

impl<'a> Tokenizer<'a> {
    pub fn skip_eol(&mut self) {
        while self.cur_token == Some(Token::Eol) || self.cur_token == Some(Token::Comment) {
            self.next_token();
        }
    }

    fn parse_while(&mut self) -> Statement {
        self.next_token();
        assert!(
            !(self.cur_token != Some(Token::LPar)),
            "'(' expected got: {:?}",
            self.cur_token
        );
        self.next_token();
        let cond = self.parse_expression().unwrap();

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
            while self.cur_token != Some(Token::EndWhile) {
                statements.push(self.parse_statement());
                self.next_token();
                self.skip_eol();
            }
            self.next_token(); // skip ENDWHILE
            Statement::DoWhile(Box::new(cond), statements.into_iter().flatten().collect())
        } else {
            Statement::While(Box::new(cond), Box::new(self.parse_statement().unwrap()))
        }
    }

    fn parse_block(&mut self) -> Statement {
        self.skip_eol();
        let mut statements = Vec::new();
        while self.cur_token != Some(Token::End) {
            statements.push(self.parse_statement());
            self.skip_eol();
        }
        self.next_token();
        Statement::Block(statements.into_iter().flatten().collect())
    }

    fn parse_for(&mut self) -> Statement {
        self.next_token();
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
            if let Some(e) = self.parse_expression() {
                Some(Box::new(e))
            } else {
                None
            }
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
            Box::new(from.unwrap()),
            Box::new(to.unwrap()),
            step,
            statements.into_iter().flatten().collect(),
        )
    }

    fn parse_if(&mut self) -> Statement {
        self.next_token();

        assert!(
            !(self.cur_token != Some(Token::LPar)),
            "'(' expected got: {:?}",
            self.cur_token
        );
        self.next_token();
        let cond = self.parse_expression().unwrap();

        assert!(
            !(self.cur_token != Some(Token::RPar)),
            "')' expected got: {:?}",
            self.cur_token
        );
        self.next_token();

        if self.cur_token != Some(Token::Then) {
            self.skip_eol();
            return Statement::If(Box::new(cond), Box::new(self.parse_statement().unwrap()));
        }
        self.next_token();
        let mut statements = Vec::new();
        self.skip_eol();
        while self.cur_token != Some(Token::EndIf)
            && self.cur_token != Some(Token::Else)
            && self.cur_token != Some(Token::ElseIf)
        {
            assert!(self.cur_token.is_some(), "unexpected eol");
            statements.push(self.parse_statement());
            self.next_token();
            self.skip_eol();
        }
        let mut else_if_blocks = Vec::new();
        if self.cur_token == Some(Token::ElseIf) {
            while self.cur_token == Some(Token::ElseIf) {
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
                if self.cur_token == Some(Token::Then) {
                    self.next_token();
                    self.skip_eol();
                    while self.cur_token != Some(Token::ElseIf)
                        && self.cur_token != Some(Token::Else)
                        && self.cur_token != Some(Token::EndIf)
                    {
                        statements.push(self.parse_statement());
                        self.next_token();
                        self.skip_eol();
                    }
                } else {
                    statements.push(self.parse_statement());
                    self.next_token();
                    self.skip_eol();
                }

                else_if_blocks.push(ElseIfBlock {
                    cond: Box::new(cond.unwrap()),
                    block: statements.into_iter().flatten().collect(),
                });
            }
        };

        let else_block = if self.cur_token == Some(Token::Else) {
            self.next_token();
            let mut statements = Vec::new();
            self.skip_eol();
            while self.cur_token != Some(Token::EndIf) {
                statements.push(self.parse_statement());
                self.next_token();
                self.skip_eol();
            }
            Some(statements)
        } else {
            None
        };

        self.next_token();

        Statement::IfThen(
            Box::new(cond),
            statements.into_iter().flatten().collect(),
            else_if_blocks,
            else_block.into_iter().flatten().collect(),
        )
    }

    fn parse_select(&mut self) -> Statement {
        self.next_token();

        assert!(
            !(self.cur_token != Some(Token::Case)),
            "'CASE' expected got: {:?}",
            self.cur_token
        );
        self.next_token();
        let case_expr = self.parse_expression().unwrap();
        self.next_token();
        self.skip_eol();

        let mut case_blocks = Vec::new();
        let mut else_block = None;

        while self.cur_token == Some(Token::Case) {
            self.next_token();
            if self.cur_token == Some(Token::Else) {
                self.next_token();
                let mut statements = Vec::new();
                self.skip_eol();
                while self.cur_token != Some(Token::EndSelect) {
                    statements.push(self.parse_statement());
                    self.next_token();
                    self.skip_eol();
                }
                else_block = Some(statements);
                break;
            };

            let expr = self.parse_expression().unwrap();
            self.next_token();
            self.skip_eol();

            let mut statements = Vec::new();
            while self.cur_token != Some(Token::Case) && self.cur_token != Some(Token::EndSelect) {
                statements.push(self.parse_statement());
                self.next_token();
                self.skip_eol();
            }
            case_blocks.push(ElseIfBlock {
                cond: Box::new(expr),
                block: statements.into_iter().flatten().collect(),
            });
        }

        self.next_token();
        self.skip_eol();

        Statement::Select(
            Box::new(case_expr),
            case_blocks,
            else_block.into_iter().flatten().collect(),
        )
    }

    /// Returns the parse statement of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.clone() {
            Some(Token::End) => {
                self.next_token();
                Some(Statement::End)
            }
            Some(Token::Begin) => {
                self.next_token();
                Some(self.parse_block())
            }
            Some(Token::While) => Some(self.parse_while()),
            Some(Token::Select) => Some(self.parse_select()),
            Some(Token::If) => Some(self.parse_if()),
            Some(Token::For) => Some(self.parse_for()),
            Some(Token::Let) => {
                self.next_token();
                let id = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                    self.next_token();
                    id
                } else {
                    panic!("no id found");
                };
                if self.cur_token == Some(Token::Eq) {
                    self.next_token();
                    let Some(right) = self.parse_expression() else {
                        return None;
                    };
                    return Some(Statement::Let(Box::new(VarInfo::Var0(id)), Box::new(right)));
                }
                panic!("error parsing let statement");
            }
            Some(Token::Break) => Some(Statement::Break),
            Some(Token::Continue) => Some(Statement::Continue),
            Some(Token::Return) => Some(Statement::Return),
            Some(Token::Gosub) => {
                self.next_token();
                if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                    self.next_token();
                    return Some(Statement::Gosub(id));
                }
                panic!("gosub expected a label, got: {:?}", self.cur_token);
            }
            Some(Token::Goto) => {
                self.next_token();
                if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                    self.next_token();
                    return Some(Statement::Goto(id));
                }
                panic!("goto expected a label, got: {:?}", self.cur_token);
            }
            Some(Token::Const(Constant::Builtin(c))) => {
                if let Some(value) = self.parse_call(c.name.to_string()) {
                    return Some(value);
                }
                self.next_token();
                return self.parse_statement();
            }

            Some(Token::Identifier(id)) => {
                if let Some(value) = self.parse_call(id) {
                    return Some(value);
                }
                self.next_token();
                return self.parse_statement();
            }

            Some(Token::Label(id)) => {
                self.next_token();
                Some(Statement::Label(id))
            }

            None => None,

            _ => {
                panic!("error unexpected token {:?}", self.cur_token);
            }
        }
    }

    fn parse_call(&mut self, id: String) -> Option<Statement> {
        self.next_token();
        for def in &STATEMENT_DEFINITIONS {
            if def.name.to_uppercase() == id {
                let mut params = Vec::new();
                while self.cur_token != Some(Token::Eol) && self.cur_token.is_some() {
                    if params.len() as i8 >= def.max_args {
                        break;
                    }
                    let Some(value) = self.parse_expression() else {
                        return None;
                    };
                    params.push(value);

                    if self.cur_token.is_none() {
                        break;
                    }
                    if self.cur_token == Some(Token::Comma) {
                        self.next_token();
                    } else {
                        break;
                    }
                }

                if (params.len() as i8) >= def.min_args {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::TooFewArguments(
                                def.name.to_string(),
                                params.len(),
                                def.min_args,
                            ),
                            range: self.lex.span(),
                        }));
                    return None;
                }

                if (params.len() as i8) <= def.max_args {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::TooManyArguments(
                                def.name.to_string(),
                                params.len(),
                                def.max_args,
                            ),
                            range: self.lex.span(),
                        }));
                    return None;
                }
                return Some(Statement::Call(def, params));
            }
        }

        if self.cur_token == Some(Token::Eq) {
            self.next_token();
            let Some(right) = self.parse_expression() else {
                return None;
            };

            return Some(Statement::Let(Box::new(VarInfo::Var0(id)), Box::new(right)));
        }
        if self.cur_token == Some(Token::LPar) {
            self.next_token();
            let mut params = Vec::new();

            while self.cur_token != Some(Token::RPar) {
                let Some(right) = self.parse_expression() else {
                    return None;
                };
                params.push(right);
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
                let Some(right) = self.parse_expression() else {
                    return None;
                };
                if params.len() == 1 {
                    return Some(Statement::Let(
                        Box::new(VarInfo::Var1(id, params[0].clone())),
                        Box::new(right),
                    ));
                }
                if params.len() == 2 {
                    return Some(Statement::Let(
                        Box::new(VarInfo::Var2(id, params[0].clone(), params[1].clone())),
                        Box::new(right),
                    ));
                }
                if params.len() == 3 {
                    return Some(Statement::Let(
                        Box::new(VarInfo::Var3(
                            id,
                            params[0].clone(),
                            params[1].clone(),
                            params[2].clone(),
                        )),
                        Box::new(right),
                    ));
                }
                panic!("too many dimensions: {}", params.len());
            }

            return Some(Statement::ProcedureCall(id, params));
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{constant::BuiltinConst, Constant, ElseIfBlock, Expression, Statement, VarInfo},
        parser::Tokenizer,
        tables::{StatementDefinition, STATEMENT_DEFINITIONS},
    };

    fn parse_statement(src: &str) -> Statement {
        let mut tokenizer = Tokenizer::new(src);
        tokenizer.next_token();
        tokenizer.parse_statement().unwrap()
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
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::FALSE)))
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
            parse_statement("INC VAR1\n")
        );
        assert_eq!(
            Statement::Call(
                get_statement_definition("DEC").unwrap(),
                vec![Expression::Identifier("VAR2".to_string()),]
            ),
            parse_statement("DEC VAR2\n")
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
                vec![Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))]
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
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::FALSE))),
                Box::new(Statement::End)
            ),
            parse_statement(" IF (FALSE) END")
        );
        let print_hello = parse_statement("PRINT \"Hello Word\"");
        let print_5 = parse_statement("PRINT 5");
        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                vec![print_hello.clone()],
                Vec::new(),
                None
            ),
            parse_statement(" IF (TRUE) THEN PRINT \"Hello Word\" ENDIF")
        );

        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                vec![print_hello.clone()],
                vec![ElseIfBlock {
                    cond: Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                    block: vec![print_5]
                }],
                None
            ),
            parse_statement("IF (TRUE) THEN PRINT \"Hello Word\" ELSEIF (TRUE) THEN PRINT 5 ENDIF")
        );

        let print_5 = parse_statement("PRINT 5");

        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::FALSE))),
                Vec::new(),
                Vec::new(),
                Some(vec![print_5])
            ),
            parse_statement("IF (FALSE) THEN ELSE\nPRINT 5 ENDIF")
        );
    }

    #[test]
    fn test_parse_while() {
        assert_eq!(
            Statement::While(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::FALSE))),
                Box::new(Statement::End)
            ),
            parse_statement("WHILE (FALSE) END")
        );
        assert_eq!(
            Statement::DoWhile(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                Vec::new()
            ),
            parse_statement("WHILE (TRUE) DO ENDWHILE")
        );
    }

    #[test]
    fn test_for_next() {
        assert_eq!(
            Statement::For(
                Box::new(Expression::Identifier("i".to_string())),
                Box::new(Expression::Const(Constant::Integer(1))),
                Box::new(Expression::Const(Constant::Integer(10))),
                None,
                Vec::new()
            ),
            parse_statement("FOR i = 1 TO 10 NEXT")
        );
        assert_eq!(
            Statement::For(
                Box::new(Expression::Identifier("i".to_string())),
                Box::new(Expression::Const(Constant::Integer(1))),
                Box::new(Expression::Const(Constant::Integer(10))),
                Some(Box::new(Expression::Const(Constant::Integer(5)))),
                Vec::new()
            ),
            parse_statement("FOR i = 1 TO 10 STEP 5 NEXT")
        );
        assert_eq!(
            Statement::For(
                Box::new(Expression::Identifier("i".to_string())),
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

    #[test]
    fn test_select_case() {
        assert_eq!(
            Statement::Select(
                Box::new(Expression::Identifier("I".to_string())),
                vec![
                    ElseIfBlock {
                        cond: Box::new(Expression::Const(Constant::Integer(1))),
                        block: vec![Statement::Call(
                            get_statement_definition("PRINT").unwrap(),
                            vec![Expression::Const(Constant::Integer(1))]
                        )]
                    },
                    ElseIfBlock {
                        cond: Box::new(Expression::Const(Constant::Integer(2))),
                        block: vec![Statement::Call(
                            get_statement_definition("PRINT").unwrap(),
                            vec![Expression::Const(Constant::Integer(2))]
                        )]
                    }
                ],
                Some(vec![Statement::Call(
                    get_statement_definition("PRINT").unwrap(),
                    vec![Expression::Const(Constant::Integer(3))]
                )])
            ),
            parse_statement(
                "SELECT CASE I\nCASE 1\n PRINT 1\nCASE 2\n  PRINT 2\nCASE ELSE\nPRINT 3 ENDSELECT"
            )
        );
        assert_eq!(
            Statement::DoWhile(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                Vec::new()
            ),
            parse_statement("WHILE (TRUE) DO ENDWHILE")
        );
    }
}
