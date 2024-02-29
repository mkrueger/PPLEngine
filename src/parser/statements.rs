use crate::{
    ast::{ElseIfBlock, Expression, Statement, VarInfo},
    tables::STATEMENT_DEFINITIONS,
};

use super::tokens::{Token, Tokenizer};

impl Tokenizer {
    pub fn skip_eol(&mut self) {
        while self.cur_token == Some(Token::Eol) {
            self.next_token();
        }
    }

    fn parse_while(&mut self) -> Statement {
        if self.cur_token != Some(Token::LPar) {
            panic!("'(' expected got: {:?}", self.cur_token);
        }
        self.next_token();
        let cond = self.parse_expression();

        if self.cur_token != Some(Token::RPar) {
            panic!("')' expected got: {:?}", self.cur_token);
        }
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

        if self.cur_token != Some(Token::Eq) {
            panic!("'=' expected got: {:?}", self.cur_token);
        }
        self.next_token();
        let from = self.parse_expression();

        if self.cur_token != Some(Token::Identifier("TO".to_string())) {
            panic!("'TO' expected got: {:?}", self.cur_token);
        }
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
        if self.cur_token != Some(Token::LPar) {
            panic!("'(' expected got: {:?}", self.cur_token);
        }
        self.next_token();
        let cond = self.parse_expression();

        if self.cur_token != Some(Token::RPar) {
            panic!("')' expected got: {:?}", self.cur_token);
        }
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
            if self.cur_token.is_none() {
                panic!("unexpected eol");
            }
            statements.push(self.parse_statement());
            self.skip_eol();
        }

        let else_if_blocks = if self.cur_token == Some(Token::Identifier("ELSEIF".to_string())) {
            let mut blocks = Vec::new();

            while self.cur_token == Some(Token::Identifier("ELSEIF".to_string())) {
                self.next_token();

                if self.cur_token != Some(Token::LPar) {
                    panic!("'(' expected got: {:?}", self.cur_token);
                }
                self.next_token();
                let cond = self.parse_expression();

                if self.cur_token != Some(Token::RPar) {
                    panic!("')' expected got: {:?}", self.cur_token);
                }
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
        if let Some(Token::Identifier(id)) = self.cur_token.clone() {
            self.next_token();

            match id.as_str() {
                "END" => return Statement::End,
                "BEGIN" => return self.parse_block(),
                "WHILE" => return self.parse_while(),
                "IF" => return self.parse_if(),
                "FOR" => return self.parse_for(),
                "LET" => {
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
                "BREAK" => return Statement::Break,
                "CONTINUE" => return Statement::Continue,
                "RETURN" => return Statement::Return,
                "STOP" => return Statement::Stop,
                "GOSUB" => {
                    if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                        self.next_token();
                        return Statement::Gosub(id);
                    }
                    panic!("gosub expected a label, got: {:?}", self.cur_token);
                }
                "GOTO" => {
                    if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                        self.next_token();
                        return Statement::Goto(id);
                    }
                    panic!("goto expected a label, got: {:?}", self.cur_token);
                }
                "INC" => {
                    if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                        self.next_token();
                        return Statement::Inc(id);
                    }
                    panic!("Inc expected a label, got: {:?}", self.cur_token);
                }
                "DEC" => {
                    if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                        self.next_token();
                        return Statement::Dec(id);
                    }
                    panic!("Inc expected a label, got: {:?}", self.cur_token);
                }
                _ => {}
            }

            for def in &STATEMENT_DEFINITIONS {
                if def.name.to_uppercase() == id {
                    let mut params = Vec::new();
                    while self.cur_token != Some(Token::Eol) && self.cur_token != None {
                        params.push(self.parse_expression());

                        if self.cur_token == None {
                            break;
                        }
                        if self.cur_token == Some(Token::Comma) {
                            self.next_token();
                        } else {
                            break;
                        }
                    }

                    if (params.len() as i8) < def.min_args {
                        panic!(
                            "{} has too few arguments {} [{}:{}]",
                            def.name,
                            params.len(),
                            def.min_args,
                            def.max_args
                        );
                        // return Err(format!("{} has too few arguments {} [{}:{}]", def.name, z.1.len(), def.min_args, def.max_args));
                    }
                    if (params.len() as i8) > def.max_args {
                        panic!(
                            "{} has too many arguments {} [{}:{}]",
                            def.name,
                            params.len(),
                            def.min_args,
                            def.max_args
                        );
                        // return Err(format!("{} has too many arguments {} [{}:{}]", def.name, z.1.len(), def.min_args, def.max_args));
                    }
                    return Statement::Call(def, params);
                }
            }

            if self.cur_token == Some(Token::Eq) {
                self.next_token();
                let right = self.parse_expression();
                return Statement::Let(Box::new(VarInfo::Var0(id)), Box::new(right));
            } else if self.cur_token == Some(Token::LPar) {
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
        }

        if self.cur_token == Some(Token::Colon) {
            self.next_token();
            let id = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                self.next_token();
                id
            } else {
                panic!("error parsing label - no id found");
            };

            return Statement::Label(id);
        }

        panic!("error unexpected token {:?}", self.cur_token);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Constant, ElseIfBlock, Expression, Statement, VarInfo},
        parser::tokens::Tokenizer,
        tables::{StatementDefinition, PPL_FALSE, PPL_TRUE, STATEMENT_DEFINITIONS},
    };
    fn parse_statement(str: &str) -> Statement {
        let mut tokenizer = Tokenizer::new(str);
        tokenizer.next_token();
        tokenizer.parse_statement()
    }
    fn get_statement_definition(name: &str) -> Option<&'static StatementDefinition> {
        let name = name.to_uppercase();
        for def in &STATEMENT_DEFINITIONS {
            if def.name.to_uppercase() == name {
                return Some(def);
            }
        }
        None
    }

    #[test]
    fn test_parse_statement() {
        assert_eq!(
            Statement::Let(
                Box::new(VarInfo::Var0("FOO_BAR".to_string())),
                Box::new(Expression::Const(Constant::Integer(1)))
            ),
            parse_statement("foo_bar=1")
        );
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
    fn test_let() {
        assert_eq!(
            Statement::Let(
                Box::new(VarInfo::Var0("FOO".to_string())),
                Box::new(Expression::Const(Constant::Integer(PPL_FALSE)))
            ),
            parse_statement("LET FOO = FALSE")
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
            Statement::Inc("VAR1".to_string()),
            parse_statement("INC VAR1")
        );
        assert_eq!(
            Statement::Dec("VAR2".to_string()),
            parse_statement("DEC VAR2")
        );
    }

    #[test]
    fn test_parse_simple_noncalls() {
        assert_eq!(Statement::End, parse_statement("End ; Predifined End"));

        assert_eq!(Statement::Break, parse_statement("BREAK"));

        assert_eq!(Statement::Continue, parse_statement("CONTINUE"));

        assert_eq!(Statement::Return, parse_statement("RETURN"));

        assert_eq!(Statement::Stop, parse_statement("STOP"));
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
                Some(Box::new(Expression::Minus(Box::new(Expression::Const(
                    Constant::Integer(1)
                ))))),
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
