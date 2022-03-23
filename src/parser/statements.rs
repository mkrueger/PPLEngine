use crate::{
    ast::{Expression, Statement},
    tables::STATEMENT_DEFINITIONS,
};

use super::tokens::{Token, Tokenizer};

impl Tokenizer {
    pub fn parse_statement(&mut self) -> Statement {
        if let Some(Token::Identifier(id)) = self.cur_token.clone() {
            self.next_token();

            match id.as_str() {
                "END" => return Statement::End,
                "WHILE" => panic!("todo"),
                "IF" => panic!("todo"),
                "DO" => panic!("todo"),
                "FOR" => panic!("todo"),
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
                        return Statement::Let(Box::new(Expression::Identifier(id)), Box::new(right));
                    } 
                    panic!("error parsing let statement");
                },
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
                        } else if self.cur_token != Some(Token::Eol) {
                            panic!("error parsing parameter list : {:?}!", self.cur_token);
                        }
                    }
                    self.next_token();
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
                return Statement::Let(Box::new(Expression::Identifier(id)), Box::new(right));
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
        ast::{BinOp, Constant, Expression, Statement, ElseIfBlock},
        parser::tokens::Tokenizer,
        tables::{
            get_function_definition, StatementDefinition, FUNCTION_DEFINITIONS, PPL_FALSE,
            STATEMENT_DEFINITIONS, PPL_TRUE,
        },
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
                Box::new(Expression::Identifier("FOO_BAR".to_string())),
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
            Statement::Let(Box::new(Expression::Identifier("FOO".to_string())), Box::new(Expression::Const(Constant::Integer(PPL_FALSE)))),
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

        assert_eq!(
            Statement::End,
            parse_statement("End ; Predifined End")
        );

        assert_eq!(
            Statement::Break,
            parse_statement("BREAK")
        );

        assert_eq!(
            Statement::Continue,
            parse_statement("CONTINUE")
        );

        assert_eq!(
            Statement::Return,
            parse_statement("RETURN")
        );

        assert_eq!(
            Statement::Stop,
            parse_statement("STOP")
        );
    }


    #[test]
    fn test_procedure_calls() {
        assert_eq!(
            Statement::ProcedureCall("PROC".to_string(), Vec::new()),
            parse_statement("PROC()")
        );
        assert_eq!(
            Statement::ProcedureCall("PROC".to_string(), vec![Expression::Const(Constant::Integer(PPL_TRUE))]),
            parse_statement("PROC(TRUE)")
        );
        assert_eq!(
            Statement::ProcedureCall("PROC".to_string(), vec![Expression::Const(Constant::Integer(5)), Expression::Const(Constant::Integer(7))]),
            parse_statement("PROC(5, 7)")
        );
    }

    #[test]
    fn test_parse_if() {
        assert_eq!(
            Statement::If(Box::new(Expression::Const(Constant::Boolean(false))), Box::new(Statement::End)),
            parse_statement("IF (FALSE) END")
        );

        let print_hello = parse_statement("PRINT \"Hello Word\"");
        let print_5 = parse_statement("PRINT 5");
        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Boolean(true))),
                vec![print_hello.clone()], None, None),
            parse_statement("IF (TRUE) THEN PRINT \"Hello Word\" ENDIF")
        );
        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Boolean(true))),
                vec![print_hello.clone()],
                Some(vec![ElseIfBlock { 
                    cond: Box::new(Expression::Const(Constant::Boolean(true))),
                    block:vec![print_5]
                }]), 
                None
            ),
            parse_statement("IF (TRUE) THEN PRINT \"Hello Word\" ELSEIF (TRUE) THEN PRINT 5 ENDIF")
        );

        let print_5 = parse_statement("PRINT 5");

        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Boolean(false))),
                Vec::new(),
                None,
                Some(vec![print_5])),
            parse_statement("IF (FALSE) THEN ELSE PRINT 5 ENDIF")
        );
    }  
    

    #[test]
    fn test_parse_while() {
        assert_eq!(
            Statement::While(Box::new(Expression::Const(Constant::Boolean(false))), Box::new(Statement::End)),
            parse_statement("WHILE (FALSE) END")
        );
        assert_eq!(
            Statement::DoWhile(Box::new(Expression::Const(Constant::Boolean(true))), Vec::new()),
            parse_statement("WHILE (TRUE) DO ENDWHILE")
        );
    }

    #[test]
    fn test_for_next() {
        assert_eq!(
            Statement::For(Box::new(Expression::Identifier("i".to_string())), Box::new(Expression::Const(Constant::Integer(1))), Box::new(Expression::Const(Constant::Integer(10))), None, Vec::new()),
            parse_statement("FOR i = 1 TO 10 NEXT")
        );
        assert_eq!(
            Statement::For(
            Box::new(Expression::Identifier("i".to_string())), 
            Box::new(Expression::Const(Constant::Integer(1))), 
            Box::new(Expression::Const(Constant::Integer(10))), 
            Some(Box::new(Expression::Const(Constant::Integer(5)))),
            Vec::new()),
            parse_statement("FOR i = 1 TO 10 STEP 5 NEXT")
        );
        assert_eq!(
            Statement::For(Box::new(Expression::Identifier("i".to_string())), Box::new(Expression::Const(Constant::Integer(1))), Box::new(Expression::Const(Constant::Integer(10))), Some(Box::new(Expression::Minus(Box::new(Expression::Const(Constant::Integer(1)))))), Vec::new()),
            parse_statement("FOR i = 1 TO 10 STEP -1 NEXT")
        );
    }
}
