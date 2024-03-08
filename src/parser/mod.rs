use std::path::PathBuf;

use crate::ast::{Block, Declaration, FunctionImplementation, Program, VarInfo, VariableType};

use self::tokens::Token;
use chumsky::prelude::*;
use logos::Logos;

mod expression;
mod statements;
pub mod tokens;

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);

pub struct Tokenizer<'a> {
    pub cur_token: Option<Token>,
    lex: logos::Lexer<'a, Token>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(text: &'a str) -> Self {
        let lex = crate::parser::tokens::Token::lexer(text);
        Tokenizer {
            cur_token: None,
            lex,
        }
    }

    /// Returns the next token of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn next_token(&mut self) -> Option<Token> {
        if let Some(token) = self.lex.next() {
            self.cur_token = Some(token.unwrap());
        } else {
            self.cur_token = None;
        }
        self.cur_token.clone()
    }
}

impl<'a> Tokenizer<'a> {
    pub fn get_variable_type(&self) -> Option<VariableType> {
        if let Some(Token::Identifier(id)) = &self.cur_token {
            match id.as_str() {
                "INTEGER" => Some(VariableType::Integer),
                "STRING" | "BIGSTR" => Some(VariableType::String),
                "BOOLEAN" => Some(VariableType::Boolean),
                "DATE" => Some(VariableType::Date),
                "TIME" => Some(VariableType::Time),
                "MONEY" => Some(VariableType::Money),
                "WORD" => Some(VariableType::Word),
                "SWORD" => Some(VariableType::SWord),
                "BYTE" => Some(VariableType::Byte),
                "UNSIGNED" => Some(VariableType::Unsigned),
                "SBYTE" => Some(VariableType::SByte),
                "REAL" | "DREAL" => Some(VariableType::Real),
                _ => None,
            }
        } else {
            None
        }
    }

    /// Returns the parse var info of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn parse_var_info(&mut self) -> VarInfo {
        let var_name;
        if let Some(Token::Identifier(id)) = self.cur_token.clone() {
            self.next_token();
            var_name = id.clone();
        } else {
            panic!("expected identifier, got: {:?}", self.cur_token);
        }

        if let Some(Token::LPar) = &self.cur_token {
            self.next_token();
            let vec = self.parse_expression();

            let var = if let Some(Token::Comma) = &self.cur_token {
                self.next_token();
                let mat = self.parse_expression();

                if let Some(Token::Comma) = &self.cur_token {
                    self.next_token();
                    let cube = self.parse_expression();
                    VarInfo::Var3(var_name, vec, mat, cube)
                } else {
                    VarInfo::Var2(var_name, vec, mat)
                }
            } else {
                VarInfo::Var1(var_name, vec)
            };

            if let Some(Token::RPar) = &self.cur_token {
                self.next_token();
                return var;
            }
            panic!("end bracket expected");
        }

        VarInfo::Var0(var_name)
    }

    /// Returns the parse function declaration of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn parse_function_declaration(&mut self) -> Option<Declaration> {
        if Some(Token::Identifier("DECLARE".to_string())) == self.cur_token {
            self.next_token();

            let is_function = if Some(Token::Identifier("PROCEDURE".to_string())) == self.cur_token
            {
                false
            } else if Some(Token::Identifier("FUNCTION".to_string())) == self.cur_token {
                true
            } else {
                panic!("FUNCTION or PROCEDURE expected. got {:?}", self.cur_token);
            };
            self.next_token();

            let name = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                self.next_token();
                id
            } else {
                panic!("IDENTIFIER expected. got {:?}", self.cur_token);
            };

            assert!(
                !(self.cur_token != Some(Token::LPar)),
                "'(' expected. got {:?}",
                self.cur_token
            );
            self.next_token();

            let mut vars = Vec::new();

            while self.cur_token != Some(Token::RPar) {
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();

                    let info = self.parse_var_info();
                    vars.push(Declaration::Variable(var_type, vec![info]));
                } else {
                    panic!("variable type expeted got: {:?}", self.cur_token);
                }

                if self.cur_token == Some(Token::Comma) {
                    self.next_token();
                }
            }

            assert!(
                !(self.cur_token != Some(Token::RPar)),
                "')' expected. got {:?}",
                self.cur_token
            );
            self.next_token();
            if !is_function {
                return Some(Declaration::Procedure(name, vars));
            }

            let func_t = if let Some(var_type) = self.get_variable_type() {
                self.next_token();
                var_type
            } else {
                panic!("variable type expeted got: {:?}", self.cur_token);
            };

            return Some(Declaration::Function(name, vars, func_t));
        }
        None
    }

    /// Returns the parse procedure of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn parse_procedure(&mut self) -> Option<FunctionImplementation> {
        if Some(Token::Identifier("PROCEDURE".to_string())) == self.cur_token {
            self.next_token();

            let name = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                self.next_token();
                id
            } else {
                panic!("IDENTIFIER expected. got {:?}", self.cur_token);
            };

            assert!(
                !(self.cur_token != Some(Token::LPar)),
                "'(' expected. got {:?}",
                self.cur_token
            );
            self.next_token();

            let mut vars = Vec::new();

            while self.cur_token != Some(Token::RPar) {
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();

                    let info = self.parse_var_info();
                    vars.push(Declaration::Variable(var_type, vec![info]));
                } else {
                    panic!("variable type expeted got: {:?}", self.cur_token);
                }

                if self.cur_token == Some(Token::Comma) {
                    self.next_token();
                }
            }

            assert!(
                !(self.cur_token != Some(Token::RPar)),
                "')' expected. got {:?}",
                self.cur_token
            );
            self.next_token();
            self.skip_eol();

            let mut variable_declarations = Vec::new();
            let mut statements = Vec::new();

            while self.cur_token != Some(Token::Identifier("ENDPROC".to_string())) {
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();

                    let mut vars = Vec::new();

                    vars.push(self.parse_var_info());
                    while self.cur_token == Some(Token::Comma) {
                        self.next_token();
                        vars.push(self.parse_var_info());
                    }
                    variable_declarations.push(Declaration::Variable(var_type, vars));
                } else {
                    statements.push(self.parse_statement());
                }
                self.skip_eol();
            }
            self.next_token();

            return Some(FunctionImplementation {
                id: -1,
                declaration: Declaration::Procedure(name, vars),
                variable_declarations,
                block: Block { statements },
            });
        }
        None
    }

    /// Returns the parse function of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn parse_function(&mut self) -> Option<FunctionImplementation> {
        if Some(Token::Identifier("FUNCTION".to_string())) == self.cur_token {
            self.next_token();

            let name = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                self.next_token();
                id
            } else {
                panic!("IDENTIFIER expected. got {:?}", self.cur_token);
            };

            assert!(
                !(self.cur_token != Some(Token::LPar)),
                "'(' expected. got {:?}",
                self.cur_token
            );
            self.next_token();

            let mut vars = Vec::new();

            while self.cur_token != Some(Token::RPar) {
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();

                    let info = self.parse_var_info();
                    vars.push(Declaration::Variable(var_type, vec![info]));
                } else {
                    panic!("variable type expeted got: {:?}", self.cur_token);
                }

                if self.cur_token == Some(Token::Comma) {
                    self.next_token();
                }
            }

            assert!(
                !(self.cur_token != Some(Token::RPar)),
                "')' expected. got {:?}",
                self.cur_token
            );
            self.next_token();

            let func_t = if let Some(var_type) = self.get_variable_type() {
                self.next_token();
                var_type
            } else {
                panic!("variable type expeted got: {:?}", self.cur_token);
            };
            self.skip_eol();

            let mut variable_declarations = Vec::new();
            let mut statements = Vec::new();

            while self.cur_token != Some(Token::Identifier("ENDFUNC".to_string())) {
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();

                    let mut vars = Vec::new();

                    vars.push(self.parse_var_info());
                    while self.cur_token == Some(Token::Comma) {
                        self.next_token();
                        vars.push(self.parse_var_info());
                    }
                    variable_declarations.push(Declaration::Variable(var_type, vars));
                } else {
                    statements.push(self.parse_statement());
                }
                self.skip_eol();
            }
            self.next_token();

            return Some(FunctionImplementation {
                id: -1,
                declaration: Declaration::Function(name, vars, func_t),
                variable_declarations,
                block: Block { statements },
            });
        }
        None
    }
}

pub fn parse_program(input: &str) -> Program {
    let mut declarations = Vec::new();
    let mut function_implementations = Vec::new();
    let mut procedure_implementations = Vec::new();
    let mut statements = Vec::new();

    let mut tokenizer = Tokenizer::new(input);
    tokenizer.next_token();
    tokenizer.skip_eol();

    while tokenizer.cur_token.is_some() {
        if let Some(var_type) = tokenizer.get_variable_type() {
            tokenizer.next_token();

            let mut vars = Vec::new();

            vars.push(tokenizer.parse_var_info());
            while tokenizer.cur_token == Some(Token::Comma) {
                tokenizer.next_token();
                vars.push(tokenizer.parse_var_info());
            }
            declarations.push(Declaration::Variable(var_type, vars));
        } else if let Some(decl) = tokenizer.parse_function_declaration() {
            declarations.push(decl);
        } else if let Some(func) = tokenizer.parse_function() {
            function_implementations.push(func);
        } else if let Some(func) = tokenizer.parse_procedure() {
            procedure_implementations.push(func);
        } else {
            statements.push(tokenizer.parse_statement());
        }

        tokenizer.skip_eol();
    }

    Program {
        declarations,
        function_implementations,
        procedure_implementations,
        main_block: Block { statements },
        file_name: PathBuf::from("/test/test.ppe"),
    }
}

// type ParserInput<'tokens> = chumsky::input::SpannedInput<Token, Span, &'tokens [(Token, Span)]>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_procedure() {
        let prg = parse_program("Procedure Proc() PRINT 5 EndProc");
        assert_eq!(1, prg.procedure_implementations.len());
    }

    #[test]
    fn test_function() {
        let prg = parse_program("Function Func() BOOLEAN PRINT 5 EndFunc");
        assert_eq!(1, prg.function_implementations.len());
    }

    #[test]
    fn test_var_declarations() {
        let prg = parse_program("BOOLEAN VAR001");
        assert_eq!(
            Declaration::create_variable(VariableType::Boolean, "VAR001".to_string()),
            prg.declarations[0]
        );
        let prg = parse_program("INTEGER VAR001");
        assert_eq!(
            Declaration::create_variable(VariableType::Integer, "VAR001".to_string()),
            prg.declarations[0]
        );
        let prg = parse_program("DATE VAR001");
        assert_eq!(
            Declaration::create_variable(VariableType::Date, "VAR001".to_string()),
            prg.declarations[0]
        );
        let prg = parse_program("STRING VAR001");
        assert_eq!(
            Declaration::create_variable(VariableType::String, "VAR001".to_string()),
            prg.declarations[0]
        );
        let prg = parse_program("MONEY VAR001");
        assert_eq!(
            Declaration::create_variable(VariableType::Money, "VAR001".to_string()),
            prg.declarations[0]
        );
        let prg = parse_program("BYTE VAR001");
        assert_eq!(
            Declaration::create_variable(VariableType::Byte, "VAR001".to_string()),
            prg.declarations[0]
        );
        let prg = parse_program("SBYTE VAR001");
        assert_eq!(
            Declaration::create_variable(VariableType::SByte, "VAR001".to_string()),
            prg.declarations[0]
        );
        let prg = parse_program("WORD VAR001");
        assert_eq!(
            Declaration::create_variable(VariableType::Word, "VAR001".to_string()),
            prg.declarations[0]
        );
        let prg = parse_program("SWORD VAR001");
        assert_eq!(
            Declaration::create_variable(VariableType::SWord, "VAR001".to_string()),
            prg.declarations[0]
        );
        let prg = parse_program("INTEGER VAR001(5)");
        assert_eq!(
            Declaration::create_variable1(VariableType::Integer, "VAR001".to_string(), 5),
            prg.declarations[0]
        );
        let prg = parse_program("INTEGER VAR001(5, 10)");
        assert_eq!(
            Declaration::create_variable2(VariableType::Integer, "VAR001".to_string(), 5, 10),
            prg.declarations[0]
        );
        let prg = parse_program("INTEGER VAR001(5, 10, 42)");
        assert_eq!(
            Declaration::create_variable3(VariableType::Integer, "VAR001".to_string(), 5, 10, 42),
            prg.declarations[0]
        );
    }

    #[test]
    fn test_func_declarations() {
        let prg = parse_program("DECLARE PROCEDURE PROC001()");
        assert_eq!(
            Declaration::Procedure("PROC001".to_string(), vec![]),
            prg.declarations[0]
        );

        let prg = parse_program("DECLARE FUNCTION FUNC001(INTEGER LOC001) INTEGER");
        assert_eq!(
            Declaration::Function(
                "FUNC001".to_string(),
                vec![Declaration::create_variable(
                    VariableType::Integer,
                    "LOC001".to_string()
                )],
                VariableType::Integer
            ),
            prg.declarations[0]
        );
    }
}
