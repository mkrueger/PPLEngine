use std::path::PathBuf;

use crate::{
    ast::{
        Constant, DimensionSpecifier, FunctionDeclarationStatement, FunctionImplementation,
        ParameterSpecifier, ProcedureDeclarationStatement, ProcedureImplementation, Program,
        Statement, VariableSpecifier, VariableType,
    },
    parser::tokens::LexingError,
};

use self::tokens::{SpannedToken, Token};
use logos::Logos;
use thiserror::Error;

mod expression;
mod statements;
pub mod tokens;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    /// Triggered if there's an issue when tokenizing, and an AST can't be made
    TokenizerError(LexingError),
    ParserError(ParserError),
}

#[derive(Error, Default, Debug, Clone, PartialEq)]
pub enum ParserErrorType {
    #[default]
    #[error("Parser Error")]
    GenericError,

    #[error("Unexpected error (should never happen)")]
    UnexpectedError,

    #[error("Too '{0}' from {1}")]
    InvalidInteger(String, String),

    #[error("Too few arguments for {0} expected {1} got {2}")]
    TooFewArguments(String, usize, i8),

    #[error("Too many arguments for {0} expected {1} got {2}")]
    TooManyArguments(String, usize, i8),

    #[error("Invalid token {0}")]
    InvalidToken(Token),

    #[error("Missing open '(' found: {0}")]
    MissingOpenParens(Token),

    #[error("Missing close ')' found: {0}")]
    MissingCloseParens(Token),

    #[error("Invalid token - label expected '{0}'")]
    LabelExpected(Token),

    #[error("Invalid token - 'END' expected")]
    EndExpected,

    #[error("Expected identifier, got '{0}'")]
    IdentifierExpected(Token),

    #[error("Expected '=', got '{0}'")]
    EqTokenExpected(Token),

    #[error("Expected 'TO', got '{0}'")]
    ToExpected(Token),

    #[error("Expected expression, got '{0}'")]
    ExpressionExpected(Token),

    #[error("Expected statement")]
    StatementExpected,

    #[error("Too many dimensions for variable '{0}' (max 3)")]
    TooManyDimensions(usize),

    #[error("Invalid token '{0}' - 'CASE' expected")]
    CaseExpected(Token),

    #[error("Unexpected identifier '{0}'")]
    UnknownIdentifier(String),

    #[error("Expected number, got '{0}'")]
    NumberExpected(Token),

    #[error("Expected type, got '{0}'")]
    TypeExpected(Token),

    #[error("Invalid declaration '{0}' expected either 'PROCEDURE' or 'FUNCTION'")]
    InvalidDeclaration(Token),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParserError {
    pub error: ParserErrorType,
    pub range: core::ops::Range<usize>,
}

pub struct Tokenizer<'a> {
    pub errors: Vec<Error>,
    pub cur_token: Option<SpannedToken>,
    lex: logos::Lexer<'a, Token>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(text: &'a str) -> Self {
        let lex = crate::parser::tokens::Token::lexer(text);
        Tokenizer {
            errors: Vec::new(),
            cur_token: None,
            lex,
        }
    }

    pub fn get_cur_token(&self) -> Option<Token> {
        self.cur_token.as_ref().map(|token| token.token.clone())
    }

    /// Returns the next token of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn next_token(&mut self) -> Option<SpannedToken> {
        if let Some(token) = self.lex.next() {
            match token {
                Ok(token) => {
                    self.cur_token = Some(SpannedToken::new(token, self.lex.span()));
                }
                Err(error) => {
                    let err = LexingError {
                        error,
                        range: self.lex.span(),
                    };
                    self.errors.push(Error::TokenizerError(err));
                    self.next_token();
                }
            }
        } else {
            self.cur_token = None;
        }
        self.cur_token.clone()
    }

    fn save_token_span(&self) -> std::ops::Range<usize> {
        if let Some(token) = &self.cur_token {
            token.span.clone()
        } else {
            0..0
        }
    }

    fn save_token(&self) -> Token {
        if let Some(token) = &self.cur_token {
            token.token.clone()
        } else {
            Token::Eol
        }
    }

    fn save_spannedtoken(&self) -> SpannedToken {
        if let Some(token) = &self.cur_token {
            token.clone()
        } else {
            SpannedToken::new(Token::Eol, 0..0)
        }
    }
}

impl<'a> Tokenizer<'a> {
    pub fn get_variable_type(&self) -> Option<VariableType> {
        let Some(token) = &self.cur_token else {
            return None;
        };

        if let Token::Identifier(id) = &token.token {
            match id.to_ascii_uppercase().as_str() {
                "INTEGER" | "INT" | "SDWORD" | "LONG" => Some(VariableType::Integer),
                "STRING" | "BIGSTR" => Some(VariableType::String),
                "BOOLEAN" => Some(VariableType::Boolean),
                "DATE" => Some(VariableType::Date),
                "TIME" => Some(VariableType::Time),
                "MONEY" => Some(VariableType::Money),
                "WORD" | "UWORD" => Some(VariableType::Word),
                "SWORD" => Some(VariableType::SWord),
                "BYTE" | "UBYTE" => Some(VariableType::Byte),

                "EDATE" => Some(VariableType::EDate),
                "DDATE" => Some(VariableType::DDate),

                "DWORD" | "UDWORD" | "UNSIGNED" => Some(VariableType::Unsigned),
                "SBYTE" | "SHORT" => Some(VariableType::SByte),
                "REAL" | "DOUBLE" | "FLOAT" | "DREAL" => Some(VariableType::Real),
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
    pub fn parse_var_info(&mut self) -> Option<VariableSpecifier> {
        let Some(Token::Identifier(_)) = self.get_cur_token() else {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::IdentifierExpected(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        };
        let identifier_token = self.save_spannedtoken();
        self.next_token();
        let mut dimensions = Vec::new();
        let mut leftpar_token = None;
        let mut rightpar_token = None;
        if let Some(Token::LPar) = &self.get_cur_token() {
            leftpar_token = Some(self.save_spannedtoken());
            self.next_token();
            let Some(Token::Const(Constant::Integer(_))) = self.get_cur_token() else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::NumberExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            };
            dimensions.push(DimensionSpecifier::new(self.save_spannedtoken()));
            self.next_token();

            if let Some(Token::Comma) = &self.get_cur_token() {
                self.next_token();
                let Some(Token::Const(Constant::Integer(_))) = self.get_cur_token() else {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::NumberExpected(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                };
                dimensions.push(DimensionSpecifier::new(self.save_spannedtoken()));
                self.next_token();

                if let Some(Token::Comma) = &self.get_cur_token() {
                    let Some(Token::Const(Constant::Integer(_))) = self.get_cur_token() else {
                        self.errors
                            .push(crate::parser::Error::ParserError(ParserError {
                                error: ParserErrorType::NumberExpected(self.save_token()),
                                range: self.lex.span(),
                            }));
                        return None;
                    };
                    dimensions.push(DimensionSpecifier::new(self.save_spannedtoken()));
                    self.next_token();
                }
            };
            if dimensions.len() > 3 {
                self.errors.push(Error::ParserError(ParserError {
                    error: ParserErrorType::TooManyDimensions(dimensions.len()),
                    range: self.lex.span(),
                }));
                return None;
            }

            if !matches!(self.get_cur_token(), Some(Token::RPar)) {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::MissingCloseParens(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            }
            rightpar_token = Some(self.save_spannedtoken());
            self.next_token();
        }

        Some(VariableSpecifier::new(
            identifier_token,
            leftpar_token,
            dimensions,
            rightpar_token,
        ))
    }

    /// Returns the parse function declaration of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn parse_declaration(&mut self) -> Option<Statement> {
        if Some(Token::Declare) == self.get_cur_token() {
            let declare_token = self.save_spannedtoken();
            self.next_token();

            let is_function = if Some(Token::Procedure) == self.get_cur_token() {
                false
            } else if Some(Token::Function) == self.get_cur_token() {
                true
            } else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::InvalidDeclaration(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            };
            let func_or_proc_token = self.save_spannedtoken();
            self.next_token();

            let Some(Token::Identifier(_name)) = self.get_cur_token() else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::IdentifierExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            };
            let identifier_token = self.save_spannedtoken();
            self.next_token();

            if self.get_cur_token() != Some(Token::LPar) {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::MissingOpenParens(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            }

            let leftpar_token = self.save_spannedtoken();
            self.next_token();

            let mut parameters = Vec::new();

            while self.get_cur_token() != Some(Token::RPar) {
                if self.get_cur_token().is_none() {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::MissingCloseParens(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                }

                let mut var_token = None;
                if let Some(Token::Identifier(id)) = self.get_cur_token() {
                    if id.to_ascii_uppercase().as_str() == "VAR" {
                        var_token = Some(self.save_spannedtoken());
                        self.next_token();
                    }
                }

                if let Some(var_type) = self.get_variable_type() {
                    let type_token = self.save_spannedtoken();
                    self.next_token();
                    let Some(info) = self.parse_var_info() else {
                        return None;
                    };
                    parameters.push(ParameterSpecifier::new(
                        var_token, type_token, var_type, info,
                    ));
                } else {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::TypeExpected(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                }

                if self.get_cur_token() == Some(Token::Comma) {
                    self.next_token();
                }
            }
            let rightpar_token = self.save_spannedtoken();
            self.next_token();

            if !is_function {
                return Some(Statement::ProcedureDeclaration(
                    ProcedureDeclarationStatement::new(
                        declare_token,
                        func_or_proc_token,
                        identifier_token,
                        leftpar_token,
                        parameters,
                        rightpar_token,
                    ),
                ));
            }

            let Some(return_type) = self.get_variable_type() else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::TypeExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            };
            let return_type_token = self.save_spannedtoken();
            self.next_token();

            return Some(Statement::FunctionDeclaration(
                FunctionDeclarationStatement::new(
                    declare_token,
                    func_or_proc_token,
                    identifier_token,
                    leftpar_token,
                    parameters,
                    rightpar_token,
                    return_type_token,
                    return_type,
                ),
            ));
        }
        None
    }

    /// Returns the parse procedure of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn parse_procedure(&mut self) -> Option<ProcedureImplementation> {
        if Some(Token::Procedure) == self.get_cur_token() {
            let procedure_token = self.save_spannedtoken();
            self.next_token();

            let Some(Token::Identifier(_)) = self.get_cur_token() else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::IdentifierExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            };
            let identifier_token = self.save_spannedtoken();
            self.next_token();
            if self.get_cur_token() != Some(Token::LPar) {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::MissingOpenParens(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            }

            let leftpar_token = self.save_spannedtoken();
            self.next_token();

            let mut parameters = Vec::new();

            while self.get_cur_token() != Some(Token::RPar) {
                if self.get_cur_token().is_none() {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::MissingCloseParens(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                }
                let mut var_token = None;
                if let Some(Token::Identifier(id)) = self.get_cur_token() {
                    if id.to_ascii_uppercase().as_str() == "VAR" {
                        var_token = Some(self.save_spannedtoken());
                        self.next_token();
                    }
                }

                if let Some(var_type) = self.get_variable_type() {
                    let type_token = self.save_spannedtoken();
                    self.next_token();

                    let Some(info) = self.parse_var_info() else {
                        return None;
                    };
                    parameters.push(ParameterSpecifier::new(
                        var_token, type_token, var_type, info,
                    ));
                } else {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::TypeExpected(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                }

                if self.get_cur_token() == Some(Token::Comma) {
                    self.next_token();
                }
            }
            let rightpar_token = self.save_spannedtoken();
            self.next_token();

            self.skip_eol();

            let mut statements = Vec::new();

            while self.get_cur_token() != Some(Token::EndProc) {
                if self.get_cur_token().is_none() {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::EndExpected,
                            range: self.lex.span(),
                        }));
                    return None;
                }
                statements.push(self.parse_statement());
                self.skip_eol();
            }
            let endproc_token = self.save_spannedtoken();

            self.next_token();

            return Some(ProcedureImplementation::new(
                -1,
                procedure_token,
                identifier_token,
                leftpar_token,
                parameters,
                rightpar_token,
                statements.into_iter().flatten().collect(),
                endproc_token,
            ));
        }
        None
    }

    /// Returns the parse function of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn parse_function(&mut self) -> Option<FunctionImplementation> {
        if Some(Token::Function) == self.get_cur_token() {
            let function_token = self.save_spannedtoken();
            self.next_token();

            let Some(Token::Identifier(_)) = self.get_cur_token() else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::IdentifierExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            };
            let identifier_token = self.save_spannedtoken();
            self.next_token();
            if self.get_cur_token() != Some(Token::LPar) {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::MissingOpenParens(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            }

            let leftpar_token = self.save_spannedtoken();
            self.next_token();

            let mut parameters = Vec::new();

            while self.get_cur_token() != Some(Token::RPar) {
                if self.get_cur_token().is_none() {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::MissingCloseParens(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                }
                let mut var_token = None;
                if let Some(Token::Identifier(id)) = self.get_cur_token() {
                    if id.to_ascii_uppercase().as_str() == "VAR" {
                        var_token = Some(self.save_spannedtoken());
                        self.next_token();
                    }
                }

                if let Some(var_type) = self.get_variable_type() {
                    let type_token = self.save_spannedtoken();
                    self.next_token();

                    let Some(info) = self.parse_var_info() else {
                        return None;
                    };
                    parameters.push(ParameterSpecifier::new(
                        var_token, type_token, var_type, info,
                    ));
                } else {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::TypeExpected(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                }

                if self.get_cur_token() == Some(Token::Comma) {
                    self.next_token();
                }
            }
            let rightpar_token = self.save_spannedtoken();
            self.next_token();

            let Some(return_type) = self.get_variable_type() else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::TypeExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            };
            let return_type_token = self.save_spannedtoken();
            self.next_token();
            self.skip_eol();

            let mut statements = Vec::new();

            while self.get_cur_token() != Some(Token::EndFunc) {
                if self.get_cur_token().is_none() {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::EndExpected,
                            range: self.lex.span(),
                        }));
                    return None;
                }
                statements.push(self.parse_statement());
                self.skip_eol();
            }
            let endproc_token = self.save_spannedtoken();

            self.next_token();

            return Some(FunctionImplementation::new(
                -1,
                function_token,
                identifier_token,
                leftpar_token,
                parameters,
                rightpar_token,
                return_type_token,
                return_type,
                statements.into_iter().flatten().collect(),
                endproc_token,
            ));
        }
        None
    }
}

pub fn parse_program(input: &str) -> Program {
    let mut function_implementations: Vec<FunctionImplementation> = Vec::new();
    let mut procedure_implementations = Vec::new();
    let mut statements = Vec::new();

    let mut tokenizer = Tokenizer::new(input);
    tokenizer.next_token();
    tokenizer.skip_eol();

    while tokenizer.cur_token.is_some() {
        if let Some(func) = tokenizer.parse_function() {
            function_implementations.push(func);
        } else if let Some(func) = tokenizer.parse_procedure() {
            procedure_implementations.push(func);
        } else {
            let tok = tokenizer.cur_token.clone();
            let stmt = tokenizer.parse_statement();
            if stmt.is_some() {
                statements.push(stmt);
            } else if let Some(t) = tok {
                if !matches!(t.token, Token::Eol | Token::Comment(_)) {
                    tokenizer.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::InvalidToken(t.token),
                        range: t.span,
                    }));
                }
            }
        }
        tokenizer.next_token();
        tokenizer.skip_eol();
    }

    Program {
        function_implementations,
        procedure_implementations,
        statements: statements.into_iter().flatten().collect(),
        file_name: PathBuf::from("/test/test.ppe"),
        errors: tokenizer.errors,
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
        /*
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
        );*/
    }
    /*
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
    }*/
}
