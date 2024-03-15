use std::{collections::HashMap, path::PathBuf};

use crate::{
    ast::{
        AstNode, CommentAstNode, Constant, DimensionSpecifier, FunctionDeclarationAstNode,
        FunctionImplementation, ParameterSpecifier, ProcedureDeclarationAstNode,
        ProcedureImplementation, Program, Statement, VariableSpecifier, VariableType,
    },
    parser::lexer::LexingError,
};

use self::lexer::{Lexer, SpannedToken, Token};
use thiserror::Error;
use unicase::Ascii;

mod expression;
pub mod lexer;
#[cfg(test)]
mod lexer_tests;
mod statements;

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

    #[error("VAR parameters are not allowed in functions")]
    VarNotAllowedInFunctions,

    #[error("Only one BEGIN block is allowed with $USEFUNCS")]
    OnlyOneBeginBlockAllowed,

    #[error("No statements allowed outside of BEGIN...END block")]
    NoStatementsAllowedOutsideBlock,

    #[error("$USEFUNCS used after statements has no effect.")]
    UsefuncAfterStatement,

    #[error("No statements allowed after functions (use $USEFUNCS)")]
    NoStatementsAfterFunctions,
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParserWarningType {
    #[error("$USEFUNCS is not valid there, ignoring.")]
    UsefuncsIgnored,
    #[error("$USEFUNCS already set, ignoring.")]
    UsefuncsAlreadySet,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParserError {
    pub error: ParserErrorType,
    pub range: core::ops::Range<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParserWarning {
    pub error: ParserWarningType,
    pub range: core::ops::Range<usize>,
}

pub struct Parser {
    pub errors: Vec<Error>,
    pub warnings: Vec<ParserWarning>,
    cur_token: Option<SpannedToken>,
    lex: Lexer,
}

impl Parser {
    pub fn new(file: PathBuf, text: &str) -> Self {
        let lex = Lexer::new(file, text);
        Parser {
            errors: Vec::new(),
            warnings: Vec::new(),
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
        if let Some(token) = self.lex.next_token() {
            match token {
                Ok(token) => {
                    self.cur_token = Some(SpannedToken::new(token, self.lex.span()));
                }
                Err(err) => {
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

lazy_static::lazy_static! {
    static ref TYPE_HASHES: HashMap<unicase::Ascii<String>, VariableType> = {
        let mut m = HashMap::new();
        m.insert(unicase::Ascii::new("INTEGER".to_string()), VariableType::Integer);
        m.insert(unicase::Ascii::new("SDWORD".to_string()), VariableType::Integer);
        m.insert(unicase::Ascii::new("LONG".to_string()), VariableType::Integer);

        m.insert(unicase::Ascii::new("STRING".to_string()), VariableType::String);
        m.insert(unicase::Ascii::new("BIGSTR".to_string()), VariableType::BigStr);

        m.insert(unicase::Ascii::new("BOOLEAN".to_string()), VariableType::Boolean);

        m.insert(unicase::Ascii::new("DATE".to_string()), VariableType::Date);

        m.insert(unicase::Ascii::new("DDATE".to_string()), VariableType::DDate);

        m.insert(unicase::Ascii::new("EDATE".to_string()), VariableType::EDate);

        m.insert(unicase::Ascii::new("TIME".to_string()), VariableType::Time);

        m.insert(unicase::Ascii::new("MONEY".to_string()), VariableType::Money);

        m.insert(unicase::Ascii::new("WORD".to_string()), VariableType::Word);
        m.insert(unicase::Ascii::new("UWORD".to_string()), VariableType::Word);

        m.insert(unicase::Ascii::new("SWORD".to_string()), VariableType::SWord);
        m.insert(unicase::Ascii::new("INT".to_string()), VariableType::SWord);

        m.insert(unicase::Ascii::new("BYTE".to_string()), VariableType::Byte);
        m.insert(unicase::Ascii::new("UBYTE".to_string()), VariableType::Byte);

        m.insert(unicase::Ascii::new("UNSIGNED".to_string()), VariableType::Unsigned);
        m.insert(unicase::Ascii::new("DWORD".to_string()), VariableType::Unsigned);
        m.insert(unicase::Ascii::new("UDWORD".to_string()), VariableType::Unsigned);

        m.insert(unicase::Ascii::new("SBYTE".to_string()), VariableType::SByte);
        m.insert(unicase::Ascii::new("SHORT".to_string()), VariableType::SByte);

        m.insert(unicase::Ascii::new("REAL".to_string()), VariableType::Float);
        m.insert(unicase::Ascii::new("FLOAT".to_string()), VariableType::Float);

        m.insert(unicase::Ascii::new("DOUBLE".to_string()), VariableType::Double);
        m.insert(unicase::Ascii::new("DREAL".to_string()), VariableType::Double);
        m


    };

}

impl Parser {
    pub fn get_variable_type(&self) -> Option<VariableType> {
        let Some(token) = &self.cur_token else {
            return None;
        };

        if let Token::Identifier(id) = &token.token {
            if let Some(vt) = TYPE_HASHES.get(id) {
                return Some(*vt);
            }
            None
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
    pub fn parse_declaration(&mut self) -> Option<AstNode> {
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
                if id == Ascii::new("VAR".to_string()) {
                    if is_function {
                        self.errors
                            .push(crate::parser::Error::ParserError(ParserError {
                                error: ParserErrorType::VarNotAllowedInFunctions,
                                range: self.lex.span(),
                            }));
                    } else {
                        var_token = Some(self.save_spannedtoken());
                    }
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
            return Some(AstNode::ProcedureDeclaration(
                ProcedureDeclarationAstNode::new(
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

        return Some(AstNode::FunctionDeclaration(
            FunctionDeclarationAstNode::new(
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
                    if id.eq_ignore_ascii_case("VAR") {
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
                if let Some(Token::Identifier(id)) = self.get_cur_token() {
                    if id == Ascii::new("VAR".to_string()) {
                        self.errors
                            .push(crate::parser::Error::ParserError(ParserError {
                                error: ParserErrorType::VarNotAllowedInFunctions,
                                range: self.lex.span(),
                            }));
                        self.next_token();
                    }
                }

                if let Some(var_type) = self.get_variable_type() {
                    let type_token = self.save_spannedtoken();
                    self.next_token();

                    let Some(info) = self.parse_var_info() else {
                        return None;
                    };
                    parameters.push(ParameterSpecifier::new(None, type_token, var_type, info));
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

pub fn parse_program(file: PathBuf, input: &str) -> Program {
    let mut nodes = Vec::new();
    let mut parser = Parser::new(file, input);
    parser.next_token();
    parser.skip_eol();
    let mut use_funcs = false;
    let mut parsed_begin = false;
    let mut got_statement = false;
    let mut got_funcs = false;

    while let Some(cur_token) = &parser.cur_token {
        match cur_token.token {
            Token::Function => {
                if let Some(func) = parser.parse_function() {
                    got_funcs = true;
                    nodes.push(AstNode::Function(func));
                }
            }
            Token::Procedure => {
                if let Some(func) = parser.parse_procedure() {
                    got_funcs = true;
                    nodes.push(AstNode::Procedure(func));
                }
            }
            Token::Declare => {
                if let Some(decl) = parser.parse_declaration() {
                    nodes.push(decl);
                }
            }
            Token::Comment(_, _) => {
                let cmt = parser.save_spannedtoken();
                nodes.push(AstNode::Comment(CommentAstNode::new(cmt)));
            }
            Token::UseFuncs(_, _) => {
                if use_funcs {
                    parser.warnings.push(ParserWarning {
                        error: ParserWarningType::UsefuncsAlreadySet,
                        range: parser.lex.span(),
                    });
                }
                if got_statement {
                    parser.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::UsefuncAfterStatement,
                        range: parser.lex.span(),
                    }));
                    parser.next_token();
                    continue;
                }
                use_funcs = true;
                let cmt = parser.save_spannedtoken();
                nodes.push(AstNode::Comment(CommentAstNode::new(cmt)));
            }
            Token::Eol => {}
            Token::Begin => {
                if parsed_begin {
                    parser.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::OnlyOneBeginBlockAllowed,
                        range: parser.lex.span(),
                    }));
                    break;
                }
                if got_funcs && !use_funcs {
                    parser.errors.push(Error::ParserError(ParserError {
                        error: ParserErrorType::NoStatementsAfterFunctions,
                        range: parser.lex.span(),
                    }));
                    break;
                }
                parsed_begin = true;
            }
            _ => {
                let tok = parser.cur_token.clone();
                let stmt = parser.parse_statement();
                if let Some(stmt) = stmt {
                    if use_funcs && !parsed_begin {
                        parser.errors.push(Error::ParserError(ParserError {
                            error: ParserErrorType::NoStatementsAllowedOutsideBlock,
                            range: parser.lex.span(),
                        }));
                        break;
                    }
                    if got_funcs && !use_funcs {
                        parser.errors.push(Error::ParserError(ParserError {
                            error: ParserErrorType::NoStatementsAfterFunctions,
                            range: parser.lex.span(),
                        }));
                        break;
                    }
                    got_statement = true;
                    nodes.push(AstNode::Statement(stmt));
                } else if let Some(t) = tok {
                    if !matches!(t.token, Token::Eol | Token::Comment(_, _)) {
                        parser.errors.push(Error::ParserError(ParserError {
                            error: ParserErrorType::InvalidToken(t.token),
                            range: t.span,
                        }));
                    }
                }
            }
        }

        parser.next_token();
    }

    Program {
        nodes,
        file_name: PathBuf::from("/test/test.ppe"),
        errors: parser.errors,
        warnings: parser.warnings,
    }
}

// type ParserInput<'tokens> = chumsky::input::SpannedInput<Token, Span, &'tokens [(Token, Span)]>;

#[cfg(test)]
mod tests {
    /*  use super::*;

    #[test]
    fn test_procedure() {
        let prg = parse_program("Procedure Proc() PRINT 5 EndProc");
        assert_eq!(1, prg.procedure_implementations.len());
    }

    #[test]
    fn test_function() {
        let prg = parse_program("Function Func() BOOLEAN PRINT 5 EndFunc");
        assert_eq!(1, prg.function_implementations.len());
    }*/

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
