use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use crate::{
    ast::{
        AstNode, CommentAstNode, Constant, DimensionSpecifier, FunctionDeclarationAstNode, FunctionImplementation, ParameterSpecifier, ProcedureDeclarationAstNode, ProcedureImplementation, Program, Statement, VariableSpecifier
    },
    executable::VariableType,
    tables::CP437_TO_UNICODE,
};

use self::lexer::{Lexer, SpannedToken, Token};
use thiserror::Error;
use unicase::Ascii;

mod expression;
pub mod lexer;
mod statements;

#[cfg(test)]
mod expr_tests;
#[cfg(test)]
mod lexer_tests;
#[cfg(test)]
mod statement_tests;

// #[cfg(test)]
// mod parser_tests;

#[derive(Error, Default, Debug, Clone, PartialEq)]
pub enum ParserErrorType {
    #[default]
    #[error("Parser Error")]
    GenericError,

    #[error("Unexpected error (should never happen)")]
    UnexpectedError,

    #[error("Too '{0}' from {1}")]
    InvalidInteger(String, String),

    #[error("Not enough arguments passed ({0}:{1}:{2})")]
    TooFewArguments(String, usize, i8),

    #[error("Too many arguments passed ({0}:{1}:{2})")]
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

    #[error("EOL expected, got '{0}'")]
    EolExpected(Token),

    #[error("Expected comma, got '{0}'")]
    CommaExpected(Token),

    #[error("Expected 'THEN', got '{0}'")]
    ThenExpected(Token),

    #[error("Expected 'CASE' keyword after 'SELECT', got '{0}'")]
    CaseExpectedAfterSelect(Token),

    #[error("IF/WHILE requires a conditional expression to evaluate")]
    IfWhileConditionNotFound,

    #[error("Block start (IF/WHILE/FOR/SELECT) must come before block end statement")]
    BlockEndBeforeBlockStart,
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum ParserWarningType {
    #[error("$USEFUNCS is not valid there, ignoring.")]
    UsefuncsIgnored,
    #[error("$USEFUNCS already set, ignoring.")]
    UsefuncsAlreadySet,

    #[error("Next Identifier '{1}' should match next variable '{0}'")]
    NextIdentifierInvalid(unicase::Ascii<String>, Token),

    // old pplc parser allows that
    #[error("Procedure closed with 'ENDFUNC'")]
    ProcedureClosedWithEndFunc,

    // old pplc parser allows that
    #[error("Function closed with 'ENDPROC'")]
    FunctionClosedWithEndProc,
}

pub struct Parser {
    pub errors: Arc<Mutex<ErrorRepoter>>,

    pub require_user_variables: bool,

    cur_token: Option<SpannedToken>,
    lookahead_token: Option<SpannedToken>,

    lex: Lexer,
}
lazy_static::lazy_static! {
    static ref PROC_TOKEN: unicase::Ascii<String> = unicase::Ascii::new("PROC".to_string());
    static ref FUNC_TOKEN: unicase::Ascii<String> = unicase::Ascii::new("FUNC".to_string());
}

impl Parser {
    pub fn new(file: PathBuf, text: &str, encoding: Encoding) -> Self {
        let errors = Arc::new(Mutex::new(ErrorRepoter::default()));

        let lex = Lexer::new(file, text, encoding, errors.clone());
        Parser {
            errors,
            cur_token: None,
            lookahead_token: None,
            lex,
            require_user_variables: false,
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
        if let Some(token) = self.lookahead_token.take() {
            self.cur_token = Some(token);
            return self.cur_token.clone();
        }

        if let Some(token) = self.lex.next_token() {
            let is_else = token == Token::Else;
            let is_end = token == Token::End;
            let is_case = token == Token::Case;
            self.cur_token = Some(SpannedToken::new(token, self.lex.span()));

            if is_else {
                let start = self.lex.span().start;
                if let Some(lookahed) = self.lex.next_token() {
                    if lookahed == Token::If {
                        self.cur_token =
                            Some(SpannedToken::new(Token::ElseIf, start..self.lex.span().end));
                    } else {
                        self.lookahead_token =
                            Some(SpannedToken::new(lookahed, start..self.lex.span().end));
                    }
                }
            } else if is_case {
                let start = self.lex.span().start;
                if let Some(lookahed) = self.lex.next_token() {
                    if lookahed == Token::Else {
                        self.cur_token = Some(SpannedToken::new(
                            Token::Default,
                            start..self.lex.span().end,
                        ));
                    } else {
                        self.lookahead_token =
                            Some(SpannedToken::new(lookahed, start..self.lex.span().end));
                    }
                }
            } else if is_end {
                let start = self.lex.span().start;
                if let Some(lookahed) = self.lex.next_token() {
                    match lookahed {
                        Token::If => {
                            self.cur_token =
                                Some(SpannedToken::new(Token::EndIf, start..self.lex.span().end));
                        }
                        Token::While => {
                            self.cur_token = Some(SpannedToken::new(
                                Token::EndWhile,
                                start..self.lex.span().end,
                            ));
                        }
                        Token::Select => {
                            self.cur_token = Some(SpannedToken::new(
                                Token::EndSelect,
                                start..self.lex.span().end,
                            ));
                        }
                        Token::For => {
                            self.cur_token =
                                Some(SpannedToken::new(Token::Next, start..self.lex.span().end));
                        }
                        _ => {
                            let set_lookahad = if let Token::Identifier(id) = &lookahed {
                                if *id == *PROC_TOKEN {
                                    self.cur_token = Some(SpannedToken::new(
                                        Token::EndProc,
                                        start..self.lex.span().end,
                                    ));
                                    false
                                } else if *id == *FUNC_TOKEN {
                                    self.cur_token = Some(SpannedToken::new(
                                        Token::EndFunc,
                                        start..self.lex.span().end,
                                    ));
                                    false
                                } else {
                                    true
                                }
                            } else {
                                true
                            };

                            if set_lookahad {
                                self.lookahead_token =
                                    Some(SpannedToken::new(lookahed, start..self.lex.span().end));
                            }
                        }
                    }
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

    fn report_error(&mut self, span: std::ops::Range<usize>, save_token: ParserErrorType) {
        //println!("{}", std::backtrace::Backtrace::force_capture());
        self.errors.lock().unwrap().report_error(span, save_token);
        while self.get_cur_token().is_some()
            && self.get_cur_token() != Some(Token::Eol)
            && !matches!(self.get_cur_token(), Some(Token::Comment(_, _)))
        {
            self.next_token();
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
            self.report_error(
                self.lex.span(),
                ParserErrorType::IdentifierExpected(self.save_token()),
            );
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
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::NumberExpected(self.save_token()),
                );
                return None;
            };
            dimensions.push(DimensionSpecifier::new(self.save_spannedtoken()));
            self.next_token();

            if let Some(Token::Comma) = &self.get_cur_token() {
                self.next_token();
                let Some(Token::Const(Constant::Integer(_))) = self.get_cur_token() else {
                    self.report_error(
                        self.lex.span(),
                        ParserErrorType::NumberExpected(self.save_token()),
                    );

                    return None;
                };
                dimensions.push(DimensionSpecifier::new(self.save_spannedtoken()));
                self.next_token();

                if let Some(Token::Comma) = &self.get_cur_token() {
                    let Some(Token::Const(Constant::Integer(_))) = self.get_cur_token() else {
                        self.report_error(
                            self.lex.span(),
                            ParserErrorType::NumberExpected(self.save_token()),
                        );

                        return None;
                    };
                    dimensions.push(DimensionSpecifier::new(self.save_spannedtoken()));
                    self.next_token();
                }
            };
            if dimensions.len() > 3 {
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::TooManyDimensions(dimensions.len()),
                );

                return None;
            }

            if !matches!(self.get_cur_token(), Some(Token::RPar)) {
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::MissingCloseParens(self.save_token()),
                );
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
            self.report_error(
                self.lex.span(),
                ParserErrorType::InvalidDeclaration(self.save_token()),
            );
            return None;
        };
        let func_or_proc_token = self.save_spannedtoken();
        self.next_token();

        let Some(Token::Identifier(_name)) = self.get_cur_token() else {
            self.report_error(
                self.lex.span(),
                ParserErrorType::IdentifierExpected(self.save_token()),
            );

            return None;
        };
        let identifier_token = self.save_spannedtoken();
        self.next_token();

        if self.get_cur_token() != Some(Token::LPar) {
            self.report_error(
                self.lex.span(),
                ParserErrorType::MissingOpenParens(self.save_token()),
            );
            return None;
        }

        let leftpar_token = self.save_spannedtoken();
        self.next_token();

        let mut parameters = Vec::new();

        while self.get_cur_token() != Some(Token::RPar) {
            if self.get_cur_token().is_none() {
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::MissingCloseParens(self.save_token()),
                );

                return None;
            }

            let mut var_token = None;
            if let Some(Token::Identifier(id)) = self.get_cur_token() {
                if id == Ascii::new("VAR".to_string()) {
                    if is_function {
                        self.report_error(
                            self.lex.span(),
                            ParserErrorType::VarNotAllowedInFunctions,
                        );
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
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::TypeExpected(self.save_token()),
                );
                return None;
            }

            if self.get_cur_token() == Some(Token::Comma) {
                self.next_token();
            }
        }
        let rightpar_token = self.save_spannedtoken();
        self.next_token();
        if !is_function {
            self.check_eol();

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
            self.report_error(
                self.lex.span(),
                ParserErrorType::TypeExpected(self.save_token()),
            );
            return None;
        };
        let return_type_token = self.save_spannedtoken();
        self.next_token();
        self.check_eol();
        Some(AstNode::FunctionDeclaration(
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
        ))
    }

    fn check_eol(&mut self) -> bool {
        if self.get_cur_token() != Some(Token::Eol)
            && !matches!(self.get_cur_token(), Some(Token::Comment(_, _)))
        {
            let err_token = self.save_spannedtoken();
            self.next_token();
            self.report_error(
                err_token.span,
                ParserErrorType::EolExpected(err_token.token),
            );
            false
        } else {
            true
        }
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
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::IdentifierExpected(self.save_token()),
                );

                return None;
            };
            let identifier_token = self.save_spannedtoken();
            self.next_token();
            if self.get_cur_token() != Some(Token::LPar) {
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::MissingOpenParens(self.save_token()),
                );
                return None;
            }

            let leftpar_token = self.save_spannedtoken();
            self.next_token();

            let mut parameters = Vec::new();

            while self.get_cur_token() != Some(Token::RPar) {
                if self.get_cur_token().is_none() {
                    self.report_error(
                        self.lex.span(),
                        ParserErrorType::MissingCloseParens(self.save_token()),
                    );

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
                    self.report_error(
                        self.lex.span(),
                        ParserErrorType::TypeExpected(self.save_token()),
                    );
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

            while self.get_cur_token() != Some(Token::EndProc)
                && self.get_cur_token() != Some(Token::EndFunc)
            {
                if self.get_cur_token().is_none() {
                    self.report_error(self.lex.span(), ParserErrorType::EndExpected);
                    return None;
                }
                statements.push(self.parse_statement());
                self.skip_eol();
            }
            let endproc_token = self.save_spannedtoken();
            if endproc_token.token == Token::EndFunc {
                self.errors.lock().unwrap().report_warning(
                    endproc_token.span.clone(),
                    ParserWarningType::ProcedureClosedWithEndFunc,
                );
            }
            self.next_token();

            return Some(ProcedureImplementation::new(
                usize::MAX,
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
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::IdentifierExpected(self.save_token()),
                );

                return None;
            };
            let identifier_token = self.save_spannedtoken();
            self.next_token();
            if self.get_cur_token() != Some(Token::LPar) {
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::MissingOpenParens(self.save_token()),
                );
                return None;
            }

            let leftpar_token = self.save_spannedtoken();
            self.next_token();

            let mut parameters = Vec::new();

            while self.get_cur_token() != Some(Token::RPar) {
                if self.get_cur_token().is_none() {
                    self.report_error(
                        self.lex.span(),
                        ParserErrorType::MissingCloseParens(self.save_token()),
                    );

                    return None;
                }
                if let Some(Token::Identifier(id)) = self.get_cur_token() {
                    if id == Ascii::new("VAR".to_string()) {
                        self.report_error(
                            self.lex.span(),
                            ParserErrorType::VarNotAllowedInFunctions,
                        );
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
                    self.report_error(
                        self.lex.span(),
                        ParserErrorType::TypeExpected(self.save_token()),
                    );
                    return None;
                }

                if self.get_cur_token() == Some(Token::Comma) {
                    self.next_token();
                }
            }
            let rightpar_token = self.save_spannedtoken();
            self.next_token();

            let Some(return_type) = self.get_variable_type() else {
                self.report_error(
                    self.lex.span(),
                    ParserErrorType::TypeExpected(self.save_token()),
                );
                return None;
            };
            let return_type_token = self.save_spannedtoken();
            self.next_token();
            self.skip_eol();

            let mut statements = Vec::new();

            while self.get_cur_token() != Some(Token::EndProc)
                && self.get_cur_token() != Some(Token::EndFunc)
            {
                if self.get_cur_token().is_none() {
                    self.report_error(self.lex.span(), ParserErrorType::EndExpected);
                    return None;
                }
                statements.push(self.parse_statement());
                self.skip_eol();
            }
            let endfunc_token = self.save_spannedtoken();
            if endfunc_token.token == Token::EndProc {
                self.errors.lock().unwrap().report_warning(
                    endfunc_token.span.clone(),
                    ParserWarningType::FunctionClosedWithEndProc,
                );
            }
            self.next_token();

            return Some(FunctionImplementation::new(
                usize::MAX,
                function_token,
                identifier_token,
                leftpar_token,
                parameters,
                rightpar_token,
                return_type_token,
                return_type,
                statements.into_iter().flatten().collect(),
                endfunc_token.clone(),
            ));
        }
        None
    }
}

/// .
///
/// # Panics
///
/// Panics if .
pub fn parse_program(
    file_name: PathBuf,
    input: &str,
    encoding: Encoding,
) -> (Program, Arc<Mutex<ErrorRepoter>>) {
    let mut nodes = Vec::new();
    let mut parser = Parser::new(file_name.clone(), input, encoding);
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
                    parser
                        .errors
                        .lock()
                        .unwrap()
                        .report_warning(parser.lex.span(), ParserWarningType::UsefuncsAlreadySet);
                }
                if got_statement {
                    parser
                        .errors
                        .lock()
                        .unwrap()
                        .report_error(parser.lex.span(), ParserErrorType::UsefuncAfterStatement);
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
                    parser
                        .errors
                        .lock()
                        .unwrap()
                        .report_error(parser.lex.span(), ParserErrorType::OnlyOneBeginBlockAllowed);

                    break;
                }
                if got_funcs && !use_funcs {
                    parser.report_error(
                        parser.lex.span(),
                        ParserErrorType::NoStatementsAfterFunctions,
                    );
                    break;
                }
                parsed_begin = true;
            }
            _ => {
                let stmt = parser.parse_statement();
                if let Some(stmt) = stmt {
                    if use_funcs && !parsed_begin {
                        parser.report_error(
                            parser.lex.span(),
                            ParserErrorType::NoStatementsAllowedOutsideBlock,
                        );
                        break;
                    }
                    if got_funcs && !use_funcs {
                        parser.report_error(
                            parser.lex.span(),
                            ParserErrorType::NoStatementsAfterFunctions,
                        );
                        break;
                    }
                    if !got_statement && !matches!(stmt, Statement::VariableDeclaration(_)) {
                        got_statement = true;
                    }
                    nodes.push(AstNode::Statement(stmt));
                }
            }
        }

        parser.next_token();
    }

    (
        Program {
            nodes,
            file_name,
            require_user_variables: parser.require_user_variables,
        },
        parser.errors.clone(),
    )
}

pub struct ErrorContainer {
    pub error: Box<dyn std::error::Error + Send + Sync>,
    pub span: core::ops::Range<usize>,
}

#[derive(Default)]
pub struct ErrorRepoter {
    pub errors: Vec<ErrorContainer>,
    pub warnings: Vec<ErrorContainer>,
}

impl ErrorRepoter {
    pub fn report_error<T: std::error::Error + 'static + Send + Sync>(
        &mut self,
        span: core::ops::Range<usize>,
        error: T,
    ) {
        self.errors.push(ErrorContainer {
            error: Box::new(error),
            span,
        });
    }

    pub fn report_warning<T: std::error::Error + 'static + Send + Sync>(
        &mut self,
        span: core::ops::Range<usize>,
        warning: T,
    ) {
        self.warnings.push(ErrorContainer {
            error: Box::new(warning),
            span,
        });
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }

    pub fn report(&self) {}
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Encoding {
    Utf8,
    CP437,
}

/// .
///
/// # Errors
///
/// This function will return an error if .
pub fn load_with_encoding(file_name: &Path, encoding: Encoding) -> std::io::Result<String> {
    let src_data = fs::read(file_name)?;
    let src = if encoding == Encoding::CP437 {
        let mut res = String::new();
        for b in src_data {
            res.push(CP437_TO_UNICODE[b as usize]);
        }
        res
    } else {
        String::from_utf8_lossy(&src_data).to_string()
    };
    Ok(src)
}
