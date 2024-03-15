use crate::ast::{constant::BuiltinConst, Constant};
use core::fmt;
use std::{collections::HashMap, fs, path::PathBuf};
use thiserror::Error;
use unicase::Ascii;

#[derive(Error, Default, Debug, Clone, PartialEq)]
pub enum LexingErrorType {
    #[default]
    #[error("Invalid token")]
    InvalidToken,

    #[error("Error parsing number: '{0}' from {1}")]
    InvalidInteger(String, String),

    #[error("Unexpected end of file in string")]
    UnexpectedEOFInString,

    #[error("Error loading include file '{0}': {1}")]
    ErrorLoadingIncludeFile(String, String),

    #[error("Can't find parent of path {0}")]
    PathError(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LexingError {
    pub error: LexingErrorType,
    pub range: core::ops::Range<usize>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: core::ops::Range<usize>,
}

impl SpannedToken {
    pub fn new(token: Token, span: core::ops::Range<usize>) -> Self {
        Self { token, span }
    }
    pub fn create_empty(token: Token) -> Self {
        Self { token, span: 0..0 }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CommentType {
    SingleLineQuote,
    SingleLineSemicolon,
    SingleLineStar,
}

impl fmt::Display for CommentType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CommentType::SingleLineSemicolon => write!(f, ";"),
            CommentType::SingleLineQuote => write!(f, "'"),
            CommentType::SingleLineStar => write!(f, "*"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Eol,

    Identifier(Ascii<String>),
    Comment(CommentType, String),
    UseFuncs(CommentType, String),

    Comma,

    LPar,
    RPar,

    PoW,
    Mul,

    Div,
    Mod,
    Add,
    Sub,

    Eq,

    NotEq,
    Lower,

    LowerEq,
    Greater,

    GreaterEq,

    And,
    Or,
    Not,

    If,
    Let,
    Begin,
    While,
    EndWhile,
    Then,
    Else,
    ElseIf,
    EndIf,
    For,
    Next,
    Break,
    Continue,
    Return,
    Gosub,
    Goto,

    Select,
    Case,
    EndSelect,

    Label(unicase::Ascii<String>),

    /*
    // Types
    #[token("BOOLEAN", |_| VariableType::Boolean, ignore(case))]
    #[token("UNSIGNED", |_| VariableType::Unsigned, ignore(case))]
    #[token("DATE", |_| VariableType::Date, ignore(case))]
    #[token("EDATE", |_| VariableType::EDate, ignore(case))]
    #[token("INTEGER", |_| VariableType::Integer, ignore(case))]
    #[token("MONEY", |_| VariableType::Money, ignore(case))]
    #[token("REAL", |_| VariableType::Real, ignore(case))]
    #[token("STRING", |_| VariableType::String, ignore(case))]
    #[token("TIME", |_| VariableType::Time, ignore(case))]
    #[token("BYTE", |_| VariableType::Byte, ignore(case))]
    #[token("WORD", |_| VariableType::Word, ignore(case))]
    #[token("SHORT", |_| VariableType::SWord, ignore(case))]
    #[token("INT", |_| VariableType::Integer, ignore(case))]
    #[token("BIGSTR", |_| VariableType::String, ignore(case))]
    #[token("DREAL", |_| VariableType::Real, ignore(case))]
    #[token("DDATE", |_| VariableType::DDate, ignore(case))]
    VarType(VariableType),*/
    Declare,
    Function,
    Procedure,
    EndProc,
    EndFunc,
    End,

    Const(Constant),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Const(c) => write!(f, "{c}"),
            Token::Identifier(s) => write!(f, "{s}"),
            Token::LPar => write!(f, "("),
            Token::RPar => write!(f, ")"),
            Token::Comma => write!(f, ","),
            Token::PoW => write!(f, "**"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Mod => write!(f, "%"),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Eq => write!(f, "="),
            Token::NotEq => write!(f, "!="),
            Token::Lower => write!(f, "<"),
            Token::LowerEq => write!(f, "<="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEq => write!(f, ">="),
            Token::And => write!(f, "&"),
            Token::Or => write!(f, "|"),
            Token::Not => write!(f, "!"),

            Token::Label(s) => write!(f, ":{s}"),

            Token::Let => write!(f, "LET"),
            Token::End => write!(f, "END"),
            Token::Begin => write!(f, "BEGIN"),
            Token::While => write!(f, "WHILE"),
            Token::EndWhile => write!(f, "ENDWHILE"),
            Token::If => write!(f, "IF"),
            Token::Then => write!(f, "THEN"),
            Token::Else => write!(f, "ELSE"),
            Token::ElseIf => write!(f, "ELSEIF"),
            Token::EndIf => write!(f, "ENDIF"),

            Token::For => write!(f, "FOR"),
            Token::Next => write!(f, "NEXT"),
            Token::Break => write!(f, "BREAK"),
            Token::Continue => write!(f, "CONTINUE"),
            Token::Return => write!(f, "RETURN"),
            Token::Gosub => write!(f, "GOSUB"),
            Token::Goto => write!(f, "GOTO"),

            Token::Select => write!(f, "SELECT"),
            Token::Case => write!(f, "CASE"),
            Token::EndSelect => write!(f, "ENDSELECT"),

            Token::Comment(ct, s) => write!(f, "{}{}", ct, s),
            Token::UseFuncs(ct, s) => write!(f, "{}{}", ct, s),
            Token::Eol => writeln!(f),

            // Token::VarType(t) => write!(f, "{:?}", t),
            Token::Declare => write!(f, "DECLARE"),
            Token::Function => write!(f, "FUNCTION"),
            Token::Procedure => write!(f, "PROCEDURE"),
            Token::EndProc => write!(f, "ENDPROC"),
            Token::EndFunc => write!(f, "ENDFUNC"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LexerState {
    AfterEol,
    AfterColonEol,
    BeyondEOL,
}

pub struct Lexer {
    file: PathBuf,
    text: Vec<char>,

    lexer_state: LexerState,
    token_start: usize,
    token_end: usize,

    include_lexer: Option<Box<Lexer>>,
}

lazy_static::lazy_static! {
    static ref TOKEN_LOOKUP_TABLE: HashMap<unicase::Ascii<String>, Token> = {
        let mut m = HashMap::new();
        m.insert(unicase::Ascii::new("if".to_string()), Token::If);
        m.insert(unicase::Ascii::new("let".to_string()), Token::Let);
        m.insert(unicase::Ascii::new("begin".to_string()), Token::Begin);
        m.insert(unicase::Ascii::new("while".to_string()), Token::While);
        m.insert(unicase::Ascii::new("endwhile".to_string()), Token::EndWhile);
        m.insert(unicase::Ascii::new("then".to_string()), Token::Then);
        m.insert(unicase::Ascii::new("else".to_string()), Token::Else);
        m.insert(unicase::Ascii::new("elseif".to_string()), Token::ElseIf);
        m.insert(unicase::Ascii::new("endif".to_string()), Token::EndIf);
        m.insert(unicase::Ascii::new("for".to_string()), Token::For);
        m.insert(unicase::Ascii::new("next".to_string()), Token::Next);
        m.insert(unicase::Ascii::new("break".to_string()), Token::Break);
        m.insert(unicase::Ascii::new("quit".to_string()), Token::Break);

        m.insert(unicase::Ascii::new("continue".to_string()), Token::Continue);
        m.insert(unicase::Ascii::new("loop".to_string()), Token::Continue);
        m.insert(unicase::Ascii::new("return".to_string()), Token::Return);
        m.insert(unicase::Ascii::new("gosub".to_string()), Token::Gosub);
        m.insert(unicase::Ascii::new("goto".to_string()), Token::Goto);
        m.insert(unicase::Ascii::new("select".to_string()), Token::Select);
        m.insert(unicase::Ascii::new("case".to_string()), Token::Case);
        m.insert(unicase::Ascii::new("endselect".to_string()), Token::EndSelect);
        m.insert(unicase::Ascii::new("declare".to_string()), Token::Declare);
        m.insert(unicase::Ascii::new("function".to_string()), Token::Function);
        m.insert(unicase::Ascii::new("procedure".to_string()), Token::Procedure);
        m.insert(unicase::Ascii::new("endproc".to_string()), Token::EndProc);
        m.insert(unicase::Ascii::new("endfunc".to_string()), Token::EndFunc);
        m.insert(unicase::Ascii::new("end".to_string()), Token::End);

        m.insert(unicase::Ascii::new("ACC_CUR_BAL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::ACC_CUR_BAL)));
        m.insert(unicase::Ascii::new("ACC_MSGREAD".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::ACC_MSGREAD)));
        m.insert(unicase::Ascii::new("ACC_MSGWRITE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::ACC_MSGWRITE)));
        m.insert(unicase::Ascii::new("ACC_STAT".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::ACC_STAT)));
        m.insert(unicase::Ascii::new("ACC_TIME".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::ACC_TIME)));
        m.insert(unicase::Ascii::new("ATTACH_LIM_P".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::ATTACH_LIM_P)));
        m.insert(unicase::Ascii::new("ATTACH_LIM_U".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::ATTACH_LIM_U)));
        m.insert(unicase::Ascii::new("AUTO".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::AUTO)));
        m.insert(unicase::Ascii::new("BELL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::BELL)));
        m.insert(unicase::Ascii::new("CHRG_CALL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_CALL)));
        m.insert(unicase::Ascii::new("CHRG_CHAT".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_CHAT)));
        m.insert(unicase::Ascii::new("CHRG_DOWNBYTES".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_DOWNBYTES)));
        m.insert(unicase::Ascii::new("CHRG_DOWNFILE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_DOWNFILE)));
        m.insert(unicase::Ascii::new("CHRG_MSGCAP".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_MSGCAP)));
        m.insert(unicase::Ascii::new("CHRG_MSGECHOED".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_MSGECHOED)));
        m.insert(unicase::Ascii::new("CHRG_MSGPRIVATE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_MSGPRIVATE)));
        m.insert(unicase::Ascii::new("CHRG_MSGREAD".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_MSGREAD)));
        m.insert(unicase::Ascii::new("CHRG_MSGWRITE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_MSGWRITE)));
        m.insert(unicase::Ascii::new("CHRG_PEAKTIME".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_PEAKTIME)));
        m.insert(unicase::Ascii::new("CHRG_TIME".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CHRG_TIME)));
        m.insert(unicase::Ascii::new("CMAXMSGS".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CMAXMSGS)));
        m.insert(unicase::Ascii::new("CRC_FILE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CRC_FILE)));
        m.insert(unicase::Ascii::new("CRC_STR".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CRC_STR)));
        m.insert(unicase::Ascii::new("CRED_SPECIAL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CRED_SPECIAL)));
        m.insert(unicase::Ascii::new("CRED_UPBYTES".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CRED_UPBYTES)));
        m.insert(unicase::Ascii::new("CRED_UPFILE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CRED_UPFILE)));
        m.insert(unicase::Ascii::new("CUR_USER".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::CUR_USER)));
        m.insert(unicase::Ascii::new("DEB_CALL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_CALL)));
        m.insert(unicase::Ascii::new("DEB_CHAT".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_CHAT)));
        m.insert(unicase::Ascii::new("DEB_DOWNBYTES".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_DOWNBYTES)));
        m.insert(unicase::Ascii::new("DEB_DOWNFILE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_DOWNFILE)));
        m.insert(unicase::Ascii::new("DEB_MSGCAP".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_MSGCAP)));
        m.insert(unicase::Ascii::new("DEB_MSGECHOED".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_MSGECHOED)));
        m.insert(unicase::Ascii::new("DEB_MSGPRIVATE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_MSGPRIVATE)));
        m.insert(unicase::Ascii::new("DEB_MSGREAD".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_MSGREAD)));
        m.insert(unicase::Ascii::new("DEB_MSGWRITE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_MSGWRITE)));
        m.insert(unicase::Ascii::new("DEB_SPECIAL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_SPECIAL)));
        m.insert(unicase::Ascii::new("DEB_TIME".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_TIME)));
        m.insert(unicase::Ascii::new("DEB_TPU".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEB_TPU)));
        m.insert(unicase::Ascii::new("DEFS".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::DEFS)));
        m.insert(unicase::Ascii::new("ECHODOTS".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::ECHODOTS)));
        m.insert(unicase::Ascii::new("ERASELINE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::ERASELINE)));
        m.insert(unicase::Ascii::new("FALSE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::FALSE)));
        m.insert(unicase::Ascii::new("FCL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::FCL)));
        m.insert(unicase::Ascii::new("FIELDLEN".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::FIELDLEN)));
        m.insert(unicase::Ascii::new("FNS".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::FNS)));
        m.insert(unicase::Ascii::new("F_EXP".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::F_EXP)));
        m.insert(unicase::Ascii::new("F_MW".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::F_MW)));
        m.insert(unicase::Ascii::new("F_NET".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::F_NET)));
        m.insert(unicase::Ascii::new("F_REG".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::F_REG)));
        m.insert(unicase::Ascii::new("F_SEL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::F_SEL)));
        m.insert(unicase::Ascii::new("F_SYS".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::F_SYS)));
        m.insert(unicase::Ascii::new("GRAPH".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::GRAPH)));
        m.insert(unicase::Ascii::new("GUIDE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::GUIDE)));
        m.insert(unicase::Ascii::new("HDR_ACTIVE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_ACTIVE)));
        m.insert(unicase::Ascii::new("HDR_BLOCKS".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_BLOCKS)));
        m.insert(unicase::Ascii::new("HDR_DATE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_DATE)));
        m.insert(unicase::Ascii::new("HDR_ECHO".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_ECHO)));
        m.insert(unicase::Ascii::new("HDR_FROM".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_FROM)));
        m.insert(unicase::Ascii::new("HDR_MSGNUM".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_MSGNUM)));
        m.insert(unicase::Ascii::new("HDR_MSGREF".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_MSGREF)));
        m.insert(unicase::Ascii::new("HDR_PWD".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_PWD)));
        m.insert(unicase::Ascii::new("HDR_REPLY".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_REPLY)));
        m.insert(unicase::Ascii::new("HDR_RPLYDATE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_RPLYDATE)));
        m.insert(unicase::Ascii::new("HDR_RPLYTIME".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_RPLYTIME)));
        m.insert(unicase::Ascii::new("HDR_STATUS".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_STATUS)));
        m.insert(unicase::Ascii::new("HDR_SUBJ".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_SUBJ)));
        m.insert(unicase::Ascii::new("HDR_TIME".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_TIME)));
        m.insert(unicase::Ascii::new("HDR_TO".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HDR_TO)));
        m.insert(unicase::Ascii::new("HIGHASCII".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::HIGHASCII)));
        m.insert(unicase::Ascii::new("LANG".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::LANG)));
        m.insert(unicase::Ascii::new("LFAFTER".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::LFAFTER)));
        m.insert(unicase::Ascii::new("LFBEFORE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::LFBEFORE)));
        m.insert(unicase::Ascii::new("LOGIT".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::LOGIT)));
        m.insert(unicase::Ascii::new("LOGITLEFT".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::LOGITLEFT)));
        m.insert(unicase::Ascii::new("MAXMSGS".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::MAXMSGS)));
        m.insert(unicase::Ascii::new("NC".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::NC)));
        m.insert(unicase::Ascii::new("NEWBALANCE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::NEWBALANCE)));
        m.insert(unicase::Ascii::new("NEWLINE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::NEWLINE)));
        m.insert(unicase::Ascii::new("NOCLEAR".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::NOCLEAR)));
        m.insert(unicase::Ascii::new("NO_USER".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::NO_USER)));
        m.insert(unicase::Ascii::new("O_RD".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::O_RD)));
        m.insert(unicase::Ascii::new("O_RW".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::O_RW)));
        m.insert(unicase::Ascii::new("O_WR".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::O_WR)));
        m.insert(unicase::Ascii::new("PAY_UPBYTES".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::PAY_UPBYTES)));
        m.insert(unicase::Ascii::new("PAY_UPFILE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::PAY_UPFILE)));
        m.insert(unicase::Ascii::new("SEC".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::SEC)));
        m.insert(unicase::Ascii::new("SEC_DROP".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::SEC_DROP)));
        m.insert(unicase::Ascii::new("SEEK_CUR".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::SEEK_CUR)));
        m.insert(unicase::Ascii::new("SEEK_END".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::SEEK_END)));
        m.insert(unicase::Ascii::new("SEEK_SET".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::SEEK_SET)));
        m.insert(unicase::Ascii::new("STACKED".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::STACKED)));
        m.insert(unicase::Ascii::new("START_BAL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::START_BAL)));
        m.insert(unicase::Ascii::new("START_SESSION".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::START_SESSION)));
        m.insert(unicase::Ascii::new("STK_LIMIT".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::STK_LIMIT)));
        m.insert(unicase::Ascii::new("S_DB".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::S_DB)));
        m.insert(unicase::Ascii::new("S_DN".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::S_DN)));
        m.insert(unicase::Ascii::new("S_DR".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::S_DR)));
        m.insert(unicase::Ascii::new("S_DW".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::S_DW)));
        m.insert(unicase::Ascii::new("TRUE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::TRUE)));
        m.insert(unicase::Ascii::new("UPCASE".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::UPCASE)));
        m.insert(unicase::Ascii::new("WARNLEVEL".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::WARNLEVEL)));
        m.insert(unicase::Ascii::new("WORDWRAP".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::WORDWRAP)));
        m.insert(unicase::Ascii::new("YESNO".to_string()), Token::Const(Constant::Builtin(&BuiltinConst::YESNO)));

        m
    };
}

impl Lexer {
    pub fn new(file: PathBuf, text: &str) -> Self {
        Self {
            file,
            text: text.chars().collect(),
            lexer_state: LexerState::AfterEol,
            token_start: 0,
            token_end: 0,
            include_lexer: None,
        }
    }

    #[inline]
    fn next_ch(&mut self) -> Option<char> {
        if self.token_end >= self.text.len() {
            None
        } else {
            let t = self.text[self.token_end];
            self.token_end += 1;
            Some(t)
        }
    }

    #[inline]
    fn put_back(&mut self) {
        self.token_end -= 1;
    }

    pub fn next_token(&mut self) -> Option<Result<Token, LexingError>> {
        if let Some(lexer) = &mut self.include_lexer {
            let result = lexer.next_token();
            match result {
                Some(Ok(token)) => {
                    return Some(Ok(token));
                }
                Some(Err(err)) => {
                    return Some(Err(LexingError {
                        error: err.error,
                        range: self.token_start..self.token_end,
                    }))
                }
                None => {
                    self.include_lexer = None;
                }
            }
        }
        let ch;
        loop {
            self.token_start = self.token_end;
            let Some(next_ch) = self.next_ch() else {
                return None;
            };
            if next_ch != ' ' && next_ch != '\t' {
                ch = next_ch;
                break;
            }
        }
        let state = match ch {
            '\'' | // comment
            ';' => {
                return Some(self.read_comment(ch));
            }
            '"' => {
                let mut string_result = String::new();
                loop {
                    let Some(sch) = self.next_ch() else {
                        return Some(Err(LexingError {
                            error: LexingErrorType::UnexpectedEOFInString,
                            range: self.token_start..self.token_end,
                        }));
                    };
                    if sch == '"'  {
                        if let Some('"') = self.next_ch() {
                            string_result.push('"');
                            continue;
                        };
                        self.put_back();
                        break;
                    }
                    string_result.push(sch);
                }
                Some(Ok(Token::Const(Constant::String(string_result))))
            }
            '_' => { // eol continuation
                let next = self.next_ch();
                if let Some('\r') = next {
                    if let Some('\n') = self.next_ch() {
                        return self.next_token();
                    }
                    return Some(Err(LexingError {
                        error: LexingErrorType::InvalidToken,
                        range: self.token_start..self.token_end,
                    }));
                }
                if let Some('\n') = next {
                    return self.next_token();
                }
                return Some(self.read_identifier());
            },
            '\r' => {
                return if let Some('\n') = self.next_ch() {
                    self.lexer_state = LexerState::AfterEol;
                    Some(Ok(Token::Eol))
                } else {
                    self.put_back();
                    self.lexer_state = LexerState::AfterEol;
                    Some(Ok(Token::Eol))
                };
            },
            '\n' => {
                self.lexer_state = LexerState::AfterEol;
                return Some(Ok(Token::Eol));
            },
            ':' => {
                if self.lexer_state == LexerState::BeyondEOL {
                    self.lexer_state = LexerState::AfterColonEol;
                    return Some(Ok(Token::Eol));
                }

                loop {
                    let Some(ch) = self.next_ch() else {
                        break;
                    };
                    //assert!(ch.is_some(), "Unexpected eof in string_literal at ({}, {}).", self.line, self.col);
                    if !(ch.is_ascii_alphanumeric() || "_@#$¢£¥€".contains(ch)) {
                            self.put_back();
                            break;
                    }
                }

                let identifier = unicase::Ascii::new(self.text[self.token_start+1..self.token_end].iter().collect::<String>());
                if let Some(token) = TOKEN_LOOKUP_TABLE.get(&identifier) {
                    return Some(Ok(token.clone()));
                }
                Some(Ok(Token::Label(identifier)))
            },

            '(' => Some(Ok(Token::LPar)),
            ')' => Some(Ok(Token::RPar)),
            ',' => Some(Ok(Token::Comma)),
            '^' => Some(Ok(Token::PoW)),
            '*' => {
                if self.lexer_state != LexerState::BeyondEOL {
                    return Some(self.read_comment(ch));
                }
                let next = self.next_ch();
                 if let Some('*') = next {
                    Some(Ok(Token::PoW))
                } else {
                    self.put_back();
                    Some(Ok(Token::Mul))
                }
             },
            '/' => Some(Ok(Token::Div)),
            '%' => Some(Ok(Token::Mod)),
            '+' => Some(Ok(Token::Add)),
            '-' => Some(Ok(Token::Sub)),
            '=' => {
                let next = self.next_ch();
                 match next {
                    Some('<') => Some(Ok(Token::LowerEq)),
                    Some('>') => Some(Ok(Token::GreaterEq)),
                    Some('=') => Some(Ok(Token::Eq)),
                     _ => {
                         self.put_back();
                         Some(Ok(Token::Eq))
                     }
                 }
             },
            '&'  => {
                let next = self.next_ch();
                 if let Some('&') = next {
                    Some(Ok(Token::And))
                } else {
                                         self.put_back();
                                         Some(Ok(Token::And))
                                     }
             },
            '|' => {
                let next = self.next_ch();
                 if let Some('|') = next {
                    Some(Ok(Token::Or))
                } else {
                    self.put_back();
                    Some(Ok(Token::Or))
                }
             },
            '!' => {
                let next = self.next_ch();
                 if let Some('=') = next {
                    Some(Ok(Token::NotEq))
                } else {
                    self.put_back();
                    Some(Ok(Token::Not))
                }
             },
            '@' => {
                let ch = self.next_ch();
                if Some('X') != ch && Some('x') != ch {
                    return Some(Err(LexingError {
                        error: LexingErrorType::InvalidToken,
                        range: self.token_start..self.token_end,
                    }));
                }
                let Some(first) = self.next_ch() else {
                    return Some(Err(LexingError {
                        error: LexingErrorType::InvalidToken,
                        range: self.token_start..self.token_end,
                    }));
                };
                let Some(second) = self.next_ch() else {
                    return Some(Err(LexingError {
                        error: LexingErrorType::InvalidToken,
                        range: self.token_start..self.token_end,
                    }));
                };
                if !first.is_ascii_hexdigit() || !second.is_ascii_hexdigit() {
                    return Some(Err(LexingError {
                        error: LexingErrorType::InvalidToken,
                        range: self.token_start..self.token_end,
                    }));
                }
                Some(Ok(Token::Const(Constant::Integer(conv_hex(first) * 16 + conv_hex(second)))))
            }
            '$' => {
                let mut identifier = String::new();
                loop {
                    let Some(ch) = self.next_ch() else {
                        break;
                    };
                    if !ch.is_ascii_digit() && ch != '.' {
                        break;
                    }
                    identifier.push(ch);
                }
                self.put_back();
                let Ok(r) = identifier.parse::<f64>() else {
                    return Some(Err(LexingError {
                        error: LexingErrorType::InvalidToken,
                        range: self.token_start..self.token_end,
                    }));
                };
                Some(Ok(Token::Const(Constant::Money((r * 100.0) as i32))))
            }

            '<' => {
                let next = self.next_ch();
                match next {
                    Some('>') => Some(Ok(Token::NotEq)),
                    Some('=') => Some(Ok(Token::LowerEq)),
                    _ => {
                        self.put_back();
                        Some(Ok(Token::Lower))
                    }
                }
            },
            '>' => {
                 let next = self.next_ch();
                 match next {
                     Some('<') => Some(Ok(Token::NotEq)),
                     Some('=') => Some(Ok(Token::GreaterEq)),
                     _ => {
                         self.put_back();
                         Some(Ok(Token::Greater))
                     }
                 }
             }
            _ => {
                if ch.is_ascii_alphabetic() || ch == '_' {
                    return Some(self.read_identifier());
                }

                if ch.is_ascii_digit() {
                    self.lexer_state = LexerState::BeyondEOL;

                    let start = self.token_start;
                    loop {
                        let Some(ch) = self.next_ch() else {
                            break;
                        };

                        match ch {
                            'D' | 'd' => {
                                let r = self.text[start..self.token_end - 1].iter().collect::<String>().parse::<i32>();
                                match r {
                                    Ok(i) => {
                                        return Some(Ok(Token::Const(Constant::Integer(i))));
                                    }
                                    Err(r) => return Some(Err(LexingError {
                                        error: LexingErrorType::InvalidInteger(r.to_string(), self.text[self.token_start..self.token_end].iter().collect::<String>()),
                                        range: self.token_start..self.token_end,
                                    }))
                                }
                            }
                            'H' | 'h' => {
                                let r = i32::from_str_radix(&self.text[start..self.token_end - 1].iter().collect::<String>(), 16);
                                match r {
                                    Ok(i) => {
                                        return Some(Ok(Token::Const(Constant::Integer(i))));
                                    }
                                    Err(r) => return Some(Err(LexingError {
                                        error: LexingErrorType::InvalidInteger(r.to_string(), self.text[self.token_start..self.token_end].iter().collect::<String>()),
                                        range: self.token_start..self.token_end,
                                    }))
                                }
                            }
                            'O' | 'o' => {
                                let r = i32::from_str_radix(&self.text[start..self.token_end - 1].iter().collect::<String>(), 8);
                                match r {
                                    Ok(i) => {
                                        return Some(Ok(Token::Const(Constant::Integer(i))));
                                    }
                                    Err(r) => return Some(Err(LexingError {
                                        error: LexingErrorType::InvalidInteger(r.to_string(), self.text[self.token_start..self.token_end].iter().collect::<String>()),
                                        range: self.token_start..self.token_end,
                                    }))
                                }
                            }
                            'B' | 'b' => {
                                let r = i32::from_str_radix(&self.text[start..self.token_end - 1].iter().collect::<String>(), 2);
                                match r {
                                    Ok(i) => {
                                        return Some(Ok(Token::Const(Constant::Integer(i))));
                                    }
                                    Err(r) => return Some(Err(LexingError {
                                        error: LexingErrorType::InvalidInteger(r.to_string(), self.text[self.token_start..self.token_end].iter().collect::<String>()),
                                        range: self.token_start..self.token_end,
                                    }))
                                }
                            }
                            _ => {}
                        }
                        if !ch.is_ascii_hexdigit() && ch != '.'  {
                            self.put_back();
                            break;
                        }
                    }
                    let end: usize = self.token_end;

                    let r = self.text[start..end].iter().collect::<String>().parse::<i64>();
                    match r {
                        Ok(i) => {
                            if i32::try_from(i).is_ok()  {
                                return Some(Ok(Token::Const(Constant::Integer(i as i32))));
                            }
                            if i <= u32::MAX as i64 {
                                return Some(Ok(Token::Const(Constant::Unsigned(i as u32))));
                            }
                        }
                        Err(r) => return Some(Err(LexingError {
                            error: LexingErrorType::InvalidInteger(r.to_string(), self.text[start..end].iter().collect::<String>()),
                            range: self.token_start..self.token_end,
                        }))
                    }
                }

                Some(Err(LexingError {
                    error: LexingErrorType::InvalidToken,
                    range: self.token_start..self.token_end,
                }))
            }
        };
        self.lexer_state = LexerState::BeyondEOL;
        state
    }

    #[allow(clippy::unnecessary_wraps)]
    fn read_identifier(&mut self) -> Result<Token, LexingError> {
        self.lexer_state = LexerState::BeyondEOL;
        loop {
            let Some(ch) = self.next_ch() else {
                break;
            };
            //assert!(ch.is_some(), "Unexpected eof in string_literal at ({}, {}).", self.line, self.col);
            if !(ch.is_ascii_alphanumeric() || "_@#$¢£¥€".contains(ch)) {
                self.put_back();
                break;
            }
        }
        let identifier = unicase::Ascii::new(
            self.text[self.token_start..self.token_end]
                .iter()
                .collect::<String>(),
        );
        if let Some(token) = TOKEN_LOOKUP_TABLE.get(&identifier) {
            return Ok(token.clone());
        }
        Ok(Token::Identifier(identifier))
    }

    fn read_comment(&mut self, ch: char) -> Result<Token, LexingError> {
        let cmt_type = match ch {
            ';' => CommentType::SingleLineSemicolon,
            '*' => CommentType::SingleLineStar,
            _ => CommentType::SingleLineQuote,
        };
        loop {
            let Some(ch) = self.next_ch() else {
                break;
            };
            if ch == '\n' {
                break;
            }
        }
        self.lexer_state = LexerState::AfterEol;
        let comment = self.text[self.token_start + 1..self.token_end]
            .iter()
            .collect::<String>();

        if comment.len() > "$INCLUDE".len()
            && comment[.."$INCLUDE".len()].to_ascii_uppercase() == "$INCLUDE"
        {
            let include_file = comment["$INCLUDE:".len()..].trim();

            let Some(parent) = self.file.parent() else {
                return Err(LexingError {
                    error: LexingErrorType::PathError(self.file.to_string_lossy().to_string()),
                    range: self.token_start..self.token_end,
                });
            };

            let path = parent.join(include_file);

            match fs::read_to_string(&path) {
                Ok(k) => {
                    self.include_lexer = Some(Box::new(Lexer::new(path, &k)));
                }
                Err(err) => {
                    return Err(LexingError {
                        error: LexingErrorType::ErrorLoadingIncludeFile(
                            include_file.to_string(),
                            err.to_string(),
                        ),
                        range: self.token_start..self.token_end,
                    });
                }
            }
        }

        if comment.len() > "$USEFUNCS".len()
            && comment[.."$USEFUNCS".len()].to_ascii_uppercase() == "$USEFUNCS"
        {
            return Ok(Token::UseFuncs(cmt_type, comment));
        }
        Ok(Token::Comment(cmt_type, comment))
    }

    pub fn span(&self) -> std::ops::Range<usize> {
        self.token_start..self.token_end
    }
}

fn conv_hex(first: char) -> i32 {
    if first.is_ascii_digit() {
        return first as i32 - b'0' as i32;
    }
    if ('a'..='f').contains(&first) {
        return first as i32 - b'a' as i32 + 10;
    }
    if ('A'..='F').contains(&first) {
        return first as i32 - b'A' as i32 + 10;
    }
    0
}
