use crate::ast::{constant::BuiltinConst, Constant};
use core::fmt;
use logos::Logos;
use thiserror::Error;

#[derive(Error, Default, Debug, Clone, PartialEq)]
pub enum LexingError {
    #[default]
    #[error("Invalid character")]
    InvalidChar,

    #[error("Error parsing number: '{0}' from {1}")]
    InvalidInteger(String, String),
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\f]+")] // Ignore this regex pattern between tokens
pub enum Token {
    #[token("\r\n")]
    #[token("\n")]
    Eol,

    #[token(",")]
    Comma,

    #[token("(")]
    LPar,
    #[token(")")]
    RPar,

    #[token("^")]
    #[token("**")]
    PoW,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("%")]
    Mod,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,

    #[token("==")]
    #[token("=")]
    Eq,

    #[token("!=")]
    #[token("<>")]
    #[token("><")]
    NotEq,

    #[token("<")]
    Lower,

    #[token("<=")]
    #[token("=<")]
    LowerEq,

    #[token(">")]
    Greater,

    #[token(">=")]
    #[token("=>")]
    GreaterEq,

    #[token("&&")]
    #[token("&")]
    And,

    #[token("||")]
    #[token("|")]
    Or,

    #[token("!")]
    Not,

    #[token("let", ignore(case))]
    Let,

    #[token("end", ignore(case))]
    End,

    #[token("begin", ignore(case))]
    Begin,

    #[token("while", ignore(case))]
    While,

    #[token("if", ignore(case))]
    If,

    #[token("then", ignore(case))]
    Then,

    #[token("else", ignore(case))]
    Else,
    #[token("elseif", ignore(case))]
    ElseIf,
    #[token("endif", ignore(case))]
    EndIf,

    #[token("for", ignore(case))]
    For,
    #[token("break", ignore(case))]
    Break,
    #[token("continue", ignore(case))]
    Continue,
    #[token("return", ignore(case))]
    Return,
    #[token("gosub", ignore(case))]
    Gosub,
    #[token("goto", ignore(case))]
    Goto,

    #[regex(r":\w+", |lex| lex.slice()[1..].to_string())]
    Label(String),

    #[regex(r"[';][^\n]*")]
    Comment,

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| Constant::String(lex.slice()[1..lex.slice().len() - 1].to_string()))]
    #[regex(r"@[xX][0-9a-fA-F][0-9a-fA-F]", |lex| {
        let slice = lex.slice();
        let num = i32::from_str_radix(&lex.slice()[2..], 16);
        match num {
            Ok(num) => Ok(Constant::Integer(num)),
            Err(err) => Err(LexingError::InvalidInteger(err.to_string(), slice.to_string()))
        }
    })]
    #[regex(r"[0-9][0-9a-fA-F]*[hH]", |lex| {
        let slice = lex.slice();
        let num = i32::from_str_radix(&slice[..slice.len() - 1], 16);
        match num {
            Ok(num) => Ok(Constant::Integer(num)),
            Err(err) => Err(LexingError::InvalidInteger(err.to_string(), slice.to_string()))
        }
    })]
    #[regex(r"[0-7]+[oO]", |lex| {
        let slice = lex.slice();
        let num = i32::from_str_radix(&slice[..slice.len() - 1], 8);
        match num {
            Ok(num) => Ok(Constant::Integer(num)),
            Err(err) => Err(LexingError::InvalidInteger(err.to_string(), slice.to_string()))
        }
    })]
    #[regex(r"[01]+[bB]", |lex| {
        let slice = lex.slice();
        let num = i32::from_str_radix(&slice[..slice.len() - 1], 2);
        match num {
            Ok(num) => Ok(Constant::Integer(num)),
            Err(err) => Err(LexingError::InvalidInteger(err.to_string(), slice.to_string()))
        }
    })]
    #[regex(r"\d+[dD]?", |lex| {
        let slice = lex.slice();
        let len = if slice.ends_with('d') || slice.ends_with('D') { slice.len() - 1 } else {  slice.len() };
        let num = slice[..len].parse::<i32>();
        match num {
            Ok(num) => Ok(Constant::Integer(num)),
            Err(err) => Err(LexingError::InvalidInteger(err.to_string(), slice.to_string()))
        }
    })]
    #[regex(r"\$\d+(\.\d+)?", |lex| {
        let slice = lex.slice();
        let num = lex.slice()[1..].parse::<f64>();
        match num {
            Ok(num) => Ok(Constant::Money(num)),
            Err(err) => Err(LexingError::InvalidInteger(err.to_string(), slice.to_string()))
        }
    })]
    #[regex(r"(?:0|[1-9]\d*)(?:(?:[eE][+-]?\d+)|\.\d+(?:[eE][+-]?\d+)?)",  |lex| {
        let slice = lex.slice();
        let num = lex.slice().parse::<f64>();
        match num {
            Ok(num) => Ok(Constant::Real(num)),
            Err(err) => Err(LexingError::InvalidInteger(err.to_string(), slice.to_string()))
        }
    })]
    // AUTO GENERATED CONSTANTS
    #[token("ACC_CUR_BAL", |_| Constant::Builtin(&BuiltinConst::ACC_CUR_BAL), ignore(case))]
    #[token("ACC_MSGREAD", |_| Constant::Builtin(&BuiltinConst::ACC_MSGREAD), ignore(case))]
    #[token("ACC_MSGWRITE", |_| Constant::Builtin(&BuiltinConst::ACC_MSGWRITE), ignore(case))]
    #[token("ACC_STAT", |_| Constant::Builtin(&BuiltinConst::ACC_STAT), ignore(case))]
    #[token("ACC_TIME", |_| Constant::Builtin(&BuiltinConst::ACC_TIME), ignore(case))]
    #[token("ATTACH_LIM_P", |_| Constant::Builtin(&BuiltinConst::ATTACH_LIM_P), ignore(case))]
    #[token("ATTACH_LIM_U", |_| Constant::Builtin(&BuiltinConst::ATTACH_LIM_U), ignore(case))]
    #[token("AUTO", |_| Constant::Builtin(&BuiltinConst::AUTO), ignore(case))]
    #[token("BELL", |_| Constant::Builtin(&BuiltinConst::BELL), ignore(case))]
    #[token("CHRG_CALL", |_| Constant::Builtin(&BuiltinConst::CHRG_CALL), ignore(case))]
    #[token("CHRG_CHAT", |_| Constant::Builtin(&BuiltinConst::CHRG_CHAT), ignore(case))]
    #[token("CHRG_DOWNBYTES", |_| Constant::Builtin(&BuiltinConst::CHRG_DOWNBYTES), ignore(case))]
    #[token("CHRG_DOWNFILE", |_| Constant::Builtin(&BuiltinConst::CHRG_DOWNFILE), ignore(case))]
    #[token("CHRG_MSGCAP", |_| Constant::Builtin(&BuiltinConst::CHRG_MSGCAP), ignore(case))]
    #[token("CHRG_MSGECHOED", |_| Constant::Builtin(&BuiltinConst::CHRG_MSGECHOED), ignore(case))]
    #[token("CHRG_MSGPRIVATE", |_| Constant::Builtin(&BuiltinConst::CHRG_MSGPRIVATE), ignore(case))]
    #[token("CHRG_MSGREAD", |_| Constant::Builtin(&BuiltinConst::CHRG_MSGREAD), ignore(case))]
    #[token("CHRG_MSGWRITE", |_| Constant::Builtin(&BuiltinConst::CHRG_MSGWRITE), ignore(case))]
    #[token("CHRG_PEAKTIME", |_| Constant::Builtin(&BuiltinConst::CHRG_PEAKTIME), ignore(case))]
    #[token("CHRG_TIME", |_| Constant::Builtin(&BuiltinConst::CHRG_TIME), ignore(case))]
    #[token("CMAXMSGS", |_| Constant::Builtin(&BuiltinConst::CMAXMSGS), ignore(case))]
    #[token("CRC_FILE", |_| Constant::Builtin(&BuiltinConst::CRC_FILE), ignore(case))]
    #[token("CRC_STR", |_| Constant::Builtin(&BuiltinConst::CRC_STR), ignore(case))]
    #[token("CRED_SPECIAL", |_| Constant::Builtin(&BuiltinConst::CRED_SPECIAL), ignore(case))]
    #[token("CRED_UPBYTES", |_| Constant::Builtin(&BuiltinConst::CRED_UPBYTES), ignore(case))]
    #[token("CRED_UPFILE", |_| Constant::Builtin(&BuiltinConst::CRED_UPFILE), ignore(case))]
    #[token("CUR_USER", |_| Constant::Builtin(&BuiltinConst::CUR_USER), ignore(case))]
    #[token("DEB_CALL", |_| Constant::Builtin(&BuiltinConst::DEB_CALL), ignore(case))]
    #[token("DEB_CHAT", |_| Constant::Builtin(&BuiltinConst::DEB_CHAT), ignore(case))]
    #[token("DEB_DOWNBYTES", |_| Constant::Builtin(&BuiltinConst::DEB_DOWNBYTES), ignore(case))]
    #[token("DEB_DOWNFILE", |_| Constant::Builtin(&BuiltinConst::DEB_DOWNFILE), ignore(case))]
    #[token("DEB_MSGCAP", |_| Constant::Builtin(&BuiltinConst::DEB_MSGCAP), ignore(case))]
    #[token("DEB_MSGECHOED", |_| Constant::Builtin(&BuiltinConst::DEB_MSGECHOED), ignore(case))]
    #[token("DEB_MSGPRIVATE", |_| Constant::Builtin(&BuiltinConst::DEB_MSGPRIVATE), ignore(case))]
    #[token("DEB_MSGREAD", |_| Constant::Builtin(&BuiltinConst::DEB_MSGREAD), ignore(case))]
    #[token("DEB_MSGWRITE", |_| Constant::Builtin(&BuiltinConst::DEB_MSGWRITE), ignore(case))]
    #[token("DEB_SPECIAL", |_| Constant::Builtin(&BuiltinConst::DEB_SPECIAL), ignore(case))]
    #[token("DEB_TIME", |_| Constant::Builtin(&BuiltinConst::DEB_TIME), ignore(case))]
    #[token("DEB_TPU", |_| Constant::Builtin(&BuiltinConst::DEB_TPU), ignore(case))]
    #[token("DEFS", |_| Constant::Builtin(&BuiltinConst::DEFS), ignore(case))]
    #[token("ECHODOTS", |_| Constant::Builtin(&BuiltinConst::ECHODOTS), ignore(case))]
    #[token("ERASELINE", |_| Constant::Builtin(&BuiltinConst::ERASELINE), ignore(case))]
    #[token("FALSE", |_| Constant::Builtin(&BuiltinConst::FALSE), ignore(case))]
    #[token("FCL", |_| Constant::Builtin(&BuiltinConst::FCL), ignore(case))]
    #[token("FIELDLEN", |_| Constant::Builtin(&BuiltinConst::FIELDLEN), ignore(case))]
    #[token("FNS", |_| Constant::Builtin(&BuiltinConst::FNS), ignore(case))]
    #[token("F_EXP", |_| Constant::Builtin(&BuiltinConst::F_EXP), ignore(case))]
    #[token("F_MW", |_| Constant::Builtin(&BuiltinConst::F_MW), ignore(case))]
    #[token("F_NET", |_| Constant::Builtin(&BuiltinConst::F_NET), ignore(case))]
    #[token("F_REG", |_| Constant::Builtin(&BuiltinConst::F_REG), ignore(case))]
    #[token("F_SEL", |_| Constant::Builtin(&BuiltinConst::F_SEL), ignore(case))]
    #[token("F_SYS", |_| Constant::Builtin(&BuiltinConst::F_SYS), ignore(case))]
    #[token("GRAPH", |_| Constant::Builtin(&BuiltinConst::GRAPH), ignore(case))]
    #[token("GUIDE", |_| Constant::Builtin(&BuiltinConst::GUIDE), ignore(case))]
    #[token("HDR_ACTIVE", |_| Constant::Builtin(&BuiltinConst::HDR_ACTIVE), ignore(case))]
    #[token("HDR_BLOCKS", |_| Constant::Builtin(&BuiltinConst::HDR_BLOCKS), ignore(case))]
    #[token("HDR_DATE", |_| Constant::Builtin(&BuiltinConst::HDR_DATE), ignore(case))]
    #[token("HDR_ECHO", |_| Constant::Builtin(&BuiltinConst::HDR_ECHO), ignore(case))]
    #[token("HDR_FROM", |_| Constant::Builtin(&BuiltinConst::HDR_FROM), ignore(case))]
    #[token("HDR_MSGNUM", |_| Constant::Builtin(&BuiltinConst::HDR_MSGNUM), ignore(case))]
    #[token("HDR_MSGREF", |_| Constant::Builtin(&BuiltinConst::HDR_MSGREF), ignore(case))]
    #[token("HDR_PWD", |_| Constant::Builtin(&BuiltinConst::HDR_PWD), ignore(case))]
    #[token("HDR_REPLY", |_| Constant::Builtin(&BuiltinConst::HDR_REPLY), ignore(case))]
    #[token("HDR_RPLYDATE", |_| Constant::Builtin(&BuiltinConst::HDR_RPLYDATE), ignore(case))]
    #[token("HDR_RPLYTIME", |_| Constant::Builtin(&BuiltinConst::HDR_RPLYTIME), ignore(case))]
    #[token("HDR_STATUS", |_| Constant::Builtin(&BuiltinConst::HDR_STATUS), ignore(case))]
    #[token("HDR_SUBJ", |_| Constant::Builtin(&BuiltinConst::HDR_SUBJ), ignore(case))]
    #[token("HDR_TIME", |_| Constant::Builtin(&BuiltinConst::HDR_TIME), ignore(case))]
    #[token("HDR_TO", |_| Constant::Builtin(&BuiltinConst::HDR_TO), ignore(case))]
    #[token("HIGHASCII", |_| Constant::Builtin(&BuiltinConst::HIGHASCII), ignore(case))]
    #[token("LANG", |_| Constant::Builtin(&BuiltinConst::LANG), ignore(case))]
    #[token("LFAFTER", |_| Constant::Builtin(&BuiltinConst::LFAFTER), ignore(case))]
    #[token("LFBEFORE", |_| Constant::Builtin(&BuiltinConst::LFBEFORE), ignore(case))]
    #[token("LOGIT", |_| Constant::Builtin(&BuiltinConst::LOGIT), ignore(case))]
    #[token("LOGITLEFT", |_| Constant::Builtin(&BuiltinConst::LOGITLEFT), ignore(case))]
    #[token("MAXMSGS", |_| Constant::Builtin(&BuiltinConst::MAXMSGS), ignore(case))]
    #[token("NC", |_| Constant::Builtin(&BuiltinConst::NC), ignore(case))]
    #[token("NEWBALANCE", |_| Constant::Builtin(&BuiltinConst::NEWBALANCE), ignore(case))]
    #[token("NEWLINE", |_| Constant::Builtin(&BuiltinConst::NEWLINE), ignore(case))]
    #[token("NOCLEAR", |_| Constant::Builtin(&BuiltinConst::NOCLEAR), ignore(case))]
    #[token("NO_USER", |_| Constant::Builtin(&BuiltinConst::NO_USER), ignore(case))]
    #[token("O_RD", |_| Constant::Builtin(&BuiltinConst::O_RD), ignore(case))]
    #[token("O_RW", |_| Constant::Builtin(&BuiltinConst::O_RW), ignore(case))]
    #[token("O_WR", |_| Constant::Builtin(&BuiltinConst::O_WR), ignore(case))]
    #[token("PAY_UPBYTES", |_| Constant::Builtin(&BuiltinConst::PAY_UPBYTES), ignore(case))]
    #[token("PAY_UPFILE", |_| Constant::Builtin(&BuiltinConst::PAY_UPFILE), ignore(case))]
    #[token("SEC", |_| Constant::Builtin(&BuiltinConst::SEC), ignore(case))]
    #[token("SEC_DROP", |_| Constant::Builtin(&BuiltinConst::SEC_DROP), ignore(case))]
    #[token("SEEK_CUR", |_| Constant::Builtin(&BuiltinConst::SEEK_CUR), ignore(case))]
    #[token("SEEK_END", |_| Constant::Builtin(&BuiltinConst::SEEK_END), ignore(case))]
    #[token("SEEK_SET", |_| Constant::Builtin(&BuiltinConst::SEEK_SET), ignore(case))]
    #[token("STACKED", |_| Constant::Builtin(&BuiltinConst::STACKED), ignore(case))]
    #[token("START_BAL", |_| Constant::Builtin(&BuiltinConst::START_BAL), ignore(case))]
    #[token("START_SESSION", |_| Constant::Builtin(&BuiltinConst::START_SESSION), ignore(case))]
    #[token("STK_LIMIT", |_| Constant::Builtin(&BuiltinConst::STK_LIMIT), ignore(case))]
    #[token("S_DB", |_| Constant::Builtin(&BuiltinConst::S_DB), ignore(case))]
    #[token("S_DN", |_| Constant::Builtin(&BuiltinConst::S_DN), ignore(case))]
    #[token("S_DR", |_| Constant::Builtin(&BuiltinConst::S_DR), ignore(case))]
    #[token("S_DW", |_| Constant::Builtin(&BuiltinConst::S_DW), ignore(case))]
    #[token("TRUE", |_| Constant::Builtin(&BuiltinConst::TRUE), ignore(case))]
    #[token("UPCASE", |_| Constant::Builtin(&BuiltinConst::UPCASE), ignore(case))]
    #[token("WARNLEVEL", |_| Constant::Builtin(&BuiltinConst::WARNLEVEL), ignore(case))]
    #[token("WORDWRAP", |_| Constant::Builtin(&BuiltinConst::WORDWRAP), ignore(case))]
    #[token("YESNO", |_| Constant::Builtin(&BuiltinConst::YESNO), ignore(case))]
    Const(Constant),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string(), priority = 5)]
    Identifier(String),
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
            Token::If => write!(f, "IF"),
            Token::Then => write!(f, "THEN"),
            Token::Else => write!(f, "ELSE"),
            Token::ElseIf => write!(f, "ELSEIF"),
            Token::EndIf => write!(f, "ENDIF"),

            Token::For => write!(f, "FOR"),
            Token::Break => write!(f, "BREAK"),
            Token::Continue => write!(f, "CONTINUE"),
            Token::Return => write!(f, "RETURN"),
            Token::Gosub => write!(f, "GOSUB"),
            Token::Goto => write!(f, "GOTO"),
            Token::Comment => write!(f, ""),
            Token::Eol => writeln!(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use crate::{ast::Constant, parser::tokens::Token};

    #[test]
    fn test_comments() {
        assert_eq!(Token::Comment, get_token("; COMMENT"));
        assert_eq!(Token::Comment, get_token("' COMMENT"));
    }

    fn get_token(src: &str) -> Token {
        let mut lex: logos::Lexer<'_, Token> = crate::parser::tokens::Token::lexer(src);
        lex.next().unwrap().unwrap()
    }

    #[test]
    fn test_string() {
        let src = "\"Hello World\"\"foo\"";
        let mut lex: logos::Lexer<'_, Token> = crate::parser::tokens::Token::lexer(src);
        assert_eq!(
            Token::Const(Constant::String("Hello World".to_string())),
            lex.next().unwrap().unwrap()
        );
        assert_eq!(
            Token::Const(Constant::String("foo".to_string())),
            lex.next().unwrap().unwrap()
        );
    }

    #[test]
    fn test_op() {
        assert_eq!(Token::Eq, get_token("=="));
        assert_eq!(Token::Eq, get_token("="));
        assert_eq!(Token::And, get_token("&&"));
        assert_eq!(Token::And, get_token("&"));
        assert_eq!(Token::Or, get_token("||"));
        assert_eq!(Token::Or, get_token("|"));
        assert_eq!(Token::Not, get_token("!"));
        assert_eq!(Token::PoW, get_token("**"));
        assert_eq!(Token::PoW, get_token("^"));

        assert_eq!(Token::NotEq, get_token("<>"));
        assert_eq!(Token::NotEq, get_token("><"));
        assert_eq!(Token::NotEq, get_token("!="));
        assert_eq!(Token::Lower, get_token("<"));
        assert_eq!(Token::LowerEq, get_token("<="));
        assert_eq!(Token::LowerEq, get_token("=<"));
        assert_eq!(Token::Greater, get_token(">"));
        assert_eq!(Token::GreaterEq, get_token(">="));
        assert_eq!(Token::GreaterEq, get_token("=>"));
    }

    #[test]
    fn test_identifier() {
        assert_eq!(Token::Identifier("PRINT".to_string()), get_token("PRINT"));

        let src = "Hello World";
        let mut lex = crate::parser::tokens::Token::lexer(src);

        assert_eq!(
            Token::Identifier("Hello".to_string()),
            lex.next().unwrap().unwrap()
        );
        assert_eq!(
            Token::Identifier("World".to_string()),
            lex.next().unwrap().unwrap()
        );
    }

    #[test]
    fn test_constants() {
        assert_eq!(Token::Const(Constant::Integer(123)), get_token("123"));
        assert_eq!(Token::Const(Constant::Integer(100)), get_token("@X64"));

        assert_eq!(Token::Const(Constant::Money(1.42)), get_token("$1.42"));
        assert_eq!(Token::Const(Constant::Integer(255)), get_token("0FFh"));
        assert_eq!(Token::Const(Constant::Integer(123)), get_token("123d"));
        assert_eq!(Token::Const(Constant::Integer(88)), get_token("130o"));
        assert_eq!(Token::Const(Constant::Integer(8)), get_token("1000b"));
        assert_eq!(
            Token::Const(Constant::Builtin(&crate::ast::constant::BuiltinConst::TRUE)),
            get_token("TRUE")
        );
        assert_eq!(
            Token::Const(Constant::Builtin(
                &crate::ast::constant::BuiltinConst::FALSE
            )),
            get_token("FALSE")
        );
    }

    #[test]
    fn test_errors() {
        let src = "34877539875349573940";
        let mut lex = crate::parser::tokens::Token::lexer(src);
        let res = lex.next().unwrap();
        assert!(res.is_err());
        println!("got expected error: {res:?}");
    }

    #[test]
    fn test_eol() {
        let src = "A\nB\r\nC";
        let mut lex = crate::parser::tokens::Token::lexer(src);

        assert_eq!(
            Token::Identifier("A".to_string()),
            lex.next().unwrap().unwrap()
        );
        assert_eq!(Token::Eol, lex.next().unwrap().unwrap());
        assert_eq!(
            Token::Identifier("B".to_string()),
            lex.next().unwrap().unwrap()
        );
        assert_eq!(Token::Eol, lex.next().unwrap().unwrap());
        assert_eq!(
            Token::Identifier("C".to_string()),
            lex.next().unwrap().unwrap()
        );
    }
}
