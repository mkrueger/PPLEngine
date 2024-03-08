use crate::{
    ast::Constant,
    tables::{PPL_FALSE, PPL_TRUE},
};
use core::fmt;
use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum Token {
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

    #[token("true", |_| Constant::Integer(PPL_TRUE), ignore(case))]
    #[token("false", |_| Constant::Integer(PPL_FALSE), ignore(case))]
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| Constant::String(lex.slice()[1..lex.slice().len() - 1].to_string()))]
    #[regex(r"@[xX][0-9a-fA-F][0-9a-fA-F]", |lex| Constant::Integer(i32::from_str_radix(&lex.slice()[2..], 16).unwrap()))]
    #[regex(r"[0-9][0-9a-fA-F]*[hH]", |lex| {
        let slice = lex.slice();
        Constant::Integer(i32::from_str_radix(&slice[..slice.len() - 1], 16).unwrap())
    })]
    #[regex(r"[0-7]+[oO]", |lex| {
        let slice = lex.slice();
        Constant::Integer(i32::from_str_radix(&slice[..slice.len() - 1], 8).unwrap())
    })]
    #[regex(r"[01]+[bB]", |lex| {
        let slice = lex.slice();
        Constant::Integer(i32::from_str_radix(&slice[..slice.len() - 1], 2).unwrap())
    })]
    #[regex(r"\d+[dD]?", |lex| {
        let slice = lex.slice();
        let len = if slice.ends_with('d') || slice.ends_with('D') { slice.len() - 1 } else {  slice.len() };
        Constant::Integer(slice[..len].parse::<i32>().unwrap())
    })]
    #[regex(r"\$\d+(\.\d+)?", |lex| Constant::Money(lex.slice()[1..].parse::<f64>().unwrap()))]
    #[regex(r"(?:0|[1-9]\d*)(?:(?:[eE][+-]?\d+)|\.\d+(?:[eE][+-]?\d+)?)", |lex| Constant::Real(lex.slice().parse::<f64>().unwrap()))]
    Const(Constant),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]+", |lex| lex.slice().to_string(), priority = 5)]
    Identifier(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Const(c) => write!(f, "{}", c),
            Token::Identifier(s) => write!(f, "{}", s),
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

            Token::Label(s) => write!(f, ":{}", s),

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
        }
    }
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use crate::{
        ast::Constant,
        parser::tokens::Token,
        tables::{PPL_FALSE, PPL_TRUE},
    };

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
        /*
        let mut token = Tokenizer::new("Hello World");
        assert_eq!(
            Token::Identifier("HELLO".to_string()),
            token.next_token().unwrap()
        );
        assert_eq!(
            Token::Identifier("WORLD".to_string()),
            token.next_token().unwrap()
        );*/
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
        assert_eq!(Token::Const(Constant::Integer(PPL_TRUE)), get_token("TRUE"));
        assert_eq!(
            Token::Const(Constant::Integer(PPL_FALSE)),
            get_token("FALSE")
        );
    }
}
