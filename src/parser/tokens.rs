use crate::{
    ast::Constant,
    tables::{PPL_FALSE, PPL_TRUE},
};
use chumsky::prelude::*;
use core::fmt;
use thiserror::Error;

use super::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Const(Constant),
    Identifier(String),
    LPar,
    RPar,
    Comma,
    Colon,
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

    Let,
    End,
    Begin,
    While,
    If,
    Then,
    Else,
    ElseIf,
    EndIf,

    For,
    Break,
    Continue,
    Return,
    Gosub,
    Goto,
    Label(String),
}

/*
                "BREAK" => return Statement::Break,
                "CONTINUE" => return Statement::Continue,
                "RETURN" => return Statement::Return,
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



*/

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Const(c) => write!(f, "{}", c),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::LPar => write!(f, "("),
            Token::RPar => write!(f, ")"),
            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, "."),
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
        }
    }
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("Number overflow {0}")]
    Overflow(String),
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token, Span)>, extra::Err<Rich<'src, char, Span>>> {
    // A parser for numbers
    let digits = text::digits(10).to_slice();
    let frac = just('.').then(digits);
    let num = text::digits(10)
        .ignore_then(text::digits(16).or_not())
        .ignore_then(frac.or_not())
        .then(one_of("hHdDbBoO").or_not())
        .to_slice()
        .map(|identifier: &str| {
            // TODO: Better error handling
            if identifier.ends_with('H') || identifier.ends_with('h') {
                return Token::Const(Constant::Integer(
                    i32::from_str_radix(&identifier[..identifier.len() - 1], 16).unwrap(),
                ));
            }
            if identifier.ends_with('D') || identifier.ends_with('d') {
                return Token::Const(Constant::Integer(
                    identifier[..identifier.len() - 1].parse::<i32>().unwrap(),
                ));
            }
            if identifier.ends_with('O') || identifier.ends_with('o') {
                return Token::Const(Constant::Integer(
                    i32::from_str_radix(&identifier[..identifier.len() - 1], 8).unwrap(),
                ));
            }
            if identifier.ends_with('B') || identifier.ends_with('b') {
                return Token::Const(Constant::Integer(
                    i32::from_str_radix(&identifier[..identifier.len() - 1], 2).unwrap(),
                ));
            }
            let radix = 10;
            let i = i64::from_str_radix(identifier, radix).unwrap();
            if i32::try_from(i).is_ok() {
                return Token::Const(Constant::Integer(i as i32));
            }
            if i <= u32::MAX as i64 {
                return Token::Const(Constant::Unsigned(i as u32));
            }
            panic!("{i} overflow.");
        });

    let label = just(':')
        .ignore_then(text::ascii::ident())
        .map(|identifier: &str| Token::Label(identifier[1..].to_string()));

    // A parser for @X color codes
    let color_code =
        just("@X")
            .then(text::digits(16).exactly(2))
            .to_slice()
            .map(|identifier: &str| {
                Token::Const(Constant::Integer(
                    i32::from_str_radix(&identifier[2..], 16).unwrap(),
                ))
            });

    let money = just("$")
        .ignore_then(text::digits(10))
        .ignore_then(frac.or_not())
        .to_slice()
        .map(|identifier: &str| {
            // TODO: Better error handling
            let i = identifier[1..].parse::<f64>().unwrap();
            Token::Const(Constant::Money(i))
        });

    // A parser for strings
    let str_ = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice()
        .map(|s: &str| Token::Const(Constant::String(s[1..s.len() - 1].to_string())));

    let op_pow = just("**").or(just("*")).map(|s: &str| {
        println!("len:{}", s.len());
        if s.len() == 1 {
            Token::Mul
        } else {
            Token::PoW
        }
    });

    let op_lower_eq1 = just("=<")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|_| Token::LowerEq);
    let op_lower_eq2 = just("<=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|_| Token::LowerEq);

    let op_greater_eq1 = just("=>")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|_| Token::GreaterEq);
    let op_greater_eq2 = just(">=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|_| Token::GreaterEq);

    let op_eq1 = just("==")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|_| Token::Eq);

    let op_not_eq2 = just("<>")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(|_| Token::NotEq);
    let op_not_eq3 = just("><")
        .repeated()
        .exactly(1)
        .to_slice()
        .map(|_| Token::NotEq);
    let op_not_eq = just("!=")
        .repeated()
        .exactly(1)
        .to_slice()
        .map(|_| Token::NotEq);

    let op_and = just("&&")
        .repeated()
        .exactly(1)
        .to_slice()
        .map(|_| Token::And);

    let op_or = just("||")
        .repeated()
        .exactly(1)
        .to_slice()
        .map(|_| Token::Or);

    // A parser for operators
    let op = one_of("(),^/%+-=&|!<>")
        .repeated()
        .exactly(1)
        .collect::<String>()
        .map(|s| match s.as_str() {
            "(" => Token::LPar,
            ")" => Token::RPar,
            "," => Token::Comma,
            "^" => Token::PoW,
            "*" => Token::Mul,
            "/" => Token::Div,
            "%" => Token::Mod,
            "+" => Token::Add,
            "-" => Token::Sub,
            "=" => Token::Eq,
            "<" => Token::Lower,
            ">" => Token::Greater,
            "&" => Token::And,
            "|" => Token::Or,
            "!" => Token::Not,
            _ => panic!("Invalid operator: {s}"),
        });

    let ident = text::ascii::ident().map(|ident: &str| match ident.to_uppercase().as_str() {
        "LET" => Token::Let,
        "TRUE" => Token::Const(Constant::Integer(PPL_TRUE)),
        "FALSE" => Token::Const(Constant::Integer(PPL_FALSE)),
        "END" => Token::End,
        "BEGIN" => Token::Begin,
        "WHILE" => Token::While,
        "IF" => Token::If,
        "THEN" => Token::Then,
        "ELSE" => Token::Else,
        "ELSEIF" => Token::ElseIf,
        "ENDIF" => Token::EndIf,
        "FOR" => Token::For,
        "BREAK" => Token::Break,
        "CONTINUE" => Token::Continue,
        "RETURN" => Token::Return,
        "GOSUB" => Token::Gosub,
        "GOTO" => Token::Goto,
        _ => Token::Identifier(ident.to_string()),
    });

    let comment = one_of(";'")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    let token = num
        .or(str_)
        .or(op_pow)
        .or(op_lower_eq1)
        .or(op_lower_eq2)
        .or(op_greater_eq1)
        .or(op_greater_eq2)
        .or(op_eq1)
        .or(op_not_eq2)
        .or(op_not_eq3)
        .or(op_not_eq)
        .or(op_and)
        .or(op_or)
        .or(op)
        .or(ident)
        .or(label)
        .or(color_code)
        .or(money);

    token
        .map_with(|tok: Token, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

pub struct Tokenizer {
    pub cur_token: Option<Token>,
    pos: usize,
    text: Vec<char>,
    line: usize,
    col: usize,
}

impl Tokenizer {
    pub fn new(text: &str) -> Self {
        Tokenizer {
            cur_token: None,
            pos: 0,
            text: text.chars().collect(),
            line: 0,
            col: 0,
        }
    }

    fn get_ch(&self) -> Option<char> {
        if self.text.len() <= self.pos {
            None
        } else {
            Some(self.text[self.pos])
        }
    }

    fn next_ch(&mut self) -> Option<char> {
        let ch = self.get_ch();
        self.pos += 1;

        self.col += 1;
        if ch == Some('\n') {
            self.col = 0;
            self.line += 1;
        }

        ch
    }

    fn put_back(&mut self) {
        self.pos -= 1;
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.cur_token = self.next_token2();
        self.cur_token.clone()
    }

    fn next_token2(&mut self) -> Option<Token> {
        let ch = self.next_ch();
        ch?;
        let ch = ch.unwrap();
        match ch {

            '\'' | // comment
            ';' => {
                while self.get_ch() != Some('\n') && self.get_ch().is_some() {
                    self.next_ch();
                }
                self.next_token2()
            }
            '"' => {
                let mut str_literal = String::new();
                loop {
                    let ch = self.next_ch();
                    assert!(ch.is_some(), "Unexpected eof in string_literal at ({}, {}).", self.line, self.col);
                    let ch = ch.unwrap();
                    if ch == '"'  { break; }
                    str_literal.push(ch);
                }
                Some(Token::Const(Constant::String(str_literal)))
            }
            ':' => Some(Token::Colon),
            '(' => Some(Token::LPar),
            ')' => Some(Token::RPar),
            ',' => Some(Token::Comma),
            '^' => Some(Token::PoW),
            '*' => {
                let next = self.next_ch();
                 if let Some('*') = next {
                    Some(Token::PoW)
                } else {
                    self.put_back();
                    Some(Token::Mul)
                }
             },
            '/' => Some(Token::Div),
            '%' => Some(Token::Mod),
            '+' => Some(Token::Add),
            '-' => Some(Token::Sub),
            '=' => {
                let next = self.next_ch();
                 match next {
                    Some('<') => Some(Token::LowerEq),
                    Some('>') => Some(Token::GreaterEq),
                    Some('=') => Some(Token::Eq),
                     _ => {
                         self.put_back();
                         Some(Token::Eq)
                     }
                 }
             },
            '&'  => {
                let next = self.next_ch();
                 if let Some('&') = next {
                    Some(Token::And)
                } else {
                                         self.put_back();
                                         Some(Token::And)
                                     }
             },
            '|' => {
                let next = self.next_ch();
                 if let Some('|') = next {
                    Some(Token::Or)
                } else {
                                         self.put_back();
                                         Some(Token::Or)
                                     }
             },
            '!' => {
                let next = self.next_ch();
                 if let Some('=') = next {
                    Some(Token::NotEq)
                } else {
                                         self.put_back();
                                         Some(Token::Not)
                                     }
             },
            '@' => {
                let ch = self.next_ch();
                assert!(!(Some('X') != ch && Some('x') != ch), "Error in hexadecimal constant should be");
                let first = self.next_ch().unwrap();
                let second = self.next_ch().unwrap();

                Some(Token::Const(Constant::Integer(conv_hex(first) * 16 + conv_hex(second))))
            }
            '$' => {
                let mut identifier = String::new();
                loop {
                    let ch = self.next_ch();
                    if ch.is_none() { break; }
                    let ch = ch.unwrap();
                    if !ch.is_ascii_digit() && ch != '.' { break; }
                    identifier.push(ch);
                }
                self.put_back();
                Some(Token::Const(Constant::Money(identifier.parse::<f64>().unwrap())))
            }

            '<' => {
                let next = self.next_ch();
                match next {
                    Some('>') => Some(Token::NotEq),
                    Some('=') => Some(Token::LowerEq),
                    _ => {
                        self.put_back();
                        Some(Token::Lower)
                    }
                }
            },
            '>' => {
                let next = self.next_ch();
                 match next {
                     Some('<') => Some(Token::NotEq),
                     Some('=') => Some(Token::GreaterEq),
                     _ => {
                         self.put_back();
                         Some(Token::Greater)
                     }
                 }
             }
            _ => {
                if ch  == ' ' || ch  == '\t' || ch  == '\r'  || ch  == '\n'   {
                    while self.get_ch().is_some() && ( self.get_ch() == Some(' ') ||  self.get_ch() == Some('\t') || self.get_ch() == Some('\r')) {
                        self.next_ch();
                    }
                    self.get_ch()?;
                    return self.next_token();
                }

                if ch.is_alphabetic() || ch == '_' {
                    let mut identifier = String::new();
                    identifier.push_str(&ch.to_uppercase().to_string());
                    while self.get_ch().is_some() && (self.get_ch().unwrap().is_alphanumeric() || self.get_ch().unwrap() == '_') {
                        identifier.push_str(&self.next_ch().unwrap().to_uppercase().to_string());
                    }

                    match identifier.as_str() {
                        "TRUE" => return Some(Token::Const(Constant::Integer(PPL_TRUE))),
                        "FALSE" => return Some(Token::Const(Constant::Integer(PPL_FALSE))),
                        _ => {}
                    }
                    // Handle predefined constants
                    for cnst in &crate::tables::CONSTANT_VALUES {
                        if cnst.0 == identifier {
                            return Some(Token::Const(Constant::Builtin(cnst.0)));
                        }
                    }

                    return Some(Token::Identifier(identifier));
                }

                if ch.is_ascii_digit() {
                    let mut identifier = String::new();
                    identifier.push(ch);
                    while self.get_ch().is_some() && self.get_ch().unwrap().is_ascii_alphanumeric() {
                        identifier.push_str(&self.next_ch().unwrap().to_uppercase().to_string());
                    }
                    if identifier.ends_with('D') {
                        identifier.pop();
                        return Some(Token::Const(Constant::Integer(identifier.parse::<i32>().unwrap())));
                    }

                    if identifier.ends_with('H') {
                        identifier.pop();
                        return Some(Token::Const(Constant::Integer(i32::from_str_radix(&identifier, 16).unwrap())));
                    }

                    if identifier.ends_with('O') {
                        identifier.pop();
                        return Some(Token::Const(Constant::Integer(i32::from_str_radix(&identifier, 8).unwrap())));
                    }
                    if identifier.ends_with('B') {
                        identifier.pop();
                        return Some(Token::Const(Constant::Integer(i32::from_str_radix(&identifier, 2).unwrap())));
                    }
                    let r = identifier.parse::<i64>();
                    if let Ok(i) = r {
                        if i32::try_from(i).is_ok()  {
                            return Some(Token::Const(Constant::Integer(i as i32)));
                        }
                        if i <= u32::MAX as i64 {
                            return Some(Token::Const(Constant::Unsigned(i as u32)));
                        }
                        panic!("{i} overflow.");
                    }
                    panic!("can't parse '{identifier}' : {r:?}");
                }
                panic!("invalid char: {ch}");
            }
        }
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
    panic!("Invalid hex char: {first}");
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;

    use crate::{
        ast::Constant,
        parser::tokens::{lexer, Token},
        tables::{PPL_FALSE, PPL_TRUE},
    };
    /*
    #[test]
    fn test_comments() {
        assert_eq!(
            Token::Eol,
            get_token("; COMMENT")
        );
        assert_eq!(
            Token::Eol,
            get_token("' COMMENT")
        );
    }*/

    fn get_token(src: &str) -> Token {
        let res = lexer().parse(src);
        assert_eq!(0, res.errors().len());
        let tokens = res.output().unwrap();
        assert_eq!(1, tokens.len());
        tokens[0].0.clone()
    }

    #[test]
    fn test_string() {
        let src = "\"Hello World\"\"foo\"";
        let res = lexer().parse(src);
        assert_eq!(0, res.errors().len());
        let tokens = res.output().unwrap();
        assert_eq!(
            Token::Const(Constant::String("Hello World".to_string())),
            tokens[0].0
        );
        assert_eq!(
            Token::Const(Constant::String("foo".to_string())),
            tokens[1].0
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
