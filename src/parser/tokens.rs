use crate::{ast::Constant, tables::{PPL_TRUE, PPL_FALSE}};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Eol,
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
}

pub struct Tokenizer {
    pub cur_token: Option<Token>,
    pos: usize,
    text: Vec<char>,
    line: usize,
    col: usize
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

    pub fn next_token(&mut self)-> Option<Token> {
        self.cur_token = self.next_token2();
        self.cur_token.clone()
    }

    fn next_token2(&mut self) -> Option<Token> {
        let ch = self.next_ch();
        if ch.is_none() { 
            return None; 
        }
        let ch = ch.unwrap();
        match ch {
            '\n' => {
                Some(Token::Eol)
            },
            '\'' | // comment
            ';' => {
                while self.get_ch() != Some('\n') && self.get_ch().is_some() {
                    self.next_ch();
                }
                Some(Token::Eol)
            }
            '"' => {
                let mut str_literal = String::new();
                loop {
                    let ch = self.next_ch(); 
                    if ch.is_none() {
                        panic!("Unexpected eof in string_literal at ({}, {}).", self.line, self.col);
                    }
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
                 match next {
                    Some('*') => Some(Token::PoW),
                     _ => {
                         self.put_back();
                         Some(Token::Mul)
                     }
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
                 match next {
                    Some('&') => Some(Token::And),
                     _ => {
                         self.put_back();
                         Some(Token::And)
                     }
                 }
             },
            '|' => {
                let next = self.next_ch();
                 match next {
                    Some('|') => Some(Token::Or),
                     _ => {
                         self.put_back();
                         Some(Token::Or)
                     }
                 }
             },
            '!' => {
                let next = self.next_ch();
                 match next {
                    Some('=') => Some(Token::NotEq),
                     _ => {
                         self.put_back();
                         Some(Token::Not)
                     }
                 }
             },
            '@' => {
                let ch = self.next_ch();
                if Some('X') != ch && Some('x') != ch {
                    panic!("Error in hexadecimal constant should be");
                }
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
                    if !ch.is_digit(10) && ch != '.' { break; }
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
                if ch  == ' ' || ch  == '\t' || ch  == '\r'   {
                    while self.get_ch().is_some() && ( self.get_ch() == Some(' ') ||  self.get_ch() == Some('\t') || self.get_ch() == Some('\r')) {
                        self.next_ch();
                    }
                    if self.get_ch().is_none() { return None; }
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

                if ch.is_digit(10) {
                    let mut identifier = String::new();
                    identifier.push(ch);
                    while self.get_ch().is_some() && self.get_ch().unwrap().is_ascii_alphanumeric() {
                        identifier.push_str(&self.next_ch().unwrap().to_uppercase().to_string());
                    }
                    if identifier.ends_with("D") {
                        identifier.pop();
                        return Some(Token::Const(Constant::Integer(identifier.parse::<i32>().unwrap())));
                    }

                    if identifier.ends_with("H") {
                        identifier.pop();
                        return Some(Token::Const(Constant::Integer(i32::from_str_radix(&identifier, 16).unwrap())));
                    }

                    if identifier.ends_with("O") {
                        identifier.pop();
                        return Some(Token::Const(Constant::Integer(i32::from_str_radix(&identifier, 8).unwrap())));
                    }
                    if identifier.ends_with("B") {
                        identifier.pop();
                        return Some(Token::Const(Constant::Integer(i32::from_str_radix(&identifier, 2).unwrap())));
                    }
                    let r = identifier.parse::<i64>();
                    if let Ok(i) = r {
                        if i <= i32::MAX as i64 && i >= i32::MIN as i64  {
                            return Some(Token::Const(Constant::Integer(i as i32)));
                        } else if i <= u32::MAX as i64 {
                            return Some(Token::Const(Constant::Unsigned(i as u32)));
                        }else {
                            panic!("{i} overflow.");
                        }
                    } else {
                        panic!("can't parse '{}' : {:?}", identifier, r);
                    }
                }
                panic!("invalid char: {}", ch);
            }
        }
    }
}

fn conv_hex(first: char) -> i32 {
    if ('0'..='9').contains(&first) {
        return first as i32 - b'0' as i32;
    }
    if ('a'..='f').contains(&first) {
        return first as i32 - b'a' as i32 + 10;
    }
    if ('A'..='F').contains(&first) {
        return first as i32 - b'A' as i32 + 10;
    }
    panic!("Invalid hex char: {}", first);
}

#[cfg(test)]
mod tests {
    use crate::{parser::tokens::{Tokenizer, Token}, ast::Constant, tables::{PPL_FALSE, PPL_TRUE}};
    
    #[test]
    fn test_comments() {
        assert_eq!(Token::Eol, Tokenizer::new("; COMMENT").next_token().unwrap());
        assert_eq!(Token::Eol, Tokenizer::new("' COMMENT").next_token().unwrap());
    }

    #[test]
    fn test_string() {
        let mut token = Tokenizer::new("\"Hello World\"\"foo\"");
        assert_eq!(Token::Const(Constant::String("Hello World".to_string())), token.next_token().unwrap());
        assert_eq!(Token::Const(Constant::String("foo".to_string())), token.next_token().unwrap());
    }
    
    #[test]
    fn test_op() {
        assert_eq!(Token::Eq, Tokenizer::new("==").next_token().unwrap());
        assert_eq!(Token::Eq, Tokenizer::new("=").next_token().unwrap());

        assert_eq!(Token::And, Tokenizer::new("&&").next_token().unwrap());
        assert_eq!(Token::And, Tokenizer::new("&").next_token().unwrap());
        assert_eq!(Token::Or, Tokenizer::new("||").next_token().unwrap());
        assert_eq!(Token::Or, Tokenizer::new("|").next_token().unwrap());
        assert_eq!(Token::Not, Tokenizer::new("!").next_token().unwrap());
        assert_eq!(Token::PoW, Tokenizer::new("**").next_token().unwrap());
        assert_eq!(Token::PoW, Tokenizer::new("^").next_token().unwrap());

        assert_eq!(Token::NotEq, Tokenizer::new("<>").next_token().unwrap());
        assert_eq!(Token::NotEq, Tokenizer::new("><").next_token().unwrap());
        assert_eq!(Token::NotEq, Tokenizer::new("!=").next_token().unwrap());
        assert_eq!(Token::Lower, Tokenizer::new("<").next_token().unwrap());
        assert_eq!(Token::LowerEq, Tokenizer::new("<=").next_token().unwrap());
        assert_eq!(Token::LowerEq, Tokenizer::new("=<").next_token().unwrap());
        assert_eq!(Token::Greater, Tokenizer::new(">").next_token().unwrap());
        assert_eq!(Token::GreaterEq, Tokenizer::new(">=").next_token().unwrap());
        assert_eq!(Token::GreaterEq, Tokenizer::new("=>").next_token().unwrap());
    }

    #[test]
    fn test_identifier() {
        assert_eq!(Token::Identifier("PRINT".to_string()), Tokenizer::new("PRINT").next_token().unwrap());

        let mut token = Tokenizer::new("Hello World");
        assert_eq!(Token::Identifier("HELLO".to_string()), token.next_token().unwrap());
        assert_eq!(Token::Identifier("WORLD".to_string()), token.next_token().unwrap());
    }

    #[test]
    fn test_constants() {
        assert_eq!(Token::Const(Constant::Integer(123)), Tokenizer::new("123").next_token().unwrap());
        assert_eq!(Token::Const(Constant::Money(1.42)), Tokenizer::new("$1.42").next_token().unwrap());
        assert_eq!(Token::Const(Constant::Integer(255)), Tokenizer::new("0FFh").next_token().unwrap());
        assert_eq!(Token::Const(Constant::Integer(123)), Tokenizer::new("123d").next_token().unwrap());
        assert_eq!(Token::Const(Constant::Integer(88)), Tokenizer::new("130o").next_token().unwrap());
        assert_eq!(Token::Const(Constant::Integer(8)), Tokenizer::new("1000b").next_token().unwrap());
        assert_eq!(Token::Const(Constant::Integer(100)), Tokenizer::new("@X64").next_token().unwrap());
        assert_eq!(Token::Const(Constant::Integer(PPL_TRUE)), Tokenizer::new("TRUE").next_token().unwrap());
        assert_eq!(Token::Const(Constant::Integer(PPL_FALSE)), Tokenizer::new("FALSE").next_token().unwrap());
    }
}