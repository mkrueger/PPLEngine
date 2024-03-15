use std::path::PathBuf;

use crate::{
    ast::Constant,
    parser::lexer::{CommentType, Lexer, Token},
};

#[test]
fn test_comments() {
    assert_eq!(
        get_token("; COMMENT"),
        Token::Comment(CommentType::SingleLineSemicolon, " COMMENT".to_string())
    );
    assert_eq!(
        get_token("' COMMENT"),
        Token::Comment(CommentType::SingleLineQuote, " COMMENT".to_string())
    );
    assert_eq!(
        get_token("* COMMENT"),
        Token::Comment(CommentType::SingleLineStar, " COMMENT".to_string())
    );
    assert_eq!(get_token("End ; COMMENT"), Token::End);
}

fn get_token(src: &str) -> Token {
    let mut lex = Lexer::new(PathBuf::from("."), src);
    match lex.next_token().unwrap() {
        Ok(t) => {
            //println!("got token: {t:?}");
            t
        }
        Err(e) => {
            println!("error parsing '{src}'");
            panic!("Error: {e:?}")
        }
    }
}

#[test]
fn test_string() {
    assert_eq!(
        Token::Const(Constant::String(String::new())),
        get_token("\"\"")
    );
    assert_eq!(
        Token::Const(Constant::String("\\".to_string())),
        get_token("\"\\\"")
    );

    assert_eq!(
        Token::Const(Constant::String("\"foo\"".to_string())),
        get_token("\"\"\"foo\"\"\"")
    );

    let src = "\"Hello World\" \"foo\"";
    let mut lex = Lexer::new(PathBuf::from("."), src);

    assert_eq!(
        Token::Const(Constant::String("Hello World".to_string())),
        lex.next_token().unwrap().unwrap()
    );
    assert_eq!(
        Token::Const(Constant::String("foo".to_string())),
        lex.next_token().unwrap().unwrap()
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
    //assert_eq!(Token::PoW, get_token("**"));
    assert_eq!(Token::PoW, get_token("^"));

    //assert_eq!(Token::Mul, get_token("*"));
    assert_eq!(Token::Div, get_token("/"));
    assert_eq!(Token::Add, get_token("+"));
    assert_eq!(Token::Sub, get_token("-"));

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
    assert_eq!(
        Token::Identifier(unicase::Ascii::new("PRINT".to_string())),
        get_token("PRINT")
    );

    assert_eq!(
        Token::Identifier(unicase::Ascii::new("_".to_string())),
        get_token("_")
    );
    assert_eq!(
        Token::Identifier(unicase::Ascii::new("_O".to_string())),
        get_token("_O")
    );

    let src = "Hello World";
    let mut lex = Lexer::new(PathBuf::from("."), src);

    assert_eq!(
        Token::Identifier(unicase::Ascii::new("Hello".to_string())),
        lex.next_token().unwrap().unwrap()
    );
    assert_eq!(
        Token::Identifier(unicase::Ascii::new("World".to_string())),
        lex.next_token().unwrap().unwrap()
    );
}

#[test]
fn test_constants() {
    assert_eq!(Token::Const(Constant::Integer(123)), get_token("123"));
    assert_eq!(Token::Const(Constant::Integer(100)), get_token("@X64"));

    assert_eq!(Token::Const(Constant::Money(142)), get_token("$1.42"));
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
    let mut lex = Lexer::new(PathBuf::from("."), src);
    let res = lex.next_token().unwrap();
    assert!(res.is_err());
    println!("got expected error: {res:?}");
}

#[test]
fn test_eol() {
    let src = "A\nB\r\nC";
    let mut lex = Lexer::new(PathBuf::from("."), src);

    assert_eq!(
        Token::Identifier(unicase::Ascii::new("A".to_string())),
        lex.next_token().unwrap().unwrap()
    );
    assert_eq!(Token::Eol, lex.next_token().unwrap().unwrap());
    assert_eq!(
        Token::Identifier(unicase::Ascii::new("B".to_string())),
        lex.next_token().unwrap().unwrap()
    );
    assert_eq!(Token::Eol, lex.next_token().unwrap().unwrap());
    assert_eq!(
        Token::Identifier(unicase::Ascii::new("C".to_string())),
        lex.next_token().unwrap().unwrap()
    );
}

#[test]
fn test_colon_eol() {
    let src = "A:B:C";
    let mut lex = Lexer::new(PathBuf::from("."), src);

    assert_eq!(
        Token::Identifier(unicase::Ascii::new("A".to_string())),
        lex.next_token().unwrap().unwrap()
    );
    assert_eq!(Token::Eol, lex.next_token().unwrap().unwrap());
    assert_eq!(
        Token::Identifier(unicase::Ascii::new("B".to_string())),
        lex.next_token().unwrap().unwrap()
    );
    assert_eq!(Token::Eol, lex.next_token().unwrap().unwrap());
    assert_eq!(
        Token::Identifier(unicase::Ascii::new("C".to_string())),
        lex.next_token().unwrap().unwrap()
    );
}

#[test]
fn test_end_constructs() {
    assert_eq!(Token::EndSelect, get_token("EndSelect"));
    assert_eq!(Token::EndFunc, get_token("ENDFUNC"));
    assert_eq!(Token::EndProc, get_token("ENDPROC"));
}

#[test]
fn test_while() {
    assert_eq!(Token::While, get_token("WHILE"));
    assert_eq!(Token::EndWhile, get_token("ENDWHILE"));
}

#[test]
fn test_break() {
    assert_eq!(Token::Break, get_token("break"));
    assert_eq!(Token::Break, get_token("quit"));
}

#[test]
fn test_continue() {
    assert_eq!(Token::Continue, get_token("continue"));
    assert_eq!(Token::Continue, get_token("loop"));
}

#[test]
fn test_skip() {
    let src = "Hello _\n World";
    let mut lex = Lexer::new(PathBuf::from("."), src);

    assert_eq!(
        Token::Identifier(unicase::Ascii::new("Hello".to_string())),
        lex.next_token().unwrap().unwrap()
    );
    assert_eq!(
        Token::Identifier(unicase::Ascii::new("World".to_string())),
        lex.next_token().unwrap().unwrap()
    );
}
#[test]
fn test_if_then() {
    assert_eq!(Token::If, get_token("IF"));
    assert_eq!(Token::Then, get_token("THEN"));
    assert_eq!(Token::Else, get_token("ELSE"));
    assert_eq!(Token::ElseIf, get_token("ElseIf"));
    assert_eq!(Token::EndIf, get_token("EndIf"));
}

#[test]
fn test_labels() {
    assert_eq!(
        get_token(":label001"),
        Token::Label(unicase::Ascii::new("label001".to_string()))
    );
}
