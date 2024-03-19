/*  use super::*;

#[test]
fn test_procedure() {
    let prg = get_ast("Procedure Proc() PRINT 5 EndProc");
    assert_eq!(1, prg.procedure_implementations.len());
}

#[test]
fn test_function() {
    let prg = get_ast("Function Func() BOOLEAN PRINT 5 EndFunc");
    assert_eq!(1, prg.function_implementations.len());
}*/

use std::path::PathBuf;

use crate::ast::{AstNode, Program, VariableDeclarationStatement};

use super::{lexer::SpannedToken, parse_program};

/*
Test cases:
BOOLEAN forward
forward  = FALSE
(forward is a function as well)

*/
#[test]
fn test_var_declarations() {
    let prg = get_ast("BOOLEAN VAR001");
/*
    check(
        prg,
        vec![
            AstNode::Statement(
                VariableDeclarationStatement::new(
                    Id("BOOLEAN", 0..)
                    SpannedToken::new(, span)
                    VariableType::Boolean, "VAR001".to_string()
                ),
            ),
        ]
    );*/


    /*
    let prg = get_ast("INTEGER VAR001");
    assert_eq!(
        Declaration::create_variable(VariableType::Integer, "VAR001".to_string()),
        prg.declarations[0]
    );
    let prg = get_ast("DATE VAR001");
    assert_eq!(
        Declaration::create_variable(VariableType::Date, "VAR001".to_string()),
        prg.declarations[0]
    );
    let prg = get_ast("STRING VAR001");
    assert_eq!(
        Declaration::create_variable(VariableType::String, "VAR001".to_string()),
        prg.declarations[0]
    );
    let prg = get_ast("MONEY VAR001");
    assert_eq!(
        Declaration::create_variable(VariableType::Money, "VAR001".to_string()),
        prg.declarations[0]
    );
    let prg = get_ast("BYTE VAR001");
    assert_eq!(
        Declaration::create_variable(VariableType::Byte, "VAR001".to_string()),
        prg.declarations[0]
    );
    let prg = get_ast("SBYTE VAR001");
    assert_eq!(
        Declaration::create_variable(VariableType::SByte, "VAR001".to_string()),
        prg.declarations[0]
    );
    let prg = get_ast("WORD VAR001");
    assert_eq!(
        Declaration::create_variable(VariableType::Word, "VAR001".to_string()),
        prg.declarations[0]
    );
    let prg = get_ast("SWORD VAR001");
    assert_eq!(
        Declaration::create_variable(VariableType::SWord, "VAR001".to_string()),
        prg.declarations[0]
    );

    let prg = get_ast("INTEGER VAR001(5)");
    assert_eq!(
        Declaration::create_variable1(VariableType::Integer, "VAR001".to_string(), 5),
        prg.declarations[0]
    );
    let prg = get_ast("INTEGER VAR001(5, 10)");
    assert_eq!(
        Declaration::create_variable2(VariableType::Integer, "VAR001".to_string(), 5, 10),
        prg.declarations[0]
    );
    let prg = get_ast("INTEGER VAR001(5, 10, 42)");
    assert_eq!(
        Declaration::create_variable3(VariableType::Integer, "VAR001".to_string(), 5, 10, 42),
        prg.declarations[0]
    );*/
}

fn get_ast(source: &str) -> Program {
    parse_program(PathBuf::from("."), source)
}

/*
#[test]
fn test_func_declarations() {
    let prg = get_ast("DECLARE PROCEDURE PROC001()");
    assert_eq!(
        Declaration::Procedure("PROC001".to_string(), vec![]),
        prg.declarations[0]
    );

    let prg = get_ast("DECLARE FUNCTION FUNC001(INTEGER LOC001) INTEGER");
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

