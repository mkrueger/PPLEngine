use std::path::PathBuf;

use crate::{
    ast::{
        BreakStatement, CaseBlock, CaseSpecifier, CommentAstNode, Constant, ConstantExpression,
        ContinueStatement, ElseBlock, ElseIfBlock, EndStatement, ForStatement, GosubStatement,
        GotoStatement, IdentifierExpression, IfStatement, IfThenStatement, LabelStatement,
        PredefinedCallStatement, ReturnStatement, SelectStatement, Statement, UnaryExpression,
        UnaryOp, WhileDoStatement, WhileStatement,
    },
    executable::OpCode,
};

use super::{Encoding, Parser};

fn parse_statement(input: &str, assert_eof: bool) -> Statement {
    let mut parser = Parser::new(PathBuf::from("."), input, Encoding::Utf8);
    parser.next_token();
    let res = parser.parse_statement().unwrap();
    if assert_eof {
        assert!(parser.get_cur_token().is_none());
    }
    res
}

fn check_statement(input: &str, check: &Statement) {
    let stmt = parse_statement(input, true);

    if !stmt.is_similar(check) {
        println!("Statement {stmt:?} is not similar to {check:?}");

        println!("was:\n{stmt}\nShould be:\n{check}");

        panic!();
    }
}
fn check_statement_without_eol(input: &str, check: &Statement) {
    let stmt = parse_statement(input, false);

    if !stmt.is_similar(check) {
        println!("Statement {stmt:?} is not similar to {check:?}");

        println!("was:\n{stmt}\nShould be:\n{check}");

        panic!();
    }
}

#[test]
fn test_parse_comment_statement() {
    check_statement(";FOO", &CommentAstNode::create_empty_statement("FOO"));
}

#[test]
fn test_parse_end_statement() {
    check_statement("END", &EndStatement::create_empty_statement());
}

#[test]
fn test_parse_return_statement() {
    check_statement("RETURN", &ReturnStatement::create_empty_statement());
}

#[test]
fn test_label_statement() {
    check_statement(
        ":MyLabel",
        &LabelStatement::create_empty_statement(unicase::Ascii::new("MyLabel".to_string())),
    );

    check_statement(
        ":END",
        &LabelStatement::create_empty_statement(unicase::Ascii::new("END".to_string())),
    );
}

#[test]
fn test_goto_statement() {
    check_statement(
        "Goto Foo",
        &GotoStatement::create_empty_statement(unicase::Ascii::new("Foo".to_string())),
    );
    check_statement(
        "goto end",
        &GotoStatement::create_empty_statement(unicase::Ascii::new("end".to_string())),
    );
}

#[test]
fn test_gosub_statement() {
    check_statement(
        "GOSUB Foo",
        &GosubStatement::create_empty_statement(unicase::Ascii::new("Foo".to_string())),
    );
    check_statement(
        "GOSUB end",
        &GosubStatement::create_empty_statement(unicase::Ascii::new("end".to_string())),
    );
}

#[test]
fn test_parse_break_statement() {
    check_statement("BREAK", &BreakStatement::create_empty_statement());
    // Alias
    check_statement("QUIT", &BreakStatement::create_empty_statement());
}

#[test]
fn test_parse_continue_statement() {
    check_statement("CONTINUE", &ContinueStatement::create_empty_statement());

    // Alias
    check_statement("LOOP", &ContinueStatement::create_empty_statement());
}

#[test]
fn test_if_statement() {
    check_statement(
        "if (A) BREAK",
        &IfStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            BreakStatement::create_empty_statement(),
        ),
    );
}

#[test]
fn test_if_then_statement() {
    check_statement(
        r"if (A) THEN
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![],
            None,
        ),
    );

    check_statement(
        r"if (A) THEN
        
        ENDIF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![],
            None,
        ),
    );

    check_statement(
        r"if (A) THEN
        BREAK
        RETURN
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![
                BreakStatement::create_empty_statement(),
                ReturnStatement::create_empty_statement(),
            ],
            vec![],
            None,
        ),
    );

    check_statement(
        r"if (A) THEN
        CONTINUE
        :FOO
        ENDIF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![
                ContinueStatement::create_empty_statement(),
                LabelStatement::create_empty_statement(unicase::Ascii::new("FOO".to_string())),
            ],
            vec![],
            None,
        ),
    );
}

#[test]
fn test_if_do_statement() {
    check_statement(
        r"if (A) DO
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![],
            None,
        ),
    );
    check_statement(
        r"if (A) DO ;COMMENT
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![],
            None,
        ),
    );
}

#[test]
fn test_if_then_else_statement() {
    check_statement(
        r"if (A) THEN
        ELSE
            BREAK
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![],
            Some(ElseBlock::empty(vec![
                BreakStatement::create_empty_statement(),
            ])),
        ),
    );
}

#[test]
fn test_if_then_ifelse_statement() {
    check_statement(
        r"if (A) THEN
        ELSEIF (B) THEN
            CONTINUE
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![ElseIfBlock::empty(
                IdentifierExpression::create_empty_expression(unicase::Ascii::new("B".to_string())),
                vec![ContinueStatement::create_empty_statement()],
            )],
            None,
        ),
    );

    check_statement(
        r"if (A) THEN
        ELSE IF (B) THEN
            CONTINUE
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![ElseIfBlock::empty(
                IdentifierExpression::create_empty_expression(unicase::Ascii::new("B".to_string())),
                vec![ContinueStatement::create_empty_statement()],
            )],
            None,
        ),
    );
}

#[test]
fn test_if_then_ifelse_withoutthen_statement() {
    check_statement(
        r"if (A) THEN
        ELSEIF (B)
            CONTINUE
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![ElseIfBlock::empty(
                IdentifierExpression::create_empty_expression(unicase::Ascii::new("B".to_string())),
                vec![ContinueStatement::create_empty_statement()],
            )],
            None,
        ),
    );

    check_statement(
        r"if (A) THEN
        ELSE IF (B)
            CONTINUE
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![ElseIfBlock::empty(
                IdentifierExpression::create_empty_expression(unicase::Ascii::new("B".to_string())),
                vec![ContinueStatement::create_empty_statement()],
            )],
            None,
        ),
    );
}

#[test]
fn test_if_then_ifelse_else_statement() {
    check_statement(
        r"if (A) THEN
        ELSEIF (B) THEN
            CONTINUE
        ELSE
            BREAK
        END IF",
        &IfThenStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![ElseIfBlock::empty(
                IdentifierExpression::create_empty_expression(unicase::Ascii::new("B".to_string())),
                vec![ContinueStatement::create_empty_statement()],
            )],
            Some(ElseBlock::empty(vec![
                BreakStatement::create_empty_statement(),
            ])),
        ),
    );
}

#[test]
fn test_while_statement() {
    check_statement(
        "while (A) BREAK",
        &WhileStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            BreakStatement::create_empty_statement(),
        ),
    );
}

#[test]
fn test_while_do_statement() {
    check_statement(
        r"WHILE (A) DO
        END WHILE",
        &WhileDoStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
        ),
    );

    check_statement(
        r"WHILE (A) DO
        
        ENDWHILE",
        &WhileDoStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
        ),
    );

    check_statement(
        r"WHILE (A) DO
        BREAK
        RETURN
        END WHILE",
        &WhileDoStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![
                BreakStatement::create_empty_statement(),
                ReturnStatement::create_empty_statement(),
            ],
        ),
    );

    check_statement(
        r"WHILE (A) DO
        CONTINUE
        :FOO
        ENDWHILE",
        &WhileDoStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![
                ContinueStatement::create_empty_statement(),
                LabelStatement::create_empty_statement(unicase::Ascii::new("FOO".to_string())),
            ],
        ),
    );
}

#[test]
fn test_empty_select_statement() {
    check_statement(
        r"SELECT CASE A
ENDSELECT",
        &SelectStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![],
        ),
    );
    check_statement(
        r"SELECT CASE A
END SELECT",
        &SelectStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![],
        ),
    );
}

#[test]
fn test_select_statement() {
    check_statement(
        r"SELECT CASE A
CASE 1
BREAK
CASE 1, 2, 3
BREAK
ENDSELECT",
        &SelectStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![
                CaseBlock::empty(
                    vec![CaseSpecifier::Expression(Box::new(
                        ConstantExpression::create_empty_expression(Constant::Integer(1)),
                    ))],
                    vec![BreakStatement::create_empty_statement()],
                ),
                CaseBlock::empty(
                    vec![
                        CaseSpecifier::Expression(Box::new(
                            ConstantExpression::create_empty_expression(Constant::Integer(1)),
                        )),
                        CaseSpecifier::Expression(Box::new(
                            ConstantExpression::create_empty_expression(Constant::Integer(2)),
                        )),
                        CaseSpecifier::Expression(Box::new(
                            ConstantExpression::create_empty_expression(Constant::Integer(3)),
                        )),
                    ],
                    vec![BreakStatement::create_empty_statement()],
                ),
                /*        CaseBlock::empty(
                vec![CaseSpecifier::FromTo(Box::new(ConstantExpression::create_empty_expression(Constant::Integer(1))), Box::new(ConstantExpression::create_empty_expression(Constant::Integer(3))))],
                vec![BreakStatement::create_empty_statement()]),*/
            ],
            vec![],
        ),
    );
}

#[test]
fn test_select_multiple_case_specifiers_statement() {
    check_statement(
        r"SELECT CASE A
CASE 1, 2, 3
BREAK
ENDSELECT",
        &SelectStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![
                CaseBlock::empty(
                    vec![
                        CaseSpecifier::Expression(Box::new(
                            ConstantExpression::create_empty_expression(Constant::Integer(1)),
                        )),
                        CaseSpecifier::Expression(Box::new(
                            ConstantExpression::create_empty_expression(Constant::Integer(2)),
                        )),
                        CaseSpecifier::Expression(Box::new(
                            ConstantExpression::create_empty_expression(Constant::Integer(3)),
                        )),
                    ],
                    vec![BreakStatement::create_empty_statement()],
                ),
                /*        CaseBlock::empty(
                vec![CaseSpecifier::FromTo(Box::new(ConstantExpression::create_empty_expression(Constant::Integer(1))), Box::new(ConstantExpression::create_empty_expression(Constant::Integer(3))))],
                vec![BreakStatement::create_empty_statement()]),*/
            ],
            vec![],
        ),
    );
}

#[test]
fn test_select_from_to_case_specifiers_statement() {
    check_statement(
        r"SELECT CASE A
CASE 1..3
BREAK
ENDSELECT",
        &SelectStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![CaseBlock::empty(
                vec![CaseSpecifier::FromTo(
                    Box::new(ConstantExpression::create_empty_expression(
                        Constant::Integer(1),
                    )),
                    Box::new(ConstantExpression::create_empty_expression(
                        Constant::Integer(3),
                    )),
                )],
                vec![BreakStatement::create_empty_statement()],
            )],
            vec![],
        ),
    );
}

#[test]
fn test_case_default_statement() {
    check_statement(
        r"SELECT CASE A
DEFAULT
BREAK
ENDSELECT",
        &SelectStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![BreakStatement::create_empty_statement()],
        ),
    );

    check_statement(
        r"SELECT CASE A
CASE ELSE
BREAK
ENDSELECT",
        &SelectStatement::create_empty_statement(
            IdentifierExpression::create_empty_expression(unicase::Ascii::new("A".to_string())),
            vec![],
            vec![BreakStatement::create_empty_statement()],
        ),
    );
}

#[test]
fn test_predefined_call() {
    check_statement(
        "PRINTLN",
        &PredefinedCallStatement::create_empty_statement(
            OpCode::PRINTLN.get_definition(),
            Vec::new(),
        ),
    );
    check_statement_without_eol(
        "PRINTLN ;COMMENT",
        &PredefinedCallStatement::create_empty_statement(
            OpCode::PRINTLN.get_definition(),
            Vec::new(),
        ),
    );
}

#[test]
fn test_for_statement() {
    check_statement(
        r"FOR I = 0 TO 5 
NEXT",
        &ForStatement::create_empty_statement(
            unicase::Ascii::new("I".to_string()),
            ConstantExpression::create_empty_expression(Constant::Integer(0)),
            ConstantExpression::create_empty_expression(Constant::Integer(5)),
            None,
            vec![],
        ),
    );

    check_statement(
        r"FOR I = 0 TO 5 
NEXT I",
        &ForStatement::create_empty_statement(
            unicase::Ascii::new("I".to_string()),
            ConstantExpression::create_empty_expression(Constant::Integer(0)),
            ConstantExpression::create_empty_expression(Constant::Integer(5)),
            None,
            vec![],
        ),
    );
}

#[test]
fn test_for_statement_alt_next() {
    check_statement(
        r"FOR I = 0 TO 5 
ENDFOR",
        &ForStatement::create_empty_statement(
            unicase::Ascii::new("I".to_string()),
            ConstantExpression::create_empty_expression(Constant::Integer(0)),
            ConstantExpression::create_empty_expression(Constant::Integer(5)),
            None,
            vec![],
        ),
    );

    check_statement(
        r"FOR I = 0 TO 5 
END FOR",
        &ForStatement::create_empty_statement(
            unicase::Ascii::new("I".to_string()),
            ConstantExpression::create_empty_expression(Constant::Integer(0)),
            ConstantExpression::create_empty_expression(Constant::Integer(5)),
            None,
            vec![],
        ),
    );
}

#[test]
fn test_for_step_statement() {
    check_statement(
        r"FOR I = 0 TO 5 STEP 3
NEXT",
        &ForStatement::create_empty_statement(
            unicase::Ascii::new("I".to_string()),
            ConstantExpression::create_empty_expression(Constant::Integer(0)),
            ConstantExpression::create_empty_expression(Constant::Integer(5)),
            Some(Box::new(ConstantExpression::create_empty_expression(
                Constant::Integer(3),
            ))),
            vec![],
        ),
    );

    check_statement(
        r"FOR I = 5 TO 0 STEP -4
NEXT I",
        &ForStatement::create_empty_statement(
            unicase::Ascii::new("I".to_string()),
            ConstantExpression::create_empty_expression(Constant::Integer(5)),
            ConstantExpression::create_empty_expression(Constant::Integer(0)),
            Some(Box::new(UnaryExpression::create_empty_expression(
                UnaryOp::Minus,
                ConstantExpression::create_empty_expression(Constant::Integer(4)),
            ))),
            vec![],
        ),
    );
}
