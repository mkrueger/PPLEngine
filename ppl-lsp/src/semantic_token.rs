use icy_ppe::{
    ast::{Constant, Program},
    parser::tokens::Token,
};
use tower_lsp::lsp_types::SemanticTokenType;

use crate::ImCompleteSemanticToken;

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
];

pub fn semantic_token_from_ast(ast: &Program) -> Vec<ImCompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    ast.main_block.statements.iter().for_each(|stmt| {
        highlight_statements(&mut semantic_tokens, stmt);
    });
    semantic_tokens
}

fn highlight_statements(
    semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
    stmt: &icy_ppe::ast::Statement,
) {
    match stmt {
        icy_ppe::ast::Statement::Comment(cmt_stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: cmt_stmt.get_comment_token().span.start,
                length: cmt_stmt.get_comment_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::COMMENT)
                    .unwrap(),
            });
        }

        icy_ppe::ast::Statement::Goto(goto_stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: goto_stmt.get_goto_token().span.start,
                length: goto_stmt.get_goto_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::End(stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: stmt.get_end_token().span.start,
                length: stmt.get_end_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::Block(blk) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: blk.get_begin_token().span.start,
                length: blk.get_begin_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            blk.get_statements().iter().for_each(|stmt| {
                highlight_statements(semantic_tokens, stmt);
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: blk.get_end_token().span.start,
                length: blk.get_end_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::If(if_stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: if_stmt.get_if_token().span.start,
                length: if_stmt.get_if_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            highlight_expressions(semantic_tokens, if_stmt.get_condition());
            highlight_statements(semantic_tokens, if_stmt.get_statement());
        }
        icy_ppe::ast::Statement::IfThen(if_then_stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: if_then_stmt.get_if_token().span.start,
                length: if_then_stmt.get_if_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: if_then_stmt.get_then_token().span.start,
                length: if_then_stmt.get_then_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            highlight_expressions(semantic_tokens, if_then_stmt.get_condition());
            if_then_stmt.get_statements().iter().for_each(|stmt| {
                highlight_statements(semantic_tokens, stmt);
            });

            for else_if_block in if_then_stmt.get_else_if_blocks() {
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: else_if_block.get_elseif_token().span.start,
                    length: else_if_block.get_elseif_token().span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                });
                highlight_expressions(semantic_tokens, else_if_block.get_condition());
                else_if_block.get_statements().iter().for_each(|stmt| {
                    highlight_statements(semantic_tokens, stmt);
                });
            }

            if let Some(else_block) = if_then_stmt.get_else_block() {
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: else_block.get_else_token().span.start,
                    length: else_block.get_else_token().span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                });
                else_block.get_statements().iter().for_each(|stmt| {
                    highlight_statements(semantic_tokens, stmt);
                });
            }

            semantic_tokens.push(ImCompleteSemanticToken {
                start: if_then_stmt.get_endif_token().span.start,
                length: if_then_stmt.get_endif_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::Select(select_stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: select_stmt.get_select_token().span.start,
                length: select_stmt.get_select_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: select_stmt.get_case_token().span.start,
                length: select_stmt.get_case_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            highlight_expressions(semantic_tokens, select_stmt.get_expr());

            select_stmt.get_case_blocks().iter().for_each(|case| {
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: case.get_case_token().span.start,
                    length: case.get_case_token().span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                });
                highlight_expressions(semantic_tokens, case.get_expr());

                case.get_statements().iter().for_each(|stmt| {
                    highlight_statements(semantic_tokens, stmt);
                });
            });

            if let Some(case_else) = select_stmt.get_case_else_block() {
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: case_else.get_case_token().span.start,
                    length: case_else.get_case_token().span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                });
                // todo "else"
                case_else.get_statements().iter().for_each(|stmt| {
                    highlight_statements(semantic_tokens, stmt);
                });
            }

            semantic_tokens.push(ImCompleteSemanticToken {
                start: select_stmt.get_endselect_token().span.start,
                length: select_stmt.get_endselect_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::While(while_stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: while_stmt.get_while_token().span.start,
                length: while_stmt.get_while_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            highlight_expressions(semantic_tokens, while_stmt.get_condition());
            highlight_statements(semantic_tokens, while_stmt.get_statement());
        }
        icy_ppe::ast::Statement::WhileDo(while_do_stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: while_do_stmt.get_while_token().span.start,
                length: while_do_stmt.get_while_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            highlight_expressions(semantic_tokens, while_do_stmt.get_condition());
            semantic_tokens.push(ImCompleteSemanticToken {
                start: while_do_stmt.get_do_token().span.start,
                length: while_do_stmt.get_do_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });

            while_do_stmt.get_statements().iter().for_each(|stmt| {
                highlight_statements(semantic_tokens, stmt);
            });

            semantic_tokens.push(ImCompleteSemanticToken {
                start: while_do_stmt.get_endwhile_token().span.start,
                length: while_do_stmt.get_endwhile_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::For(stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: stmt.get_for_token().span.start,
                length: stmt.get_for_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            semantic_tokens.push(ImCompleteSemanticToken {
                start: stmt.get_to_token().span.start,
                length: stmt.get_to_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
            highlight_expressions(semantic_tokens, stmt.get_start_expr());

            if let Some(step) = stmt.get_step_token() {
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: step.span.start,
                    length: step.span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                });
                highlight_expressions(semantic_tokens, stmt.get_step_expr().as_ref().unwrap());
            }
            stmt.get_statements().iter().for_each(|stmt| {
                highlight_statements(semantic_tokens, stmt);
            });

            semantic_tokens.push(ImCompleteSemanticToken {
                start: stmt.get_next_token().span.start,
                length: stmt.get_next_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }

        icy_ppe::ast::Statement::Break(stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: stmt.get_break_token().span.start,
                length: stmt.get_break_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::Continue(stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: stmt.get_continue_token().span.start,
                length: stmt.get_continue_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::Gosub(stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: stmt.get_gosub_token().span.start,
                length: stmt.get_gosub_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::Return(stmt) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: stmt.get_return_token().span.start,
                length: stmt.get_return_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::KEYWORD)
                    .unwrap(),
            });
        }
        icy_ppe::ast::Statement::Let(let_stmt) => {
            if let Some(let_token) = let_stmt.get_let_token() {
                semantic_tokens.push(ImCompleteSemanticToken {
                    start: let_token.span.start,
                    length: let_token.span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &SemanticTokenType::KEYWORD)
                        .unwrap(),
                });
            }
            highlight_expressions(semantic_tokens, let_stmt.get_value_expression());
        }
        icy_ppe::ast::Statement::Label(_) => {}
        icy_ppe::ast::Statement::Call(call) => {
            for arg in call.get_arguments() {
                highlight_expressions(semantic_tokens, arg);
            }
        }
        icy_ppe::ast::Statement::PredifinedCall(call) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: call.get_identifier_token().span.start,
                length: call.get_identifier_token().span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::FUNCTION)
                    .unwrap(),
            });
            for arg in call.get_arguments() {
                highlight_expressions(semantic_tokens, arg);
            }
        }
    }
}

fn highlight_expressions(
    semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
    expr: &icy_ppe::ast::Expression,
) {
    match expr {
        icy_ppe::ast::Expression::Identifier(_) => {}
        icy_ppe::ast::Expression::Const(const_expr) => {
            if let Token::Const(constant) = &const_expr.get_constant_token().token {
                match constant {
                    Constant::String(_) => {
                        semantic_tokens.push(ImCompleteSemanticToken {
                            start: const_expr.get_constant_token().span.start,
                            length: const_expr.get_constant_token().span.len(),
                            token_type: LEGEND_TYPE
                                .iter()
                                .position(|item| item == &SemanticTokenType::STRING)
                                .unwrap(),
                        });
                    }
                    Constant::Integer(_) | Constant::Unsigned(_) | Constant::Real(_) => {
                        semantic_tokens.push(ImCompleteSemanticToken {
                            start: const_expr.get_constant_token().span.start,
                            length: const_expr.get_constant_token().span.len(),
                            token_type: LEGEND_TYPE
                                .iter()
                                .position(|item| item == &SemanticTokenType::NUMBER)
                                .unwrap(),
                        });
                    }
                    _ => {}
                }
            }
        }
        icy_ppe::ast::Expression::Parens(expr) => {
            highlight_expressions(semantic_tokens, expr.get_expression());
        }
        icy_ppe::ast::Expression::FunctionCall(call) => {
            for arg in call.get_arguments() {
                highlight_expressions(semantic_tokens, arg);
            }
        }
        icy_ppe::ast::Expression::Unary(un_expr) => {
            highlight_expressions(semantic_tokens, un_expr.get_expression());
        }
        icy_ppe::ast::Expression::Binary(bin_expr) => {
            highlight_expressions(semantic_tokens, bin_expr.get_left_expression());
            highlight_expressions(semantic_tokens, bin_expr.get_right_expression());
        }
    }
}
