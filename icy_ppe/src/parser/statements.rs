use crate::{
    ast::{
        BlockStatement, BreakStatement, CaseBlock, CommentAstNode, Constant, ContinueStatement,
        ElseBlock, ElseIfBlock, EndStatement, Expression, ForStatement, GosubStatement,
        GotoStatement, IdentifierExpression, IfStatement, IfThenStatement, LabelStatement,
        LetStatement, PredefinedCallStatement, ProcedureCallStatement, ReturnStatement,
        SelectStatement, Statement, VariableDeclarationStatement, WhileDoStatement, WhileStatement,
    },
    executable::STATEMENT_DEFINITIONS,
    parser::{ParserError, ParserErrorType},
};

use super::{
    lexer::{SpannedToken, Token},
    Error, Parser, ParserWarning, ParserWarningType,
};

impl Parser {
    pub fn skip_eol(&mut self) {
        while self.get_cur_token() == Some(Token::Eol) {
            self.next_token();
        }
    }

    fn parse_while(&mut self) -> Option<Statement> {
        let while_token = self.save_spannedtoken();
        self.next_token();

        if self.get_cur_token() != Some(Token::LPar) {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::MissingOpenParens(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        }
        let lpar_token = self.save_spannedtoken();

        self.next_token();
        let Some(cond) = self.parse_expression() else {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::ExpressionExpected(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        };

        if self.get_cur_token() != Some(Token::RPar) {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::MissingCloseParens(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        }
        let rightpar_token = self.save_spannedtoken();
        self.next_token();

        if self.get_cur_token() == Some(Token::Identifier(unicase::Ascii::new("DO".to_string()))) {
            let do_token = self.save_spannedtoken();
            self.next_token();

            let mut statements = Vec::new();
            self.skip_eol();
            let mut end_while_token = None;
            while self.get_cur_token() != Some(Token::EndWhile) {
                if self.get_cur_token().is_none() {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::EndExpected,
                            range: self.lex.span(),
                        }));
                    return None;
                }
                if let Some(Token::End) = self.get_cur_token() {
                    let ct = self.save_spannedtoken();
                    self.next_token();
                    if let Some(Token::While) = self.get_cur_token() {
                        end_while_token = Some(SpannedToken {
                            token: Token::EndWhile,
                            span: ct.span.start..self.lex.span().end,
                        });
                        break;
                    }
                    self.skip_eol();

                    statements.push(Some(Statement::End(EndStatement::new(ct))));
                    continue;
                }
                statements.push(self.parse_statement());
                self.next_token();
                self.skip_eol();
            }
            if end_while_token.is_none() {
                end_while_token = Some(self.save_spannedtoken());
            }
            self.next_token(); // skip ENDWHILE

            Some(Statement::WhileDo(WhileDoStatement::new(
                while_token,
                lpar_token,
                Box::new(cond),
                rightpar_token,
                do_token,
                statements.into_iter().flatten().collect(),
                end_while_token.unwrap(),
            )))
        } else {
            self.skip_eol();
            let start = self.lex.span().start;
            if let Some(stmt) = self.parse_statement() {
                Some(Statement::While(WhileStatement::new(
                    while_token,
                    lpar_token,
                    Box::new(cond),
                    rightpar_token,
                    Box::new(stmt),
                )))
            } else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::StatementExpected,
                        range: start..self.lex.span().end,
                    }));
                None
            }
        }
    }

    fn _parse_block(&mut self, begin_token: SpannedToken) -> Option<Statement> {
        self.next_token();
        self.skip_eol();
        let mut statements = Vec::new();
        loop {
            let Some(token) = self.get_cur_token() else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::EndExpected,
                        range: self.lex.span(),
                    }));
                return None;
            };
            if token == Token::End {
                break;
            }
            statements.push(self.parse_statement());
            self.next_token();
            self.skip_eol();
        }
        let end_token = self.save_spannedtoken();
        Some(Statement::Block(BlockStatement::new(
            begin_token,
            statements.into_iter().flatten().collect(),
            end_token,
        )))
    }

    fn parse_for(&mut self) -> Option<Statement> {
        let for_token = self.save_spannedtoken();
        self.next_token();
        let identifier_token = self.save_spannedtoken();

        let _var = if let Some(Token::Identifier(id)) = self.get_cur_token() {
            self.next_token();
            IdentifierExpression::create_empty_expression(id)
        } else {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::IdentifierExpected(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        };

        if self.get_cur_token() != Some(Token::Eq) {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::EqTokenExpected(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        }

        let eq_token = self.save_spannedtoken();
        self.next_token();
        let Some(start_expr) = self.parse_expression() else {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::ExpressionExpected(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        };

        if let Some(Token::Identifier(id)) = self.get_cur_token() {
            if id != "TO" {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::ToExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            }
        }

        let to_token = self.save_spannedtoken();
        self.next_token();
        let Some(end_expr) = self.parse_expression() else {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::ExpressionExpected(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        };

        let (step_expr, step_token) = if self.get_cur_token()
            == Some(Token::Identifier(unicase::Ascii::new("STEP".to_string())))
        {
            let to_token = self.save_spannedtoken();
            self.next_token();
            (self.parse_expression().map(Box::new), Some(to_token))
        } else {
            (None, None)
        };

        let mut statements = Vec::new();
        self.skip_eol();
        while self.get_cur_token() != Some(Token::Next) {
            if self.get_cur_token().is_none() {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::EndExpected,
                        range: self.lex.span(),
                    }));
                return None;
            }
            statements.push(self.parse_statement());
            self.skip_eol();
        }
        let next_token = self.save_spannedtoken();

        Some(Statement::For(ForStatement::new(
            for_token,
            identifier_token,
            eq_token,
            Box::new(start_expr),
            to_token,
            Box::new(end_expr),
            step_token,
            step_expr,
            statements.into_iter().flatten().collect(),
            next_token,
        )))
    }

    fn parse_if(&mut self) -> Option<Statement> {
        let if_token = self.save_spannedtoken();
        self.next_token();

        if self.get_cur_token() != Some(Token::LPar) {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::MissingOpenParens(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        }
        let lpar_token = self.save_spannedtoken();
        self.next_token();
        let Some(cond) = self.parse_expression() else {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::ExpressionExpected(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        };

        if self.get_cur_token() != Some(Token::RPar) {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::MissingCloseParens(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        }
        let rightpar_token = self.save_spannedtoken();
        self.next_token();

        if self.get_cur_token() != Some(Token::Then) {
            self.skip_eol();

            let start = self.lex.span().start;
            if let Some(stmt) = self.parse_statement() {
                return Some(Statement::If(IfStatement::new(
                    if_token,
                    lpar_token,
                    Box::new(cond),
                    rightpar_token,
                    Box::new(stmt),
                )));
            }
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::StatementExpected,
                    range: start..self.lex.span().end,
                }));
            return None;
        }
        let then_token = self.save_spannedtoken();

        self.next_token();
        let mut statements = Vec::new();
        self.skip_eol();
        let mut end_if_token = None;

        while self.get_cur_token() != Some(Token::EndIf)
            && self.get_cur_token() != Some(Token::Else)
            && self.get_cur_token() != Some(Token::ElseIf)
        {
            if self.get_cur_token().is_none() {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::EndExpected,
                        range: self.lex.span(),
                    }));
                return None;
            }
            if let Some(Token::End) = self.get_cur_token() {
                let ct = self.save_spannedtoken();
                self.next_token();
                if let Some(Token::If) = self.get_cur_token() {
                    end_if_token = Some(SpannedToken {
                        token: Token::EndSelect,
                        span: ct.span.start..self.lex.span().end,
                    });
                    break;
                }
                self.skip_eol();
                statements.push(Some(Statement::End(EndStatement::new(ct))));
                continue;
            }
            statements.push(self.parse_statement());
            self.next_token();
            self.skip_eol();
        }
        let mut else_if_blocks = Vec::new();
        if self.get_cur_token() == Some(Token::ElseIf) {
            while self.get_cur_token() == Some(Token::ElseIf) {
                let else_if_token = self.save_spannedtoken();

                self.next_token();

                if self.get_cur_token() != Some(Token::LPar) {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::MissingOpenParens(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                }
                let else_if_lpar_token = self.save_spannedtoken();

                self.next_token();
                let Some(cond) = self.parse_expression() else {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::ExpressionExpected(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                };

                if self.get_cur_token() != Some(Token::RPar) {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::MissingCloseParens(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                }
                let else_if_rightpar_token = self.save_spannedtoken();
                self.next_token();

                let mut statements = Vec::new();
                while self.get_cur_token() != Some(Token::EndIf)
                    && self.get_cur_token() != Some(Token::Else)
                    && self.get_cur_token() != Some(Token::ElseIf)
                {
                    if self.get_cur_token().is_none() {
                        self.errors
                            .push(crate::parser::Error::ParserError(ParserError {
                                error: ParserErrorType::EndExpected,
                                range: self.lex.span(),
                            }));
                        return None;
                    }

                    if let Some(Token::End) = self.get_cur_token() {
                        let ct = self.save_spannedtoken();
                        self.next_token();
                        if let Some(Token::If) = self.get_cur_token() {
                            end_if_token = Some(SpannedToken {
                                token: Token::EndIf,
                                span: ct.span.start..self.lex.span().end,
                            });
                            break;
                        }
                        self.skip_eol();
                        statements.push(Some(Statement::End(EndStatement::new(ct))));
                        continue;
                    }
                    statements.push(self.parse_statement());
                    self.next_token();
                    self.skip_eol();
                }
                else_if_blocks.push(ElseIfBlock::new(
                    else_if_token,
                    else_if_lpar_token,
                    Box::new(cond),
                    else_if_rightpar_token,
                    statements.into_iter().flatten().collect(),
                ));
            }
        };

        let else_block = if self.get_cur_token() == Some(Token::Else) {
            let else_token = self.save_spannedtoken();

            self.next_token();
            let mut statements = Vec::new();
            self.skip_eol();
            while self.get_cur_token() != Some(Token::EndIf) {
                if self.get_cur_token().is_none() {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::EndExpected,
                            range: self.lex.span(),
                        }));
                    return None;
                }

                if let Some(Token::End) = self.get_cur_token() {
                    let ct = self.save_spannedtoken();
                    self.next_token();
                    if let Some(Token::If) = self.get_cur_token() {
                        end_if_token = Some(SpannedToken {
                            token: Token::EndIf,
                            span: ct.span.start..self.lex.span().end,
                        });
                        break;
                    }
                    self.skip_eol();
                    statements.push(Statement::End(EndStatement::new(ct)));
                    continue;
                }

                if let Some(stmt) = self.parse_statement() {
                    statements.push(stmt);
                }
                self.next_token();
                self.skip_eol();
            }
            Some(ElseBlock::new(else_token, statements))
        } else {
            None
        };

        if self.get_cur_token() != Some(Token::EndIf) && self.get_cur_token() != Some(Token::If) {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::InvalidToken(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        }
        if end_if_token.is_none() {
            end_if_token = Some(self.save_spannedtoken());
        }

        Some(Statement::IfThen(IfThenStatement::new(
            if_token,
            lpar_token,
            Box::new(cond),
            rightpar_token,
            then_token,
            statements.into_iter().flatten().collect(),
            else_if_blocks,
            else_block,
            end_if_token.unwrap(),
        )))
    }

    fn parse_select(&mut self) -> Option<Statement> {
        let select_token = self.save_spannedtoken();

        self.next_token();

        if self.get_cur_token() != Some(Token::Case) {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::CaseExpected(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        }

        let case_token = self.save_spannedtoken();
        self.next_token();
        let Some(case_expr) = self.parse_expression() else {
            self.errors
                .push(crate::parser::Error::ParserError(ParserError {
                    error: ParserErrorType::ExpressionExpected(self.save_token()),
                    range: self.lex.span(),
                }));
            return None;
        };
        self.next_token();
        self.skip_eol();

        let mut case_blocks = Vec::new();
        let mut else_block = None;
        let mut end_select_token = None;

        while self.get_cur_token() == Some(Token::Case) {
            let inner_case_token = self.save_spannedtoken();
            self.next_token();
            if self.get_cur_token() == Some(Token::Else) {
                let else_token = self.save_spannedtoken();
                self.next_token();
                let mut statements = Vec::new();
                self.skip_eol();
                while self.get_cur_token() != Some(Token::EndSelect) {
                    if let Some(Token::End) = self.get_cur_token() {
                        let ct = self.save_spannedtoken();
                        self.next_token();
                        if let Some(Token::Select) = self.get_cur_token() {
                            end_select_token = Some(SpannedToken {
                                token: Token::EndSelect,
                                span: ct.span.start..self.lex.span().end,
                            });
                            break;
                        }
                        self.skip_eol();
                        statements.push(Some(Statement::End(EndStatement::new(ct))));
                        continue;
                    }
                    statements.push(self.parse_statement());
                    self.next_token();
                    self.skip_eol();
                }
                else_block = Some(CaseBlock::new(
                    inner_case_token,
                    Box::new(Expression::Identifier(IdentifierExpression::new(
                        else_token,
                    ))),
                    statements.into_iter().flatten().collect(),
                ));
                break;
            };

            let Some(expr) = self.parse_expression() else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::ExpressionExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            };
            self.next_token();
            self.skip_eol();

            let mut statements = Vec::new();
            while self.get_cur_token() != Some(Token::Case)
                && self.get_cur_token() != Some(Token::EndSelect)
            {
                if let Some(Token::End) = self.get_cur_token() {
                    let ct = self.save_spannedtoken();
                    self.next_token();
                    if let Some(Token::Select) = self.get_cur_token() {
                        end_select_token = Some(SpannedToken {
                            token: Token::EndSelect,
                            span: ct.span.start..self.lex.span().end,
                        });
                        break;
                    }
                    self.skip_eol();
                    statements.push(Some(Statement::End(EndStatement::new(ct))));
                    continue;
                }

                statements.push(self.parse_statement());
                self.next_token();
                self.skip_eol();
            }
            case_blocks.push(CaseBlock::new(
                inner_case_token,
                Box::new(expr),
                statements.into_iter().flatten().collect(),
            ));
        }
        if end_select_token.is_none() {
            end_select_token = Some(self.save_spannedtoken());
        }

        Some(Statement::Select(SelectStatement::new(
            select_token,
            case_token,
            Box::new(case_expr),
            case_blocks,
            else_block,
            end_select_token.unwrap(),
        )))
    }

    /// Returns the parse statement of this [`Tokenizer`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.get_cur_token() {
            Some(Token::Comment(_, _)) => {
                let cmt = self.save_spannedtoken();
                Some(Statement::Comment(CommentAstNode::new(cmt)))
            }
            Some(Token::UseFuncs(_, _)) => {
                self.warnings.push(ParserWarning {
                    error: ParserWarningType::UsefuncsIgnored,
                    range: self.lex.span(),
                });
                None
            }
            Some(Token::End) => {
                let ct = self.save_spannedtoken();
                Some(Statement::End(EndStatement::new(ct)))
            }
            /*
            Some(Token::Begin) => {
                let begin_token = self.save_spannedtoken();
                self.parse_block(begin_token)
            }*/
            Some(Token::While) => self.parse_while(),
            Some(Token::Select) => self.parse_select(),
            Some(Token::If) => self.parse_if(),
            Some(Token::For) => self.parse_for(),
            Some(Token::Let) => {
                let let_token = self.save_spannedtoken();
                self.next_token();
                let identifier_token = self.save_spannedtoken();
                let _id = if let Token::Identifier(id) = &identifier_token.token {
                    self.next_token();
                    id
                } else {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::IdentifierExpected(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                };
                let mut leftpar_token = None;
                let mut rightpar_token = None;
                let mut params = Vec::new();
        
                if self.get_cur_token() == Some(Token::LPar) {
                    leftpar_token = Some(self.save_spannedtoken());
                    self.next_token();

                    
                    loop {
                        let Some(token) = self.get_cur_token() else {
                            self.errors
                                .push(crate::parser::Error::ParserError(ParserError {
                                    error: ParserErrorType::EndExpected,
                                    range: self.lex.span(),
                                }));
                            return None;
                        };

                        if self.get_cur_token() == Some(Token::RPar) {
                            break;
                        }
                        let Some(expr) = self.parse_expression() else {
                            self.errors
                                .push(crate::parser::Error::ParserError(ParserError {
                                    error: ParserErrorType::ExpressionExpected(self.save_token()),
                                    range: self.lex.span(),
                                }));
                            return None;
                        };
                        params.push(expr);
                        self.skip_eol();
                        
                        if self.get_cur_token() == Some(Token::RPar) {
                            break;
                        }
                        if self.get_cur_token() == Some(Token::Comma) {
                            self.next_token();
                        } else {
                            self.errors
                                .push(crate::parser::Error::ParserError(ParserError {
                                    error: ParserErrorType::CommaExpected(self.save_token()),
                                    range: self.lex.span(),
                                }));
                            return None;
                        }
                    }
                    rightpar_token = Some(self.save_spannedtoken());
                    self.next_token();
                }

                if self.get_cur_token() == Some(Token::Eq) {
                    let eq_token = self.save_spannedtoken();
                    self.next_token();
                    let Some(value_expression) = self.parse_expression() else {
                        self.errors
                            .push(crate::parser::Error::ParserError(ParserError {
                                error: ParserErrorType::ExpressionExpected(self.save_token()),
                                range: self.lex.span(),
                            }));
                        return None;
                    };
                    return Some(Statement::Let(LetStatement::new(
                        Some(let_token),
                        identifier_token,
                        leftpar_token,
                        params,
                        rightpar_token,
                        eq_token,
                        Box::new(value_expression),
                    )));
                }

                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::EqTokenExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                None
            }
            Some(Token::Break) => Some(Statement::Break(BreakStatement::new(
                self.save_spannedtoken(),
            ))),
            Some(Token::Continue) => Some(Statement::Continue(ContinueStatement::new(
                self.save_spannedtoken(),
            ))),
            Some(Token::Return) => Some(Statement::Return(ReturnStatement::new(
                self.save_spannedtoken(),
            ))),
            Some(Token::Gosub) => {
                let gosub_token = self.save_spannedtoken();
                self.next_token();
                if let Some(Token::Identifier(_)) = self.get_cur_token() {
                    let id_token = self.save_spannedtoken();
                    self.next_token();
                    return Some(Statement::Gosub(GosubStatement::new(gosub_token, id_token)));
                }
                self.next_token();
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::LabelExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                None
            }
            Some(Token::Goto) => {
                let goto_token = self.save_spannedtoken();
                self.next_token();
                if let Some(Token::Identifier(_)) = self.get_cur_token() {
                    let id_token = self.save_spannedtoken();
                    self.next_token();
                    return Some(Statement::Goto(GotoStatement::new(goto_token, id_token)));
                }
                self.next_token();
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::LabelExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                None
            }
            Some(Token::Const(Constant::Builtin(c))) => {
                if let Some(value) = self.parse_call(c.name) {
                    return Some(value);
                }
                self.next_token();
                self.parse_statement()
            }

            Some(Token::Identifier(id)) => {
                if let Some(var_type) = self.get_variable_type() {
                    let type_token = self.save_spannedtoken();
                    self.next_token();
                    let mut vars = Vec::new();
                    if let Some(v) = self.parse_var_info() {
                        vars.push(v);
                    } else {
                        return None;
                    }
                    while self.get_cur_token() == Some(Token::Comma) {
                        self.next_token();
                        if let Some(v) = self.parse_var_info() {
                            vars.push(v);
                        } else {
                            return None;
                        }
                    }
                    return Some(Statement::VariableDeclaration(
                        VariableDeclarationStatement::new(type_token, var_type, vars),
                    ));
                }

                if let Some(value) = self.parse_call(&id) {
                    return Some(value);
                }
                self.next_token();
                self.parse_statement()
            }

            Some(Token::Label(_)) => {
                let label_token = self.save_spannedtoken();
                self.next_token();
                Some(Statement::Label(LabelStatement::new(label_token)))
            }

            Some(Token::Eol) | None => None,
            _ => {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::InvalidToken(self.save_token()),
                        range: self.lex.span(),
                    }));
                self.next_token();
                None
            }
        }
    }

    fn parse_call(&mut self, id: &str) -> Option<Statement> {
        let id_token = self.save_spannedtoken();

        self.next_token();
        for def in &STATEMENT_DEFINITIONS {
            if unicase::Ascii::new(id.to_string()) == unicase::Ascii::new(def.name.to_string()) {
                let mut params = Vec::new();
                while self.get_cur_token() != Some(Token::Eol) && self.cur_token.is_some() {
                    // TODO : Signature check
                    /*If params.len() as i8 >= def.max_args {
                        break;
                    }*/
                    let Some(value) = self.parse_expression() else {
                        self.errors
                            .push(crate::parser::Error::ParserError(ParserError {
                                error: ParserErrorType::ExpressionExpected(self.save_token()),
                                range: self.lex.span(),
                            }));
                        return None;
                    };
                    params.push(value);

                    if self.cur_token.is_none() {
                        break;
                    }
                    if self.get_cur_token() == Some(Token::Comma) {
                        self.next_token();
                    } else {
                        break;
                    }
                }
                /*  TODO : Signature check.
                if (params.len() as i8) < def.min_args {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::TooFewArguments(
                                def.name.to_string(),
                                params.len(),
                                def.min_args,
                            ),
                            range: self.lex.span(),
                        }));
                    return None;
                }

                if (params.len() as i8) > def.max_args {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::TooManyArguments(
                                def.name.to_string(),
                                params.len(),
                                def.max_args,
                            ),
                            range: self.lex.span(),
                        }));
                    return None;
                }*/
                return Some(Statement::PredifinedCall(PredefinedCallStatement::new(
                    id_token, def, params,
                )));
            }
        }

        if self.get_cur_token() == Some(Token::Eq) {
            let eq_token = self.save_spannedtoken();
            self.next_token();
            let Some(value_expression) = self.parse_expression() else {
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::ExpressionExpected(self.save_token()),
                        range: self.lex.span(),
                    }));
                return None;
            };
            return Some(Statement::Let(LetStatement::new(
                None,
                id_token,
                None,
                Vec::new(),
                None,
                eq_token,
                Box::new(value_expression),
            )));
        }

        if self.get_cur_token() == Some(Token::LPar) {
            let lpar_token = self.save_spannedtoken();

            self.next_token();
            let mut params = Vec::new();

            while self.get_cur_token() != Some(Token::RPar) {
                let Some(right) = self.parse_expression() else {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::ExpressionExpected(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                };
                params.push(right);
                if self.get_cur_token() == Some(Token::Comma) {
                    self.next_token();
                }
            }
            if self.get_cur_token() != Some(Token::RPar) {
                self.errors.push(Error::ParserError(ParserError {
                    error: ParserErrorType::MissingCloseParens(self.save_token()),
                    range: self.save_token_span(),
                }));
                return None;
            }
            let rightpar_token = self.save_spannedtoken();

            self.next_token();
            if self.get_cur_token() == Some(Token::Eq) {
                let eq_token = self.save_spannedtoken();
                self.next_token();
                let Some(value_expression) = self.parse_expression() else {
                    self.errors
                        .push(crate::parser::Error::ParserError(ParserError {
                            error: ParserErrorType::ExpressionExpected(self.save_token()),
                            range: self.lex.span(),
                        }));
                    return None;
                };
                if !params.is_empty() && params.len() <= 3 {
                    return Some(Statement::Let(LetStatement::new(
                        None,
                        id_token,
                        Some(lpar_token),
                        params,
                        Some(rightpar_token),
                        eq_token,
                        Box::new(value_expression),
                    )));
                }
                self.errors
                    .push(crate::parser::Error::ParserError(ParserError {
                        error: ParserErrorType::TooManyDimensions(params.len()),
                        range: self.lex.span(),
                    }));
                return None;
            }

            return Some(Statement::Call(ProcedureCallStatement::new(
                id_token,
                lpar_token,
                params,
                rightpar_token,
            )));
        }
        self.errors
            .push(crate::parser::Error::ParserError(ParserError {
                error: ParserErrorType::UnknownIdentifier(id_token.token.to_string()),
                range: id_token.span,
            }));
        None
    }
}
/*
#[cfg(test)]
mod tests {
    use crate::{
        ast:: Statement,
    };

    fn parse_statement(src: &str) -> Statement {
        let mut tokenizer = Tokenizer::new(src);
        tokenizer.next_token();
        tokenizer.parse_statement().unwrap()
    }

    fn get_statement_definition(name: &str) -> Option<&'static StatementDefinition> {
        let name = name.to_uppercase();
        STATEMENT_DEFINITIONS
            .iter()
            .find(|&def| def.name.to_uppercase() == name)
    }

    #[test]
    fn test_let() {
        assert_eq!(
            Statement::Let(
                Box::new(VarInfo::Var0("foo_bar".to_string())),
                Box::new(Expression::Const(Constant::Integer(1)))
            ),
            parse_statement("foo_bar=1")
        );

        assert_eq!(
            Statement::Let(
                Box::new(VarInfo::Var0("FOO".to_string())),
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::FALSE)))
            ),
            parse_statement("LET FOO = FALSE")
        );
    }

    #[test]
    fn test_parse_statement() {
        assert_eq!(
            Statement::Call(
                get_statement_definition("ADJTIME").unwrap(),
                vec![Expression::Const(Constant::Integer(1))]
            ),
            parse_statement("ADJTIME 1")
        );
        assert_eq!(
            Statement::Call(
                get_statement_definition("ANSIPOS").unwrap(),
                vec![
                    Expression::Const(Constant::Integer(1)),
                    Expression::Const(Constant::Integer(2))
                ]
            ),
            parse_statement("ANSIPOS 1, 2")
        );
        assert_eq!(
            Statement::Call(
                get_statement_definition("BROADCAST").unwrap(),
                vec![
                    Expression::Const(Constant::Integer(1)),
                    Expression::Const(Constant::Integer(2)),
                    Expression::Const(Constant::Integer(3))
                ]
            ),
            parse_statement("BROADCAST 1, 2, 3")
        );
        assert_eq!(
            Statement::Call(get_statement_definition("BYE").unwrap(), vec![]),
            parse_statement("BYE")
        );
        assert_eq!(
            Statement::Call(get_statement_definition("PRINTLN").unwrap(), vec![]),
            parse_statement("PRINTLN")
        );
        assert_eq!(
            Statement::Call(
                get_statement_definition("PRINTLN").unwrap(),
                vec![Expression::Const(Constant::String(
                    "Hello World".to_string()
                ))]
            ),
            parse_statement("PRINTLN \"Hello World\"")
        );
    }
*/
/*
#[test]
fn test_parse_hello_world() {
    // check_statements(";This is a comment\nPRINT \"Hello World\"\n\t\n\n", vec![Statement::Call(get_statement_definition("PRINT").unwrap(), vec![Expression::Const(Constant::String("Hello World".to_string()))])]);
}

#[test]
fn test_gotogosub() {
    assert_eq!(
        Statement::Goto("LABEL1".to_string()),
        parse_statement("GOTO LABEL1")
    );

    assert_eq!(
        Statement::Gosub("LABEL2".to_string()),
        parse_statement("GOSUB LABEL2")
    );
    assert_eq!(
        Statement::Label("LABEL1".to_string()),
        parse_statement(":LABEL1")
    );
}

#[test]
fn test_incdec() {
    assert_eq!(
        Statement::Call(
            get_statement_definition("INC").unwrap(),
            vec![Expression::Identifier("VAR1".to_string()),]
        ),
        parse_statement("INC VAR1\n")
    );
    assert_eq!(
        Statement::Call(
            get_statement_definition("DEC").unwrap(),
            vec![Expression::Identifier("VAR2".to_string()),]
        ),
        parse_statement("DEC VAR2\n")
    );
}

#[test]
fn test_parse_simple_noncalls() {
    assert_eq!(Statement::End, parse_statement("End ; Predifined End"));

    assert_eq!(Statement::Break, parse_statement("BREAK"));

    assert_eq!(Statement::Continue, parse_statement("CONTINUE"));

    assert_eq!(Statement::Return, parse_statement("RETURN"));
}*/
/*
    #[test]
    fn test_procedure_calls() {
        assert_eq!(
            Statement::ProcedureCall("PROC".to_string(), Vec::new()),
            parse_statement("PROC()")
        );
        assert_eq!(
            Statement::ProcedureCall(
                "PROC".to_string(),
                vec![Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))]
            ),
            parse_statement("PROC(TRUE)")
        );
        assert_eq!(
            Statement::ProcedureCall(
                "PROC".to_string(),
                vec![
                    Expression::Const(Constant::Integer(5)),
                    Expression::Const(Constant::Integer(7))
                ]
            ),
            parse_statement("PROC(5, 7)")
        );
    }

    #[test]
    fn test_parse_if() {
        assert_eq!(
            Statement::If(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::FALSE))),
                Box::new(Statement::End)
            ),
            parse_statement(" IF (FALSE) END")
        );
        let print_hello = parse_statement("PRINT \"Hello Word\"");
        let print_5 = parse_statement("PRINT 5");
        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                vec![print_hello.clone()],
                Vec::new(),
                None
            ),
            parse_statement(" IF (TRUE) THEN PRINT \"Hello Word\" ENDIF")
        );

        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                vec![print_hello.clone()],
                vec![ElseIfBlock {
                    cond: Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                    block: vec![print_5]
                }],
                None
            ),
            parse_statement("IF (TRUE) THEN PRINT \"Hello Word\" ELSEIF (TRUE) THEN PRINT 5 ENDIF")
        );

        let print_5 = parse_statement("PRINT 5");

        assert_eq!(
            Statement::IfThen(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::FALSE))),
                Vec::new(),
                Vec::new(),
                Some(vec![print_5])
            ),
            parse_statement("IF (FALSE) THEN ELSE\nPRINT 5 ENDIF")
        );
    }

    #[test]
    fn test_parse_while() {
        assert_eq!(
            Statement::While(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::FALSE))),
                Box::new(Statement::End)
            ),
            parse_statement("WHILE (FALSE) END")
        );
        assert_eq!(
            Statement::DoWhile(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                Vec::new()
            ),
            parse_statement("WHILE (TRUE) DO ENDWHILE")
        );
    }

        #[test]
        fn test_for_next() {
            assert_eq!(
                Statement::For(
                    Box::new(Expression::Identifier("i".to_string())),
                    Box::new(Expression::Const(Constant::Integer(1))),
                    Box::new(Expression::Const(Constant::Integer(10))),
                    None,
                    Vec::new()
                ),
                parse_statement("FOR i = 1 TO 10 NEXT")
            );
            assert_eq!(
                Statement::For(
                    Box::new(Expression::Identifier("i".to_string())),
                    Box::new(Expression::Const(Constant::Integer(1))),
                    Box::new(Expression::Const(Constant::Integer(10))),
                    Some(Box::new(Expression::Const(Constant::Integer(5)))),
                    Vec::new()
                ),
                parse_statement("FOR i = 1 TO 10 STEP 5 NEXT")
            );
            assert_eq!(
                Statement::For(
                    Box::new(Expression::Identifier("i".to_string())),
                    Box::new(Expression::Const(Constant::Integer(1))),
                    Box::new(Expression::Const(Constant::Integer(10))),
                    Some(Box::new(Expression::UnaryExpression(
                        crate::ast::UnaryOp::Minus,
                        Box::new(Expression::Const(Constant::Integer(1)))
                    ))),
                    Vec::new()
                ),
                parse_statement("FOR i = 1 TO 10 STEP -1 NEXT")
            );
        }

    #[test]
    fn test_parse_block() {
        assert_eq!(Statement::Block(Vec::new()), parse_statement("BEGIN END"));
    }
    #[test]
    fn test_select_case() {
        assert_eq!(
            Statement::Select(
                Box::new(Expression::Identifier("I".to_string())),
                vec![
                    ElseIfBlock {
                        cond: Box::new(Expression::Const(Constant::Integer(1))),
                        block: vec![Statement::Call(
                            get_statement_definition("PRINT").unwrap(),
                            vec![Expression::Const(Constant::Integer(1))]
                        )]
                    },
                    ElseIfBlock {
                        cond: Box::new(Expression::Const(Constant::Integer(2))),
                        block: vec![Statement::Call(
                            get_statement_definition("PRINT").unwrap(),
                            vec![Expression::Const(Constant::Integer(2))]
                        )]
                    }
                ],
                Some(vec![Statement::Call(
                    get_statement_definition("PRINT").unwrap(),
                    vec![Expression::Const(Constant::Integer(3))]
                )])
            ),
            parse_statement(
                "SELECT CASE I\nCASE 1\n PRINT 1\nCASE 2\n  PRINT 2\nCASE ELSE\nPRINT 3 ENDSELECT"
            )
        );
        assert_eq!(
            Statement::DoWhile(
                Box::new(Expression::Const(Constant::Builtin(&BuiltinConst::TRUE))),
                Vec::new()
            ),
            parse_statement("WHILE (TRUE) DO ENDWHILE")
        );
    }
}*/
