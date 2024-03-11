use std::fmt;

use crate::{
    output_keyword_indented,
    parser::tokens::{CommentType, SpannedToken, Token},
    tables::PPL_TRUE,
};

use super::{
    Constant, ConstantExpression, Expression, ProgramContext, UnaryExpression, UnaryOp, VarInfo,
    VariableType,
};
use crate::output_keyword;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Comment(CommentStatement),
    End(EndStatement),
    Block(BlockStatement),

    If(IfStatement),
    IfThen(IfThenStatement),
    Select(SelectStatement),

    While(WhileStatement),
    WhileDo(WhileDoStatement),
    For(ForStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Gosub(GosubStatement),
    Return(ReturnStatement),
    Let(Box<VarInfo>, Box<Expression>),
    Goto(GotoStatement),
    Label(LabelStatement),
    Call(ProcedureCallStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CommentStatement {
    comment_token: SpannedToken,
}

impl CommentStatement {
    pub fn new(comment_token: SpannedToken) -> Self {
        Self { comment_token }
    }

    pub fn empty(comment: impl Into<String>) -> Self {
        Self {
            comment_token: SpannedToken::create_empty(Token::Comment((
                CommentType::SingleLineQuote,
                comment.into(),
            ))),
        }
    }

    pub fn get_comment_token(&self) -> &SpannedToken {
        &self.comment_token
    }

    /// Returns a reference to the get identifier of this [`IdentifierExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_comment(&self) -> &String {
        if let Token::Comment(id) = &self.comment_token.token {
            return &id.1;
        }
        panic!("Expected identifier token")
    }

    /// # Panics
    ///
    /// Panics if .
    pub fn get_comment_type(&self) -> CommentType {
        if let Token::Comment(id) = &self.comment_token.token {
            return id.0;
        }
        panic!("Expected identifier token")
    }

    pub fn create_empty_statement(identifier: impl Into<String>) -> Statement {
        Statement::Comment(CommentStatement::empty(identifier))
    }
}

impl fmt::Display for CommentStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.get_comment_type() {
            CommentType::SingleLineSemicolon => write!(f, ";")?,
            CommentType::SingleLineQuote => write!(f, "'")?,
        }
        write!(f, "{}", self.get_comment())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EndStatement {
    end_token: SpannedToken,
}

impl EndStatement {
    pub fn new(end_token: SpannedToken) -> Self {
        Self { end_token }
    }

    pub fn empty() -> Self {
        Self {
            end_token: SpannedToken::create_empty(Token::End),
        }
    }

    pub fn get_end_token(&self) -> &SpannedToken {
        &self.end_token
    }

    pub fn create_empty_statement() -> Statement {
        Statement::End(EndStatement::empty())
    }
}

impl fmt::Display for EndStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "End")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BreakStatement {
    break_token: SpannedToken,
}

impl BreakStatement {
    pub fn new(break_token: SpannedToken) -> Self {
        Self { break_token }
    }

    pub fn empty() -> Self {
        Self {
            break_token: SpannedToken::create_empty(Token::Break),
        }
    }

    pub fn get_break_token(&self) -> &SpannedToken {
        &self.break_token
    }

    pub fn create_empty_statement() -> Statement {
        Statement::Break(BreakStatement::empty())
    }
}

impl fmt::Display for BreakStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Break")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ContinueStatement {
    continue_token: SpannedToken,
}

impl ContinueStatement {
    pub fn new(continue_token: SpannedToken) -> Self {
        Self { continue_token }
    }

    pub fn empty() -> Self {
        Self {
            continue_token: SpannedToken::create_empty(Token::Break),
        }
    }

    pub fn get_continue_token(&self) -> &SpannedToken {
        &self.continue_token
    }

    pub fn create_empty_statement() -> Statement {
        Statement::Continue(ContinueStatement::empty())
    }
}

impl fmt::Display for ContinueStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Continue")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    return_token: SpannedToken,
}

impl ReturnStatement {
    pub fn new(return_token: SpannedToken) -> Self {
        Self { return_token }
    }

    pub fn empty() -> Self {
        Self {
            return_token: SpannedToken::create_empty(Token::Return),
        }
    }

    pub fn get_return_token(&self) -> &SpannedToken {
        &self.return_token
    }

    pub fn create_empty_statement() -> Statement {
        Statement::Return(ReturnStatement::empty())
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Return")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    begin_token: SpannedToken,
    statements: Vec<Statement>,
    end_token: SpannedToken,
}

impl BlockStatement {
    pub fn new(
        begin_token: SpannedToken,
        statements: Vec<Statement>,
        end_token: SpannedToken,
    ) -> Self {
        Self {
            begin_token,
            statements,
            end_token,
        }
    }

    pub fn empty(statements: Vec<Statement>) -> Self {
        Self {
            begin_token: SpannedToken::create_empty(Token::Begin),
            statements,
            end_token: SpannedToken::create_empty(Token::End),
        }
    }

    pub fn get_begin_token(&self) -> &SpannedToken {
        &self.begin_token
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }

    pub fn get_end_token(&self) -> &SpannedToken {
        &self.begin_token
    }

    pub fn create_empty_statement(statements: Vec<Statement>) -> Statement {
        Statement::Block(BlockStatement::empty(statements))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement {
    if_token: SpannedToken,
    leftpar_token: SpannedToken,
    condition: Box<Expression>,
    rightpar_token: SpannedToken,
    statement: Box<Statement>,
}

impl IfStatement {
    pub fn new(
        if_token: SpannedToken,
        leftpar_token: SpannedToken,
        condition: Box<Expression>,
        rightpar_token: SpannedToken,
        statement: Box<Statement>,
    ) -> Self {
        Self {
            if_token,
            leftpar_token,
            condition,
            rightpar_token,
            statement,
        }
    }

    pub fn empty(condition: Box<Expression>, statement: Box<Statement>) -> Self {
        Self {
            if_token: SpannedToken::create_empty(Token::While),
            leftpar_token: SpannedToken::create_empty(Token::LPar),
            condition,
            rightpar_token: SpannedToken::create_empty(Token::RPar),
            statement,
        }
    }

    pub fn get_if_token(&self) -> &SpannedToken {
        &self.if_token
    }
    pub fn get_lpar_token(&self) -> &SpannedToken {
        &self.leftpar_token
    }

    pub fn get_condition(&self) -> &Expression {
        &self.condition
    }

    pub fn get_condition_mut(&mut self) -> &mut Expression {
        &mut self.condition
    }

    pub fn get_rpar_token(&self) -> &SpannedToken {
        &self.rightpar_token
    }

    pub fn get_statement(&self) -> &Statement {
        &self.statement
    }

    pub fn get_statement_mut(&mut self) -> &mut Statement {
        &mut self.statement
    }

    pub fn set_statement(&mut self, statement: Box<Statement>) {
        self.statement = statement;
    }

    pub fn create_empty_statement(
        condition: Box<Expression>,
        statement: Box<Statement>,
    ) -> Statement {
        Statement::If(IfStatement::empty(condition, statement))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseIfBlock {
    elseif_token: SpannedToken,
    leftpar_token: SpannedToken,
    cond: Box<Expression>,
    rightpar_token: SpannedToken,
    statements: Vec<Statement>,
}

impl ElseIfBlock {
    pub fn new(
        elseif_token: SpannedToken,
        leftpar_token: SpannedToken,
        cond: Box<Expression>,
        rightpar_token: SpannedToken,
        statements: Vec<Statement>,
    ) -> Self {
        Self {
            elseif_token,
            leftpar_token,
            cond,
            rightpar_token,
            statements,
        }
    }

    pub fn empty(cond: Box<Expression>, statements: Vec<Statement>) -> Self {
        Self {
            elseif_token: SpannedToken::create_empty(Token::ElseIf),
            leftpar_token: SpannedToken::create_empty(Token::LPar),
            cond,
            rightpar_token: SpannedToken::create_empty(Token::RPar),
            statements,
        }
    }

    pub fn get_elseif_token(&self) -> &SpannedToken {
        &self.elseif_token
    }

    pub fn get_leftpar_token(&self) -> &SpannedToken {
        &self.leftpar_token
    }

    pub fn get_condition(&self) -> &Expression {
        &self.cond
    }

    pub fn get_condition_mut(&mut self) -> &mut Expression {
        &mut self.cond
    }

    pub fn get_rightpar_token(&self) -> &SpannedToken {
        &self.rightpar_token
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseBlock {
    else_token: SpannedToken,
    statements: Vec<Statement>,
}

impl ElseBlock {
    pub fn new(else_token: SpannedToken, statements: Vec<Statement>) -> Self {
        Self {
            else_token,
            statements,
        }
    }

    pub fn empty(statements: Vec<Statement>) -> Self {
        Self {
            else_token: SpannedToken::create_empty(Token::Else),
            statements,
        }
    }

    pub fn get_else_token(&self) -> &SpannedToken {
        &self.else_token
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfThenStatement {
    if_token: SpannedToken,
    leftpar_token: SpannedToken,
    condition: Box<Expression>,
    rightpar_token: SpannedToken,
    then_token: SpannedToken,
    statements: Vec<Statement>,

    else_if_blocks: Vec<ElseIfBlock>,
    else_block: Option<ElseBlock>,
    endif_token: SpannedToken,
}

impl IfThenStatement {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        if_token: SpannedToken,
        leftpar_token: SpannedToken,
        condition: Box<Expression>,
        rightpar_token: SpannedToken,
        then_token: SpannedToken,
        statements: Vec<Statement>,
        else_if_blocks: Vec<ElseIfBlock>,
        else_block: Option<ElseBlock>,
        endif_token: SpannedToken,
    ) -> Self {
        Self {
            if_token,
            leftpar_token,
            condition,
            rightpar_token,
            then_token,
            statements,
            else_if_blocks,
            else_block,
            endif_token,
        }
    }

    pub fn get_if_token(&self) -> &SpannedToken {
        &self.if_token
    }
    pub fn get_lpar_token(&self) -> &SpannedToken {
        &self.leftpar_token
    }

    pub fn get_condition(&self) -> &Expression {
        &self.condition
    }

    pub fn get_condition_mut(&mut self) -> &mut Expression {
        &mut self.condition
    }

    pub fn get_rpar_token(&self) -> &SpannedToken {
        &self.rightpar_token
    }

    pub fn get_then_token(&self) -> &SpannedToken {
        &self.then_token
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }

    pub fn get_else_if_blocks(&self) -> &Vec<ElseIfBlock> {
        &self.else_if_blocks
    }

    pub fn get_else_if_blocks_mut(&mut self) -> &mut Vec<ElseIfBlock> {
        &mut self.else_if_blocks
    }

    pub fn get_else_block(&self) -> &Option<ElseBlock> {
        &self.else_block
    }

    pub fn get_else_block_mut(&mut self) -> &mut Option<ElseBlock> {
        &mut self.else_block
    }

    pub fn get_endif_token(&self) -> &SpannedToken {
        &self.endif_token
    }

    pub fn empty(
        condition: Box<Expression>,
        statements: Vec<Statement>,
        else_if_blocks: Vec<ElseIfBlock>,
        else_block: Option<ElseBlock>,
    ) -> Self {
        Self {
            if_token: SpannedToken::create_empty(Token::If),
            leftpar_token: SpannedToken::create_empty(Token::LPar),
            condition,
            rightpar_token: SpannedToken::create_empty(Token::RPar),
            then_token: SpannedToken::create_empty(Token::Then),
            statements,
            else_if_blocks,
            else_block,
            endif_token: SpannedToken::create_empty(Token::EndIf),
        }
    }

    pub fn create_empty_statement(
        condition: Box<Expression>,
        statements: Vec<Statement>,
        else_if_blocks: Vec<ElseIfBlock>,
        else_block: Option<ElseBlock>,
    ) -> Statement {
        Statement::IfThen(IfThenStatement::empty(
            condition,
            statements,
            else_if_blocks,
            else_block,
        ))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStatement {
    while_token: SpannedToken,
    leftpar_token: SpannedToken,
    condition: Box<Expression>,
    rightpar_token: SpannedToken,
    statement: Box<Statement>,
}

impl WhileStatement {
    pub fn new(
        while_token: SpannedToken,
        leftpar_token: SpannedToken,
        condition: Box<Expression>,
        rightpar_token: SpannedToken,
        statement: Box<Statement>,
    ) -> Self {
        Self {
            while_token,
            leftpar_token,
            condition,
            rightpar_token,
            statement,
        }
    }

    pub fn empty(condition: Box<Expression>, statement: Box<Statement>) -> Self {
        Self {
            while_token: SpannedToken::create_empty(Token::While),
            leftpar_token: SpannedToken::create_empty(Token::LPar),
            condition,
            rightpar_token: SpannedToken::create_empty(Token::RPar),
            statement,
        }
    }

    pub fn get_while_token(&self) -> &SpannedToken {
        &self.while_token
    }
    pub fn get_lpar_token(&self) -> &SpannedToken {
        &self.leftpar_token
    }

    pub fn get_condition(&self) -> &Expression {
        &self.condition
    }

    pub fn get_condition_mut(&mut self) -> &mut Expression {
        &mut self.condition
    }

    pub fn get_rpar_token(&self) -> &SpannedToken {
        &self.rightpar_token
    }

    pub fn get_statement(&self) -> &Statement {
        &self.statement
    }

    pub fn get_statement_mut(&mut self) -> &mut Statement {
        &mut self.statement
    }

    pub fn create_empty_statement(
        condition: Box<Expression>,
        statement: Box<Statement>,
    ) -> Statement {
        Statement::While(WhileStatement::empty(condition, statement))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileDoStatement {
    while_token: SpannedToken,
    leftpar_token: SpannedToken,
    condition: Box<Expression>,
    rightpar_token: SpannedToken,
    do_token: SpannedToken,
    statements: Vec<Statement>,
    endwhile_token: SpannedToken,
}

impl WhileDoStatement {
    pub fn new(
        while_token: SpannedToken,
        leftpar_token: SpannedToken,
        condition: Box<Expression>,
        rightpar_token: SpannedToken,
        do_token: SpannedToken,
        statements: Vec<Statement>,
        endwhile_token: SpannedToken,
    ) -> Self {
        Self {
            while_token,
            leftpar_token,
            condition,
            rightpar_token,
            do_token,
            statements,
            endwhile_token,
        }
    }

    pub fn empty(condition: Box<Expression>, statements: Vec<Statement>) -> Self {
        Self {
            while_token: SpannedToken::create_empty(Token::While),
            leftpar_token: SpannedToken::create_empty(Token::LPar),
            condition,
            rightpar_token: SpannedToken::create_empty(Token::RPar),
            do_token: SpannedToken::create_empty(Token::Identifier("Do".to_string())),
            statements,
            endwhile_token: SpannedToken::create_empty(Token::EndWhile),
        }
    }

    pub fn get_while_token(&self) -> &SpannedToken {
        &self.while_token
    }

    pub fn get_lpar_token(&self) -> &SpannedToken {
        &self.leftpar_token
    }

    pub fn get_condition(&self) -> &Expression {
        &self.condition
    }

    pub fn get_condition_mut(&mut self) -> &mut Expression {
        &mut self.condition
    }

    pub fn get_rpar_token(&self) -> &SpannedToken {
        &self.rightpar_token
    }

    pub fn get_do_token(&self) -> &SpannedToken {
        &self.do_token
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }

    pub fn get_endwhile_token(&self) -> &SpannedToken {
        &self.endwhile_token
    }

    pub fn create_empty_statement(
        condition: Box<Expression>,
        statements: Vec<Statement>,
    ) -> Statement {
        Statement::WhileDo(WhileDoStatement::empty(condition, statements))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ForStatement {
    for_token: SpannedToken,
    identifier_token: SpannedToken,
    eq_token: SpannedToken,
    start_expr: Box<Expression>,
    to_token: SpannedToken,
    end_expr: Box<Expression>,
    step_token: Option<SpannedToken>,
    step_expr: Option<Box<Expression>>,
    statements: Vec<Statement>,
    next_token: SpannedToken,
}

impl ForStatement {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        for_token: SpannedToken,
        identifier_token: SpannedToken,
        eq_token: SpannedToken,
        start_expr: Box<Expression>,
        to_token: SpannedToken,
        end_expr: Box<Expression>,
        step_token: Option<SpannedToken>,
        step_expr: Option<Box<Expression>>,
        statements: Vec<Statement>,
        next_token: SpannedToken,
    ) -> Self {
        Self {
            for_token,
            identifier_token,
            eq_token,
            start_expr,
            to_token,
            end_expr,
            step_token,
            step_expr,
            statements,
            next_token,
        }
    }

    pub fn empty(
        variable_name: String,
        start_expr: Box<Expression>,
        end_expr: Box<Expression>,
        step_expr: Option<Box<Expression>>,
        statements: Vec<Statement>,
    ) -> Self {
        Self {
            for_token: SpannedToken::create_empty(Token::For),
            identifier_token: SpannedToken::create_empty(Token::Identifier(variable_name)),
            eq_token: SpannedToken::create_empty(Token::Eq),
            start_expr,
            to_token: SpannedToken::create_empty(Token::Identifier("TO".to_string())),
            end_expr,
            step_token: step_expr
                .as_ref()
                .map(|_| SpannedToken::create_empty(Token::Identifier("Step".to_string()))),
            step_expr,
            statements,
            next_token: SpannedToken::create_empty(Token::Next),
        }
    }

    pub fn get_for_token(&self) -> &SpannedToken {
        &self.for_token
    }

    pub fn get_identifier_token(&self) -> &SpannedToken {
        &self.identifier_token
    }

    /// Returns a reference to the get identifier of this [`ForStatement`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_identifier(&self) -> &String {
        if let Token::Identifier(id) = &self.identifier_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub fn set_identifier(&mut self, new_id: &str) {
        if let Token::Identifier(id) = &mut self.identifier_token.token {
            *id = new_id.to_string();
        }
    }

    pub fn get_eq_token(&self) -> &SpannedToken {
        &self.eq_token
    }

    pub fn get_start_expr(&self) -> &Expression {
        &self.start_expr
    }

    pub fn get_start_expr_mut(&mut self) -> &mut Expression {
        &mut self.start_expr
    }

    pub fn get_to_token(&self) -> &SpannedToken {
        &self.to_token
    }

    pub fn get_end_expr(&self) -> &Expression {
        &self.end_expr
    }

    pub fn get_end_expr_mut(&mut self) -> &mut Expression {
        &mut self.end_expr
    }

    pub fn get_step_token(&self) -> &Option<SpannedToken> {
        &self.step_token
    }

    pub fn get_step_expr(&self) -> &Option<Box<Expression>> {
        &self.step_expr
    }

    pub fn get_step_expr_mut(&mut self) -> &mut Option<Box<Expression>> {
        &mut self.step_expr
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }

    pub fn get_next_token(&self) -> &SpannedToken {
        &self.next_token
    }

    pub fn create_empty_statement(
        variable_name: String,
        start_expr: Box<Expression>,
        end_expr: Box<Expression>,
        step_expr: Option<Box<Expression>>,
        statements: Vec<Statement>,
    ) -> Statement {
        Statement::For(ForStatement::empty(
            variable_name,
            start_expr,
            end_expr,
            step_expr,
            statements,
        ))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CaseBlock {
    case_token: SpannedToken,
    expr: Box<Expression>,
    statements: Vec<Statement>,
}

impl CaseBlock {
    pub fn new(
        case_token: SpannedToken,
        expr: Box<Expression>,
        statements: Vec<Statement>,
    ) -> Self {
        Self {
            case_token,
            expr,
            statements,
        }
    }

    pub fn empty(expr: Box<Expression>, statements: Vec<Statement>) -> Self {
        Self {
            case_token: SpannedToken::create_empty(Token::Case),
            expr,
            statements,
        }
    }

    pub fn get_case_token(&self) -> &SpannedToken {
        &self.case_token
    }

    pub fn get_expr(&self) -> &Expression {
        &self.expr
    }

    pub fn get_expr_mut(&mut self) -> &mut Expression {
        &mut self.expr
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SelectStatement {
    select_token: SpannedToken,
    case_token: SpannedToken,
    expr: Box<Expression>,
    case_blocks: Vec<CaseBlock>,
    case_else_block: Option<CaseBlock>,
    endselect_token: SpannedToken,
}

impl SelectStatement {
    pub fn new(
        select_token: SpannedToken,
        case_token: SpannedToken,
        expr: Box<Expression>,
        case_blocks: Vec<CaseBlock>,
        case_else_block: Option<CaseBlock>,
        endselect_token: SpannedToken,
    ) -> Self {
        Self {
            select_token,
            case_token,
            expr,
            case_blocks,
            case_else_block,
            endselect_token,
        }
    }

    pub fn empty(
        expr: Box<Expression>,
        case_blocks: Vec<CaseBlock>,
        case_else_block: Option<CaseBlock>,
    ) -> Self {
        Self {
            select_token: SpannedToken::create_empty(Token::Select),
            case_token: SpannedToken::create_empty(Token::Case),
            expr,
            case_blocks,
            case_else_block,
            endselect_token: SpannedToken::create_empty(Token::EndSelect),
        }
    }

    pub fn get_select_token(&self) -> &SpannedToken {
        &self.select_token
    }
    pub fn get_case_token(&self) -> &SpannedToken {
        &self.case_token
    }

    pub fn get_lpar_token(&self) -> &SpannedToken {
        &self.case_token
    }

    pub fn get_expr(&self) -> &Expression {
        &self.expr
    }

    pub fn get_expr_mut(&mut self) -> &mut Expression {
        &mut self.expr
    }

    pub fn get_case_blocks(&self) -> &Vec<CaseBlock> {
        &self.case_blocks
    }

    pub fn get_case_blocks_mut(&mut self) -> &mut Vec<CaseBlock> {
        &mut self.case_blocks
    }

    pub fn get_case_else_block(&self) -> &Option<CaseBlock> {
        &self.case_else_block
    }

    pub fn get_case_else_block_mut(&mut self) -> &mut Option<CaseBlock> {
        &mut self.case_else_block
    }

    pub fn get_endselect_token(&self) -> &SpannedToken {
        &self.endselect_token
    }

    pub fn create_empty_statement(
        expr: Box<Expression>,
        case_blocks: Vec<CaseBlock>,
        case_else_block: Option<CaseBlock>,
    ) -> Statement {
        Statement::Select(SelectStatement::empty(expr, case_blocks, case_else_block))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct GosubStatement {
    gosub_token: SpannedToken,
    label_token: SpannedToken,
}

impl GosubStatement {
    pub fn new(gosub_token: SpannedToken, label_token: SpannedToken) -> Self {
        Self {
            gosub_token,
            label_token,
        }
    }

    pub fn empty(label: String) -> Self {
        Self {
            gosub_token: SpannedToken::create_empty(Token::Gosub),
            label_token: SpannedToken::create_empty(Token::Identifier(label)),
        }
    }

    pub fn get_gosub_token(&self) -> &SpannedToken {
        &self.gosub_token
    }

    pub fn get_label_token(&self) -> &SpannedToken {
        &self.label_token
    }

    /// Returns a reference to the get label of this [`GosubStatement`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_label(&self) -> &String {
        if let Token::Identifier(id) = &self.label_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub fn create_empty_statement(label: String) -> Statement {
        Statement::Gosub(GosubStatement::empty(label))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct GotoStatement {
    goto_token: SpannedToken,
    label_token: SpannedToken,
}

impl GotoStatement {
    pub fn new(goto_token: SpannedToken, label_token: SpannedToken) -> Self {
        Self {
            goto_token,
            label_token,
        }
    }

    pub fn empty(label: String) -> Self {
        Self {
            goto_token: SpannedToken::create_empty(Token::Gosub),
            label_token: SpannedToken::create_empty(Token::Identifier(label)),
        }
    }

    pub fn get_goto_token(&self) -> &SpannedToken {
        &self.goto_token
    }

    pub fn get_label_token(&self) -> &SpannedToken {
        &self.label_token
    }

    /// Returns a reference to the get label of this [`GotoStatement`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_label(&self) -> &String {
        if let Token::Identifier(id) = &self.label_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub fn create_empty_statement(label: String) -> Statement {
        Statement::Goto(GotoStatement::empty(label))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LabelStatement {
    label_token: SpannedToken,
}

impl LabelStatement {
    pub fn new(label_token: SpannedToken) -> Self {
        Self { label_token }
    }

    pub fn empty(label: impl Into<String>) -> Self {
        Self {
            label_token: SpannedToken::create_empty(Token::Label(label.into())),
        }
    }

    pub fn get_label_token(&self) -> &SpannedToken {
        &self.label_token
    }

    /// Returns a reference to the get identifier of this [`IdentifierExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_label(&self) -> &String {
        if let Token::Label(id) = &self.label_token.token {
            return id;
        }
        panic!("Expected label token")
    }

    /// # Panics
    ///
    /// Panics if .
    pub fn get_comment_type(&self) -> CommentType {
        if let Token::Comment(id) = &self.label_token.token {
            return id.0;
        }
        panic!("Expected identifier token")
    }

    pub fn create_empty_statement(identifier: impl Into<String>) -> Statement {
        Statement::Label(LabelStatement::empty(identifier))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProcedureCallStatement {
    identifier_token: SpannedToken,
    lpar_token: SpannedToken,
    arguments: Vec<Expression>,
    rpar_token: SpannedToken,
}

impl ProcedureCallStatement {
    pub fn new(
        identifier_token: SpannedToken,
        leftpar_token: SpannedToken,
        arguments: Vec<Expression>,
        rightpar_token: SpannedToken,
    ) -> Self {
        Self {
            identifier_token,
            lpar_token: leftpar_token,
            arguments,
            rpar_token: rightpar_token,
        }
    }

    pub fn empty(identifier: impl Into<String>, arguments: Vec<Expression>) -> Self {
        Self {
            identifier_token: SpannedToken::create_empty(Token::Identifier(identifier.into())),
            lpar_token: SpannedToken::create_empty(Token::LPar),
            arguments,
            rpar_token: SpannedToken::create_empty(Token::RPar),
        }
    }

    pub fn get_identifier_token(&self) -> &SpannedToken {
        &self.identifier_token
    }
    pub fn get_lpar_token_token(&self) -> &SpannedToken {
        &self.lpar_token
    }

    pub fn get_arguments(&self) -> &Vec<Expression> {
        &self.arguments
    }

    pub fn get_arguments_mut(&mut self) -> &mut Vec<Expression> {
        &mut self.arguments
    }

    pub fn get_rpar_token_token(&self) -> &SpannedToken {
        &self.rpar_token
    }

    /// Returns a reference to the get identifier of this [`IdentifierExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_identifier(&self) -> &String {
        if let Token::Identifier(id) = &self.identifier_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub fn create_empty_statement(
        identifier: impl Into<String>,
        arguments: Vec<Expression>,
    ) -> Statement {
        Statement::Call(ProcedureCallStatement::empty(identifier, arguments))
    }
}

pub fn get_var_name(expr: &Expression) -> String {
    if let Expression::FunctionCall(call_expr) = expr {
        call_expr.get_identifier().clone()
    } else {
        expr.to_string()
    }
}

impl Statement {
    pub fn param_list_to_string(l: &Vec<Expression>) -> String {
        let mut res = String::new();
        for expr in l {
            if !res.is_empty() {
                res.push_str(", ");
            }
            res.push_str(expr.to_string().as_str());
        }
        res
    }

    pub fn try_boolean_conversion(expr: &Expression) -> Expression {
        match expr {
            Expression::Const(c) => match c.get_constant_value() {
                Constant::Integer(i) => {
                    if *i == PPL_TRUE {
                        ConstantExpression::create_empty_expression(Constant::Boolean(true))
                    } else {
                        ConstantExpression::create_empty_expression(Constant::Boolean(false))
                    }
                }
                _ => expr.clone(),
            },
            Expression::Parens(expr) => Statement::try_boolean_conversion(expr.get_expression()),
            Expression::Unary(un_expr) => {
                if !matches!(un_expr.get_op(), UnaryOp::Not) {
                    return expr.clone();
                }

                match un_expr.get_expression() {
                    Expression::Const(c) => match c.get_constant_value() {
                        Constant::Boolean(b) => {
                            Expression::Const(ConstantExpression::empty(Constant::Boolean(!b)))
                        }
                        _ => expr.clone(),
                    },
                    Expression::Unary(notexpr) => {
                        if matches!(notexpr.get_op(), UnaryOp::Not) {
                            return Statement::try_boolean_conversion(notexpr.get_expression());
                        }
                        UnaryExpression::create_empty_expression(
                            un_expr.get_op(),
                            Box::new(Statement::try_boolean_conversion(un_expr.get_expression())),
                        )
                    }
                    _ => expr.clone(),
                }
            }
            _ => expr.clone(),
        }
    }

    pub fn get_indent(indent: i32) -> String {
        let mut res = String::new();
        for _ in 0..indent {
            res.push_str("    ");
        }
        res
    }

    fn output_stmts(prg: &dyn ProgramContext, stmts: &Vec<Statement>, indent: i32) -> String {
        let mut res = String::new();
        for stmt in stmts {
            let (str, ind, modifier) = stmt.to_string(prg, indent);
            res.push_str(Statement::get_indent(ind + modifier).as_str());
            res.push_str(&str);
            res.push('\n');
        }

        //   Statement::ElseIf(cond) => (format!("{} ({}) {}", output_keyword("ElseIf"), Statement::out_bool_func(cond), output_keyword("Then")), indent, -1),

        res
    }

    fn output_if_stmts(
        prg: &dyn ProgramContext,
        stmts: &Vec<Statement>,
        else_if_blocks: &Vec<ElseIfBlock>,
        else_block: &Option<ElseBlock>,
        indent: i32,
    ) -> String {
        let mut res = String::new();

        res.push_str(&Statement::output_stmts(prg, stmts, indent));

        for else_if_block in else_if_blocks {
            res.push_str(&format!(
                "{} ({}) {}",
                output_keyword_indented(indent - 1, "ElseIf"),
                Statement::out_bool_func(&else_if_block.cond),
                output_keyword("Then")
            ));
            res.push('\n');
            res.push_str(&Statement::output_stmts(
                prg,
                else_if_block.get_statements(),
                indent,
            ));
        }
        if let Some(else_block) = else_block {
            res.push_str(&output_keyword_indented(indent - 1, "Else"));
            res.push('\n');
            res.push_str(&Statement::output_stmts(
                prg,
                else_block.get_statements(),
                indent,
            ));
        }
        res
    }

    fn output_case_blocks(
        prg: &dyn ProgramContext,
        case_blocks: &Vec<CaseBlock>,
        else_block: &Option<CaseBlock>,
        indent: i32,
    ) -> String {
        let mut res = String::new();

        for case_block in case_blocks {
            res.push_str(&format!(
                "{} {}",
                output_keyword_indented(indent - 1, "Case"),
                &case_block.expr
            ));
            res.push('\n');
            res.push_str(&Statement::output_stmts(
                prg,
                case_block.get_statements(),
                indent,
            ));
        }

        if let Some(else_block) = else_block {
            res.push_str(&output_keyword_indented(indent - 1, "Case Else"));
            res.push('\n');
            res.push_str(&Statement::output_stmts(
                prg,
                else_block.get_statements(),
                indent,
            ));
        }
        res
    }

    pub fn strip_outer_parens(exp: &Expression) -> &Expression {
        if let Expression::Parens(pexpr) = exp {
            pexpr.get_expression()
        } else {
            exp
        }
    }

    pub fn out_bool_func(expr: &Expression) -> String {
        Statement::strip_outer_parens(&Statement::try_boolean_conversion(
            Statement::strip_outer_parens(expr),
        ))
        .to_string()
    }

    pub fn to_string(&self, prg: &dyn ProgramContext, indent: i32) -> (String, i32, i32) // (str, indent, cur_line_inden_tmodifier)
    {
        match self {
            Statement::Comment(str) => (format!(";{str}"), indent, 0),
            Statement::Block(block_stmt) => (
                format!(
                    "{}\n{}{}",
                    output_keyword("Begin"),
                    Statement::output_stmts(prg, block_stmt.get_statements(), indent + 1),
                    output_keyword_indented(indent, "End")
                ),
                indent,
                0,
            ),
            Statement::While(while_stmt) => (
                format!(
                    "{} ({}) {}",
                    output_keyword("While"),
                    Statement::out_bool_func(while_stmt.get_condition()),
                    while_stmt.get_statement().to_string(prg, 0).0
                ),
                indent,
                0,
            ),
            Statement::If(if_stmt) => (
                format!(
                    "{} ({}) {}",
                    output_keyword("If"),
                    Statement::out_bool_func(if_stmt.get_condition()),
                    if_stmt.get_statement().to_string(prg, 0).0
                ),
                indent,
                0,
            ),
            Statement::IfThen(if_then_stmt) => (
                format!(
                    "{} ({}) {}\n{}{}",
                    output_keyword("If"),
                    Statement::out_bool_func(if_then_stmt.get_condition()),
                    output_keyword("Then"),
                    Statement::output_if_stmts(
                        prg,
                        if_then_stmt.get_statements(),
                        if_then_stmt.get_else_if_blocks(),
                        if_then_stmt.get_else_block(),
                        indent + 1
                    ),
                    output_keyword_indented(indent, "EndIf")
                ),
                indent,
                0,
            ),

            Statement::Select(select_stmt) => (
                format!(
                    "{} ({})\n{}{}",
                    output_keyword("Select Case"),
                    select_stmt.expr,
                    Statement::output_case_blocks(
                        prg,
                        select_stmt.get_case_blocks(),
                        select_stmt.get_case_else_block(),
                        indent + 1
                    ),
                    output_keyword_indented(indent, "EndSelect")
                ),
                indent,
                0,
            ),
            Statement::WhileDo(while_do_stmt) => (
                format!(
                    "{} ({}) {}\n{}{}",
                    output_keyword("While"),
                    Statement::out_bool_func(while_do_stmt.get_condition()),
                    output_keyword("Do"),
                    Statement::output_stmts(prg, while_do_stmt.get_statements(), indent + 1),
                    output_keyword_indented(indent, "EndWhile")
                ),
                indent,
                0,
            ),
            Statement::Break(_) => (output_keyword("Break"), indent, 0),
            Statement::Continue(_) => (output_keyword("Continue"), indent, 0),
            Statement::End(_) => (output_keyword("End"), indent, 0),
            Statement::Gosub(gosub_stmt) => (
                format!("{} {}", output_keyword("GoSub"), gosub_stmt.get_label()),
                indent,
                0,
            ),
            Statement::Return(_) => (output_keyword("Return"), indent, 0),
            Statement::Let(var, expr) => {
                let expected_type = prg.get_var_type(var.get_name());
                let expr2 = if expected_type == VariableType::Boolean {
                    Statement::try_boolean_conversion(expr)
                } else {
                    (**expr).clone()
                };
                (format!("{var} = {expr2}"), indent, 0)
            }
            Statement::Goto(goto_stmt) => (
                format!("{} {}", output_keyword("GoTo"), goto_stmt.get_label()),
                indent,
                0,
            ),
            Statement::For(for_stmt) => {
                let var_name = for_stmt.get_identifier();
                if let Some(step_expr) = for_stmt.get_step_expr() {
                    (
                        format!(
                            "{} {} = {} {} {} {} {}\n{}{}",
                            output_keyword("For"),
                            var_name,
                            for_stmt.get_start_expr(),
                            output_keyword("To"),
                            for_stmt.get_end_expr(),
                            output_keyword("Step"),
                            step_expr,
                            Statement::output_stmts(prg, for_stmt.get_statements(), indent + 1),
                            output_keyword_indented(indent, "Next")
                        ),
                        indent,
                        0,
                    )
                } else {
                    (
                        format!(
                            "{} {} = {} {} {}\n{}{}",
                            output_keyword("For"),
                            var_name,
                            for_stmt.get_start_expr(),
                            output_keyword("To"),
                            for_stmt.get_end_expr(),
                            Statement::output_stmts(prg, for_stmt.get_statements(), indent + 1),
                            output_keyword_indented(indent, "Next")
                        ),
                        indent,
                        0,
                    )
                }
            }
            Statement::Label(label_stmt) => (
                format!(
                    "\n{}:{}",
                    Statement::get_indent(indent - 1),
                    label_stmt.get_label()
                ),
                indent,
                -1,
            ),
            Statement::Call(call_stmt) => (
                format!(
                    "{}({})",
                    call_stmt.get_identifier(),
                    Statement::param_list_to_string(call_stmt.get_arguments())
                ),
                indent,
                0,
            ),
            /*
            Statement::Call(def, params) => {
                if params.is_empty() {
                    (output_keyword(def.name), indent, 0)
                } else {
                    (
                        format!(
                            "{} {}",
                            output_keyword(def.name),
                            Statement::param_list_to_string(params)
                        ),
                        indent,
                        0,
                    )
                }
            }*/
        }
    }
}
