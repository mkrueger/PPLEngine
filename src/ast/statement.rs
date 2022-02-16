use crate::{tables::{StatementDefinition, PPL_TRUE}, interpreter::ProgramContext};

use super::*;
use crate::output_keyword;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Comment(String),
    End,
    While(Box<Expression>, Box<Statement>),
    If(Box<Expression>, Box<Statement>),
    IfThen(Box<Expression>),
    ElseIf(Box<Expression>),
    Else,
    EndIf,
    DoWhile(Box<Expression>),
    EndWhile,
    For(Box<Expression>, Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Next,
    Break,
    Continue,
    Gosub(String),
    Return,
    Stop,
    EndFunc,
    EndProc,
    Let(Box<Expression>, Box<Expression>),
    Goto(String),
    Label(String),
    ProcedureCall(String, Vec<Expression>),
    Inc(String),
    Dec(String),
    Call(&'static StatementDefinition<'static>, Vec<Expression>),
}

impl Statement
{
    pub fn param_list_to_string(l: &Vec<Expression>) -> String
    {
        let mut res = String::new();
        for expr in l {
            if res.len() > 0 {
                res.push_str(", ");
            }
            res.push_str(expr.to_string().as_str());
        }
        res
    }

    fn try_boolean_conversion(expr: &Expression) -> &Expression
    {
        match Statement::strip_outer_parens(expr) {
            Expression::Const(Constant::Integer(i)) => {
                if *i == PPL_TRUE {
                    &Expression::Const(Constant::Boolean(true))
                } else {
                    &Expression::Const(Constant::Boolean(false))
                }
            }
            Expression::Parens(expr) => {
                Statement::try_boolean_conversion(&expr)
            }
            Expression::Not(notexpr) => {
                match &**notexpr {
                    Expression::Const(Constant::Boolean(false)) => &Expression::Const(Constant::Boolean(true)),
                    Expression::Const(Constant::Boolean(true)) => &Expression::Const(Constant::Boolean(false)),
                    Expression::Not(notexpr2) => {
                        Statement::try_boolean_conversion(&*notexpr2)
                    },
                    _ => expr
                }
            }
            _ => { expr }
        }
    }

    pub fn get_indent(indent : i32) -> String
    {
        let mut res = String::new();
        for _ in 0..indent {
            res.push_str("    ");
        }
        res
    }

    fn get_var_name(expr : &Expression) -> String {
        match expr {
            Expression::Dim1(expr, _vec) => Statement::get_var_name(expr),
            Expression::Dim2(expr, _vec, _mat) => Statement::get_var_name(expr),
            Expression::Dim3(expr, _vec, _mat, _cube) => Statement::get_var_name(expr),
            _ => expr.to_string()
        }
    }

    fn strip_outer_parens(exp : &Expression) -> &Expression
    {
        if let Expression::Parens(pexpr) = exp {
            &**pexpr
        } else {
            exp
        }
    }

    pub fn out_bool_func(expr : &Box<Expression>) -> String
    {
        Statement::strip_outer_parens(&Statement::try_boolean_conversion(Statement::strip_outer_parens(expr))).to_string()
    }

    pub fn to_string(&self, prg: &dyn ProgramContext, indent: i32) -> (String, i32, i32) // (str, indent, cur_line_inden_tmodifier)
    {
        match self {
            Statement::Comment(str) => (format!(";{}", str), indent, 0),
            Statement::While(cond, stmt) => (format!("{} ({}) {}", output_keyword("While"), Statement::out_bool_func(cond), stmt.to_string(prg, 0).0), indent, 0),
            Statement::If(cond, stmt) => (format!("{} ({}) {}", output_keyword("If"), Statement::out_bool_func(cond), stmt.to_string(prg, 0).0), indent, 0),
            Statement::IfThen(cond) => (format!("{} ({}) {}", output_keyword("If"), Statement::out_bool_func(cond), output_keyword("Then")), indent + 1, 0),
            Statement::ElseIf(cond) => (format!("{} ({}) {}", output_keyword("ElseIf"), Statement::out_bool_func(cond), output_keyword("Then")), indent, -1),
            Statement::Else => (output_keyword("Else"), indent, -1),
            Statement::EndIf => (output_keyword("EndIf"), indent - 1, 0),
            Statement::DoWhile(cond) => (format!("{} ({}) {}", output_keyword("While"), Statement::out_bool_func(cond), output_keyword("Do")), indent + 1, 0),
            Statement::EndWhile => (output_keyword("EndWhile"), indent - 1, 0),
            Statement::Break => (output_keyword("Break"), indent, 0),
            Statement::Continue => (output_keyword("Continue"), indent, 0),
            Statement::End => (output_keyword("End"), indent, 0),
            Statement::Gosub(label) => (format!("{} {}", output_keyword("GoSub"), label.to_string()), indent, 0),
            Statement::Return => (output_keyword("Return"), indent, 0),
            Statement::EndFunc => (output_keyword("EndFunc"), indent, 0),
            Statement::EndProc => (output_keyword("EndProc"), indent, 0),
            Statement::Let(var, expr) => {
                let expected_type = prg.get_var_type(&Statement::get_var_name(var));
                let expr2;
                if expected_type == VariableType::Boolean {
                    expr2 = Statement::try_boolean_conversion(&**expr);
                } else {
                    expr2 = &**expr;
                }
                (format!("{} = {}", var.to_string(), expr2.to_string()), indent, 0)
            },
            Statement::Goto(label) => (format!("{} {}", output_keyword("GoTo"), label.to_string()), indent, 0),
            Statement::Inc(expr) => (format!("{} {}", output_keyword("Inc"), expr.to_string()), indent, 0),
            Statement::Dec(expr) => (format!("{} {}", output_keyword("Dec"), expr.to_string()), indent, 0),
            Statement::For(var_name, from, to, step) => {
                let var_name = &Statement::get_var_name(var_name);
                if let Some(s) = step {
                    (format!("{} {} = {} {} {} {} {}", output_keyword("For"), var_name, from.to_string(), output_keyword("To"), to.to_string(), output_keyword("Step"), s.to_string()), indent + 1, 0)
                } else {
                    (format!("{} {} = {} {} {}",  output_keyword("For"), var_name, from.to_string(), output_keyword("To"), to.to_string()), indent + 1, 0)
                }
            }
            Statement::Next => (output_keyword("Next"), indent - 1, 0),
            Statement::Stop => (output_keyword("Stop"), indent, 0),
            Statement::Label(str) => (format!("\n{}:{}", Statement::get_indent(indent - 1), str), indent, -1),
            Statement::ProcedureCall(name, params) => (format!("{}({})", name, Statement::param_list_to_string(params)), indent, 0),
            Statement::Call(def, params) => {
                if params.is_empty() {
                    (output_keyword(def.name), indent, 0)
                } else {
                    (format!("{} {}", output_keyword(def.name), Statement::param_list_to_string(params)), indent, 0)
                }
            }
        }
    }
}
