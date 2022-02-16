use crate::{tables::{StatementDefinition, PPL_TRUE}, interpreter::ProgramContext};

use super::*;

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
    For(String, Box<Expression>, Box<Expression>, Box<Expression>),
    Next,
    Break,
    Continue,
    Gosub(String),
    Return,
    Stop,
    Let(String, Box<Expression>),
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
                    &Expression::Const(Constant::TRUE)
                } else {
                    &Expression::Const(Constant::FALSE)
                }
            }
            Expression::Parens(expr) => {
                Statement::try_boolean_conversion(&expr)
            }
            Expression::Not(notexpr) => {
                match &**notexpr {
                    Expression::Const(Constant::FALSE) => &Expression::Const(Constant::TRUE),
                    Expression::Const(Constant::TRUE) => &Expression::Const(Constant::FALSE),
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
/* 
    fn get_var_name(expr : &Expression) -> String {
        match expr {
            Expression::Dim1(expr, _vec) => Statement::get_var_name(expr),
            Expression::Dim2(expr, _vec, _mat) => Statement::get_var_name(expr),
            Expression::Dim3(expr, _vec, _mat, _cube) => Statement::get_var_name(expr),
            _ => expr.to_string()
        }
    }*/

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
            Statement::While(cond, stmt) => (format!("WHILE ({}) {}", Statement::out_bool_func(cond), stmt.to_string(prg, 0).0), indent, 0),
            Statement::If(cond, stmt) => (format!("IF ({}) {}", Statement::out_bool_func(cond), stmt.to_string(prg, 0).0), indent, 0),
            Statement::IfThen(cond) => (format!("IF ({}) THEN", Statement::out_bool_func(cond)), indent + 1, 0),
            Statement::ElseIf(cond) => (format!("ELSEIF ({}) THEN", Statement::out_bool_func(cond)), indent, -1),
            Statement::Else => ("ELSE".to_string(), indent, -1),
            Statement::EndIf => ("ENDIF".to_string(), indent - 1, 0),
            Statement::DoWhile(cond) => (format!("WHILE ({}) DO", Statement::out_bool_func(cond)), indent + 1, 0),
            Statement::EndWhile => ("ENDWHILE".to_string(), indent - 1, 0),
            Statement::Break => ("BREAK".to_string(), indent, 0),
            Statement::Continue => ("CONTINUE".to_string(), indent, 0),
            Statement::End => ("END".to_string(), indent, 0),
            Statement::Gosub(label) => (format!("GOSUB {}", label.to_string()), indent, 0),
            Statement::Return => ("RETURN".to_string(), indent, 0),
            Statement::Let(var, expr) => {
                let expected_type = prg.get_var_type(&var);
                let expr2;
                if expected_type == VariableType::Boolean {
                    expr2 = Statement::try_boolean_conversion(&**expr);
                } else {
                    expr2 = &**expr;
                }
                (format!("{} = {}", var.to_string(), expr2.to_string()), indent, 0)
            },
            Statement::Goto(label) => (format!("GOTO {}", label.to_string()), indent, 0),
            Statement::Inc(expr) => (format!("INC {}", expr.to_string()), indent, 0),
            Statement::Dec(expr) => (format!("DEC {}", expr.to_string()), indent, 0),
            Statement::For(var_name, from, to, step) => {
                let step = step.to_string();
                if step == "1" {
                    (format!("FOR {} = {} TO {}", var_name, from.to_string(), to.to_string()), indent + 1, 0)
                } else {
                    (format!("FOR {} = {} TO {} STEP {}", var_name, from.to_string(), to.to_string(), step), indent + 1, 0)
                }
            }
            Statement::Next => ("NEXT".to_string(), indent - 1, 0),
            Statement::Stop => ("STOP".to_string(), indent, 0),
            Statement::Label(str) => (format!("\n{}:{}", Statement::get_indent(indent - 1), str), indent, -1),
            Statement::ProcedureCall(name, params) => (format!("{}({})", name, Statement::param_list_to_string(params)), indent, 0),
            Statement::Call(def, params) => {
                if params.is_empty() {
                    (def.name.to_string(), indent, 0)
                } else {
                    (format!("{} {}", def.name, Statement::param_list_to_string(params)), indent, 0)
                }
            }
        }
    }
}
