use crate::{tables::{StatementDefinition, OpCode}, interpreter::ProgramContext, executable::VariableType};

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Comment(String),
    While(Box<Expression>, Box<Statement>),
    If(Box<Expression>, Box<Statement>),
    IfThen(Box<Expression>),
    Else,
    EndIf,
    DoWhile(Box<Expression>),
    EndWhile,
    For(String, Box<Expression>, Box<Expression>, Box<Expression>),
    Next,
    Break,
    Continue,
    Label(String),
    ProcedureCall(String, Vec<Expression>),
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
        match expr {
            Expression::Const(Constant::Integer(i)) => {
                if *i == 0 {
                    &Expression::Const(Constant::FALSE)
                } else {
                    &Expression::Const(Constant::TRUE)
                }
            }
            Expression::Parens(expr) => {
                Statement::try_boolean_conversion(&expr)
            }
            Expression::Not(notexpr) => {
                let converted_expression = Statement::try_boolean_conversion(&notexpr);
                match converted_expression {
                    Expression::Const(Constant::FALSE) => &Expression::Const(Constant::TRUE),
                    Expression::Const(Constant::TRUE) => &Expression::Const(Constant::FALSE),
                    Expression::Not(_notexpr2) => {
                        _notexpr2
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

    pub fn to_string(&self, prg: &dyn ProgramContext, indent: i32) -> (String, i32)
    {
        match self {
            Statement::Comment(str) => (format!(";{}", str), indent),
            Statement::While(cond, stmt) => (format!("WHILE ({}) {}", Statement::out_bool_func(cond), stmt.to_string(prg, 0).0), indent),
            Statement::If(cond, stmt) => (format!("IF ({}) {}", Statement::out_bool_func(cond), stmt.to_string(prg, 0).0), indent),
            Statement::IfThen(cond) => (format!("IF ({}) THEN", Statement::out_bool_func(cond)), indent + 1),
            Statement::Else => ("ELSE".to_string(), indent),
            Statement::EndIf => ("END IF".to_string(), indent - 1),
            Statement::DoWhile(cond) => (format!("WHILE ({}) DO", Statement::out_bool_func(cond)), indent + 1),
            Statement::EndWhile => ("END WHILE".to_string(), indent - 1),
            Statement::Break => ("BREAK".to_string(), indent),
            Statement::Continue => ("CONTINUE".to_string(), indent),
            Statement::For(var_name, from, to, step) => {
                let step = step.to_string();
                if step == "1" {
                    (format!("FOR {} = {} TO {}", var_name, from.to_string(), to.to_string()), indent + 1)
                } else {
                    (format!("FOR {} = {} TO {} STEP {}", var_name, from.to_string(), to.to_string(), step), indent + 1)
                }
            }
            Statement::Next => ("NEXT".to_string(), indent - 1),
            Statement::Label(str) => (format!("\n{}:{}", Statement::get_indent(indent - 1), str), indent),
            Statement::ProcedureCall(name, params) => (format!("{}({})", name, Statement::param_list_to_string(params)), indent),
            Statement::Call(def, params) => {
                let op: OpCode = unsafe { std::intrinsics::transmute(def.opcode) };
                match op {
                    OpCode::LET => {
                        let var = Statement::get_var_name(&params[0]);
                        let expected_type = prg.get_var_type(&var);
                        let mut expr = &params[1];
                        if expected_type == VariableType::Boolean {
                            expr = &Statement::try_boolean_conversion(expr);
                        }
                        (format!("LET {} = {}", var.as_str(), expr.to_string().as_str()), indent)
                    }
                    OpCode::IF => {
                        (format!("IF ({})", params[0].to_string().as_str()), indent + 1)
                    }
                    _ => {
                        if params.is_empty() {
                            (def.name.to_string(), indent)
                        } else {
                            (format!("{} {}", def.name, Statement::param_list_to_string(params)), indent)
                        }
                    }
                }
            }
        }
    }
}
