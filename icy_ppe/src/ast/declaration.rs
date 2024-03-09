use std::fmt;

use super::{Constant, Expression, VariableType};

#[derive(Debug, Clone, PartialEq)]
pub enum VarInfo {
    Var0(String),
    Var1(String, Expression),                         // vector
    Var2(String, Expression, Expression),             // matrix
    Var3(String, Expression, Expression, Expression), // cube
}

impl VarInfo {
    pub fn as_expr(&self) -> Expression {
        match &self {
            VarInfo::Var0(name) => Expression::Identifier(name.clone()),
            VarInfo::Var1(name, vec) => Expression::FunctionCall(name.clone(), vec![vec.clone()]),
            VarInfo::Var2(name, vec, mat) => {
                Expression::FunctionCall(name.clone(), vec![vec.clone(), mat.clone()])
            }
            VarInfo::Var3(name, vec, mat, cube) => {
                Expression::FunctionCall(name.clone(), vec![vec.clone(), mat.clone(), cube.clone()])
            }
        }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn from(expr: &Expression) -> Self {
        match expr {
            Expression::Const(Constant::String(name)) | Expression::Identifier(name) => {
                VarInfo::Var0(name.clone())
            }
            Expression::FunctionCall(name, vec) => match vec.len() {
                1 => VarInfo::Var1(name.clone(), vec[0].clone()),
                2 => VarInfo::Var2(name.clone(), vec[0].clone(), vec[1].clone()),
                3 => VarInfo::Var3(name.clone(), vec[0].clone(), vec[2].clone(), vec[3].clone()),
                _ => panic!("can't translate func call t var info {expr:?}"),
            },
            Expression::BinaryExpression(_op, left, _right) => Self::from(left),
            Expression::Parens(expr) | Expression::UnaryExpression(_, expr) => Self::from(expr),

            _ => panic!("unsupported expr {expr:?}"),
        }
    }

    pub fn get_name(&self) -> &String {
        match self {
            VarInfo::Var0(name)
            | VarInfo::Var1(name, _)
            | VarInfo::Var2(name, _, _)
            | VarInfo::Var3(name, _, _, _) => name,
        }
    }

    pub fn get_dims(&self) -> u8 {
        match self {
            VarInfo::Var0(_) => 0,
            VarInfo::Var1(_, _) => 1,
            VarInfo::Var2(_, _, _) => 2,
            VarInfo::Var3(_, _, _, _) => 3,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            VarInfo::Var0(_) => false,
            VarInfo::Var1(_, _) | VarInfo::Var2(_, _, _) | VarInfo::Var3(_, _, _, _) => true,
        }
    }

    pub fn rename(&mut self, new_name: String) {
        match self {
            VarInfo::Var0(name)
            | VarInfo::Var1(name, _)
            | VarInfo::Var2(name, _, _)
            | VarInfo::Var3(name, _, _, _) => *name = new_name,
        }
    }
}

impl fmt::Display for VarInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VarInfo::Var0(name) => write!(f, "{name}"),
            VarInfo::Var1(name, vs) => write!(f, "{name}({vs})"),
            VarInfo::Var2(name, vs, ms) => write!(f, "{name}({vs}, {ms})"),
            VarInfo::Var3(name, vs, ms, cs) => write!(f, "{name}({vs}, {ms}, {cs})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Variable(VariableType, Vec<VarInfo>),
    Function(String, Vec<Declaration>, VariableType),
    Procedure(String, Vec<Declaration>),
}

impl Declaration {
    /// Returns a reference to the get name of this [`Declaration`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_name(&self) -> &String {
        match self {
            Declaration::Variable(_, _) => {
                panic!("no name")
            }
            Declaration::Function(name, _, _) | Declaration::Procedure(name, _) => name,
        }
    }

    /// Returns the get return vartype of this [`Declaration`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_return_vartype(&self) -> i32 {
        match self {
            Declaration::Variable(_, _) => {
                panic!("no return type")
            }
            Declaration::Function(_, _, return_type) => *return_type as i32,
            Declaration::Procedure(_, _) => 0,
        }
    }
}

fn var_infos_to_string(vars: &Vec<VarInfo>) -> String {
    let mut result = String::new();

    for v in vars {
        if !result.is_empty() {
            result.push_str(", ");
        }
        result.push_str(&v.to_string());
    }

    result
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Declaration::Variable(var_type, name) => {
                write!(f, "{} {}", var_type, var_infos_to_string(name))
            }
            Declaration::Procedure(name, parameters) => write!(
                f,
                "DECLARE PROCEDURE {}({})",
                name,
                Declaration::declr_vec_to_string(parameters)
            ),
            Declaration::Function(name, parameters, return_type) => write!(
                f,
                "DECLARE FUNCTION {}({}) {}",
                name,
                Declaration::declr_vec_to_string(parameters),
                return_type
            ),
        }
    }
}

impl Declaration {
    fn declr_vec_to_string(list: &Vec<Declaration>) -> String {
        let mut res = String::new();
        for decl in list {
            if !res.is_empty() {
                res.push_str(", ");
            }
            res.push_str(decl.to_string().as_str());
        }
        res
    }

    pub fn create_variable(var_type: VariableType, name: String) -> Self {
        Declaration::Variable(var_type, vec![VarInfo::Var0(name)])
    }

    pub fn create_variable1(var_type: VariableType, name: String, vs: i32) -> Self {
        Declaration::Variable(
            var_type,
            vec![VarInfo::Var1(
                name,
                Expression::Const(Constant::Integer(vs)),
            )],
        )
    }

    pub fn create_variable2(var_type: VariableType, name: String, vs: i32, ms: i32) -> Self {
        Declaration::Variable(
            var_type,
            vec![VarInfo::Var2(
                name,
                Expression::Const(Constant::Integer(vs)),
                Expression::Const(Constant::Integer(ms)),
            )],
        )
    }

    pub fn create_variable3(
        var_type: VariableType,
        name: String,
        vs: i32,
        ms: i32,
        cs: i32,
    ) -> Self {
        Declaration::Variable(
            var_type,
            vec![VarInfo::Var3(
                name,
                Expression::Const(Constant::Integer(vs)),
                Expression::Const(Constant::Integer(ms)),
                Expression::Const(Constant::Integer(cs)),
            )],
        )
    }

    pub fn print_header(&self) -> String {
        match self {
            Declaration::Procedure(name, parameters) => format!(
                "PROCEDURE {}({})",
                name,
                Declaration::declr_vec_to_string(parameters)
            ),
            Declaration::Function(name, parameters, return_type) => format!(
                "FUNCTION {}({}) {}",
                name,
                Declaration::declr_vec_to_string(parameters),
                return_type
            ),
            Declaration::Variable(_, _) => "ERR".to_string(),
        }
    }
}
