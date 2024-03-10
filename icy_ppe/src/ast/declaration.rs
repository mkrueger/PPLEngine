use std::fmt;

use super::{
    Constant, ConstantExpression, Expression, FunctionCallExpression, IdentifierExpression,
    VariableType,
};

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
            VarInfo::Var0(name) => IdentifierExpression::create_empty_expression(name),
            VarInfo::Var1(name, vec) => {
                FunctionCallExpression::create_empty_expression(name.clone(), vec![vec.clone()])
            }
            VarInfo::Var2(name, vec, mat) => FunctionCallExpression::create_empty_expression(
                name.clone(),
                vec![vec.clone(), mat.clone()],
            ),
            VarInfo::Var3(name, vec, mat, cube) => FunctionCallExpression::create_empty_expression(
                name.clone(),
                vec![vec.clone(), mat.clone(), cube.clone()],
            ),
        }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn from(expr: &Expression) -> Self {
        match expr {
            Expression::Identifier(name) => VarInfo::Var0(name.get_identifier().clone()),
            Expression::Const(constant_expression) => {
                match constant_expression.get_constant_value() {
                    Constant::String(name) => VarInfo::Var0(name.clone()),
                    _ => panic!("can't translate const to var info {expr:?}"),
                }
            }

            Expression::FunctionCall(expr) => match expr.get_arguments().len() {
                1 => VarInfo::Var1(
                    expr.get_identifier().clone(),
                    expr.get_arguments()[0].clone(),
                ),
                2 => VarInfo::Var2(
                    expr.get_identifier().clone(),
                    expr.get_arguments()[0].clone(),
                    expr.get_arguments()[1].clone(),
                ),
                3 => VarInfo::Var3(
                    expr.get_identifier().clone(),
                    expr.get_arguments()[0].clone(),
                    expr.get_arguments()[2].clone(),
                    expr.get_arguments()[3].clone(),
                ),
                _ => panic!("can't translate func call t var info {expr:?}"),
            },
            Expression::BinaryExpression(_op, left, _right) => Self::from(left),
            Expression::Parens(expr) => Self::from(expr.get_expression()),
            Expression::Unary(_, expr) => Self::from(expr),
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

    /// Returns the get vector size of this [`VarInfo`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_vector_size(&self) -> usize {
        match self {
            VarInfo::Var0(_) => 0,
            VarInfo::Var1(_, vec) | VarInfo::Var2(_, vec, _) | VarInfo::Var3(_, vec, _, _) => {
                match vec {
                    Expression::Const(constant_expression) => {
                        match constant_expression.get_constant_value() {
                            Constant::Integer(size) => *size as usize,
                            _ => panic!("vector size not int {constant_expression:?}"),
                        }
                    }
                    _ => panic!(""),
                }
            }
        }
    }

    /// Returns the get matrix size of this [`VarInfo`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_matrix_size(&self) -> usize {
        match self {
            VarInfo::Var0(_) | VarInfo::Var1(_, _) => 0,
            VarInfo::Var2(_, _, mat) | VarInfo::Var3(_, _, mat, _) => match mat {
                Expression::Const(constant_expression) => {
                    match constant_expression.get_constant_value() {
                        Constant::Integer(size) => *size as usize,
                        _ => panic!("vector size not int {constant_expression:?}"),
                    }
                }
                _ => panic!("matrix size not int"),
            },
        }
    }

    /// Returns the get cube size of this [`VarInfo`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_cube_size(&self) -> usize {
        match self {
            VarInfo::Var0(_) | VarInfo::Var1(_, _) | VarInfo::Var2(_, _, _) => 0,
            VarInfo::Var3(_, _, _, cube) => match cube {
                Expression::Const(constant_expression) => {
                    match constant_expression.get_constant_value() {
                        Constant::Integer(size) => *size as usize,
                        _ => panic!("vector size not int {constant_expression:?}"),
                    }
                }
                _ => panic!("cube size not int"),
            },
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
                ConstantExpression::create_empty_expression(Constant::Integer(vs)),
            )],
        )
    }

    pub fn create_variable2(var_type: VariableType, name: String, vs: i32, ms: i32) -> Self {
        Declaration::Variable(
            var_type,
            vec![VarInfo::Var2(
                name,
                ConstantExpression::create_empty_expression(Constant::Integer(vs)),
                ConstantExpression::create_empty_expression(Constant::Integer(ms)),
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
                ConstantExpression::create_empty_expression(Constant::Integer(vs)),
                ConstantExpression::create_empty_expression(Constant::Integer(ms)),
                ConstantExpression::create_empty_expression(Constant::Integer(cs)),
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
