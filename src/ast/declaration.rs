use std::fmt;

use super::VariableType;

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Variable(VariableType, String),
    Variable1(VariableType, String, i32),
    Variable2(VariableType, String, i32, i32),
    Variable3(VariableType, String, i32, i32, i32),
    Function(String, Vec<Declaration>, VariableType),
    Procedure(String, Vec<Declaration>),
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Declaration::Variable(var_type, name) => write!(f, "{} {}", var_type, name),
            Declaration::Variable1(var_type, name, dim1) => write!(f, "{} {}({})", var_type, name, dim1),
            Declaration::Variable2(var_type, name, dim1, dim2) => write!(f, "{} {}({}, {})", var_type, name, dim1, dim2),
            Declaration::Variable3(var_type, name, dim1, dim2, dim3) => write!(f, "{} {}({}, {}, {})", var_type, name, dim1, dim2, dim3),
            Declaration::Procedure(name, parameters) => write!(f, "DECLARE PROCEDURE {}({})", name, Declaration::declr_vec_to_string(parameters)),
            Declaration::Function(name, parameters, return_type) => write!(f, "DECLARE FUNCTION {}({}) {}", name, Declaration::declr_vec_to_string(parameters), return_type),
        }
    }
}

impl Declaration {
    fn declr_vec_to_string(list: &Vec<Declaration>) -> String
    {
        let mut res = String::new();
        for decl in list {
            if !res.is_empty() {
                res.push_str(", ");
            }
            res.push_str(decl.to_string().as_str());
        }
        res
    }

    

    pub fn print_header(&self) -> String
    {
        match self {
            Declaration::Procedure(name, parameters) => format!("PROCEDURE {}({})", name, Declaration::declr_vec_to_string(parameters)),
            Declaration::Function(name, parameters, return_type) => format!("FUNCTION {}({}) {}", name, Declaration::declr_vec_to_string(parameters), return_type),
            _ => { "ERR".to_string() }
        }
    }
}
