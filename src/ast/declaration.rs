use std::fmt;

use super::VariableType;


#[derive(Debug, Clone, PartialEq)]
pub enum VarInfo {
    Var0(String),
    Var1(String, i32),          // vector
    Var2(String, i32, i32),     // matrix
    Var3(String, i32, i32, i32) // cube
}

impl VarInfo {
    pub fn get_name(&self) -> &String {
        match self {
            VarInfo::Var0(name) => name,
            VarInfo::Var1(name, _) => name,
            VarInfo::Var2(name, _, _) => name,
            VarInfo::Var3(name, _, _, _) => name,
        }
    }
}

impl fmt::Display for VarInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VarInfo::Var0(name) => write!(f, "{}", name),
            VarInfo::Var1(name, vs) => write!(f, "{}({})", name, vs),
            VarInfo::Var2(name, vs, ms) => write!(f, "{}({}, {})", name, vs, ms),
            VarInfo::Var3(name, vs, ms, cs) => write!(f, "{}({}, {}, {})", name, vs, ms, cs)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Variable(VariableType, Vec<VarInfo>),
    Function(String, Vec<Declaration>, VariableType),
    Procedure(String, Vec<Declaration>),
}

fn var_infos_to_string(vars: &Vec<VarInfo>) -> String
{
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
            Declaration::Variable(var_type, name) => write!(f, "{} {}", var_type, var_infos_to_string(name)),
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

    
    pub fn create_variable(var_type: VariableType, name: String) -> Self
    {
        Declaration::Variable(var_type, vec![VarInfo::Var0(name)])
    }

    pub fn create_variable1(var_type: VariableType, name: String, vs: i32) -> Self
    {
        Declaration::Variable(var_type, vec![VarInfo::Var1(name, vs)])
    }

    pub fn create_variable2(var_type: VariableType, name: String, vs: i32, ms: i32) -> Self
    {
        Declaration::Variable(var_type, vec![VarInfo::Var2(name, vs, ms)])
    }

    pub fn create_variable3(var_type: VariableType, name: String, vs: i32, ms: i32, cs: i32) -> Self
    {
        Declaration::Variable(var_type, vec![VarInfo::Var3(name, vs, ms, cs)])
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
