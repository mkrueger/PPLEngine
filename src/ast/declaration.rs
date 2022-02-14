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

impl Declaration {
    fn declr_vec_to_string(list: &Vec<Declaration>) -> String
    {
        let mut res = String::new();
        for decl in list {
            if res.len() > 0 {
                res.push_str(", ");
            }
            res.push_str(decl.to_string().as_str());
        }
        res
    }

    pub fn to_string(&self) -> String
    {
        match self {
            Declaration::Variable(var_type, name) => format!("{} {}", var_type.to_string(), name),
            Declaration::Variable1(var_type, name, dim1) => format!("{} {}({})", var_type.to_string(), name, dim1),
            Declaration::Variable2(var_type, name, dim1, dim2) => format!("{} {}({}, {})", var_type.to_string(), name, dim1, dim2),
            Declaration::Variable3(var_type, name, dim1, dim2, dim3) => format!("{} {}({}, {}, {})", var_type.to_string(), name, dim1, dim2, dim3),
            Declaration::Procedure(name, parameters) => format!("DECLARE PROCEDURE {}({})", name, Declaration::declr_vec_to_string(parameters)),
            Declaration::Function(name, parameters, return_type) => format!("DECLARE FUNCTION {}({}) {}", name, Declaration::declr_vec_to_string(parameters), return_type.to_string()),
        }
    }

    pub fn print_header(&self) -> String
    {
        match self {
            Declaration::Procedure(name, parameters) => format!("PROCEDURE {}({})", name, Declaration::declr_vec_to_string(parameters)),
            Declaration::Function(name, parameters, return_type) => format!("FUNCTION {}({}) {}", name, Declaration::declr_vec_to_string(parameters), return_type.to_string()),
            _ => { "ERR".to_string() }
        }
    }
}
