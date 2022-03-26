use std::fmt;

use crate::{interpreter::ProgramContext};

use super::{Declaration, Block, VariableType};

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionImplementation
{
    pub id: i32,
    pub declaration: Declaration,
    pub variable_declarations: Vec<Declaration>,
    pub block: Block,
}

impl fmt::Display for FunctionImplementation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.declaration)
    }
}

impl FunctionImplementation {
    pub fn print_content(&self) -> String
    {
        let mut res = self.declaration.print_header();
        res.push('\n');
        let mut indent = 1;

        if !self.variable_declarations.is_empty() {
            for v in &self.variable_declarations {
                res.push_str("    ");
                res.push_str(&v.to_string());
                res.push('\n');
            }
            res.push('\n');
        }

        for stmt in &self.block.statements {
            let out = stmt.to_string(self, indent);
            if indent > out.1 {
                indent = out.1;
            }
            for _ in 0..(indent + out.2) {
                res.push_str("    ");
            }
            res.push_str(out.0.as_str());
            indent = out.1;
            res.push('\n');
        }
        if let Declaration::Function(name, _, _) = &self.declaration {
            res.push_str(format!("ENDFUNC ;--{}", name).as_str());

        } else if let Declaration::Procedure(name, _)= &self.declaration {
            res.push_str(format!("ENDPROC ;--{}", name).as_str());
        }


        res.push('\n');

        res
    }
}

fn match_var_name(decl : &Declaration, var_name : &str) -> Option<VariableType>
{
    match decl {
        Declaration::Variable(var_type, name) => {
            for info in name { 
                if *info.get_name() == *var_name { 
                    return Some(*var_type); 
                } 
            }
        }
        _ => {  }
    }
    None
}

impl ProgramContext for FunctionImplementation
{
    fn get_var_type(&self, var_name: &str) -> VariableType
    {
        if let Declaration::Function(func_name, param, func_type) = &self.declaration {
            if var_name == func_name {
                return *func_type;
            }
            for p in param {
                if let Some(t) = match_var_name(p, var_name) {
                    return t;
                }
            }
        }
        for decl in &self.variable_declarations {
            if let Some(t) = match_var_name(decl, var_name) {
                return t;
            }
        }
        VariableType::Unknown
    }
}
