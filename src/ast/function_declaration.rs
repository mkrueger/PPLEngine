use std::fmt;

use crate::{interpreter::ProgramContext, tables::OpCode};

use super::{Declaration, Block, Statement, VariableType};

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration
{
    pub id: i32,
    pub declaration: Declaration,
    pub variable_declarations: Vec<Declaration>,
    pub block: Block,
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.declaration)
    }
}

impl FunctionDeclaration {
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
            if let Statement::Call(def, _params) = stmt {
                if def.opcode == OpCode::FEND || def.opcode == OpCode::FPCLR {
                    match &self.declaration {
                        Declaration::Procedure(name, _param) => {
                            res.push_str(format!("ENDPROC ;--{}", name).as_str());
                            continue;
                        }
                        Declaration::Function(name, _param, _t) => {
                            res.push_str(format!("ENDFUNC ;--{}", name).as_str());
                            continue;
                        }
                        _ => {}
                    }
                }
            }
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
        res.push('\n');

        res
    }
}


fn match_var_name(decl : &Declaration, var_name : &str) -> Option<VariableType>
{
    match decl {
        Declaration::Variable(var_type, name) => if *name == *var_name { return Some(*var_type); },
        Declaration::Variable1(var_type, name, _dim1) => if *name == *var_name { return Some(*var_type); },
        Declaration::Variable2(var_type, name, _dim1, _dim2) => if *name == *var_name { return Some(*var_type); },
        Declaration::Variable3(var_type, name, _dim1, _dim2, _dim3) => if *name == *var_name { return Some(*var_type); },
        _ => {  }
    }
    None
}

impl ProgramContext for FunctionDeclaration
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
