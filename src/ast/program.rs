use std::fmt;

use crate::interpreter::ProgramContext;

use super::{Declaration, Block, FunctionImplementation, VariableType};


#[derive(Debug, PartialEq)]
pub struct Program
{
    pub declarations: Vec<Declaration>,
    pub function_implementations: Vec<FunctionImplementation>,
    pub procedure_implementations: Vec<FunctionImplementation>,
    pub main_block: Block,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut res = String::new();
        res.push_str("; ---------------------------------------\n");
        res.push_str("; PCBoard programming language decompiler\n");
        res.push_str("; ---------------------------------------\n");

        if !self.function_implementations.is_empty() || !self.procedure_implementations.is_empty() {
            res.push_str("; Function declarations\n");
        }
        for v in &self.function_implementations {
            res.push_str(&v.to_string());
            res.push('\n');
        }
        for v in &self.procedure_implementations {
            res.push_str(&v.to_string());
            res.push('\n');
        }
        for v in &self.declarations {
            res.push_str(&v.to_string());
            res.push('\n');
        }

        res.push_str(&self.main_block.to_string(self));

        if !self.function_implementations.is_empty() || !self.procedure_implementations.is_empty() {
            res.push_str("; Function implementations\n");
        }
        for v in &self.function_implementations {
            res.push_str(v.print_content().as_str());
            res.push('\n');
        }

        for v in &self.procedure_implementations {
            res.push_str(v.print_content().as_str());
            res.push('\n');
        }
        writeln!(f, "{}", res)
    }
}


impl Program
{
    pub fn new() -> Self
    {
        Program {
            declarations: vec![],
            main_block: Block {
                statements: vec![]
            },
            function_implementations: vec![],
            procedure_implementations: vec![],
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramContext for Program
{
    fn get_var_type(&self, var_name: &str) -> VariableType
    {
        for decl in &self.declarations {
            match decl {
                Declaration::Variable(var_type, var_infos) => {
                    for var_info in var_infos {
                        if *var_info.get_name() == *var_name { return *var_type; }
                    }
                
                },
                _ => {}
            }
        }
        VariableType::Unknown
    }
}
