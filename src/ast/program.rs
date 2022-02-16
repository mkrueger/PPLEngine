use crate::interpreter::ProgramContext;

use super::*;

#[derive(Debug, PartialEq)]
pub struct Program
{
    pub variable_declarations: Vec<Declaration>,
    pub function_declarations: Vec<Box<FunctionDeclaration>>,
    pub procedure_declarations: Vec<Box<FunctionDeclaration>>,
    pub main_block: Block,
}

impl Program
{
    pub fn new() -> Self
    {
        Program {
            variable_declarations: vec![],
            main_block: Block {
                statements: vec![]
            },
            function_declarations: vec![],
            procedure_declarations: vec![],
        }
    }

    pub fn to_string(&self) -> String
    {
        let mut res = String::new();
        res.push_str("; ---------------------------------------\n");
        res.push_str("; PCBoard programming language decompiler\n");
        res.push_str("; ---------------------------------------\n");

        if !self.function_declarations.is_empty() || !self.procedure_declarations.is_empty() {
            res.push_str("; Function declarations\n");
        }
        for v in &self.function_declarations {
            res.push_str(&v.to_string());
            res.push('\n');
        }
        for v in &self.procedure_declarations {
            res.push_str(&v.to_string());
            res.push('\n');
        }
        for v in &self.variable_declarations {
            res.push_str(&v.to_string());
            res.push('\n');
        }

        res.push_str(&self.main_block.to_string(self));

        if !self.function_declarations.is_empty() || !self.procedure_declarations.is_empty() {
            res.push_str("; Function implementations\n");
        }
        for v in &self.function_declarations {
            res.push_str(v.print_content().as_str());
            res.push('\n');
        }

        for v in &self.procedure_declarations {
            res.push_str(&v.print_content().as_str());
            res.push('\n');
        }
        res
    }
}

impl ProgramContext for Program
{
    fn get_var_type(&self, var_name: &String) -> VariableType
    {
        for decl in &self.variable_declarations {
            match decl {
                Declaration::Variable(var_type, name) => if *name == *var_name { return *var_type; },
                Declaration::Variable1(var_type, name, _dim1) => if *name == *var_name { return *var_type; },
                Declaration::Variable2(var_type, name, _dim1, _dim2) => if *name == *var_name { return *var_type; },
                Declaration::Variable3(var_type, name, _dim1, _dim2, _dim3) => if *name == *var_name { return *var_type; },
                _ => {}
            }
        }
        VariableType::Unknown
    }
}
