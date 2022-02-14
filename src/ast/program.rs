use crate::interpreter::FunctionDeclaration;
use super::*;

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
        res.push_str("; Entrypoint\n");

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
