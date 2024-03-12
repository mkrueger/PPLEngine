use super::{ AstVisitor, Implementations, Statement};
use std::{fmt, path::PathBuf};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub implementations: Vec<Implementations>,
    pub file_name: PathBuf,
    pub errors: Vec<crate::parser::Error>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        /*
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

        res.push_str(&self.block.to_string(self));

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
        }*/
        writeln!(f, "TODO")
    }
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: vec![],
            implementations: vec![],
            file_name: PathBuf::new(),
            errors: Vec::new(),
        }
    }

    pub fn visit<T: Default, V: AstVisitor<T>>(&self, visitor: &mut V) {
        for stmt in &self.statements {
            stmt.visit(visitor);
        }
        for impls in &self.implementations {
            impls.visit(visitor);
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}
