use super::{AstVisitor, Implementations, Statement};
use std::path::PathBuf;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub implementations: Vec<Implementations>,
    pub file_name: PathBuf,
    pub errors: Vec<crate::parser::Error>,
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
        visitor.visit_program(self);
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}
