use super::{AstNode, AstVisitor, AstVisitorMut, Statement};
use std::{fmt, path::PathBuf};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub nodes: Vec<AstNode>,
    pub file_name: PathBuf,
    pub errors: Vec<crate::parser::Error>,
    pub warnings: Vec<crate::parser::ParserWarning>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            nodes: vec![],
            file_name: PathBuf::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    pub fn visit<T: Default, V: AstVisitor<T>>(&self, visitor: &mut V) {
        visitor.visit_program(self);
    }

    pub fn visit_mut<T: Default, V: AstVisitorMut<T>>(&mut self, visitor: &mut V) {
        visitor.visit_program(self);
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut output_visitor = crate::ast::output_visitor::OutputVisitor::default();
        self.visit(&mut output_visitor);

        write!(f, "{}", output_visitor.output)
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}
