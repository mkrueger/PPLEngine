use super::{AstNode, AstVisitor, AstVisitorMut};
use std::{fmt, path::PathBuf};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub nodes: Vec<AstNode>,
    pub file_name: PathBuf,
    pub errors: Vec<crate::parser::Error>,
    pub warnings: Vec<crate::parser::ParserWarning>,

    pub require_user_variables: bool,
}

impl Program {
    pub fn new() -> Self {
        Program {
            nodes: vec![],
            file_name: PathBuf::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
            require_user_variables: false,
        }
    }

    pub fn visit<T: Default, V: AstVisitor<T>>(&self, visitor: &mut V) {
        visitor.visit_program(self);
    }

    #[must_use]
    pub fn visit_mut<V: AstVisitorMut>(&self, visitor: &mut V) -> Self {
        visitor.visit_program(self)
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
