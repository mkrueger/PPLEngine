use std::collections::HashMap;

use crate::{
    ast::{walk_for_stmt, AstVisitor, Expression},
    executable::OpCode,
};

#[derive(Default)]
pub struct RenameScanVistitor {
    pub rename_map: HashMap<unicase::Ascii<String>, unicase::Ascii<String>>,
    cur_index_var: usize,
    file_names: usize,
}

const INDEX_VARS: [&str; 4] = ["i", "j", "k", "l"];

impl AstVisitor<()> for RenameScanVistitor {
    fn visit_for_statement(&mut self, for_stmt: &crate::ast::ForStatement) {
        let var_name = for_stmt.get_identifier();
        if !self.rename_map.contains_key(var_name) && self.cur_index_var < INDEX_VARS.len() {
            self.rename_map.insert(
                var_name.clone(),
                unicase::Ascii::new(INDEX_VARS[self.cur_index_var].to_string()),
            );
            self.cur_index_var += 1;
        }
        walk_for_stmt(self, for_stmt);
    }

    fn visit_predefined_call_statement(&mut self, call: &crate::ast::PredefinedCallStatement) {
        match &call.get_func().opcode {
            OpCode::FOPEN | OpCode::DELETE | OpCode::DISPFILE => {
                if let Expression::Identifier(id) = &call.get_arguments()[0] {
                    let var_name = id.get_identifier();
                    if !self.rename_map.contains_key(var_name) {
                        self.file_names += 1;
                        self.rename_map.insert(
                            var_name.clone(),
                            unicase::Ascii::new(format!("fileName{}", self.file_names)),
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

use crate::ast::AstVisitorMut;
#[derive(Default)]
pub struct RenameVisitor {
    pub rename_map: HashMap<unicase::Ascii<String>, unicase::Ascii<String>>,
}

impl RenameVisitor {
    pub fn new(rename_map: HashMap<unicase::Ascii<String>, unicase::Ascii<String>>) -> Self {
        Self { rename_map }
    }
}

impl AstVisitorMut for RenameVisitor {
    fn visit_identifier(&mut self, id: &unicase::Ascii<String>) -> unicase::Ascii<String> {
        if let Some(rename) = self.rename_map.get(id) {
            rename.clone()
        } else {
            id.clone()
        }
    }
}
