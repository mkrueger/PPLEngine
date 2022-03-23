use super::{Statement, Program};


#[derive(Debug, Clone, PartialEq)]
pub struct Block
{
    pub statements: Vec<Statement>,
}

impl Block
{
    pub fn new() -> Self
    {
        Block {
            statements: vec![]
        }
    }

    pub fn to_string(&self, prg: &Program) -> std::string::String
    {
        let mut result = std::string::String::new();
        let mut indent= 0;
        for s in &self.statements {
            let out = s.to_string(prg, indent);
            if indent > out.1 {
                indent = out.1;
            }
            for _ in 0..(indent + out.2) {
                result.push_str("    ");
            }
            result.push_str(out.0.as_str());
            indent = out.1;
            result.push('\n');
        }
        result
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}
