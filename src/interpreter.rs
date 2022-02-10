use crate::parser;
use crate::parser::*;
use std::string::String;

pub struct Block
{
    pub statements: Vec<Statement>,

    label_stack: Vec<String>
}
impl Block
{
    pub fn new() -> Self
    {
        Block {
            statements : vec![],
            label_stack : vec![]
        }
    }

    pub fn mark_label(&mut self, label : String)
    {
        self.label_stack.push(label);
    }

    pub fn to_string(&self) -> std::string::String
    {
        let mut result = std::string::String::new();
        for s in &self.statements {
            result.push_str(s.to_string().as_str());
            result.push_str("\n");
        }
        result
    }
}
pub struct FunctionDeclaration
{
    pub declaration: parser::Declaration,
    pub block: Block
}

use std::fmt;

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{})", self.declaration.to_string())
    }
}

trait ProgramContext
{
    fn print(&mut self, str : String);
}

pub struct Program
{
    pub variable_declarations: Vec<parser::Declaration>,
    pub main_block: Block,
    pub function_declarations: Vec<FunctionDeclaration>,
    pub procedure_declarations: Vec<FunctionDeclaration>
}


struct TestContext
{
    output : String
}

impl ProgramContext for TestContext
{
    fn print(&mut self, str : String)
    {
        self.output.push_str(str.as_str());
    }
}

impl Program
{
    pub fn new() -> Self
    {
        Program {
            variable_declarations: vec![],
            main_block: Block {
                statements: vec![],
                label_stack: vec![]
            },
            function_declarations: vec![],
            procedure_declarations: vec![]
        }
    }

    fn execute_statement(&self, ctx : &mut dyn ProgramContext, stmt : &Statement)
    {
        match stmt {
            Statement::PRINT(vec) => {
                for expr in vec {
                    ctx.print(expr.to_string());
                }
            },

            Statement::PRINTLN(vec) => {
                match vec {
                    Some(t) => {
                        for expr in t {
                            ctx.print(expr.to_string());
                        }
                        ctx.print("\n".to_string());
                    },
                    _ => {}
                }
            },
            _ => {}
        }
    }

    pub fn run(&self, ctx : &mut dyn ProgramContext)
    {
        for stmt in &self.main_block.statements {
            self.execute_statement(ctx, stmt);
        }
    }

    pub fn to_string(&self) -> String
    {
        let mut res = String::new();
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
        res.push_str(&self.main_block.to_string());


        res
    }
}

fn parse_program(input : &str) -> Program
{
    let stmt = parse_statement(input).unwrap();

    Program {
        variable_declarations: vec![],
        main_block: Block {
            statements: vec![stmt.1],
            label_stack: vec![]
        },
        function_declarations: vec![],
        procedure_declarations: vec![]
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_println() {
        let mut ctx = TestContext { output: String::new() };
        parse_program("PRINTLN 1, 2, 3, \"Hello World\"").run(&mut ctx);
        assert_eq!("123\"Hello World\"\n".to_string(), ctx.output);

        ctx = TestContext { output: String::new() };
        parse_program("PRINT TRUE,  \",\", $41.43, \",\", 10h").run(&mut ctx);
        assert_eq!("TRUE\",\"$41.43\",\"16".to_string(), ctx.output);
    }
}