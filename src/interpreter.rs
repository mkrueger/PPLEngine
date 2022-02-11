use crate::parser;
use crate::parser::*;
use std::string::String;

pub struct Block
{
    pub statements: Vec<Statement>,
}
impl Block
{
    pub fn new() -> Self
    {
        Block {
            statements : vec![]
        }
    }

    pub fn mark_label(&mut self, label : String)
    {
        self.statements.push(Statement::Label(label));
    }

    pub fn to_string(&self, prg : &Program) -> std::string::String
    {
        let mut result = std::string::String::new();
        for s in &self.statements {
            result.push_str(s.to_string(prg).as_str());
            result.push_str("\n");
        }
        result
    }
}
pub struct FunctionDeclaration
{
    pub id : i32,
    pub declaration: parser::Declaration,
    pub block: Block
}

use std::fmt;
use std::mem::transmute;
use crate::executable::VariableType;
use crate::tables::OpCode;

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.declaration.to_string())
    }
}

impl FunctionDeclaration {
    pub fn print_content(&self, prg : &Program) -> String
    {
        let mut res = self.declaration.print_header();
        res.push('\n');

        for stmt in &self.block.statements {
            match stmt {
                Statement::Call(def, params) => {
                    if def.opcode == OpCode::FEND as u8 {
                        match &self.declaration {
                            Declaration::Procedure(name, param) => {
                                res.push_str("ENDPROC");
                                continue;
                            },
                            Declaration::Function(name, param, t) => {
                                res.push_str("ENDFUNC");
                                continue;
                            },
                            _ => {}
                        }
                    }
                    if def.opcode == OpCode::FPCLR as u8 {
                        res.push_str("ENDPROC");
                        continue;
                    }
                },
                _ => {}
            }
            res.push_str(stmt.to_string(prg).as_str());
            res.push('\n');
        }

        return res;
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
                statements: vec![]
            },
            function_declarations: vec![],
            procedure_declarations: vec![]
        }
    }

    fn execute_statement(&self, ctx : &mut dyn ProgramContext, stmt : &Statement)
    {
        match stmt {
            Statement::Call(def, params) => {
                let op : OpCode = unsafe { transmute(def.opcode) };
                match op  {
                    OpCode::PRINT=> {
                        for expr in params {
                            ctx.print(expr.to_string());
                        }
                    },
                    OpCode::PRINTLN => {
                        for expr in params {
                            ctx.print(expr.to_string());
                        }
                        ctx.print("\n".to_string());
                    }
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

    pub fn get_variable(&self, var_name : &String) -> VariableType
    {
        for decl in &self.variable_declarations {
            match decl {
                Declaration::Variable(var_type, name) => if *name == *var_name { return *var_type; },
                Declaration::Variable1(var_type, name, dim1) => if *name == *var_name { return *var_type; },
                Declaration::Variable2(var_type, name, dim1, dim2) => if *name == *var_name { return *var_type; },
                Declaration::Variable3(var_type, name, dim1, dim2, dim3) => if *name == *var_name { return *var_type; },
                _ => {}
            }
        }

        VariableType::Unknown
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
            res.push_str(v.print_content(self).as_str());
            res.push('\n');
        }

        for v in &self.procedure_declarations {
            res.push_str(&v.print_content(self).as_str());
            res.push('\n');
        }

        res
    }
}

fn parse_program(input : &str) -> Program
{
    let stmt = parse_statement(input).unwrap();

    Program {
        variable_declarations: vec![],
        main_block: Block {
            statements: vec![stmt.1]
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