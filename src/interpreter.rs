use crate::ast::*;
use std::string::String;

pub struct FunctionDeclaration
{
    pub id: i32,
    pub declaration: Declaration,
    pub variable_declarations: Vec<Declaration>,
    pub block: Block,
}

fn match_var_name(decl : &Declaration, var_name : &String) -> Option<VariableType>
{
    match decl {
        Declaration::Variable(var_type, name) => if *name == *var_name { return Some(*var_type); },
        Declaration::Variable1(var_type, name, _dim1) => if *name == *var_name { return Some(*var_type); },
        Declaration::Variable2(var_type, name, _dim1, _dim2) => if *name == *var_name { return Some(*var_type); },
        Declaration::Variable3(var_type, name, _dim1, _dim2, _dim3) => if *name == *var_name { return Some(*var_type); },
        _ => {  }
    }
    None
}

impl ProgramContext for FunctionDeclaration
{
    fn get_var_type(&self, var_name: &String) -> VariableType
    {
        match &self.declaration {
            Declaration::Function(func_name, param, func_type) => {
                if var_name == func_name {
                    return *func_type;
                }
                for p in param {
                    if let Some(t) = match_var_name(p, var_name) {
                        return t;
                    }
                }
            }
            _ => {}
        }
        for decl in &self.variable_declarations {
            if let Some(t) = match_var_name(decl, var_name) {
                return t;
            }
        }
        VariableType::Unknown
    }
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
    pub fn print_content(&self) -> String
    {
        let mut res = self.declaration.print_header();
        res.push('\n');
        let mut indent = 1;

        if self.variable_declarations.len() > 0 {
            for v in &self.variable_declarations {
                res.push_str("    ");
                res.push_str(&v.to_string());
                res.push('\n');
            }
            res.push('\n');
        }

        for stmt in &self.block.statements {
            match stmt {
                Statement::Call(def, _params) => {
                    if def.opcode == OpCode::FEND || def.opcode == OpCode::FPCLR {
                        match &self.declaration {
                            Declaration::Procedure(_name, _param) => {
                                res.push_str(format!("ENDPROC ;--{}", _name).as_str());
                                continue;
                            }
                            Declaration::Function(_name, _param, _t) => {
                                res.push_str(format!("ENDFUNC ;--{}", _name).as_str());
                                continue;
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
            let out = stmt.to_string(self, indent);
            if indent > out.1 {
                indent = out.1;
            }
            for _ in 0..indent {
                res.push_str("    ");
            }
            res.push_str(out.0.as_str());
            indent = out.1;
            res.push('\n');
        }
        res.push('\n');

        return res;
    }
}

pub trait ProgramContext
{
    fn get_var_type(&self, var_name: &String) -> VariableType;
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

pub trait ExecutionContext
{
    fn print(&mut self, str: String);
}

struct TestContext
{
    output: String,
}

impl ExecutionContext for TestContext
{
    fn print(&mut self, str: String)
    {
        self.output.push_str(str.as_str());
    }
}

fn execute_statement(_prg : &Program, ctx: &mut dyn ExecutionContext, stmt: &Statement)
{
    match stmt {
        Statement::Call(def, params) => {
            let op: OpCode = unsafe { transmute(def.opcode) };
            match op {
                OpCode::PRINT => {
                    for expr in params {
                        ctx.print(expr.to_string());
                    }
                }
                OpCode::PRINTLN => {
                    for expr in params {
                        ctx.print(expr.to_string());
                    }
                    ctx.print("\n".to_string());
                }
                _ => {}
            }
        }
        _ => {}
    }
}

pub fn run(prg : &Program, ctx: &mut dyn ExecutionContext)
{
    for stmt in &prg.main_block.statements  {
        execute_statement(prg, ctx, stmt);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_program;

    use super::*;

    #[test]
    fn test_println() {
        let mut ctx = TestContext { output: String::new() };
        
        run(&parse_program("PRINTLN 1, 2, 3, \"Hello World\""), &mut ctx);
        assert_eq!("123\"Hello World\"\n".to_string(), ctx.output);

        ctx = TestContext { output: String::new() };
        run(&parse_program("PRINT TRUE,  \",\", $41.43, \",\", 10h"), &mut ctx);
        assert_eq!("TRUE\",\"$41.43\",\"16".to_string(), ctx.output);
    }
}