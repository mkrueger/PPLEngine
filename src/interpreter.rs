use crate::ast::*;
use core::panic;
use std::string::String;
use std::collections::HashMap;

use std::mem::transmute;
use crate::tables::{OpCode, PPL_TRUE, PPL_FALSE};
use crate::ast::VariableType;

pub trait ProgramContext
{
    fn get_var_type(&self, var_name: &String) -> VariableType;
}

pub trait ExecutionContext
{
    fn print(&mut self, str: String);
}

#[derive(Debug, Clone)]
enum VariableValue
{
//    Unsigned(u32),
//   Date(u32),
//    EDate(u32),
    Integer(i32),
    Money(f64),
//    Real(f32),
    String(String),
//    Time(u32),
//    Byte(u8),
//    Word(u16),
//    SByte(i8),
//    SWord(i16),
//    BigStr(String),
    Double(f64),
//    None
}

impl VariableValue {
    pub fn add(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = other {
                    VariableValue::Integer(x + y)
                } else {
                    panic!("can't add int to rvalue {:?}", other)
                }
            }
            VariableValue::String(x) => {
                match other {
                    VariableValue::Integer(y) => {
                        let mut x = x.clone();
                        x.push_str(&y.to_string());
                        VariableValue::String(x)
                    }
                    VariableValue::String(y) => {
                        let mut x = x.clone();
                        x.push_str(&y);
                        VariableValue::String(x)
                    }
                    _ => panic!("can't add string to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }

    pub fn sub(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = other {
                    VariableValue::Integer(x - y)
                } else {
                    panic!("can't add int to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }

    pub fn or(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = other {
                    VariableValue::Integer(x | y)
                } else {
                    panic!("can't add int to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }

    pub fn and(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = other {
                    VariableValue::Integer(x & y)
                } else {
                    panic!("can't add int to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }
    
    pub fn eq(&self, other : VariableValue) -> bool
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = other {
                    *x == y
                } else {
                    panic!("can't add int to rvalue {:?}", other)
                }
            }
            VariableValue::String(x) => {
                match other {
                    VariableValue::String(y) => {
                        *x == y
                    }
                    _ => panic!("can't add string to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }

    pub fn lower(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = other {
                    if *x < y {
                        VariableValue::Integer(PPL_TRUE)
                    } else {
                        VariableValue::Integer(PPL_FALSE)
                    }
                } else {
                    panic!("can't add int to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }

    pub fn lower_eq(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = other {
                    if *x <= y {
                        VariableValue::Integer(PPL_TRUE)
                    } else {
                        VariableValue::Integer(PPL_FALSE)
                    }
                } else {
                    panic!("can't add int to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }

    pub fn greater(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = other {
                    if *x > y {
                        VariableValue::Integer(PPL_TRUE)
                    } else {
                        VariableValue::Integer(PPL_FALSE)
                    }
                } else {
                    panic!("can't add int to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    
    }
    pub fn greater_eq(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = other {
                    if *x >= y {
                        VariableValue::Integer(PPL_TRUE)
                    } else {
                        VariableValue::Integer(PPL_FALSE)
                    }
                } else {
                    panic!("can't add int to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }


    
    pub fn to_string(&self) -> String
    {
        match self {
            VariableValue::Money(f) => format!("${}", f),
            VariableValue::Integer(i) => format!("{}", i),
            VariableValue::String(str) => format!("{}", str),
            VariableValue::Double(f) => format!("{}", f),
        }
    }
}
struct StackFrame {
    values: HashMap<String, VariableValue>,
    cur_ptr: usize,
    label_table: i32
}

struct Interpreter/*<'a> */{
    // lookup: HashMap<&'a Block, i32>,
    label_tables: Vec<HashMap<String, usize>>,
   //  stack_frames: Vec<StackFrame>
}

fn evaluate_exp(prg : &Program, cur_frame: &StackFrame, ctx: &mut dyn ExecutionContext, expr: &Expression) -> VariableValue
{
    match expr {
        Expression::Identifier(str) => {
            let res = cur_frame.values.get(str).unwrap().clone();
            res
        },
        Expression::Const(constant) => { 
            match constant {
                Constant::TRUE => VariableValue::Integer(PPL_TRUE),
                Constant::FALSE => VariableValue::Integer(PPL_FALSE),
                Constant::Money(x) => VariableValue::Money(*x),
                Constant::Integer(x) => VariableValue::Integer(*x),
                Constant::String(x) => VariableValue::String(x.clone()),
                Constant::Double(x) => VariableValue::Double(*x),
                _ => {panic!("unsupported const {:?}", constant)}
            }
        },
        Expression::Parens(pexpr) => { evaluate_exp(prg, cur_frame, ctx, pexpr) },
        Expression::FunctionCall(func_name, _params) => { panic!("not supported function call {}", func_name); },
        Expression::Not(expr) => {
            let value = evaluate_exp(prg, cur_frame, ctx, expr);
            match value {
                VariableValue::Integer(x) => VariableValue::Integer(if x == PPL_FALSE { PPL_TRUE } else { PPL_FALSE }),
                _ => {panic!("unsupported for minus {:?} value {:?}", expr, value);}
            }
        },
        Expression::Minus(expr) => { 
            let value = evaluate_exp(prg, cur_frame, ctx, expr);
            match value {
                VariableValue::Integer(x) => VariableValue::Integer(-x),
                _ => {panic!("unsupported for minus {:?} value {:?}", expr, value);}
            }
        },
        Expression::BinaryExpression(op, lvalue, rvalue) => {
            match op {
                BinOp::Add => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).add(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::Sub => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).sub(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::Eq => {
                    if evaluate_exp(prg, cur_frame, ctx, lvalue).eq(evaluate_exp(prg, cur_frame, ctx, rvalue)) {
                        VariableValue::Integer(PPL_TRUE)
                    } else {
                        VariableValue::Integer(PPL_FALSE)
                    }
                }
                BinOp::NotEq => {
                    let eq = evaluate_exp(prg, cur_frame, ctx, lvalue).eq(evaluate_exp(prg, cur_frame, ctx, rvalue));
                    if eq {
                        VariableValue::Integer(PPL_FALSE)
                    } else {
                        VariableValue::Integer(PPL_TRUE)
                    }
                }
                BinOp::Or => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).or(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::And => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).and(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::Lower => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).lower(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::LowerEq => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).lower_eq(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::Greater => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).greater(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::GreaterEq => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).greater_eq(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                _ => { panic!("not supported bin op {:?}", op); }
            }
        },
        Expression::Dim1(_expr, _vec) => { panic!("not supported"); },
        Expression::Dim2(_expr, _vec, _mat) => { panic!("not supported"); },
        Expression::Dim3(_expr, _vec, _mat, _cube) => { panic!("not supported"); },
    }
}

fn execute_statement(prg : &Program, interpreter: &Interpreter, cur_frame: &mut StackFrame, ctx: &mut dyn ExecutionContext, stmt: &Statement)
{
    match stmt {
        Statement::Call(def, params) => {
            let op: OpCode = unsafe { transmute(def.opcode) };
            match op {
                OpCode::LET => {
                    let value = evaluate_exp(prg, cur_frame, ctx, &params[1]);
                    cur_frame.values.insert(params[0].to_string(), value);
                },
                OpCode::PRINT => {
                    for expr in params {
                        let value = evaluate_exp(prg, cur_frame, ctx, expr);
                        ctx.print(value.to_string());
                    }
                }
                OpCode::PRINTLN => {
                    for expr in params {
                        let value = evaluate_exp(prg, cur_frame, ctx, expr);
                        ctx.print(value.to_string());
                    }
                    ctx.print("\n".to_string());
                }
                OpCode::GOTO => {
                    let label = params[0].to_string();
                    let table = &interpreter.label_tables[cur_frame.label_table as usize];
                    cur_frame.cur_ptr = *table.get(&label).unwrap();
                }
                _ => { panic!("unsupported op code {:?}", op); }
            }
        }
        Statement::ProcedureCall(_, _) => { panic!("procedures not yet supported."); },          

        Statement::If(cond, statement) => {
            let value = evaluate_exp(prg, cur_frame, ctx, cond);
            if let VariableValue::Integer(x) = value {
                if x == PPL_TRUE {
                    execute_statement(prg, interpreter, cur_frame, ctx, statement);
                }
            } else {
                panic!("no bool value {:?}", value);
            }

        }

        /* unsupported for now - the compiler does not generate them */
        Statement::Continue => { panic!("unsupported statement Continue")},
        Statement::Break => { panic!("unsupported statement Break")},
        Statement::EndWhile => { panic!("unsupported statement EndWhile")},
        Statement::EndIf => { panic!("unsupported statement EndIf")},
        Statement::Next => { panic!("unsupported statement Next")},
        Statement::For(_, _, _, _) => { panic!("unsupported statement For")},
        Statement::DoWhile(_) => { panic!("unsupported statement DoWhile")},
        Statement::IfThen(_) => { panic!("unsupported statement IfThen")},
        Statement::Else => { panic!("unsupported statement Else")},

        // nop statements
        Statement::Label(_) => { /* skip */},
        Statement::Comment(_) => { /* skip */ },

        _ => { panic!("unsupported statement {:?}", stmt); }
    }
}

fn calc_table<'a>(blk : &Block) -> HashMap<String, usize>
{
    let mut res = HashMap::new();

    for i in 0..blk.statements.len() {
        if let Statement::Label(label) = &blk.statements[i] {
            res.insert(label.clone(), i);
        }
    }

    res
}

pub fn run(prg : &Program, ctx: &mut dyn ExecutionContext)
{
    let interpreter = &mut Interpreter {
       // lookup: HashMap::new(),
        label_tables: Vec::new(),
      //  stack_frames: vec![]
    };

    interpreter.label_tables.push(calc_table(&prg.main_block));
    //nterpreter.lookup.insert(&prg.main_block, 0);

    let mut cur_frame = StackFrame { 
        values: HashMap::new(),
        cur_ptr:0,
        label_table:0
    };

    while cur_frame.cur_ptr < prg.main_block.statements.len() {
        let stmt = &prg.main_block.statements[cur_frame.cur_ptr as usize];
//        println!("{}", stmt.to_string(prg, 0).0);
        execute_statement(prg, &interpreter, &mut cur_frame, ctx, stmt);
        cur_frame.cur_ptr += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_program;
    use super::*;

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

    #[test]
    fn test_println() {
        let mut ctx = TestContext { output: String::new() };
        
        run(&parse_program("PRINTLN 1, 2, 3, \"Hello World\""), &mut ctx);
        assert_eq!("123Hello World\n".to_string(), ctx.output);

        ctx = TestContext { output: String::new() };
        run(&parse_program("PRINT TRUE,  \",\", $41.43, \",\", 10h"), &mut ctx);
        assert_eq!("1,$41.43,16".to_string(), ctx.output);
    }
}