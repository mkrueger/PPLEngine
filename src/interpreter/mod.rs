use crate::ast::*;
use core::panic;
use std::string::String;
use std::collections::HashMap;

pub mod variable_value;
pub use self::variable_value::*;

use std::mem::transmute;
use crate::tables::{OpCode, PPL_TRUE, PPL_FALSE, CONSTANT_VALUES};
use crate::ast::VariableType;

pub trait ProgramContext
{
    fn get_var_type(&self, var_name: &String) -> VariableType;
}

pub trait ExecutionContext
{
    fn print(&mut self, str: String);
}

fn convert_to(var_type: VariableType, value : &VariableValue) -> VariableValue
{
    match var_type {
        VariableType::Integer => {
            match value {
                VariableValue::Byte(v) => VariableValue::Integer(*v as i32),
                VariableValue::SByte(v) => VariableValue::Integer(*v as i32),
                VariableValue::Integer(v) => VariableValue::Integer(*v),
                VariableValue::Unsigned(v) => VariableValue::Integer(*v as i32),
                VariableValue::Word(v) => VariableValue::Integer(*v as i32),
                VariableValue::SWord(v) => VariableValue::Integer(*v as i32),
                VariableValue::Real(v) => VariableValue::Integer(*v as i32),
                VariableValue::Boolean(v) => VariableValue::Integer(if *v { PPL_TRUE as i32 } else { PPL_FALSE as i32}),
                _ => panic!("can't convert {:?} to i32", value)
            }
        }
        VariableType::Unsigned => {
            match value {
                VariableValue::Byte(v) => VariableValue::Unsigned(*v as u32),
                VariableValue::SByte(v) => VariableValue::Unsigned(*v as u32),
                VariableValue::Integer(v) => VariableValue::Unsigned(*v as u32),
                VariableValue::Unsigned(v) => VariableValue::Unsigned(*v),
                VariableValue::Word(v) => VariableValue::Unsigned(*v as u32),
                VariableValue::SWord(v) => VariableValue::Unsigned(*v as u32),
                VariableValue::Real(v) => VariableValue::Unsigned(*v as u32),
                VariableValue::Boolean(v) => VariableValue::Unsigned(if *v { PPL_TRUE as u32 } else { PPL_FALSE as u32}),
                _ => panic!("can't convert {:?} to u32", value)
            }
        }

        VariableType::Word => {
            match value {
                VariableValue::Byte(v) => VariableValue::Word(*v as u16),
                VariableValue::SByte(v) => VariableValue::Word(*v as u16),
                VariableValue::Integer(v) => VariableValue::Word(*v as u16),
                VariableValue::Unsigned(v) => VariableValue::Word(*v as u16),
                VariableValue::Word(v) => VariableValue::Word(*v),
                VariableValue::SWord(v) => VariableValue::Word(*v as u16),
                VariableValue::Real(v) => VariableValue::Word(*v as u16),
                VariableValue::Boolean(v) => VariableValue::Word(if *v { PPL_TRUE as u16 } else { PPL_FALSE as u16}),
                _ => panic!("can't convert {:?} to u16", value)
            }
        }

        VariableType::SWord => {
            match value {
                VariableValue::Byte(v) => VariableValue::SWord(*v as i16),
                VariableValue::SByte(v) => VariableValue::SWord(*v as i16),
                VariableValue::Integer(v) => VariableValue::SWord(*v as i16),
                VariableValue::Unsigned(v) => VariableValue::SWord(*v as i16),
                VariableValue::Word(v) => VariableValue::SWord(*v as i16),
                VariableValue::SWord(v) => VariableValue::SWord(*v),
                VariableValue::Real(v) => VariableValue::SWord(*v as i16),
                VariableValue::Boolean(v) => VariableValue::SWord(if *v { PPL_TRUE as i16 } else { PPL_FALSE as i16}),
                _ => panic!("can't convert {:?} to i16", value)
            }
        }

        VariableType::Byte => {
            match value {
                VariableValue::Byte(v) => VariableValue::Byte(*v),
                VariableValue::SByte(v) => VariableValue::Byte(*v as u8),
                VariableValue::Integer(v) => VariableValue::Byte(*v as u8),
                VariableValue::Unsigned(v) => VariableValue::Byte(*v as u8),
                VariableValue::Word(v) => VariableValue::Byte(*v as u8),
                VariableValue::SWord(v) => VariableValue::Byte(*v as u8),
                VariableValue::Real(v) => VariableValue::Byte(*v as u8),
                VariableValue::Boolean(v) => VariableValue::Byte(if *v { PPL_TRUE as u8 } else { PPL_FALSE as u8}),
                _ => panic!("can't convert {:?} to u8", value)
            }
        }

        VariableType::SByte => {
            match value {
                VariableValue::Byte(v) => VariableValue::SByte(*v as i8),
                VariableValue::SByte(v) => VariableValue::SByte(*v),
                VariableValue::Integer(v) => VariableValue::SByte(*v as i8),
                VariableValue::Unsigned(v) => VariableValue::SByte(*v as i8),
                VariableValue::Word(v) => VariableValue::SByte(*v as i8),
                VariableValue::SWord(v) => VariableValue::SByte(*v as i8),
                VariableValue::Real(v) => VariableValue::SByte(*v as i8),
                VariableValue::Boolean(v) => VariableValue::SByte(if *v { PPL_TRUE as i8 } else { PPL_FALSE as i8}),
                _ => panic!("can't convert {:?} to i8", value)
            }
        }

        VariableType::Real => {
            match value {
                VariableValue::Real(v) => VariableValue::Real(*v),
                VariableValue::Byte(v) => VariableValue::Real(*v as f64),
                VariableValue::SByte(v) => VariableValue::Real(*v as f64),
                VariableValue::Integer(v) => VariableValue::Real(*v as f64),
                VariableValue::Unsigned(v) => VariableValue::Real(*v as f64),
                VariableValue::Word(v) => VariableValue::Real(*v as f64),
                VariableValue::SWord(v) => VariableValue::Real(*v as f64),
                VariableValue::Boolean(v) => VariableValue::Real(if *v { PPL_TRUE as f64 } else { PPL_FALSE as f64}),
                _ => panic!("can't convert {:?} to f64", value)
            }
        }

        VariableType::String => {
            match value {
                VariableValue::String(v) => VariableValue::String(v.clone()),
                _ => VariableValue::String(value.to_string())
            }
        }
        VariableType::Money => {
            match value {
                VariableValue::Money(v) => VariableValue::Money(*v),
                _ => VariableValue::Money(value.to_string().parse().unwrap())
            }
        }
        VariableType::Boolean => {
            match value {
                VariableValue::Boolean(v) => VariableValue::Boolean(*v),
                _ => {
                    match value.to_string().as_str() {
                        "1" => VariableValue::Boolean(true),
                        "0" => VariableValue::Boolean(false),
                        _ => panic!("no bool value {:?}", value)
                    }
                }
            }
        }

        VariableType::Date => {
            match value {
                VariableValue::Byte(v) => VariableValue::Date(*v as u16),
                VariableValue::SByte(v) => VariableValue::Date(*v as u16),
                VariableValue::Integer(v) => VariableValue::Date(*v as u16),
                VariableValue::Unsigned(v) => VariableValue::Date(*v as u16),
                VariableValue::Word(v) => VariableValue::Date(*v as u16),
                VariableValue::SWord(v) => VariableValue::Date(*v as u16),
                VariableValue::Real(v) => VariableValue::Date(*v as u16),
                VariableValue::Date(v) => VariableValue::Date(*v),
                VariableValue::EDate(v) => VariableValue::Date(*v),
                VariableValue::Time(v) => VariableValue::Date(*v as u16),
                VariableValue::Boolean(v) => VariableValue::Date(if *v { PPL_TRUE as u16 } else { PPL_FALSE as u16}),
                _ => panic!("can't convert {:?} to u16", value)
            }
        }

        VariableType::EDate => {
            match value {
                VariableValue::Byte(v) => VariableValue::EDate(*v as u16),
                VariableValue::SByte(v) => VariableValue::EDate(*v as u16),
                VariableValue::Integer(v) => VariableValue::EDate(*v as u16),
                VariableValue::Unsigned(v) => VariableValue::EDate(*v as u16),
                VariableValue::Word(v) => VariableValue::EDate(*v as u16),
                VariableValue::SWord(v) => VariableValue::EDate(*v as u16),
                VariableValue::Real(v) => VariableValue::EDate(*v as u16),
                VariableValue::Date(v) => VariableValue::EDate(*v as u16),
                VariableValue::EDate(v) => VariableValue::EDate(*v),
                VariableValue::Time(v) => VariableValue::EDate(*v as u16),
                VariableValue::Boolean(v) => VariableValue::EDate(if *v { PPL_TRUE as u16 } else { PPL_FALSE as u16}),
                _ => panic!("can't convert {:?} to u16", value)
            }
        }

        VariableType::Time => {
            match value {
                VariableValue::Byte(v) => VariableValue::Time(*v as u32),
                VariableValue::SByte(v) => VariableValue::Time(*v as u32),
                VariableValue::Integer(v) => VariableValue::Time(*v as u32),
                VariableValue::Unsigned(v) => VariableValue::Time(*v as u32),
                VariableValue::Word(v) => VariableValue::Time(*v as u32),
                VariableValue::SWord(v) => VariableValue::Time(*v as u32),
                VariableValue::Real(v) => VariableValue::Time(*v as u32),
                VariableValue::Date(v) => VariableValue::Time(*v as u32),
                VariableValue::EDate(v) => VariableValue::Time(*v as u32),
                VariableValue::Time(v) => VariableValue::Time(*v),
                VariableValue::Boolean(v) => VariableValue::Time(if *v { PPL_TRUE as u32 } else { PPL_FALSE as u32}),
                _ => panic!("can't convert {:?} to u16", value)
            }
        }

        _ => {panic!("unsupported {:?}", var_type);}
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
                Constant::Boolean(true) => VariableValue::Integer(PPL_TRUE),
                Constant::Boolean(false) => VariableValue::Integer(PPL_FALSE),
                Constant::Money(x) => VariableValue::Money(*x),
                Constant::Integer(x) => VariableValue::Integer(*x),
                Constant::String(x) => VariableValue::String(x.clone()),
                Constant::Real(x) => VariableValue::Real(*x),
                Constant::Unsigned(x) => VariableValue::Unsigned(*x),
                Constant::Builtin(x) => {
                    for val in &CONSTANT_VALUES {
                        if *x == val.0 {
                            return val.1.clone();
                        }
                    }
                    panic!("unknown built in const {}", x)
                }
            }
        },
        Expression::Parens(pexpr) => { evaluate_exp(prg, cur_frame, ctx, pexpr) },
        Expression::FunctionCall(func_name, _params) => { panic!("not supported function call {}", func_name); },
        Expression::PredefinedFunctionCall(func_def, _params) => { panic!("not supported predef function call {}", func_def.name); },
        Expression::Not(expr) => {
            let value = evaluate_exp(prg, cur_frame, ctx, expr);
            match value {
                VariableValue::Integer(x) => VariableValue::Integer(if x == PPL_FALSE { PPL_TRUE } else { PPL_FALSE }),
                VariableValue::Boolean(x) => VariableValue::Boolean(!x),
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
        Expression::Plus(expr) => { 
            evaluate_exp(prg, cur_frame, ctx, expr)
        },
        Expression::BinaryExpression(op, lvalue, rvalue) => {
            match op {
                BinOp::Add => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue) + evaluate_exp(prg, cur_frame, ctx, rvalue)
                }
                BinOp::Sub => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue) - evaluate_exp(prg, cur_frame, ctx, rvalue)
                }
                BinOp::Mul => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue) * evaluate_exp(prg, cur_frame, ctx, rvalue)
                }
                BinOp::Div => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue) / evaluate_exp(prg, cur_frame, ctx, rvalue)
                }
                BinOp::Mod => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).modulo(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::PoW => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).pow(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::Eq => {
                    let l = evaluate_exp(prg, cur_frame, ctx, lvalue);
                    let r = evaluate_exp(prg, cur_frame, ctx, rvalue);
                    VariableValue::Boolean(l == r)
                }
                BinOp::NotEq => {
                    let l = evaluate_exp(prg, cur_frame, ctx, lvalue);
                    let r = evaluate_exp(prg, cur_frame, ctx, rvalue);
                    VariableValue::Boolean(l != r)
                }
                BinOp::Or => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).or(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::And => {
                    evaluate_exp(prg, cur_frame, ctx, lvalue).and(evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::Lower => {
                    VariableValue::Boolean(evaluate_exp(prg, cur_frame, ctx, lvalue) < evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::LowerEq => {
                    VariableValue::Boolean(evaluate_exp(prg, cur_frame, ctx, lvalue) <= evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::Greater => {
                    VariableValue::Boolean(evaluate_exp(prg, cur_frame, ctx, lvalue) > evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
                BinOp::GreaterEq => {
                    VariableValue::Boolean(evaluate_exp(prg, cur_frame, ctx, lvalue) >= evaluate_exp(prg, cur_frame, ctx, rvalue))
                }
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
        Statement::Let(variable, expr) => {
            let value = evaluate_exp(prg, cur_frame, ctx, &expr);
            let var_name = get_var_name(variable);
            let var_type = prg.get_var_type(&var_name);

            cur_frame.values.insert(var_name, convert_to(var_type, &value));
        }
        Statement::Goto(label) => {
            let table = &interpreter.label_tables[cur_frame.label_table as usize];
            cur_frame.cur_ptr = *table.get(label).unwrap();
        }
        Statement::Call(def, params) => {
            let op: OpCode = unsafe { transmute(def.opcode) };
            match op {
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

        Statement::Inc(expr) => {
            let new_value = evaluate_exp(prg, cur_frame, ctx, expr) + VariableValue::Integer(1);
            cur_frame.values.insert(expr.to_string(), new_value);
        }

        Statement::Dec(expr) => {
            let new_value = evaluate_exp(prg, cur_frame, ctx, expr) + VariableValue::Integer(-1);
            cur_frame.values.insert(expr.to_string(), new_value);
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
    fn test_bool() {        
        check_output(r#"
BOOLEAN B
B = TRUE
PRINT B, ","
B = !B
PRINT B, ","
B = !B
PRINT B
"#, "1,0,1"); // This is no error. Bools were printed as numbers
    }
/* 
    #[test]
    fn test_real() {        
        check_output(r#"
REAL v
v = 0.653
v = v + 10
PRINT v    
"#, "10.653"); // This is no error. Bools were printed as numbers
    }
*/
    #[test]
    fn test_byte_overflow() {        
        check_output(r#"
BYTE B
B = 255
PRINT B, ","
INC B
PRINT B, ","
DEC B
PRINT B
"#, "255,0,255");
    }

    #[test]
    fn test_sbyte_overflow() {        
        check_output(r#"
SBYTE B
B = 127
PRINT B, ","
INC B
PRINT B, ","
DEC B
PRINT B
"#, "127,-128,127");
    }
    
    #[test]
    fn test_int_overflow() {        
        check_output(r#"
INTEGER B
B = 7FFFFFFFh
PRINT B, ","
INC B
PRINT B, ","
DEC B
PRINT B
"#, "2147483647,-2147483648,2147483647");
    }

    #[test]
    fn test_unsigned_overflow() {        
        check_output(r#"
UNSIGNED B
B = 4294967295
PRINT B, ","
INC B
PRINT B, ","
DEC B
PRINT B        
"#, "4294967295,0,4294967295");
    }


    #[test]
    fn test_word_overflow() {        
        check_output(r#"
WORD B
B = 65535
PRINT B, ","
INC B
PRINT B, ","
B = 0
DEC B
PRINT B   
"#, "65535,0,65535");
    }


    #[test]
    fn test_sword_overflow() {        
        check_output(r#"
SWORD B
B = 32767
PRINT B, ","
INC B
PRINT B, ","
DEC B
PRINT B
"#, "32767,-32768,32767");
    }
    
    #[test]
    fn test_constants() {        
        check_output(r#"
        PRINT AUTO,","
        PRINT BELL,","
        PRINT DEFS,","
        PRINT ECHODOTS,","
        PRINT ERASELINE,","
        PRINT FCL,","
        PRINT FIELDLEN,","
        PRINT FNS,","
        PRINT F_EXP,","
        PRINT F_MW,","
        PRINT F_REG,","
        PRINT F_SEL,","
        PRINT F_SYS,","
        PRINT GRAPH,","
        PRINT GUIDE,","
        PRINT HIGHASCII,","
        PRINT LANG,","
        PRINT LFAFTER,","
        PRINT LFBEFORE,","
        PRINT LOGIT,","
        PRINT LOGITLEFT,","
        PRINT NC,","
        PRINT NEWLINE,","
        PRINT NOCLEAR,","
        PRINT O_RD,","
        PRINT O_RW,","
        PRINT O_WR,","
        PRINT SEC,","
        PRINT SEEK_CUR,","
        PRINT SEEK_END,","
        PRINT SEEK_SET,","
        PRINT STACKED,","
        PRINT S_DB,","
        PRINT S_DN,","
        PRINT S_DR,","
        PRINT S_DW,","
        PRINT UPCASE,","
        PRINT WORDWRAP,","
        PRINT YESNO,","
        PRINT START_BAL,","
        PRINT START_SESSION,","
        PRINT DEB_CALL,","
        PRINT DEB_TIME,","
        PRINT DEB_MSGREAD,","
        PRINT DEB_MSGCAP,","
        PRINT DEB_MSGWRITE,","
        PRINT DEB_MSGECHOED,","
        PRINT DEB_MSGPRIVATE,","
        PRINT DEB_DOWNFILE,","
        PRINT DEB_DOWNBYTES,","
        PRINT DEB_CHAT,","
        PRINT DEB_TPU,","
        PRINT DEB_SPECIAL,","
        PRINT CRED_UPFILE,","
        PRINT CRED_UPBYTES,","
        PRINT CRED_SPECIAL,","
        PRINT SEC_DROP,","
        "#, "8192,2048,0,1,32,2,2,1,2,16,1,4,8,1,4,4096,4,256,128,32768,65536,0,64,1024,0,2,1,2,1,2,0,16,3,0,1,2,8,512,16384,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,");
    }
    

    fn check_output(prg: &str, out: &str) {        
        let mut ctx = TestContext { output: String::new() };
        run(&parse_program(prg), &mut ctx);
        assert_eq!(out, ctx.output);
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