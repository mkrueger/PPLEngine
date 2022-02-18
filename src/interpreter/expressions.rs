use crate::{ast::{program::Program, expression::Expression}, tables::{FunctionDefinition, FuncOpCode}};
use super::*;

pub fn evaluate_exp(prg : &Program, cur_frame: &StackFrame, ctx: &dyn ExecutionContext, expr: &Expression) -> VariableValue
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
                Constant::Integer(x) => VariableValue::Integer (*x),
                Constant::Unsigned(x) => VariableValue::Unsigned(*x),
                Constant::String(x) => VariableValue::String(x.clone()),
                Constant::Real(x) => VariableValue::Real(*x),
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
        Expression::PredefinedFunctionCall(func_def, params) => { call_function(prg, cur_frame, ctx, func_def, params) },
        Expression::FunctionCall(func_name, _params) => { panic!("not supported function call {}", func_name); },
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

fn get_int(val : &VariableValue) -> i32
{
    if let VariableValue::Integer(i) = convert_to(VariableType::Integer,val) {
       i
    } else {
        panic!("can't get int from {:?}", val);
    }
}

fn call_function(prg : &Program, cur_frame: &StackFrame, ctx: &dyn ExecutionContext, func_def : &'static FunctionDefinition, params : &Vec<Expression>) -> VariableValue
{
    match func_def.opcode {
        FuncOpCode::LEN => predefined_functions::len(evaluate_exp(prg, cur_frame, ctx, &params[0])),
        FuncOpCode::LOWER => predefined_functions::lower(evaluate_exp(prg, cur_frame, ctx, &params[0])),
        FuncOpCode::UPPER => predefined_functions::upper(evaluate_exp(prg, cur_frame, ctx, &params[0])),
        FuncOpCode::MID => predefined_functions::mid(evaluate_exp(prg, cur_frame, ctx, &params[0]), evaluate_exp(prg, cur_frame, ctx, &params[1]), evaluate_exp(prg, cur_frame, ctx, &params[2])),
        FuncOpCode::LEFT => predefined_functions::left(evaluate_exp(prg, cur_frame, ctx, &params[0]), evaluate_exp(prg, cur_frame, ctx, &params[1])),
        FuncOpCode::RIGHT => predefined_functions::right(evaluate_exp(prg, cur_frame, ctx, &params[0]), evaluate_exp(prg, cur_frame, ctx, &params[1])),
        FuncOpCode::SPACE => predefined_functions::space(evaluate_exp(prg, cur_frame, ctx, &params[0])),
        FuncOpCode::FERR => predefined_functions::ferr(evaluate_exp(prg, cur_frame, ctx, &params[0])),
        FuncOpCode::CHR => predefined_functions::chr(evaluate_exp(prg, cur_frame, ctx, &params[0])),
        FuncOpCode::ASC => predefined_functions::asc(evaluate_exp(prg, cur_frame, ctx, &params[0])),
        FuncOpCode::INSTR => predefined_functions::instr(evaluate_exp(prg, cur_frame, ctx, &params[0]), evaluate_exp(prg, cur_frame, ctx, &params[1])),
        FuncOpCode::ABORT => predefined_functions::abort(ctx),
        FuncOpCode::LTRIM => predefined_functions::ltrim(evaluate_exp(prg, cur_frame, ctx, &params[0]), evaluate_exp(prg, cur_frame, ctx, &params[1])),

        FuncOpCode::DRIVESPACE => panic!("TODO DRIVESPACE"), // TODO
        _ => panic!("Unsupported function {:?}", func_def)
    }
}

mod predefined_functions 
{
    use std::str::*;

    use substring::Substring;

    use crate::interpreter::{VariableValue, ExecutionContext};

    use super::get_int;

    /// Returns the length of a strning
    /// # Arguments
    ///  * `str` - A string value
    /// # Returns
    ///  VariableValue::Integer - the length of `str`
    /// # Remarks
    /// 0 means empty string
    /// According to specs 256 is the maximum returned
    pub fn len(str : VariableValue) -> VariableValue
    {
        VariableValue::Integer(str.to_string().len() as i32)
    }
    
    /// Returns the lowercase equivalent of a string
    /// # Arguments
    ///  * `str` - A string value
    /// # Returns
    ///  VariableValue::String - lowercase equivalent of `str`
    pub fn lower(str : VariableValue) -> VariableValue
    {
        VariableValue::String(str.to_string().to_lowercase())
    }

    /// Returns the uppercase equivalent of a string
    /// # Arguments
    ///  * `str` - A string value
    /// # Returns
    ///  VariableValue::String - uppercase equivalent of `str`
    pub fn upper(str : VariableValue) -> VariableValue
    {
        VariableValue::String(str.to_string().to_uppercase())
    }

    /// Returns a substring
    /// # Arguments
    ///  * `str` - A string value
    ///  * `pos` - An integer value with a position from str to begin the substring 1 == first char
    ///  * `chars` - An integer value with the number of chars to take from `str`
    /// # Returns
    ///  the substring of `str`, "" if chars <= 0, Will add padding up to the full length specified
    pub fn mid(str: VariableValue, pos: VariableValue, chars: VariableValue) -> VariableValue
    {
        let mut pos = get_int(&pos) - 1; // 1 based
        let mut chars = get_int(&chars);
        if chars <= 0 { return VariableValue::String(String::new()); }

        let mut res = String::new();
        while pos < 0 {
            res.push(' ');
            pos += 1;
            chars -= 1;
        }

        if chars > 0 {
            res.push_str(str.to_string().substring(pos as usize, (pos + chars) as usize));
        }
        VariableValue::String(res)
    }

    pub fn left(str: VariableValue, chars: VariableValue) -> VariableValue
    {
        let mut chars = get_int(&chars);
        if chars <= 0 { return VariableValue::String(String::new()); }

        let mut res = String::new();
        if chars > 0 {
            let str = str.to_string();
            if chars < str.len() as i32 {
                res.push_str(str.substring(0,  chars as usize));
            } else {
                res.push_str(str.as_str());
                chars -= str.len() as i32 ;
                while chars > 0 {
                    res.push(' ');
                    chars -= 1;
                }
            }
        }
        VariableValue::String(res)
    }

    pub fn right(str: VariableValue, chars: VariableValue) -> VariableValue
    {
        let chars = get_int(&chars);
        if chars <= 0 { return VariableValue::String(String::new()); }
        let mut chars = chars as usize ;

        let mut res = String::new();
        if chars > 0 {
            let str = str.to_string();
            while chars > str.len() {
                res.push(' ');
                chars -= 1;
            }
            res.push_str(str.substring(str.len() - chars,  str.len()));
        }
        VariableValue::String(res)
    }

    pub fn space(chars: VariableValue) -> VariableValue
    {
        let mut chars = get_int(&chars);
        if chars <= 0 { return VariableValue::String(String::new()); }
        let mut res = String::new();
        while chars > 0 {
            res.push(' ');
            chars -= 1;
        }
        VariableValue::String(res)
    }

    pub fn ferr(_str : VariableValue) -> VariableValue
    {
        panic!("TODO FILES") // TODO
    }

    pub fn chr(chr : VariableValue) -> VariableValue
    {            
        let c = get_int(&chr);
        if c <= 0 { return VariableValue::String(String::new()); }
        // undocumented: returns a space for c > 255
        if c > 255 { return VariableValue::String(" ".to_string()); }
        let mut res = String::new();
        unsafe {
            res.push(char::from_u32_unchecked(c as u32));
        }
        VariableValue::String(res)
    }

    pub fn asc(chr : VariableValue) -> VariableValue
    {            
        let c = chr.to_string();
        if c.len() <= 0 { return VariableValue::Integer(0); }
        VariableValue::Integer(c.as_bytes()[0] as i32)
    }

    /// Returns the position of a substring
    /// # Arguments
    ///  * `str` - A string value
    ///  * `sub` - A string expression to search for
    /// # Returns
    ///  A 1 based integer of the position of sub or 0 if sub is not found.
    pub fn instr(str : VariableValue, sub : VariableValue) -> VariableValue
    {            
        let str = str.to_string();
        let sub = sub.to_string();
        if sub.len() <= 0 { return VariableValue::Integer(0); }

        match str.find(&sub) {
            Some(x) => VariableValue::Integer(1 + x as i32),
            _ => VariableValue::Integer(0)
        }
    }

    /// Returns a flag indicating if the user has aborted the display of information.
    pub fn abort(ctx: &dyn ExecutionContext) -> VariableValue
    {
        panic!("TODO ABORT") // TODO
    }

    /// Trim specified characters from the beginning of a string
    /// # Arguments
    ///  * `str` - A string value
    ///  * `ch` - A string with the charakter to strip from the beginning of `str` 
    /// # Returns
    ///  The trimmed `str`
    pub fn ltrim(str: VariableValue, ch: VariableValue) -> VariableValue
    {
        let mut ch = ch.to_string();
        if ch.len() == 0 {
            return str;
        }
        
        let res = str.to_string();
        let pat = ch.remove(0);
        VariableValue::String(res.trim_matches(pat).to_string())
    }

}