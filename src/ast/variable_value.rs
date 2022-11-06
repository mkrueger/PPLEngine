use std::{ops::{Add, Sub, Mul, Div}, cmp::Ordering, fmt};

use crate::{ast::variable_type::VariableType, tables::{PPL_TRUE, PPL_FALSE}};

use super::VarInfo;

#[derive(Debug, Clone)]
pub enum VariableValue
{
    Boolean(bool),   
    Integer(i32),   
    Unsigned(u32),
    Word(u16),
    SWord(i16),

    Byte(u8),
    SByte(i8),
    
    Money(f64),
    Real(f64),

    String(String),
    Date(u16),
    EDate(u16),
    Time(u32),

    Dim1(VariableType, Vec<VariableValue>),
    Dim2(VariableType, Vec<Vec<VariableValue>>),
    Dim3(VariableType, Vec<Vec<Vec<VariableValue>>>)
}


impl PartialEq for VariableValue {
    fn eq(&self, other: &Self) -> bool
    {
        match self {
            VariableValue::Boolean(x) => {
                if let VariableValue::Boolean(y) = convert_to(VariableType::Boolean, other) {
                    *x == y
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }            
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = convert_to(VariableType::Integer, other) {
                    *x == y
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }            
            VariableValue::Unsigned(x) => {
                if let VariableValue::Unsigned(y) = convert_to(VariableType::Unsigned, other) {
                    *x == y
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Byte(x) => {
                if let VariableValue::Byte(y) = convert_to(VariableType::Byte, other) {
                    *x == y
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SByte(x) => {
                if let VariableValue::SByte(y) = convert_to(VariableType::SByte, other) {
                    *x == y
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Word(x) => {
                if let VariableValue::Word(y) = convert_to(VariableType::Word, other) {
                    *x == y
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SWord(x) => {
                if let VariableValue::SWord(y) = convert_to(VariableType::SWord, other) {
                    *x == y
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Real(x) => {
                if let VariableValue::Real(y) = convert_to(VariableType::Real, other) {
                    *x == y
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::String(x) => {
                if let VariableValue::String(y) = convert_to(VariableType::String, other) {
                    *x == y
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }
}

impl Add<VariableValue> for VariableValue {
    type Output = VariableValue;

    fn add(self, other : VariableValue) -> Self {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = convert_to(VariableType::Integer, &other) {
                    VariableValue::Integer(x.overflowing_add(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }            
            VariableValue::Unsigned(x) => {
                if let VariableValue::Unsigned(y) = convert_to(VariableType::Unsigned, &other) {
                    VariableValue::Unsigned(x.overflowing_add(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Byte(x) => {
                if let VariableValue::Byte(y) = convert_to(VariableType::Byte, &other) {
                    VariableValue::Byte(x.overflowing_add(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SByte(x) => {
                if let VariableValue::SByte(y) = convert_to(VariableType::SByte, &other) {
                    VariableValue::SByte(x.overflowing_add(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Word(x) => {
                if let VariableValue::Word(y) = convert_to(VariableType::Word, &other) {
                    VariableValue::Word(x.overflowing_add(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SWord(x) => {
                if let VariableValue::SWord(y) = convert_to(VariableType::SWord, &other) {
                    VariableValue::SWord(x.overflowing_add(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Real(x) => {
                if let VariableValue::Real(y) = convert_to(VariableType::Real, &other) {
                    VariableValue::Real(x + y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }

            VariableValue::String(x) => {
                match other {
                    VariableValue::Integer(y) => {
                        let mut x = x;
                        x.push_str(&y.to_string());
                        VariableValue::String(x)
                    }
                    VariableValue::String(y) => {
                        let mut x = x;
                        x.push_str(&y);
                        VariableValue::String(x)
                    }
                    _ => panic!("can't add string to rvalue {:?}", other)
                }
            }
            _ => panic!("unsupported lvalue for add {:?}", self)
        }
    }
}

impl Sub<VariableValue> for VariableValue {
    type Output = VariableValue;
    
    fn sub(self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = convert_to(VariableType::Integer, &other) {
                    VariableValue::Integer(x.overflowing_sub(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }            
            VariableValue::Unsigned(x) => {
                if let VariableValue::Unsigned(y) = convert_to(VariableType::Unsigned, &other) {
                    VariableValue::Unsigned(x.overflowing_sub(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Byte(x) => {
                if let VariableValue::Byte(y) = convert_to(VariableType::Byte, &other) {
                    VariableValue::Byte(x.overflowing_sub(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SByte(x) => {
                if let VariableValue::SByte(y) = convert_to(VariableType::SByte, &other) {
                    VariableValue::SByte(x.overflowing_sub(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Word(x) => {
                if let VariableValue::Word(y) = convert_to(VariableType::Word, &other) {
                    VariableValue::Word(x.overflowing_sub(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SWord(x) => {
                if let VariableValue::SWord(y) = convert_to(VariableType::SWord, &other) {
                    VariableValue::SWord(x.overflowing_sub(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Real(x) => {
                if let VariableValue::Real(y) = convert_to(VariableType::Real, &other) {
                    VariableValue::Real(x - y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }

            _ => panic!("unsupported lvalue for sub {:?}", self)
        }
    }
}

impl Mul<VariableValue> for VariableValue {
    type Output = VariableValue;

    fn mul(self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = convert_to(VariableType::Integer, &other) {
                    VariableValue::Integer(x.overflowing_mul(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }            
            VariableValue::Unsigned(x) => {
                if let VariableValue::Unsigned(y) = convert_to(VariableType::Unsigned, &other) {
                    VariableValue::Unsigned(x.overflowing_mul(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Byte(x) => {
                if let VariableValue::Byte(y) = convert_to(VariableType::Byte, &other) {
                    VariableValue::Byte(x.overflowing_mul(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SByte(x) => {
                if let VariableValue::SByte(y) = convert_to(VariableType::SByte, &other) {
                    VariableValue::SByte(x.overflowing_mul(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Word(x) => {
                if let VariableValue::Word(y) = convert_to(VariableType::Word, &other) {
                    VariableValue::Word(x.overflowing_mul(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SWord(x) => {
                if let VariableValue::SWord(y) = convert_to(VariableType::SWord, &other) {
                    VariableValue::SWord(x.overflowing_mul(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Real(x) => {
                if let VariableValue::Real(y) = convert_to(VariableType::Real, &other) {
                    VariableValue::Real(x * y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }

            _ => panic!("unsupported lvalue for sub {:?}", self)
        }
    }

}

impl Div<VariableValue> for VariableValue {
    type Output = VariableValue;
    
    fn div(self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = convert_to(VariableType::Integer, &other) {
                    VariableValue::Integer(x.overflowing_div(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }            
            VariableValue::Unsigned(x) => {
                if let VariableValue::Unsigned(y) = convert_to(VariableType::Unsigned, &other) {
                    VariableValue::Unsigned(x.overflowing_div(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Byte(x) => {
                if let VariableValue::Byte(y) = convert_to(VariableType::Byte, &other) {
                    VariableValue::Byte(x.overflowing_div(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SByte(x) => {
                if let VariableValue::SByte(y) = convert_to(VariableType::SByte, &other) {
                    VariableValue::SByte(x.overflowing_div(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Word(x) => {
                if let VariableValue::Word(y) = convert_to(VariableType::Word, &other) {
                    VariableValue::Word(x.overflowing_div(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SWord(x) => {
                if let VariableValue::SWord(y) = convert_to(VariableType::SWord, &other) {
                    VariableValue::SWord(x.overflowing_div(y).0)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Real(x) => {
                if let VariableValue::Real(y) = convert_to(VariableType::Real, &other) {
                    VariableValue::Real(x / y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }

            _ => panic!("unsupported lvalue for sub {:?}", self)
        }
    }
}

impl PartialOrd for VariableValue {
    
    fn partial_cmp(&self, other : &VariableValue) -> Option<Ordering>
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = convert_to(VariableType::Integer, other) {
                    x.partial_cmp(&y)
                } else {
                    panic!("unsupported lvalue {:?}", &self);
                } 
            }            
            VariableValue::Unsigned(x) => {
                if let VariableValue::Unsigned(y) = convert_to(VariableType::Unsigned, other) {
                    x.partial_cmp(&y)
                } else {
                    panic!("unsupported lvalue {:?}", &self);
                } 
            }
            VariableValue::Byte(x) => {
                if let VariableValue::Byte(y) = convert_to(VariableType::Byte, other) {
                    x.partial_cmp(&y)
                } else {
                    panic!("unsupported lvalue {:?}", &self);
                } 
            }
            VariableValue::SByte(x) => {
                if let VariableValue::SByte(y) = convert_to(VariableType::SByte, other) {
                    x.partial_cmp(&y)
                } else {
                    panic!("unsupported lvalue {:?}", &self);
                } 
            }
            VariableValue::Word(x) => {
                if let VariableValue::Word(y) = convert_to(VariableType::Word, other) {
                    x.partial_cmp(&y)
                } else {
                    panic!("unsupported lvalue {:?}", &self);
                } 
            }
            VariableValue::SWord(x) => {
                if let VariableValue::SWord(y) = convert_to(VariableType::SWord, other) {
                    x.partial_cmp(&y)
                } else {
                    panic!("unsupported lvalue {:?}", &self);
                } 
            }
            VariableValue::Real(x) => {
                if let VariableValue::Real(y) = convert_to(VariableType::Real, other) {
                    x.partial_cmp(&y)
                } else {
                    panic!("unsupported lvalue {:?}", &self);
                } 
            }
            VariableValue::String(x) => {
                if let VariableValue::String(y) = convert_to(VariableType::String, other) {
                    x.partial_cmp(&y)
                } else {
                    panic!("unsupported lvalue {:?}", &self);
                } 
            }
            _ => panic!("unsupported lvalue {:?}", self)
        } 
    }


}

impl fmt::Display for VariableValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariableValue::Money(i) => write!(f, "${}", i),
            VariableValue::Integer(i) => write!(f, "{}", i),
            VariableValue::Unsigned(i) |
            VariableValue::Time(i) => write!(f, "{}", i),
            VariableValue::Byte(i) => write!(f, "{}", i),
            VariableValue::SByte(i) => write!(f, "{}", i),
            VariableValue::Date(i) |
            VariableValue::Word(i) |
            VariableValue::EDate(i) => write!(f, "{}", i),
            VariableValue::SWord(i) => write!(f, "{}", i),
            VariableValue::String(str) => write!(f, "{}", str),
            VariableValue::Real(i) => write!(f, "{}", i),
            VariableValue::Boolean(b) => if *b { write!(f, "1") } else { write!(f, "0") }
            VariableValue::Dim1(var_type, data) => write!(f, "{}({})", var_type.to_string(), data.len()),
            VariableValue::Dim2(var_type, data) => write!(f, "{}({}, {})", var_type.to_string(), data.len(), data[0].len()),
            VariableValue::Dim3(var_type, data) => write!(f, "{}({}, {}, {})", var_type.to_string(), data.len(), data[0].len(), data[0][0].len()),
        }
    }
}

#[allow(clippy::needless_pass_by_value)]
impl VariableValue {
    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    #[must_use] pub fn modulo(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = convert_to(VariableType::Integer, &other) {
                    VariableValue::Integer(x % y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }            
            VariableValue::Unsigned(x) => {
                if let VariableValue::Unsigned(y) = convert_to(VariableType::Unsigned, &other) {
                    VariableValue::Unsigned(x % y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Byte(x) => {
                if let VariableValue::Byte(y) = convert_to(VariableType::Byte, &other) {
                    VariableValue::Byte(x % y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SByte(x) => {
                if let VariableValue::SByte(y) = convert_to(VariableType::SByte, &other) {
                    VariableValue::SByte(x % y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Word(x) => {
                if let VariableValue::Word(y) = convert_to(VariableType::Word, &other) {
                    VariableValue::Word(x % y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SWord(x) => {
                if let VariableValue::SWord(y) = convert_to(VariableType::SWord, &other) {
                    VariableValue::SWord(x % y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Real(x) => {
                if let VariableValue::Real(y) = convert_to(VariableType::Real, &other) {
                    VariableValue::Real(x % y)
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }

            _ => panic!("unsupported lvalue for sub {:?}", self)
        }
    }

    /// .
    ///
    ///
    /// # Panics
    ///
    /// Panics if .
    #[must_use] pub fn pow(&self, other : VariableValue) -> VariableValue
    {
        match self {
            VariableValue::Integer(x) => {
                if let VariableValue::Integer(y) = convert_to(VariableType::Integer, &other) {
                    VariableValue::Integer(x.pow( y as u32))
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }            
            VariableValue::Unsigned(x) => {
                if let VariableValue::Unsigned(y) = convert_to(VariableType::Unsigned, &other) {
                    VariableValue::Unsigned(x.pow( y))
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Byte(x) => {
                if let VariableValue::Byte(y) = convert_to(VariableType::Byte, &other) {
                    VariableValue::Byte(x.pow( y as u32))
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SByte(x) => {
                if let VariableValue::SByte(y) = convert_to(VariableType::SByte, &other) {
                    VariableValue::SByte(x.pow( y as u32))
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Word(x) => {
                if let VariableValue::Word(y) = convert_to(VariableType::Word, &other) {
                    VariableValue::Word(x.pow( y as u32))
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::SWord(x) => {
                if let VariableValue::SWord(y) = convert_to(VariableType::SWord, &other) {
                    VariableValue::SWord(x.pow( y as u32))
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }
            VariableValue::Real(x) => {
                if let VariableValue::Real(y) = convert_to(VariableType::Real, &other) {
                    VariableValue::Real(x.powf(y))
                } else {
                    panic!("can't add to lvalue {:?}", &self);
                } 
            }

            _ => panic!("unsupported lvalue for sub {:?}", self)
        }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    #[must_use] pub fn or(&self, other : VariableValue) -> VariableValue
    {
        if let VariableValue::Boolean(x) = convert_to(VariableType::Boolean, &self.clone()) { 
            if let VariableValue::Boolean(y) = convert_to(VariableType::Boolean, &other) {
                VariableValue::Boolean(x | y)
            } else {
                panic!("can't or to rvalue");
            } 
        } else {
            panic!("can't or to lvalue {:?}", &self);
        } 
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    #[must_use] pub fn and(&self, other : VariableValue) -> VariableValue
    {
        if let VariableValue::Boolean(x) = convert_to(VariableType::Boolean, &self.clone()) { 
            if let VariableValue::Boolean(y) = convert_to(VariableType::Boolean, &other) {
                VariableValue::Boolean(x & y)
            } else {
                panic!("can't and to rvalue");
            } 
        } else {
            panic!("can't and to lvalue {:?}", &self);
        } 
    }
}

pub fn convert_to(var_type: VariableType, value : &VariableValue) -> VariableValue
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
                VariableValue::Unsigned(v) |
                VariableValue::Time(v) => VariableValue::Date(*v as u16),
                VariableValue::Word(v) => VariableValue::Date(*v as u16),
                VariableValue::SWord(v) => VariableValue::Date(*v as u16),
                VariableValue::Real(v) => VariableValue::Date(*v as u16),
                VariableValue::Date(v) |
                VariableValue::EDate(v) => VariableValue::Date(*v),
                VariableValue::Boolean(v) => VariableValue::Date(if *v { PPL_TRUE as u16 } else { PPL_FALSE as u16}),
                _ => panic!("can't convert {:?} to u16", value)
            }
        }

        VariableType::EDate => {
            match value {
                VariableValue::Byte(v) => VariableValue::EDate(*v as u16),
                VariableValue::SByte(v) => VariableValue::EDate(*v as u16),
                VariableValue::Integer(v) => VariableValue::EDate(*v as u16),
                VariableValue::Unsigned(v) |
                VariableValue::Time(v) => VariableValue::EDate(*v as u16),
                VariableValue::Word(v) |
                VariableValue::Date(v) |
                VariableValue::EDate(v) => VariableValue::EDate(*v),
                VariableValue::SWord(v) => VariableValue::EDate(*v as u16),
                VariableValue::Real(v) => VariableValue::EDate(*v as u16),
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
                VariableValue::SWord(v) => VariableValue::Time(*v as u32),
                VariableValue::Real(v) => VariableValue::Time(*v as u32),
                VariableValue::Word(v) |
                VariableValue::Date(v) |
                VariableValue::EDate(v) => VariableValue::Time(*v as u32),
                VariableValue::Time(v) => VariableValue::Time(*v),
                VariableValue::Boolean(v) => VariableValue::Time(if *v { PPL_TRUE as u32 } else { PPL_FALSE as u32}),
                _ => panic!("can't convert {:?} to u16", value)
            }
        }

        _ => {panic!("unsupported {:?}", var_type);}
    }
}
