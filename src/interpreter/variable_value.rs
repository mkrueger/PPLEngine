use std::{ops::{Add, Sub, Mul, Div}, cmp::Ordering, fmt};

use crate::ast::variable_type::VariableType;

use super::convert_to;

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