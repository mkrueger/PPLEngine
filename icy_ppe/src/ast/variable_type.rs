use std::fmt;

use super::{Variable, VariableData};

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Debug, Default)]
#[allow(dead_code)]
pub enum VariableType {
    Boolean = 0,
    Unsigned = 1,
    Date = 2,
    EDate = 3,
    #[default]
    Integer = 4,
    Money = 5,
    Real = 6,
    String = 7,
    Time = 8,
    Byte = 9,
    Word = 10,
    SByte = 11,
    SWord = 12,
    BigStr = 13,
    DoubleReal = 14,
    Function = 15,
    Procedure = 16,
    DDate = 17,
    Unknown = 255,
}

impl VariableType {
    pub fn create_empty_value(&self) -> Variable {
        match self {
            VariableType::String | VariableType::BigStr => Variable::new_string(String::new()),
            _ => Variable::new(*self, VariableData { unsigned_value: 0 }),
        }
    }
}

impl fmt::Display for VariableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariableType::Boolean => write!(f, "Boolean"), // BOOL 0 = false, 1 = true
            VariableType::Unsigned => write!(f, "Unsigned"), // u32
            VariableType::Date => write!(f, "Date"),       // 2*u8 - julian date
            VariableType::EDate => write!(f, "EDate"),     // 2*u8 - julian date
            VariableType::Integer => write!(f, "Integer"), // i32
            VariableType::Money => write!(f, "Money"),     // i32 - x/100 Dollar x%100 Cents
            VariableType::Real => write!(f, "Real"),       // f32
            VariableType::String => write!(f, "String"), // String without \0 and maximum length of 255 (Pascal like)
            VariableType::Time => write!(f, "Time"),     // u32 - Seconds elapsed since midnight
            VariableType::Byte => write!(f, "Byte"),     // u8
            VariableType::Word => write!(f, "Word"),     // u16
            VariableType::SByte => write!(f, "SByte"),   // i8
            VariableType::SWord => write!(f, "SWord"),   // i16
            VariableType::BigStr => write!(f, "BigStr"), // String (max 2kb)
            VariableType::DoubleReal => write!(f, "Double"), // f65
            VariableType::Function => write!(f, "FUNC"), // 2*u8
            VariableType::Procedure => write!(f, "PROC"), // 2*u8
            VariableType::DDate => write!(f, "DDate"),   // i32
            VariableType::Unknown => write!(f, "Unknown"),
        }
    }
}
