use std::fmt;

use crate::output_keyword;

use super::VariableValue;

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Debug)]
#[allow(dead_code)]
pub enum VariableType {
    Boolean = 0,
    Unsigned = 1,
    Date = 2,
    EDate = 3,
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
    Double = 14,
    Function = 15,
    Procedure = 16,
    DDate = 17,
    Unknown = 255,
}

impl VariableType {
    pub fn create_empty_value(&self) -> VariableValue {
        match self {
            VariableType::Boolean => VariableValue::Boolean(false),
            VariableType::Unsigned => VariableValue::Unsigned(0),
            VariableType::Date => VariableValue::Date(0),
            VariableType::EDate => VariableValue::EDate(0),
            VariableType::Integer => VariableValue::Integer(0),
            VariableType::Money => VariableValue::Money(0.0),
            VariableType::Real => VariableValue::Real(0.0),
            VariableType::String => VariableValue::String(String::new()),
            VariableType::Time => VariableValue::Time(0),
            VariableType::Byte => VariableValue::Byte(0),
            VariableType::Word => VariableValue::Word(0),
            VariableType::SByte => VariableValue::SByte(0),
            VariableType::SWord => VariableValue::SWord(0),
            VariableType::BigStr => VariableValue::String("".to_string()),
            VariableType::Double => VariableValue::Real(0.0),
            VariableType::Function => todo!(),
            VariableType::Procedure => todo!(),
            VariableType::DDate => VariableValue::Date(0),
            VariableType::Unknown => todo!(),
        }
    }
}

impl fmt::Display for VariableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariableType::Boolean => write!(f, "{}", output_keyword("Boolean")), // BOOL 0 = false, 1 = true
            VariableType::Unsigned => write!(f, "{}", output_keyword("Unsigned")), // u32
            VariableType::Date => write!(f, "{}", output_keyword("Date")), // 2*u8 - julian date
            VariableType::EDate => write!(f, "{}", output_keyword("EDate")), // 2*u8 - julian date
            VariableType::Integer => write!(f, "{}", output_keyword("Integer")), // i32
            VariableType::Money => write!(f, "{}", output_keyword("Money")), // i32 - x/100 Dollar x%100 Cents
            VariableType::Real => write!(f, "{}", output_keyword("Real")),   // f32
            VariableType::String => write!(f, "{}", output_keyword("String")), // String without \0 and maximum length of 255 (Pascal like)
            VariableType::Time => write!(f, "{}", output_keyword("Time")), // u32 - Seconds elapsed since midnight
            VariableType::Byte => write!(f, "{}", output_keyword("Byte")), // u8
            VariableType::Word => write!(f, "{}", output_keyword("Word")), // u16
            VariableType::SByte => write!(f, "{}", output_keyword("SByte")), // i8
            VariableType::SWord => write!(f, "{}", output_keyword("SWord")), // i16
            VariableType::BigStr => write!(f, "{}", output_keyword("BigStr")), // String (max 2kb)
            VariableType::Double => write!(f, "{}", output_keyword("Double")), // f65
            VariableType::Function => write!(f, "{}", output_keyword("FUNC")), // 2*u8
            VariableType::Procedure => write!(f, "{}", output_keyword("PROC")), // 2*u8
            VariableType::DDate => write!(f, "{}", output_keyword("DDate")), // i32
            VariableType::Unknown => write!(f, "{}", output_keyword("Unknown")),
        }
    }
}
