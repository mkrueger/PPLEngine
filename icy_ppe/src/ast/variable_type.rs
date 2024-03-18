use std::fmt;

use super::{VariableData, VariableValue};

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Debug, Default, Eq, Hash)]
#[allow(dead_code)]
pub enum VariableType {
    /// unsigned character (1 byte) 0 = FALSE, non-0 = TRUE
    Boolean = 0,

    /// 4-byte unsigned integer Range: 0 - 4,294,967,295
    Unsigned = 1,

    /// unsigned integer (2 bytes) PCBoard julian date (count of days since 1/1/1900)
    Date = 2,

    /// Julian date in earth date format Deals with dates formatted YYMM.DD Range: Same as DATE
    EDate = 3,

    /// signed long integer (4 bytes) Range: -2,147,483,648 → +2,147,483,647
    #[default]
    Integer = 4,
    /// signed long integer (4 bytes) Range: -$21,474,836.48 → +$21,474,836.47
    Money = 5,
    ///  4-byte floating point number Range: +/-3.4E-38 - +/-3.4E+38 (7-digit precision)
    Float = 6,
    /// far character pointer (4 bytes) NULL is an empty string non-NULL points to a string of some length less than or equal to 256
    String = 7,

    /// signed long integer (4 bytes) Count of seconds since midnight
    Time = 8,

    /// 1-byte unsigned integer Range: 0 - 255
    Byte = 9,
    /// 2-byte unsigned integer Range: 0 - 65,535
    Word = 10,
    /// 1-byte signed Integer Range: -128 - 127
    SByte = 11,
    /// 2-byte signed integer Range: -32,768 - 32,767
    SWord = 12,

    /// Allows up to 2048 characters per big string (up from 256 for STRING variables) May include CHR(0) characters in the middle of the big string (unlike STRING variables which may not)
    BigStr = 13,
    /// 8-byte floating point number Range: +/-1.7E-308 - +/-1.7E+308 (15-digit precision)
    Double = 14,

    Function = 15,
    Procedure = 16,

    /// Signed long integer for julian date. DDATE is for use with DBase date fields.
    /// It holds a long integer for julian dates.
    /// When coerced to string type it is in the format CCYYMMDD or 19940527
    DDate = 17,
    Unknown = 255,
}

impl VariableType {
    pub fn create_empty_value(&self) -> VariableValue {
        match self {
            VariableType::String | VariableType::BigStr => VariableValue::new_string(String::new()),
            _ => VariableValue::new(*self, VariableData::default()),
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
            VariableType::Float => write!(f, "Real"),      // f32
            VariableType::String => write!(f, "String"), // String without \0 and maximum length of 255 (Pascal like)
            VariableType::Time => write!(f, "Time"),     // u32 - Seconds elapsed since midnight
            VariableType::Byte => write!(f, "Byte"),     // u8
            VariableType::Word => write!(f, "Word"),     // u16
            VariableType::SByte => write!(f, "SByte"),   // i8
            VariableType::SWord => write!(f, "SWord"),   // i16
            VariableType::BigStr => write!(f, "BigStr"), // String (max 2kb)
            VariableType::Double => write!(f, "Double"), // f65
            VariableType::Function => write!(f, "FUNC"), // 2*u8
            VariableType::Procedure => write!(f, "PROC"), // 2*u8
            VariableType::DDate => write!(f, "DDate"),   // i32
            VariableType::Unknown => write!(f, "Unknown"),
        }
    }
}
