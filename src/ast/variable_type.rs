use crate::output_keyword;

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Debug)]
#[allow(dead_code)]
pub enum VariableType
{
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
    pub fn to_string(&self) -> String
    {
        match self {
            VariableType::Boolean => output_keyword("Boolean"),    // BOOL 0 = false, 1 = true
            VariableType::Unsigned => output_keyword("Unsigned"),  // u32
            VariableType::Date => output_keyword("Date"),          // 2*u8 - julian date
            VariableType::EDate => output_keyword("EDate"),        // 2*u8 - julian date
            VariableType::Integer => output_keyword("Integer"),    // i32
            VariableType::Money => output_keyword("Money"),        // i32 - x/100 Dollar x%100 Cents 
            VariableType::Real => output_keyword("Real"),          // f32
            VariableType::String => output_keyword("String"),      // String without \0 and maximum length of 255 (Pascal like)
            VariableType::Time => output_keyword("Time"),          // u32 - Seconds elapsed since midnight
            VariableType::Byte => output_keyword("Byte"),          // u8
            VariableType::Word => output_keyword("Word"),          // u16
            VariableType::SByte => output_keyword("SByte") ,       // i8
            VariableType::SWord => output_keyword("SWord"),        // i16 
            VariableType::BigStr => output_keyword("BigStr"),      // String (max 2kb)
            VariableType::Double => output_keyword("Double"),      // f65
            VariableType::Function => output_keyword("FUNC"),      // 2*u8
            VariableType::Procedure => output_keyword("PROC"),     // 2*u8
            VariableType::DDate => output_keyword("DDate"),        // i32
            VariableType::Unknown => output_keyword("Unknown")
        }
    }
}