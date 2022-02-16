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
            VariableType::Boolean => output_keyword("Boolean"),
            VariableType::Unsigned => output_keyword("Unsigned"),
            VariableType::Date => output_keyword("Date"),
            VariableType::EDate => output_keyword("EDate"),
            VariableType::Integer => output_keyword("Integer"),
            VariableType::Money => output_keyword("Money"),
            VariableType::Real => output_keyword("Real"),
            VariableType::String => output_keyword("String"),
            VariableType::Time => output_keyword("Time"),
            VariableType::Byte => output_keyword("Byte"),
            VariableType::Word => output_keyword("Word"),
            VariableType::SByte => output_keyword("SByte"),
            VariableType::SWord => output_keyword("Int"),
            VariableType::BigStr => output_keyword("BigStr"),
            VariableType::Double => output_keyword("Double"),
            VariableType::Function => output_keyword("???FUNCTION"),
            VariableType::Procedure => output_keyword("???PROCEDURE"),
            VariableType::DDate => output_keyword("DDate"),
            VariableType::Unknown => output_keyword("Unknown")
        }
    }
}