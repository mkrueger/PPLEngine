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
            VariableType::Boolean => "BOOLEAN".to_string(),
            VariableType::Unsigned => "UNSIGNED".to_string(),
            VariableType::Date => "DATE".to_string(),
            VariableType::EDate => "EDATE".to_string(),
            VariableType::Integer => "INTEGER".to_string(),
            VariableType::Money => "MONEY".to_string(),
            VariableType::Real => "REAL".to_string(),
            VariableType::String => "STRING".to_string(),
            VariableType::Time => "TIME".to_string(),
            VariableType::Byte => "BYTE".to_string(),
            VariableType::Word => "WORD".to_string(),
            VariableType::SByte => "SBYTE".to_string(),
            VariableType::SWord => "INT".to_string(),
            VariableType::BigStr => "BIGSTR".to_string(),
            VariableType::Double => "DOUBLE".to_string(),
            VariableType::Function => "???FUNCTION".to_string(),
            VariableType::Procedure => "???PROCEDURE".to_string(),
            VariableType::DDate => "DATE".to_string(),
            VariableType::Unknown => "???".to_string()
        }
    }
}