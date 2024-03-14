use std::{
    cmp::Ordering,
    fmt,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

use crate::{
    ast::variable_type::VariableType,
    executable::{FunctionValue, ProcedureValue},
};

#[derive(Clone, Copy, Debug)]
pub struct StdStruct {
    lo: u32,
    hi: u32,
}

#[derive(Clone, Copy)]
pub union VariableData {
    pub bool_value: bool,
    pub unsigned_value: u32,
    pub date_value: u32,
    pub ddate_value: i32,
    pub edate_value: u32,
    pub int_value: i32,
    pub money_value: i32,
    pub real_value: f32,
    pub dreal_value: f64,
    pub time_value: i32,
    pub byte_value: u8,
    pub word_value: u16,
    pub sword_value: i16,
    pub sbyte_value: i8,
    pub u64_value: u64,
    pub function_value: FunctionValue,
    pub procedure_value: ProcedureValue,

    pub std_struct: StdStruct,
}

impl Default for VariableData {
    fn default() -> Self {
        VariableData { unsigned_value: 0 }
    }
}

impl fmt::Debug for VariableData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", unsafe { self.unsigned_value })
    }
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd)]
pub enum VariableValue {
    #[default]
    None,
    String(String),

    Dim1(Vec<Variable>),
    Dim2(Vec<Vec<Variable>>),
    Dim3(Vec<Vec<Vec<Variable>>>),
}

#[derive(Debug, Clone)]
pub struct Variable {
    vtype: VariableType,
    data: VariableData,
    pub generic_data: VariableValue,
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            match self.vtype {
                VariableType::Boolean => write!(f, "{}", self.data.bool_value),
                VariableType::Unsigned => write!(f, "{}", self.data.unsigned_value),
                VariableType::Date => write!(f, "{}", self.data.date_value),
                VariableType::EDate => write!(f, "{}", self.data.edate_value),
                VariableType::Integer => write!(f, "{}", self.data.int_value),
                VariableType::Money => write!(f, "{}", self.data.money_value),
                VariableType::Real => write!(f, "{}", self.data.real_value),
                VariableType::DoubleReal => write!(f, "{}", self.data.dreal_value),
                VariableType::Time => write!(f, "{}", self.data.time_value),
                VariableType::Byte => write!(f, "{}", self.data.byte_value),
                VariableType::Word => write!(f, "{}", self.data.word_value),
                VariableType::SByte => write!(f, "{}", self.data.sbyte_value),
                VariableType::SWord => write!(f, "{}", self.data.sword_value),

                VariableType::String | VariableType::BigStr => {
                    if let VariableValue::String(s) = &self.generic_data {
                        write!(f, "{}", s)
                    } else {
                        write!(f, "")
                    }
                }
                _ => {
                    write!(f, "")
                }
            }
        }
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        let dest_type: VariableType = promote_to(self.vtype, other.vtype);
        unsafe {
            match dest_type {
                VariableType::Boolean => self.data.bool_value == other.data.bool_value,
                VariableType::Unsigned => self.data.unsigned_value == other.data.unsigned_value,
                VariableType::Date => self.data.date_value == other.data.date_value,
                VariableType::DDate => self.data.ddate_value == other.data.ddate_value,
                VariableType::EDate => self.data.edate_value == other.data.edate_value,

                VariableType::Integer => self.data.int_value == other.data.int_value,
                VariableType::Money => self.data.money_value == other.data.money_value,
                VariableType::String | VariableType::BigStr => {
                    self.as_string() == other.as_string()
                }

                VariableType::Time => self.data.time_value == other.data.time_value,
                VariableType::Real => self.data.real_value == other.data.real_value,
                VariableType::DoubleReal => self.data.dreal_value == other.data.dreal_value,
                VariableType::Byte => self.data.byte_value == other.data.byte_value,
                VariableType::SByte => self.data.sbyte_value == other.data.sbyte_value,
                VariableType::Word => self.data.word_value == other.data.word_value,
                VariableType::SWord => self.data.sword_value == other.data.sword_value,

                _ => false,
            }
        }
    }
}

fn promote_to(l: VariableType, r: VariableType) -> VariableType {
    if l == r {
        return l;
    }
    if (l == VariableType::String || l == VariableType::BigStr)
        && (r == VariableType::String || r == VariableType::BigStr)
    {
        return VariableType::BigStr;
    }
    if (l == VariableType::Real || l == VariableType::DoubleReal)
        && (r == VariableType::Real || r == VariableType::DoubleReal)
    {
        return VariableType::DoubleReal;
    }
    VariableType::Integer
}

impl Add<Variable> for Variable {
    type Output = Variable;

    fn add(self, other: Variable) -> Self {
        let mut dest_type: VariableType = promote_to(self.vtype, other.vtype);
        match dest_type {
            VariableType::Boolean
            | VariableType::Date
            | VariableType::EDate
            | VariableType::Money
            | VariableType::Time
            | VariableType::DDate => {
                dest_type = VariableType::Integer;
            }
            _ => {}
        }
        let mut data = VariableData::default();
        let mut generic_data = VariableValue::None;
        unsafe {
            match dest_type {
                VariableType::Unsigned => {
                    data.unsigned_value = self
                        .data
                        .unsigned_value
                        .wrapping_add(other.data.unsigned_value)
                }
                VariableType::Integer => {
                    data.int_value = self.data.int_value.wrapping_add(other.data.int_value)
                }
                VariableType::Real => {
                    data.real_value = self.data.real_value + other.data.real_value
                }
                VariableType::DoubleReal => {
                    data.dreal_value = self.data.dreal_value + other.data.dreal_value
                }

                VariableType::String | VariableType::BigStr => {
                    generic_data = VariableValue::String(format!("{}{}", self, other))
                }

                VariableType::Byte => {
                    data.byte_value = self.data.byte_value.wrapping_add(other.data.byte_value)
                }
                VariableType::SByte => {
                    data.sbyte_value = self.data.sbyte_value.wrapping_add(other.data.sbyte_value)
                }
                VariableType::Word => {
                    data.word_value = self.data.word_value.wrapping_add(other.data.word_value)
                }
                VariableType::SWord => {
                    data.sword_value = self.data.sword_value.wrapping_add(other.data.sword_value)
                }

                _ => {
                    panic!("unsupported lvalue for add {self:?}");
                }
            }
        }
        Self {
            vtype: dest_type,
            data,
            generic_data,
        }
    }
}

impl Sub<Variable> for Variable {
    type Output = Variable;

    fn sub(self, other: Variable) -> Variable {
        let mut dest_type: VariableType = promote_to(self.vtype, other.vtype);
        match dest_type {
            VariableType::Boolean
            | VariableType::Date
            | VariableType::EDate
            | VariableType::Money
            | VariableType::Time
            | VariableType::DDate => {
                dest_type = VariableType::Integer;
            }
            VariableType::String | VariableType::BigStr => {
                let l = self.as_string().parse::<i32>().unwrap_or_default();
                let r = other.as_string().parse::<i32>().unwrap_or_default();
                return Self {
                    vtype: VariableType::Integer,
                    data: VariableData {
                        int_value: l.wrapping_sub(r),
                    },
                    generic_data: VariableValue::None,
                };
            }
            _ => {}
        }
        let mut data = VariableData::default();
        let generic_data = VariableValue::None;
        unsafe {
            match dest_type {
                VariableType::Unsigned => {
                    data.unsigned_value = self
                        .data
                        .unsigned_value
                        .wrapping_sub(other.data.unsigned_value)
                }
                VariableType::Integer => {
                    data.int_value = self.data.int_value.wrapping_sub(other.data.int_value)
                }
                VariableType::Real => {
                    data.real_value = self.data.real_value - other.data.real_value
                }
                VariableType::DoubleReal => {
                    data.dreal_value = self.data.dreal_value - other.data.dreal_value
                }
                VariableType::Byte => {
                    data.byte_value = self.data.byte_value.wrapping_sub(other.data.byte_value)
                }
                VariableType::SByte => {
                    data.sbyte_value = self.data.sbyte_value.wrapping_sub(other.data.sbyte_value)
                }
                VariableType::Word => {
                    data.word_value = self.data.word_value.wrapping_sub(other.data.word_value)
                }
                VariableType::SWord => {
                    data.sword_value = self.data.sword_value.wrapping_sub(other.data.sword_value)
                }
                _ => {
                    panic!("unsupported lvalue for add {self:?}");
                }
            }
        }
        Self {
            vtype: dest_type,
            data,
            generic_data,
        }
    }
}

impl Mul<Variable> for Variable {
    type Output = Variable;

    fn mul(self, other: Variable) -> Variable {
        let mut dest_type: VariableType = promote_to(self.vtype, other.vtype);
        match dest_type {
            VariableType::Boolean
            | VariableType::Date
            | VariableType::EDate
            | VariableType::Money
            | VariableType::Time
            | VariableType::DDate => {
                dest_type = VariableType::Integer;
            }
            VariableType::String | VariableType::BigStr => {
                let l = self.as_string().parse::<i32>().unwrap_or_default();
                let r = other.as_string().parse::<i32>().unwrap_or_default();
                return Self {
                    vtype: VariableType::Integer,
                    data: VariableData {
                        int_value: l.wrapping_mul(r),
                    },
                    generic_data: VariableValue::None,
                };
            }
            _ => {}
        }
        let mut data = VariableData::default();
        let generic_data = VariableValue::None;
        unsafe {
            match dest_type {
                VariableType::Unsigned => {
                    data.unsigned_value = self
                        .data
                        .unsigned_value
                        .wrapping_mul(other.data.unsigned_value)
                }
                VariableType::Integer => {
                    data.int_value = self.data.int_value.wrapping_mul(other.data.int_value)
                }
                VariableType::Real => {
                    data.real_value = self.data.real_value * other.data.real_value
                }
                VariableType::DoubleReal => {
                    data.dreal_value = self.data.dreal_value * other.data.dreal_value
                }
                VariableType::Byte => {
                    data.byte_value = self.data.byte_value.wrapping_mul(other.data.byte_value)
                }
                VariableType::SByte => {
                    data.sbyte_value = self.data.sbyte_value.wrapping_mul(other.data.sbyte_value)
                }
                VariableType::Word => {
                    data.word_value = self.data.word_value.wrapping_mul(other.data.word_value)
                }
                VariableType::SWord => {
                    data.sword_value = self.data.sword_value.wrapping_mul(other.data.sword_value)
                }
                _ => {
                    panic!("unsupported lvalue for add {self:?}");
                }
            }
        }
        Self {
            vtype: dest_type,
            data,
            generic_data,
        }
    }
}

impl Div<Variable> for Variable {
    type Output = Variable;

    fn div(self, other: Variable) -> Variable {
        let mut dest_type: VariableType = promote_to(self.vtype, other.vtype);
        match dest_type {
            VariableType::Boolean
            | VariableType::Date
            | VariableType::EDate
            | VariableType::Money
            | VariableType::Time
            | VariableType::DDate => {
                dest_type = VariableType::Integer;
            }
            VariableType::String | VariableType::BigStr => {
                let l = self.as_string().parse::<i32>().unwrap_or_default();
                let r = other.as_string().parse::<i32>().unwrap_or_default();
                return Self {
                    vtype: VariableType::Integer,
                    data: VariableData {
                        int_value: l.wrapping_div(r),
                    },
                    generic_data: VariableValue::None,
                };
            }
            _ => {}
        }
        let mut data = VariableData::default();
        let generic_data = VariableValue::None;
        unsafe {
            match dest_type {
                VariableType::Unsigned => {
                    data.unsigned_value = self
                        .data
                        .unsigned_value
                        .wrapping_div(other.data.unsigned_value)
                }
                VariableType::Integer => {
                    data.int_value = self.data.int_value.wrapping_div(other.data.int_value)
                }
                VariableType::Real => {
                    data.real_value = self.data.real_value / other.data.real_value
                }
                VariableType::DoubleReal => {
                    data.dreal_value = self.data.dreal_value / other.data.dreal_value
                }
                VariableType::Byte => {
                    data.byte_value = self.data.byte_value.wrapping_div(other.data.byte_value)
                }
                VariableType::SByte => {
                    data.sbyte_value = self.data.sbyte_value.wrapping_div(other.data.sbyte_value)
                }
                VariableType::Word => {
                    data.word_value = self.data.word_value.wrapping_div(other.data.word_value)
                }
                VariableType::SWord => {
                    data.sword_value = self.data.sword_value.wrapping_div(other.data.sword_value)
                }
                _ => {
                    panic!("unsupported lvalue for add {self:?}");
                }
            }
        }
        Self {
            vtype: dest_type,
            data,
            generic_data,
        }
    }
}

impl Rem<Variable> for Variable {
    type Output = Variable;

    fn rem(self, other: Variable) -> Variable {
        let mut dest_type: VariableType = promote_to(self.vtype, other.vtype);
        match dest_type {
            VariableType::Boolean
            | VariableType::Date
            | VariableType::EDate
            | VariableType::Money
            | VariableType::Time
            | VariableType::DDate => {
                dest_type = VariableType::Integer;
            }
            VariableType::Real | VariableType::DoubleReal => {
                dest_type = VariableType::Integer;
            }

            VariableType::String | VariableType::BigStr => {
                let l = self.as_string().parse::<i32>().unwrap_or_default();
                let r = other.as_string().parse::<i32>().unwrap_or_default();
                return Self {
                    vtype: VariableType::Integer,
                    data: VariableData {
                        int_value: l.wrapping_rem(r),
                    },
                    generic_data: VariableValue::None,
                };
            }
            _ => {}
        }
        let mut data = VariableData::default();
        let generic_data = VariableValue::None;
        unsafe {
            match dest_type {
                VariableType::Unsigned => {
                    data.unsigned_value = self
                        .data
                        .unsigned_value
                        .wrapping_rem(other.data.unsigned_value)
                }
                VariableType::Integer => {
                    data.int_value = self.data.int_value.wrapping_rem(other.data.int_value)
                }
                VariableType::Byte => {
                    data.byte_value = self.data.byte_value.wrapping_rem(other.data.byte_value)
                }
                VariableType::SByte => {
                    data.sbyte_value = self.data.sbyte_value.wrapping_rem(other.data.sbyte_value)
                }
                VariableType::Word => {
                    data.word_value = self.data.word_value.wrapping_rem(other.data.word_value)
                }
                VariableType::SWord => {
                    data.sword_value = self.data.sword_value.wrapping_rem(other.data.sword_value)
                }
                _ => {
                    panic!("unsupported lvalue for add {self:?}");
                }
            }
        }
        Self {
            vtype: dest_type,
            data,
            generic_data,
        }
    }
}

impl PartialOrd for Variable {
    fn partial_cmp(&self, other: &Variable) -> Option<Ordering> {
        let dest_type: VariableType = promote_to(self.vtype, other.vtype);
        unsafe {
            match dest_type {
                VariableType::Boolean => Some(self.data.bool_value.cmp(&other.data.bool_value)),
                VariableType::Unsigned => {
                    Some(self.data.unsigned_value.cmp(&other.data.unsigned_value))
                }
                VariableType::Date => Some(self.data.date_value.cmp(&other.data.date_value)),
                VariableType::DDate => Some(self.data.ddate_value.cmp(&other.data.ddate_value)),
                VariableType::EDate => Some(self.data.edate_value.cmp(&other.data.edate_value)),

                VariableType::Integer => Some(self.data.int_value.cmp(&other.data.int_value)),
                VariableType::Money => Some(self.data.money_value.cmp(&other.data.money_value)),
                VariableType::String | VariableType::BigStr => {
                    Some(self.as_string().cmp(&other.as_string()))
                }

                VariableType::Time => Some(self.data.time_value.cmp(&other.data.time_value)),
                VariableType::Real => self.data.real_value.partial_cmp(&other.data.real_value),
                VariableType::DoubleReal => {
                    self.data.dreal_value.partial_cmp(&other.data.dreal_value)
                }
                VariableType::Byte => Some(self.data.byte_value.cmp(&other.data.byte_value)),
                VariableType::SByte => Some(self.data.sbyte_value.cmp(&other.data.sbyte_value)),
                VariableType::Word => Some(self.data.word_value.cmp(&other.data.word_value)),
                VariableType::SWord => Some(self.data.sword_value.cmp(&other.data.sword_value)),

                _ => None,
            }
        }
    }
}

impl Neg for Variable {
    type Output = Variable;

    fn neg(self) -> Variable {
        let mut dest_type = self.vtype;
        match dest_type {
            VariableType::Boolean
            | VariableType::Unsigned
            | VariableType::Date
            | VariableType::EDate
            | VariableType::Money
            | VariableType::Time
            | VariableType::Byte
            | VariableType::Word
            | VariableType::DDate => {
                dest_type = VariableType::Integer;
            }
            VariableType::String | VariableType::BigStr => {
                let l = self.as_string().parse::<i32>().unwrap_or_default();
                return Self {
                    vtype: VariableType::Integer,
                    data: VariableData { int_value: -l },
                    generic_data: VariableValue::None,
                };
            }
            _ => {}
        }
        let mut data = VariableData::default();
        let generic_data = VariableValue::None;
        unsafe {
            match dest_type {
                VariableType::Integer => data.int_value = -self.data.int_value,
                VariableType::SByte => data.sbyte_value = -self.data.sbyte_value,
                VariableType::SWord => data.sword_value = -self.data.sword_value,
                VariableType::Real => data.real_value = -self.data.real_value,
                VariableType::DoubleReal => data.dreal_value = -self.data.dreal_value,
                _ => {
                    panic!("unsupported lvalue for add {self:?}");
                }
            }
        }
        Self {
            vtype: dest_type,
            data,
            generic_data,
        }
    }
}

/*
impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Variable::Money(i) => write!(f, "${i}"),
            Variable::Integer(i) => write!(f, "{i}"),
            Variable::Unsigned(i) | Variable::Time(i) => write!(f, "{i}"),
            Variable::Byte(i) => write!(f, "{i}"),
            Variable::SByte(i) => write!(f, "{i}"),
            Variable::Date(i) | Variable::Word(i) | Variable::EDate(i) => {
                write!(f, "{i}")
            }
            Variable::SWord(i) => write!(f, "{i}"),
            Variable::String(str) => write!(f, "{str}"),
            Variable::Real(i) => write!(f, "{i}"),
            Variable::Boolean(b) => {
                if *b {
                    write!(f, "1")
                } else {
                    write!(f, "0")
                }
            }
            Variable::Dim1(var_type, data) => {
                write!(f, "{}({})", var_type, data.len())
            }
            Variable::Dim2(var_type, data) => {
                write!(f, "{}({}, {})", var_type, data.len(), data[0].len())
            }
            Variable::Dim3(var_type, data) => write!(
                f,
                "{}({}, {}, {})",
                var_type,
                data.len(),
                data[0].len(),
                data[0][0].len()
            ),
        }
    }
}
*/

#[allow(clippy::needless_pass_by_value)]
impl Variable {
    pub fn new(vtype: VariableType, data: VariableData) -> Self {
        Self {
            vtype,
            data,
            generic_data: VariableValue::None,
        }
    }

    pub fn new_string(s: String) -> Self {
        Self {
            vtype: VariableType::String,
            data: VariableData::default(),
            generic_data: VariableValue::String(s),
        }
    }

    pub fn new_int(i: i32) -> Self {
        Self {
            vtype: VariableType::Integer,
            data: VariableData { int_value: i },
            generic_data: VariableValue::None,
        }
    }

    pub fn new_bool(b: bool) -> Self {
        Self {
            vtype: VariableType::Boolean,
            data: VariableData { bool_value: b },
            generic_data: VariableValue::None,
        }
    }

    pub fn new_vector(variable_type: VariableType, vec: Vec<Variable>) -> Self {
        Self {
            vtype: variable_type,
            data: VariableData::default(),
            generic_data: VariableValue::Dim1(vec),
        }
    }

    pub fn new_matrix(variable_type: VariableType, vec: Vec<Vec<Variable>>) -> Self {
        Self {
            vtype: variable_type,
            data: VariableData::default(),
            generic_data: VariableValue::Dim2(vec),
        }
    }

    pub fn new_cube(variable_type: VariableType, vec: Vec<Vec<Vec<Variable>>>) -> Self {
        Self {
            vtype: variable_type,
            data: VariableData::default(),
            generic_data: VariableValue::Dim3(vec),
        }
    }

    pub fn get_type(&self) -> VariableType {
        self.vtype
    }

    pub fn get_dimensions(&self) -> u8 {
        match self.generic_data {
            VariableValue::Dim1(_) => 1,
            VariableValue::Dim2(_) => 2,
            VariableValue::Dim3(_) => 3,
            _ => 0,
        }
    }

    pub fn get_vector_size(&self) -> usize {
        match &self.generic_data {
            VariableValue::Dim1(data) => data.len(),
            VariableValue::Dim2(data) => data.len(),
            VariableValue::Dim3(data) => data.len(),
            _ => 0,
        }
    }

    pub fn get_matrix_size(&self) -> usize {
        match &self.generic_data {
            VariableValue::Dim2(data) => data[0].len(),
            VariableValue::Dim3(data) => data[0].len(),
            _ => 0,
        }
    }

    pub fn get_cube_size(&self) -> usize {
        match &self.generic_data {
            VariableValue::Dim3(data) => data[0][0].len(),
            _ => 0,
        }
    }

    pub fn get_u64_value(&self) -> u64 {
        unsafe { self.data.u64_value }
    }

    /// .
    ///
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn pow(&self, other: Variable) -> Variable {
        let mut dest_type: VariableType = promote_to(self.vtype, other.vtype);
        match dest_type {
            VariableType::Boolean
            | VariableType::Unsigned
            | VariableType::Date
            | VariableType::EDate
            | VariableType::Money
            | VariableType::Time
            | VariableType::Byte
            | VariableType::Word
            | VariableType::DDate => {
                dest_type = VariableType::Integer;
            }
            VariableType::String | VariableType::BigStr => {
                let l = self.as_string().parse::<i32>().unwrap_or_default();
                let r = other.as_string().parse::<i32>().unwrap_or_default();
                return Self {
                    vtype: VariableType::Integer,
                    data: VariableData {
                        int_value: l.wrapping_pow(r as u32),
                    },
                    generic_data: VariableValue::None,
                };
            }
            _ => {}
        }
        let mut data = VariableData::default();
        let generic_data = VariableValue::None;
        unsafe {
            match dest_type {
                VariableType::Integer => {
                    data.int_value = self
                        .data
                        .int_value
                        .wrapping_pow(other.data.int_value as u32)
                }
                VariableType::Real => {
                    data.real_value = self.data.real_value.powf(other.data.real_value)
                }
                VariableType::DoubleReal => {
                    data.dreal_value = self.data.dreal_value.powf(other.data.dreal_value)
                }
                VariableType::SByte => {
                    data.sbyte_value = self
                        .data
                        .sbyte_value
                        .wrapping_pow(other.data.sbyte_value as u32)
                }
                VariableType::SWord => {
                    data.sword_value = self
                        .data
                        .sword_value
                        .wrapping_pow(other.data.sword_value as u32)
                }
                _ => {
                    panic!("unsupported lvalue for add {self:?}");
                }
            }
        }
        Self {
            vtype: dest_type,
            data,
            generic_data,
        }
    }

    /// .
    ///
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn not(&self) -> Variable {
        unsafe {
            Self {
                vtype: VariableType::Boolean,
                data: VariableData {
                    bool_value: !self.data.bool_value,
                },
                generic_data: VariableValue::None,
            }
        }
    }

    /// .
    ///
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn abs(&self) -> Variable {
        let mut dest_type: VariableType = self.vtype;
        match dest_type {
            VariableType::Boolean
            | VariableType::Unsigned
            | VariableType::Date
            | VariableType::EDate
            | VariableType::Money
            | VariableType::Time
            | VariableType::Byte
            | VariableType::Word
            | VariableType::DDate => {
                dest_type = VariableType::Integer;
            }
            VariableType::String | VariableType::BigStr => {
                let l = self.as_string().parse::<i32>().unwrap_or_default();
                return Self {
                    vtype: VariableType::Integer,
                    data: VariableData { int_value: l.abs() },
                    generic_data: VariableValue::None,
                };
            }
            _ => {}
        }
        let mut data = VariableData::default();
        let generic_data = VariableValue::None;
        unsafe {
            match dest_type {
                VariableType::Integer => data.int_value = self.data.int_value.abs(),
                VariableType::Real => data.real_value = self.data.real_value.abs(),
                VariableType::DoubleReal => data.dreal_value = self.data.dreal_value.abs(),
                VariableType::SByte => data.sbyte_value = self.data.sbyte_value.abs(),
                VariableType::SWord => data.sword_value = self.data.sword_value.abs(),
                _ => {
                    panic!("unsupported lvalue for add {self:?}");
                }
            }
        }
        Self {
            vtype: dest_type,
            data,
            generic_data,
        }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn as_bool(&self) -> bool {
        unsafe { self.data.bool_value }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn as_int(&self) -> i32 {
        unsafe { self.data.int_value }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn as_string(&self) -> String {
        unsafe {
            match &self.generic_data {
                VariableValue::String(s) => s.to_string(),
                _ => match self.vtype {
                    VariableType::Boolean => {
                        if self.data.bool_value {
                            "1".to_string()
                        } else {
                            "0".to_string()
                        }
                    }
                    VariableType::Unsigned => self.data.unsigned_value.to_string(),
                    VariableType::Date => self.data.date_value.to_string(),
                    VariableType::EDate => self.data.edate_value.to_string(),
                    VariableType::Integer => self.data.int_value.to_string(),
                    VariableType::Money => self.data.money_value.to_string(),
                    VariableType::Real => self.data.real_value.to_string(),
                    VariableType::DoubleReal => self.data.dreal_value.to_string(),
                    VariableType::Time => self.data.time_value.to_string(),
                    VariableType::Byte => self.data.byte_value.to_string(),
                    VariableType::Word => self.data.word_value.to_string(),
                    VariableType::SByte => self.data.sbyte_value.to_string(),
                    VariableType::SWord => self.data.sword_value.to_string(),
                    _ => "".to_string(),
                },
            }
        }
    }
}

/// .
///
/// # Panics
///
/// Panics if .
pub fn convert_to(var_type: VariableType, value: &Variable) -> Variable {
    let mut res = value.clone();
    res.vtype = var_type;
    if var_type == VariableType::String || var_type == VariableType::BigStr {
        res.generic_data = VariableValue::String(value.as_string());
    }

    res
}

#[cfg(test)]
mod tests {
    use crate::ast::variable_value::VariableData;

    #[test]
    fn check_variable_size() {
        assert_eq!(8, std::mem::size_of::<VariableData>());
    }
}
