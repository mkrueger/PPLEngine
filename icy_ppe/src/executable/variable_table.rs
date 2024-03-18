use std::fmt;

use crate::crypt::encrypt;

use super::{ExecutableError, GenericVariableData, VariableData, VariableType, VariableValue};

#[derive(Clone, Default, Debug)]
pub struct VarHeader {
    pub id: usize,
    pub dim: u8,
    pub vector_size: usize,
    pub matrix_size: usize,
    pub cube_size: usize,
    pub variable_type: VariableType,
    pub flags: u8,
}

impl VarHeader {
    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn from_bytes(cur_block: &[u8]) -> VarHeader {
        let dim = cur_block[2];
        assert!(dim <= 3, "Invalid dimension: {dim}");
        Self {
            id: u16::from_le_bytes(cur_block[0..2].try_into().unwrap()) as usize,
            dim,
            vector_size: u16::from_le_bytes(cur_block[3..5].try_into().unwrap()) as usize,
            matrix_size: u16::from_le_bytes(cur_block[5..7].try_into().unwrap()) as usize,
            cube_size: u16::from_le_bytes(cur_block[7..9].try_into().unwrap()) as usize,
            variable_type: unsafe { ::std::mem::transmute(cur_block[9]) },
            flags: cur_block[10],
        }
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut buffer = Vec::new();
        buffer.extend(u16::to_le_bytes(self.id as u16));
        assert!(self.dim <= 3, "Invalid dimension: {}", self.dim);
        buffer.push(self.dim);
        buffer.extend(u16::to_le_bytes(self.vector_size as u16));
        buffer.extend(u16::to_le_bytes(self.matrix_size as u16));
        buffer.extend(u16::to_le_bytes(self.cube_size as u16));

        buffer.push(self.variable_type as u8);
        buffer.push(self.flags);
        buffer
    }

    /// Returns the create generic data of this [`VarHeader`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn create_generic_data(&self) -> GenericVariableData {
        match self.dim {
            0 => GenericVariableData::None,
            1 => GenericVariableData::Dim1(vec![
                self.variable_type.create_empty_value();
                self.vector_size
            ]),
            2 => GenericVariableData::Dim2(vec![
                vec![
                    self.variable_type.create_empty_value();
                    self.matrix_size
                ];
                self.vector_size
            ]),
            3 => GenericVariableData::Dim3(vec![
                vec![
                    vec![
                        self.variable_type.create_empty_value();
                        self.cube_size
                    ];
                    self.matrix_size
                ];
                self.vector_size
            ]),
            _ => panic!("Invalid dimension: {}", self.dim),
        }
    }
}

#[derive(Clone, Copy, Default)]
pub struct FunctionValue {
    pub parameters: u8,
    pub local_variables: u8,
    pub start_offset: u16,
    pub first_var_id: i16,
    pub return_var: i16,
}

impl fmt::Debug for FunctionValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "parameters:{} locals:{} offset:{:04X}h first:{:04X}h return:{:04X}h",
            self.parameters,
            self.local_variables,
            self.start_offset,
            self.first_var_id,
            self.return_var
        )
    }
}

#[derive(Clone, Copy, Default)]
pub struct ProcedureValue {
    pub parameters: u8,
    pub local_variables: u8,
    pub start_offset: u16,
    pub first_var_id: i16,
    pub pass_flags: u16,
}

impl ProcedureValue {
    pub fn to_data(self) -> VariableData {
        let mut res = VariableData::default();
        res.procedure_value = self;
        res
    }
}

impl fmt::Debug for ProcedureValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "parameters:{} locals:{} offset:{:04X}h first:{:04X}h pass:{:b}b",
            self.parameters,
            self.local_variables,
            self.start_offset,
            self.first_var_id,
            self.pass_flags
        )
    }
}

impl FunctionValue {
    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn from_bytes(cur_buf: &[u8]) -> FunctionValue {
        Self {
            parameters: cur_buf[4],
            local_variables: cur_buf[5],
            start_offset: u16::from_le_bytes((cur_buf[6..=7]).try_into().unwrap()),
            first_var_id: i16::from_le_bytes((cur_buf[8..=9]).try_into().unwrap()),
            return_var: i16::from_le_bytes((cur_buf[10..=11]).try_into().unwrap()),
        }
    }

    pub fn append(&self, buffer: &mut Vec<u8>) {
        buffer.push(self.parameters);
        buffer.push(self.local_variables);
        buffer.extend(u16::to_le_bytes(self.start_offset));
        buffer.extend(i16::to_le_bytes(self.first_var_id));
        buffer.extend(i16::to_le_bytes(self.return_var));
    }

    pub fn to_data(self) -> VariableData {
        let mut res = VariableData::default();
        res.function_value = self;
        res
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum EntryType {
    #[default]
    Constant,
    UserVariable,
    Variable,
    FunctionResult,
    Parameter,
    Function,
    Procedure,
}
impl EntryType {
    pub fn use_name(self) -> bool {
        self != EntryType::Constant
    }
}

#[derive(Clone, Default, Debug)]
pub struct VariableEntry {
    pub header: VarHeader,
    pub name: String,
    pub entry_type: EntryType,
    pub number: usize,
    pub value: VariableValue,
    pub function_id: usize,
}

impl VariableEntry {
    pub fn new(header: VarHeader, variable: VariableValue) -> Self {
        Self {
            header,
            name: "Unnamed".to_string(),
            number: 0,
            value: variable,
            function_id: 0,
            entry_type: EntryType::Constant,
        }
    }

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    pub fn get_type(&self) -> EntryType {
        self.entry_type
    }

    pub fn set_type(&mut self, entry_type: EntryType) {
        self.entry_type = entry_type;
    }

    pub fn report_variable_usage(&mut self) {
        if self.entry_type == EntryType::Constant {
            self.entry_type = EntryType::Variable;
        }
    }

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn to_buffer(&self, version: u16) -> Result<Vec<u8>, ExecutableError> {
        let mut buffer = Vec::new();
        buffer.extend(u16::to_le_bytes(self.header.id as u16));
        buffer.push(self.header.dim);
        buffer.extend(u16::to_le_bytes(self.header.vector_size as u16));
        buffer.extend(u16::to_le_bytes(self.header.matrix_size as u16));
        buffer.extend(u16::to_le_bytes(self.header.cube_size as u16));

        buffer.push(self.header.variable_type as u8);
        buffer.push(self.header.flags);
        encrypt(&mut buffer, version);

        let b = buffer.len();
        if self.header.variable_type == VariableType::Procedure
            || self.header.variable_type == VariableType::Function
        {
            buffer.push(0);
            buffer.push(0);
            buffer.push(self.header.variable_type as u8);
            buffer.push(0);
            unsafe {
                self.value.data.function_value.append(&mut buffer);
            }
            encrypt(&mut buffer[b..], version);
        } else if self.header.variable_type == VariableType::String {
            if self.header.dim == 0 {
                let GenericVariableData::String(s) = &self.value.generic_data else {
                    return Err(ExecutableError::StringTypeInvalid(self.value.vtype));
                };
                if s.len() > u16::MAX as usize {
                    return Err(ExecutableError::StringConstantTooLong(s.len()));
                }
                let mut string_buffer: Vec<u8> = Vec::new();
                for c in s.chars() {
                    if let Some(b) = crate::tables::UNICODE_TO_CP437.get(&c) {
                        string_buffer.push(*b);
                    } else {
                        string_buffer.push(c as u8);
                    }
                }
                string_buffer.push(0);

                buffer.extend_from_slice(&u16::to_le_bytes(string_buffer.len() as u16));
                encrypt(&mut string_buffer, version);
                buffer.extend(string_buffer);
            } else {
                buffer.extend_from_slice(&[0, 0]);
            };
        } else {
            // VTABLE - get's ignored by PCBoard - pure garbage
            buffer.push(0);
            buffer.push(0);

            // variable type
            buffer.push(self.header.variable_type as u8);
            buffer.push(0);

            buffer.extend_from_slice(&u64::to_le_bytes(self.value.get_u64_value()));
            encrypt(&mut buffer[b..], version);
        }
        Ok(buffer)
    }
}
