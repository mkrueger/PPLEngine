use std::collections::HashMap;
use std::fs::File;
use std::io::{stdout, Read};

use crossterm::execute;
use crossterm::style::{Attribute, Print, SetAttribute};
use thiserror::Error;

use crate::crypt::{decode_rle, decrypt, encode_rle, encrypt};
use crate::executable::disassembler::DisassembleVisitor;
use crate::Res;

pub mod deserializer;
pub mod disassembler;
pub use deserializer::*;

pub mod commands;
pub use commands::*;

pub mod smt_op_codes;
pub use smt_op_codes::*;

pub mod func_op_codes;
pub use func_op_codes::*;

pub mod variable_value;
pub use variable_value::*;

pub mod variable_table;
pub use variable_table::*;

#[cfg(test)]
pub mod expr_tests;
#[cfg(test)]
pub mod stmt_tests;

#[derive(Clone, Debug, Default)]
pub struct VariableNameGenerator {
    version: u16,
    has_user_vars: bool,

    string_vars: usize,
    int_vars: usize,
    bool_vars: usize,
    money_vars: usize,
    byte_vars: usize,
    time_vars: usize,
    date_vars: usize,
    generic_vars: usize,

    function_vars: usize,
    procedure_vars: usize,
    constants: usize,
}

impl VariableNameGenerator {
    pub fn get_next_name(&mut self, decl: &VariableEntry) -> (String, bool) {
        if self.has_user_vars
            && (self.version < 300 && decl.header.id <= 0x17
                || self.version >= 300 && decl.header.id <= 0x18)
        {
            return (
                VariableNameGenerator::get_user_variable_name(decl.header.id),
                true,
            );
        }

        if decl.get_type() == EntryType::Constant {
            self.constants += 1;
            return (format!("CONST{:>03}", self.constants), false);
        }

        let name = match decl.header.variable_type {
            VariableType::String => {
                self.string_vars += 1;
                format!("STR{:>03}", self.string_vars)
            }
            VariableType::BigStr => {
                self.string_vars += 1;
                format!("BSTR{:>03}", self.string_vars)
            }
            VariableType::Integer => {
                self.int_vars += 1;
                format!("INT{:>03}", self.int_vars)
            }
            VariableType::Boolean => {
                self.bool_vars += 1;
                format!("BOOL{:>03}", self.bool_vars)
            }
            VariableType::Byte => {
                self.byte_vars += 1;
                format!("BYTE{:>03}", self.byte_vars)
            }
            VariableType::Money => {
                self.money_vars += 1;
                format!("MONEY{:>03}", self.money_vars)
            }
            VariableType::Time => {
                self.time_vars += 1;
                format!("TIME{:>03}", self.time_vars)
            }
            VariableType::Date => {
                self.date_vars += 1;
                format!("DATE{:>03}", self.date_vars)
            }
            VariableType::Function => {
                self.function_vars += 1;
                format!("FUNC{:>03}", self.function_vars)
            }
            VariableType::Procedure => {
                self.procedure_vars += 1;
                format!("PROC{:>03}", self.procedure_vars)
            }
            _ => {
                self.generic_vars += 1;
                format!("VAR{:>03}", self.generic_vars)
            }
        };
        (name, false)
    }

    fn get_user_variable_name(number: usize) -> String {
        match number {
            1 => "U_EXPERT".to_string(),
            2 => "U_FSE".to_string(),
            3 => "U_FSEP".to_string(),
            4 => "U_CLS".to_string(),
            5 => "U_EXPDATE".to_string(),
            6 => "U_SEC".to_string(),
            7 => "U_PAGELEN".to_string(),
            8 => "U_EXPSEC".to_string(),
            9 => "U_CITY".to_string(),
            10 => "U_BDPHONE".to_string(),
            11 => "U_HVPHONE".to_string(),
            12 => "U_TRANS".to_string(),
            13 => "U_CMNT1".to_string(),
            14 => "U_CMNT2".to_string(),
            15 => "U_PWD".to_string(),
            16 => "U_SCROLL".to_string(),
            17 => "U_LONGHDR".to_string(),
            18 => "U_DEF79".to_string(),
            19 => "U_ALIAS".to_string(),
            20 => "U_VER".to_string(),
            21 => "U_ADDR".to_string(),
            22 => "U_NOTES".to_string(),
            23 => "U_PWDEXP".to_string(),
            24 => "U_ACCOUNT".to_string(),

            // Added in 3.40
            25 => "U_SHORTDESC".to_string(),
            26 => "U_GENDER".to_string(),
            27 => "U_BIRTHDATE".to_string(),
            28 => "U_EMAIL".to_string(),
            29 => "U_WEB".to_string(),

            _ => {
                log::error!("Unknown user variable number: {number}");
                format!("UNKNOWN_VAR{number}")
            }
        }
    }

    pub fn new(version: u16, has_user_vars: bool) -> Self {
        Self {
            version,
            has_user_vars,
            ..Default::default()
        }
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum ExecutableError {
    #[error("Invalid PPE file")]
    InvalidPPEFile,

    #[error("Unsupported version: {0} (Only up to {LAST_PPLC})")]
    UnsupporrtedVersion(u16),

    #[error("Too many declarations: {0}")]
    TooManyDeclarations(usize),

    #[error("String constant too long: {0}")]
    StringConstantTooLong(usize),

    #[error("String type invalid: {0}")]
    StringTypeInvalid(VariableType),

    #[error("Invalid index in variable table: {0} > max:{1}")]
    InvalidVariableIndexInTable(usize, usize),
}

pub struct Executable {
    pub version: u16,
    pub variable_table: Vec<VariableEntry>,
    pub script_buffer: Vec<i16>,
}

impl Executable {
    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn from_buffer(
        buffer: &mut [u8],
        print_header_information: bool,
    ) -> Result<Executable, ExecutableError> {
        if !buffer.starts_with(PREAMBLE) {
            return Err(ExecutableError::InvalidPPEFile);
        }
        let version = ((buffer[40] & 15) as u16 * 10 + (buffer[41] as u16 & 15)) * 100
            + (buffer[43] as u16 & 15) * 10
            + (buffer[44] as u16 & 15);

        if version > LAST_PPLC {
            return Err(ExecutableError::UnsupporrtedVersion(version));
        }

        let max_var = u16::from_le_bytes(
            (buffer[HEADER_SIZE..=(HEADER_SIZE + 1)])
                .try_into()
                .unwrap(),
        ) as usize;

        let (mut i, variable_table) = read_variable_table(version, buffer, max_var)?;
        let code_size = u16::from_le_bytes(buffer[i..=(i + 1)].try_into().unwrap()) as usize;
        i += 2;
        let real_size = buffer.len() - i;

        if print_header_information {
            execute!(
                stdout(),
                Print("Format ".to_string()),
                SetAttribute(Attribute::Bold),
                Print(format!("{}.{:00}", version / 100, version % 100)),
                SetAttribute(Attribute::Reset),
                Print(" detected ".to_string()),
                SetAttribute(Attribute::Bold),
                Print(format!("{max_var}")),
                SetAttribute(Attribute::Reset),
                Print(" variables, ".to_string()),
                SetAttribute(Attribute::Bold),
                Print(format!("{code_size}/{real_size} bytes")),
                SetAttribute(Attribute::Reset),
                Print(" code/compressed size,".to_string()),
                SetAttribute(Attribute::Bold),
                Print(format!("{} bytes", i - 2 - HEADER_SIZE)),
                SetAttribute(Attribute::Reset),
                Print(" variable table".to_string()),
                Print("\n".to_string()),
            )
            .unwrap();
        }

        let code_data: &mut [u8] = &mut buffer[i..];
        let mut decrypted_data: Vec<u8> = Vec::new();

        let mut offset = 0;
        let data: &[u8] = if version > 300 {
            let use_rle = real_size != code_size;
            let code_data_len = code_data.len();

            while offset < code_data_len {
                let mut chunk_size = 2047;
                let end = (offset + chunk_size).min(code_data_len);
                let mut chunk = &mut code_data[offset..end];
                decrypt(chunk, version);

                if use_rle && offset + chunk_size < code_data_len && chunk[chunk_size - 1] == 0 {
                    chunk = &mut code_data[offset..=end];
                    chunk_size += 1;
                }

                offset += chunk_size;

                if use_rle {
                    let decoded = decode_rle(chunk);
                    decrypted_data.extend_from_slice(&decoded);
                } else {
                    decrypted_data.extend_from_slice(chunk);
                }
            }
            &decrypted_data
        } else {
            code_data
        };
        if data.len() != code_size {
            log::warn!(
                "WARNING: decoded size({}) differs from expected size ({}).",
                data.len(),
                code_size
            );
        }
        let mut script_buffer = Vec::new();
        let mut i = 0;
        while i < data.len() {
            let k = if i + 1 >= data.len() {
                data[i] as i16
            } else {
                i16::from_le_bytes(data[i..i + 2].try_into().unwrap())
            };
            script_buffer.push(k);
            i += 2;
        }
        Ok(Executable {
            version,
            variable_table,
            script_buffer,
        })
    }

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn to_buffer(&self) -> Result<Vec<u8>, ExecutableError> {
        if self.version > LAST_PPLC {
            return Err(ExecutableError::UnsupporrtedVersion(self.version));
        }
        let mut buffer = Vec::new();
        buffer.extend_from_slice(PREAMBLE);
        buffer.push(b'0' + (self.version / 100) as u8);
        buffer.push(b'.');
        let minor = self.version % 100;
        buffer.push(b'0' + (minor / 10) as u8);
        buffer.push(b'0' + (minor % 10) as u8);
        buffer.extend_from_slice(b"\x0D\x0A\x1A");

        if self.variable_table.len() >= 65535 {
            return Err(ExecutableError::TooManyDeclarations(
                self.variable_table.len(),
            ));
        }
        let max_var = u16::to_le_bytes(self.variable_table.len() as u16);
        buffer.extend_from_slice(&max_var);
        for d in self.variable_table.iter().rev() {
            let var_data = d.to_buffer(self.version)?;
            buffer.extend(var_data);
        }

        let mut script_buffer = Vec::new();
        for s in &self.script_buffer {
            script_buffer.extend_from_slice(&s.to_le_bytes());
        }
        let mut code_data = encode_rle(&script_buffer);

        buffer.extend_from_slice(&u16::to_le_bytes(self.script_buffer.len() as u16 * 2));
        // in the very unlikely case the rle compressed buffer is larger than the original buffer
        let use_rle = code_data.len() < script_buffer.len() && self.version >= 300;
        if !use_rle {
            code_data = script_buffer;
        }

        let mut offset = 0;
        while offset < code_data.len() {
            let chunk_size = 2027;
            let add_byte = use_rle
                && offset + chunk_size < code_data.len()
                && code_data[offset + chunk_size] == 0;

            let end = (offset + chunk_size).min(code_data.len());
            let chunk = &mut code_data[offset..end];

            offset += chunk_size;

            encrypt(chunk, self.version);
            buffer.extend_from_slice(chunk);

            if add_byte {
                buffer.push(code_data[end]);
                offset += 1;
            }
        }
        Ok(buffer)
    }

    pub fn create_variable_lookup_table(&self) -> HashMap<unicase::Ascii<String>, usize> {
        let mut variable_lookup = HashMap::new();
        for (i, var_decl) in self.variable_table.iter().enumerate() {
            variable_lookup.insert(unicase::Ascii::new(var_decl.get_name().clone()), i);
        }
        variable_lookup
    }

    pub fn print_variable_table(&self) {
        DisassembleVisitor::print_variable_table(self);
    }
    pub fn print_script_buffer_dump(&self) {
        DisassembleVisitor::print_script_buffer_dump(self);
    }
    pub fn print_disassembler(&self) {
        DisassembleVisitor::new(self).print_disassembler();
    }
}

impl Default for Executable {
    fn default() -> Self {
        Self {
            version: LAST_PPLC,
            variable_table: Vec::new(),
            script_buffer: Vec::new(),
        }
    }
}

static PREAMBLE: &[u8] = "PCBoard Programming Language Executable  ".as_bytes();

pub const LAST_PPLC: u16 = 330;
const HEADER_SIZE: usize = 48;

/// .
///
/// # Examples
///
/// ```
/// use icy_ppe::executable::read_file;
///
/// ```
///
/// # Errors
///
/// Panics if .
pub fn read_file(file_name: &str, print_header_information: bool) -> Res<Executable> {
    let mut f = File::open(file_name)?;

    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;

    Ok(Executable::from_buffer(
        &mut buffer,
        print_header_information,
    )?)
}

fn read_variable_table(
    version: u16,
    buf: &mut [u8],
    max_var: usize,
) -> Result<(usize, Vec<VariableEntry>), ExecutableError> {
    let mut result = vec![VariableEntry::default(); max_var];
    let mut i = HEADER_SIZE + 2;
    if max_var == 0 {
        return Ok((i, result));
    }
    let mut var_count = max_var + 1;
    while var_count > 1 {
        decrypt(&mut (buf[i..(i + 11)]), version);
        let cur_block = &buf[i..(i + 11)];

        var_count = u16::from_le_bytes(cur_block[0..2].try_into().unwrap()) as usize;

        if var_count > max_var {
            return Err(ExecutableError::InvalidVariableIndexInTable(
                max_var, var_count,
            ));
        }

        let header = VarHeader::from_bytes(cur_block);

        i += 11;
        let variable;
        match header.variable_type {
            VariableType::String => {
                let string_length =
                    u16::from_le_bytes((buf[i..=i + 1]).try_into().unwrap()) as usize;
                i += 2;
                decrypt(&mut (buf[i..(i + string_length)]), version);
                let generic_data = if header.dim > 0 {
                    header.create_generic_data()
                } else {
                    let mut str = String::new();
                    for c in &buf[i..(i + string_length - 1)] {
                        str.push(crate::tables::CP437_TO_UNICODE[*c as usize]);
                    }
                    GenericVariableData::String(str)
                };
                variable = VariableValue {
                    vtype: VariableType::String,
                    generic_data,
                    ..Default::default()
                };
                i += string_length;
            }
            VariableType::Function | VariableType::Procedure => {
                decrypt(&mut buf[i..(i + 12)], version);
                let cur_buf = &buf[i..(i + 12)];
                let vtype: VariableType = unsafe { ::std::mem::transmute(cur_buf[2]) };
                assert!(
                    !(vtype != header.variable_type),
                    "Invalid function type: {vtype}"
                );
                let function_value = FunctionValue::from_bytes(cur_buf);
                i += 4; // skip vtable + type
                variable = VariableValue {
                    vtype,
                    data: VariableData { function_value },
                    ..Default::default()
                };
                i += 8;
            }
            _ => {
                if version <= 100 {
                    i += 2; // SKIP VTABLE - seems to get stored by accident.
                    let vtype: VariableType = unsafe { ::std::mem::transmute(buf[i]) };
                    assert!(
                        !(vtype != header.variable_type),
                        "Invalid variable type: {vtype}"
                    );
                    i += 2; // what's stored here ?
                    let mut data = VariableData::default();
                    data.unsigned_value = u32::from_le_bytes((buf[i..i + 4]).try_into().unwrap());
                    variable = VariableValue {
                        vtype,
                        data,
                        ..Default::default()
                    };
                    i += 4;
                } else {
                    decrypt(&mut buf[i..(i + 12)], version);
                    i += 2; // SKIP VTABLE - seems to get stored by accident.
                    let vtype: VariableType = unsafe { ::std::mem::transmute(buf[i]) };
                    assert!(
                        !(vtype != header.variable_type),
                        "Invalid variable type: {vtype}"
                    );
                    i += 2; // what's stored here ?
                    let mut data = VariableData::default();
                    data.u64_value = u64::from_le_bytes((buf[i..i + 8]).try_into().unwrap());

                    variable = VariableValue {
                        vtype,
                        data,
                        generic_data: header.create_generic_data(),
                    };
                    i += 8;
                }
            } // B9 4b
        }

        result[var_count - 1] = VariableEntry::new(header, variable);
    }

    let mut k = result.len() - 1;
    while k > 0 {
        let cur = result[k].clone();
        match cur.header.variable_type {
            VariableType::Function => unsafe {
                let last = cur.value.data.function_value.local_variables as usize
                    + cur.value.data.function_value.return_var as usize
                    - 1;
                for (j, i) in
                    (cur.value.data.function_value.first_var_id as usize..last).enumerate()
                {
                    let fvar = &mut result[i];
                    if i == cur.value.data.function_value.return_var as usize - 1 {
                        fvar.set_type(EntryType::FunctionResult);
                        fvar.number = k;
                    } else if j < cur.value.data.function_value.parameters as usize {
                        fvar.set_type(EntryType::Parameter);
                        println!("{} i:{i}", cur.value.data.function_value.first_var_id + 1);
                        fvar.number = ((cur.value.data.function_value.first_var_id + 1) as usize)
                            .saturating_sub(i);
                    }
                }
            },
            VariableType::Procedure => unsafe {
                let mut j = 0;
                let last = cur.value.data.procedure_value.local_variables as usize
                    + cur.value.data.procedure_value.parameters as usize
                    + cur.value.data.procedure_value.first_var_id as usize;

                (cur.value.data.procedure_value.first_var_id as usize..last).for_each(|i| {
                    let fvar = &mut result[i];
                    if j < cur.value.data.procedure_value.parameters as usize {
                        fvar.set_type(EntryType::Parameter);
                    }
                    j += 1;
                    fvar.number = j;
                });
            },
            _ => {}
        }
        if k == 0 {
            break;
        }
        k -= 1;
    }

    Ok((i, result))
}
