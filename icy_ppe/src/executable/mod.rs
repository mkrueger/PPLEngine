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
    pub fn get_next_name(&mut self, decl: &TableEntry) -> (String, bool) {
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
        if let Some(name) = USER_VARIABLES.get(number -1) {
            return name.name.to_string();
        }
        log::error!("Unknown user variable number: {number}");
        format!("UNKNOWN_VAR{number}")
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
    pub variable_table: VariableTable,
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

        let (mut i, variable_table) = VariableTable::deserialize(version, buffer)?;

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
                Print(format!("{}", variable_table.len())),
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

        self.variable_table.serialize(&mut buffer)?;

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

    pub fn print_variable_table(&self) {
        self.variable_table.print_variable_table();
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
            variable_table: VariableTable::default(),
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
