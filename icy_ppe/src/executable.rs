use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use crate::ast::{Variable, VariableData, VariableType, VariableValue};
use crate::crypt::{decode_rle, decrypt};

#[derive(Clone, Debug)]
pub struct VarHeader {
    pub id: usize,
    pub dim: u8,
    pub vector_size: i32,
    pub matrix_size: i32,
    pub cube_size: i32,
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
            vector_size: u16::from_le_bytes(cur_block[3..5].try_into().unwrap()) as i32,
            matrix_size: u16::from_le_bytes(cur_block[5..7].try_into().unwrap()) as i32,
            cube_size: u16::from_le_bytes(cur_block[7..9].try_into().unwrap()) as i32,
            variable_type: unsafe { ::std::mem::transmute(cur_block[9]) },
            flags: cur_block[10],
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct FunctionValue {
    pub parameters: u8,
    pub local_variables: u8,
    pub start_offset: u16,
    pub first_var_id: i16,
    pub return_var: i16,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ProcedureValue {
    pub parameters: u8,
    pub local_variables: u8,
    pub start_offset: u16,
    pub first_var_id: i16,
    pub pass_flags: u16,
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
}

#[derive(Clone)]
pub struct VarDecl {
    pub header: VarHeader,
    pub var_name: String,

    pub content: u64,
    pub content2: u64,

    pub flag: u8,
    pub lflag: u8,
    pub fflag: u8,
    pub number: i32,
    pub variable: Variable,
    pub function_id: i32,
}

pub struct Executable {
    pub version: u16,
    pub variable_declarations: HashMap<i32, Box<VarDecl>>,
    pub source_buffer: Vec<i32>,
    pub max_var: i32,
    pub code_size: i32,
}

static PREAMBLE: &[u8] = "PCBoard Programming Language Executable".as_bytes();

const LAST_PPLC: u16 = 330;
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
/// # Panics
///
/// Panics if .
#[must_use]
pub fn read_file(file_name: &str) -> Executable {
    let mut f = File::open(file_name)
        .unwrap_or_else(|_| panic!("Error: {file_name} not found on disk, aborting..."));

    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)
        .expect("Error while reading file.");

    for i in 0..PREAMBLE.len() {
        assert!(PREAMBLE[i] == buffer[i], "Invalid PPE file");
    }
    let version = ((buffer[40] & 15) as u16 * 10 + (buffer[41] as u16 & 15)) * 100
        + (buffer[43] as u16 & 15) * 10
        + (buffer[44] as u16 & 15);

    assert!(
        version <= LAST_PPLC,
        "Version number {version} not supported. (Only up to {LAST_PPLC})"
    );
    let max_var = u16::from_le_bytes(
        (buffer[HEADER_SIZE..=(HEADER_SIZE + 1)])
            .try_into()
            .unwrap(),
    ) as i32;
    let (mut i, variable_declarations) = read_vars(version, &mut buffer, max_var);
    let code_size = u16::from_le_bytes(buffer[i..=(i + 1)].try_into().unwrap()) as usize;
    i += 2;
    let real_size = buffer.len() - i;
    /*
    let data: Vec<u8> = if version >= 300 {
        let data = &mut buffer[i..real_size + i];
        decrypt(data, version);
        if real_size == code_size {
            data.to_vec()
        } else {
            decode_rle(data)
        }
    } else {
        buffer[i..real_size + i].to_vec()
    };*/

    let code_data: &mut [u8] = &mut buffer[i..];
    let mut decrypted_data = Vec::new();

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
        println!(
            "WARNING: decoded size({}) differs from expected size ({}).",
            data.len(),
            code_size
        );
    }
    let mut source_buffer = Vec::new();
    let mut i = 0;
    while i < data.len() {
        let k = if i + 1 >= data.len() {
            data[i] as i32
        } else {
            i16::from_le_bytes(data[i..i + 2].try_into().unwrap()) as i32
        };
        source_buffer.push(k);
        i += 2;
    }

    Executable {
        version,
        variable_declarations,
        source_buffer,
        max_var,
        code_size: (code_size - 2) as i32, // forget the last END
    }
}

fn read_vars(version: u16, buf: &mut [u8], max_var: i32) -> (usize, HashMap<i32, Box<VarDecl>>) {
    let mut result: HashMap<i32, Box<VarDecl>> = HashMap::new();
    let mut i = HEADER_SIZE + 2;
    if max_var == 0 {
        return (i, result);
    }
    let mut var_count = max_var + 1;
    println!("Read {max_var} entries from variable table");
    while var_count > 1 {
        decrypt(&mut (buf[i..(i + 11)]), version);
        let cur_block = &buf[i..(i + 11)];

        var_count = u16::from_le_bytes(cur_block[0..2].try_into().unwrap()) as i32;

        let mut var_decl = VarDecl {
            header: VarHeader::from_bytes(cur_block),
            number: 0,
            function_id: 0,
            var_name: String::new(),
            content: 0,
            content2: 0,
            flag: 0,
            lflag: 0,
            fflag: 0,
            variable: Variable::default(),
        };
        i += 11;
        // println!("var_decl: {:?}", var_decl.header);
        match var_decl.header.variable_type {
            VariableType::String => {
                let string_length =
                    u16::from_le_bytes((buf[i..=i + 1]).try_into().unwrap()) as usize;
                i += 2;
                decrypt(&mut (buf[i..(i + string_length)]), version);
                let mut str = String::new();
                for c in &buf[i..(i + string_length - 1)] {
                    str.push(crate::tables::CP437_TO_UNICODE[*c as usize]);
                }
                var_decl.variable = Variable {
                    vtype: VariableType::String,
                    generic_data: VariableValue::String(str),
                    ..Default::default()
                };
                i += string_length;
            }
            VariableType::Function => unsafe {
                decrypt(&mut buf[i..(i + 12)], version);
                let cur_buf = &buf[i..(i + 12)];
                let function_value = FunctionValue::from_bytes(cur_buf);
                i += 4; // skip vtable + type
                var_decl.variable = Variable {
                    vtype: VariableType::Function,
                    data: VariableData { function_value },
                    ..Default::default()
                };
                var_decl.variable.data.function_value.local_variables -= 1;
                i += 8;
            },
            VariableType::Procedure => {
                decrypt(&mut buf[i..(i + 12)], version);
                let cur_buf = &buf[i..(i + 12)];
                let function_value = FunctionValue::from_bytes(cur_buf);
                i += 4; // skip vtable + type
                var_decl.variable = Variable {
                    vtype: VariableType::Procedure,
                    data: VariableData { function_value },
                    ..Default::default()
                };
                i += 8;
            }
            _ => {
                if version <= 100 {
                    i += 2; // SKIP VTABLE - seems to get stored by accident.
                    let vtype: VariableType = unsafe { ::std::mem::transmute(buf[i]) };
                    i += 2; // what's stored here ?
                    var_decl.variable = Variable {
                        vtype,
                        data: VariableData {
                            unsigned_value: u32::from_le_bytes((buf[i..i + 4]).try_into().unwrap()),
                        },
                        ..Default::default()
                    };
                    i += 4;
                } else if version < 300 {
                    i += 2; // SKIP VTABLE - seems to get stored by accident.
                    let vtype: VariableType = unsafe { ::std::mem::transmute(buf[i]) };
                    i += 2; // what's stored here ?
                    var_decl.variable = Variable {
                        vtype,
                        data: VariableData {
                            u64_value: u64::from_le_bytes((buf[i..i + 8]).try_into().unwrap()),
                        },
                        ..Default::default()
                    };
                    i += 8;
                } else {
                    decrypt(&mut buf[i..(i + 12)], version);
                    i += 2; // SKIP VTABLE - seems to get stored by accident.
                    let vtype: VariableType = unsafe { ::std::mem::transmute(buf[i]) };
                    i += 2; // what's stored here ?
                    var_decl.variable = Variable {
                        vtype,
                        data: VariableData {
                            u64_value: u64::from_le_bytes((buf[i..i + 8]).try_into().unwrap()),
                        },
                        ..Default::default()
                    };
                    i += 8;
                }
            } // B9 4b
        }
        result.insert(var_count - 1, Box::new(var_decl.clone()));
    }

    let mut k = (result.len() - 1) as i32;
    while k >= 0 {
        let cur = result.get(&k).unwrap().clone();
        match cur.header.variable_type {
            VariableType::Function => unsafe {
                let mut j = 0;
                let last = cur.variable.data.function_value.local_variables as i32
                    + cur.variable.data.function_value.return_var as i32;
                for i in cur.variable.data.function_value.first_var_id as i32..last {
                    let fvar = result.get_mut(&i).unwrap();
                    fvar.lflag = 1;
                    if j < cur.variable.data.function_value.parameters as i32 {
                        fvar.flag = 1;
                    }
                    if i != cur.variable.data.function_value.return_var as i32 - 1 {
                        j += 1;
                        fvar.number = j;
                    }
                }

                let next = result
                    .get_mut(&(cur.variable.data.function_value.return_var as i32 - 1))
                    .unwrap();
                next.fflag = 1;
            },
            VariableType::Procedure => unsafe {
                let mut j = 0;
                let last = cur.variable.data.procedure_value.local_variables as i32
                    + cur.variable.data.procedure_value.parameters as i32
                    + cur.variable.data.procedure_value.first_var_id as i32;

                for i in cur.variable.data.procedure_value.first_var_id as i32..last {
                    if let Some(fvar) = result.get_mut(&i) {
                        fvar.lflag = 1;
                        if j < cur.variable.data.procedure_value.parameters as i32 {
                            fvar.flag = 1;
                        }
                        j += 1;
                        fvar.number = j;
                    } else {
                        panic!(
                            "Variable {i} not found. Invalid function header {:?}",
                            cur.variable.data.procedure_value
                        );
                    }
                }
            },
            _ => {}
        }

        k -= 1;
    }
    (i, result)
}
