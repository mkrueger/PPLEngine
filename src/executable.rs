use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use crate::ast::VariableType;
use crate::crypt::{decode_rle, decrypt};

#[derive(Clone)]
pub struct VarDecl {
    pub dim: u8,
    pub vector_size: i32,
    pub matrix_size: i32,
    pub cube_size: i32,

    pub variable_type: VariableType,
    pub var_name: String,

    pub content: u64,
    pub content2: u64,
    pub string_value: String,

    pub flag: u8,
    pub lflag: u8,
    pub fflag: u8,
    pub number: i32,
    pub args: i32,
    pub total_var: i32,
    pub start: i32,
    pub first_var: i32,
    pub return_var: i32,
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
/// use ppl_engine::executable::read_file;
///
/// ```
///
/// # Panics
///
/// Panics if .
#[must_use]
pub fn read_file(file_name: &str) -> Executable {
    let mut f = File::open(file_name)
        .unwrap_or_else(|_| panic!("Error: {} not found on disk, aborting...", file_name));

    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)
        .expect("Error while reading file.");

    for i in 0..PREAMBLE.len() {
        assert!(PREAMBLE[i] == buffer[i], "Invalid PPE file");
    }
    let version = ((buffer[40] & 15) as u16 * 10 + (buffer[41] as u16 & 15)) * 100
        + (buffer[43] as u16 & 15) * 10
        + (buffer[44] as u16 & 15);

    assert!(version <= LAST_PPLC, "Invalid PPE file");
    let max_var = u16::from_le_bytes(
        (buffer[HEADER_SIZE..=(HEADER_SIZE + 1)])
            .try_into()
            .unwrap(),
    ) as i32;
    let (mut i, variable_declarations) = read_vars(version, &mut buffer, max_var);
    let code_size = u16::from_le_bytes(buffer[i..=(i + 1)].try_into().unwrap()) as usize;
    i += 2;
    let real_size = buffer.len() - i;

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
    };

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
        max_var: max_var as i32,
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

    while var_count > 1 {
        decrypt(&mut (buf[i..(i + 11)]), version);
        let cur_block = &buf[i..(i + 11)];

        var_count = u16::from_le_bytes(cur_block[0..2].try_into().unwrap()) as i32;

        let variable_type = unsafe { ::std::mem::transmute(cur_block[9]) };
        let mut var_decl = VarDecl {
            number: 0,
            args: 0,
            total_var: 0,
            start: 0,
            first_var: 0,
            function_id: 0,
            variable_type,
            var_name: "".to_string(),
            dim: cur_block[2],
            vector_size: u16::from_le_bytes(cur_block[3..5].try_into().unwrap()) as i32,
            matrix_size: u16::from_le_bytes(cur_block[5..7].try_into().unwrap()) as i32,
            cube_size: u16::from_le_bytes(cur_block[7..9].try_into().unwrap()) as i32,
            content: 0,
            content2: 0,
            string_value: String::new(),
            flag: 0,
            lflag: 0,
            fflag: 0,
            return_var: 0,
        };
        // println!("read var {} type {}", var_count, var_decl.variable_type as u8);
        i += 11;

        match var_decl.variable_type {
            VariableType::String => {
                let string_length =
                    u16::from_le_bytes((buf[i..=i + 1]).try_into().unwrap()) as usize;
                i += 2;
                decrypt(&mut (buf[i..(i + string_length)]), version);
                var_decl.string_value =
                    String::from_utf8_lossy(&buf[i..(i + string_length - 1)]).to_string(); // C strings always end with \0
                i += string_length;
            }
            VariableType::Function => {
                decrypt(&mut buf[i..(i + 12)], version);
                let cur_buf = &buf[i..(i + 12)];
                var_decl.args = cur_buf[4] as i32;
                var_decl.total_var = cur_buf[5] as i32 - 1;
                var_decl.start = u16::from_le_bytes((cur_buf[6..=7]).try_into().unwrap()) as i32;
                var_decl.first_var =
                    u16::from_le_bytes((cur_buf[8..=9]).try_into().unwrap()) as i32;
                var_decl.return_var =
                    u16::from_le_bytes((cur_buf[10..=11]).try_into().unwrap()) as i32;
                i += 12;
            }
            VariableType::Procedure => {
                decrypt(&mut buf[i..(i + 12)], version);
                let cur_buf = &buf[i..(i + 12)];
                var_decl.args = cur_buf[4] as i32;
                var_decl.total_var = cur_buf[5] as i32;
                var_decl.start = u16::from_le_bytes((cur_buf[6..=7]).try_into().unwrap()) as i32;
                var_decl.first_var =
                    u16::from_le_bytes((cur_buf[8..=9]).try_into().unwrap()) as i32;
                var_decl.return_var =
                    u16::from_le_bytes((cur_buf[10..=11]).try_into().unwrap()) as i32;
                i += 12;
            }
            _ => {
                if version <= 100 {
                    i += 4; // what's stored here ?
                    var_decl.content =
                        u32::from_le_bytes((buf[i..i + 4]).try_into().unwrap()) as u64;
                    i += 4;
                } else if version < 300 {
                    i += 4; // what's stored here ?
                    var_decl.content = u64::from_le_bytes((buf[i..i + 8]).try_into().unwrap());
                    i += 8;
                } else {
                    decrypt(&mut buf[i..(i + 12)], version);
                    i += 4; // what's stored here ?
                    var_decl.content =
                        u32::from_le_bytes((buf[i..i + 4]).try_into().unwrap()) as u64;
                    i += 4;
                    var_decl.content2 =
                        u32::from_le_bytes((buf[i..i + 4]).try_into().unwrap()) as u64;
                    i += 4;
                }
            }
        }
        result.insert(var_count - 1, Box::new(var_decl.clone()));
    }

    let mut k = (result.len() - 1) as i32;
    while k >= 0 {
        let cur = result.get(&k).unwrap().clone();

        match cur.variable_type {
            VariableType::Function => {
                let mut j = 0;
                let last = cur.total_var + cur.return_var;
                for i in cur.first_var..last {
                    let fvar = result.get_mut(&i).unwrap();
                    fvar.lflag = 1;
                    if j < cur.args {
                        fvar.flag = 1;
                    }
                    if i != cur.return_var - 1 {
                        j += 1;
                        fvar.number = j;
                    }
                }

                let next = result.get_mut(&(cur.return_var - 1)).unwrap();
                next.fflag = 1;
            }
            VariableType::Procedure => {
                let mut j = 0;
                let last = cur.total_var + cur.args + cur.first_var;

                for i in cur.first_var..last {
                    let fvar = &mut **result.get_mut(&i).unwrap();
                    fvar.lflag = 1;
                    if j < cur.args {
                        fvar.flag = 1;
                    }
                    j += 1;
                    fvar.number = j;
                }
            }
            _ => {}
        }

        k -= 1;
    }
    (i, result)
}
