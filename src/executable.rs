use std::fs::*;
use std::io::*;
use std::collections::HashMap;

use crate::decode::decode;

#[repr(u8)]
#[derive(Clone, Copy, PartialEq)]
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
    Method = 16,
    DDate = 17
}

#[derive(Clone)]
pub struct VarDecl
{
    pub variable_type: VariableType,
    pub dim: u8,
    pub dims: [u16;3],
    pub content: u64,
    pub content2: u64,
    pub string_value: String,
    pub flag: u8,
    pub lflag: u8,
    pub fflag: u8,
    pub number: u32,
    pub args: u8,
    pub total_var: u16,
    pub start: u16,
    pub first_var: u16,
    pub return_var: u16,
    pub func: u16
}

pub struct Executable
{
    pub version : u16,
    pub variable_declarations : HashMap<u16, Box<VarDecl>>,
    pub source_buffer : Vec<i16>,
    pub max_var: u32,
    pub code_size : u32
}

static PREAMBLE: &[u8] = "PCBoard Programming Language Executable".as_bytes();

const LAST_PPLC : u16 = 330;
const HEADER_SIZE: usize = 48;

pub fn read_file(file_name : &str) -> Executable
{
    let mut f = File::open(file_name).expect("Error while opening file.");

    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer).expect("Error while reading file.");

    for i in 0..PREAMBLE.len() {
        if PREAMBLE[i] != buffer[i]
        {
            panic!("Invalid PPE file");
        }
    }
    let version = ((buffer[40] & 15) as u16 * 10 + (buffer[41] as u16 & 15)) * 100 +
        (buffer[43] as u16 & 15) * 10 + (buffer[44]  as u16 & 15);

    if version > LAST_PPLC
    {
        panic!("Invalid PPE file");
    }
    println!("{}.{} detected", version / 100, version % 100);
    let max_var = u16::from_le_bytes((&buffer[HEADER_SIZE..=(HEADER_SIZE + 1)]).try_into().unwrap());
    let (mut i, variable_declarations) = read_vars(version, &mut buffer, max_var);
    let code_size = u16::from_le_bytes((&buffer[i..=(i + 1)]).try_into().unwrap()) as usize;
    i += 2;
    let real_size = buffer.len() - i;

    let data: Vec<u8> = if version >= 300 {
        let data =&mut buffer[i..real_size + i];
        decode(data);
        if real_size != code_size {
            decode_other(data, real_size, code_size)
        } else {
            data.to_vec()
        }
    } else { buffer[i..real_size + i].to_vec() };

    let mut source_buffer  = Vec::new();
    let mut i = 0;
    while i < data.len() {
        source_buffer.push(i16::from_le_bytes((&data[i..=(i + 1)]).try_into().unwrap()));
        i += 2;
    }
    Executable {
        version,
        variable_declarations,
        source_buffer,
        max_var: max_var as u32,
        code_size : (code_size - 2) as u32 // forget the last END
    }
}

fn read_vars(version: u16, buf : &mut [u8], max_var: u16) -> (usize, HashMap<u16, Box<VarDecl>>) {
    let mut result : HashMap<u16, Box<VarDecl>> = HashMap::new();
    let mut i  = HEADER_SIZE + 2;
    if max_var == 0 {
        return (i, result);
    }
    let mut var_count = max_var + 1;

    while var_count > 1 {
        if version >= 300 {
            decode(&mut (buf[i..(i + 11)]));
        }
        let cur_block =&buf[i..(i + 11)];

        var_count = u16::from_le_bytes(cur_block[0..=1].try_into().unwrap());
        let mut var_decl = VarDecl {
            number :  0,
            args: 0,
            total_var: 0,
            start: 0,
            first_var: 0,
            func : 0,
            variable_type : unsafe{ ::std::mem::transmute(cur_block[9]) },
            dim : cur_block[2],
            dims : [
                u16::from_le_bytes(cur_block[3..=4].try_into().unwrap()),
                u16::from_le_bytes(cur_block[5..=6].try_into().unwrap()),
                u16::from_le_bytes(cur_block[7..=8].try_into().unwrap()),
            ],
            content : 0,
            content2 : 0,
            string_value : String::new(),
            flag : 0,
            lflag : 0,
            fflag : 0,
            return_var: 0
        };
        // println!("read var {} type {}", var_count, var_decl.variable_type as u8);
        i += 11;

        match var_decl.variable_type {
            VariableType::String => {
                let string_length = u16::from_le_bytes((buf[i..=i + 1]).try_into().unwrap())  as usize;
                i += 2;
                if version >= 300 {
                    decode(&mut (buf[i..i + string_length]));
                }
                var_decl.string_value = String::from_utf8_lossy(&buf[i..=i + string_length]).to_string();
                i += string_length;
            },
            VariableType::Function => {
                if version >= 300 {
                    decode(&mut buf[i..=(i + 12)]);
                }
                let cur_buf =&buf[i..=(i + 12)];
                var_decl.args = cur_buf[4];
                var_decl.total_var = (cur_buf[5] - 1) as u16;
                var_decl.start = u16::from_le_bytes((cur_buf[6..=7]).try_into().unwrap());
                var_decl.first_var = u16::from_le_bytes((cur_buf[8..=9]).try_into().unwrap());
                var_decl.return_var = u16::from_le_bytes((cur_buf[10..=11]).try_into().unwrap());
                i += 12;
            },
            VariableType::Method => {
                if version >= 300 {
                    decode(&mut buf[i..=(i + 12)]);
                }
                let cur_buf =&buf[i..=(i + 12)];
                var_decl.args = cur_buf[4];
                var_decl.total_var = cur_buf[5] as u16;
                var_decl.start = u16::from_le_bytes((cur_buf[6..=7]).try_into().unwrap());
                var_decl.first_var = u16::from_le_bytes((cur_buf[8..=9]).try_into().unwrap());
                var_decl.return_var = u16::from_le_bytes((cur_buf[10..=11]).try_into().unwrap());
                i += 12;
            },
            _ => {
                if version <=100 {
                    i += 4; // what's stored here ?
                    var_decl.content = u32::from_le_bytes((buf[i..i + 4]).try_into().unwrap()) as u64;
                    i += 4;
                } else if version < 300 {
                    i += 4; // what's stored here ?
                    var_decl.content = u64::from_le_bytes((buf[i..i + 8]).try_into().unwrap());
                    i += 8;
                } else {
                    decode(&mut buf[i..(i + 12)]);
                    i += 4; // what's stored here ?
                    var_decl.content = u32::from_le_bytes((buf[i..i + 4]).try_into().unwrap()) as u64;
                    i += 4;
                    var_decl.content2 = u32::from_le_bytes((buf[i..i + 4]).try_into().unwrap()) as u64;
                    i += 4;
                }
            }
        }
        result.insert(var_count - 1, Box::new(var_decl.clone()));
    }

    let mut k = (result.len() - 1) as i32;
    while k >= 0 {
        let cur = (**result.get(&(k as u16)).unwrap()).clone();

        match cur.variable_type {
            VariableType::Function => {
                let next = &mut **result.get_mut(&(cur.return_var - 1)).unwrap();
                next.fflag = 1;

                let mut j = 0;
                let last = cur.total_var + cur.return_var;
                for i in cur.first_var..last {
                    let fvar = &mut **result.get_mut(&i).unwrap();
                    fvar.lflag = 1;
                    if j < cur.args  {
                        fvar.flag = 1;
                    }
                    if i != cur.return_var - 1 {
                        j += 1;
                        fvar.number = j as u32;
                    }
                }
            },
            VariableType::Method => {
                let mut j = 0;
                let last = cur.total_var + cur.args as u16 + cur.first_var;

                for i in cur.first_var..last {
                    let fvar = &mut **result.get_mut(&i).unwrap();
                    fvar.lflag = 1;
                    if j < cur.args  {
                        fvar.flag = 1;
                    }
                    j += 1;
                    fvar.number = j as u32;
                }
            },
            _ => {}
        }

        k -= 1;
    }
    (i, result)
}

pub fn decode_other(block1 : &mut [u8], size : usize, size2 : usize) -> Vec<u8> {
    let mut block2 : Vec<u8> = Vec::new();
    let mut i =0;

    while i < size && block2.len() < size2 {
        block2.push(block1[i]);
        i += 1;
        block2.push(block1[i]);
        i += 1;
        if block2[block2.len() - 1] == 0 {
            while block1[i] > 1 {
                block2.push(0);
                block1[i] -= 1;
            }
            i +=1;
        } else if block1[i] == 0 {
            block2.push(0);
            i += 1;
            while block1[i] > 1 {
                block2.push(0);
                block1[i] -= 1;
            }
            i += 1;
        }
    }

    block2
}