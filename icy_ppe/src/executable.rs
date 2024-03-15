use std::collections::HashMap;
use std::fmt;
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
}

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
                VariableNameGenerator::get_user_variable_name(decl.header.id - 1),
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
            0 => "U_EXPERT".to_string(),
            1 => "U_FSE".to_string(),
            2 => "U_FSEP".to_string(),
            3 => "U_CLS".to_string(),
            4 => "U_EXPDATE".to_string(),
            5 => "U_SEC".to_string(),
            6 => "U_PAGELEN".to_string(),
            7 => "U_EXPSEC".to_string(),
            8 => "U_CITY".to_string(),
            9 => "U_BDPHONE".to_string(),
            10 => "U_HVPHONE".to_string(),
            11 => "U_TRANS".to_string(),
            12 => "U_CMNT1".to_string(),
            13 => "U_CMNT2".to_string(),
            14 => "U_PWD".to_string(),
            15 => "U_SCROLL".to_string(),
            16 => "U_LONGHDR".to_string(),
            17 => "U_DEF79".to_string(),
            18 => "U_ALIAS".to_string(),
            19 => "U_VER".to_string(),
            20 => "U_ADDR".to_string(),
            21 => "U_NOTES".to_string(),
            22 => "U_PWDEXP".to_string(),
            23 => "U_ACCOUNT".to_string(),

            // Added in 3.40
            24 => "U_SHORTDESC".to_string(),
            25 => "U_GENDER".to_string(),
            26 => "U_BIRTHDATE".to_string(),
            27 => "U_EMAIL".to_string(),
            28 => "U_WEB".to_string(),

            _ => {
                log::error!("Unknown user variable number: {number}");
                "????".to_string()
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum EntryType {
    #[default]
    Constant,
    UserVariable,
    Variable,
    FunctionResult,
    Parameter,
}
impl EntryType {
    pub fn use_name(self) -> bool {
        self != EntryType::Constant
    }
}

#[derive(Clone, Debug)]
pub struct VariableEntry {
    pub header: VarHeader,
    name: String,
    entry_type: EntryType,
    pub number: i32,
    pub variable: Variable,
    pub function_id: i32,
}

impl VariableEntry {
    fn new(header: VarHeader, variable: Variable) -> Self {
        Self {
            header,
            name: "Unnamed".to_string(),
            number: 0,
            variable,
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
}

pub struct Executable {
    pub version: u16,
    pub variable_declarations: HashMap<i32, Box<VariableEntry>>,
    pub variable_lookup: HashMap<unicase::Ascii<String>, usize>,
    pub source_buffer: Vec<i32>,
    pub max_var: i32,
    pub code_size: i32,
}
impl Executable {
    pub fn from_buffer(buffer: &mut [u8]) -> Executable {
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
        let (mut i, variable_declarations) = read_vars(version, buffer, max_var);
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
        println!("Code size: {code_size} bytes real size {real_size}.");
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

        let mut variable_lookup = HashMap::new();
        for (i, var_decl) in &variable_declarations {
            variable_lookup.insert(
                unicase::Ascii::new(var_decl.get_name().clone()),
                *i as usize,
            );
        }
        Executable {
            version,
            variable_declarations,
            variable_lookup,
            source_buffer,
            max_var,
            code_size: code_size as i32 - 2, // drop last END
        }
    }
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

    Executable::from_buffer(&mut buffer)
}

fn read_vars(
    version: u16,
    buf: &mut [u8],
    max_var: i32,
) -> (usize, HashMap<i32, Box<VariableEntry>>) {
    let mut result = HashMap::new();
    let mut i = HEADER_SIZE + 2;
    if max_var == 0 {
        return (i, result);
    }
    let mut var_count = max_var + 1;
    while var_count > 1 {
        decrypt(&mut (buf[i..(i + 11)]), version);
        let cur_block = &buf[i..(i + 11)];

        var_count = u16::from_le_bytes(cur_block[0..2].try_into().unwrap()) as i32;

        let header = VarHeader::from_bytes(cur_block);

        i += 11;
        let variable;
        match header.variable_type {
            VariableType::String => {
                let string_length =
                    u16::from_le_bytes((buf[i..=i + 1]).try_into().unwrap()) as usize;
                i += 2;
                decrypt(&mut (buf[i..(i + string_length)]), version);
                let mut str = String::new();
                for c in &buf[i..(i + string_length - 1)] {
                    str.push(crate::tables::CP437_TO_UNICODE[*c as usize]);
                }
                variable = Variable {
                    vtype: VariableType::String,
                    generic_data: VariableValue::String(str),
                    ..Default::default()
                };
                i += string_length;
            }
            VariableType::Function => {
                decrypt(&mut buf[i..(i + 12)], version);
                let cur_buf = &buf[i..(i + 12)];
                let vtype: VariableType = unsafe { ::std::mem::transmute(cur_buf[2]) };
                assert!(
                    !(vtype != VariableType::Function),
                    "Invalid function type: {vtype}"
                );
                let mut function_value = FunctionValue::from_bytes(cur_buf);
                function_value.local_variables -= 1;
                i += 4; // skip vtable + type
                variable = Variable {
                    vtype,
                    data: VariableData { function_value },
                    ..Default::default()
                };
                i += 8;
            }
            VariableType::Procedure => {
                decrypt(&mut buf[i..(i + 12)], version);
                let cur_buf = &buf[i..(i + 12)];
                let vtype: VariableType = unsafe { ::std::mem::transmute(cur_buf[2]) };
                assert!(
                    !(vtype != VariableType::Procedure),
                    "Invalid function type: {vtype}"
                );
                let function_value = FunctionValue::from_bytes(cur_buf);
                i += 4; // skip vtable + type
                variable = Variable {
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
                    variable = Variable {
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
                    assert!(
                        !(vtype != header.variable_type),
                        "Invalid variable type: {vtype}"
                    );
                    i += 2; // what's stored here ?
                    variable = Variable {
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
                    assert!(
                        !(vtype != header.variable_type),
                        "Invalid variable type: {vtype}"
                    );
                    i += 2; // what's stored here ?
                    variable = Variable {
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
        result.insert(
            var_count - 1,
            Box::new(VariableEntry::new(header, variable)),
        );
    }

    let mut k = (result.len() - 1) as i32;
    while k >= 0 {
        let cur = result.get(&k).unwrap().clone();
        match cur.header.variable_type {
            VariableType::Function => unsafe {
                let last = cur.variable.data.function_value.local_variables as i32
                    + cur.variable.data.function_value.return_var as i32;
                let mut j = 0;
                for i in cur.variable.data.function_value.first_var_id as i32..last {
                    let fvar = result.get_mut(&i).unwrap();
                    if i == cur.variable.data.function_value.return_var as i32 - 1 {
                        fvar.set_type(EntryType::FunctionResult);
                        fvar.number = k;
                    } else if j < cur.variable.data.function_value.parameters as i32 {
                        fvar.set_type(EntryType::Parameter);
                        fvar.number =
                            (cur.variable.data.function_value.first_var_id + 1) as i32 - i;
                    }
                    j += 1;
                }
            },
            VariableType::Procedure => unsafe {
                let mut j = 0;
                let last = cur.variable.data.procedure_value.local_variables as i32
                    + cur.variable.data.procedure_value.parameters as i32
                    + cur.variable.data.procedure_value.first_var_id as i32;

                for i in cur.variable.data.procedure_value.first_var_id as i32..last {
                    if let Some(fvar) = result.get_mut(&i) {
                        if j < cur.variable.data.procedure_value.parameters as i32 {
                            fvar.set_type(EntryType::Parameter);
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
