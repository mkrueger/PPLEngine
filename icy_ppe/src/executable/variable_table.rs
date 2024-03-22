use std::{fmt, io::stdout};

use crossterm::{
    execute,
    style::{Attribute, Color, Print, SetAttribute, SetForegroundColor},
};

use crate::crypt::{decrypt, encrypt};

use super::{
    ExecutableError, GenericVariableData, PPEExpr, PPEScript, VariableData, VariableNameGenerator,
    VariableType, VariableValue, HEADER_SIZE,
};

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
        let mut dim = cur_block[2];
        if dim > 3 {
            log::warn!("Invalid dimension: {}, setting to 3", dim);
            dim = 3;
        }
        Self {
            id: u16::from_le_bytes(cur_block[0..2].try_into().unwrap()) as usize,
            dim,
            vector_size: u16::from_le_bytes(cur_block[3..5].try_into().unwrap()) as usize,
            matrix_size: u16::from_le_bytes(cur_block[5..7].try_into().unwrap()) as usize,
            cube_size: u16::from_le_bytes(cur_block[7..9].try_into().unwrap()) as usize,
            variable_type: VariableType::from_byte(cur_block[9]),
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
            1..=3 => GenericVariableData::create_array(
                self.dim,
                self.vector_size,
                self.matrix_size,
                self.cube_size,
            ),
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
pub struct TableEntry {
    pub header: VarHeader,
    pub name: String,
    pub entry_type: EntryType,
    pub number: usize,
    pub value: VariableValue,
    pub function_id: usize,
}

impl TableEntry {
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
        let mut buffer = self.header.to_bytes();
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

#[derive(Default, Clone)]
pub struct VariableTable {
    version: u16,
    entries: Vec<TableEntry>,
}

impl VariableTable {
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn deserialize(version: u16, buf: &mut [u8]) -> Result<(usize, Self), ExecutableError> {
        let mut i = HEADER_SIZE;
        let max_var = u16::from_le_bytes((buf[i..=(i + 1)]).try_into().unwrap()) as usize;
        i += 2;

        let mut result = vec![TableEntry::default(); max_var];
        if max_var == 0 {
            return Ok((
                i,
                VariableTable {
                    version,
                    entries: result,
                },
            ));
        }
        let mut var_count = max_var as i32 - 1;

        while var_count >= 0 {
            decrypt(&mut (buf[i..(i + 11)]), version);
            let cur_block = &buf[i..(i + 11)];

            let header_id = u16::from_le_bytes(cur_block[0..2].try_into().unwrap()) as usize;
            if header_id > max_var {
                log::warn!(
                    "Variable count exceeds maximum: {} ({})",
                    var_count,
                    max_var
                );
            }
            if header_id != var_count as usize + 1 {
                log::warn!(
                    "Variable id mismatch: {} != {}",
                    header_id,
                    var_count as usize + 1
                );
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
                    let vtype = VariableType::from_byte(cur_buf[2]);
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
                        let vtype = VariableType::from_byte(buf[i]);
                        assert!(
                            !(vtype != header.variable_type),
                            "Invalid variable type: {vtype}"
                        );
                        i += 2; // what's stored here ?
                        let mut data = VariableData::default();
                        data.unsigned_value =
                            u64::from_le_bytes((buf[i..i + 8]).try_into().unwrap());
                        variable = VariableValue {
                            vtype,
                            data,
                            ..Default::default()
                        };
                        i += 4;
                    } else {
                        decrypt(&mut buf[i..(i + 12)], version);
                        i += 2; // SKIP VTABLE - seems to get stored by accident.
                        let vtype = VariableType::from_byte(buf[i]);
                        if vtype != header.variable_type {
                            log::error!("Encountered anomaly in variable table: {} variable type and variable value {} are not matching.", header.variable_type, vtype);
                            log::error!("File is potentially damaged.");
                        }
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

            result[var_count as usize] = TableEntry::new(header, variable);
            var_count -= 1;
        }

        for k in (0..result.len()).rev() {
            let cur = result[k].clone();
            match cur.header.variable_type {
                VariableType::Function => unsafe {
                    let last = (cur.value.data.function_value.local_variables as usize
                        + cur.value.data.function_value.return_var as usize)
                        .saturating_sub(1);
                    for (j, i) in
                        (cur.value.data.function_value.first_var_id as usize..last).enumerate()
                    {
                        let fvar = &mut result[i];
                        if i == (cur.value.data.function_value.return_var as usize)
                            .saturating_sub(1)
                        {
                            fvar.set_type(EntryType::FunctionResult);
                            fvar.number = k;
                        } else if j < cur.value.data.function_value.parameters as usize {
                            fvar.set_type(EntryType::Parameter);
                            fvar.number = ((cur.value.data.function_value.first_var_id + 1)
                                as usize)
                                .saturating_sub(i);
                        }
                    }
                    result[k].set_type(EntryType::Function);
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
                    result[k].set_type(EntryType::Procedure);
                },
                _ => {}
            }
        }

        let mut table = VariableTable {
            version,
            entries: result,
        };
        table.generate_names();
        Ok((i, table))
    }

    pub fn generate_names(&mut self) {
        let user_vars_version = self.scan_user_variables_version();
        let mut name_generator = VariableNameGenerator::new(self.version, user_vars_version);
        let mut function_result = 1;
        for res in &mut self.entries {
            if res.get_type() == EntryType::FunctionResult {
                res.set_name(format!("RESULT{function_result:>03}"));
                function_result += 1;
                continue;
            }

            let (name, is_user_variable) = name_generator.get_next_name(res);
            if is_user_variable {
                res.set_type(EntryType::UserVariable);
            }

            res.set_name(name);
        }
    }

    pub fn analyze_usage(&mut self, script: &PPEScript) {
        for stmt in &script.statements {
            self.analyze_statement(&stmt.command);
        }
    }
    fn analyze_statement(&mut self, stmt: &super::PPECommand) {
        match stmt {
            super::PPECommand::ProcedureCall(id, args) => unsafe {
                let flags = self.get_value(*id).data.procedure_value.pass_flags;
                for (i, arg) in args.iter().enumerate() {
                    if flags & (1 << i) != 0 {
                        self.report_usage(arg);
                    }
                }
            },
            super::PPECommand::PredefinedCall(id, args) => match id.sig {
                super::StatementSignature::Invalid => {}
                super::StatementSignature::ArgumentsWithVariable(var_arg, _)
                | super::StatementSignature::VariableArguments(var_arg) => {
                    if var_arg > 0 {
                        self.report_usage(&args[var_arg - 1]);
                    }
                }
                super::StatementSignature::SpecialCaseDcreate => {
                    self.report_usage(&args[3]);
                }
                super::StatementSignature::SpecialCaseDlockg
                | super::StatementSignature::SpecialCaseSort => {
                    self.report_usage(&args[1]);
                }
                super::StatementSignature::SpecialCaseVarSeg => {
                    self.report_usage(&args[0]);
                    self.report_usage(&args[1]);
                }
                super::StatementSignature::SpecialCasePop => {
                    for arg in args {
                        self.report_usage(arg);
                    }
                }
            },
            super::PPECommand::Let(variable, _) => {
                self.report_usage(variable);
            }
            _ => {}
        }
    }

    fn report_usage(&mut self, variable: &PPEExpr) {
        if let Some(id) = variable.get_id() {
            if id < self.entries.len() + 1 && id > 0 {
                self.get_var_entry_mut(id).report_variable_usage();
            }
        }
    }

    pub fn push(&mut self, entry: TableEntry) {
        self.entries.push(entry);
    }

    pub fn set_value(&mut self, id: usize, value: VariableValue) {
        self.entries[id - 1].value = value;
    }

    pub fn get_value(&self, id: usize) -> &VariableValue {
        &self.entries[id - 1].value
    }

    pub fn get_value_mut(&mut self, id: usize) -> &mut VariableValue {
        &mut self.entries[id - 1].value
    }

    pub fn try_get_value(&self, id: usize) -> Option<&VariableValue> {
        if id > self.entries.len() {
            return None;
        }
        Some(&self.entries[id - 1].value)
    }

    pub fn get_var_entry(&self, id: usize) -> &TableEntry {
        &self.entries[id - 1]
    }

    pub fn try_get_entry(&self, id: usize) -> Option<&TableEntry> {
        if id == 0 || id > self.entries.len() {
            return None;
        }
        Some(&self.entries[id - 1])
    }

    pub fn get_var_entry_mut(&mut self, id: usize) -> &mut TableEntry {
        &mut self.entries[id - 1]
    }

    pub fn scan_user_variables_version(&self) -> u16 {
        for (i, u_var) in USER_VARIABLES.iter().enumerate() {
            if i >= self.entries.len()
                || self.entries[i].header.variable_type != u_var.value.vtype
                || self.entries[i].header.vector_size != u_var.value.get_vector_size()
            {
                return if u_var.version > 340 {
                    340
                } else if u_var.version > 300 {
                    300
                } else if u_var.version > 100 {
                    100
                } else {
                    0
                };
            }
        }
        0
    }

    pub fn print_variable_table(&self) {
        println!();
        execute!(
            stdout(),
            Print("Variable Table ".to_string()),
            SetAttribute(Attribute::Bold),
            Print(format!("{}", self.len())),
            SetAttribute(Attribute::Reset),
            Print(" variables\n\n".to_string())
        )
        .unwrap();

        println!("   # Type         Flags Role           Name        Value");
        println!("---------------------------------------------------------------------------------------");
        for var in self.entries.iter().rev() {
            let ts = if var.header.dim > 0 {
                format!("{}({})", var.header.variable_type, var.header.dim)
            } else {
                var.header.variable_type.to_string()
            };
            execute!(
                stdout(),
                SetForegroundColor(Color::Green),
                Print(format!("{:04X} ", var.header.id)),
                SetAttribute(Attribute::Reset),
            )
            .unwrap();

            execute!(
                stdout(),
                SetForegroundColor(Color::Yellow),
                Print(format!("{ts:<13}")),
                SetAttribute(Attribute::Reset),
            )
            .unwrap();

            let ts = format!("{:?}", var.get_type());

            execute!(
                stdout(),
                SetAttribute(Attribute::Bold),
                Print(format!("{}", var.header.flags)),
                SetAttribute(Attribute::Reset),
            )
            .unwrap();

            print!("     {ts:<15}");
            print!("{:<12}", var.get_name());

            if var.header.variable_type == VariableType::Function {
                unsafe {
                    execute!(
                        stdout(),
                        SetAttribute(Attribute::Bold),
                        Print(format!("{:?}", var.value.data.function_value)),
                        SetAttribute(Attribute::Reset)
                    )
                    .unwrap();
                }
            } else if var.header.variable_type == VariableType::Procedure {
                unsafe {
                    execute!(
                        stdout(),
                        SetAttribute(Attribute::Bold),
                        Print(format!("{:?}", var.value.data.procedure_value)),
                        SetAttribute(Attribute::Reset)
                    )
                    .unwrap();
                }
            } else if var.header.dim > 0 {
                let d = match var.header.dim {
                    1 => format!("{}", var.header.vector_size),
                    2 => format!("{}, {}", var.header.vector_size, var.header.matrix_size),
                    _ => format!(
                        "{}, {}, {}",
                        var.header.vector_size, var.header.matrix_size, var.header.cube_size
                    ),
                };
                execute!(
                    stdout(),
                    Print("[".to_string()),
                    SetAttribute(Attribute::Bold),
                    Print(d),
                    SetAttribute(Attribute::Reset),
                    Print("]".to_string()),
                )
                .unwrap();
            } else if var.header.variable_type == VariableType::String
                || var.header.variable_type == VariableType::BigStr
            {
                execute!(
                    stdout(),
                    SetAttribute(Attribute::Bold),
                    Print(format!("\"{}\"", var.value)),
                    SetAttribute(Attribute::Reset)
                )
                .unwrap();
            } else {
                execute!(
                    stdout(),
                    SetAttribute(Attribute::Bold),
                    Print(format!("{}", var.value)),
                    SetAttribute(Attribute::Reset)
                )
                .unwrap();
            }
            println!();
        }
    }

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn serialize(&self, buffer: &mut Vec<u8>) -> Result<(), ExecutableError> {
        if self.entries.len() > u16::MAX as usize {
            return Err(ExecutableError::TooManyDeclarations(self.entries.len()));
        }
        let max_var = u16::to_le_bytes(self.entries.len() as u16);
        buffer.extend_from_slice(&max_var);

        for d in self.entries.iter().rev() {
            let var_data = d.to_buffer(self.version)?;
            buffer.extend(var_data);
        }
        Ok(())
    }

    pub fn get_version(&self) -> u16 {
        self.version
    }
    pub fn set_version(&mut self, version: u16) {
        self.version = version;
    }

    pub fn get_entries(&self) -> &[TableEntry] {
        &self.entries
    }
}

pub struct UserVariable {
    pub name: &'static str,
    pub version: u16,
    pub value: VariableValue,
}

lazy_static::lazy_static! {
    pub static ref USER_VARIABLES: [UserVariable;29] = [
        UserVariable { name: "U_EXPERT", version:100, value: VariableValue::new_bool(false)  },
        UserVariable { name: "U_FSE", version:100, value:VariableValue::new_bool(false) },
        UserVariable { name: "U_FSEP", version:100, value:VariableValue::new_bool(false) },
        UserVariable { name: "U_CLS", version:100, value:VariableValue::new_bool(false) },
        UserVariable { name: "U_EXPDATE", version:100, value:VariableValue::new(VariableType::Date, VariableData::default()) },
        UserVariable { name: "U_SEC", version:100, value:VariableValue::new_int(0) },
        UserVariable { name: "U_PAGELEN", version:100, value:VariableValue::new_int(0) },
        UserVariable { name: "U_EXPSEC", version:100, value:VariableValue::new_int(0) },
        UserVariable { name: "U_CITY", version:100, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_BDPHONE", version:100, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_HVPHONE", version:100, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_TRANS", version:100, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_CMNT1", version:100, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_CMNT2", version:100, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_PWD", version:100, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_SCROLL", version:100, value:VariableValue::new_bool(false) },
        UserVariable { name: "U_LONGHDR", version:100, value:VariableValue::new_bool(false) },
        UserVariable { name: "U_DEF79", version:100, value:VariableValue::new_bool(false) },
        UserVariable { name: "U_ALIAS", version:100, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_VER", version:100, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_ADDR", version:100, value:VariableValue::new_vector(
            VariableType::String,
            vec![VariableValue::new_string(String::new()); 5],
        )},
        UserVariable { name: "U_NOTES", version:100, value:VariableValue::new_vector(VariableType::String, vec![VariableValue::new_string(String::new()); 4]) },
        UserVariable { name: "U_PWDEXP", version:100, value:VariableValue::new(VariableType::Date, VariableData::default()) },

        UserVariable { name: "U_ACCOUNT", version:300, value:VariableValue::new_vector(VariableType::Integer, vec![VariableValue::new_int(0); 16]) },

        UserVariable { name: "U_SHORTDESC", version:340, value:VariableValue::new_bool(false) },
        UserVariable { name: "U_GENDER", version:340, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_BIRTHDATE", version:340, value:VariableValue::new_date(0) },
        UserVariable { name: "U_EMAIL", version:340, value:VariableValue::new_string(String::new()) },
        UserVariable { name: "U_WEB", version:340, value:VariableValue::new_string(String::new()) },
    ];
}
