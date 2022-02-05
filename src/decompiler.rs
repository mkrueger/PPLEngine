use std::collections::HashMap;
use std::str::*;
use crate::executable::*;
use crate::tables::*;

const LAST_FUNC: i32 = 0xfee2;
const LAST_STMT: i32 = 0x00e2;

struct FuncL
{
    func: i32
}

pub struct Decompiler
{
    executable : Executable,
    akt_stat : i32,
    akt_proc : i32,
    uvar_flag : bool,
    pass : i32,
    next_label : i32,
    next_func : i32,
    func_flag : i32,
    proc_flag : i32,
    src_ptr : i32,
    symbol : i32,
    valid_buffer : Vec<bool>,
    exp_count : i32,
    trash_flag : i32,
    label_used : HashMap<i32, i32>,
    func_used : HashMap<i32, FuncL>,
    label_stack : Vec<i32>,
    string_stack : Vec<String>,
    pub output : String
}

impl Decompiler {
    fn new(executable: Executable) -> Self {
        let mut valid_buffer = Vec::new();
        for _ in 0..executable.code_size / 2 + 1 {
            valid_buffer.push(false);
        }
        Decompiler {
            executable,
            akt_stat: 0,
            akt_proc: 0,
            uvar_flag: false,
            pass: 0,
            next_label: 0,
            next_func: 0,
            func_flag: 0,
            proc_flag: 0,
            src_ptr: 0,
            symbol: 0,
            valid_buffer,
            exp_count: 0,
            trash_flag: 0,
            label_used: HashMap::new(),
            func_used: HashMap::new(),
            label_stack: Vec::new(),
            string_stack: Vec::new(),
            output: String::new()
        }
    }

    pub fn read(file_name: &str) -> Decompiler {
        let mut d = Decompiler::new(read_file(file_name));
        d.do_pass1();
        d.pass += 1;
        d.dump_vars();
        d.do_pass2();
        d
    }

    fn fill_valid(&mut self, i: i32, j: i32) {
        for n in i..j {
            self.valid_buffer[n as usize] = true;
        }
    }

    fn do_pass1(&mut self) {
        let mut last_point = 0;
        let mut if_ptr = -5;

        self.akt_stat = 0;
        self.next_label = 0;
        self.next_func = 0;
        self.src_ptr = 0;

        while self.src_ptr != -1 && self.src_ptr <= self.executable.code_size / 2 {
            if self.src_ptr >= self.executable.code_size / 2 || self.valid_buffer[self.src_ptr as usize] {
                self.fill_valid(last_point, self.src_ptr);
            } else {
                let prev_stat = self.akt_stat;
                if self.src_ptr == if_ptr { self.src_ptr += 2; }

                self.akt_stat = self.executable.source_buffer[self.src_ptr as usize];
                if self.akt_stat > LAST_STMT || (self.akt_stat as usize) < STATEMENT_SIGNATURE_TABLE.len() && STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] == 0xaa {
                    panic!("Error: Unknown statement {}.", self.akt_stat);
                }
                if self.akt_stat < 0 {
                    panic!("act_stat < 0 : {}", self.akt_stat);
                }
                let mut trap = false;


                match STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] {
                    0xf6 => {
                        self.src_ptr += 2; 
                        self.akt_proc = self.executable.source_buffer[(self.src_ptr - 1) as usize] - 1;
                        self.executable.variable_declarations.get_mut(&self.akt_proc).unwrap().flag = 1;
                        let act_dec_start = self.executable.variable_declarations.get(&self.akt_proc).unwrap().start;
                        let act_dec_args = self.executable.variable_declarations.get(&self.akt_proc).unwrap().args;

                        self.funcin(act_dec_start, self.akt_proc);
                         self.pushlabel(act_dec_start);
                        trap = self.getexpr(act_dec_args, 0) != 0;
                    },
                    0xf7 => {
                        if self.getexpr(0x01, 0) != 0 {
                            trap = true;
                        } else {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            trap = self.getexpr(0x01, 0) != 0;
                        }
                    },
                    0xf8 => {
                        if self.getexpr(0x03, 0) != 0 {
                            trap = true;
                        } else {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            self.src_ptr += 1;
                        }
                    },
                    0xf9 => {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 1] - 1)).unwrap().flag = 1;
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 2] - 1)).unwrap().flag = 1;
                        self.src_ptr += 3;
                    },
                    0xfe => {
                        self.src_ptr += 1;
                        trap = self.getexpr(self.executable.source_buffer[self.src_ptr as usize], 0) != 0;
                    },
                    0xfa => {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 2] - 1)).unwrap().flag = 1;
                        self.src_ptr += 2;
                        trap = self.getexpr(self.executable.source_buffer[self.src_ptr as usize - 1] - 1, 0) != 0;
                    },
                    0xfc => {
                        self.src_ptr += 1;
                        trap = self.getexpr(self.executable.source_buffer[self.src_ptr as usize] | 0xf00, 0) != 0;
                    },
                    0xfd => {
                        self.src_ptr += 1;
                        self.labelin(self.executable.source_buffer[self.src_ptr as usize]);
                        self.src_ptr += 1;
                    },
                    0xff => {
                        if_ptr = self.src_ptr;
                        if self.getexpr(0x01, 0) != 0 {
                            trap = true;
                        } else {
                            if_ptr = self.set_if_ptr(if_ptr);
                            self.src_ptr += 1;
                        }
                    },
                    _ => {
                        trap = self.getexpr((STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] & 0x0f), 0) != 0;
                    }
                }
                if !trap {
                    match self.akt_stat {
                        0x0001 | // END
                        0x002a | // RETURN
                        0x00a9 | // ENDPROC
                        0x00ab | // ENDFUNC
                        0x003a => {
                            self.fill_valid(last_point, self.src_ptr); // STOP
                            last_point = self.src_ptr;
                            if prev_stat == 0x000b {
                                continue;
                            }
                        },
                        0x0029 => {
                            self.fill_valid(last_point, self.src_ptr); // GOSUB
                            last_point = self.src_ptr;
                            self.pushlabel(self.executable.source_buffer[self.src_ptr as usize - 1]);
                            continue;
                        },
                        0x0007 => {
                            self.fill_valid(last_point, self.src_ptr); // GOTO
                            last_point = self.src_ptr;
                             self.pushlabel(self.executable.source_buffer[self.src_ptr as usize - 1]);
                            if prev_stat == 0x000b {
                                continue;
                            }
                        },
                        0x000b => {
                            self.fill_valid(last_point, self.src_ptr); // IF
                            last_point = self.src_ptr;
                            self.pushlabel(self.executable.source_buffer[self.src_ptr as usize - 1]);
                            continue;
                        }
                        _ => {
                            continue;
                        }
                    }
                }
            }
            self.src_ptr = self.poplabel() / 2;
            last_point = self.src_ptr;
        }
    }

    fn dump_vars(&mut self) {
        self.uvar_flag =
            self.executable.variable_declarations[&0].variable_type == VariableType::Boolean &&
                (self.executable.variable_declarations[&1].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&2].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&3].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&4].variable_type == VariableType::Date) &&
                (self.executable.variable_declarations[&5].variable_type == VariableType::Integer) &&
                (self.executable.variable_declarations[&6].variable_type == VariableType::Integer) &&
                (self.executable.variable_declarations[&7].variable_type == VariableType::Integer) &&
                (self.executable.variable_declarations[&8].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&9].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&10].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&11].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&12].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&13].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&14].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&15].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&16].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&17].variable_type == VariableType::Boolean) &&
                (self.executable.variable_declarations[&18].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&19].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&20].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&21].variable_type == VariableType::String) &&
                (self.executable.variable_declarations[&22].variable_type == VariableType::Date) &&
                (self.executable.variable_declarations[&20].dims[0] == 5);

        if self.uvar_flag && self.executable.version >= 300 {
            if !(self.executable.variable_declarations[&23].variable_type == VariableType::Integer && self.executable.variable_declarations[&23].dims[0] == 16) {
                self.uvar_flag = false;
            }
        }

        if self.symbol == 0 {
            self.output.push_str("\n;Source Code:\n");
            self.output.push_str(";------------------------------------------------------------------------------\n\n");
        }

        let mut i: i32 = 0;
        let mut c_vars = 0;
        let mut c_const = 0;
        let mut c_func = 0;
        let mut c_proc = 0;

        while i < self.executable.max_var {
            if i > 0x16 || !self.uvar_flag {
                if self.executable.variable_declarations.get_mut(&i).unwrap().lflag == 0 {
                    match self.executable.variable_declarations.get_mut(&i).unwrap().variable_type {
                        VariableType::Function => {
                            if self.executable.variable_declarations.get_mut(&i).unwrap().flag == 1 {
                                c_func += 1;
                                self.executable.variable_declarations.get_mut(&i).unwrap().number = c_func;
                                let n = &(self.executable.variable_declarations.get_mut(&i).unwrap().return_var - 1);
                                self.executable.variable_declarations.get_mut(n).unwrap().number = c_func;
                                if self.symbol == 0 {
                                    self.output.push_str("    DECLARE ");
                                    self.output_func(i);
                                    self.output.push_str("\n");
                                }
                            }
                        },
                        VariableType::Method => {
                            let mut cur_var = self.executable.variable_declarations.get_mut(&i).unwrap();
                            if cur_var.flag == 1 {
                                c_proc += 1;
                                cur_var.number = c_proc;
                                if self.symbol == 0 {
                                    self.output.push_str("    DECLARE ");
                                    self.output_proc(i);
                                    self.output.push_str("\n");
                                }
                            }
                        },
                        _ => {
                            let cur_var = self.executable.variable_declarations.get_mut(&i).unwrap();
                            if cur_var.flag == 0 {
                                c_const += 1;
                                cur_var.number = c_const;
                            } else {
                                c_vars += 1;
                                cur_var.number = c_vars;
                                if self.symbol == 0 {
                                    self.output.push_str(format!("    {}", TYPE_NAMES[cur_var.variable_type as usize]).as_str());
                                    self.output.push_str(format!("  VAR{0:>03}", c_vars).as_str());
                                    match cur_var.dim {
                                        1 => self.output.push_str(format!("({})", cur_var.dims[0]).as_str()),
                                        2 => self.output.push_str(format!("({}, {})", cur_var.dims[0], cur_var.dims[1]).as_str()),
                                        3 => self.output.push_str(format!("({}, {}, {})", cur_var.dims[0], cur_var.dims[1], cur_var.dims[2]).as_str()),
                                        _ => { }
                                    }
                                    self.output.push_str("\n");
                                }
                            }
                        }
                    }
                }
            }
            i += 1;
        }

        if self.symbol == 0 {
            self.output.push_str("\n;------------------------------------------------------------------------------\n\n");
        }
    }

    fn output_func(&mut self, func : i32) {
        self.output.push_str(format!("FUNCTION FUNC{0:>03}(", self.executable.variable_declarations.get(&func).unwrap().number).as_str());
        let mut j = self.executable.variable_declarations.get(&func).unwrap().first_var;
        for i in 0..self.executable.variable_declarations.get(&func).unwrap().args {
            if i != 0 {
                self.output.push_str(",");
            }
            self.output.push_str(format!("{}", TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize]).as_str());
            self.executable.variable_declarations.get_mut(&j).unwrap().func = func;
            self.output.push_str(format!(" LOC{0:>03}", self.executable.variable_declarations.get(&j).unwrap().number).as_str());
            j += 1;
        }
        self.output.push_str(format!(") {}", TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize]).as_str());
    }

    fn output_proc(&mut self, proc : i32) {
        self.output.push_str(format!("PROCEDURE PROC{0:>03}(", self.executable.variable_declarations.get(&proc).unwrap().number).as_str());
        let mut j = self.executable.variable_declarations.get(&proc).unwrap().first_var;
        for i in 0..self.executable.variable_declarations.get(&proc).unwrap().args {
            if i != 0 {
                self.output.push_str(",");
            }
            if ((self.executable.variable_declarations.get(&proc).unwrap().return_var >> i) & 1) != 0
            {
                self.output.push_str("VAR ");
            }
            self.output.push_str(format!("{}", TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize]).as_str());
            self.executable.variable_declarations.get_mut(&j).unwrap().func = proc;
            self.output.push_str(format!(" LOC{0:>03}", self.executable.variable_declarations.get(&j).unwrap().number).as_str());
            j += 1;
        }
        self.output.push_str(")");
    }

    fn funcin(&mut self, label: i32, func: i32) {
        self.func_used.insert(label, FuncL
        {
            func: func/*,
            label: self.func_used.len()*/
        });
    }

    fn funcout(&mut self, label: i32) {
        match self.func_used.get(&label) {
            Some(funcl)=>{
                let func = funcl.func;
                if self.func_flag == 1 {
                    self.output.push_str("    ENDFUNC\n\n");
                }

                if self.proc_flag == 1
                {
                    self.output.push_str("    ENDPROC\n\n");
                }
                self.func_flag = 0;
                self.proc_flag = 0;

                self.output.push_str("\n");

                if self.executable.variable_declarations.get(&func).unwrap().variable_type == VariableType::Function {
                    self.output_func(func);
                    self.func_flag = 1;
                } else {
                    self.output_proc(func);
                    self.proc_flag = 1;
                }
                self.dump_locs(func);
                if self.symbol == 0 {
                    self.output.push_str("\n");
                }
            },
            _ => {}
        }
    }

    fn dump_locs(&mut self, func : i32) {
        let mut i;
        let mx_var : i32;
        let mut j = 0;
        // StackPtr = 0;
        let cur_var = self.executable.variable_declarations.get(&func).unwrap();
        if cur_var.variable_type == VariableType::Function
        {
            i = cur_var.return_var;
            mx_var = cur_var.return_var + cur_var.total_var;
        } else {
            i = cur_var.first_var + cur_var.args;
            mx_var = cur_var.first_var + cur_var.total_var; //+1;
        }

        while i < mx_var
        {
            if self.executable.variable_declarations.get(&i).unwrap().flag == 1
            {
                self.executable.variable_declarations.get_mut(&i).unwrap().func = func;
                if self.symbol == 0
                {
                    self.output.push_str(format!("\n    {}", crate::tables::TYPE_NAMES[self.executable.variable_declarations.get(&i).unwrap().variable_type as usize]).as_str());
                    self.output.push_str(format!(" LOC{}", self.executable.variable_declarations.get(&i).unwrap().number).as_str());
                    match self.executable.variable_declarations[&i].dim {
                        1 => {
                            self.output.push_str(format!("({}) ", self.executable.variable_declarations.get(&i).unwrap().dims[0]).as_str());
                        },
                        2 => {
                            self.output.push_str(format!("({},{}) ", self.executable.variable_declarations.get(&i).unwrap().dims[0], self.executable.variable_declarations.get(&i).unwrap().dims[1]).as_str());
                        },
                        3 => {
                            self.output.push_str(format!("({},{},{}) ", self.executable.variable_declarations.get(&i).unwrap().dims[0], self.executable.variable_declarations.get(&i).unwrap().dims[1], self.executable.variable_declarations.get(&i).unwrap().dims[2]).as_str());
                        },
                        _  => {}
                    }
                }
            }
            i += 1;
            j += 1;
        }
        if j > 0 {
            self.output.push_str("\n");
        }
    }

    fn labelin(&mut self, label : i32) {
        self.label_used.insert(label, (self.label_used.len() + 1) as i32);
    }

    fn labelnr(&mut self, label : i32) -> &i32 {
        match self.label_used.get(&label) {
            Some(x) => x,
            _ => &-1
        }
    }

    fn labelout(&mut self, label : i32) {
        match self.label_used.get(&label) {
            Some(x) => {
                self.output.push_str(format!("\n:LABEL{0:>03}\n", x).as_str());
            },
            _ => {}
        }
    }

    fn pushlabel(&mut self, label : i32) {
        self.label_stack.push(label);
    }

    fn poplabel(&mut self) -> i32 {
        if self.label_stack.len() == 0 {
            -2
        } else {
            self.label_stack.pop().unwrap()
        }
    }

    fn pushstr(&mut self, str : String) {
        self.exp_count += 1;
        self.string_stack.push(str);
    }

    fn popstr(&mut self) -> String {
        if self.string_stack.len() == 0 {
            String::new()
        } else {
            self.exp_count -= 1;
            self.string_stack.pop().unwrap()
        }
    }

    fn stripper(str : String) -> String {
        /*
        while (str.startsWith("(") && str.EndsWith(")"))
        {
            str = str.Substring(1, str.Length - 2);
        }*/
        str
    }

    fn popstrip(&mut self) -> String {
        return Decompiler::stripper(self.popstr());
    }

    fn const_name<'a>(& mut self, vars : &[i32], names : &[&str]) -> String {
        let temp_str = self.popstrip();
        let parse_result = i32::from_str(&temp_str);
        match parse_result {
            Ok(val) => {
                let mut i = 0;
                while vars[i] != -1 && vars[i] != val {
                    i += 1;
                }
                if vars[i] != -1 {
                    return names[i].to_string();
                }
                temp_str.to_string()
            },
            _ => { format!("(UNKNOWN CONST:{})", temp_str) }
        }
    }

    fn trans_exp(&mut self, cur_expr: i32) -> String {
        match self.akt_stat {
            0x00c => {
                if cur_expr != 2 {
                    self.popstrip()
                } else {
                    self.const_name(&crate::tables::CONSTANT_CONFERENCE_OFFSETS, &crate::tables::CONSTANT_CONFERENCE_NAMES)
                }
            },
            0x00d => {
                if cur_expr != 2 {
                    self.popstrip()
                } else {
                    self.const_name(&crate::tables::CONSTANT_CONFERENCE_OFFSETS, &crate::tables::CONSTANT_CONFERENCE_NAMES)
                }
            },
            0x00e => {
                if cur_expr != 2 {
                    self.popstrip()
                } else {
                    self.const_name(&crate::tables::CONSTANT_NAMES_OFFSETS, &crate::tables::CONSTANT_NAMES_DISPLAY)
                }
            },
            0x010 => {
                if cur_expr == 3 {
                    self.const_name(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if cur_expr == 4 {
                    self.const_name(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                } else {
                    self.popstrip()
                }
            },
            0x011 => {
                if cur_expr == 3 {
                    self.const_name(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if cur_expr == 4 {
                    self.const_name(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                }
                else {
                    self.popstrip()
                }
            },
            0x012 => {
                if cur_expr == 3 {
                    self.const_name(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if cur_expr == 4 {
                    self.const_name(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                } else {
                    self.popstrip()
                }
            },
            0x018 => {
                self.const_name(&crate::tables::CONSTANT_LINECOUNT_OFFSETS, &crate::tables::CONSTANT_LINECOUNT_NAMES)
            },
            0x022 => {
                if cur_expr != 6 {
                    self.popstrip()
                } else {
                    self.const_name(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                }
            },
            0x02b => {
                if cur_expr != 5 {
                    self.popstrip()
                } else {
                    self.const_name(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                }
            },
            0x039 => {
                if cur_expr != 2 {
                    self.popstrip()
                }
                else  {
                    self.const_name(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                }
            },
            0x070 => {
                if cur_expr != 3 {
                    self.popstrip()
                }
                else {
                    self.const_name(&crate::tables::CONSTANT_SEEK_OFFSETS, &crate::tables::CONSTANT_SEEK_NAMES)
                }
            },
            0x0cd | 0x0ce => {
                if cur_expr != 1 {
                    self.popstrip()
                }
                else  {
                    self.const_name(&crate::tables::CONSTANT_2_OFFSETS, &crate::tables::CONSTANT_2_NAMES)
                }
            },
            _ => {
                self.popstrip()
            }
        }
    }

    fn varout(&mut self, var_number : i32) -> String
    {
        let var_nr = var_number - 1;
        if (self.executable.version < 300 && var_nr < 0x17 && self.uvar_flag) || (self.executable.version >= 300 && var_nr < 0x18 && self.uvar_flag) {
            return String::from(USER_VARIABLE_NAMES[var_nr as usize + 1]);
        }
        
        if self.executable.variable_declarations.get(&var_nr).unwrap().flag == 1
        {
            match self.executable.variable_declarations.get(&var_nr).unwrap().variable_type {
                VariableType::Function => {
                    return format!("FUNC{0:>03}", self.executable.variable_declarations.get(&var_nr).unwrap().number);
                },
                VariableType::Method => {
                    return format!("PROC{0:>03}", self.executable.variable_declarations.get(&var_nr).unwrap().number);
                },
                _ => {
                    if self.executable.variable_declarations.get(&var_nr).unwrap().fflag == 1 {
                        return format!("FUNC{0:>03}", self.executable.variable_declarations.get(&var_nr).unwrap().number);
                    }
                    if self.executable.variable_declarations.get(&var_nr).unwrap().lflag == 1 {
                        return format!("LOC{0:>03}", self.executable.variable_declarations.get(&var_nr).unwrap().number);
                    }
                    return format!("VAR{0:>03}", self.executable.variable_declarations.get(&var_nr).unwrap().number);
                }
            }
        }
        // println!("var type: {}", self.executable.variable_declarations.get(&var_nr).unwrap().variable_type as u8);
        match self.executable.variable_declarations.get(&var_nr).unwrap().variable_type {
            VariableType::Boolean => {
                if self.executable.variable_declarations.get(&var_nr).unwrap().content != 0 {
                    String::from("TRUE")
                } else {
                    String::from("FALSE")
                }
            },
            VariableType::Date => {
                return self.executable.variable_declarations.get(&var_nr).unwrap().content.to_string();
            },
            VariableType::Money => {
                return format!("${}", (self.executable.variable_declarations.get(&var_nr).unwrap().content as f32) / 10.0);
            },
            VariableType::String => {
                return format!("\"{}\"", self.executable.variable_declarations.get(&var_nr).unwrap().string_value);
            },
            VariableType::Time => {
                return self.executable.variable_declarations.get(&var_nr).unwrap().content.to_string();
            },
            VariableType::Double => {
                return (self.executable.variable_declarations.get(&var_nr).unwrap().content as f64).to_string();
            },
            _ =>{
                return (self.executable.variable_declarations.get(&var_nr).unwrap().content).to_string();
            }
        }
    }

    fn fnktout(&mut self, func : u16) -> i32
    {
        let mut i = 0;

        match FUNCTION_SIGNATURE_TABLE[func as usize] {
             0x10 => {
                 if self.exp_count < 1 {
                     return -1;
                 }
                 let tmp = self.popstr();
                 self.pushstr(format!("({}{})", EXPR_NAMES[func as usize], tmp));
                 return 0;
             },
            0x11 => {
                if self.exp_count < 2 {
                    return -1;
                }
                let temp_str2 = format!("{})",self.popstr());
                let temp_str =  format!("({}", self.popstr());
                self.pushstr(format!("{}{}{}", temp_str, EXPR_NAMES[func as usize], temp_str2));
                return 0;
            },
            _=> {
                if self.exp_count < FUNCTION_SIGNATURE_TABLE[func as usize] {
                    return -1;
                }
                let mut temp_str2 = String::from(")");
                while FUNCTION_SIGNATURE_TABLE[func as usize] > i {
                    i += 1;
                    let mut temp_str = self.popstr();
                    if i != 1 {
                        temp_str.push_str(",")
                    }
                    temp_str.push_str(temp_str2.as_str());
                    temp_str2 = temp_str;
                }
                self.pushstr(format!("{}({}", EXPR_NAMES[func as usize], temp_str2));
                return 0;
            }
        }
    }

    fn dimexpr(&mut self, dims : i32) -> i32
    {
        let mut cur_dim = 0;
        let temp_expr  = self.exp_count;

        if self.pass == 1 {
            let tmp = self.popstr();
            self.pushstr(format!("{}(", tmp));
        }

        loop {
            if cur_dim == dims {
                break;
            }
            cur_dim += 1;

            self.exp_count = 0;
            let mut tmp_func = 0;
            self.src_ptr += 1;
            if cur_dim != 1 && self.pass == 1 {
                let tmp = self.popstr();
                self.pushstr(format!("{},", tmp));
            }

            while (self.src_ptr as usize) < self.executable.source_buffer.len() && self.executable.source_buffer[self.src_ptr as usize] != 0 {
                if self.executable.source_buffer[self.src_ptr as usize] <= self.executable.max_var {
                    if self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().variable_type == VariableType::Function {
                         self.pushlabel(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().start);
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize - 1])).unwrap().flag = 1;
                        self.funcin(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().start, self.executable.source_buffer[self.src_ptr as usize] - 1);
                        if self.pass == 1
                        {
                            let tmp2 = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                            let tmp3 = self.popstr();
                            self.pushstr(format!("{}{}(", tmp3, tmp2));
                        }
                        self.src_ptr += 1;
                        if self.getexpr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().args, 1) != 0 {
                            return 1;
                        }
                        self.src_ptr -= 1;
                        if self.pass == 1 {
                            let tmp = self.popstr();
                            self.pushstr(format!("{})", tmp));
                        }
                    }
                    else
                    {
                        if self.pass == 1 {
                            let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                            self.pushstr(tmp);
                        }
                        self.src_ptr += 1;
                        if self.executable.source_buffer[self.src_ptr as usize] != 0
                        {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().flag = 1;
                            if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize]) != 0 {
                                return 1;
                            }
                        }
                    }
                }
                else
                {
                    let x = -self.executable.source_buffer[self.src_ptr as usize] - 1;
                    if self.executable.source_buffer[self.src_ptr as usize] < LAST_FUNC || (x >= 0 && FUNCTION_SIGNATURE_TABLE[x as usize]  == 0xaa) {
                        panic!("Error: Unknown function {}", self.executable.source_buffer[self.src_ptr as usize]);
                    }

                    if self.pass == 1 {
                        if self.fnktout((-self.executable.source_buffer[self.src_ptr as usize]) as u16 - 1) != 0 {
                            tmp_func = self.executable.source_buffer[self.src_ptr as usize];
                            self.trash_flag = 1;
                        }
                        else if tmp_func != 0 && self.fnktout((-tmp_func - 1) as u16) == 0 {
                            tmp_func = 0;
                        }
                    }
                }
                self.src_ptr += 1;
            }

            if self.pass == 1 {
                let temp_str2 = self.popstr();
                let tmp = self.popstr();
                self.pushstr(format!("{}{}", tmp, temp_str2));
            }
        }

        if self.pass == 1 {
            let tmp = self.popstr();
            self.pushstr(format!("{})", tmp));
        }

        self.exp_count = temp_expr;
        return 0;
    }

    fn getexpr(&mut self, max_expr: i32, rec : i32) -> i32
    {
        let mut cur_expr = 0;
        let temp_expr = self.exp_count;

        self.src_ptr += 1;
        while (max_expr & 0x0ff) != cur_expr {
            cur_expr += 1;

            self.exp_count = 0;
            let mut tmp_func = 0;
            if cur_expr != 1 && self.pass == 1 {
                let tmp = self.popstr();
                if self.akt_stat == 8 && rec == 0 {
                    self.pushstr(format!("{}=", tmp));
                } else {
                    self.pushstr(format!("{},", tmp));
                }
            }

            while self.executable.source_buffer[self.src_ptr as usize] != 0 {
                if self.executable.source_buffer[self.src_ptr as usize] >= 0 && self.executable.source_buffer[self.src_ptr as usize] <= self.executable.max_var {
                    if max_expr / 256 == cur_expr || max_expr / 256 == 0x0f {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                        if self.pass == 1 {
                            let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                            self.pushstr(tmp);
                        }
                        self.src_ptr += 1;
                        if self.executable.source_buffer[self.src_ptr as usize] != 0
                        {
                            if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize]) != 0 {
                                return 1;
                            }
                            if self.pass == 1 {
                                let temp_str2 = self.popstr();
                                let temp_str = self.popstr();
                                self.pushstr(format!("{}{}", temp_str, temp_str2));
                            }
                        }
                    }
                    else
                    {
                        if self.akt_stat == 0xa8
                        {
                            if ((self.executable.variable_declarations.get(&self.akt_proc).unwrap().return_var >> (cur_expr - 1)) & 1) != 0 {
                                self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().flag = 1;
                            }
                        }
                        if self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().variable_type == VariableType::Function {
                            self.pushlabel(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().start);
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize]   - 1)).unwrap().flag = 1;
                            self.funcin(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] - 1)).unwrap().start, self.executable.source_buffer[self.src_ptr as usize] - 1);
                            if self.pass == 1  {
                                let tmp2 = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                                let tmp3 = self.popstr();
                                self.pushstr(format!("{}{}(", tmp3, tmp2));
                            }
                            self.src_ptr += 1;
                            if self.getexpr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().args, 1) != 0 {
                                return 1;
                            }
                            if self.pass == 1 {
                                let tmp = self.popstr();
                                self.pushstr(format!("{})", tmp));
                            }
                        }
                        else
                        {
                            if self.pass == 1 {
                                let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                                self.pushstr(tmp);
                            }
                            self.src_ptr += 1;
                            if self.executable.source_buffer[self.src_ptr as usize] != 0 {
                                self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().flag = 1;
                                if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize]) != 0 {
                                    return 1;
                                }
                                /*
                                if self.pass == 1 {
                                    								strcpy(temp_str2,self.popstr());
                                                                    strcpy(temp_str,self.popstr());
                                                                    strcat(temp_str,temp_str2);
                                                                    self.pushstr(temp_str);
                                }
                                 */
                            }
                            self.src_ptr += 1;
                        }
                    }
                }
                else
                {
                    let func_idx =(-self.executable.source_buffer[self.src_ptr as usize] - 1) as usize;
                    if func_idx >= FUNCTION_SIGNATURE_TABLE.len() || FUNCTION_SIGNATURE_TABLE[func_idx] == 0xaa {
                        self.pushstr(format!("Error: Unknown function {}", self.executable.source_buffer[self.src_ptr as usize]));
                        return 0;
                    }

                    if self.pass == 1 {
                        if self.fnktout((-self.executable.source_buffer[self.src_ptr as usize] - 1) as u16) != 0 {
                            tmp_func = self.executable.source_buffer[self.src_ptr as usize];
                            self.trash_flag = 1;
                        }
                        else if tmp_func != 0 && self.fnktout((-tmp_func - 1) as u16) == 0 {
                            tmp_func = 0;
                        }
                    }
                    self.src_ptr += 1;
                }
            }

            self.src_ptr += 1;
            if self.pass == 1 {
                let tmp2 = self.trans_exp(cur_expr);
                let tmp3 = self.popstr();
                self.pushstr(format!("{}{}", tmp3, tmp2));
            }
        }
        self.exp_count = temp_expr + 1;
        return 0;
    }

    fn set_if_ptr(&mut self, i : i32) -> i32
    {
        let j = self.executable.source_buffer[self.src_ptr as usize] as usize / 2;
        if self.executable.source_buffer[j - 2] == 0x0007 && self.executable.source_buffer[j - 1] / 2 == i {
            self.executable.source_buffer[i as usize] = 0;
            self.executable.source_buffer[self.src_ptr as usize] / 2 - 2
        } else {
            i - 5
        }
    }

    fn do_pass2(&mut self)
    {
        self.akt_stat = -1;
        self.next_label = 0;
        self.next_func = 0;
        self.src_ptr = 0;
        self.trash_flag = 0;
        let mut if_ptr = -1;

        while self.src_ptr < self.executable.code_size / 2 {
            let prev_stat = self.akt_stat;
            if self.src_ptr == if_ptr {
                self.src_ptr += 2;
            }

            self.funcout(self.src_ptr * 2);
            self.labelout(self.src_ptr * 2);
            if !self.valid_buffer[self.src_ptr as usize] {
                self.src_ptr += 1;
                continue;
            }

            self.akt_stat = self.executable.source_buffer[self.src_ptr as usize];
            if self.akt_stat > LAST_STMT || STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] == 0xaa {
                panic!("Error: Unknown statement {}, aborting…", self.akt_stat);
            }

            if prev_stat < 0 || STATEMENT_SIGNATURE_TABLE[prev_stat as usize] != 0xff {
                self.output.push_str("    ");
            }

            self.output.push_str(format!("{}", STATEMENT_NAMES[self.akt_stat as usize]).as_str());

            if self.akt_stat != 0xa8 {
                self.output.push_str(" ");
            }

            match STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] {
                0xf6 => {
                    self.src_ptr += 2;
                    self.akt_proc = self.executable.source_buffer[self.src_ptr as usize - 1] - 1;
                    self.output.push_str(format!("{0:>03}(", self.executable.variable_declarations.get(&self.akt_proc).unwrap().number).as_str());
                    if self.getexpr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] - 1)).unwrap().args, 0) != 0 {
                        return;
                    }
                    let tmp = self.popstr();
                    self.output.push_str(tmp.as_str());
                    self.output.push_str(")");
                },
                0xf7 => {
                    if self.getexpr(0x01, 0) != 0 {
                        return;
                    }
                    let tmp = self.varout(*self.executable.source_buffer.get(self.src_ptr as usize).unwrap());
                    self.output.push_str(format!(",{},", tmp.as_str()).as_str());
                    if self.getexpr(0x01, 0) != 0 {
                        return;
                    }
                    let tmp = self.popstr();
                    self.output.push_str(tmp.as_str());
                },
                0xf8 => {
                    if self.getexpr(0x03, 0) != 0 {
                        return;
                    }
                    let tmp = self.popstr();
                    self.output.push_str(tmp.as_str());
                    let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize]);
                    self.output.push_str(format!(",{}", tmp.as_str()).as_str());
                    self.src_ptr += 1;
                },
                0xf9 => {
                    let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize + 1]);
                    self.output.push_str(format!("{},", tmp).as_str());
                    let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize + 2]);
                    self.output.push_str(format!("{}", tmp).as_str());
                    self.src_ptr += 3;
                },
                0xfe => {
                    self.src_ptr += 1;
                    if self.getexpr(self.executable.source_buffer[self.src_ptr as usize], 0) != 0 {
                        return;
                    }
                    let tmp = self.popstr();
                    self.output.push_str(tmp.as_str());
                },
                0xfa => {
                    let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize + 2]);
                    self.output.push_str(format!("{},", tmp).as_str());
                    self.src_ptr += 2;
                    if self.getexpr(self.executable.source_buffer[self.src_ptr as usize - 1] - 1, 0) != 0 {
                        return;
                    }
                    let tmp = self.popstr();
                    self.output.push_str(tmp.as_str());
                },
                0xfc => {
                    self.src_ptr += 1;
                    if self.getexpr(self.executable.source_buffer[self.src_ptr as usize] | 0xf00, 0) != 0 {
                        return;
                    }
                    let tmp = self.popstr();
                    self.output.push_str(tmp.as_str());
                },
                0xfd => {
                    self.src_ptr += 1;
                    let tmp = *self.labelnr(self.executable.source_buffer[self.src_ptr as usize]);
                    self.output.push_str(format!("LABEL{0:>03}", tmp).as_str());
                    self.src_ptr += 1;
                },
                0xff => {
                    if_ptr = self.src_ptr;
                    self.output.push_str("(");
                    if self.getexpr(0x001, 0) != 0 {
                        return;
                    }
                    let tmp = self.popstr();
                    self.output.push_str(format!("{}) ", tmp).as_str());
                    if_ptr = self.set_if_ptr(if_ptr);
                    self.src_ptr += 1;
                },
                _ => {
                    if self.getexpr((STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] & 0x0f), 0) != 0 {
                        return;
                    }
                    let tmp = self.popstr();
                    self.output.push_str(tmp.as_str());
                }
            }

            if STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] != 0xff {
                if self.trash_flag != 0 {
                    self.output.push_str("   ; PPLC bug detected");
                    self.trash_flag = 0;
                }
                self.output.push_str("\n");
            }

            match self.akt_stat {
                0x0001 | // END
                0x002a | // RETURN
                0x003a => { // STOP
                    self.output.push_str("\n");
                },
                0x00a9 | // ENDPROC
                0x00ab  // ENDFUNC
                 => { //STOP
                    self.func_flag = 0;
                    self.proc_flag = 0;
                    self.output.push_str("\n");
                },
                _ => {}
            }

        }
        self.labelout(self.src_ptr * 2);
    }
}


#[cfg(test)]
mod tests {
    use std::env;

    fn is_match(output : &String, original : &String) -> bool {
        let mut i = 0;
        let mut j = 0;

        let output = output.as_bytes();
        let original = original.as_bytes();

        while i < output.len() && j < original.len() {
            if output[i] == original[j] {
                i += 1;
                j += 1;
                continue;
            }
            if char::is_whitespace(output[i] as char) {
                i += 1;
                continue;
            }
            if char::is_whitespace(original[j] as char) {
                j += 1;
                continue;
            }
            i += 1;
        }
        // skip original trailing ws.
        while j < original.len() && char::is_whitespace(original[j] as char) {
            j += 1;
        }
        return j >= original.len();
    }

    #[test]
    fn test_decompiler() {
        use std::fs::{self};

        let mut data_path =env::current_dir().unwrap();
        data_path.push("test_data");
        let mut success = 0;
        let mut skipped = 0;
        for entry in fs::read_dir(data_path).expect("Error reading test_data directory.") {
            let cur_entry = entry.unwrap().path();

            if cur_entry.extension().unwrap() != "ppe" {
                continue;
            }

            let file_name = cur_entry.as_os_str();
            print!("File: {}…", cur_entry.file_name().unwrap().to_str().unwrap());

            if  cur_entry.file_name().unwrap().to_str().unwrap().eq("if_elseif_else_endif_end.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("f_exp_f_mw_f_reg_f_sel_f_sys.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("echodots_promptstr_mask_pwd_upcase.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("boolean.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("auto.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("dispfile_sec_graph_lang.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("newline_stmt.ppe") || // probably easy
                cur_entry.file_name().unwrap().to_str().unwrap().eq("for_next.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("fieldlen_guide.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("gosub_return.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("bell_lfafter_lfbefore.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("u_trans.ppe") || // should be function call and not a const
                cur_entry.file_name().unwrap().to_str().unwrap().eq("tokenize.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("false.ppe") || //context casting unsupported ATM
                cur_entry.file_name().unwrap().to_str().unwrap().eq("while.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("input.ppe") ||
                cur_entry.file_name().unwrap().to_str().unwrap().eq("disptext_bell_lfafter_lfbefore_logit_logitleft_newline.ppe") {
                println!("skip.");
                skipped += 1;
                continue;
            }


            let d = crate::decompiler::Decompiler::read(file_name.to_str().unwrap());
            let source_file = cur_entry.with_extension("ppl");
            let orig_text = fs::read_to_string(source_file).unwrap();

            let are_equal = is_match(&d.output, &orig_text);

            if !are_equal {
                println!(" not matched…");
                print!("{}", d.output);
                println!("-----");
                print!("{}", orig_text);
            } else {
                println!(" match.");
                success += 1;
            }

            assert!(are_equal);
        }
        println!("successful {} skipped {}", success, skipped);
    }
}
