use std::collections::HashMap;
use std::str::*;
use crate::executable::*;
use crate::tables::*;

const LAST_FUNC: u16 = 0xfee2;
const LAST_STMT: i16 = 0x00e2;

struct FuncL
{
    Label : i32,
    Func : u16
}

pub struct Decompiler
{
    executable : Executable,
    akt_stat : i16,
    akt_proc : u16,
    uvar_flag : i32,
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
    string_stack : Vec<String>
}

impl Decompiler {
    fn new(executable: Executable) -> Self {
        let mut valid_buffer = Vec::new();
        for i in 0..executable.code_size / 2 + 1 {
            valid_buffer.push(false);
        }
        Decompiler {
            executable,
            akt_stat: 0,
            akt_proc: 0,
            uvar_flag: 0,
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
            string_stack: Vec::new()
        }
    }

    pub fn read(file_name: &str) {
        let mut d = Decompiler::new(read_file(file_name));
        d.DoPass1();
        d.pass += 1;
        d.DumpVars();
        d.DoPass2();
    }

    fn FillValid(&mut self, i: i32, j: i32) {
        for n in i..j {
            self.valid_buffer[n as usize] = true;
        }
    }

    fn DoPass1(&mut self) {
        let mut prevStat = 0;
        let mut lastPoint = 0;
        let mut ifPtr = -5;

        self.akt_stat = 0;
        self.next_label = 0;
        self.next_func = 0;
        self.src_ptr = 0;

        while self.src_ptr != -1 && self.src_ptr <= (self.executable.code_size / 2) as i32 {
            if self.src_ptr >= (self.executable.code_size / 2) as i32 || self.valid_buffer[self.src_ptr as usize] {
                self.FillValid(lastPoint, self.src_ptr);
            } else {
                prevStat = self.akt_stat;
                if self.src_ptr == ifPtr { self.src_ptr += 2; }

                self.akt_stat = self.executable.source_buffer[self.src_ptr as usize];
                if self.akt_stat > LAST_STMT || (self.akt_stat as usize) < STATEMENT_SIGNATURE_TABLE.len() && STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] == 0xaa {
                    panic!("Error: Unknown statement {}.", self.akt_stat);
                }
                let mut trap = false;

                match STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] {
                    0xf6 => {
                        self.src_ptr += 2; 
                        self.akt_proc = self.executable.source_buffer[(self.src_ptr - 1) as usize] as u16 - 1;
                        self.executable.variable_declarations.get_mut(&(self.akt_proc as u16)).unwrap().flag = 1;
                        let act_dec_start = self.executable.variable_declarations[&(self.akt_proc as u16)].start as i32;
                        let act_dec_args = self.executable.variable_declarations[&(self.akt_proc as u16)].args as i32;

                        self.funcin(act_dec_start, self.akt_proc);
                         self.pushlabel(act_dec_start);
                        trap = self.getexpr(act_dec_args, 0) != 0;
                    },
                    0xf7 => {
                        if self.getexpr(0x01, 0) != 0 {
                            trap = true;
                        } else {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] as u16 - 1)).unwrap().flag = 1;
                            trap = self.getexpr(0x01, 0) != 0;
                        }
                    },
                    0xf8 => {
                        if self.getexpr(0x03, 0) != 0 {
                            trap = true;
                        } else {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] as u16 - 1)).unwrap().flag = 1;
                            self.src_ptr += 1;
                        }
                    },
                    0xf9 => {
                        self.executable.variable_declarations.get_mut(&((self.executable.source_buffer[self.src_ptr as usize + 1] - 1).try_into().unwrap())).unwrap().flag = 1;
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 2] as u16 - 1)).unwrap().flag = 1;
                        self.src_ptr += 3;
                    },
                    0xfe => {
                        self.src_ptr += 1;
                        trap = self.getexpr(self.executable.source_buffer[self.src_ptr as usize] as i32, 0) != 0;
                    },
                    0xfa => {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize + 2] as u16 - 1)).unwrap().flag = 1;
                        self.src_ptr += 2;
                        trap = self.getexpr(self.executable.source_buffer[self.src_ptr as usize - 1] as i32 - 1, 0) != 0;
                    },
                    0xfc => {
                        self.src_ptr += 1;
                        trap = self.getexpr(self.executable.source_buffer[self.src_ptr as usize] as i32 | 0xf00, 0) != 0;
                    },
                    0xfd => {
                        self.src_ptr += 1;
                        self.labelin(self.executable.source_buffer[self.src_ptr as usize] as i32);
                        self.src_ptr += 1;
                    },
                    0xff => {
                        ifPtr = self.src_ptr;
                        if self.getexpr(0x01, 0) != 0 {
                            trap = true;
                        } else {
                            ifPtr = self.setifptr(ifPtr);
                            self.src_ptr += 1;
                        }
                    },
                    _ => {
                        trap = self.getexpr((STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] as i32 & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] as i32 & 0x0f), 0) != 0;
                    }
                }
                if !trap {
                    match self.akt_stat {
                        0x0001 | // END
                        0x002a | // RETURN
                        0x00a9 | // ENDPROC
                        0x00ab | // ENDFUNC
                        0x003a => {
                            self.FillValid(lastPoint, self.src_ptr); // STOP
                            lastPoint = self.src_ptr;
                            if prevStat == 0x000b {
                                continue;
                            }
                        },
                        0x0029 => {
                            self.FillValid(lastPoint, self.src_ptr); // GOSUB
                            lastPoint = self.src_ptr;
                            self.pushlabel(self.executable.source_buffer[self.src_ptr as usize - 1] as i32);
                            continue;
                        },
                        0x0007 => {
                            self.FillValid(lastPoint, self.src_ptr); // GOTO
                            lastPoint = self.src_ptr;
                             self.pushlabel(self.executable.source_buffer[self.src_ptr as usize - 1] as i32);
                            if prevStat == 0x000b {
                                continue;
                            }
                        },
                        0x000b => {
                            self.FillValid(lastPoint, self.src_ptr); // IF
                            lastPoint = self.src_ptr;
                            self.pushlabel(self.executable.source_buffer[self.src_ptr as usize - 1] as i32);
                            continue;
                        }
                        _ => {
                            continue;
                        }
                    }
                }
            }
            self.src_ptr = self.poplabel() as i32 / 2;
            lastPoint = self.src_ptr;
        }
    }

    fn DumpVars(&mut self) {
        let mut uvar_flag =
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

        if uvar_flag && self.executable.version >= 300 {
            if !(self.executable.variable_declarations[&23].variable_type == VariableType::Integer && self.executable.variable_declarations[&23].dims[0] == 16) {
                uvar_flag = false;
            }
        }

        if self.symbol == 0 {
            println!("\n;Source Code:\n");
            println!(";------------------------------------------------------------------------------\n\n");
        }

        let mut i: u16 = 0;
        let mut cVars = 0;
        let mut cConst = 0;
        let mut cFunc = 0;
        let mut cProc = 0;

        while (i as u32) < self.executable.max_var {
            if i > 0x16 || !uvar_flag {
                if self.executable.variable_declarations.get_mut(&i).unwrap().lflag == 0 {
                    match self.executable.variable_declarations.get_mut(&i).unwrap().variable_type {
                        VariableType::Function => {
                            if self.executable.variable_declarations.get_mut(&i).unwrap().flag == 1 {
                                cFunc += 1;
                                self.executable.variable_declarations.get_mut(&i).unwrap().number = cFunc;
                                let n = &(self.executable.variable_declarations.get_mut(&i).unwrap().return_var - 1);
                                self.executable.variable_declarations.get_mut(n).unwrap().number = cFunc;
                                if self.symbol == 0 {
                                    println!("    DECLARE ");
                                    self.outputFunc(i);
                                    println!();
                                }
                            }
                        },
                        VariableType::Method => {
                            let mut curVar = self.executable.variable_declarations.get_mut(&i).unwrap();
                            if curVar.flag == 1 {
                                cProc += 1;
                                curVar.number = cProc;
                                if self.symbol == 0 {
                                    println!("    DECLARE ");
                                    self.outputProc(i);
                                    println!();
                                }
                            }
                        },
                        _ => {
                            let mut curVar = self.executable.variable_declarations.get_mut(&i).unwrap();
                            if curVar.flag == 0 {
                                cConst += 1;
                                curVar.number = cConst;
                            } else {
                                cVars += 1;
                                curVar.number = cVars;
                                if self.symbol == 0 {
                                    print!("    {}", TYPE_NAMES[curVar.variable_type as usize]);
                                    print!("  VAR{0:>03}", cVars);
                                    match curVar.dim {
                                        1 => println!("({})", curVar.dims[0]),
                                        2 => println!("({}, {})", curVar.dims[0], curVar.dims[1]),
                                        3 => println!("({}, {}, {})", curVar.dims[0], curVar.dims[1], curVar.dims[2]),
                                        _ => { }
                                    }
                                    println!();
                                }
                            }
                        }
                    }
                }
            }
            i += 1;
        }

        if self.symbol == 0 {
            println!("\n;------------------------------------------------------------------------------\n\n");
        }
    }

    fn outputFunc(&mut self, func: u16) {
        print!("FUNCTION FUNC{}(", self.executable.variable_declarations.get(&func).unwrap().number);
        let mut j = self.executable.variable_declarations.get(&func).unwrap().first_var;
        for i in 0..self.executable.variable_declarations.get(&func).unwrap().args {
            if i != 0 {
                print!(",");
            }
            print!("{}", TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize]);
            self.executable.variable_declarations.get_mut(&j).unwrap().func = func;
            print!(" LOC{}", self.executable.variable_declarations.get(&j).unwrap().number);
            j += 1;
        }
        print!(") {}", TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize]);
    }

    fn outputProc(&mut self, proc: u16) {
        print!("PROCEDURE PROC{}(", self.executable.variable_declarations.get(&proc).unwrap().number);
        let mut j = self.executable.variable_declarations.get(&proc).unwrap().first_var;
        for i in 0..self.executable.variable_declarations.get(&proc).unwrap().args {
            if i != 0 {
                print!(",");
            }
            if ((self.executable.variable_declarations.get(&proc).unwrap().return_var >> i) & 1) != 0
            {
                print!("VAR ");
            }
            print!("{}", TYPE_NAMES[self.executable.variable_declarations.get(&j).unwrap().variable_type as usize]);
            self.executable.variable_declarations.get_mut(&j).unwrap().func = proc;
            print!(" LOC{}", self.executable.variable_declarations.get(&j).unwrap().number);
            j += 1;
        }
        print!(")");
    }

    fn funcin(&mut self, label: i32, func: u16) {
        self.func_used.insert(label, FuncL
        {
            Func: func,
            Label: self.func_used.len() as i32
        });
    }

    fn funcout(&mut self, label: i32) {
        match self.func_used.get(&label) {
            Some(funcl)=>{
                let func = funcl.Func;
                if self.func_flag == 1 {
                    println!("    ENDFUNC\n");
                }

                if self.proc_flag == 1
                {
                    println!("    ENDPROC\n");
                }
                self.func_flag = 0;
                self.proc_flag = 0;

                println!();

                if self.executable.variable_declarations.get(&func).unwrap().variable_type == VariableType::Function {
                    self.outputFunc(func);
                    self.func_flag = 1;
                } else {
                    self.outputProc(func);
                    self.proc_flag = 1;
                }

                println!();

                self.DumpLocs(func);
                if self.symbol == 0 {
                    println!();
                }
            },
            _ => {}
        }
    }

    fn DumpLocs(&mut self, func : u16) {
        let mut i = 0;
        let mut mx_var = 0;
        let mut j = 0;
        // StackPtr = 0;
        if self.executable.variable_declarations.get(&func).unwrap().variable_type == VariableType::Function
        {
            i = self.executable.variable_declarations.get(&func).unwrap().return_var;
            mx_var = self.executable.variable_declarations.get(&func).unwrap().return_var + self.executable.variable_declarations.get(&func).unwrap().total_var;
        } else {
            i = self.executable.variable_declarations.get(&func).unwrap().first_var + self.executable.variable_declarations.get(&func).unwrap().args as u16;
            mx_var = self.executable.variable_declarations.get(&func).unwrap().first_var + self.executable.variable_declarations.get(&func).unwrap().total_var; //+1;
        }

        while i < mx_var
        {
            if self.executable.variable_declarations.get(&i).unwrap().flag == 1
            {
                self.executable.variable_declarations.get_mut(&i).unwrap().func = func;
                if self.symbol == 0
                {
                    println!();
                    print!("    {}", crate::tables::TYPE_NAMES[self.executable.variable_declarations.get(&i).unwrap().variable_type as usize]);
                    print!(" LOC{}", self.executable.variable_declarations.get(&i).unwrap().number);
                    match self.executable.variable_declarations[&i].dim {
                        1 => {
                            print!("({}) ", self.executable.variable_declarations.get(&i).unwrap().dims[0]);
                        },
                        2 => {
                            print!("({},{}) ", self.executable.variable_declarations.get(&i).unwrap().dims[0], self.executable.variable_declarations.get(&i).unwrap().dims[1]);
                        },
                        3 => {
                            print!("({},{},{}) ", self.executable.variable_declarations.get(&i).unwrap().dims[0], self.executable.variable_declarations.get(&i).unwrap().dims[1], self.executable.variable_declarations.get(&i).unwrap().dims[2]);
                        },
                        _  => {}
                    }
                }
            }
            i += 1;
            j += 1;
        }
        if j > 0 {
            println!();
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
                println!();
                println!(":LABEL{}", x);
            },
            _ => {}
        }
    }

    fn pushlabel(&mut self, label : i32) {
        self.label_stack.push(label.into());
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

    fn ConstName<'a>(& mut self, vars : &[i32], names : &[&str]) -> String {
        let temp_str = self.popstrip();
        let val = i32::from_str(&temp_str).unwrap();
        let mut i = 0;
        while vars[i] != -1 && vars[i] != val {
            i += 1;
        }
        if vars[i] != -1 {
            return names[i].to_string();
        }
        temp_str.to_string()
    }

    fn TransExp(&mut self, curExpr : i32) -> String {
        match self.akt_stat {
            0x00c => {
                if curExpr != 2 {
                    self.popstrip()
                } else {
                    self.ConstName(&crate::tables::CONSTANT_CONFERENCE_OFFSETS, &crate::tables::CONSTANT_CONFERENCE_NAMES)
                }
            },
            0x00d => {
                if curExpr != 2 {
                    self.popstrip()
                } else {
                    self.ConstName(&crate::tables::CONSTANT_CONFERENCE_OFFSETS, &crate::tables::CONSTANT_CONFERENCE_NAMES)
                }
            },
            0x00e => {
                if curExpr != 2 {
                    self.popstrip()
                } else {
                    self.ConstName(&crate::tables::CONSTANT_NAMES_OFFSETS, &crate::tables::CONSTANT_NAMES_DISPLAY)
                }
            },
            0x010 => {
                if curExpr == 3 {
                    self.ConstName(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if curExpr == 4 {
                    self.ConstName(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                } else {
                    self.popstrip()
                }
            },
            0x011 => {
                if curExpr == 3 {
                    self.ConstName(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if curExpr == 4 {
                    self.ConstName(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                }
                else {
                    self.popstrip()
                }
            },
            0x012 => {
                if curExpr == 3 {
                    self.ConstName(&crate::tables::CONSTANT_FACCESS_OFFSETS, &crate::tables::CONSTANT_FACCESS_NAMES)
                } else if curExpr == 4 {
                    self.ConstName(&crate::tables::CONSTANT_OPENFLAGS_OFFSETS, &crate::tables::CONSTANT_OPENFLAGS_NAMES)
                } else {
                    self.popstrip()
                }
            },
            0x018 => {
                self.ConstName(&crate::tables::CONSTANT_LINECOUNT_OFFSETS, &crate::tables::CONSTANT_LINECOUNT_NAMES)
            },
            0x022 => {
                if curExpr != 6 {
                    self.popstrip()
                } else {
                    self.ConstName(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                }
            },
            0x02b => {
                if curExpr != 5 {
                    self.popstrip()
                } else {
                    self.ConstName(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                }
            },
            0x039 => {
                if curExpr != 2 {
                    self.popstrip()
                }
                else  {
                    self.ConstName(&crate::tables::CONSTANT_1_OFFSETS, &crate::tables::CONSTANT_1_NAMES)
                }
            },
            0x070 => {
                if curExpr != 3 {
                    self.popstrip()
                }
                else {
                    self.ConstName(&crate::tables::CONSTANT_SEEK_OFFSETS, &crate::tables::CONSTANT_SEEK_NAMES)
                }
            },
            0x0cd | 0x0ce => {
                if curExpr != 1 {
                    self.popstrip()
                }
                else  {
                    self.ConstName(&crate::tables::CONSTANT_2_OFFSETS, &crate::tables::CONSTANT_2_NAMES)
                }
            },
            _ => {
                self.popstrip()
            }
        }
    }

    fn varout(&mut self, var_nr : u16) -> String
    {
        let mut varNr = var_nr - 1;

        if (self.executable.version < 300 && varNr < 0x17 && self.uvar_flag == 1) || (self.executable.version >= 300 && varNr < 0x18 && self.uvar_flag == 1) {
            return String::from(USER_VARIABLE_NAMES[varNr as usize + 1]);
        }
        
        if self.executable.variable_declarations.get(&varNr).unwrap().flag == 1
        {
            match self.executable.variable_declarations.get(&varNr).unwrap().variable_type {
                VariableType::Function => {
                    return format!("FUNC{}", self.executable.variable_declarations.get(&varNr).unwrap().number);
                },
                VariableType::Method => {
                    return format!("PROC{}", self.executable.variable_declarations.get(&varNr).unwrap().number);
                },
                _ => {
                    if self.executable.variable_declarations.get(&varNr).unwrap().fflag == 1 {
                        return format!("FUNC{}", self.executable.variable_declarations.get(&varNr).unwrap().number);
                    }
                    if self.executable.variable_declarations.get(&varNr).unwrap().lflag == 1 {
                        return format!("LOC{}", self.executable.variable_declarations.get(&varNr).unwrap().number);
                    }
                    return format!("VAR{0:>03}", self.executable.variable_declarations.get(&varNr).unwrap().number);

                }
            }
        }

        match self.executable.variable_declarations.get(&varNr).unwrap().variable_type {
            VariableType::Boolean => {
                if self.executable.variable_declarations.get(&varNr).unwrap().content != 0 {
                    String::from("TRUE")
                } else {
                    String::from("FALSE")
                }
            },
            VariableType::Date => {
                return self.executable.variable_declarations.get(&varNr).unwrap().content.to_string();
            },
            VariableType::Money => {
                return format!("${}", (self.executable.variable_declarations.get(&varNr).unwrap().content as f32) / 10.0);
            },
            VariableType::String => {
                return format!("\"{}\"", self.executable.variable_declarations.get(&varNr).unwrap().string_value);
            },
            VariableType::Time => {
                return self.executable.variable_declarations.get(&varNr).unwrap().content.to_string();
            },
            VariableType::Double => {
                return (self.executable.variable_declarations.get(&varNr).unwrap().content as f64).to_string();
            },
            _ =>{
                return (self.executable.variable_declarations.get(&varNr).unwrap().content).to_string();
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
                if self.exp_count < FUNCTION_SIGNATURE_TABLE[func as usize] as i32 {
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
        let mut tmp_func = 0;
        let mut temp_expr  = self.exp_count;

        if self.pass == 1 {
            let tmp = self.popstr();
            self.pushstr(format!("{}(", tmp));
        }

        loop {
            cur_dim += 1;
            if cur_dim == dims {
                break;
            }
            self.exp_count = 0; tmp_func = 0;
            self.src_ptr += 1;
            if cur_dim != 1 && self.pass == 1 {
                let tmp = self.popstr();
                self.pushstr(format!("{},", tmp));
            }

            while (self.src_ptr as usize) < self.executable.source_buffer.len() && self.executable.source_buffer[self.src_ptr as usize] != 0 {
                if self.executable.source_buffer[self.src_ptr as usize] as u32 <= self.executable.max_var {
                    if self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] as u16 - 1)).unwrap().variable_type == VariableType::Function {
                         self.pushlabel(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] as u16 - 1)).unwrap().start as i32);
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] as u16 - 1)).unwrap().flag = 1;
                        self.funcin(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] as u16 - 1)).unwrap().start as i32, self.executable.source_buffer[self.src_ptr as usize] as u16 - 1);
                        if self.pass == 1
                        {
                            let tmp2 = self.varout(self.executable.source_buffer[self.src_ptr as usize] as u16);
                            let tmp3 = self.popstr();
                            self.pushstr(format!("{}{}(", tmp3, tmp2));
                        }
                        self.src_ptr += 1;
                        if self.getexpr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] as u16 - 1)).unwrap().args as i32, 1) != 0 {
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
                            let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize] as u16);
                            self.pushstr(tmp);
                        }
                        self.src_ptr += 1;
                        if self.executable.source_buffer[self.src_ptr as usize] != 0
                        {
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize - 1] as u16 - 1)).unwrap().flag = 1;
                            if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize] as i32) != 0 {
                                return 1;
                            }
                        }
                    }
                }
                else
                {
                    let x = -self.executable.source_buffer[self.src_ptr as usize] - 1;
                    if self.executable.source_buffer[self.src_ptr as usize] < LAST_FUNC as i16 || (x >= 0 && FUNCTION_SIGNATURE_TABLE[x as usize]  == 0xaa) {
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

    fn getexpr(&mut self, maxExpr : i32, rec : i32) -> i32
    {
        let mut cur_expr = 0;
        let mut i = 0;
        let temp_expr = self.exp_count;

        self.src_ptr += 1;
        while (maxExpr & 0x0ff) != cur_expr {
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
                if self.executable.source_buffer[self.src_ptr as usize] >= 0 && self.executable.source_buffer[self.src_ptr as usize] as u32 <= self.executable.max_var {
                    if maxExpr / 256 == cur_expr || maxExpr / 256 == 0x0f {
                        self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] as u16 - 1)).unwrap().flag = 1;
                        if self.pass == 1 {
                            let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize] as u16);
                            self.pushstr(tmp);
                        }
                        self.src_ptr += 1;
                        if self.executable.source_buffer[self.src_ptr as usize] != 0
                        {
                            if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize] as i32) != 0 {
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
                                self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] as u16 - 1)).unwrap().flag = 1;
                            }
                        }
                        if self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] as u16 - 1)).unwrap().variable_type == VariableType::Function {
                            self.pushlabel(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] as u16  - 1)).unwrap().start as i32);
                            self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize] as u16  - 1)).unwrap().flag = 1;
                            self.funcin(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize] as u16  - 1)).unwrap().start as i32, self.executable.source_buffer[self.src_ptr as usize] as u16 - 1);
                            if self.pass == 1  {
                                let tmp2 = self.varout(self.executable.source_buffer[self.src_ptr as usize] as u16);
                                let tmp3 = self.popstr();
                                self.pushstr(format!("{}{}(", tmp3, tmp2));
                            }
                            self.src_ptr += 1;
                            if self.getexpr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] as u16 - 1)).unwrap().args as i32, 1) != 0 {
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
                                let tmp = self.varout(self.executable.source_buffer[self.src_ptr as usize] as u16);
                                self.pushstr(tmp);
                            }
                            self.src_ptr += 1;
                            if self.executable.source_buffer[self.src_ptr as usize] != 0 {
                                self.executable.variable_declarations.get_mut(&(self.executable.source_buffer[self.src_ptr as usize - 1] as u16 - 1)).unwrap().flag = 1;
                                if self.dimexpr(self.executable.source_buffer[self.src_ptr as usize] as i32) != 0 {
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
                    if self.executable.source_buffer[self.src_ptr as usize] < LAST_FUNC as i16 || FUNCTION_SIGNATURE_TABLE[(-self.executable.source_buffer[self.src_ptr as usize] - 1) as usize] == 0xaa {
                        panic!("Error: Unknown function {}", self.executable.source_buffer[self.src_ptr as usize]);
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
                let tmp2 = self.TransExp(cur_expr);
                let tmp3 = self.popstr();
                self.pushstr(format!("{}{}", tmp3, tmp2));
            }
        }
        self.exp_count = temp_expr + 1;
        return 0;
    }

    fn setifptr(&mut self, i : i32) -> i32
    {
        if self.executable.source_buffer[self.executable.source_buffer[self.src_ptr as usize] as usize / 2 - 2] == 0x0007 && self.executable.source_buffer[self.executable.source_buffer[self.src_ptr as usize] as usize / 2 - 1] as i32 / 2 == i {
            self.executable.source_buffer[i as usize] = 0;
            ((self.executable.source_buffer[self.src_ptr as usize] / 2 - 2) as usize) as i32
        } else {
            i - 5
        }
    }

    fn DoPass2(&mut self)
    {
        let mut prevStat = 0;

        self.akt_stat = -1;
        self.next_label = 0;
        self.next_func = 0;
        self.src_ptr = 0;
        self.trash_flag = 0;
        let mut ifPtr = -1;

        while self.src_ptr < (self.executable.code_size / 2).try_into().unwrap() {
            prevStat = self.akt_stat;
            if self.src_ptr == ifPtr {
                self.src_ptr += 2;
            }

            self.funcout((self.src_ptr * 2).try_into().unwrap());
            self.labelout((self.src_ptr * 2).try_into().unwrap());
            if !self.valid_buffer[self.src_ptr as usize] {
                self.src_ptr += 1;
                continue;
            }

            self.akt_stat = self.executable.source_buffer[self.src_ptr as usize];
           // println!(SrcPtr +":"+ AktStat);
            if self.akt_stat > LAST_STMT || STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] == 0xaa {
                panic!("Error: Unknown statement {}, aborting…", self.akt_stat);
            }

            if prevStat < 0 || STATEMENT_SIGNATURE_TABLE[prevStat as usize] != 0xff {
                print!("    ");
            }

            print!("{}", STATEMENT_NAMES[self.akt_stat as usize]);

            if self.akt_stat != 0xa8 {
                print!(" ");
            }

            match STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] {
                0xf6 => {
                    self.src_ptr += 2;
                    self.akt_proc = self.executable.source_buffer[self.src_ptr as usize - 1] as u16 - 1;
                    print!("{}(", self.executable.variable_declarations.get(&self.akt_proc).unwrap().number);
                    if self.getexpr(self.executable.variable_declarations.get(&(self.executable.source_buffer[self.src_ptr as usize - 1] as u16 - 1)).unwrap().args.into(), 0) != 0 {
                        return;
                    }
                    print!("{}", self.popstr());
                    print!(")");
                },
                0xf7 => {
                    if self.getexpr(0x01, 0) != 0 {
                        return;
                    }
                    print!(",{},", self.varout(*self.executable.source_buffer.get(self.src_ptr as usize).unwrap() as u16));
                    if self.getexpr(0x01, 0) != 0 {
                        return;
                    }
                    print!("{}", self.popstr());
                },
                0xf8 => {
                    if self.getexpr(0x03, 0) != 0 {
                        return;
                    }
                    print!("{}", self.popstr());
                    print!(",{}", self.varout(self.executable.source_buffer[self.src_ptr as usize] as u16));
                    self.src_ptr += 1;
                },
                0xf9 => {
                    print!("{}", self.varout(self.executable.source_buffer[self.src_ptr as usize + 1] as u16));
                    print!(",");
                    print!("{}", self.varout(self.executable.source_buffer[self.src_ptr as usize + 2] as u16));
                    self.src_ptr += 3;
                },
                0xfe => {
                    self.src_ptr += 1;
                    if self.getexpr(self.executable.source_buffer[self.src_ptr as usize] as i32, 0) != 0 {
                        return;
                    }
                    print!("{}", self.popstr());
                },
                0xfa => {
                    print!("{},", self.varout(self.executable.source_buffer[self.src_ptr as usize + 2] as u16));
                    self.src_ptr += 2;
                    if self.getexpr(self.executable.source_buffer[self.src_ptr as usize - 1] as i32 - 1, 0) != 0 {
                        return;
                    }
                    print!("{}", self.popstr());
                },
                0xfc => {
                    self.src_ptr += 1;
                    if self.getexpr(self.executable.source_buffer[self.src_ptr as usize] as i32 | 0xf00, 0) != 0 {
                        return;
                    }
                    print!("{}", self.popstr());
                },
                0xfd => {
                    self.src_ptr += 1;
                    print!("LABEL{}", self.labelnr(self.executable.source_buffer[self.src_ptr as usize] as i32));
                    self.src_ptr += 1;
                },
                0xff => {
                    ifPtr = self.src_ptr;
                    print!("(");
                    if self.getexpr(0x001, 0) != 0 {
                        return;
                    }
                    print!("{}) ", self.popstr());
                    ifPtr = self.setifptr(ifPtr);
                    self.src_ptr += 1;
                },
                _ => {
                    if self.getexpr(((STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] & 0xf0) as i32 * 16 + (STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] & 0x0f) as i32).into(), 0) != 0 {
                        return;
                    }
                    print!("{}", self.popstr());
                }
            }

            if STATEMENT_SIGNATURE_TABLE[self.akt_stat as usize] != 0xff {
                if self.trash_flag != 0 {
                    print!("   ; PPLC bug detected");
                    self.trash_flag = 0;
                }
                println!();
            }

            match self.akt_stat {
                0x0001 | // END
                0x002a | // RETURN
                0x003a => { // STOP
                    println!();
                },
                0x00a9 | // ENDPROC
                0x00ab | // ENDFUNC
                0x003a => { //STOP
                    self.func_flag = 0;
                    self.proc_flag = 0;
                    println!();
                },
                _ => {}
            }

        }
        self.labelout((self.src_ptr * 2).try_into().unwrap());
    }
}


#[cfg(test)]
mod tests {
    use std::env;

    #[test]
    fn test_decompiler() {
        use std::fs::{self, DirEntry};
        use std::path::Path;

        let mut data_path =env::current_dir().unwrap();
        data_path.push("test_data");

        for entry in fs::read_dir(data_path).expect("Error reading test_data directory.") {
            let curEntry = entry.unwrap().path();

            if curEntry.extension().unwrap() != "ppe" {
                continue;
            }

            let fileName = curEntry.as_os_str();
            crate::decompiler::Decompiler::read(fileName.to_str().unwrap());
        }
    }
}
