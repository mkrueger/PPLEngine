use std::collections::HashMap;
use std::string::String;

pub mod expressions;
use thiserror::Error;

use crate::ast::convert_to;
use crate::ast::Block;
use crate::ast::Constant;
use crate::ast::Declaration;
use crate::ast::Expression;
use crate::ast::Program;
use crate::ast::ProgramContext;
use crate::ast::Statement;
use crate::ast::VarInfo;
use crate::ast::VariableType;
use crate::ast::VariableValue;
use crate::icy_board::data::IcyBoardData;
use crate::icy_board::data::Node;
use crate::icy_board::data::UserRecord;
use crate::tables::OpCode;
use crate::tables::PPL_TRUE;
use crate::Res;

pub use self::expressions::*;

pub mod statements;
pub use self::statements::*;

pub mod io;
pub use self::io::*;

pub mod errors;
mod tests;

#[derive(Error, Debug, Clone, Copy)]
pub enum InterpreterError {
    #[error("unsupported constant: {0}")]
    UnsupportedConst(&'static str),
    #[error("unsupported opcode: {0}")]
    UnsupportedOpCode(OpCode),
}

#[derive(Clone, Copy)]
pub enum TerminalTarget {
    Both,
    User,
    Sysop,
}

#[derive(Clone, Copy)]
pub enum HangupType {
    Hangup,
    Bye,
    Goodbye,
}

pub trait ExecutionContext {
    fn has_sysop(&self) -> bool;

    /// .
    ///
    /// # Errors
    /// Errors if the variable is not found.
    fn gotoxy(&mut self, target: TerminalTarget, x: i32, y: i32) -> Res<()>;

    /// .
    ///
    /// # Errors
    /// Errors if the variable is not found.
    fn print(&mut self, target: TerminalTarget, str: &str) -> Res<()>;

    /// .
    ///
    /// # Errors
    /// Errors if the variable is not found.
    fn write_raw(&mut self, target: TerminalTarget, data: &[u8]) -> Res<()>;

    /// .
    ///
    /// # Errors
    /// Errors if the variable is not found.
    fn read(&mut self) -> Res<String>;

    /// .
    ///
    /// # Errors
    /// Errors if the variable is not found.
    fn get_char(&mut self) -> Res<Option<char>>;

    fn inbytes(&mut self) -> i32;

    fn set_color(&mut self, color: u8);

    fn get_caret_position(&mut self) -> (i32, i32);

    /// simulate user input for later processing
    /// # Errors
    /// Errors if the variable is not found.
    fn send_to_com(&mut self, data: &str) -> Res<()>;

    /// .
    /// # Errors
    /// Errors if the variable is not found.
    fn hangup(&mut self, hangup_type: HangupType) -> Res<()>;
}

pub struct StackFrame {
    values: HashMap<String, VariableValue>,

    gosub_stack: Vec<usize>,
    cur_ptr: usize,
    label_table: HashMap<String, usize>,
}

pub fn calc_table(blk: &Block) -> HashMap<String, usize> {
    let mut res = HashMap::new();
    for i in 0..blk.statements.len() {
        if let Statement::Label(label) = &blk.statements[i] {
            res.insert(label.clone(), i);
        }
    }
    res
}

pub struct Interpreter<'a> {
    prg: &'a Program,
    ctx: &'a mut dyn ExecutionContext,
    // lookup: HashMap<&'a Block, i32>,
    cur_frame: Vec<StackFrame>,
    io: &'a mut dyn PCBoardIO,
    pub is_running: bool,

    pub icy_board_data: IcyBoardData,
    pub cur_user: usize,
    pub current_user: Option<UserRecord>,
    pub pcb_node: Option<Node>,

    pub cur_tokens: Vec<String>, //  stack_frames: Vec<StackFrame>
}

impl<'a> Interpreter<'a> {
    fn set_user_variables(&mut self, cur_user: &UserRecord) {
        self.cur_frame[0].values.insert(
            "self".to_string(),
            VariableValue::Integer(cur_user.page_len),
        );
        self.cur_frame[0].values.insert(
            "U_PWD".to_string(),
            VariableValue::String(cur_user.password.clone()),
        );
        self.cur_frame[0].values.insert(
            "U_PWDEXP".to_string(),
            VariableValue::Date(0000), // TODO
        );
        self.cur_frame[0].values.insert(
            "U_SCROLL".to_string(),
            VariableValue::Boolean(cur_user.scroll_flag),
        );
        self.cur_frame[0].values.insert(
            "U_SEC".to_string(),
            VariableValue::Integer(cur_user.security_level),
        );
        self.cur_frame[0].values.insert(
            "U_CITY".to_string(),
            VariableValue::String(cur_user.city.clone()),
        );
        self.cur_frame[0].values.insert(
            "U_ADDR".to_string(),
            VariableValue::Dim1(
                VariableType::String,
                vec![
                    VariableValue::String("Address Line 1".to_string()),
                    VariableValue::String("Address Line 2".to_string()),
                    VariableValue::String(cur_user.city.clone()),
                    VariableValue::String("State".to_string()),
                    VariableValue::String("ZIP Code".to_string()),
                    VariableValue::String("Country".to_string()),
                ],
            ),
        );
    }
}

/// .
///
/// # Panics
///
/// Panics if .
/// # Errors
/// Errors if the variable is not found.
pub fn create_array(
    interpreter: &mut Interpreter,
    var_type: VariableType,
    var_info: &VarInfo,
) -> Res<VariableValue> {
    match var_info {
        VarInfo::Var0(_) => panic!(""),
        VarInfo::Var1(_, vec) => {
            let dim = get_int(&evaluate_exp(interpreter, vec)?)?;
            let mut v = Vec::new();
            v.resize(dim as usize, VariableValue::Integer(0));
            Ok(VariableValue::Dim1(var_type, v))
        }
        VarInfo::Var2(_, _, _) => Ok(VariableValue::Dim2(var_type, Vec::new())),
        VarInfo::Var3(_, _, _, _) => Ok(VariableValue::Dim3(var_type, Vec::new())),
    }
}

/// .
///
/// # Panics
///
/// Panics if .
/// # Errors
/// Errors if the variable is not found.
pub fn set_array_value(
    arr: &mut VariableValue,
    val: VariableValue,
    dim1: usize,
    dim2: usize,
    dim3: usize,
) {
    match arr {
        VariableValue::Dim1(_, data) => {
            data[dim1] = val;
        }
        VariableValue::Dim2(_, data) => {
            data[dim2][dim1] = val;
        }
        VariableValue::Dim3(_, data) => {
            data[dim3][dim2][dim1] = val;
        }
        _ => panic!("no array variable: {arr}"),
    }
}

/// .
///
/// # Panics
///
/// Panics if .
/// # Errors
/// Errors if the variable is not found.
pub fn get_first_index(var_info: &VarInfo) -> &Expression {
    if let VarInfo::Var1(_, v) | VarInfo::Var2(_, v, _) | VarInfo::Var3(_, v, _, _) = var_info {
        v
    } else {
        panic!("")
    }
}

/// .
///
/// # Panics
///
/// Panics if .
/// # Errors
/// Errors if the variable is not found.
pub fn get_second_index(var_info: &VarInfo) -> &Expression {
    if let VarInfo::Var1(_, v) | VarInfo::Var2(_, _, v) | VarInfo::Var3(_, _, v, _) = var_info {
        v
    } else {
        panic!("")
    }
}

/// .
///
/// # Panics
///
/// Panics if .
/// # Errors
/// Errors if the variable is not found.
pub fn get_third_index(var_info: &VarInfo) -> &Expression {
    if let VarInfo::Var1(_, v) | VarInfo::Var2(_, _, v) | VarInfo::Var3(_, _, _, v) = var_info {
        v
    } else {
        panic!("")
    }
}

fn execute_statement(interpreter: &mut Interpreter, stmt: &Statement) -> Res<()> {
    match stmt {
        Statement::Let(variable, expr) => {
            let value: VariableValue = evaluate_exp(interpreter, expr)?;
            let var_name = variable.get_name().clone();
            let var_type = interpreter.prg.get_var_type(&var_name);

            if let Some(var_info) = interpreter.prg.get_var_info(&var_name) {
                if var_info.is_array() {
                    let dim1 = &evaluate_exp(interpreter, get_first_index(variable))?;
                    let dim2 = &evaluate_exp(interpreter, get_second_index(variable))?;
                    let dim3 = &evaluate_exp(interpreter, get_third_index(variable))?;
                    let val = match interpreter
                        .cur_frame
                        .last_mut()
                        .unwrap()
                        .values
                        .get_mut(&var_name)
                    {
                        Some(val) => {
                            // println!("found in local");
                            val
                        }
                        None => {
                            if let Some(val) = interpreter
                                .cur_frame
                                .first_mut()
                                .unwrap()
                                .values
                                .get_mut(&var_name)
                            {
                                // println!("found in global");
                                val
                            } else {
                                let arr = create_array(interpreter, var_type, var_info)?;
                                interpreter
                                    .cur_frame
                                    .last_mut()
                                    .unwrap()
                                    .values
                                    .insert(var_name.clone(), arr);
                                interpreter
                                    .cur_frame
                                    .last_mut()
                                    .unwrap()
                                    .values
                                    .get_mut(&var_name)
                                    .unwrap()
                            }
                        }
                    };
                    set_array_value(
                        val,
                        value,
                        get_int(dim1)? as usize - 1,
                        get_int(dim2)? as usize - 1,
                        get_int(dim3)? as usize - 1,
                    );
                } else {
                    interpreter
                        .cur_frame
                        .last_mut()
                        .unwrap()
                        .values
                        .insert(var_name, convert_to(var_type, &value));
                }
            } else {
                interpreter
                    .cur_frame
                    .last_mut()
                    .unwrap()
                    .values
                    .insert(var_name, value);
            }
        }
        Statement::Goto(label) => {
            if let Some(frame) = interpreter.cur_frame.last_mut() {
                let Some(label_ptr) = frame.label_table.get(label) else {
                    panic!("label not found {label}");
                };
                frame.cur_ptr = *label_ptr;
            }
        }

        Statement::Gosub(label) => {
            if let Some(frame) = interpreter.cur_frame.last_mut() {
                let Some(label_ptr) = frame.label_table.get(label) else {
                    panic!("label not found {label}");
                };
                frame.gosub_stack.push(frame.cur_ptr);
                frame.cur_ptr = *label_ptr;
            }
        }
        Statement::Return => {
            //let table = &interpreter.label_tables[interpreter.cur_frame.last().unwrap().label_table as usize];
            interpreter.cur_frame.last_mut().unwrap().cur_ptr = interpreter
                .cur_frame
                .last_mut()
                .unwrap()
                .gosub_stack
                .pop()
                .unwrap();
        }

        Statement::Call(def, params) => {
            call_predefined_procedure(interpreter, def, params)?;
        }
        Statement::ProcedureCall(name, parameters) => {
            let mut found = false;
            for f in &interpreter.prg.procedure_implementations {
                if let Declaration::Procedure(pname, params) = &f.declaration {
                    if name != pname {
                        continue;
                    }
                    let label_table = calc_table(&f.block);
                    let mut prg_frame = StackFrame {
                        values: HashMap::new(),
                        gosub_stack: Vec::new(),
                        cur_ptr: 0,
                        label_table,
                    };

                    for i in 0..parameters.len() {
                        if let Declaration::Variable(var_type, infos) = &params[i] {
                            let value = evaluate_exp(interpreter, &parameters[i])?;
                            prg_frame
                                .values
                                .insert(infos[0].get_name().clone(), convert_to(*var_type, &value));
                        } else {
                            panic!("invalid parameter declaration {:?}", params[i]);
                        }
                    }
                    interpreter.cur_frame.push(prg_frame);

                    while interpreter.cur_frame.last().unwrap().cur_ptr < f.block.statements.len() {
                        let stmt =
                            &f.block.statements[interpreter.cur_frame.last().unwrap().cur_ptr];
                        execute_statement(interpreter, stmt)?;
                        interpreter.cur_frame.last_mut().unwrap().cur_ptr += 1;
                    }
                    interpreter.cur_frame.pop();
                    found = true;
                    break;
                }
            }
            assert!(found, "procedure not found {name}");
        }

        Statement::End => {
            interpreter.cur_frame.last_mut().unwrap().cur_ptr = usize::MAX - 1;
        }
        Statement::If(cond, statement) => {
            let value = evaluate_exp(interpreter, cond)?;
            if let VariableValue::Integer(x) = value {
                if x == PPL_TRUE {
                    execute_statement(interpreter, statement)?;
                }
            } else if let VariableValue::Boolean(x) = value {
                if x {
                    execute_statement(interpreter, statement)?;
                }
            } else {
                panic!("no bool value {value:?}");
            }
        }

        /* unsupported - the compiler does not generate them and ast transformation should remove them */
        Statement::Continue => {
            panic!("unsupported statement Continue")
        }
        Statement::Break => {
            panic!("unsupported statement Break")
        }
        Statement::For(_, _, _, _, _) => {
            panic!("unsupported statement For")
        }
        Statement::DoWhile(_, _) => {
            panic!("unsupported statement DoWhile")
        }
        Statement::IfThen(_, _, _, _) => {
            panic!("unsupported statement IfThen")
        }
        Statement::Block(_) => {
            panic!("unsupported statement Begin…End block")
        }

        // nop statements
        Statement::Label(_) | Statement::Comment(_) => { /* skip */ }

        _ => {
            panic!("unsupported statement {stmt:?}");
        }
    }
    Ok(())
}

/// .
///
/// # Panics
///
/// Panics if .
/// # Errors
pub fn run(
    prg: &Program,
    ctx: &mut dyn ExecutionContext,
    io: &mut dyn PCBoardIO,
    icy_board_data: IcyBoardData,
) -> Res<bool> {
    let label_table = calc_table(&prg.main_block);
    let mut cur_frame = StackFrame {
        values: HashMap::new(),
        gosub_stack: Vec::new(),
        cur_ptr: 0,
        label_table,
    };

    for decl in &prg.declarations {
        if let Declaration::Variable(var_type, name) = decl {
            let var = var_type.create_empty_value();
            for name in name {
                match name {
                    VarInfo::Var0(name) => {
                        cur_frame.values.insert(name.clone(), var.clone());
                    }
                    VarInfo::Var1(name, expression) => {
                        let dim1 = evaluate_constant_int(expression) as usize;
                        cur_frame.values.insert(
                            name.clone(),
                            VariableValue::Dim1(*var_type, vec![var.clone(); dim1]),
                        );
                    }
                    VarInfo::Var2(name, expression1, expression2) => {
                        let dim1 = evaluate_constant_int(expression1) as usize;
                        let dim2 = evaluate_constant_int(expression2) as usize;
                        cur_frame.values.insert(
                            name.clone(),
                            VariableValue::Dim2(*var_type, vec![vec![var.clone(); dim1]; dim2]),
                        );
                    }
                    VarInfo::Var3(name, expression1, expression2, expression3) => {
                        let dim1 = evaluate_constant_int(expression1) as usize;
                        let dim2 = evaluate_constant_int(expression2) as usize;
                        let dim3 = evaluate_constant_int(expression3) as usize;
                        cur_frame.values.insert(
                            name.clone(),
                            VariableValue::Dim3(
                                *var_type,
                                vec![vec![vec![var.clone(); dim1]; dim2]; dim3],
                            ),
                        );
                    }
                }
            }
        }
    }

    let mut interpreter = Interpreter {
        prg,
        ctx,
        // lookup: HashMap::new(),
        cur_frame: vec![cur_frame],
        io,
        is_running: true,
        cur_tokens: Vec::new(),
        icy_board_data,
        cur_user: 0,
        current_user: None,
        pcb_node: None,
        //  stack_frames: vec![]
    };
    interpreter.set_user_variables(&UserRecord::default());

    while interpreter.is_running
        && interpreter.cur_frame.last().unwrap().cur_ptr < prg.main_block.statements.len()
    {
        let stmt = &prg.main_block.statements[interpreter.cur_frame.last().unwrap().cur_ptr];
        match execute_statement(&mut interpreter, stmt) {
            Ok(()) => {}
            Err(err) => {
                println!("error executing {stmt:?} : {err}");
                break;
            }
        }

        interpreter.cur_frame.last_mut().unwrap().cur_ptr += 1;
    }
    Ok(true)
}

fn evaluate_constant_int(expression: &Expression) -> i32 {
    match expression {
        Expression::Const(c) => match c.get_constant_value() {
            Constant::Integer(x) => *x,
            _ => panic!("expected constant integer"),
        },
        _ => panic!("expected constant integer"),
    }
}

pub mod constants {
    pub const AUTO: i32 = 0x2000;
    pub const BELL: i32 = 0x0800;
    pub const DEFS: i32 = 0x0000;
    pub const ECHODOTS: i32 = 0x0001;
    pub const ERASELINE: i32 = 0x0020;
    pub const FIELDLEN: i32 = 0x0002;
    pub const GUIDE: i32 = 0x0004;
    pub const HIGHASCII: i32 = 0x1000;
    pub const LFAFTER: i32 = 0x0100;
    pub const LFBEFORE: i32 = 0x0080;
    pub const LOGIT: i32 = 0x8000;
    pub const LOGITLEFT: i32 = 0x10000;
    pub const NEWLINE: i32 = 0x0040;
    pub const NOCLEAR: i32 = 0x0400;
    pub const STACKED: i32 = 0x0010;
    pub const UPCASE: i32 = 0x0008;
    pub const WORDWRAP: i32 = 0x0200;
    pub const YESNO: i32 = 0x4000;
}
