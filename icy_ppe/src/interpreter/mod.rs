use std::collections::HashMap;
use std::os::linux::raw::stat;
use std::string::String;

pub mod expressions;
use thiserror::Error;

use crate::ast::convert_to;
use crate::ast::AstNode;
use crate::ast::Expression;
use crate::ast::Program;
use crate::ast::Statement;
use crate::ast::Variable;
use crate::ast::VariableData;
use crate::ast::VariableType;
use crate::ast::VariableValue;
use crate::icy_board::data::IcyBoardData;
use crate::icy_board::data::Node;
use crate::icy_board::data::UserRecord;
use crate::tables::OpCode;
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

    fn get_bps(&self) -> i32 {
        115_200
    }

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
    pub values: HashMap<unicase::Ascii<String>, Variable>,
    pub gosub_stack: Vec<usize>,
    pub cur_ptr: usize,
    pub label_table: HashMap<unicase::Ascii<String>, usize>,
}

pub fn calc_stmt_table(blk: &[Statement]) -> HashMap<unicase::Ascii<String>, usize> {
    let mut res = HashMap::new();
    for (i, stmt) in blk.iter().enumerate() {
        if let Statement::Label(label) = stmt {
            res.insert(label.get_label().clone(), i);
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
            unicase::Ascii::new("self".to_string()),
            Variable::new(
                VariableType::Integer,
                VariableData {
                    int_value: cur_user.page_len,
                },
            ),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_PWD".to_string()),
            Variable::new_string(cur_user.password.clone()),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_PWDEXP".to_string()),
            Variable::new(
                VariableType::Date,
                VariableData {
                    int_value: cur_user.security_level,
                },
            ),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_SCROLL".to_string()),
            Variable::new_bool(cur_user.scroll_flag),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_SEC".to_string()),
            Variable::new(
                VariableType::Integer,
                VariableData {
                    int_value: cur_user.security_level,
                },
            ),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_CITY".to_string()),
            Variable::new_string(cur_user.city.clone()),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_ADDR".to_string()),
            Variable::new_vector(
                VariableType::String,
                vec![
                    Variable::new_string("Address Line 1".to_string()),
                    Variable::new_string("Address Line 2".to_string()),
                    Variable::new_string(cur_user.city.clone()),
                    Variable::new_string("State".to_string()),
                    Variable::new_string("ZIP Code".to_string()),
                    Variable::new_string("Country".to_string()),
                ],
            ),
        );
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_variable(&self, var_name: &unicase::Ascii<String>) -> Option<&Variable> {
        if self.cur_frame.len() > 1 {
            let last = self.cur_frame.last().unwrap();
            if let Some(val) = last.values.get(var_name) {
                return Some(val);
            }
        }
        self.cur_frame.first().unwrap().values.get(var_name)
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_variable_mut(&mut self, var_name: &unicase::Ascii<String>) -> Option<&mut Variable> {
        let cf = &mut self.cur_frame;
        if cf.len() > 1 && cf.last().unwrap().values.contains_key(var_name) {
            if let Some(last) = cf.last_mut() {
                if let Some(val) = last.values.get_mut(var_name) {
                    return Some(val);
                }
            }
        } else if let Some(first) = cf.first_mut() {
            return first.values.get_mut(var_name);
        }
        None
    }

    fn set_default_variables(&mut self) {
        self.add_predefined_variable("U_EXPERT", Variable::new_bool(false));
        self.add_predefined_variable("U_FSE", Variable::new_bool(false));
        self.add_predefined_variable("U_FSEP", Variable::new_bool(false));
        self.add_predefined_variable("U_CLS", Variable::new_bool(false));
        self.add_predefined_variable(
            "U_EXPDATE",
            Variable::new(VariableType::Date, VariableData::default()),
        );
        self.add_predefined_variable("U_SEC", Variable::new_int(0));
        self.add_predefined_variable("U_PAGELEN", Variable::new_int(0));
        self.add_predefined_variable("U_EXPSEC", Variable::new_int(0));
        self.add_predefined_variable("U_CITY", Variable::new_string(String::new()));
        self.add_predefined_variable("U_BDPHONE", Variable::new_string(String::new()));
        self.add_predefined_variable("U_HVPHONE", Variable::new_string(String::new()));
        self.add_predefined_variable("U_TRANS", Variable::new_string(String::new()));
        self.add_predefined_variable("U_CMNT1", Variable::new_string(String::new()));
        self.add_predefined_variable("U_CMNT2", Variable::new_string(String::new()));
        self.add_predefined_variable("U_PWD", Variable::new_string(String::new()));
        self.add_predefined_variable("U_SCROLL", Variable::new_bool(false));
        self.add_predefined_variable("U_LONGHDR", Variable::new_bool(false));
        self.add_predefined_variable("U_DEF79", Variable::new_bool(false));

        self.add_predefined_variable("U_VER", Variable::new_string(String::new()));
        self.add_predefined_variable(
            "U_ADDR",
            Variable::new_vector(
                VariableType::String,
                vec![Variable::new_string(String::new()); 5],
            ),
        );
        self.add_predefined_variable(
            "U_NOTES",
            Variable::new_vector(
                VariableType::String,
                vec![Variable::new_string(String::new()); 4],
            ),
        );

        self.add_predefined_variable(
            "U_PWDEXP",
            Variable::new(VariableType::Date, VariableData::default()),
        );

        self.add_predefined_variable(
            "U_ACCOUNT",
            Variable::new_vector(VariableType::Integer, vec![Variable::new_int(0); 16]),
        );

        // 3.40 variables
        self.add_predefined_variable("U_SHORTDESC", Variable::new_bool(false));
        self.add_predefined_variable("U_GENDER", Variable::new_string(String::new()));
        self.add_predefined_variable("U_BIRTHDATE", Variable::new_string(String::new()));
        self.add_predefined_variable("U_EMAIL", Variable::new_string(String::new()));
        self.add_predefined_variable("U_WEB", Variable::new_string(String::new()));
    }

    fn add_predefined_variable(&mut self, arg: &str, val: Variable) {
        self.cur_frame[0]
            .values
            .insert(unicase::Ascii::new(arg.to_string()), val);
    }

    fn set_variable_value(&mut self, params: &Expression, value: &Variable) -> Res<()> {
        match params {
            Expression::Identifier(ident) => {
                if let Some(val) = self.get_variable_mut(ident.get_identifier()) {
                    *val = convert_to(val.get_type(), value);
                } else {
                    panic!("variable not found {}", ident.get_identifier());
                }
            }
            Expression::FunctionCall(func_call) => {
                let val = if let Some(val) = self.get_variable(func_call.get_identifier()) {
                    val.clone()
                } else {
                    panic!("variable not found {}", func_call.get_identifier());
                };
                let var_type = val.get_type();
                match val.generic_data {
                    VariableValue::Dim1(args) => {
                        if func_call.get_arguments().len() == 1 {
                            let dim1 = evaluate_exp(self, &func_call.get_arguments()[0])?.as_int()
                                as usize;
                            if dim1 < args.len() {
                                if let Some(v) = self.get_variable_mut(func_call.get_identifier()) {
                                    if let VariableValue::Dim1(args) = &mut v.generic_data {
                                        args[dim1] = convert_to(var_type, value);
                                    }
                                }
                            }
                        } else {
                            panic!("incompatible variable {}", func_call.get_identifier());
                        }
                    }
                    VariableValue::Dim2(args) => {
                        if func_call.get_arguments().len() == 2 {
                            let dim1 = evaluate_exp(self, &func_call.get_arguments()[0])?.as_int()
                                as usize;
                            if dim1 < args.len() {
                                let dim2 = evaluate_exp(self, &func_call.get_arguments()[1])?
                                    .as_int() as usize;
                                if dim2 < args[dim1].len() {
                                    if let Some(v) =
                                        self.get_variable_mut(func_call.get_identifier())
                                    {
                                        if let VariableValue::Dim2(args) = &mut v.generic_data {
                                            args[dim1][dim2] = convert_to(var_type, value);
                                        }
                                    }
                                }
                            }
                        } else {
                            panic!("incompatible variable {}", func_call.get_identifier());
                        }
                    }
                    VariableValue::Dim3(args) => {
                        if func_call.get_arguments().len() == 2 {
                            let dim1 = evaluate_exp(self, &func_call.get_arguments()[0])?.as_int()
                                as usize;
                            if dim1 < args.len() {
                                let dim2 = evaluate_exp(self, &func_call.get_arguments()[1])?
                                    .as_int() as usize;
                                if dim2 < args[dim1].len() {
                                    let dim3 = evaluate_exp(self, &func_call.get_arguments()[2])?
                                        .as_int()
                                        as usize;
                                    if dim3 < args[dim1][dim2].len() {
                                        if let Some(v) =
                                            self.get_variable_mut(func_call.get_identifier())
                                        {
                                            if let VariableValue::Dim3(args) = &mut v.generic_data {
                                                args[dim1][dim2][dim3] =
                                                    convert_to(var_type, value);
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            panic!("incompatible variable {}", func_call.get_identifier());
                        }
                    }
                    _ => {
                        panic!("incompatible variable {}", func_call.get_identifier());
                    }
                }
            }
            _ => panic!("unsupported expression {params:?}"),
        }
        Ok(())
    }
}

/// .
///
/// # Panics
///
/// Panics if .
/// # Errors
/// Errors if the variable is not found.
pub fn set_array_value(arr: &mut Variable, val: Variable, dim1: usize, dim2: usize, dim3: usize) {
    match &mut arr.generic_data {
        VariableValue::Dim1(data) => {
            if dim1 < data.len() {
                data[dim1] = val;
            }
        }
        VariableValue::Dim2(data) => {
            if dim1 < data.len() && dim2 < data[dim1].len() {
                data[dim2][dim1] = val;
            }
        }
        VariableValue::Dim3(data) => {
            if dim1 < data.len() && dim2 < data[dim1].len() && dim3 < data[dim1][dim2].len() {
                data[dim3][dim2][dim1] = val;
            }
        }
        _ => panic!("no array variable: {arr}"),
    }
}

fn execute_statement(interpreter: &mut Interpreter, stmt: &Statement) -> Res<()> {
    match stmt {
        Statement::Let(let_stmt) => {
            let value = evaluate_exp(interpreter, let_stmt.get_value_expression())?;
            let var_name = let_stmt.get_identifier().clone();

            if let Some(var_info) = interpreter.get_variable(&var_name) {
                let var_type = var_info.get_type();
                let value = convert_to(var_type, &value);
                if var_info.get_dimensions() > 0 {
                    let dim1: usize = if let_stmt.get_arguments().is_empty() {
                        0
                    } else {
                        let v = &evaluate_exp(interpreter, &let_stmt.get_arguments()[0])?;
                        v.as_int() as usize
                    };

                    let dim2 = if let_stmt.get_arguments().len() <= 1 {
                        0
                    } else {
                        let v = &evaluate_exp(interpreter, &let_stmt.get_arguments()[1])?;
                        v.as_int() as usize
                    };

                    let dim3 = if let_stmt.get_arguments().len() <= 2 {
                        0
                    } else {
                        let v = &evaluate_exp(interpreter, &let_stmt.get_arguments()[2])?;
                        v.as_int() as usize
                    };

                    if let Some(val) = interpreter.get_variable_mut(&var_name) {
                        set_array_value(val, value, dim1, dim2, dim3);
                    } else {
                        panic!("variable not found {var_name:?}");
                    }
                } else if let Some(val) = interpreter.get_variable_mut(&var_name) {
                    *val = value;
                } else {
                    panic!("variable not found {var_name:?}");
                }
            } else {
                log::error!("variable not found {var_name:?}, creating.");
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
                let Some(label_ptr) = frame.label_table.get(label.get_label()) else {
                    panic!("label not found {}", label.get_label());
                };
                frame.cur_ptr = *label_ptr;
            }
        }

        Statement::Gosub(gosub_label) => {
            if let Some(frame) = interpreter.cur_frame.last_mut() {
                let Some(label_ptr) = frame.label_table.get(gosub_label.get_label()) else {
                    panic!("label not found {}", gosub_label.get_label());
                };
                frame.gosub_stack.push(frame.cur_ptr);
                frame.cur_ptr = *label_ptr;
            }
        }
        Statement::Return(_) => {
            //let table = &interpreter.label_tables[interpreter.cur_frame.last().unwrap().label_table as usize];
            interpreter.cur_frame.last_mut().unwrap().cur_ptr = interpreter
                .cur_frame
                .last_mut()
                .unwrap()
                .gosub_stack
                .pop()
                .unwrap();
        }
        Statement::PredifinedCall(call_stmt) => {
            return call_predefined_procedure(
                interpreter,
                call_stmt.get_func(),
                call_stmt.get_arguments(),
            );
        }
        Statement::Call(call_stmt) => {
            for imp in &interpreter.prg.nodes {
                let AstNode::Procedure(f) = imp else {
                    continue;
                };

                if call_stmt.get_identifier() != f.get_identifier() {
                    continue;
                }
                let label_table = calc_stmt_table(f.get_statements());
                let mut prg_frame = StackFrame {
                    values: HashMap::new(),
                    gosub_stack: Vec::new(),
                    cur_ptr: 0,
                    label_table,
                };

                for (i, param) in f.get_parameters().iter().enumerate() {
                    let value = evaluate_exp(interpreter, &call_stmt.get_arguments()[i])?;
                    prg_frame.values.insert(
                        param.get_variable().get_identifier().clone(),
                        convert_to(param.get_variable_type(), &value),
                    );
                }
                interpreter.cur_frame.push(prg_frame);

                while interpreter.cur_frame.last().unwrap().cur_ptr < f.get_statements().len() {
                    let stmt = &f.get_statements()[interpreter.cur_frame.last().unwrap().cur_ptr];
                    execute_statement(interpreter, stmt)?;
                    interpreter.cur_frame.last_mut().unwrap().cur_ptr += 1;
                }
                let prg_frame = interpreter.cur_frame.pop().unwrap();

                for (i, param) in f.get_parameters().iter().enumerate() {
                    let expr = &call_stmt.get_arguments()[i];

                    if let Expression::Identifier(ident) = expr {
                        if let Some(value) =
                            prg_frame.values.get(param.get_variable().get_identifier())
                        {
                            if interpreter
                                .cur_frame
                                .last_mut()
                                .unwrap()
                                .values
                                .contains_key(ident.get_identifier())
                            {
                                interpreter
                                    .cur_frame
                                    .last_mut()
                                    .unwrap()
                                    .values
                                    .insert(ident.get_identifier().clone(), value.clone());
                            } else if interpreter
                                .cur_frame
                                .first_mut()
                                .unwrap()
                                .values
                                .contains_key(ident.get_identifier())
                            {
                                interpreter
                                    .cur_frame
                                    .first_mut()
                                    .unwrap()
                                    .values
                                    .insert(ident.get_identifier().clone(), value.clone());
                            } else {
                                log::warn!(
                                    "variable not found {} for parameter write back.",
                                    ident.get_identifier()
                                );
                            }
                        }
                    }

                    // TODO: Array values.
                }

                break;
            }
        }

        Statement::End(_) => {
            interpreter.cur_frame.last_mut().unwrap().cur_ptr = usize::MAX - 1;
        }
        Statement::If(if_statement) => {
            let value = evaluate_exp(interpreter, if_statement.get_condition())?.as_bool();
            if value {
                execute_statement(interpreter, if_statement.get_statement())?;
            }
        }

        Statement::While(while_statement) => loop {
            let value = evaluate_exp(interpreter, while_statement.get_condition())?.as_bool();
            if !value {
                break;
            }
            execute_statement(interpreter, while_statement.get_statement())?;
        },

        Statement::VariableDeclaration(var_decl) => {
            for var in var_decl.get_variables() {
                if !interpreter
                    .cur_frame
                    .last()
                    .unwrap()
                    .values
                    .contains_key(var.get_identifier())
                {
                    interpreter.cur_frame.last_mut().unwrap().values.insert(
                        var.get_identifier().clone(),
                        var.create_empty_value(var_decl.get_variable_type()),
                    );
                }
            }
        }

        /* unsupported - the compiler does not generate them and ast transformation should remove them */
        Statement::Continue(_) => {
            panic!("unsupported statement Continue")
        }
        Statement::Break(_) => {
            panic!("unsupported statement Break")
        }
        Statement::For(_) => {
            panic!("unsupported statement For")
        }
        Statement::WhileDo(_) => {
            panic!("unsupported statement DoWhile")
        }
        Statement::IfThen(_) => {
            panic!("unsupported statement IfThen")
        }
        Statement::Block(block) => {
            panic!("unsupported statement Begin…End block")
        }
        Statement::Select(_) => {
            panic!("unsupported statement Select")
        }

        // nop statements
        Statement::Label(_) | Statement::Comment(_) => { /* skip */ }
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
    prg: &mut Program,
    ctx: &mut dyn ExecutionContext,
    io: &mut dyn PCBoardIO,
    icy_board_data: IcyBoardData,
) -> Res<bool> {
    let mut statements = Vec::new();
    for imp in &mut prg.nodes {
        let AstNode::Statement(stmt) = imp else {
            continue;
        };
        if let Statement::Block(b) = stmt {
            statements.extend(b.get_statements_mut().drain(0..));
        } else {
            statements.push(stmt.clone());
        }
    }

    let label_table = calc_stmt_table(&statements);
    let cur_frame = StackFrame {
        values: HashMap::new(),
        gosub_stack: Vec::new(),
        cur_ptr: 0,
        label_table,
    };

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
    interpreter.set_default_variables();
    interpreter.set_user_variables(&UserRecord::default());

    while interpreter.is_running {
        let idx = interpreter.cur_frame.last().unwrap().cur_ptr;
        if idx >= statements.len() {
            break;
        }
        let stmt = &statements[idx];
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
