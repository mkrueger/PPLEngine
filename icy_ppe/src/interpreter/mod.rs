use std::collections::HashMap;
use std::string::String;

pub mod expressions;
use thiserror::Error;

use crate::ast::convert_to;
use crate::ast::Expression;
use crate::ast::Implementations;
use crate::ast::Program;
use crate::ast::Statement;
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
pub mod rename_vars_visitor;

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
    pub values: HashMap<unicase::Ascii<String>, VariableValue>,
    pub gosub_stack: Vec<usize>,
    pub cur_ptr: usize,
    pub label_table: HashMap<unicase::Ascii<String>, usize>,
}

pub fn calc_table(blk: &[Statement]) -> HashMap<unicase::Ascii<String>, usize> {
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
            VariableValue::Integer(cur_user.page_len),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_PWD".to_string()),
            VariableValue::String(cur_user.password.clone()),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_PWDEXP".to_string()),
            VariableValue::Date(0000), // TODO
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_SCROLL".to_string()),
            VariableValue::Boolean(cur_user.scroll_flag),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_SEC".to_string()),
            VariableValue::Integer(cur_user.security_level),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_CITY".to_string()),
            VariableValue::String(cur_user.city.clone()),
        );
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("U_ADDR".to_string()),
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

    fn get_variable(&self, var_name: &unicase::Ascii<String>) -> Option<&VariableValue> {
        if self.cur_frame.len() > 1 {
            let last = self.cur_frame.last().unwrap();
            if let Some(val) = last.values.get(var_name) {
                return Some(val);
            }
        }
        self.cur_frame.first().unwrap().values.get(var_name)
    }
    /*
     */
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

fn execute_statement(interpreter: &mut Interpreter, stmt: &Statement) -> Res<()> {
    match stmt {
        Statement::Let(let_stmt) => {
            let value = evaluate_exp(interpreter, let_stmt.get_value_expression())?;
            let var_name = let_stmt.get_identifier().clone();

            if let Some(var_info) = interpreter.get_variable(&var_name) {
                let var_type = var_info.get_type();
                if var_info.get_dimensions() > 0 {
                    let dim1 = if let_stmt.get_arguments().is_empty() {
                        0
                    } else {
                        let v = &evaluate_exp(interpreter, &let_stmt.get_arguments()[0])?;
                        get_int(v)? as usize - 1
                    };

                    let dim2 = if let_stmt.get_arguments().len() <= 1 {
                        0
                    } else {
                        let v = &evaluate_exp(interpreter, &let_stmt.get_arguments()[1])?;
                        get_int(v)? as usize - 1
                    };

                    let dim3 = if let_stmt.get_arguments().len() <= 2 {
                        0
                    } else {
                        let v = &evaluate_exp(interpreter, &let_stmt.get_arguments()[2])?;
                        get_int(v)? as usize - 1
                    };

                    if let Some(val) = interpreter
                        .cur_frame
                        .last_mut()
                        .unwrap()
                        .values
                        .get_mut(&var_name)
                    {
                        set_array_value(val, value, dim1, dim2, dim3);
                    } else {
                        panic!("variable not found {var_name:?}");
                    }
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
            for imp in &interpreter.prg.implementations {
                let Implementations::Procedure(f) = imp else {
                    continue;
                };

                if call_stmt.get_identifier() != f.get_identifier() {
                    continue;
                }
                let label_table = calc_table(f.get_statements());
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
                        if let Some(value) = prg_frame.values.get(param.get_variable().get_identifier()) {
                            if interpreter
                            .cur_frame
                            .last_mut()
                            .unwrap()
                            .values.contains_key(ident.get_identifier()) {
                                interpreter
                                    .cur_frame
                                    .last_mut()
                                    .unwrap()
                                    .values
                                    .insert(ident.get_identifier().clone(), value.clone());
                            } else  if interpreter
                                .cur_frame
                                .first_mut()
                                .unwrap()
                                .values.contains_key(ident.get_identifier()) {
                                    interpreter
                                    .cur_frame
                                        .first_mut()
                                        .unwrap()
                                        .values
                                        .insert(ident.get_identifier().clone(), value.clone());
                            } else {
                                log::warn!("variable not found {} for parameter write back.", ident.get_identifier());
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
            let value = evaluate_exp(interpreter, if_statement.get_condition())?;
            if let VariableValue::Integer(x) = value {
                if x == PPL_TRUE {
                    execute_statement(interpreter, if_statement.get_statement())?;
                }
            } else if let VariableValue::Boolean(x) = value {
                if x {
                    execute_statement(interpreter, if_statement.get_statement())?;
                }
            } else {
                panic!("no bool value {value:?}");
            }
        }

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

        Statement::ProcedureDeclaration(_) | Statement::FunctionDeclaration(_) => {
            // skip for now.
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
    prg: &mut Program,
    ctx: &mut dyn ExecutionContext,
    io: &mut dyn PCBoardIO,
    icy_board_data: IcyBoardData,
) -> Res<bool> {
    let label_table = calc_table(&prg.statements);
    let cur_frame = StackFrame {
        values: HashMap::new(),
        gosub_stack: Vec::new(),
        cur_ptr: 0,
        label_table,
    };

    //prg.visit_mut(&mut rename_vars_visitor::RenameVarsVisitor::default());

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
        && interpreter.cur_frame.last().unwrap().cur_ptr < prg.statements.len()
    {
        let stmt = &prg.statements[interpreter.cur_frame.last().unwrap().cur_ptr];
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
