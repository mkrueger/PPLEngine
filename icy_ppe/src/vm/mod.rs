use std::backtrace::Backtrace;
use std::collections::HashMap;
use std::path::PathBuf;
use std::string::String;
use thiserror::Error;

use crate::ast::BinOp;
use crate::ast::GenericVariableData;
use crate::ast::Statement;
use crate::ast::UnaryOp;
use crate::ast::VariableType;
use crate::ast::VariableValue;
use crate::executable::Executable;
use crate::executable::PPECommand;
use crate::executable::PPEExpr;
use crate::executable::PPEScript;
use crate::executable::PPEStatement;
use crate::executable::VariableEntry;
use crate::icy_board::data::IcyBoardData;
use crate::icy_board::data::Node;
use crate::icy_board::data::UserRecord;
use crate::Res;

pub mod expressions;

pub mod statements;
pub use self::statements::*;

pub mod io;
pub use self::io::*;

pub mod errors;
mod tests;

#[derive(Error, Debug, Clone, Copy)]
pub enum VMError {
    #[error("Internal VM error")]
    InternalVMError,

    #[error("Label not found: {0}")]
    LabelNotFound(usize),
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
    pub values: HashMap<unicase::Ascii<String>, VariableValue>,
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

pub struct ReturnAddress {
    ptr: usize,
    id: usize,
}

impl ReturnAddress {
    pub fn gosub(cur_ptr: usize) -> ReturnAddress {
        ReturnAddress {
            ptr: cur_ptr,
            id: 0,
        }
    }
    fn func_call(cur_ptr: usize, proc_id: usize) -> ReturnAddress {
        ReturnAddress {
            ptr: cur_ptr,
            id: proc_id,
        }
    }

    pub fn get_ptr(&self) -> usize {
        self.ptr
    }

    pub fn get_id(&self) -> usize {
        self.id
    }

    pub fn is_gosub(&self) -> bool {
        self.id == 0
    }
}

pub struct VirtualMachine<'a> {
    ctx: &'a mut dyn ExecutionContext,
    io: &'a mut dyn PCBoardIO,
    pub file_name: PathBuf,
    pub variable_table: Vec<VariableEntry>,

    pub script: PPEScript,
    pub cur_ptr: usize,
    pub is_running: bool,
    pub fpclear: bool,

    pub icy_board_data: IcyBoardData,
    pub cur_user: usize,
    pub current_user: Option<UserRecord>,
    pub pcb_node: Option<Node>,

    pub cur_tokens: Vec<String>, //  stack_frames: Vec<StackFrame>

    return_addresses: Vec<ReturnAddress>,
    parameter_stack: Vec<VariableValue>,
    write_back_stack: Vec<PPEExpr>,

    pub label_table: HashMap<usize, usize>,
}

impl<'a> VirtualMachine<'a> {
    fn set_user_variables(&mut self, cur_user: &UserRecord) {
        // TODO
        /*
        self.cur_frame[0].values.insert(
            unicase::Ascii::new("self".to_string()),
            Variable::new(
                VariableType::Integer,
                VariableData::from_int(cur_user.page_len),
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
                VariableData::from_int(cur_user.security_level),
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
                VariableData::from_int(cur_user.security_level),
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
        );*/
    }
    /*
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
    */

    fn eval_expr(&mut self, expr: &PPEExpr) -> Result<VariableValue, VMError> {
        match expr {
            PPEExpr::Invalid => Err(VMError::InternalVMError),
            PPEExpr::Value(id) => Ok(self.get_value(*id).clone()),
            PPEExpr::UnaryExpression(op, expr) => {
                let val = self.eval_expr(expr)?;
                match op {
                    UnaryOp::Not => Ok(val.not()),
                    UnaryOp::Minus => Ok(-val),
                    UnaryOp::Plus => Ok(val),
                }
            }
            PPEExpr::BinaryExpression(op, left, right) => {
                let left_value = self.eval_expr(left)?;
                let right_value = self.eval_expr(right)?;

                let res = match op {
                    BinOp::Add => Ok(left_value + right_value),
                    BinOp::Sub => Ok(left_value - right_value),
                    BinOp::Mul => Ok(left_value * right_value),
                    BinOp::Div => Ok(left_value / right_value),
                    BinOp::Mod => Ok(left_value % right_value),
                    BinOp::PoW => Ok(left_value.pow(right_value)),
                    BinOp::Eq => Ok(VariableValue::new_bool(left_value == right_value)),
                    BinOp::NotEq => Ok(VariableValue::new_bool(left_value != right_value)),
                    BinOp::Or => Ok(VariableValue::new_bool(
                        left_value.as_bool() || right_value.as_bool(),
                    )),
                    BinOp::And => Ok(VariableValue::new_bool(
                        left_value.as_bool() && right_value.as_bool(),
                    )),
                    BinOp::Lower => Ok(VariableValue::new_bool(left_value < right_value)),
                    BinOp::LowerEq => Ok(VariableValue::new_bool(left_value <= right_value)),
                    BinOp::Greater => Ok(VariableValue::new_bool(left_value > right_value)),
                    BinOp::GreaterEq => Ok(VariableValue::new_bool(left_value >= right_value)),
                };

                res
            }
            PPEExpr::Dim(id, dims) => {
                let dim_1 = self.eval_expr(&dims[0])?.as_int() as usize;
                let dim_2 = if dims.len() >= 2 {
                    self.eval_expr(&dims[1])?.as_int() as usize
                } else {
                    0
                };
                let dim_3 = if dims.len() >= 3 {
                    self.eval_expr(&dims[2])?.as_int() as usize
                } else {
                    0
                };

                let var = &self.get_value(*id);
                if let GenericVariableData::Dim1(data) = &var.generic_data {
                    if dim_1 < data.len() {
                        Ok(data[dim_1].clone())
                    } else {
                        Ok(var.vtype.create_empty_value())
                    }
                } else if let GenericVariableData::Dim2(data) = &var.generic_data {
                    if dim_1 < data.len() && dim_2 < data[dim_1].len() {
                        Ok(data[dim_1][dim_2].clone())
                    } else {
                        Ok(var.vtype.create_empty_value())
                    }
                } else if let GenericVariableData::Dim3(data) = &var.generic_data {
                    if dim_1 < data.len()
                        && dim_2 < data[dim_1].len()
                        && dim_3 < data[dim_1][dim_2].len()
                    {
                        Ok(data[dim_1][dim_2][dim_3].clone())
                    } else {
                        Ok(var.vtype.create_empty_value())
                    }
                } else {
                    Ok(var.vtype.create_empty_value())
                }
            }
            PPEExpr::PredefinedFunctionCall(func, arguments) => {
                let mut args = Vec::new();
                for arg in arguments {
                    args.push(self.eval_expr(arg)?);
                }
                Ok((func.function)(self, &args).unwrap())
            }

            PPEExpr::FunctionCall(func_id, arguments) => {
                let proc_offset;
                let locals;
                let parameters;
                let first;
                let return_var_id;
                unsafe {
                    let proc = &self.get_var_entry(*func_id);
                    proc_offset = proc.value.data.function_value.start_offset as usize;
                    first = (proc.value.data.function_value.first_var_id + 1) as usize;
                    locals = proc.value.data.function_value.local_variables as usize;
                    parameters = proc.value.data.function_value.parameters as usize;
                    return_var_id = proc.value.data.function_value.return_var as usize;
                }
                for i in 0..locals {
                    let id = first + i;
                    if self.get_var_entry(id).header.flags & 0x1 == 0x0 && id != return_var_id {
                        let value = self.get_value(id).clone();
                        self.parameter_stack.push(value);
                    }

                    if i < parameters && id != return_var_id {
                        let expr = self.eval_expr(&arguments[i])?;
                        self.set_value(id, expr);
                    }
                }
                self.return_addresses
                    .push(ReturnAddress::func_call(self.cur_ptr, *func_id));
                self.goto(proc_offset)?;
                self.run()?;
                self.fpclear = false;
                Ok(self.get_value(return_var_id).clone())
            }
        }
    }

    fn run(&mut self) -> Result<(), VMError> {
        let max_ptr = self.script.statements.len();
        while !self.fpclear && self.is_running && self.cur_ptr < max_ptr {
            let p = self.cur_ptr;
            self.cur_ptr += 1;
            let c = self.script.statements[p].command.clone();
            // println!("{}: {:?}", p, &c);
            self.execute_statement(&c)?;
        }
        Ok(())
    }

    fn set_value(&mut self, id: usize, value: VariableValue) {
        self.variable_table[id - 1].value = value;
    }

    fn get_value(&mut self, id: usize) -> &VariableValue {
        &self.variable_table[id - 1].value
    }
    fn get_var_entry(&mut self, id: usize) -> &VariableEntry {
        &self.variable_table[id - 1]
    }

    fn set_variable(&mut self, variable: &PPEExpr, value: VariableValue) -> Result<(), VMError> {
        match variable {
            PPEExpr::Value(id) => {
                self.set_value(*id, value);
            }
            PPEExpr::Dim(id, dims) => {
                let dim_1 = self.eval_expr(&dims[0])?.as_int() as usize;
                let dim_2 = if dims.len() >= 2 {
                    self.eval_expr(&dims[1])?.as_int() as usize
                } else {
                    0
                };
                let dim_3 = if dims.len() >= 3 {
                    self.eval_expr(&dims[2])?.as_int() as usize
                } else {
                    0
                };

                let var = &mut self.variable_table[*id - 1].value;
                Self::set_array_value(var, value, dim_1, dim_2, dim_3);
            }
            _ => {
                return Err(VMError::InternalVMError);
            }
        }
        Ok(())
    }

    fn execute_statement(&mut self, stmt: &PPECommand) -> Result<(), VMError> {
        //crossterm::terminal::disable_raw_mode().unwrap();

        match stmt {
            PPECommand::End | PPECommand::Stop => {
                self.is_running = false;
            }

            PPECommand::EndFunc | PPECommand::EndProc | PPECommand::Return => {
                if let Some(addr) = self.return_addresses.pop() {
                    self.cur_ptr = addr.get_ptr();
                    let proc_id = addr.get_id();
                    if proc_id > 0 {
                        let locals;
                        let first;
                        let parameters;
                        let return_var_id;
                        let pass_flags;
                        unsafe {
                            let proc = &self.get_var_entry(proc_id);
                            first = (proc.value.data.procedure_value.first_var_id + 1) as usize;
                            locals = proc.value.data.procedure_value.local_variables as usize;
                            parameters = proc.value.data.procedure_value.parameters as usize;
                            if proc.header.variable_type == VariableType::Function {
                                return_var_id = proc.value.data.function_value.return_var as usize;
                                pass_flags = 0;
                            } else {
                                return_var_id = 0;
                                pass_flags = proc.value.data.procedure_value.pass_flags;
                            }
                        }

                        if pass_flags > 0 {
                            for i in 0..parameters {
                                let id = first + i;
                                if (1 << i) & pass_flags != 0 {
                                    let val = self.get_value(id).clone();
                                    let argument_expr = self.write_back_stack.pop().unwrap();
                                    self.set_variable(&argument_expr, val)?;
                                }
                            }
                        }

                        for i in (0..locals).rev() {
                            let id = first + i;
                            if self.get_var_entry(id).header.flags & 0x1 == 0x0
                                && return_var_id != id
                            {
                                let value = self.parameter_stack.pop().unwrap();
                                self.set_value(id, value);
                            }
                        }
                        if stmt == &PPECommand::EndFunc {
                            self.fpclear = true;
                        }
                    }
                } else {
                    self.is_running = false;
                }
            }

            PPECommand::IfNot(expr, label) => {
                if !self.eval_expr(expr)?.as_bool() {
                    self.goto(*label)?;
                }
            }
            PPECommand::While(exp, stmt, label) => {
                while self.eval_expr(exp)?.as_bool() {
                    self.execute_statement(stmt)?;
                }
                self.goto(*label)?;
            }
            PPECommand::ProcedureCall(proc_id, arguments) => {
                let proc_offset;
                let locals;
                let parameters;
                let first;
                let pass_flags;

                unsafe {
                    let proc = &self.get_var_entry(*proc_id);
                    proc_offset = proc.value.data.procedure_value.start_offset as usize;
                    first = (proc.value.data.procedure_value.first_var_id + 1) as usize;
                    locals = proc.value.data.procedure_value.local_variables as usize;
                    parameters = proc.value.data.procedure_value.parameters as usize;
                    pass_flags = proc.value.data.procedure_value.pass_flags;
                }
                for i in 0..locals {
                    let id = first + i;
                    if self.get_var_entry(id).header.flags & 0x1 == 0x0 {
                        let val = self.get_value(id).clone();
                        self.parameter_stack.push(val);
                    }
                    if (1 << i) & pass_flags != 0 {
                        self.write_back_stack.push(arguments[i].clone());
                    }

                    if i < parameters {
                        let expr = self.eval_expr(&arguments[i])?;
                        self.set_value(id, expr);
                    }
                }
                self.return_addresses
                    .push(ReturnAddress::func_call(self.cur_ptr, *proc_id));
                self.goto(proc_offset)?;
            }
            PPECommand::PredefinedCall(proc, arguments) => {
                let mut args = Vec::new();
                for arg in arguments {
                    let expr = self.eval_expr(arg)?;
                    args.push(expr);
                }
                (proc.function)(self, &mut args);
            }
            PPECommand::Goto(label) => {
                self.goto(*label)?;
            }
            PPECommand::Gosub(label) => {
                self.return_addresses
                    .push(ReturnAddress::gosub(self.cur_ptr));
                self.goto(*label)?;
            }
            PPECommand::Let(variable, expr) => {
                let val = self.eval_expr(expr)?;
                self.set_variable(variable, val)?;
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
    /// Errors if the variable is not found.
    pub fn set_array_value(
        arr: &mut VariableValue,
        val: VariableValue,
        dim1: usize,
        dim2: usize,
        dim3: usize,
    ) {
        match &mut arr.generic_data {
            GenericVariableData::Dim1(data) => {
                if dim1 < data.len() {
                    data[dim1] = val;
                }
            }
            GenericVariableData::Dim2(data) => {
                if dim1 < data.len() && dim2 < data[dim1].len() {
                    data[dim2][dim1] = val;
                }
            }
            GenericVariableData::Dim3(data) => {
                if dim1 < data.len() && dim2 < data[dim1].len() && dim3 < data[dim1][dim2].len() {
                    data[dim3][dim2][dim1] = val;
                }
            }
            _ => panic!("no array variable: {arr}"),
        }
    }

    fn goto(&mut self, label: usize) -> Result<(), VMError> {
        if let Some(label) = self.label_table.get(&label) {
            self.cur_ptr = *label;
            Ok(())
        } else {
            Err(VMError::LabelNotFound(label))
        }
    }
}

/// .
/// # Errors
pub fn run(
    file_name: PathBuf,
    prg: &Executable,
    ctx: &mut dyn ExecutionContext,
    io: &mut dyn PCBoardIO,
    icy_board_data: IcyBoardData,
) -> Res<bool> {
    let Ok(script) = PPEScript::from_ppe_file(prg) else {
        return Ok(false);
    };

    let mut label_table = calc_labe_table(&script.statements);
    for (i, stmt) in script.statements.iter().enumerate() {
        label_table.insert(stmt.span.start * 2, i);
    }

    let mut vm = VirtualMachine {
        file_name,
        ctx,
        return_addresses: Vec::new(),
        script,
        io,
        is_running: true,
        fpclear: false,
        cur_tokens: Vec::new(),
        icy_board_data,
        cur_user: 0,
        current_user: None,
        pcb_node: None,
        variable_table: prg.variable_table.clone(),
        cur_ptr: 0,
        label_table,
        parameter_stack: Vec::new(),
        write_back_stack: Vec::new(),
    };

    vm.set_user_variables(&UserRecord::default());
    vm.run()?;
    Ok(true)
}

fn calc_labe_table(statements: &[PPEStatement]) -> HashMap<usize, usize> {
    let mut res = HashMap::new();
    let mut offset = 0;
    for (i, stmt) in statements.iter().enumerate() {
        res.insert(offset, i);
        offset += stmt.command.get_size();
    }
    res
}
