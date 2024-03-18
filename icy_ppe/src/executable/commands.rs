use std::ops::Range;

use thiserror::Error;

use crate::{
    ast::{BinOp, UnaryOp, Variable, VariableValue},
    tables::FunctionDefinition,
};

use super::{DeserializationErrorType, Executable, OpCode, StatementDefinition};

pub struct DeserializationError {
    pub error_type: DeserializationErrorType,
    pub span: Range<usize>,
}

pub struct PPEStatement {
    pub span: Range<usize>,
    pub command: PPECommand,
}

#[derive(Default)]
pub struct PPEScript {
    pub statements: Vec<PPEStatement>,
}

impl PPEScript {
    pub fn serialize(&self) -> Vec<i16> {
        let mut result = Vec::new();
        for stmt in &self.statements {
            stmt.command.serialize(&mut result);
        }
        result
    }

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn from_ppe_file(exe: &Executable) -> Result<Self, (Self, DeserializationError)> {
        let mut deserializer = super::PPEDeserializer::default();
        let mut result = PPEScript::default();
        loop {
            match deserializer.deserialize_statement(exe) {
                Ok(Some(stmt)) => result.statements.push(PPEStatement {
                    span: deserializer.stmt_span(),
                    command: stmt,
                }),
                Ok(None) => break,
                Err(err) => {
                    return Err((
                        result,
                        DeserializationError {
                            error_type: err,
                            span: deserializer.stmt_span(),
                        },
                    ))
                }
            }
        }
        Ok(result)
    }

    pub fn visit<T: Sized, V: PPEVisitor<T>>(&self, visitor: &mut V) {
        visitor.visit_script(self);
    }

    pub(crate) fn add_statement(&mut self, cur_offset: &mut usize, command: PPECommand) {
        let size = command.get_size();
        self.statements.push(PPEStatement {
            span: *cur_offset..*cur_offset + size,
            command,
        });
        *cur_offset += size;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PPECommand {
    End,
    Return,
    IfNot(Box<PPEExpr>, usize),
    While(Box<PPEExpr>, Box<PPECommand>, usize),
    ProcedureCall(usize, Vec<PPEExpr>),
    PredefinedCall(&'static StatementDefinition<'static>, Vec<PPEExpr>),
    Goto(usize),
    Gosub(usize),
    EndFunc,
    EndProc,
    Stop,
    Let(Box<PPEExpr>, Box<PPEExpr>),
}

impl PPECommand {
    /// .
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn serialize(&self, vec: &mut Vec<i16>) {
        match self {
            PPECommand::End => vec.push(OpCode::END as i16),
            PPECommand::Return => vec.push(OpCode::RETURN as i16),
            PPECommand::EndFunc => vec.push(OpCode::FEND as i16),
            PPECommand::EndProc => vec.push(OpCode::FPCLR as i16),
            PPECommand::Stop => vec.push(OpCode::STOP as i16),

            PPECommand::Goto(pos) => {
                vec.push(OpCode::GOTO as i16);
                vec.push(*pos as i16);
            }
            PPECommand::Gosub(pos) => {
                vec.push(OpCode::GOSUB as i16);
                vec.push(*pos as i16);
            }
            PPECommand::IfNot(expr, label) => {
                vec.push(OpCode::IFNOT as i16);
                expr.serialize(vec);
                vec.push(0);
                vec.push(*label as i16);
            }
            PPECommand::While(expr, stmt, label) => {
                vec.push(OpCode::WHILE as i16);
                expr.serialize(vec);
                vec.push(0);
                vec.push(*label as i16);
                stmt.serialize(vec);
            }
            PPECommand::PredefinedCall(def, args) => {
                vec.push(def.opcode as i16);
                match def.sig {
                    super::StatementSignature::ArgumentsWithVariable(var_index, arg_count) => {
                        assert!(arg_count == args.len(), "Invalid argument count");
                        for (i, arg) in args.iter().enumerate() {
                            arg.serialize(vec);
                            if i + 1 != var_index {
                                vec.push(0);
                            }
                        }
                    }
                    super::StatementSignature::SpecialCaseProcedure => {
                        panic!("SpecialCaseProcedure is not allowed here")
                    }
                    super::StatementSignature::SpecialCaseDlockg => {
                        panic!("SpecialCaseDlockg is not allowed here")
                    }
                    super::StatementSignature::SpecialCaseDcreate => {
                        panic!("SpecialCaseDcreate is not allowed here")
                    }
                    super::StatementSignature::SpecialCaseSort => {
                        panic!("SpecialCaseSort is not allowed here")
                    }
                    super::StatementSignature::VariableArguments(var_index) => {
                        vec.push(args.len() as i16);
                        for (i, arg) in args.iter().enumerate() {
                            arg.serialize(vec);
                            if i + 1 != var_index {
                                vec.push(0);
                            }
                        }
                    }
                    super::StatementSignature::SpecialCasePop => {
                        panic!("SpecialCasePop is not allowed here")
                    }
                    super::StatementSignature::Label => {
                        panic!("Label is not allowed here")
                    }
                    super::StatementSignature::SpecialIfWhen => {
                        panic!("SpecialIfWhen is not allowed here")
                    }
                }
            }
            PPECommand::ProcedureCall(proc_id, args) => {
                vec.push(OpCode::PCALL as i16);
                vec.push(*proc_id as i16);
                vec.push(0);
                for arg in args {
                    arg.serialize(vec);
                    vec.push(0);
                }
            }
            PPECommand::Let(target, value) => {
                vec.push(OpCode::LET as i16);
                target.serialize(vec);
                value.serialize(vec);
                vec.push(0);
            }
        }
    }

    /// Returns the get size of this [`PPECommand`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_size(&self) -> usize {
        match self {
            PPECommand::End
            | PPECommand::Return
            | PPECommand::EndFunc
            | PPECommand::EndProc
            | PPECommand::Stop => 1,

            PPECommand::Goto(_) | PPECommand::Gosub(_) => 2,
            PPECommand::IfNot(expr, _) => 1 + expr.get_size() + 2,
            PPECommand::While(expr, stmt, _) => 1 + expr.get_size() + 2 + stmt.get_size(),
            PPECommand::ProcedureCall(_, args) => 3 + PPEExpr::count_size(args) + args.len(),
            PPECommand::PredefinedCall(def, args) => match def.sig {
                super::StatementSignature::ArgumentsWithVariable(var_index, _) => {
                    1 + PPEExpr::count_size(args)
                        + if var_index > 0 {
                            args.len() - 1
                        } else {
                            args.len()
                        }
                }
                super::StatementSignature::VariableArguments(var_index) => {
                    2 + PPEExpr::count_size(args)
                        + if var_index > 0 {
                            args.len() - 1
                        } else {
                            args.len()
                        }
                }
                _ => panic!("Invalid signature"),
            },
            PPECommand::Let(target, value) => 2 + target.get_size() + value.get_size(),
        }
    }
    pub fn visit<T, V: PPEVisitor<T>>(&self, visitor: &mut V) -> T {
        match self {
            PPECommand::End => visitor.visit_end(),
            PPECommand::Return => visitor.visit_return(),
            PPECommand::IfNot(cond, label) => visitor.visit_if(cond, label),
            PPECommand::While(cond, stmt, label) => visitor.visit_while(cond, stmt, label),
            PPECommand::ProcedureCall(id, args) => visitor.visit_proc_call(id, args),
            PPECommand::PredefinedCall(def, args) => visitor.visit_predefined_call(def, args),
            PPECommand::Goto(label) => visitor.visit_goto(label),
            PPECommand::Gosub(label) => visitor.visit_gosub(label),
            PPECommand::EndFunc => visitor.visit_end_func(),
            PPECommand::EndProc => visitor.visit_end_proc(),
            PPECommand::Stop => visitor.visit_stop(),
            PPECommand::Let(target, value) => visitor.visit_let(target, value),
        }
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub enum PPEExpr {
    #[default]
    Invalid,

    Value(usize),
    UnaryExpression(UnaryOp, Box<PPEExpr>),
    BinaryExpression(BinOp, Box<PPEExpr>, Box<PPEExpr>),
    Dim(usize, Vec<PPEExpr>),
    PredefinedFunctionCall(&'static FunctionDefinition<'static>, Vec<PPEExpr>),
    FunctionCall(usize, Vec<PPEExpr>),
}

impl PPEExpr {
    /// .
    ///
    /// # Panics
    ///
    /// Panics if expression is invalid.
    pub fn serialize(&self, vec: &mut Vec<i16>) {
        match self {
            PPEExpr::Invalid => {
                panic!("Invalid expression");
            }
            PPEExpr::Value(id) => {
                vec.push(*id as i16);
                vec.push(0);
            }
            PPEExpr::Dim(id, vars) => {
                vec.push(*id as i16);
                vec.push(vars.len() as i16);
                for var in vars {
                    var.serialize(vec);
                    vec.push(0);
                }
            }
            PPEExpr::PredefinedFunctionCall(func, args) => {
                for arg in args {
                    arg.serialize(vec);
                }
                vec.push(func.opcode as i16);
            }
            PPEExpr::FunctionCall(func, args) => {
                vec.push(*func as i16);
                vec.push(0);
                for arg in args {
                    arg.serialize(vec);
                    vec.push(0);
                }
            }
            PPEExpr::UnaryExpression(op, expr) => {
                expr.serialize(vec);
                vec.push(*op as i16);
            }
            PPEExpr::BinaryExpression(op, left_expr, right_expr) => {
                left_expr.serialize(vec);
                right_expr.serialize(vec);
                vec.push(*op as i16);
            }
        }
    }

    pub fn get_size(&self) -> usize {
        match self {
            PPEExpr::Invalid => 0,
            PPEExpr::Value(_) => 2,
            PPEExpr::Dim(_, args) => 2 + Self::count_size(args) + args.len(),
            PPEExpr::PredefinedFunctionCall(_, args) => Self::count_size(args) + 1,
            PPEExpr::FunctionCall(_, args) => Self::count_size(args) + 2 + args.len(),
            PPEExpr::UnaryExpression(_, expr) => expr.get_size() + 1,
            PPEExpr::BinaryExpression(_, left_expr, right_expr) => {
                left_expr.get_size() + right_expr.get_size() + 1
            }
        }
    }

    pub fn count_size(args: &[PPEExpr]) -> usize {
        let r = args.iter().map(PPEExpr::get_size).sum();
        r
    }

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn evaluate_constant_value(&self, executable: &Executable) -> Result<Variable, PPEError> {
        PPEConstantValueVisitor::new(executable).visit_value(0)
    }

    /// .
    ///
    /// # Panics
    ///
    /// Panics if expression is invalid.
    pub fn visit<T, V: PPEVisitor<T>>(&self, visitor: &mut V) -> T {
        match self {
            PPEExpr::Invalid => panic!("Invalid expression"),
            PPEExpr::Value(id) => visitor.visit_value(*id),
            PPEExpr::UnaryExpression(op, expr) => visitor.visit_unary_expression(*op, expr),
            PPEExpr::BinaryExpression(op, left, right) => {
                visitor.visit_binary_expression(*op, left, right)
            }
            PPEExpr::Dim(id, dim) => visitor.visit_dim_expression(*id, dim),
            PPEExpr::PredefinedFunctionCall(def, arguments) => {
                visitor.visit_predefined_function_call(def, arguments)
            }
            PPEExpr::FunctionCall(id, arguments) => visitor.visit_function_call(*id, arguments),
        }
    }
}
#[allow(unused_variables)]
pub trait PPEVisitor<T>: Sized {
    fn visit_script(&mut self, script: &PPEScript) {
        for stmt in &script.statements {
            stmt.command.visit(self);
        }
    }
    fn visit_value(&mut self, id: usize) -> T;
    fn visit_unary_expression(&mut self, op: UnaryOp, expr: &PPEExpr) -> T;
    fn visit_binary_expression(&mut self, op: BinOp, left: &PPEExpr, right: &PPEExpr) -> T;
    fn visit_dim_expression(&mut self, id: usize, dim: &[PPEExpr]) -> T;
    fn visit_predefined_function_call(
        &mut self,
        def: &FunctionDefinition,
        arguments: &[PPEExpr],
    ) -> T;
    fn visit_function_call(&mut self, id: usize, arguments: &[PPEExpr]) -> T;

    fn visit_end(&mut self) -> T;
    fn visit_return(&mut self) -> T;
    fn visit_if(&mut self, cond: &PPEExpr, label: &usize) -> T;
    fn visit_while(&mut self, cond: &PPEExpr, stmt: &PPECommand, label: &usize) -> T;
    fn visit_proc_call(&mut self, id: &usize, args: &[PPEExpr]) -> T;
    fn visit_predefined_call(&mut self, def: &StatementDefinition, args: &[PPEExpr]) -> T;
    fn visit_goto(&mut self, label: &usize) -> T;
    fn visit_gosub(&mut self, label: &usize) -> T;
    fn visit_end_func(&mut self) -> T;
    fn visit_end_proc(&mut self) -> T;
    fn visit_stop(&mut self) -> T;
    fn visit_let(&mut self, target: &PPEExpr, value: &PPEExpr) -> T;
}

struct PPEConstantValueVisitor<'a> {
    executable: &'a Executable,
}

impl<'a> PPEConstantValueVisitor<'a> {
    fn new(executable: &'a Executable) -> Self {
        Self { executable }
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum PPEError {
    #[error("Unsupported dimension {0} for variable {1:?}")]
    UnsupportedDimension(usize, Variable),

    #[error("Only constant expressions are allowed")]
    OnlyConstantsAllowed,
}

impl<'a> PPEVisitor<Result<Variable, PPEError>> for PPEConstantValueVisitor<'a> {
    fn visit_value(&mut self, id: usize) -> Result<Variable, PPEError> {
        Ok(self.executable.variable_table[id].value.clone())
    }

    fn visit_unary_expression(
        &mut self,
        op: UnaryOp,
        expr: &PPEExpr,
    ) -> Result<Variable, PPEError> {
        let val = expr.visit(self)?;
        let val = match op {
            UnaryOp::Plus => val,
            UnaryOp::Minus => -val,
            UnaryOp::Not => val.not(),
        };
        Ok(val)
    }

    fn visit_binary_expression(
        &mut self,
        op: BinOp,
        left: &PPEExpr,
        right: &PPEExpr,
    ) -> Result<Variable, PPEError> {
        let left_val = left.visit(self)?;
        let right_val = right.visit(self)?;
        let val = match op {
            BinOp::Add => left_val + right_val,
            BinOp::Sub => left_val - right_val,
            BinOp::Mul => left_val * right_val,
            BinOp::Div => left_val / right_val,
            BinOp::Mod => left_val % right_val,
            BinOp::PoW => left_val.pow(right_val),
            BinOp::And => Variable::new_bool(left_val.as_bool() && right_val.as_bool()),
            BinOp::Or => Variable::new_bool(left_val.as_bool() || right_val.as_bool()),
            BinOp::Eq => Variable::new_bool(left_val.as_bool() == right_val.as_bool()),
            BinOp::NotEq => Variable::new_bool(left_val.as_bool() != right_val.as_bool()),
            BinOp::Lower => Variable::new_bool(left_val < right_val),
            BinOp::LowerEq => Variable::new_bool(left_val <= right_val),
            BinOp::Greater => Variable::new_bool(left_val > right_val),
            BinOp::GreaterEq => Variable::new_bool(left_val >= right_val),
        };

        Ok(val)
    }

    fn visit_dim_expression(&mut self, id: usize, dim: &[PPEExpr]) -> Result<Variable, PPEError> {
        let var_value = self.executable.variable_table[id].value.clone();
        match dim.len() {
            1 => {
                let vector = dim[0].visit(self)?.as_int();
                if let VariableValue::Dim1(data) = &var_value.generic_data {
                    if vector < 0 || vector >= data.len() as i32 {
                        return Ok(var_value.get_type().create_empty_value());
                    }
                    return Ok(data[vector as usize].clone());
                }
                Err(PPEError::UnsupportedDimension(1, var_value.clone()))
            }
            2 => {
                let vector = dim[0].visit(self)?.as_int();
                let matrix = dim[1].visit(self)?.as_int();
                if let VariableValue::Dim2(data) = &var_value.generic_data {
                    if vector < 0
                        || vector >= data.len() as i32
                        || matrix < 0
                        || matrix >= data[0].len() as i32
                    {
                        return Ok(var_value.get_type().create_empty_value());
                    }
                    return Ok(data[vector as usize][matrix as usize].clone());
                }
                Err(PPEError::UnsupportedDimension(2, var_value.clone()))
            }
            3 => {
                let vector = dim[0].visit(self)?.as_int();
                let matrix = dim[1].visit(self)?.as_int();
                let cube = dim[2].visit(self)?.as_int();
                if let VariableValue::Dim3(data) = &var_value.generic_data {
                    if vector < 0
                        || vector >= data.len() as i32
                        || matrix < 0
                        || matrix >= data[0].len() as i32
                        || cube < 0
                        || cube >= data[0][0].len() as i32
                    {
                        return Ok(var_value.get_type().create_empty_value());
                    }
                    return Ok(data[vector as usize][matrix as usize][cube as usize].clone());
                }
                Err(PPEError::UnsupportedDimension(3, var_value.clone()))
            }
            len => Err(PPEError::UnsupportedDimension(len, var_value.clone())),
        }
    }

    fn visit_predefined_function_call(
        &mut self,
        _def: &FunctionDefinition,
        _arguments: &[PPEExpr],
    ) -> Result<Variable, PPEError> {
        Err(PPEError::OnlyConstantsAllowed)
    }

    fn visit_function_call(
        &mut self,
        _id: usize,
        _arguments: &[PPEExpr],
    ) -> Result<Variable, PPEError> {
        Err(PPEError::OnlyConstantsAllowed)
    }

    fn visit_end(&mut self) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_return(&mut self) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_if(&mut self, _cond: &PPEExpr, _label: &usize) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_while(
        &mut self,
        _cond: &PPEExpr,
        _stmt: &PPECommand,
        _label: &usize,
    ) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_proc_call(&mut self, _id: &usize, _args: &[PPEExpr]) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_predefined_call(
        &mut self,
        _def: &StatementDefinition,
        _args: &[PPEExpr],
    ) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_goto(&mut self, _label: &usize) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_gosub(&mut self, _label: &usize) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_end_func(&mut self) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_end_proc(&mut self) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_stop(&mut self) -> Result<Variable, PPEError> {
        todo!()
    }

    fn visit_let(&mut self, _target: &PPEExpr, _value: &PPEExpr) -> Result<Variable, PPEError> {
        todo!()
    }
}
