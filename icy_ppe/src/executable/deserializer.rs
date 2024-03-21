use std::{mem::transmute, ops::Range};

use thiserror::Error;

use crate::{
    ast::UnaryOp,
    decompiler::LAST_STMT,
    executable::{OpCode, FUNCTION_DEFINITIONS, STATEMENT_DEFINITIONS},
    tables::{BIN_EXPR, STATEMENT_SIGNATURE_TABLE},
};

use super::{Executable, FuncOpCode, PPECommand, PPEExpr, VariableType, VariableValue};

#[derive(Error, Debug, Clone, PartialEq)]
pub enum DeserializationErrorType {
    #[error("Expressionstack is empty")]
    ExpressionStackEmpty,

    #[error("No expression found")]
    NoExpression,

    #[error("Invalid expression stack state")]
    InvalidExpressionStackState,

    #[error("Unknown unary function {0:?}")]
    UnknownUnaryFunction(FuncOpCode),

    #[error("Too few arguments for binary expression")]
    TooFewArgumentsForBinaryExpression,

    #[error("Too few arguments for unary expression")]
    TooFewArgumentsForUnaryExpression,

    #[error("Too few function arguments for {0:?}, expected {1}, got {2}")]
    TooFewFunctionArguments(FuncOpCode, i8, usize),

    #[error("Index out of bounds")]
    IndexOutOfBounds,

    #[error("Unknown statement {0}")]
    UnknownStatement(i16),

    #[error("Called non-procedure({0:04X}) in PCALL {1:?}")]
    CalledNonProcedureInPCall(usize, VariableValue),

    #[error("No variable table entry for {0}")]
    NoVTableEntry(usize),

    #[error("Invalid statement {0}")]
    InvalidStatement(i16),

    #[error("Got procedure call in expression {0}")]
    GotProcedureCallInExpression(i16),
}

#[derive(Default)]
pub struct PPEDeserializer {
    expr_stack: Vec<PPEExpr>,
    pub offset: usize,

    stmt_offset: usize,
    expr_offset: usize,
}

impl PPEDeserializer {
    pub fn stmt_span(&self) -> Range<usize> {
        self.stmt_offset..self.offset
    }
    pub fn expr_span(&self) -> Range<usize> {
        self.expr_offset..self.offset
    }

    /// .
    /// # Errors
    ///
    /// This function will return an error if .
    /// # Panics
    ///
    pub fn deserialize_statement(
        &mut self,
        executable: &Executable,
    ) -> Result<Option<PPECommand>, DeserializationErrorType> {
        self.stmt_offset = self.offset;
        if self.offset >= executable.script_buffer.len() {
            return Ok(None);
        }
        let cur_stmt = executable.script_buffer[self.offset];
        self.offset += 1;

        if !(0..LAST_STMT).contains(&cur_stmt)
            || STATEMENT_SIGNATURE_TABLE[cur_stmt as usize] == 0xAA
        {
            return Err(DeserializationErrorType::InvalidStatement(cur_stmt));
        }

        let op: OpCode = unsafe { transmute(cur_stmt) };

        let res = match op {
            OpCode::END => Ok(Some(PPECommand::End)),
            OpCode::RETURN => Ok(Some(PPECommand::Return)),
            OpCode::FEND => Ok(Some(PPECommand::EndFunc)),
            OpCode::FPCLR => Ok(Some(PPECommand::EndProc)),
            OpCode::STOP => Ok(Some(PPECommand::Stop)),
            OpCode::LET => {
                let target = self.read_variable_expression(executable)?;
                let value = self.deserialize_expression(executable)?;

                Ok(Some(PPECommand::Let(Box::new(target), Box::new(value))))
            }
            OpCode::IFNOT => {
                let expr = self.deserialize_expression(executable)?;
                let label = executable.script_buffer[self.offset] as usize;
                self.offset += 1;
                Ok(Some(PPECommand::IfNot(Box::new(expr), label)))
            }
            OpCode::WHILE => {
                let expr = self.deserialize_expression(executable)?;
                let label = executable.script_buffer[self.offset] as usize;
                self.offset += 1;
                Ok(Some(PPECommand::IfNot(Box::new(expr), label)))
            }
            OpCode::GOSUB => {
                let label = executable.script_buffer[self.offset] as usize;
                self.offset += 1;
                Ok(Some(PPECommand::Gosub(label)))
            }
            OpCode::GOTO => {
                let label = executable.script_buffer[self.offset] as usize;
                self.offset += 1;
                Ok(Some(PPECommand::Goto(label)))
            }
            OpCode::PCALL => {
                // TODO: implement read var correctld ?
                let proc_id = executable.script_buffer[self.offset] as usize;
                self.offset += 2;

                let Some(var) = executable.variable_table.try_get_entry(proc_id) else {
                    return Err(DeserializationErrorType::NoVTableEntry(proc_id));
                };

                if var.value.vtype != VariableType::Procedure {
                    return Err(DeserializationErrorType::CalledNonProcedureInPCall(
                        proc_id,
                        var.value.clone(),
                    ));
                }

                let argument_count = unsafe { var.value.data.procedure_value.parameters };
                let mut arguments = Vec::new();
                for _ in 0..argument_count {
                    let expr = self.deserialize_expression(executable)?;
                    arguments.push(expr);
                }
                Ok(Some(PPECommand::ProcedureCall(proc_id, arguments)))
            }
            _ => {
                let idx = op as usize;
                let Some(def) = STATEMENT_DEFINITIONS.get(idx) else {
                    return Err(DeserializationErrorType::UnknownStatement(cur_stmt));
                };

                let (var_idx, argument_count) = match def.sig {
                    crate::executable::StatementSignature::ArgumentsWithVariable(
                        var_idx,
                        argument_count,
                    ) => (var_idx, argument_count),
                    crate::executable::StatementSignature::VariableArguments(var_idx) => {
                        let argument_count = executable.script_buffer[self.offset];
                        assert!(argument_count >= 0, "negative argument count");
                        self.offset += 1;
                        (var_idx, argument_count as usize)
                    }
                    crate::executable::StatementSignature::SpecialCaseSort => {
                        let arguments = vec![
                            PPEExpr::Value(executable.script_buffer[self.offset] as usize),
                            PPEExpr::Value(executable.script_buffer[self.offset + 1] as usize),
                        ];
                        self.offset += 2;

                        return Ok(Some(PPECommand::PredefinedCall(def, arguments)));
                    }
                    crate::executable::StatementSignature::SpecialCaseVarSeg => {
                        let arguments = vec![
                            self.read_variable_expression(executable)?,
                            self.read_variable_expression(executable)?,
                        ];
                        return Ok(Some(PPECommand::PredefinedCall(def, arguments)));
                    }
                    crate::executable::StatementSignature::SpecialCaseDcreate => {
                        let arguments = vec![
                            self.deserialize_expression(executable)?,
                            self.deserialize_expression(executable)?,
                            self.deserialize_expression(executable)?,
                            PPEExpr::Value(executable.script_buffer[self.offset] as usize),
                        ];
                        self.offset += 1;
                        return Ok(Some(PPECommand::PredefinedCall(def, arguments)));
                    }
                    super::StatementSignature::SpecialCaseDlockg => {
                        let mut arguments = vec![
                            self.deserialize_expression(executable)?,
                            PPEExpr::Value(executable.script_buffer[self.offset] as usize),
                        ];
                        self.offset += 1;
                        arguments.push(self.deserialize_expression(executable)?);
                        return Ok(Some(PPECommand::PredefinedCall(def, arguments)));
                    }
                    crate::executable::StatementSignature::SpecialCasePop => {
                        let count = executable.script_buffer[self.offset] as usize;
                        self.offset += 1;
                        let mut arguments = Vec::new();
                        for _ in 0..count {
                            arguments.push(self.read_variable_expression(executable)?);
                        }
                        return Ok(Some(PPECommand::PredefinedCall(def, arguments)));
                    }
                    crate::executable::smt_op_codes::StatementSignature::Invalid => {
                        panic!("unhandled statement signature {:?}", def.sig);
                    }
                };

                let mut arguments = Vec::new();
                for i in 0..argument_count {
                    let expr = if i + 1 == var_idx {
                        self.read_variable_expression(executable)?
                    } else {
                        self.deserialize_expression(executable)?
                    };
                    arguments.push(expr);
                }
                return Ok(Some(PPECommand::PredefinedCall(def, arguments)));
            }
        };
        res
    }

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn deserialize_expression(
        &mut self,
        executable: &Executable,
    ) -> Result<PPEExpr, DeserializationErrorType> {
        self.expr_offset = self.offset;

        loop {
            if self.offset >= executable.script_buffer.len() {
                break;
            }
            let id = executable.script_buffer[self.offset];
            if id == 0 {
                self.offset += 1;
                break;
            }
            if id > 0 {
                let id = id as usize;
                let Some(val) = executable.variable_table.try_get_value(id) else {
                    log::warn!("Potential error in expression deserialization: No variable table entry for {}, skipping.", id);
                    self.offset += 1;
                    break;
                };
                match val.vtype {
                    VariableType::Function => unsafe {
                        self.offset += 2;
                        let parameters = executable
                            .variable_table
                            .get_value(id)
                            .data
                            .function_value
                            .parameters;
                        let mut arguments = Vec::new();
                        for _ in 0..parameters {
                            let expr = self.deserialize_expression(executable)?;
                            arguments.push(expr);
                        }
                        self.push_expr(PPEExpr::FunctionCall(id, arguments));
                        continue;
                    },
                    VariableType::Procedure => {
                        self.offset += 1;
                        return Err(DeserializationErrorType::GotProcedureCallInExpression(
                            id as i16,
                        ));
                    }
                    _ => {}
                }

                let var_expr = self.read_variable_expression(executable)?;
                self.push_expr(var_expr);
            } else {
                if id == FuncOpCode::CPAR as i16 {
                    self.offset += 1;
                    break;
                }
                let func = -id as usize;
                let func_def = &FUNCTION_DEFINITIONS[func];

                match func_def.args {
                    0x10 => {
                        self.offset += 1;
                        match self.pop_expr() {
                            Some(unary_expr) => {
                                let op = match func_def.opcode {
                                    FuncOpCode::NOT => UnaryOp::Not,
                                    FuncOpCode::UMINUS => UnaryOp::Minus,
                                    FuncOpCode::UPLUS => UnaryOp::Plus,
                                    _ => {
                                        return Err(
                                            DeserializationErrorType::UnknownUnaryFunction(
                                                func_def.opcode,
                                            ),
                                        );
                                    }
                                };
                                self.push_expr(PPEExpr::UnaryExpression(op, Box::new(unary_expr)));
                            }
                            None => {
                                return Err(
                                    DeserializationErrorType::TooFewArgumentsForUnaryExpression,
                                );
                            }
                        }
                    }
                    0x11 => {
                        self.offset += 1;
                        let Some(r_value) = self.pop_expr() else {
                            return Err(
                                DeserializationErrorType::TooFewArgumentsForBinaryExpression,
                            );
                        };
                        let Some(l_value) = self.pop_expr() else {
                            return Err(
                                DeserializationErrorType::TooFewArgumentsForBinaryExpression,
                            );
                        };

                        let binop = BIN_EXPR[-(func_def.opcode as i32) as usize];
                        self.push_expr(PPEExpr::BinaryExpression(
                            binop,
                            Box::new(l_value),
                            Box::new(r_value),
                        ));
                    }
                    _ => {
                        self.offset += 1;
                        if func_def.args < 0 {
                            self.push_expr(PPEExpr::PredefinedFunctionCall(func_def, vec![]));
                        } else {
                            if (self.expr_stack.len() as i8) < func_def.args {
                                return Err(DeserializationErrorType::TooFewFunctionArguments(
                                    func_def.opcode,
                                    func_def.args,
                                    self.expr_stack.len(),
                                ));
                            }
                            let arguments = self
                                .expr_stack
                                .drain(self.expr_stack.len() - func_def.args as usize..)
                                .collect();
                            self.push_expr(PPEExpr::PredefinedFunctionCall(func_def, arguments));
                        }
                    }
                }
            }
        }

        match self.pop_expr() {
            Some(expr) => Ok(expr),
            None => Err(DeserializationErrorType::ExpressionStackEmpty),
        }
    }

    fn push_expr(&mut self, expr: PPEExpr) {
        self.expr_stack.push(expr);
    }

    fn pop_expr(&mut self) -> Option<PPEExpr> {
        self.expr_stack.pop()
    }

    fn read_variable_expression(
        &mut self,
        executable: &Executable,
    ) -> Result<PPEExpr, DeserializationErrorType> {
        let id = executable.script_buffer[self.offset];
        self.offset += 1;
        if self.offset >= executable.script_buffer.len() {
            return Err(DeserializationErrorType::IndexOutOfBounds);
        }
        let dim = executable.script_buffer[self.offset];
        self.offset += 1;

        if dim == 0 {
            return Ok(PPEExpr::Value(id as usize));
        }
        for _ in 0..dim {
            let e = self.deserialize_expression(executable)?;
            self.push_expr(e);
        }
        if self.expr_stack.len() < dim as usize {
            return Err(DeserializationErrorType::InvalidExpressionStackState);
        }
        let dims = self
            .expr_stack
            .drain(self.expr_stack.len() - dim as usize..)
            .collect();
        Ok(PPEExpr::Dim(id as usize, dims))
    }
}
