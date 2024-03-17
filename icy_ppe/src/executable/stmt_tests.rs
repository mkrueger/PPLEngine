use crate::{
    ast::{BinOp, Variable, VariableType},
    executable::{Executable, FunctionValue, VariableEntry},
    tables::{FuncOpCode, FUNCTION_DEFINITIONS},
};

use super::{OpCode, PPECommand, PPEExpr};

#[test]
fn test_end_serialization() {
    let val = PPECommand::End;
    test_serialize(&val, &[OpCode::END as i16]);
}

#[test]
fn test_return_serialization() {
    let val = PPECommand::Return;
    test_serialize(&val, &[OpCode::RETURN as i16]);
}

#[test]
fn test_fend_serialization() {
    let val = PPECommand::EndFunc;
    test_serialize(&val, &[OpCode::FEND as i16]);
}

#[test]
fn test_fpclr_serialization() {
    let val = PPECommand::EndProc;
    test_serialize(&val, &[OpCode::FPCLR as i16]);
}

#[test]
fn test_stop_serialization() {
    let val = PPECommand::Stop;
    test_serialize(&val, &[OpCode::STOP as i16]);
}

#[test]
fn test_predef_serialization() {
    let val = PPECommand::PredefinedCall(
        OpCode::ANSIPOS.get_definition(),
        vec![PPEExpr::Value(2), PPEExpr::Value(3)],
    );
    test_serialize(&val, &[OpCode::ANSIPOS as i16, 2, 0, 0, 3, 0, 0]);
}

#[test]
fn test_let_serialization() {
    let val = PPECommand::Let(Box::new(PPEExpr::Value(2)), Box::new(PPEExpr::Value(3)));
    test_serialize(&val, &[OpCode::LET as i16, 2, 0, 3, 0, 0]);

    let val = PPECommand::Let(
        Box::new(PPEExpr::Dim(1, vec![PPEExpr::Value(2)])),
        Box::new(PPEExpr::Value(3)),
    );
    test_serialize(&val, &[OpCode::LET as i16, 1, 1, 2, 0, 0, 3, 0, 0]);
}

#[test]
fn test_print_midserialization() {
    let left = PPEExpr::FunctionCall(6, vec![PPEExpr::Value(2)]);
    let i: i32 = -(FuncOpCode::MID as i32);
    let right = PPEExpr::PredefinedFunctionCall(
        &FUNCTION_DEFINITIONS[i as usize],
        vec![PPEExpr::Value(3), PPEExpr::Value(2), PPEExpr::Value(2)],
    );

    let val = PPECommand::PredefinedCall(
        OpCode::PRINT.get_definition(),
        vec![PPEExpr::BinaryExpression(
            BinOp::Add,
            Box::new(left),
            Box::new(right),
        )],
    );
    test_serialize(&val, &[9, 1, 6, 0, 2, 0, 0, 3, 0, 2, 0, 2, 0, -23, -8, 0]);
}

fn test_serialize(val: &PPECommand, expected: &[i16]) {
    let mut result = Vec::new();
    val.serialize(&mut result);
    assert_eq!(
        val.get_size(),
        result.len(),
        "Serialization length differs expected: {expected:?}, got: {result:?}"
    );
    assert_eq!(
        result, expected,
        "Serialization result differs expected: {expected:?}, got: {result:?}"
    );
    test_deserialize(val, &result);
}

fn test_deserialize(expected: &PPECommand, script: &[i16]) {
    let mut exe = Executable::default();
    for i in 0..5 {
        exe.variable_table.push(VariableEntry {
            name: format!("int{i}"),
            value: Variable::new_int(i),
            header: super::VarHeader {
                id: i as usize + 1,
                variable_type: VariableType::Integer,
                ..Default::default()
            },
            entry_type: super::EntryType::Constant,
            number: 0,
            function_id: 0,
        });
    }
    let func = FunctionValue {
        parameters: 1,
        local_variables: 1,
        start_offset: 1,
        first_var_id: 5,
        return_var: 6,
    };

    exe.variable_table.push(VariableEntry {
        name: "func1".to_string(),
        value: Variable {
            vtype: VariableType::Function,
            data: func.to_data(),
            ..Default::default()
        },
        header: super::VarHeader {
            id: 6,
            variable_type: VariableType::Function,
            ..Default::default()
        },
        entry_type: super::EntryType::Constant,
        number: 0,
        function_id: 0,
    });

    exe.script_buffer = script.to_vec();
    let mut deserializer = super::PPEDeserializer::default();
    let result = deserializer.deserialize_statement(&exe).unwrap().unwrap();
    // assert_eq!(result.get_size(), exe.script_buffer.len(), "Deserialization length differs expected: {expected:?}, got: {result:?}");
    assert_eq!(
        result, *expected,
        "Deserialization result differs expected: {expected:?}, got: {result:?}"
    );
    assert_eq!(
        deserializer.offset,
        exe.script_buffer.len(),
        "Deserialization offset differs expected: {expected:?}, got: {result:?}"
    );
}
