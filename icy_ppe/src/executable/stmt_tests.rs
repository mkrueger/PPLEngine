use crate::{
    ast::{Variable, VariableType},
    executable::{Executable, VariableEntry},
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

fn test_serialize(val: &PPECommand, expected: &[i16]) {
    let mut result = Vec::new();
    val.serialize(&mut result);
    assert_eq!(val.get_size(), result.len());
    assert_eq!(result, expected);
}

#[test]
fn test_end_deserialization() {
    let val = PPECommand::End;
    test_deserialize(&[OpCode::END as i16], &val);
}

#[test]
fn test_return_deserialization() {
    let val = PPECommand::Return;
    test_deserialize(&[OpCode::RETURN as i16], &val);
}

#[test]
fn test_fend_deserialization() {
    let val = PPECommand::EndFunc;
    test_deserialize(&[OpCode::FEND as i16], &val);
}

#[test]
fn test_fpclr_deserialization() {
    let val = PPECommand::EndProc;
    test_deserialize(&[OpCode::FPCLR as i16], &val);
}

#[test]
fn test_stop_deserialization() {
    let val = PPECommand::Stop;
    test_deserialize(&[OpCode::STOP as i16], &val);
}

#[test]
fn test_predef_deserialization() {
    let val = PPECommand::PredefinedCall(
        OpCode::ANSIPOS.get_definition(),
        vec![PPEExpr::Value(2), PPEExpr::Value(3)],
    );
    test_deserialize(&[OpCode::ANSIPOS as i16, 2, 0, 0, 3, 0, 0], &val);
}

#[test]
fn test_let_deserialization() {
    let val = PPECommand::Let(Box::new(PPEExpr::Value(2)), Box::new(PPEExpr::Value(3)));
    test_deserialize(&[OpCode::LET as i16, 2, 0, 3, 0, 0], &val);

    let val = PPECommand::Let(
        Box::new(PPEExpr::Dim(1, vec![PPEExpr::Value(2)])),
        Box::new(PPEExpr::Value(3)),
    );
    test_deserialize(&[OpCode::LET as i16, 1, 1, 2, 0, 0, 3, 0, 0], &val);
}

fn test_deserialize(script: &[i16], expected: &PPECommand) {
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
    exe.script_buffer = script.to_vec();
    let mut deserializer = super::PPEDeserializer::default();
    let stmt = deserializer.deserialize_statement(&exe).unwrap().unwrap();
    assert_eq!(stmt.get_size(), exe.script_buffer.len());
    assert_eq!(stmt, *expected);
    assert_eq!(deserializer.offset, exe.script_buffer.len());
}
