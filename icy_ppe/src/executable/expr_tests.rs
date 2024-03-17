use crate::{
    ast::{Variable, VariableType},
    tables::{FuncOpCode, FUNCTION_DEFINITIONS},
};

use super::{Executable, PPEExpr, VariableEntry};

#[test]
fn test_value_serialization() {
    let val = PPEExpr::Value(42);
    test_serialize(&val, &[42, 0]);
}

#[test]
fn test_dim_serialization() {
    let val = PPEExpr::Dim(42, vec![PPEExpr::Value(2)]);
    test_serialize(&val, &[42, 1, 2, 0, 0]);

    let val = PPEExpr::Dim(42, vec![PPEExpr::Value(2), PPEExpr::Value(3)]);
    test_serialize(&val, &[42, 2, 2, 0, 0, 3, 0, 0]);

    let val = PPEExpr::Dim(
        42,
        vec![PPEExpr::Value(2), PPEExpr::Value(3), PPEExpr::Value(4)],
    );
    test_serialize(&val, &[42, 3, 2, 0, 0, 3, 0, 0, 4, 0, 0]);
}

#[test]
fn test_predefined_functions_serialization() {
    let i = -(FuncOpCode::PLUS as i32);
    let val = PPEExpr::PredefinedFunctionCall(
        &FUNCTION_DEFINITIONS[i as usize],
        vec![PPEExpr::Value(2), PPEExpr::Value(3)],
    );
    test_serialize(&val, &[2, 0, 3, 0, FuncOpCode::PLUS as i16]);


    let i = -(FuncOpCode::MID as i32);
    let val = PPEExpr::PredefinedFunctionCall(
        &FUNCTION_DEFINITIONS[i as usize],
        vec![PPEExpr::Value(2), PPEExpr::Value(3), PPEExpr::Value(4)],
    );
    test_serialize(&val, &[2, 0, 3, 0, 4, 0, FuncOpCode::MID as i16]);

}


#[test]
fn test_binary_expression_serialization() {
    let val = PPEExpr::BinaryExpression(
        crate::ast::BinOp::Add,
        Box::new(PPEExpr::Value(2)),
        Box::new(PPEExpr::Value(3)),
    );
    test_serialize(&val, &[2, 0, 3, 0, FuncOpCode::PLUS as i16]);
}

#[test]
fn test_unary_expression_serialization() {
    let val = PPEExpr::UnaryExpression(crate::ast::UnaryOp::Minus, Box::new(PPEExpr::Value(2)));
    test_serialize(&val, &[2, 0, FuncOpCode::UMINUS as i16]);
}

#[test]
fn test_function_call_serialization() {
    let val = PPEExpr::FunctionCall(6, vec![]);
    test_serialize(&val, &[6, 0, 0]);

    let val = PPEExpr::FunctionCall(55, vec![PPEExpr::Value(99)]);
    test_serialize(&val, &[55, 0, 99, 0, 0]);

    let val = PPEExpr::FunctionCall(12, vec![PPEExpr::Value(2), PPEExpr::Value(3)]);
    test_serialize(&val, &[12, 0, 2, 0, 3, 0, 0]);
}

fn test_serialize(val: &PPEExpr, expected: &[i16]) {
    assert_eq!(val.get_size(), expected.len());
    let mut result = Vec::new();
    val.serialize(&mut result);
    assert_eq!(result, expected);
}

#[test]
fn test_value_deserialization() {
    test_deserialization(&[2, 0], &PPEExpr::Value(2));
}

#[test]
fn test_dim_deserialization() {
    test_deserialization(&[2, 1, 2, 0, 0], &PPEExpr::Dim(2, vec![PPEExpr::Value(2)]));
    test_deserialization(
        &[2, 2, 2, 0, 0, 3, 0, 0],
        &PPEExpr::Dim(2, vec![PPEExpr::Value(2), PPEExpr::Value(3)]),
    );
    test_deserialization(
        &[2, 3, 2, 0, 0, 3, 0, 0, 4, 0, 0],
        &PPEExpr::Dim(
            2,
            vec![PPEExpr::Value(2), PPEExpr::Value(3), PPEExpr::Value(4)],
        ),
    );
}

#[test]
fn test_binary_functions_deserialization() {
    let val = PPEExpr::BinaryExpression(
        crate::ast::BinOp::Add,
        Box::new(PPEExpr::Value(2)),
        Box::new(PPEExpr::Value(3)),
    );
    test_deserialization(&[2, 0, 3, 0, FuncOpCode::PLUS as i16], &val);
}

#[test]
fn test_predefined_functions_deserialization() {
    let i: i32 = -(FuncOpCode::MID as i32);
    let val = PPEExpr::PredefinedFunctionCall(
        &FUNCTION_DEFINITIONS[i as usize],
        vec![PPEExpr::Value(1), PPEExpr::Value(2), PPEExpr::Value(3)],
    );
    test_deserialization( &[1, 0, 2, 0, 3, 0, FuncOpCode::MID as i16], &val);
}

fn test_deserialization(script: &[i16], expected: &PPEExpr) {
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
    let expr = deserializer.deserialize_expression(&exe).unwrap();

    assert_eq!(expr, *expected);
    assert_eq!(expr.get_size(), exe.script_buffer.len());

    assert_eq!(deserializer.offset, exe.script_buffer.len());
}
