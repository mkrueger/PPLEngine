pub use ast_transform::*;
pub mod ast_transform;

/*
use crate::ast::{Program, Declaration};

static PREAMBLE: &[u8] = b"PCBoard Programming Language Executable  3.30\x0D\x0A\x1A";

fn create_var_info(id: usize, d: &Declaration)
{
    let mut result = Vec::new();

    result.extend(u16::to_le_bytes(id as u16));

    match d {
        Declaration::Variable(var_type, vars) => todo!(),
        Declaration::Function(_, _, _) => todo!(),
        Declaration::Procedure(_, _) => todo!(),
    }

    result
}

pub fn compile(prg: &Program) -> Vec<u8>
{
    let mut result = Vec::new();

    result.extend(PREAMBLE);

    let mut o = result.len();
    for id in 0..prg.declarations.len() {
        let cur = &prg.declarations[id];
    }

    result
}*/
