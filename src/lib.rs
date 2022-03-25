#![feature(core_intrinsics)]
#![feature(core_panic)]
#![feature(unchecked_math)]
#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::cast_sign_loss, clippy::cast_possible_truncation, clippy::cast_possible_wrap, clippy::too_many_lines, clippy::cast_lossless, clippy::cast_precision_loss)]

extern crate core;
extern crate lazy_static;

mod crypt;
mod ast;
pub mod tables;
pub mod executable;
pub mod decompiler;
pub mod parser;
pub mod interpreter;

#[derive(PartialEq, Debug)]
pub enum OutputFunc {
    Upper,
    Lower,
    CamelCase
}

pub static mut DEFAULT_OUTPUT_FUNC : OutputFunc = OutputFunc::Upper;

fn output_keyword(str: &str) -> String
{
    unsafe {
        match DEFAULT_OUTPUT_FUNC {
            OutputFunc::Upper => str.to_uppercase(),
            OutputFunc::Lower => str.to_lowercase(),
            OutputFunc::CamelCase => str.to_string()
        }
    }
}