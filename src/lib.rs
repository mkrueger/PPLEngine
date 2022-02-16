#![feature(core_intrinsics)]
#![feature(core_panic)]
#![feature(unchecked_math)]

use argh::FromArgs;
extern crate core;
extern crate nom;
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