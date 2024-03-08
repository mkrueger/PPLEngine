#![warn(clippy::all, clippy::pedantic)]
#![allow(
    clippy::must_use_candidate,
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::too_many_lines,
    clippy::cast_lossless,
    clippy::cast_precision_loss
)]
use ast::Statement;

extern crate core;
extern crate lazy_static;

pub mod ast;
pub mod compiler;
pub mod crypt;
pub mod decompiler;
pub mod executable;
pub mod parser;
pub mod tables;

#[derive(PartialEq, Debug)]
pub enum OutputFunc {
    Upper,
    Lower,
    CamelCase,
}

pub static mut DEFAULT_OUTPUT_FUNC: OutputFunc = OutputFunc::Upper;

fn output_keyword(str: &str) -> String {
    unsafe {
        match DEFAULT_OUTPUT_FUNC {
            OutputFunc::Upper => str.to_uppercase(),
            OutputFunc::Lower => str.to_lowercase(),
            OutputFunc::CamelCase => str.to_string(),
        }
    }
}

fn output_keyword_indented(indent: i32, str: &str) -> String {
    let mut indent = Statement::get_indent(indent);

    unsafe {
        indent.push_str(
            match DEFAULT_OUTPUT_FUNC {
                OutputFunc::Upper => str.to_uppercase(),
                OutputFunc::Lower => str.to_lowercase(),
                OutputFunc::CamelCase => str.to_string(),
            }
            .as_str(),
        );
    }

    indent
}
