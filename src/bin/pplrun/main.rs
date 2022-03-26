use std::{path::Path, ffi::OsStr, fs};

use ppl_engine::{decompiler::*, interpreter::MemoryIO};

mod console_context;
use console_context::*;
use argh::FromArgs;

#[derive(FromArgs)]
/// https://github.com/mkrueger/PPLEngine 
struct Arguments {
    /// srcname[.ext] .ext defaults to .ppe
    #[argh(positional)]
    file_name: String,
}

fn main() {
    println!("PPLRUN Version 0.01 - PCBoard Programming Language Runner");
    let arguments: Arguments = argh::from_env();

    let mut file_name = arguments.file_name;
    let extension = Path::new(&file_name).extension().and_then(OsStr::to_str);
    let is_ppe = if extension.is_none() {
        file_name.push_str(".ppe");
        true
    } else {
        extension.unwrap() == "ppe"
    };

    let prg = if is_ppe {
        load_file(&file_name)
    } else {
        let content = fs::read_to_string(&file_name).unwrap();
        let mut prg = ppl_engine::parser::parse_program(&content);
        ppl_engine::compiler::transform_ast(&mut prg);
        prg
    };
    let mut io = MemoryIO::new();

    ppl_engine::interpreter::run(&prg, &mut ConsoleContext{}, &mut io);
}
