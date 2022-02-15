use std::{fs, path::Path, ffi::OsStr};

use argh::FromArgs;
use ppl_engine::parser::parse_program;

#[derive(FromArgs)]
/// original by Clark Development Company, Inc. rewritten in rust https://github.com/mkrueger/PPLEngine 
struct Arguments {
    /// srcname[.ext] .ext defaults to .ppe
    #[argh(positional)]
    file_name: String,
}

fn main() {
    println!("PPLC Version 0.01 - PCBoard Programming Language Compiler reborn");
    let arguments: Arguments = argh::from_env();
    
    let mut file_name = arguments.file_name;

    let extension = Path::new(&file_name).extension().and_then(OsStr::to_str);
    if let None = extension {
        file_name.push_str(".pps");
    }

    let content = fs::read_to_string(file_name).unwrap();
    
    let prg = parse_program(&content);

    println!("parse result: {:?}", prg);
}

