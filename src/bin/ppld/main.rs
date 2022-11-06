use std::ffi::OsStr;
use std::fs::*;
use std::io::*;
use std::path::Path;
use argh::FromArgs;
use ppl_engine::*;

#[derive(FromArgs)]
/// original by chicken/tools4fools https://github.com/astuder/ppld rust port https://github.com/mkrueger/PPLEngine
struct Arguments {

    /// raw ppe without reconstruction control structures
    #[argh(switch, short = 'r')]
    raw: bool,

    #[argh(option, short = 's')]
    /// keyword casing style, valid values are u=upper (default), l=lower, c=camel
    style: Option<char>,

    /// srcname[.ext] .ext defaults to .ppe
    #[argh(positional)]
    srcname: String,

    /// dstname[.ext] .ext defaults to .ppd
    #[argh(positional)]
    dstname: Option<String>,
}

fn main() {
    println!("PPLD Version 4.00alpha - PCBoard Programming Language Decompiler");
    let mut arguments: Arguments = argh::from_env();

    unsafe {
        match arguments.style {
            Some('u') => DEFAULT_OUTPUT_FUNC = OutputFunc::Upper,
            Some('l') => DEFAULT_OUTPUT_FUNC = OutputFunc::Lower,
            Some('c') => DEFAULT_OUTPUT_FUNC = OutputFunc::CamelCase,
            Some(x) => panic!("unsupported keyword style {}", x),
            None => {},
        }
    }

    let file_name = &mut arguments.srcname;

    let extension = Path::new(&file_name).extension().and_then(OsStr::to_str);
    if extension.is_none() {
        file_name.push_str(".ppe");
    }

    if let Some(out_file_name) = &mut arguments.dstname  {
        let extension = Path::new(&out_file_name).extension().and_then(OsStr::to_str);
        if extension.is_none() {
                out_file_name.push_str(".ppd");
        }
        let d = ppl_engine::decompiler::decompile(file_name, true, arguments.raw);
        let cpy_file = out_file_name.as_str();
        println!("output written to {}", out_file_name);
        let mut output = File::create(cpy_file).unwrap();
        write!(output, "{}", d).unwrap_or_else(|_| panic!("Error: Can't create {} on disk, aborting...", cpy_file));
    } else {
        let d = ppl_engine::decompiler::decompile(file_name, false, arguments.raw);
        println!("{}", d);
    }
}
