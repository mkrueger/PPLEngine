use std::fs::*;
use std::io::*;
use argh::FromArgs;

#[derive(FromArgs)]
/// original by chicken/tools4fools https://github.com/astuder/ppld rust port https://github.com/mkrueger/PPLEngine
struct Arguments {
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

    let mut file_name = &mut arguments.srcname;
    if !file_name.ends_with(".ppe") {
        file_name.push_str(".ppe");
    }

    if let Some(out_file_name) = &mut arguments.dstname  {
        if !out_file_name.ends_with(".ppd") {
            out_file_name.push_str(".ppd");
        }
        let d = ppl_engine::decompiler::decompile(file_name, true);
        let cpy_file = out_file_name.as_str().clone();
        println!("output written to {}", out_file_name);
        let mut output = File::create(cpy_file).unwrap();
        write!(output, "{}", d.to_string()).expect(format!("Error: Can't create {} on disk, aborting...", cpy_file).as_str());
    } else {
        let d = ppl_engine::decompiler::decompile(file_name, false);
        println!("{}", d.to_string());
        return;
    }
}
