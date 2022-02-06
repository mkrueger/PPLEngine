#![feature(core_intrinsics)]
#![feature(core_panic)]
extern crate core;
extern crate nom;

use std::env;
use std::fs::File;
use std::io::Write;

mod decode;
mod tables;
mod executable;
mod decompiler;
mod parser;
mod interpreter;

// comment: Declaration::, pair(char(';'), is_not("\n\r")))(line)

fn main() {
    let mut args : Vec<String> = env::args().collect();

    if args.len() < 3 || args.len() > 4 {
        usage_error();
        return;
    }
    println!("pplengine Version 0.10 - PCBoard Programming Language Engine");
    println!();

    match args[1].as_str() {
        "-d" => {
            let file_name = args.get_mut(2).unwrap();
            if !file_name.ends_with(".ppe") {
                file_name.push_str(".ppe");
            }

            let d = crate::decompiler::Decompiler::read(&file_name);

            if args.len() == 3 {
                println!("{}", d.output);
                return;
            } else {
                let out_file_name = args.get_mut(3).unwrap();
                if !out_file_name.ends_with(".ppd") {
                    out_file_name.push_str(".ppd");
                }
                let cpy_file = out_file_name.as_str().clone();
                println!("output written to {}", out_file_name);
                let mut output = File::create(cpy_file).unwrap();
                write!(output, "{}", d.output).expect(format!("Error: Can't create {} on disk, aborting...", cpy_file).as_str());
            }
        },
        _ => {
        }
    }
}

fn usage_error()  {
    println!("USAGE: pplengine [OPTIONS]");
    println!("-d SRCNAME[.EXT] [DSTNAME[.EXT]] decompile .ppe file");
    println!("-c SRCNAME[.EXT] [DSTNAME[.EXT]] compile .ppl file to .ppe");
    println!("-r NAME[.EXT] Run PPE locally");
}
