#![feature(core_intrinsics)]
#![feature(core_panic)]
#![feature(unchecked_math)]
extern crate core;
extern crate nom;
extern crate lazy_static;

use std::env;
use std::io::*;

mod crypt;
mod tables;
mod executable;
mod decompiler;
mod parser;
mod interpreter;

// comment: Declaration::, pair(char(';'), is_not("\n\r")))(line)

fn main() {
  /*  let mut data_path =env::current_dir().unwrap();
    data_path.push("test_data");

    data_path.push("if_then_else.ppe");
    let d = crate::decompiler::Decompiler::read(data_path.to_str().unwrap());



    println!("debug:");
    print!("{}", d.to_string());
    println!();
*/
    let args : Vec<String> = env::args().collect();

    let arg_len = args.len();
    if arg_len < 3 || arg_len > 4 {
        usage_error();
        return;
    }
    match args[1].as_str() {
        "-d" => {
            let mut file_name = args[2].clone();
            if !file_name.ends_with(".ppe") {
                file_name.push_str(".ppe");
            }

            let d = crate::decompiler::Decompiler::read(&file_name);

            if arg_len == 3 {
                println!("{}", d.to_string());
                return;
            } else {
                let mut out_file_name = args[3].clone();
                if !out_file_name.ends_with(".ppd") {
                    out_file_name.push_str(".ppd");
                }
                let cpy_file = out_file_name.as_str().clone();
                println!("output written to {}", out_file_name);
                let mut output = std::fs::File::create(cpy_file).unwrap();
                write!(output, "{}", d.to_string()).expect(format!("Error: Can't create {} on disk, aborting...", cpy_file).as_str());
            }
        },
        "-r" => {
            panic!("Not implemented");
        }
        "-c" => {
            panic!("Not production ready.");
        }
        _ => {
            usage_error();
        }
    }
}

fn usage_error()  {
    println!("pplengine Version 0.10 - PCBoard Programming Language Engine");
    println!();
    println!("USAGE: pplengine [OPTIONS]");
    println!("-d SRCNAME[.EXT] [DSTNAME[.EXT]] decompile .ppe file");
    println!("-c SRCNAME[.EXT] [DSTNAME[.EXT]] compile .pps file to .ppe (Not yet implemented)");
    println!("-r NAME[.EXT] Run PPE locally");
}
