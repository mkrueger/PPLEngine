#![feature(core_intrinsics)]
#![feature(core_panic)]
#![feature(unchecked_math)]
extern crate core;
extern crate nom;
extern crate lazy_static;

use std::env;

mod decode;
mod tables;
mod executable;
mod decompiler;
mod parser;
mod interpreter;

// comment: Declaration::, pair(char(';'), is_not("\n\r")))(line)

fn main() {
    let mut data_path =env::current_dir().unwrap();
    data_path.push("test_data");

    data_path.push("while.ppe");
    let d = crate::decompiler::Decompiler::read(data_path.to_str().unwrap());

  //  let d = crate::decompiler::Decompiler::read("/home/mkrueger/Downloads/PPL/CNFNNEW/CNFN.PPE");

    println!("debug:");
    print!("{}", d.to_string());
    println!();

    /*
    let args : Vec<String> = env::args().collect();

    let argLen = args.len();
    if argLen < 3 || argLen > 4 {
        usage_error();
        return;
    }
    println!("pplengine Version 0.10 - PCBoard Programming Language Engine");
    println!();

    match args[1].as_str() {
        "-d" => {
            let mut file_name = args[2].clone();
            if !file_name.ends_with(".ppe") {
                file_name.push_str(".ppe");
            }

            let d = crate::decompiler::Decompiler::read(&file_name);

            if argLen == 3 {
                println!("{}", d.to_string());
                return;
            } else {
                let mut out_file_name = args[3].clone();
                if !out_file_name.ends_with(".ppd") {
                    out_file_name.push_str(".ppd");
                }
                let cpy_file = out_file_name.as_str().clone();
                println!("output written to {}", out_file_name);
                let mut output = File::create(cpy_file).unwrap();
                write!(output, "{}", d.to_string()).expect(format!("Error: Can't create {} on disk, aborting...", cpy_file).as_str());
            }
        },
        _ => {
        }
    }*/
}

fn usage_error()  {
    println!("USAGE: pplengine [OPTIONS]");
    println!("-d SRCNAME[.EXT] [DSTNAME[.EXT]] decompile .ppe file");
    println!("-c SRCNAME[.EXT] [DSTNAME[.EXT]] compile .pps file to .ppe");
    println!("-r NAME[.EXT] Run PPE locally");
}
