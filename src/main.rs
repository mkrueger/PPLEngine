#![feature(core_intrinsics)]
#![feature(core_panic)]
extern crate core;

use std::env;
use std::fs::*;
use std::io::*;

mod decode;
mod tables;
mod executable;
mod decompiler;

fn main() {
    let mut args : Vec<String> = env::args().collect();

    if args.len() < 2 || args.len() > 3 {
        usage_error();
        return;
    }
    println!("ppld Version 3.20 - PCBoard Programming Language Decompiler");
    println!(" original by chicken/tools4fools https://github.com/astuder/ppld rust port https://github.com/mkrueger/PPLEngine/tree/dev/ppld/3.20");
    println!();

    let file_name = args.get_mut(1).unwrap();
    if !file_name.ends_with(".ppe") {
        file_name.push_str(".ppe");
    }

    let d = crate::decompiler::Decompiler::read(&file_name);

    if args.len() == 2  {
        println!("{}", d.output);
        return;
    } else {
        let out_file_name = args.get_mut(2).unwrap();
        if !out_file_name.ends_with(".ppd") {
            out_file_name.push_str(".ppd");
        }
        let cpy_file = out_file_name.as_str().clone();
        println!("output written to {}", out_file_name);
        let mut output = File::create(cpy_file).unwrap();
        write!(output, "{}", d.output).expect(format!("Error: Can't create {} on disk, aborting...", cpy_file).as_str());
    }
}

fn usage_error()  {
    println!("USAGE: PPLD [OPTIONS] SRCNAME[.EXT] [DSTNAME[.EXT]]");
    println!("NOTE: .EXT defaults to .PPE and .PPD if not specified");
    //println!("Options:");
    //println!("/NOTRACE - Disable Trace Mode");
    //println!("/SYMBOLIC - Create Symbolic File (For PPLDebug)");
}
