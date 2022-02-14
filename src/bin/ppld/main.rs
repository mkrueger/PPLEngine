use std::env;
use std::fs::*;
use std::io::*;

fn main() {
    println!("PPLD Version 4.00alpha - PCBoard Programming Language Decompiler");
    println!(" original by chicken/tools4fools https://github.com/astuder/ppld rust port https://github.com/mkrueger/PPLEngine");
    println!();

    let mut args : Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 3 {
        usage_error();
        return;
    }

    let to_file = args.len() == 3;

    let file_name = args.get_mut(1).unwrap();
    if !file_name.ends_with(".ppe") {
        file_name.push_str(".ppe");
    }

    let d = ppl_engine::decompiler::decompile(file_name, to_file);

    if to_file  {
        let out_file_name = args.get_mut(2).unwrap();
        if !out_file_name.ends_with(".ppd") {
            out_file_name.push_str(".ppd");
        }
        let cpy_file = out_file_name.as_str().clone();
        println!("output written to {}", out_file_name);
        let mut output = File::create(cpy_file).unwrap();
        write!(output, "{}", d.to_string()).expect(format!("Error: Can't create {} on disk, aborting...", cpy_file).as_str());
    } else {
        println!("{}", d.to_string());
        return;
    }
}

fn usage_error()  {
    println!("usage: ppld [options] srcname[.ext] [dstname[.ext]]");
    println!(" note: .ext defaults to .ppe and .ppd if not specified");
    //println!("Options:");
    //println!("/NOTRACE - Disable Trace Mode");
    //println!("/SYMBOLIC - Create Symbolic File (For PPLDebug)");
}
