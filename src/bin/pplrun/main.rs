use std::env;
use ppl_engine::decompiler::*;

mod console_context;
use console_context::*;

fn main() {
    println!("PPLRUN Version 0.01 - PCBoard Programming Language Runner");
    println!("https://github.com/mkrueger/PPLEngine");
    println!();

    let mut args : Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 3 {
        usage_error();
        return;
    }

    let file_name = args.get_mut(1).unwrap();
    if !file_name.ends_with(".ppe") {
        file_name.push_str(".ppe");
    }

    let prg = load_file(&file_name);

    ppl_engine::interpreter::run(&prg, &mut ConsoleContext{});
}

fn usage_error()  {
    println!("usage: pplrun [options] srcname[.ext]");
    println!(" note: .ext defaults to .ppe if not specified");
}
