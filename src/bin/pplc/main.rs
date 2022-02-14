use std::env;

fn main() {
    println!("PPLC Version 0.01 - PCBoard Programming Language Compiler reborn");
    println!(" original by Clark Development Company, Inc. rewritten in rust https://github.com/mkrueger/PPLEngine");
    println!();

    let mut args : Vec<String> = env::args().collect();
    if args.len() != 2 {
        usage_error();
        return;
    }

    let file_name = args.get_mut(1).unwrap();
    if !file_name.ends_with(".pps") {
        file_name.push_str(".pps");
    }
}

fn usage_error()  {
    println!("usage: pplc srcname[.ext]");
    println!();
    println!(" note: .ext defaults to .pps if not specified");
}
