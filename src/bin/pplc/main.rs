use argh::FromArgs;

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
    if !file_name.to_lowercase().ends_with(".pps") {
        file_name.push_str(".pps");
    }
    println!("parse {}", file_name);
}

