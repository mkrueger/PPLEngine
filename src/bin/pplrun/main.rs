use ppl_engine::decompiler::*;

mod console_context;
use console_context::*;
use argh::FromArgs;

#[derive(FromArgs)]
/// https://github.com/mkrueger/PPLEngine 
struct Arguments {
    /// srcname[.ext] .ext defaults to .ppe
    #[argh(positional)]
    file_name: String,
}

fn main() {
    println!("PPLRUN Version 0.01 - PCBoard Programming Language Runner");
    let arguments: Arguments = argh::from_env();

    let mut file_name = arguments.file_name;
    if !file_name.ends_with(".pps") {
        file_name.push_str(".pps");
    }
    let prg = load_file(&file_name);

    ppl_engine::interpreter::run(&prg, &mut ConsoleContext{});
}
