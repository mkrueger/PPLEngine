#![feature(core_intrinsics)]
#![feature(core_panic)]
extern crate core;

use std::env;

mod decode;
mod tables;
mod executable;
mod decompiler;

fn main() {
    let file_name = "./test_data/declare_procedure.ppe";
    println!("{}", env::current_dir().unwrap().to_str().unwrap());
    let d = crate::decompiler::Decompiler::read(&file_name);
    println!("{}", d.output);
}
