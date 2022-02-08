#![feature(core_intrinsics)]
#![feature(core_panic)]
extern crate core;
extern crate nom;

mod decode;
mod tables;
mod executable;
mod decompiler;
mod parser;
mod interpreter;

// comment: Declaration::, pair(char(';'), is_not("\n\r")))(line)

fn main() {
 /*delimited(
        ,
        space1,
        identifier
    )(line);*/
    /*
    let file_name = "./test_data/declare_procedure.ppe";
    println!("{}", env::current_dir().unwrap().to_str().unwrap());
    let d = crate::decompiler::Decompiler::read(&file_name);
    println!("{}", d.output);*/
}
