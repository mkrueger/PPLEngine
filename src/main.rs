#![feature(core_intrinsics)]
#![feature(core_panic)]
extern crate core;

mod decode;
mod tables;
mod executable;
mod decompiler;

struct Funcl
{
    label : i16,
    func : i16
}

fn main() {
    let file_name = "/home/mkrueger/work/PCBoard/C/BADBOY.PPE";
    crate::decompiler::Decompiler::read(&file_name);
}
