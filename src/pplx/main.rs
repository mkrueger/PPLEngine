use argh::FromArgs;
use icy_ppe::icy_board::data::IcyBoardData;
use icy_ppe::icy_board::data::Node;
use icy_ppe::icy_board::data::PcbDataType;
use icy_ppe::icy_board::data::UserRecord;
use icy_ppe::interpreter::run;
use icy_ppe::interpreter::DiskIO;
use icy_ppe::*;
use semver::Version;
use std::default;
use std::ffi::OsStr;
use std::fs::*;
use std::io::*;
use std::path::Path;

use crate::output::Output;
mod output;

#[derive(FromArgs)]
/// original by chicken/tools4fools https://github.com/astuder/ppld rust port https://github.com/mkrueger/PPLEngine
struct Arguments {
    /// raw ppe without reconstruction control structures
    #[argh(switch, short = 'r')]
    raw: bool,

    #[argh(option, short = 's')]
    /// keyword casing style, valid values are u=upper (default), l=lower, c=camel
    style: Option<char>,

    /// srcname[.ext] .ext defaults to .ppe
    #[argh(positional)]
    srcname: String,

    /// dstname[.ext] .ext defaults to .ppd
    #[argh(positional)]
    dstname: Option<String>,
}

lazy_static::lazy_static! {
    static ref VERSION: Version = Version::parse(env!("CARGO_PKG_VERSION")).unwrap();
}

fn main() {
    println!(
        "PPLX Version {} - PCBoard Programming Language Runtime",
        *crate::VERSION
    );
    let mut arguments: Arguments = argh::from_env();

    unsafe {
        match arguments.style {
            Some('u') => DEFAULT_OUTPUT_FUNC = OutputFunc::Upper,
            Some('l') => DEFAULT_OUTPUT_FUNC = OutputFunc::Lower,
            Some('c') => DEFAULT_OUTPUT_FUNC = OutputFunc::CamelCase,
            Some(x) => panic!("unsupported keyword style {}", x),
            None => {}
        }
    }

    let file_name = &mut arguments.srcname;

    let extension = Path::new(&file_name).extension().and_then(OsStr::to_str);
    if extension.is_none() {
        file_name.push_str(".ppe");
    }
    let users = vec![
        UserRecord {
            name: "sysop".to_string(),
            password: "sysop".to_string(),
            security_level: 110,
            ..Default::default()
        },
        UserRecord {
            name: "guest".to_string(),
            password: "guest".to_string(),
            security_level: 10,
            ..Default::default()
        },
    ];
    
    let mut pcb_data = PcbDataType::default();
    let mut icy_board_data = IcyBoardData {
        users,
        nodes: vec![Node::default()],
        pcb_data,
        pcb_text: Vec::new(),
        yes_char: 'Y',
        no_char: 'N',
    };
    icy_board_data.load_data();

    let prg = icy_ppe::decompiler::load_file(file_name);
    let mut io = DiskIO::new("/home/mkrueger/work/pcx_board");
    let mut connection = Output::default();
    run(&prg, &mut connection, &mut io, icy_board_data).unwrap();


}
