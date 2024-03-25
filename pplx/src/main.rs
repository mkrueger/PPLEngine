use argh::FromArgs;
use crossterm::cursor::MoveTo;
use crossterm::ExecutableCommand;
use icy_ppe::executable::Executable;
use icy_ppe::icy_board::data::IcyBoardData;
use icy_ppe::icy_board::data::Node;
use icy_ppe::icy_board::state::IcyBoardState;
use icy_ppe::icy_board::users::UserRecord;
use semver::Version;
use std::ffi::OsStr;
use std::io::stdout;
use std::path::Path;
use std::path::PathBuf;
use vm::run;
use vm::DiskIO;

use crate::output::Output;
use crossterm::event::{KeyboardEnhancementFlags, PushKeyboardEnhancementFlags};
use crossterm::queue;
mod output;
pub mod vm;

#[derive(FromArgs)]
#[argh(description = "PCBoard Programming Language Execution Environment")]
struct Args {
    /// if set, the executable will run in sysop mode
    #[argh(switch, short = 's')]
    sysop: bool,

    /// file[.ppe] to run
    #[argh(positional)]
    input: String,
}

lazy_static::lazy_static! {
    static ref VERSION: Version = Version::parse(env!("CARGO_PKG_VERSION")).unwrap();
}

fn main() {
    let arguments: Args = argh::from_env();
    let mut file_name = arguments.input;
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

    let pcb_data = IcyBoardData::default();
    let icy_board_data = IcyBoardState {
        users,
        nodes: vec![Node::default()],
        data: pcb_data,
        yes_char: 'Y',
        no_char: 'N',
        display_text: true,
        ..Default::default()
    };

    let exe = Executable::read_file(&file_name, false).unwrap();
    let mut io = DiskIO::new(".");
    let mut output = Output::default();

    output.is_sysop = arguments.sysop;
    // enable_raw_mode().unwrap();
    let supports_keyboard_enhancement = matches!(
        crossterm::terminal::supports_keyboard_enhancement(),
        Ok(true)
    );

    if supports_keyboard_enhancement {
        queue!(
            stdout(),
            PushKeyboardEnhancementFlags(
                KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
                    | KeyboardEnhancementFlags::REPORT_ALL_KEYS_AS_ESCAPE_CODES
                    | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS
                    | KeyboardEnhancementFlags::REPORT_EVENT_TYPES
            )
        )
        .unwrap();
    }

    stdout()
        .execute(crossterm::terminal::Clear(
            crossterm::terminal::ClearType::All,
        ))
        .unwrap()
        .execute(MoveTo(0, 0))
        .unwrap();
    run(
        PathBuf::from(file_name),
        &exe,
        &mut output,
        &mut io,
        icy_board_data,
        arguments.sysop,
    )
    .unwrap();

    // disable_raw_mode().unwrap();
}
