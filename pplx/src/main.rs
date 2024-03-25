use clap::Parser;
use crossterm::cursor::MoveTo;
use crossterm::ExecutableCommand;
use icy_ppe::executable::read_file;
use icy_ppe::icy_board::data::IcyBoardData;
use icy_ppe::icy_board::data::Node;
use icy_ppe::icy_board::state::IcyBoardState;
use icy_ppe::icy_board::users::UserRecord;
use icy_ppe::vm::run;
use icy_ppe::vm::DiskIO;
use semver::Version;
use std::ffi::OsStr;
use std::io::stdout;
use std::path::Path;
use std::path::PathBuf;

use crate::output::Output;
use crossterm::event::{KeyboardEnhancementFlags, PushKeyboardEnhancementFlags};
use crossterm::queue;
mod output;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// If set, the executable will run in sysop mode
    #[arg(short, long, default_value_t = false)]
    sysop: bool,

    /// file[.ppe] to run
    input: String,
}

lazy_static::lazy_static! {
    static ref VERSION: Version = Version::parse(env!("CARGO_PKG_VERSION")).unwrap();
}

fn main() {
    let arguments = Args::parse();

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

    let exe = read_file(&file_name, false).unwrap();
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
    )
    .unwrap();

    // disable_raw_mode().unwrap();
}
