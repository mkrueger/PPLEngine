use clap::Parser;
use crossterm::cursor::MoveTo;
use crossterm::execute;
use crossterm::style::Attribute;
use crossterm::style::Color;
use crossterm::style::Print;
use crossterm::style::SetAttribute;
use crossterm::style::SetForegroundColor;
use crossterm::terminal::disable_raw_mode;
use crossterm::terminal::enable_raw_mode;
use crossterm::ExecutableCommand;
use icy_board_engine::icy_board::data::IcyBoardData;
use icy_board_engine::icy_board::data::Node;
use icy_board_engine::icy_board::state::IcyBoardState;
use icy_board_engine::icy_board::users::UserRecord;
use icy_board_engine::icy_board::User;
use icy_board_engine::vm::run;
use icy_board_engine::vm::DiskIO;
use icy_ppe::executable::Executable;
use semver::Version;
use std::ffi::OsStr;
use std::io::stdout;
use std::path::Path;
use std::path::PathBuf;

use crate::output::Output;
use crossterm::event::{KeyboardEnhancementFlags, PushKeyboardEnhancementFlags};
use crossterm::queue;
mod output;

#[derive(clap::Parser)]
#[command(version="", about="PCBoard Programming Language Execution Environment", long_about = None)]
struct Cli {
    /// if set, the executable will run in sysop mode
    #[arg(long, short)]
    sysop: bool,

    /// file[.ppe] to run
    file: String,
}

lazy_static::lazy_static! {
    static ref VERSION: Version = Version::parse(env!("CARGO_PKG_VERSION")).unwrap();
}

fn main() {
    let arguments = Cli::parse();
    let mut file_name = arguments.file;
    let extension = Path::new(&file_name).extension().and_then(OsStr::to_str);
    if extension.is_none() {
        file_name.push_str(".ppe");
    }
    let users = vec![
        User {
            user: UserRecord {
                name: "sysop".to_string(),
                password: "sysop".to_string(),
                security_level: 110,
                ..Default::default()
            },
            inf: Default::default(),
        },
        User {
            user: UserRecord {
                name: "guest".to_string(),
                password: "guest".to_string(),
                security_level: 10,
                ..Default::default()
            },
            inf: Default::default(),
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

    match Executable::read_file(&file_name, false) {
        Ok(exe) => {
            let mut io = DiskIO::new(".");
            let mut output = Output::default();

            output.is_sysop = arguments.sysop;
            enable_raw_mode().unwrap();
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
            disable_raw_mode().unwrap();
        }
        Err(err) => {
            execute!(
                stdout(),
                SetAttribute(Attribute::Bold),
                SetForegroundColor(Color::Red),
                Print("ERROR: ".to_string()),
                SetAttribute(Attribute::Reset),
                SetAttribute(Attribute::Bold),
                Print(format!("{}", err)),
                SetAttribute(Attribute::Reset),
            )
            .unwrap();
            println!();
            println!();
        }
    }
}
