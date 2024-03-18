use std::{env, fs::read_to_string, path::PathBuf};

use crate::{
    compiler,
    decompiler::decompile,
    executable::Executable,
    icy_board::data::IcyBoardData,
    vm::{run, ExecutionContext, HangupType, MemoryIO, TerminalTarget},
    parser::parse_program,
    Res,
};

#[test]
fn test_compiler() {
    use std::fs::{self};

    let mut data_path = env::current_dir().unwrap();
    data_path.push("src/compiler/test_data");
    //let mut success = 0;
    //let mut skipped = 0;
    for entry in fs::read_dir(data_path).expect("Error reading test_data directory.") {
        let cur_entry = entry.unwrap().path();

        if cur_entry.extension().unwrap() != "pps" {
            continue;
        }

        let file_name = cur_entry.as_os_str();
        println!(
            "File: {}...",
            cur_entry.file_name().unwrap().to_str().unwrap()
        );

        let mut data = String::new();
        let mut next = false;
        for line in read_to_string(file_name).unwrap().lines() {
            if next {
                run_test(&data, line);
            }
            if line.starts_with(";;") {
                next = true;
            } else {
                data.push_str(line);
                data.push('\n');
            }
        }
    }
}

fn run_test(data: &String, output: &str) {
    let prg = parse_program(PathBuf::from("."), data);

    let mut exec = compiler::PPECompiler::new();
    exec.compile(&prg, false);
    let binary = exec.create_executable(330).unwrap();
    let mut buffer = binary.to_buffer().unwrap();
    let exe = Executable::from_buffer(&mut buffer, false).unwrap();
    let mut io = MemoryIO::new();
    let mut ctx = TestContext::new();
    run(PathBuf::from("."), &exe, &mut ctx, &mut io, IcyBoardData::default()).unwrap();

    let error = output != ctx.output;
    if error {
        let exe = Executable::from_buffer(&mut buffer, false).unwrap();
        exe.print_variable_table();
        exe.print_script_buffer_dump();
        exe.print_disassembler();
    }
    assert!(!error);
    println!("{output}");
    println!("------------)");
    println!("{data}");
}

struct TestContext {
    output: String,
}
impl TestContext {
    pub fn new() -> Self {
        Self {
            output: String::new(),
        }
    }
}

impl ExecutionContext for TestContext {
    fn has_sysop(&self) -> bool {
        false
    }

    fn gotoxy(&mut self, _terminal_target: TerminalTarget, _x: i32, _y: i32) -> Res<()> {
        Ok(())
    }
    fn get_char(&mut self) -> Res<Option<char>> {
        todo!()
    }

    fn print(&mut self, _terminal_target: TerminalTarget, str: &str) -> Res<()> {
        self.output.push_str(str);
        Ok(())
    }

    fn write_raw(&mut self, _terminal_target: TerminalTarget, _data: &[u8]) -> Res<()> {
        Ok(())
    }

    fn send_to_com(&mut self, _data: &str) -> Res<()> {
        todo!()
    }

    fn read(&mut self) -> Res<String> {
        Ok(String::new())
    }
    fn inbytes(&mut self) -> i32 {
        0
    }
    fn get_caret_position(&mut self) -> (i32, i32) {
        (0, 0)
    }

    fn set_color(&mut self, _color: u8) {}
    fn hangup(&mut self, _hangup_type: HangupType) -> Res<()> {
        Ok(())
    }
}
