use ariadne::{Label, Report, ReportKind, Source};
use clap::Parser;
use icy_ppe::{
    compiler::PPECompiler, executable::LAST_PPLC, parser::parse_program, tables::CP437_TO_UNICODE,
};
use semver::Version;
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

#[derive(Parser, Debug)]
#[command(version, about="https://github.com/mkrueger/PPLEngine", long_about = None)]
struct Args {
    /// Don't egnerate a binary just output the disassembly
    #[arg(short, long)]
    disassemble: bool,

    /// Input file is CP437
    #[arg(short, long)]
    dos: bool,

    /// file[.pps] to compile (extension defaults to .pps if not specified)
    input: String,
}

lazy_static::lazy_static! {
    static ref VERSION: Version = Version::parse(env!("CARGO_PKG_VERSION")).unwrap();
}

fn main() {
    println!(
        "PCBoard Programming Language Compiler^RUST {}",
        *crate::VERSION
    );
    let arguments = Args::parse();

    let mut file_name = arguments.input;
    let extension = Path::new(&file_name).extension().and_then(OsStr::to_str);
    if extension.is_none() {
        file_name.push_str(".pps");
    }

    let src_data = fs::read(&file_name).expect("Failed to read file");
    let src = if arguments.dos {
        let mut res = String::new();
        for b in src_data {
            res.push(CP437_TO_UNICODE[b as usize]);
        }
        res
    } else {
        String::from_utf8_lossy(&src_data).to_string()
    };

    println!();
    println!("Parsing...");
    let prg = parse_program(PathBuf::from(&file_name), &src);
    println!("Compiling...");
    //prg.visit_mut(&mut icy_ppe::interpreter::rename_vars_visitor::RenameVarsVisitor::default());
    let mut compiler = PPECompiler::new(LAST_PPLC);
    compiler.compile(&prg);

    if !prg.errors.is_empty()
        || !prg.warnings.is_empty()
        || !compiler.warnings.is_empty()
        || !compiler.errors.is_empty()
    {
        let mut errors = 0;
        let mut warnings = 0;

        for e in &prg.errors {
            match e {
                icy_ppe::parser::Error::ParserError(err) => {
                    errors += 1;
                    Report::build(ReportKind::Error, &file_name, 12)
                        .with_code(errors)
                        .with_message(format!("{}", err.error))
                        .with_label(
                            Label::new((&file_name, err.range.clone()))
                                .with_color(ariadne::Color::Red),
                        )
                        .finish()
                        .print((&file_name, Source::from(&src)))
                        .unwrap();
                }
                icy_ppe::parser::Error::TokenizerError(err) => {
                    errors += 1;
                    Report::build(ReportKind::Error, &file_name, 12)
                        .with_code(errors)
                        .with_message(format!("{}", err.error))
                        .with_label(
                            Label::new((&file_name, err.range.clone()))
                                .with_color(ariadne::Color::Red),
                        )
                        .finish()
                        .print((&file_name, Source::from(&src)))
                        .unwrap();
                }
            }
        }

        for err in &prg.warnings {
            warnings += 1;
            Report::build(ReportKind::Warning, &file_name, 12)
                .with_code(warnings)
                .with_message(format!("{}", err.error))
                .with_label(
                    Label::new((&file_name, err.range.clone())).with_color(ariadne::Color::Yellow),
                )
                .finish()
                .print((&file_name, Source::from(&src)))
                .unwrap();
        }

        for err in &compiler.errors {
            errors += 1;
            Report::build(ReportKind::Error, &file_name, 12)
                .with_code(errors)
                .with_message(format!("{}", err.error))
                .with_label(
                    Label::new((&file_name, err.range.clone())).with_color(ariadne::Color::Red),
                )
                .finish()
                .print((&file_name, Source::from(&src)))
                .unwrap();
        }

        for err in &compiler.warnings {
            warnings += 1;
            Report::build(ReportKind::Warning, &file_name, 12)
                .with_code(warnings)
                .with_message(format!("{}", err.error))
                .with_label(
                    Label::new((&file_name, err.range.clone())).with_color(ariadne::Color::Yellow),
                )
                .finish()
                .print((&file_name, Source::from(&src)))
                .unwrap();
        }
        println!("{} errors, {} warnings", errors, warnings);
        return;
    }

    println!();

    match compiler.create_executable(LAST_PPLC) {
        Ok(executable) => {
            if arguments.disassemble {
                println!();
                executable.print_variable_table();
                println!();
                let mut visitor =
                    icy_ppe::executable::disassembler::DisassembleVisitor::new(&executable);
                visitor.show_statement_data = true;

                compiler.get_script().visit(&mut visitor);
                println!();
                println!("Generated:");
                executable.print_script_buffer_dump();
                println!();

                return;
            }

            let bin = executable.to_buffer().unwrap();
            let out_file_name = Path::new(&file_name).with_extension("ppe");
            let len = bin.len();
            fs::write(&out_file_name, bin).expect("Unable to write file");
            let lines = src.lines().count();
            println!(
                "{} lines, {} chars compiled. {} bytes written to {:?}",
                lines,
                src.len(),
                len,
                &out_file_name
            );
        }
        Err(err) => {
            println!("Error while creating binary {}", err);
        }
    }
}
