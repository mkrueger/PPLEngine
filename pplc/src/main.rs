use ariadne::{Label, Report, ReportKind, Source};
use clap::Parser;
use icy_ppe::{compiler::transform_ast, parser::parse_program};
use semver::Version;
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};
mod output;
pub mod parser;

#[cfg(test)]
pub mod tests;

#[derive(Parser, Debug)]
#[command(version, about="https://github.com/mkrueger/PPLEngine", long_about = None)]
struct Args {
    /// Disable automatic user variable generation
    #[arg(long = "nouvar")]
    no_user_variables: bool,

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

    let src = fs::read_to_string(&file_name).expect("Failed to read file");
    println!();
    println!("Parsing...");
    let mut prg = parse_program(PathBuf::from(&file_name), &src);
    println!("Compiling...");
    //prg.visit_mut(&mut icy_ppe::interpreter::rename_vars_visitor::RenameVarsVisitor::default());
    transform_ast(&mut prg);
    let mut exec = output::PPEOutput::new();
    exec.compile(&prg, arguments.no_user_variables);

    if !prg.errors.is_empty()
        || !prg.warnings.is_empty()
        || !exec.warnings.is_empty()
        || !exec.errors.is_empty()
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

        for err in &exec.errors {
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

        for err in &exec.warnings {
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
    match exec.create_binary(330) {
        Ok(executable) => {
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
