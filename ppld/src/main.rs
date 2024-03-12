use clap::Parser;
use crossterm::style::Color;
use crossterm::style::Print;
use crossterm::style::ResetColor;
use crossterm::style::SetForegroundColor;
use crossterm::ExecutableCommand;
use icy_ppe::ast::output_visitor;
use icy_ppe::ast::CommentStatement;
use icy_ppe::ast::OutputFunc;
use icy_ppe::ast::Program;
use icy_ppe::decompiler::reconstruct;
use icy_ppe::decompiler::Decompiler;
use icy_ppe::executable::read_file;
use semver::Version;
use std::ffi::OsStr;
use std::fs::*;
use std::io::*;
use std::path::Path;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// raw ppe without reconstruction control structures
    #[arg(short, long)]
    raw: bool,

    /// output to console instead of writing to file
    #[arg(short, long)]
    output: bool,

    #[arg(short, long)]
    /// keyword casing style, valid values are u=upper (default), l=lower, c=camel
    style: Option<char>,

    /// file[.ppe] to decompile
    input: String,
}

lazy_static::lazy_static! {
    static ref VERSION: Version = Version::parse(env!("CARGO_PKG_VERSION")).unwrap();
}

#[must_use]
pub fn decompile(file_name: &str, to_file: bool, raw: bool) -> Program {
    let mut prg = Program::new();
    let mut d = Decompiler::new(read_file(file_name));

    prg.statements
        .push(CommentStatement::create_empty_statement(
            "-----------------------------------------",
        ));
    prg.statements
        .push(CommentStatement::create_empty_statement(
            " PCBoard programming language decompiler ",
        ));
    prg.statements
        .push(CommentStatement::create_empty_statement(
            "-----------------------------------------",
        ));

    println!(
        "Format: {}.{:00} detected",
        d.executable.version / 100,
        d.executable.version % 100
    );

    if to_file {
        println!("Pass 1 ...");
    }
    d.do_pass1();
    d.dump_vars(&mut prg);
    if to_file {
        println!("Pass 2 ...");
    }
    d.do_pass2(&mut prg);
    if !raw {
        if to_file {
            println!("Pass 3 ...");
        }
        reconstruct::do_pass3(&mut prg);
        reconstruct::do_pass4(&mut prg);
    }

    if !d.warnings.is_empty() || d.errors.is_empty() {
        d.output_stmt(
            &mut prg,
            CommentStatement::create_empty_statement(
                "---------------------------------------".to_string(),
            ),
        );
        d.output_stmt(
            &mut prg,
            CommentStatement::create_empty_statement(format!(
                "!!! {} WARNING(S), {} ERROR(S) CAUSED BY PPLC BUGS DETECTED",
                d.warnings.len(),
                d.errors.len()
            )),
        );
        d.output_stmt(
            &mut prg,
            CommentStatement::create_empty_statement(
                "PROBLEM: These expressions most probably looked like !0+!0+!0 or similar"
                    .to_string(),
            ),
        );
        d.output_stmt(
            &mut prg,
            CommentStatement::create_empty_statement(
                "         before PPLC fucked it up to something like !(!(+0)+0)!0. This may"
                    .to_string(),
            ),
        );
        d.output_stmt(
            &mut prg,
            CommentStatement::create_empty_statement(
                "         also apply to - / and * aswell. Also occurs upon use of multiple !'s."
                    .to_string(),
            ),
        );
        d.output_stmt(
            &mut prg,
            CommentStatement::create_empty_statement(
                "         Some smartbrains use this BUG to make programms running different"
                    .to_string(),
            ),
        );
        d.output_stmt(
            &mut prg,
            CommentStatement::create_empty_statement(
                "         (or not at all) when being de- and recompiled.".to_string(),
            ),
        );
        if to_file {
            if d.warnings.is_empty() && d.errors.is_empty() {
                println!("NO DECOMPILER ERROR DETECTED");
            } else {
                for err in &d.errors {
                    stdout()
                        .execute(SetForegroundColor(Color::Red))
                        .unwrap()
                        .execute(Print("ERROR: "))
                        .unwrap()
                        .execute(ResetColor)
                        .unwrap()
                        .execute(Print(err))
                        .unwrap()
                        .flush()
                        .unwrap();
                    println!();
                }

                for warn in &d.warnings {
                    stdout()
                        .execute(SetForegroundColor(Color::Yellow))
                        .unwrap()
                        .execute(Print("WARNING: "))
                        .unwrap()
                        .execute(ResetColor)
                        .unwrap()
                        .execute(Print(warn))
                        .unwrap()
                        .flush()
                        .unwrap();
                    println!();
                }

                if !d.warnings.is_empty() {
                    stdout()
                        .execute(SetForegroundColor(Color::Yellow))
                        .unwrap()
                        .execute(Print(format!("{} WARNING(S)", d.warnings.len())))
                        .unwrap()
                        .execute(ResetColor)
                        .unwrap();
                }

                if !d.errors.is_empty() {
                    if !d.warnings.is_empty() {
                        stdout().execute(Print(", ")).unwrap();
                    }

                    stdout()
                        .execute(SetForegroundColor(Color::Red))
                        .unwrap()
                        .execute(Print(format!("{} ERROR(S)", d.errors.len())))
                        .unwrap()
                        .execute(ResetColor)
                        .unwrap();
                }

                stdout()
                    .execute(Print(" DURING DECOMPILATION DETECTED"))
                    .unwrap()
                    .flush()
                    .unwrap();
                println!();
            }
        }
    }
    prg
}

fn main() {
    pretty_env_logger::init();
    println!(
        "PCBoard Programming Language Decompiler^RUST {}",
        *crate::VERSION
    );
    let mut arguments = Args::parse();
    let mut output_func = OutputFunc::Upper;
    match arguments.style {
        Some('u') => output_func = OutputFunc::Upper,
        Some('l') => output_func = OutputFunc::Lower,
        Some('c') => output_func = OutputFunc::CamelCase,
        Some(x) => panic!("unsupported keyword style {}", x),
        None => {}
    }

    let file_name = &mut arguments.input;

    let extension = Path::new(&file_name).extension().and_then(OsStr::to_str);
    if extension.is_none() {
        file_name.push_str(".ppe");
    }

    let out_file_name = Path::new(&file_name).with_extension("ppd");
    let decompilation = decompile(file_name, true, arguments.raw);

    let mut output_visitor = output_visitor::OutputVisitor::default();
    output_visitor.output_func = output_func;
    decompilation.visit(&mut output_visitor);

    if arguments.output {
        println!("{}", output_visitor.output);
    } else {
        let mut output = File::create(&out_file_name).unwrap();
        if let Err(err) = write!(output, "{}", output_visitor.output) {
            stdout()
                .execute(SetForegroundColor(Color::Red))
                .unwrap()
                .execute(Print(format!(
                    "Can't create {:?} on disk, reason: {}",
                    &out_file_name, err
                )))
                .unwrap()
                .execute(ResetColor)
                .unwrap()
                .flush()
                .unwrap();
            return;
        }
        println!();
        println!("Source decompilation complete...");
        println!("'{}' decompiled to '{:?}'.", &file_name, &out_file_name);
    }
}

/*
let mut res = String::new();
if !self.function_implementations.is_empty() || !self.procedure_implementations.is_empty() {
    res.push_str("; Function declarations\n");
}
for v in &self.function_implementations {
    res.push_str(&v.to_string());
    res.push('\n');
}
for v in &self.procedure_implementations {
    res.push_str(&v.to_string());
    res.push('\n');
}
for v in &self.declarations {
    res.push_str(&v.to_string());
    res.push('\n');
}

res.push_str(&self.block.to_string(self));

if !self.function_implementations.is_empty() || !self.procedure_implementations.is_empty() {
    res.push_str("; Function implementations\n");
}
for v in &self.function_implementations {
    res.push_str(v.print_content().as_str());
    res.push('\n');
}

for v in &self.procedure_implementations {
    res.push_str(v.print_content().as_str());
    res.push('\n');
}*/
