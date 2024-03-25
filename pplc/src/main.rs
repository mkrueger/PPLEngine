use argh::FromArgs;
use ariadne::{Label, Report, ReportKind, Source};
use icy_ppe::{
    compiler::PPECompiler,
    parser::{load_with_encoding, parse_ast, Encoding},
};
use semver::Version;
use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

#[derive(FromArgs)]
#[argh(description = "PCBoard Programming Language Compiler")]
struct Args {
    /// output the disassembly instead of compiling
    #[argh(switch, short = 'd')]
    disassemble: bool,

    /// force no user variables
    #[argh(switch)]
    nouvar: bool,

    /// force user variables
    #[argh(switch)]
    forceuvar: bool,

    /// don't report any warnings
    #[argh(switch)]
    nowarnings: bool,

    /// version number for the compiler, valid: 100, 200, 300, 310, 330 (default), 340
    #[argh(option)]
    ppl_version: Option<u16>,

    /// input file is CP437
    #[argh(switch)]
    dos: bool,

    /// file[.pps] to compile (extension defaults to .pps if not specified)
    #[argh(positional)]
    input: String,
}

lazy_static::lazy_static! {
    static ref VERSION: Version = Version::parse(env!("CARGO_PKG_VERSION")).unwrap();
}

fn main() {
    let arguments: Args = argh::from_env();
    let version = if let Some(v) = arguments.ppl_version {
        v
    } else {
        330
    };
    let valid_versions: Vec<u16> = vec![100, 200, 300, 310, 330, 340];
    if !valid_versions.contains(&version) {
        println!("Invalid version number valid values {valid_versions:?}");
        return;
    }
    if arguments.nouvar && arguments.forceuvar {
        println!("--nouvar can't be used in conjunction with --forceuvar");
        return;
    }
    let mut file_name = arguments.input;
    let extension = Path::new(&file_name).extension().and_then(OsStr::to_str);
    if extension.is_none() {
        file_name.push_str(".pps");
    }
    let encoding = if arguments.dos {
        Encoding::CP437
    } else {
        Encoding::Utf8
    };
    let src = load_with_encoding(&PathBuf::from(&file_name), encoding).unwrap();

    println!();
    println!("Parsing...");
    let (mut prg, errors) = parse_ast(PathBuf::from(&file_name), &src, encoding, version);
    if arguments.nouvar {
        prg.require_user_variables = false;
    }
    if arguments.forceuvar {
        prg.require_user_variables = true;
    }
    println!("Compiling...");
    let mut compiler = PPECompiler::new(version, errors.clone());
    compiler.compile(&prg);

    if errors.lock().unwrap().has_errors()
        || (errors.lock().unwrap().has_warnings() && !arguments.nowarnings)
    {
        let mut error_count = 0;
        let mut warning_count = 0;

        for err in &errors.lock().unwrap().errors {
            error_count += 1;
            Report::build(ReportKind::Error, &file_name, err.span.start)
                .with_code(error_count)
                .with_message(format!("{}", err.error))
                .with_label(
                    Label::new((&file_name, err.span.clone())).with_color(ariadne::Color::Red),
                )
                .finish()
                .print((&file_name, Source::from(&src)))
                .unwrap();
        }

        if !arguments.nowarnings {
            for err in &errors.lock().unwrap().warnings {
                warning_count += 1;
                Report::build(ReportKind::Warning, &file_name, err.span.start)
                    .with_code(warning_count)
                    .with_message(format!("{}", err.error))
                    .with_label(
                        Label::new((&file_name, err.span.clone()))
                            .with_color(ariadne::Color::Yellow),
                    )
                    .finish()
                    .print((&file_name, Source::from(&src)))
                    .unwrap();
            }
            println!("{} errors, {} warnings", error_count, warning_count);
        } else {
            println!("{} errors", error_count);
        }
        return;
    }

    println!();
    match compiler.create_executable(version) {
        Ok(executable) => {
            if arguments.disassemble {
                println!();
                executable.print_variable_table();
                println!();
                let mut visitor =
                    icy_ppe::executable::disassembler::DisassembleVisitor::new(&executable);
                visitor.generate_statement_data = true;
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
