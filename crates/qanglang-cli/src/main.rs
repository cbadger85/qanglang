mod repl;

use qanglang_core::{
    CompilerConfig, CompilerPipeline, ErrorMessageFormat, HeapAllocator, SourceMap, Vm,
    disassemble_program,
};
use qanglang_ls::run_language_server;
use repl::run_repl;
use std::fs;

use clap::{ArgAction, Parser, Subcommand};

#[derive(Parser)]
#[command(name = "Qang")]
#[command(version = "0.0.1")]
#[command(version, about = "CLI tooling for QangLang", long_about = None)]
struct QangCli {
    #[command(subcommand)]
    command: Option<QangCommand>,
    #[arg(short, long, action = ArgAction::SetTrue, help = "Enable debug mode for REPL")]
    debug: bool,
}

#[derive(Subcommand)]
enum QangCommand {
    Run {
        path: String,
        #[arg(short, long, action = ArgAction::SetTrue, help = "Enable debug output during script execution")]
        debug: bool,

        #[arg(short = 'm', long, action = ArgAction::SetTrue, help = "Dump compiled bytecode and heap contents before execution")]
        heap: bool,

        #[arg(
            short = 'e',
            long,
            default_value = "verbose",
            help = "Error message format [possible values: minimal, compact, verbose]"
        )]
        eformat: String,
    },
    Check {
        path: String,

        #[arg(
            short,
            long,
            help = "Patterns to ignore when recursively checking directories"
        )]
        ignore: Vec<String>,

        #[arg(
            short = 'e',
            long,
            default_value = "verbose",
            help = "Error message format [possible values: minimal, compact, verbose]"
        )]
        eformat: String,

        #[arg(short, long, action = ArgAction::SetFalse, help = "Silences console output.")]
        silent: bool,
    },
    Ls,
    Test {
        path: String,
        #[arg(short, long, action = ArgAction::SetTrue, help = "Enable debug output during script execution")]
        debug: bool,

        #[arg(short = 'm', long, action = ArgAction::SetTrue, help = "Dump compiled bytecode and heap contents before execution")]
        heap: bool,

        #[arg(
            short,
            long,
            help = "Patterns to ignore when recursively checking directories"
        )]
        ignore: Vec<String>,

        #[arg(
            short = 'e',
            long,
            default_value = "verbose",
            help = "Error message format [possible values: minimal, compact, verbose]"
        )]
        eformat: String,
    },
}

fn main() {
    let cli = QangCli::parse();

    match cli.command {
        Some(QangCommand::Run {
            path,
            debug,
            heap,
            eformat,
        }) => run_script(&path, debug, heap, &eformat),
        Some(QangCommand::Check {
            path,
            ignore: _ignore,
            silent: _silent,
            eformat,
        }) => {
            let resolver = match qanglang_test::SourceFileResolver::new() {
                Ok(resolver) => resolver,
                Err(err) => {
                    eprintln!("Error: Unable to determine working directory: {}", err);
                    std::process::exit(1);
                }
            };

            let source_files = match resolver.resolve(&path) {
                Ok(files) => files,
                Err(err) => {
                    eprintln!("Error: {}", err);
                    std::process::exit(1);
                }
            };

            let error_format = match eformat.to_lowercase().as_str() {
                "minimal" => ErrorMessageFormat::Minimal,
                "compact" => ErrorMessageFormat::Compact,
                "verbose" => ErrorMessageFormat::Verbose,
                _ => {
                    eprintln!("Invalid error format '{}'. Using verbose format.", eformat);
                    ErrorMessageFormat::Verbose
                }
            };

            let results = qanglang_test::check_files_from_sources(source_files, error_format);
            let output = qanglang_test::format_check_results(&results);
            print!("{}", output);
        }
        Some(QangCommand::Ls) => run_language_server(),
        Some(QangCommand::Test {
            path,
            ignore: _ignore,
            debug: _,
            heap: _,
            eformat: _,
        }) => {
            let resolver = match qanglang_test::TestFileResolver::new() {
                Ok(resolver) => resolver,
                Err(err) => {
                    eprintln!("Error: Unable to determine working directory: {}", err);
                    std::process::exit(1);
                }
            };

            let test_files = match resolver.resolve(&path) {
                Ok(files) => files,
                Err(err) => {
                    eprintln!("Error: {}", err);
                    std::process::exit(1);
                }
            };

            let results = qanglang_test::run_tests_from_files(test_files, None);
            let output = qanglang_test::format_results(&results);
            print!("{}", output);
        }
        _ => run_repl(cli.debug),
    }
}

fn run_script(filename: &str, debug_mode: bool, heap_dump: bool, error_format: &str) {
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            std::process::exit(1);
        }
    };

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    let error_message_format = match error_format.to_lowercase().as_str() {
        "minimal" => ErrorMessageFormat::Minimal,
        "compact" => ErrorMessageFormat::Compact,
        "verbose" => ErrorMessageFormat::Verbose,
        _ => {
            eprintln!(
                "Invalid error format '{}'. Using verbose format.",
                error_format
            );
            ErrorMessageFormat::Verbose
        }
    };

    let program = match CompilerPipeline::new()
        .with_config(CompilerConfig {
            error_message_format,
        })
        .compile(&source_map, &mut allocator)
    {
        Ok(program) => {
            if heap_dump {
                disassemble_program(&allocator);
            }
            program
        }
        Err(errors) => {
            for error in errors.all() {
                eprintln!("Compile error: {}", error.message);
            }
            return;
        }
    };

    match Vm::new(allocator).set_debug(debug_mode).interpret(program) {
        Ok(_) => (),
        Err(error) => {
            eprintln!("Runtime error: {}", error.message);
        }
    }
}
