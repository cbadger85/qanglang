mod repl;

use qanglang_core::{
    CompilerPipeline, ErrorMessageFormat, ObjectHeap, SourceMap, Vm, disassemble_program,
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
    #[arg(short, long, action = ArgAction::SetTrue)]
    debug: bool,
}

#[derive(Subcommand)]
enum QangCommand {
    Run {
        path: String,
        #[arg(short, long, action = ArgAction::SetTrue)]
        debug: bool,

        #[arg(short = 'm', long, action = ArgAction::SetTrue)]
        heap: bool,
        // TODO - add --eformat flag to format the error output.
    },
    Check {
        path: String,
        // TODO - Add --ignore flag to filter out files to skip.

        // TODO - add --eformat flag to format the error output.
    },
    Ls,
}

fn main() {
    let cli = QangCli::parse();

    match cli.command {
        Some(QangCommand::Run { path, debug, heap }) => run_script(&path, debug, heap),
        Some(QangCommand::Check { path }) => check_script(&path),
        Some(QangCommand::Ls) => run_language_server(),
        _ => run_repl(cli.debug),
    }
}

fn run_script(filename: &str, debug_mode: bool, heap_dump: bool) {
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            std::process::exit(1);
        }
    };

    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    let program = match CompilerPipeline::new(source_map, &mut heap)
        .error_message_format(ErrorMessageFormat::Verbose)
        .run()
    {
        Ok(program) => {
            if heap_dump {
                disassemble_program(&heap);
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

    match Vm::new(heap).set_debug(debug_mode).interpret(program) {
        Ok(_) => (),
        Err(error) => {
            eprintln!("Runtime error: {}", error.message);
        }
    }
}

// TODO - Check if input is file or directory, if directory, recursively grab all '.ql' files to check.
fn check_script(filename: &str) {
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            std::process::exit(1);
        }
    };

    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap)
        .error_message_format(ErrorMessageFormat::Verbose)
        .run()
    {
        Err(errors) => {
            for error in errors.all() {
                eprintln!("Compile error: {}", error.message);
            }
        }
        Ok(_) => {
            // TODO - Improve this message
            println!("Complete.");
        }
    }
}
