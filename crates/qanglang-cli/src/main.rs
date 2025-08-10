mod repl;

use qanglang_core::{CompilerPipeline, ObjectHeap, SourceMap, Vm, disassemble_program};
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
    },
    Check {
        path: String,
    },
    Ls,
}

fn main() {
    let cli = QangCli::parse();

    match cli.command {
        Some(QangCommand::Run { path, debug, heap }) => run_script(&path, debug, heap),
        Some(QangCommand::Check { path }) => check_script(&path),
        Some(QangCommand::Ls) => todo!("turn on language server here."),
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

    let program = match CompilerPipeline::new(source_map, &mut heap).run() {
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

    if let Err(errors) = CompilerPipeline::new(source_map, &mut heap).run() {
        for error in errors.all() {
            eprintln!("Compile error: {}", error.message);
        }
        return;
    }
}
