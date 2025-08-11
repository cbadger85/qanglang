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
    },
    Ls,
    Test {
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
            ignore,
            eformat,
        }) => check_script(&path, &ignore, &eformat),
        Some(QangCommand::Ls) => run_language_server(),
        Some(QangCommand::Test {
            path,
            debug: _,
            heap: _,
            eformat: _,
        }) => {
            use std::path::Path;
            let path_obj = Path::new(&path);

            let files = if path_obj.is_dir() {
                collect_ql_files(&path, &[])
            } else {
                vec![path_obj.into()]
            };

            let results = qanglang_test::run_tests(files);
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
    let mut heap = ObjectHeap::new();

    let format = match error_format.to_lowercase().as_str() {
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

    let program = match CompilerPipeline::new(source_map, &mut heap)
        .error_message_format(format)
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

fn check_script(path: &str, ignore_patterns: &[String], error_format: &str) {
    use std::path::Path;

    let path_obj = Path::new(path);

    let format = match error_format.to_lowercase().as_str() {
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

    if path_obj.is_file() {
        check_single_file(path, format);
    } else if path_obj.is_dir() {
        let files = collect_ql_files(path, ignore_patterns);
        if files.is_empty() {
            println!("No .ql files found in directory '{}'", path);
            return;
        }

        let mut total_errors = 0;
        let mut checked_files = 0;

        for file in &files {
            let error_count = check_single_file(&file.to_string_lossy(), format);
            total_errors += error_count;
            checked_files += 1;
        }

        if total_errors == 0 {
            println!(
                "Successfully checked {} file(s). No errors found.",
                checked_files
            );
        } else {
            println!(
                "Checked {} file(s). Found {} error(s) total.",
                checked_files, total_errors
            );
        }
    } else {
        eprintln!("Error: '{}' is not a valid file or directory", path);
        std::process::exit(1);
    }
}

fn check_single_file(filename: &str, format: ErrorMessageFormat) -> usize {
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            return 0;
        }
    };

    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap)
        .error_message_format(format)
        .run()
    {
        Err(errors) => {
            let error_count = errors.all().len();
            for error in errors.all() {
                eprintln!("{}:{}", filename, error.message);
            }
            error_count
        }
        Ok(_) => 0,
    }
}

fn collect_ql_files(dir: &str, ignore_patterns: &[String]) -> Vec<std::path::PathBuf> {
    use std::fs;
    use std::path::Path;

    let mut files = Vec::new();

    fn visit_dir(dir: &Path, files: &mut Vec<std::path::PathBuf>, ignore_patterns: &[String]) {
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                let path_str = path.to_string_lossy();

                // Check if path should be ignored
                let should_ignore = ignore_patterns
                    .iter()
                    .any(|pattern| path_str.contains(pattern));

                if should_ignore {
                    continue;
                }

                if path.is_dir() {
                    visit_dir(&path, files, ignore_patterns);
                } else if path.extension().and_then(|s| s.to_str()) == Some("ql") {
                    files.push(path);
                }
            }
        }
    }

    visit_dir(Path::new(dir), &mut files, ignore_patterns);
    files.sort();
    files
}
