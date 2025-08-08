use qanglang::{CompilerPipeline, ObjectHeap, SourceMap, Vm};
use std::env;
use std::fs;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_repl(),
        3 if args[1] == "run" => run_script(&args[2]),
        _ => {
            eprintln!("Usage:");
            eprintln!("  qanglang                 # Start REPL");
            eprintln!("  qanglang run <script>    # Run script file");
            std::process::exit(1);
        }
    }
}

fn run_script(filename: &str) {
    let source = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            std::process::exit(1);
        }
    };

    execute_code(&source, filename);
}

fn run_repl() {
    println!("QangLang REPL - Type 'exit' to quit");

    let heap = ObjectHeap::new();
    let mut vm = Vm::new(heap);

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = input.trim();
                if input == "exit" {
                    break;
                }
                if !input.is_empty() {
                    execute_repl_line(input, &mut vm);
                }
            }
            Err(err) => {
                eprintln!("Error reading input: {}", err);
                break;
            }
        }
    }
}

fn execute_repl_line(source: &str, vm: &mut Vm) {
    let source_map = SourceMap::new(source.to_string());

    let program = match CompilerPipeline::new(source_map, vm.heap_mut()).run() {
        Ok(program) => program,
        Err(errors) => {
            for error in errors.all() {
                eprintln!("Compile error: {}", error.message);
            }
            return;
        }
    };

    match vm.interpret(program) {
        Ok(_) => (),
        Err(error) => {
            eprintln!("Runtime error: {}", error.message);
        }
    }
}

fn execute_code(source: &str, _filename: &str) {
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    let program = match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => program,
        Err(errors) => {
            for error in errors.all() {
                eprintln!("Compile error: {}", error.message);
            }
            return;
        }
    };

    match Vm::new(heap).interpret(program) {
        Ok(_) => (),
        Err(error) => {
            eprintln!("Runtime error: {}", error.message);
        }
    }
}
