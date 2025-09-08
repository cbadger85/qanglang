use qanglang_core::{CompilerPipeline, HeapAllocator, SourceMap, Vm};
use std::io::{self, Write};

pub fn run_repl(debug: bool) {
    println!("QangLang REPL - Type 'exit' to quit");

    let allocator = HeapAllocator::new();
    let mut vm = Vm::new(allocator).set_debug(debug);

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

pub fn execute_repl_line(source: &str, vm: &mut Vm) {
    let source_map = SourceMap::new(source.to_string());

    let program = match CompilerPipeline::new().compile(&source_map, &mut vm.alloc) {
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
