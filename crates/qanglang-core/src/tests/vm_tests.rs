use crate::{
    CompilerPipeline, FunctionValueKind, ObjectHandle, ObjectHeap, QangObject, SourceMap, Value,
    Vm, disassemble_program,
};

#[test]
fn test_globals() {
    let source = r#"
        var two = 3.14;
        println(two);
        two = 2;
        println(two);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            disassemble_program(&heap);
            let vm = Vm::new(heap);
            match vm.set_debug(false).interpret(program) {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", error.message)
                }
            }
        }
        Err(error) => {
            for error in error.all() {
                eprintln!("{}", error);
            }

            panic!("Compiler errors.")
        }
    }
}

#[test]
fn test_runtime_error_with_source_span() {
    let source = r#"
  -"hello";
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        match Vm::new(heap).interpret(program) {
            Ok(_) => {
                panic!("Expected runtime error for negating a string")
            }
            Err(error) => {
                let error_message = error.message;
                assert!(error_message.contains("Operand must be a number"));
                println!("Error correctly includes source span: {}", error_message);
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_pipe_operator() {
    let source = r#"
        var foo = 12 |> to_string;
        assert_eq(typeof(foo), "string");
        assert_eq((12 |> to_string) + " is a number", "12 is a number");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            // disassemble_program(&heap);
            match Vm::new(heap).set_debug(false).interpret(program) {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", error);
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.")
        }
    }
}

#[test]
fn test_calling_functions_from_native() {
    let source = r#"
        fn identity(x) { return x; }
    "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    fn find_function_handle(identifier: &str, vm: &mut Vm) -> ObjectHandle {
        let function_identifier_handle = vm.heap_mut().intern_string(identifier.into());
        let (_, value) = vm
            .globals()
            .iter()
            .find(|(handle, _value)| **handle == function_identifier_handle)
            .unwrap();

        match value {
            Value::Function(FunctionValueKind::Closure(handle)) => *handle,
            _ => panic!("Identity function not found!"),
        }
    }

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            let mut vm = Vm::new(heap).set_debug(false);
            match vm.interpret(program) {
                Ok(_) => {}
                Err(error) => {
                    panic!("{}", error);
                }
            }

            let function_handle = find_function_handle("identity", &mut vm);

            match vm.call_function(function_handle, vec![Value::Number(42.0)]) {
                Ok(Value::Number(number)) => {
                    assert_eq!(number, 42.0);
                }
                Err(error) => {
                    println!("{}", error.message);
                    panic!("Operation failed")
                }
                _ => panic!("Unexpected type conversion."),
            }

            let foo = vm.heap_mut().intern_string("foo".into());

            match vm.call_function(function_handle, vec![Value::String(foo)]) {
                Ok(Value::String(handle)) => {
                    let string = match vm.heap().get(handle) {
                        Some(QangObject::String(string)) => string,
                        _ => panic!("Not a string!"),
                    };
                    assert_eq!("foo".to_string(), string.clone().into_string());
                }
                Err(error) => {
                    println!("{}", error.message);
                    panic!("Operation failed")
                }
                _ => panic!("Unexpected type conversion."),
            }

            match vm.call_function(function_handle, vec![]) {
                Ok(Value::Nil) => (),
                Err(error) => {
                    println!("{}", error.message);
                    panic!("Operation failed")
                }
                _ => panic!("Unexpected type conversion."),
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.")
        }
    }
}

#[test]
fn test_lambda_declaration() {
    let source = r#"
        var lambda = () -> "hello world";

        assert_eq(lambda(), "hello world");
        
        var lambda_two = () -> {
            return "hello world";
        };

        assert_eq(lambda_two(), "hello world");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            disassemble_program(&heap);
            match Vm::new(heap).set_debug(false).interpret(program) {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", error);
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.")
        }
    }
}

#[test]
fn test_lambda_expression() {
    let source = r#"
        fn identity(x) {
            return x;
        }

        var y = identity(() -> "hello world");
        assert_eq(y(), "hello world");
        
        var z = identity(() -> {
            return "hello world";
        });
            
        assert_eq(z(), "hello world");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            disassemble_program(&heap);
            match Vm::new(heap).set_debug(true).interpret(program) {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", error);
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.")
        }
    }
}

#[test]
fn test_immediately_invoked_functional_expressions() {
    let source = r#"
  assert_eq((() -> nil)(), nil);
  assert_eq(() -> { return nil; }(), nil);
"#;

    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            disassemble_program(&heap);
            match Vm::new(heap).set_debug(true).interpret(program) {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", error);
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.")
        }
    }
}
