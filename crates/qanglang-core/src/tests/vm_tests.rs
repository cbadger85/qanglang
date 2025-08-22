use crate::{
    CompilerPipeline, HeapAllocator, SourceMap, Value, Vm, disassemble_program,
    memory::ClosureHandle,
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
    let mut allocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => {
            disassemble_program(&allocator);
            let vm = Vm::new(allocator);
            match vm.set_gc_status(false).set_debug(false).interpret(program) {
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
fn test_string_concat() {
    let source = r#"
        var pen = "pen";
        var apple = "apple"; 
        var pineapple = "pineapple"; 
        assert_eq(pen + apple + pen + pineapple + apple + pen, "penapplepenpineappleapplepen");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => {
            let vm = Vm::new(allocator);
            match vm.set_gc_status(false).set_debug(false).interpret(program) {
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
    let mut allocator = HeapAllocator::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut allocator).run() {
        match Vm::new(allocator).set_gc_status(false).interpret(program) {
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
        // assert_eq(typeof(foo), "string");
        assert_eq((12 |> to_string) + " is a number", "12 is a number");
        // println((12 |> to_string) + " is a number");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => {
            // disassemble_program(&allocator);
            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(false)
                .interpret(program)
            {
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
    let mut allocator: HeapAllocator = HeapAllocator::new();

    fn find_function_handle(identifier: &str, vm: &mut Vm) -> ClosureHandle {
        let function_identifier_handle = vm.allocator_mut().strings.intern(identifier.into());
        let (_, value) = vm
            .globals()
            .iter()
            .find(|(handle, _value)| **handle == function_identifier_handle)
            .unwrap();

        match value {
            Value::Closure(handle) => *handle,
            _ => panic!("Identity function not found!"),
        }
    }

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => {
            let mut vm = Vm::new(allocator).set_gc_status(false).set_debug(false);
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

            let foo = vm.allocator_mut().strings.intern("foo".into());

            match vm.call_function(function_handle, vec![Value::String(foo)]) {
                Ok(Value::String(handle)) => {
                    let string = vm.allocator().strings.get_string(handle);
                    assert_eq!("foo".to_string(), string.to_string());
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
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => {
            disassemble_program(&allocator);
            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(false)
                .interpret(program)
            {
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
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => {
            disassemble_program(&allocator);
            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(true)
                .interpret(program)
            {
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
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => {
            disassemble_program(&allocator);
            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(true)
                .interpret(program)
            {
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
fn test_closures() {
    let source = r#"
        fn make_working_counter() {
            var count = 0;
            var counter = () -> {
                assert_eq(typeof(count), NUMBER, "Expected " + NUMBER + " but found " + typeof(count));
                println("typeof count " + typeof(count));
                count = count + 1;
                return count;
            };

            var value = counter();
            assert_eq(value, 1, "Expected value to be 1."); // This test passes.

            return true;
        }

        assert(make_working_counter());

        fn make_counter() {
            var count = 0;
            return () -> {
                assert_eq(typeof(count), NUMBER, "Expected " + NUMBER + " but found " + typeof(count));
                println("typeof count " + typeof(count));
                count = count + 1;
                return count;
            };
        }
                
            fn perform_count() {
                var counter = make_counter();
                assert_eq(typeof(counter), FUNCTION, "Expected " + FUNCTION + " but found " + typeof(counter));
            return counter();
        }

        assert_eq(perform_count(), 1);
"#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => {
            // disassemble_program(&allocator);
            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(false)
                .interpret(program)
            {
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
fn test_pipe_partial_application() {
    let source = r#"
        // Test basic partial application
        fn add(a, b) {
            return a + b;
        }
        
        var result1 = 5 |> add(3);
        assert_eq(result1, 8);
        
        // Test with string concatenation
        fn concat(str1, str2) {
            return str1 + str2;
        }
        
        var result2 = "hello " |> concat("world!");
        assert_eq(result2, "hello world!");
        
        // Test with multiple arguments
        fn triple_add(a, b, c) {
            return a + b + c;
        }
        
        var result3 = 1 |> triple_add(2, 3);
        assert_eq(result3, 6);
        
        // Test backward compatibility - function without parentheses
        fn double(x) {
            return x * 2;
        }
        
        var result4 = 5 |> double;
        assert_eq(result4, 10);
        
        // Test empty parentheses (equivalent to no parentheses)
        var result5 = 5 |> double();
        assert_eq(result5, 10);
        
        // Test with built-in functions
        var result6 = 42 |> to_string;
        assert_eq(typeof(result6), "string");
        assert_eq(result6, "42");
"#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => match Vm::new(allocator)
            .set_gc_status(false)
            .set_debug(false)
            .interpret(program)
        {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error);
            }
        },
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.")
        }
    }
}

#[test]
fn test_pipe_chaining() {
    let source = r#"
        fn add_one(x) {
            return x + 1;
        }
        
        fn multiply_by(x, factor) {
            return x * factor;
        }
        
        fn to_string_with_suffix(x, suffix) {
            return to_string(x) + suffix;
        }
        
        // Test chaining pipes
        var result = 5 |> add_one |> multiply_by(3) |> to_string_with_suffix(" units");
        assert_eq(result, "18 units");
        
        // Test that original functions still work normally
        assert_eq(add_one(5), 6);
        assert_eq(multiply_by(6, 3), 18);
        assert_eq(to_string_with_suffix(18, " units"), "18 units");
"#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => match Vm::new(allocator)
            .set_gc_status(false)
            .set_debug(false)
            .interpret(program)
        {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error);
            }
        },
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.")
        }
    }
}

#[test]
fn test_native_function_with_return() {
    let source = r#"
        var time = system_time();
        assert_eq(typeof(time), NUMBER);
"#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => match Vm::new(allocator)
            .set_gc_status(false)
            .set_debug(false)
            .interpret(program)
        {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error);
            }
        },
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.")
        }
    }
}

#[test]
fn test_class_declaration() {
    let source = r#"
        class Foo {}
        class Bar {}

        println(Foo);
        var foo = Foo();
        println(foo);
        foo.bar = "baz";
        println(foo.bar);
        foo.bar = Bar();
        foo.bar.baz = "baz";
        println(foo.bar.baz);
        var temp = foo.bar.baz;
        println("does this work? " + temp);
        foo.bar.baz;

        fn bar() {
            class Bar {}

            println(Bar);
            println(Bar());
        }

        bar();
"#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(program) => {
            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(false)
                .interpret(program)
            {
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
