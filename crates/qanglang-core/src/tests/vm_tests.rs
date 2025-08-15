use crate::{
    CompilerPipeline, FunctionValueKind, HeapObject, ObjectHandle, ObjectHeap, SourceMap, Value,
    Vm, disassemble_program,
};

#[test]
fn test_display() {
    let source = r#"
  1 * 2 / 3 + 4 - 5 + -1;
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(_) = CompilerPipeline::new(source_map, &mut heap).run() {
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_run() {
    let source = r#"
        var hello_world = "hello" + " " + "world!";
        println(hello_world);
        assert_eq(hello_world, "hello world!", "Expect \"hello world\" to equal \"hello world!\"");
        assert_eq("hello world", "hello world", "Expect \"hello world\" to equal \"hello world\"");
        var two = nil;
        assert_eq(two, nil);
        two = 2;
        assert_eq(two, 2);

        {
            var two = "2";
            assert_eq(two, "2");
            println(two);
        }   
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        },
        Err(error) => {
            for error in error.all() {
                eprintln!("{}", error);
            }
            panic!("Compiler errors.")
        }
    }
}

#[test]
fn test_while() {
    let source = r#"
        var a = 10;

        while (a > 0) {
            println(a);
            a = a - 1;
        } 
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).set_debug(false).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
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
fn test_subtraction() {
    let source = r#"
        var a = 3;
        a = 1 - a;
        println(a - -3);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            disassemble_program(&heap);
            match Vm::new(heap).set_debug(false).interpret(program) {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", error.message)
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
        }
    }
}

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
fn test_locals_simple() {
    let source = r#"
    {
        var two = 3.14;
        println(two);
    }
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
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
fn test_locals_complex() {
    let source = r#"
        var two = "two";
        println(two);
        two = 2;
        {
            var two = "2";
            println(two);
            println("TEST");
        }
        println(two);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
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
fn test_conditionals() {
    let source = r#"
        if (true) {
            println("It's true!");
        } else {
            assert(false, "Expected true to be true");
            }
            
        if (false) {
            assert(false, "Expected false to be false");
        } else {
            println("It's false!"); 
        }
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        match Vm::new(heap).set_debug(true).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_ternary_expressions() {
    let source = r#"
        var result1 = true ? "correct" : "wrong";
        println(result1);
        
        var result2 = false ? "wrong" : "correct";
        println(result2);
        
        var result3 = nil ? "wrong" : "correct";
        println(result3);

        var result4 = true ? "should do this" : false ? "don't do this" : "nope";
        println(result4);

        var result5 = false ? "should not do this" : true ? "do this" : "nope";
        println(result5);

        var result6 = false ? "should not do this" : false ? "don't do this" : "yup";
        println(result6);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        match Vm::new(heap).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_logical_expressions() {
    let source = r#"
        var result1 = true or false;
        println(result1);
        
        var result2 = false or true;
        println(result2);
        
        var result3 = false or false;
        println(result3);
        
        var result4 = true and false;
        println(result4);
        
        var result5 = true and true;
        println(result5);
        
        var result6 = false and true;
        println(result6);
        
        var result7 = "hello" or nil;
        println(result7);
        
        var result8 = nil and "world";
        println(result8);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let mut vm = Vm::new(heap);
        match vm.interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn math_operations_test() {
    let source = r#"
  1 / 1 + 2 * (12 % 5);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        match Vm::new(heap).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn equality_operations_test() {
    let source = r#"
        "true" != "true";
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        match Vm::new(heap).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn comparison_operations_test() {
    let source = r#"
        println(10 >= 9);     // true
        println(10 >= 10);    // true
        println(10 >= 11);    // false
        println(10 > 9);      // true
        println(9 > 9);       // false
        println(10 > 11);     // false
        println(9 <= 10);     // true
        println(10 <= 10);    // true
        println(11 <= 10);    // false
        println(9 < 10);      // true
        println(9 < 9);       // false
        println(11 < 10);     // false
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        match Vm::new(heap).set_debug(false).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
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
fn test_booleans() {
    let source = r#"
  !true;
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        match Vm::new(heap).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_targeted_type_error_spans() {
    let source = r#"
  42 + "hello";
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        match Vm::new(heap).interpret(program) {
            Ok(_) => {
                panic!("Expected runtime error for adding number and string")
            }
            Err(error) => {
                let error_message = error.message;
                assert!(error_message.contains("Cannot add number to string"));
                println!(
                    "Targeted error correctly points to string operand: {}",
                    error_message
                );
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_left_operand_error_span() {
    let source = r#"
  true + 5;
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        match Vm::new(heap).interpret(program) {
            Ok(_) => {
                panic!("Expected runtime error for adding boolean and number")
            }
            Err(error) => {
                let error_message = error.message;
                assert!(error_message.contains("Cannot add boolean to number"));
                println!(
                    "Targeted error correctly points to boolean operand: {}",
                    error_message
                );
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_for_loop() {
    let source = r#"
        var sum = 0;
        for (var i = 1; i <= 3; i = i + 1) {
            println(i);
            sum = sum + i;
        }
        println(sum);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).set_debug(false).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
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
fn test_native_functions() {
    let source = r#"
        assert(true, "This should not error.");
        assert(false, "This should error.");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).set_debug(true).interpret(program) {
            Ok(_) => {
                panic!("Expected error but received none.")
            }
            Err(error) => {
                assert!(error.message.contains("This should error."));
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
fn test_escape_sequences_in_print() {
    let source = r#"
        print("And then she said, \"Go to hell!\"");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).set_debug(false).interpret(program) {
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
fn test_comments_in_strings() {
    let source = r#"
        print("\\\\This should\n not care.");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).set_debug(false).interpret(program) {
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
fn test_function_calls() {
    let source = r#"
        fn this_is_a_test() {
            return "hello?";
        }

        assert_eq(this_is_a_test(), "hello?");
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
fn test_function_calls_called_with_extra_args() {
    let source = r#"
        fn this_is_a_test() {
            return "hello?";
        }

        assert_eq(this_is_a_test("This too."), "hello?");
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
fn test_nested_function_calls() {
    let source = r#"
        fn this_is_a_test() {
            fn inner() {
                return "hello?";
            }

            return inner();
        }

        assert_eq(this_is_a_test(), "hello?");
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
fn test_function_calls_with_args() {
    let source = r#"
        fn this_is_a_test(arg1, arg2) {
            return arg1 + arg2;
        }

        assert_eq(this_is_a_test("hello ", "world"), "hello world");
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
fn test_nested_function_calls_with_args() {
    let source = r#"
        fn this_is_a_test(arg1, arg2) {
            fn inner(arg1, arg2) {
                return arg1 + arg2;
            }

            return inner(arg1, arg2);
        }

        assert_eq(this_is_a_test("hello ", "world"), "hello world");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).set_debug(true).interpret(program) {
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
fn test_system_time() {
    let source = r#"
        // Not saved as a local
        print("System time is ");
        print(system_time());
        println("");
        assert(system_time() != nil);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).set_debug(false).interpret(program) {
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
fn test_typeof_function() {
    let source = r#"
        assert_eq(typeof("this is a string"), STRING, "Expected 'this is a string' to equal a string.");
        assert_eq(typeof(0), NUMBER, "Expected 0 to be a number.");
        assert_eq(typeof(true), BOOLEAN, "Expected true to be a boolean.");
        assert_eq(typeof(nil), NIL, "Expected nil to be nil");

        fn identity(x) { return x; }

        assert_eq(typeof(identity), FUNCTION, "Expected identity() to be a function.");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).set_debug(false).interpret(program) {
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
                        Some(HeapObject::String(string)) => string,
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
fn test_qanglang_test_runner_regression() {
    // This test reproduces the issue where the QangLang test runner fails
    // with "Value not callable" during the initial interpret() call

    // Try reading the actual test file
    let source = match std::fs::read_to_string(
        "C:\\Users\\hassl\\projects\\qanglang\\tests\\test_assertions.ql",
    ) {
        Ok(content) => content,
        Err(_) => {
            // Fallback to inline content
            r#"var test_description = "Testing the native assertion functions.";

fn test_assert_true() {
  assert_eq(true, true, "Expected 'true' to equal 'true'.");
  assert(true, "Expected 'true' to be truthy");
  assert_eq(true, !false, "Expected 'true' not to be falsy.");
}

fn test_assert_false() {
  assert_eq(false, false, "Expected 'false' to equal 'false'.");
  assert(!false, "Expected 'false' to be falsy");
  assert_eq(false, !true, "Expected 'false' not to be truthy.");
}

fn test_assert_nil() {
  assert(nil == nil, "Expected 'nil' to equal 'nil'.");
  assert(!nil, "Expected 'nil' to be falsy");
  assert_eq(!nil, true, "Expected 'nil' not to be truthy.");
}"#
            .to_string()
        }
    };
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    // Simulate what the test runner does
    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            let mut vm = Vm::new(heap);

            // This should succeed (like interpret() in test runner)
            match vm.interpret(program) {
                Ok(_) => {
                    // Test runner logic: extract test functions and call them
                    let test_description_handle =
                        vm.heap_mut().intern_string("test_description".into());
                    let test_func_handle = vm.heap_mut().intern_string("test_assert_true".into());

                    // Debug: Print what globals we actually have
                    println!("Globals in VM:");
                    for (handle, value) in vm.globals().iter() {
                        if let Some(obj) = vm.heap().get(*handle) {
                            if let HeapObject::String(name) = obj {
                                println!("  {} -> {:?}", name, value);
                            }
                        }
                    }

                    // Check that globals were set correctly
                    assert!(
                        vm.globals().contains_key(&test_description_handle),
                        "test_description not found"
                    );
                    assert!(
                        vm.globals().contains_key(&test_func_handle),
                        "test_assert_true not found"
                    );

                    // Extract the test function handle
                    if let Some(Value::Function(FunctionValueKind::Closure(func_handle))) =
                        vm.globals().get(&test_func_handle)
                    {
                        // This should succeed (like call_function in test runner)
                        match vm.call_function(*func_handle, vec![]) {
                            Ok(_) => (), // Test passed
                            Err(error) => panic!("Test function call failed: {}", error.message),
                        }
                    } else {
                        panic!("Test function not found in globals");
                    }
                }
                Err(error) => {
                    panic!(
                        "Runtime error during interpretation (this is the bug): {}",
                        error.message
                    );
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
fn test_recursive_function_calls() {
    let source = r#"
        fn fib(n) {
            if (n < 2) {
               return n;
            }
            return fib(n - 2) + fib(n - 1);
        }
        assert_eq(fib(3), 2);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => match Vm::new(heap).set_debug(false).interpret(program) {
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
fn test_closures() {
    let source = r#"
        fn function_a() {
            var a = 1;

            fn function_b() {
                return a;
            }

            return function_b;
        }

        println(function_a());
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
