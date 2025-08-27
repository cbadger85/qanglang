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
        let function_identifier_handle = vm.alloc.strings.intern(identifier.into());
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

            let foo = vm.alloc.strings.intern("foo".into());

            match vm.call_function(function_handle, vec![Value::String(foo)]) {
                Ok(Value::String(handle)) => {
                    let string = vm.alloc.strings.get_string(handle);
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
        println(Foo);
        var foo = Foo();
        println(foo);
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
fn test_class_declaration_with_methods() {
    let source = r#"
        class Foo {
            init() {
                this.foo = "bar";
            }
        
            get_one() {
                return 1;
            }

            sum_one_with(num) {
                return num + this.get_one();
            }
        
        }

        assert_eq(Foo().sum_one_with(2), 3);
        assert_eq(Foo().foo, "bar");
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
fn test_classing_fields_that_reference_functions() {
    let source = r#"
        class Oops {
            init() {
                this.field = () -> 3;
                this.other_field = 2;
            }
        }

        var oops = Oops();
        assert_eq(oops.field(), 3);
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
fn test_class_inheritance() {
    let source = r#"
        class A {
            a() {
                return 42;
            }
        }

        class B : A {
            b() {
                return super.a();
            }
        }
        var value = B().a();
        assert_eq(value, 42, "Expected '42', recieved " + (value |> to_string));
        var value_2 = B().b();
        assert_eq(value_2, 42, "Expected '42', recieved " + (value |> to_string));
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
fn test_class_inheritance_with_constructors() {
    let source = r#"
        class Foo {
            init() {
                this.a = 12;
            }
        }

        class Bar : Foo {
            init() {
                super.init();
            }
        }

        assert_eq(Bar().a, 12);
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
fn test_field_declarations() {
    let source = r#"
        class Foo {
            foo = 4;
            bar;
        }

        assert_eq(Foo().foo, 4);
        assert_eq(Foo().bar, nil);
        
        class Bar : Foo {}
        assert_eq(Bar().foo, 4);
        assert_eq(Bar().bar, nil);
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
fn test_field_declarations_with_inheritance() {
    let source = r#"
        class TestClass {
            test_field = 42;
        }

        class OtherClass : TestClass {
            other_field;

            init() {
            super.init();
            this.other_field = super.test_field;
            }
        }  

        assert_eq(TestClass().test_field, OtherClass().other_field, "Expected fields to be equal.");
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
fn test_intrinsic_methods() {
    let source = r#"
        var loud = "loud".to_uppercase();

        assert_eq(loud, "LOUD", "Expected loud to be 'LOUD'.");
        
        var loud_to_uppercase = "loud".to_uppercase;
        assert_eq(loud_to_uppercase(), "LOUD", "Expected loud to be 'LOUD'.");
        println(loud_to_uppercase);
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
fn test_break_and_continue() {
    let source = r#"
        // Test just continue
        var result = "";
        var i = 0;
        while (i < 4) {
            i = i + 1;
            if (i == 3) {
                continue;
            }
            result = result + to_string(i);
        }
        // Should be "124" (skipping 3)
        assert_eq(result, "124", "Continue test failed: " + result);
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
fn test_break_continue_error_cases() {
    // Test break outside of loop
    let source_break = r#"
        break;
    "#;

    let source_map = SourceMap::new(source_break.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(_) => panic!("Expected compiler error for break outside loop"),
        Err(errors) => {
            let error_messages: Vec<String> =
                errors.all().iter().map(|e| e.message.clone()).collect();
            let has_break_error = error_messages
                .iter()
                .any(|msg| msg.contains("'break' can only be used inside loops"));
            assert!(
                has_break_error,
                "Expected break error message, got: {:?}",
                error_messages
            );
        }
    }

    // Test continue outside of loop
    let source_continue = r#"
        continue;
    "#;

    let source_map = SourceMap::new(source_continue.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(_) => panic!("Expected compiler error for continue outside loop"),
        Err(errors) => {
            let error_messages: Vec<String> =
                errors.all().iter().map(|e| e.message.clone()).collect();
            let has_continue_error = error_messages
                .iter()
                .any(|msg| msg.contains("'continue' can only be used inside loops"));
            assert!(
                has_continue_error,
                "Expected continue error message, got: {:?}",
                error_messages
            );
        }
    }
}

#[test]
fn test_break_error_cases_inside_nested_function() {
    // Test break outside of loop
    let source_break: &'static str = r#"
        var continue_loop = true;
        while (continue_loop) {
            
            var lambda = () -> {
                break; // this should be a syntax error.
            };
            continue_loop = false;
        }
    "#;

    let source_map = SourceMap::new(source_break.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(_) => panic!("Expected compiler error for break outside loop"),
        Err(errors) => {
            let error_messages: Vec<String> =
                errors.all().iter().map(|e| e.message.clone()).collect();
            let has_break_error = error_messages
                .iter()
                .any(|msg| msg.contains("'break' can only be used inside loops"));
            assert!(
                has_break_error,
                "Expected break error message, got: {:?}",
                error_messages
            );
        }
    }

    // Test continue outside of loop
    let source_continue = r#"
        for (var i = 0; i < 1; i = i + 1) {
            var lambda = () -> {
                break; // this should be a syntax error.
            };
        }
    "#;

    let source_map = SourceMap::new(source_continue.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(_) => panic!("Expected compiler error for continue outside loop"),
        Err(errors) => {
            let error_messages: Vec<String> =
                errors.all().iter().map(|e| e.message.clone()).collect();
            let has_continue_error = error_messages
                .iter()
                .any(|msg| msg.contains("'break' can only be used inside loops"));
            assert!(
                has_continue_error,
                "Expected continue error message, got: {:?}",
                error_messages
            );
        }
    }
}

#[test]
fn test_continue_error_cases_inside_nested_function() {
    // Test break outside of loop
    let source_break: &'static str = r#"
        var continue_loop = true;
        while (continue_loop) {
            
            var lambda = () -> {
                continue; // this should be a syntax error.
            };
            continue_loop = false;
        }
    "#;

    let source_map = SourceMap::new(source_break.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(_) => panic!("Expected compiler error for break outside loop"),
        Err(errors) => {
            let error_messages: Vec<String> =
                errors.all().iter().map(|e| e.message.clone()).collect();
            let has_break_error = error_messages
                .iter()
                .any(|msg| msg.contains("'continue' can only be used inside loops"));
            assert!(
                has_break_error,
                "Expected break error message, got: {:?}",
                error_messages
            );
        }
    }

    // Test continue outside of loop
    let source_continue = r#"
        for (var i = 0; i < 1; i = i + 1) {
            var lambda = () -> {
                continue; // this should be a syntax error.
            };
        }
    "#;

    let source_map = SourceMap::new(source_continue.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        Ok(_) => panic!("Expected compiler error for continue outside loop"),
        Err(errors) => {
            let error_messages: Vec<String> =
                errors.all().iter().map(|e| e.message.clone()).collect();
            let has_continue_error = error_messages
                .iter()
                .any(|msg| msg.contains("'continue' can only be used inside loops"));
            assert!(
                has_continue_error,
                "Expected continue error message, got: {:?}",
                error_messages
            );
        }
    }
}

#[test]
fn test_null_methods() {
    let source = r#"
        class Foo {}

        var foo = Foo().foo;
        assert_eq(foo, nil);
        
        class Bar : Foo {}
        var bar = Bar().foo;
        assert_eq(bar, nil);
        
        class Baz : Foo {
            baz() {
                return super.foo;
                }
            }
        var baz = Baz().baz();
        assert_eq(baz, nil);
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
fn test_arrays() {
    let source = r#"
        var test_array = [];

        println(test_array);

        var test_array_2 = ["foo", 1, true,];

        println(test_array_2);
        println(test_array_2[0]);
        println(test_array_2[1]);
        println(test_array_2[2]);

        test_array_2[2] = false;
        println(test_array_2[2]);
        println(test_array_2[2]);
        test_array_2.push(nil);
        println(test_array_2.length());
        println(test_array_2[3]);
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
fn test_intrinsic_call_fn() {
    let source = r#"
        fn test_function() {
            return true;
        }

        var foo = test_function.call;

        assert(foo());
        assert(foo.call([]));

        var identity = (x) -> x;

        assert_eq(identity.call([1]), 1);
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
fn test_object_literals() {
    let source = r#"
        var obj = {{}};

        obj.foo = true;
        assert(obj.foo);
        assert_eq(obj.not_exists, nil);

        var obj2 = {{
          bar = "foo",
        }};
        assert_eq(obj2.bar, "foo");

        var baz = 2;
        var obj3 = {{ baz, }};
        assert_eq(obj3.baz, 2);
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
fn test_object_with_lambda_properties() {
    let source = r#"
        var obj = {{
            fun = () -> true,
        }};

        var fun = obj.fun;
        assert(fun()); // works
        assert(obj.fun()); // does not
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
fn test_is_operator() {
    let source = r#"
        assert([] is ARRAY);
        assert({{}} is OBJECT);
        assert("this is a string" is STRING);
        assert(0 is NUMBER);
        assert(true is BOOLEAN);
        assert(false is BOOLEAN);
        assert(nil is NIL);

        fn test_function() {}
        assert(test_function is FUNCTION);
        var test_lambda = () -> nil;
        assert(test_lambda is FUNCTION);

        class TestClass {
          test_method() {}
        }
        assert(TestClass is CLASS);
        var test_instance = TestClass();
        assert(test_instance.test_method is FUNCTION);
        var bound_method = test_instance.test_method;
        assert(bound_method is FUNCTION);
        assert(test_instance is TestClass);
        assert(test_instance is TestClass);
        
        class SubTestClass : TestClass {}
        var sub_test_instance = SubTestClass();
        assert(sub_test_instance is SubTestClass);
        assert(sub_test_instance is TestClass);

        assert(!(test_instance is SubTestClass));
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
fn test_pipe_method() {
    let source = r#"
        fn sum(a, b) {
            return a + b;
        }
        
        assert_eq(sum.call, sum);
        assert_eq(sum.call([1, 2]), 3);
        // assert_eq([1, 2] |> sum.call, 3);
        
        class Foo {
            sum(a, b) {
                return a + b; 
            }
        }
        var foo = Foo();
        assert_eq([1, 2] |> foo.sum, 3);
        // assert_eq([1, 2] |> foo.sum.call, 3);
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
