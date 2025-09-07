use crate::{
    HeapAllocator, SourceMap, Value, Vm, backend::assembler::CompilerPipeline, compile,
    disassemble_program, memory::ClosureHandle,
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    if let Ok(program) = compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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
fn test_immediately_invoked_functional_expressions() {
    let source = r#"
  assert_eq((() -> nil)(), nil);
  assert_eq(() -> { return nil; }(), nil);
"#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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
            c = 1;

            a() {
                return 41;
            }
        }

        class B : A {
            b() {
                return super.a() + super.c;
            }
        }
        var value = B().a();
        assert_eq(value, 41, "Expected '41', recieved " + (value |> to_string));
        var value_2 = B().b();
        assert_eq(value_2, 42, "Expected '42', recieved " + (value |> to_string));
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    // match CompilerPipeline::new(source_map, &mut allocator).run() {
    match compile(&source_map, &mut allocator) {
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

    // match CompilerPipeline::new(source_map, &mut allocator).run() {
    match compile(&source_map, &mut allocator) {
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

    // match CompilerPipeline::new(source_map, &mut allocator).run() {
    match compile(&source_map, &mut allocator) {
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

    // match CompilerPipeline::new(source_map, &mut allocator).run() {
    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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

    match compile(&source_map, &mut allocator) {
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
        assert(foo.apply([]));

        var identity = (x) -> x;

        assert_eq(identity.apply([1]), 1);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
        // match compile(&source_map, &mut allocator) {
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
        // match compile(&source_map, &mut allocator) {
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
        assert(test_instance is OBJECT);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
        
        assert_eq(sum.apply([1, 2]), 3);
        assert_eq([1, 2] |> sum.apply, 3);
        
        class Foo {
            sum(a, b) {
                return a + b; 
            }
        }
        var foo = Foo();
        assert_eq([1, 2] |> foo.sum.apply, 3);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_pipe_with_intrinsic() {
    let source = r#"
        var arr = [true];
        var result = [false] |> arr.concat;
        println(result);
        assert_eq(result.length(), 2);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_pipe_with_apply() {
    let source = r#"
        var arr = [true];
        var result = [false] |> arr.concat();
        println(result);
        assert_eq(result.length(), 2);
        assert_eq(result[0], true);
        assert_eq(result[1], false);

        
        var identity = (x) -> x;
        assert_eq(true |> identity, true);
        assert_eq(true |> identity(), true);
        var sum = (a, b) -> a + b;
        assert_eq(1 |> sum(2), 3);
        assert_eq([1, 2] |> sum.apply, 3);    
        assert_eq([1, 2] |> sum.apply(), 3); 
        assert_throws(() -> 1 |> sum.apply(2));
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_call_and_apply_intrinsics() {
    let source = r#"
        var sum = (a, b) -> a + b;
        class Math {
            sum(a, b) {
                return a + b;
            }
        }

        assert_eq(sum.call(1, 2), 3);
        assert_eq(sum.apply([1, 2]), 3);
        var math = Math();
        assert_eq(math.sum.call(1, 2), 3);
        assert_eq(math.sum.apply([1, 2]), 3);

        var sum_call = sum.call;
        assert_eq(sum_call(1, 2), 3);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_optional_properties() {
    let source = r#"
        var obj = {{}};

        assert_eq(obj.foo?.bar, nil);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_optional_calling_of_methods() {
    let source = r#"
        var obj = {{}};

        assert_eq(obj.foo?.call(), nil);
        assert_eq(nil?.call(), nil);
        assert_throws(() -> nil?.length());
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_class_init() {
    let source = r#"
        class Foo {
            bar;

            init() {
                this.bar = true;
            }
        }

        var foo = Foo();
        assert_throws(() -> foo.init());
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_map_expression() {
    let source = r#"
        var number = 0;

        var number_plus_one = number||n -> n + 1|;

        assert_eq(number_plus_one, 1);

        fn test_map_expression_with_value() {
            var number = 0;
            var one = 1;
            var other_one = 1;
            var other_other_other_one = 1;
            var one_again = 1;
            var yet_another_one = 1;
            var last_one = 1;

            assert_eq(number||n -> n + one + other_one + other_other_other_one + one_again + yet_another_one|, 5);
        }

        test_map_expression_with_value();

        var lazy_true = true|| x -> () -> x|;
        assert(lazy_true());

        var value = false;
        assert(value||v -> !v|);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    // match compile(&source_map, &mut allocator) {
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
fn test_map_expression_with_boolean() {
    let source = r#"
        var obj = {{
        inner = {{ value = true }},
        }};
        var result = println(obj||o -> o.inner|.value);

        // println(obj||o -> o.inner|.value);

        // println(!(obj||o -> o.inner|.value));

        // if (obj||o -> o.inner|.value) { // this works
        //     println("IT WORKED!"); 
        //     println(obj||o -> o.inner|.value);
        // }

        // var value = obj||o -> o.inner|.value; // works.
        // assert(value);
        // assert(obj||o -> o.inner|.value); // doesn't work.

        /*
            Are there other contexts where it fails?

            Does var result = println(obj||o -> o.inner|.value) fail? // no
            Does return obj||o -> o.inner|.value work? // yes
            Does [obj||o -> o.inner|.value] (array literal) work? // no


            Is this specifically about function arguments, or about nested expressions?

            Does (obj||o -> o.inner|.value) + 1 work? // no
            Does !(obj||o -> o.inner|.value) work? // no

        */
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        // match compile(&source_map, &mut allocator) {
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

fn test_inheritance_with_n_methods(n: usize) -> Result<(), String> {
    // Create base class with n methods
    let mut base_methods = String::new();
    for i in 1..=n {
        base_methods.push_str(&format!(
            "        baseMethod{:03}() {{ return \"base{}\"; }}\n",
            i, i
        ));
    }

    // Create derived class that calls super methods
    let mut derived_methods = String::new();
    for i in 1..=n {
        derived_methods.push_str(&format!(
            "        derivedMethod{:03}() {{ return super.baseMethod{:03}(); }}\n",
            i, i
        ));
    }

    let source = format!(
        r#"
        class BaseClass {{
{}
        }}

        class DerivedClass : BaseClass {{
{}
        }}

        var obj = DerivedClass();
        assert_eq(obj.derivedMethod001(), "base1");
        assert_eq(obj.derivedMethod{:03}(), "base{}");
    "#,
        base_methods, derived_methods, n, n
    );

    let source_map = SourceMap::new(source);
    let mut allocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
        Ok(program) => {
            let mut vm = Vm::new(allocator).set_gc_status(false).set_debug(false);
            match vm.interpret(program) {
                Ok(_) => Ok(()),
                Err(error) => Err(format!(
                    "Runtime error with {} methods: {}",
                    n, error.message
                )),
            }
        }
        Err(errors) => {
            let error_msgs: Vec<String> = errors.all().iter().map(|e| e.message.clone()).collect();
            Err(format!(
                "Compiler error with {} methods: {:?}",
                n, error_msgs
            ))
        }
    }
}

#[test]
fn test_inheritance_threshold() {
    println!("Testing inheritance threshold...");

    // Test with different numbers of methods to find the exact threshold
    for n in [3, 4, 5] {
        match test_inheritance_with_n_methods(n) {
            Ok(_) => println!("✓ {} methods: PASS", n),
            Err(e) => println!("✗ {} methods: FAIL - {}", n, e),
        }
    }
}

#[test]
fn test_class_without_methods() {
    let source = r#"
        class TestClass {}
        println("Creating class instance...");
        var obj = TestClass();
        println("Class created successfully");
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
        Ok(program) => {
            disassemble_program(&allocator);
            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(false)
                .interpret(program)
            {
                Ok(_) => println!("✓ Empty class test passed"),
                Err(error) => {
                    println!("✗ Empty class test failed: {}", error.message);
                    panic!("{}", error);
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.");
        }
    }
}

#[test]
fn test_identifier_constant_corruption() {
    let source = r#"
        class TestClass {
            method1() { return 1; }
            method2() { return 2; }
            method3() { return 3; }
            method4() { return 4; }
            method5() { return 5; }
        }
        
        println("Creating class instance...");
        var obj = TestClass();
        println("Class created successfully");
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
        Ok(program) => {
            // Enable disassembly to see the bytecode
            disassemble_program(&allocator);

            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(false) // Disable debug output for cleaner view
                .interpret(program)
            {
                Ok(_) => println!("✓ Test passed - no identifier corruption"),
                Err(error) => {
                    println!("✗ Test failed: {}", error.message);
                    panic!("{}", error);
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
            panic!("Failed with compiler errors.");
        }
    }
}

#[test]
fn test_fifth_method_debug() {
    let source = r#"
        class BaseClass {
            baseMethod001() { return "base1"; }
            baseMethod002() { return "base2"; }
            baseMethod003() { return "base3"; }
            baseMethod004() { return "base4"; }
            baseMethod005() { return "base5"; }
        }

        class DerivedClass : BaseClass {
            derivedMethod001() { return super.baseMethod001(); }
            derivedMethod002() { return super.baseMethod002(); }
            derivedMethod003() { return super.baseMethod003(); }
            derivedMethod004() { return super.baseMethod004(); }
            derivedMethod005() { return super.baseMethod005(); }
        }

        // Test each method individually
        var obj = DerivedClass();
        println("Testing method 1...");
        assert_eq(obj.derivedMethod001(), "base1");
        println("Testing method 2...");
        assert_eq(obj.derivedMethod002(), "base2");
        println("Testing method 3...");
        assert_eq(obj.derivedMethod003(), "base3");
        println("Testing method 4...");
        assert_eq(obj.derivedMethod004(), "base4");
        println("Testing method 5...");
        assert_eq(obj.derivedMethod005(), "base5");
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
        Ok(program) => {
            // Enable disassembly to see the bytecode
            disassemble_program(&allocator);

            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(true) // Enable debug output
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
            panic!("Failed with compiler errors.");
        }
    }
}

#[test]
fn test_debug_16bit_identifiers() {
    let source = r#"
        class BaseClass {
        baseMethod001() { return "base1"; }
        baseMethod002() { return "base2"; }
        baseMethod003() { return "base3"; }
        baseMethod004() { return "base4"; }
        baseMethod005() { return "base5"; }
        baseMethod006() { return "base6"; }
        baseMethod007() { return "base7"; }
        baseMethod008() { return "base8"; }
        baseMethod009() { return "base9"; }
        baseMethod010() { return "base10"; }
        }

        class DerivedClass : BaseClass {
        derivedMethod001() { return super.baseMethod001(); }
        derivedMethod002() { return super.baseMethod002(); }
        derivedMethod003() { return super.baseMethod003(); }
        derivedMethod004() { return super.baseMethod004(); }
        derivedMethod005() { return super.baseMethod005(); }
        }

        // Test super method calls with many identifiers (GetSuper16/SuperInvoke16)
        var obj = DerivedClass();

        assert_eq(obj.derivedMethod001(), "base1");
        assert_eq(obj.derivedMethod002(), "base2");
        assert_eq(obj.derivedMethod003(), "base3");
        assert_eq(obj.derivedMethod004(), "base4");
        assert_eq(obj.derivedMethod005(), "base5");
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
        Ok(program) => {
            // Enable disassembly to see the bytecode
            disassemble_program(&allocator);

            match Vm::new(allocator)
                .set_gc_status(false)
                .set_debug(true) // Enable debug output
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
fn test_map_optional_expression() {
    let source = r#"
        // With non-nil values
        assert_eq(42?|n -> n + 1|, 43);
        // assert_eq("hello"?|s -> s + "!"|, "hello!");
        // assert(true?|b -> !b| == false);
        
        // With nil values (should all return nil)
        // assert_eq(nil?|n -> n + 1|, nil);
        // assert_eq(nil?|s -> s + "!"|, nil);
        // assert_eq(nil?|x -> x.property|, nil);
        // assert_eq(nil?|x -> some_function(x)|, nil);
        
        // Complex expressions with nil
        var maybe_obj = nil;
        // assert_eq(maybe_obj?|o -> o.value * 1000|, nil);
        
        // Chained optional maps
        var obj = {{ value = 5 }};
        // assert_eq(obj?|o -> o.value|?|n -> n * 2|, 10);
        
        // Optional map with nil in chain
        var nil_obj = nil;
        // assert_eq(nil_obj?|o -> o.value|?|n -> n * 2|, nil);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new(source_map, &mut allocator).run() {
        // match compile(&source_map, &mut allocator) {
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
fn test_simple_16bit_super_call() {
    let source = r#"
        class BaseClass {
            method001() { return "base1"; }
            method002() { return "base2"; }
            method003() { return "base3"; }
            method004() { return "base4"; }
            method005() { return "base5"; }
        }

        class DerivedClass : BaseClass {
            derivedMethod() { return super.method005(); }
        }

        var obj = DerivedClass();
        assert_eq(obj.derivedMethod(), "base5");
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_op_constant_16() {
    let source = r#"
        fn test_constant16_usage() {
        var total = 000 + 001 + 002 + 003 + 004 + 005 + 006 + 007 + 008 + 009
                    + 010 + 011 + 012 + 013 + 014 + 015 + 016 + 017 + 018 + 019
                    + 020 + 021 + 022 + 023 + 024 + 025 + 026 + 027 + 028 + 029
                    + 030 + 031 + 032 + 033 + 034 + 035 + 036 + 037 + 038 + 039
                    + 040 + 041 + 042 + 043 + 044 + 045 + 046 + 047 + 048 + 049
                    + 050 + 051 + 052 + 053 + 054 + 055 + 056 + 057 + 058 + 059
                    + 060 + 061 + 062 + 063 + 064 + 065 + 066 + 067 + 068 + 069
                    + 070 + 071 + 072 + 073 + 074 + 075 + 076 + 077 + 078 + 079
                    + 080 + 081 + 082 + 083 + 084 + 085 + 086 + 087 + 088 + 089
                    + 090 + 091 + 092 + 093 + 094 + 095 + 096 + 097 + 098 + 099
                    + 100 + 101 + 102 + 103 + 104 + 105 + 106 + 107 + 108 + 109
                    + 110 + 111 + 112 + 113 + 114 + 115 + 116 + 117 + 118 + 119
                    + 120 + 121 + 122 + 123 + 124 + 125 + 126 + 127 + 128 + 129
                    + 130 + 131 + 132 + 133 + 134 + 135 + 136 + 137 + 138 + 139
                    + 140 + 141 + 142 + 143 + 144 + 145 + 146 + 147 + 148 + 149
                    + 150 + 151 + 152 + 153 + 154 + 155 + 156 + 157 + 158 + 159
                    + 160 + 161 + 162 + 163 + 164 + 165 + 166 + 167 + 168 + 169
                    + 170 + 171 + 172 + 173 + 174 + 175 + 176 + 177 + 178 + 179
                    + 180 + 181 + 182 + 183 + 184 + 185 + 186 + 187 + 188 + 189
                    + 190 + 191 + 192 + 193 + 194 + 195 + 196 + 197 + 198 + 199
                    + 200 + 201 + 202 + 203 + 204 + 205 + 206 + 207 + 208 + 209
                    + 210 + 211 + 212 + 213 + 214 + 215 + 216 + 217 + 218 + 219
                    + 220 + 221 + 222 + 223 + 224 + 225 + 226 + 227 + 228 + 229
                    + 230 + 231 + 232 + 233 + 234 + 235 + 236 + 237 + 238 + 239
                    + 240 + 241 + 242 + 243 + 244 + 245 + 246 + 247 + 248 + 249
                    + 250 + 251 + 252 + 253 + 254 + 255 + 256 + 257 + 258 + 259
                    + 260 + 261 + 262 + 263 + 264 + 265 + 266 + 267 + 268 + 269
                    + 270 + 271 + 272 + 273 + 274 + 275 + 276 + 277 + 278 + 279
                    + 280 + 281 + 282 + 283 + 284 + 285 + 286 + 287 + 288 + 289
                    + 290 + 291 + 292 + 293 + 294 + 295 + 296 + 297 + 298 + 299
                    + 300 + 301 + 302 + 303 + 304 + 305 + 306 + 307 + 308 + 309;
        
            var total_is_number = total is NUMBER;
            assert(total_is_number);
            assert_eq(total, 47895);
        }
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_stdlib_call() {
    let source = r#"
        var one = 1;

        var three = one |> transform((x) -> x + 2);

        assert_eq(three, 3);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
fn test_class_instance_inside_function() {
    let source = r#"
        class Iterator {
            has_next() {}

            next() {}
        }

        class ArrayIterator : Iterator {
            index = 0;

            init(arr) {
                this.arr = arr;
            }

            has_next() {
                return this.index < this.arr.length();
            }

            next() {
                if (!this.has_next()) {
                return nil;
                }

                var value = this.arr[this.index];
                this.index += 1;

                return value;
            }
        }

        fn iter_array(arr) { // This works
            var iter = ArrayIterator(arr);

            println(iter); // prints "instanceof ArrayIterator"

            return iter;
        }

        fn iter_array2(arr) { // This doesn't
            return ArrayIterator(arr);
        }
        
        fn test() {
            return "test";
        }

        fn test2() {
            return test(); // Also works
        }

        println(test2()); // prints "test"


        println(iter_array2([1, 2, 3, 4])); // prints "nil"

        var value = iter_array([1, 2, 3, 4]).next();
        var value2 = iter_array2([1, 2, 3, 4]).next();
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match compile(&source_map, &mut allocator) {
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
