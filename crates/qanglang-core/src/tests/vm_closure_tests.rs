use crate::{CompilerPipeline, HeapAllocator, SourceMap, Vm, compile, disassemble_program};

#[test]
fn test_three_level_closure_capture_bug() {
    let source = r#"
        // Test 3-level nested closures using IIFEs to isolate the closure capture bug
        var result = ((a) -> {
            println("Level 1 - a = ");
            println(a);
            return ((b) -> {
                println("Level 2 - b = ");
                println(b);
                println("Level 2 - captured a = ");
                println(a);
                return ((c) -> {
                    println("Level 3 - c = ");
                    println(c);
                    println("Level 3 - captured b = ");
                    println(b);
                    println("Level 3 - captured a = ");
                    println(a);
                    println("Computing a + b + c:");
                    var sum = a + b + c;
                    println(sum);
                    return sum;
                })(4);
            })(3);
        })(2);
        
        println("Final result:");
        println(result);
        println("Expected: 9 (2 + 3 + 4)");
        
        // This assertion will fail due to the upvalue bug (returns 10 instead of 9)
        // Comment out to see the debug output without test failure
        assert_eq(result, 9);
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
fn test_simple_two_level_closure() {
    // Simpler test to debug upvalue resolution
    let source = r#"
        var result = ((a) -> {
            println("Level 1 - a = ");
            println(a);
            return ((b) -> {
                println("Level 2 - b = ");
                println(b);
                println("Level 2 - captured a = ");
                println(a);
                return a + b;
            })(3);
        })(2);
        
        println("Final result:");
        println(result);
        println("Expected: 5 (2 + 3)");
        assert_eq(result, 5);
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
fn test_nested_iefes() {
    let source = r#"
    assert_eq(((a) -> ((b) -> ((c) -> a + b + c)(4))(3))(2), 9); // works

    // Fixed the typo: x + y + z instead of x + y + x
    assert_eq(4||x -> (3||y -> (2||z -> x + y + z|)|)|, 9); // Should be 9 now
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
fn test_map_optional_expression() {
    let source = r#"
        var number = 0;
        var number_plus_one = number?|n -> n + 1|;

        assert_eq(number_plus_one, 1); // works
        assert_eq(number?|n -> n + 1|, 1); // does no not work.
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
fn test_multiple_overflow_chunks() {
    let source = r#"
        fn wrapper_function_that_is_required_for_failure() {
            // Test creating multiple closures that each need overflow chunks
            var shared1 = 100; var shared2 = 200; var shared3 = 300;
            var a1 = 1; var a2 = 2; var a3 = 3; var a4 = 4; var a5 = 5;
            var b1 = 10; var b2 = 20; var b3 = 30; var b4 = 40; var b5 = 50;
            
            var closure1 = () -> shared1 + a1 + a2 + a3 + a4 + a5;
            var closure2 = () -> shared2 + b1 + b2 + b3 + b4 + b5;
            var closure3 = () -> shared3 + shared1 + shared2;
            
            assert_eq(closure1(), 115, "Expected closure1 to return 100 + 15 = 115.");
            assert_eq(closure2(), 350, "Expected closure2 to return 200 + 150 = 350.");
            assert_eq(closure3(), 600, "Expected closure3 to return 300 + 100 + 200 = 600.");   
        }
        wrapper_function_that_is_required_for_failure();
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
fn test_upvalue_chain_linking() {
    let source = r#"
        var v1 = 1; var v2 = 2; var v3 = 3; var v4 = 4; var v5 = 5;
        var v6 = 6; var v7 = 7; var v8 = 8; var v9 = 9; var v10 = 10;
        var v11 = 11; var v12 = 12; var v13 = 13; var v14 = 14; var v15 = 15;
        var v16 = 16; var v17 = 17; var v18 = 18; var v19 = 19; var v20 = 20;
        
        var closure = () -> {
            return v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 +
                v11 + v12 + v13 + v14 + v15 + v16 + v17 + v18 + v19 + v20;
        };
        
        assert_eq(closure(), 210, "Expected closure with 20 upvalues to return sum of 1-20 = 210.");
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
fn test_minimal_upvalue_overflow() {
    let source = r#"
        var shared = 42;
        
        var c1 = () -> shared + 1;
        var c2 = () -> shared + 2;
        var c3 = () -> shared + 3;
        var c4 = () -> shared + 4;
        var c5 = () -> shared + 5;
        
        assert_eq(c1(), 43);
        assert_eq(c2(), 44);
        assert_eq(c3(), 45);
        assert_eq(c4(), 46);
        assert_eq(c5(), 47);
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
            panic!("Failed with compiler errors.");
        }
    }
}

#[test]
fn test_isolated_upvalue_overflow_scenario() {
    let source = r#"
        fn test_upvalue_overflow() {
            var v1 = 1; var v2 = 2; var v3 = 3; var v4 = 4; var v5 = 5;
            var v6 = 6; var v7 = 7; var v8 = 8; var v9 = 9; var v10 = 10;
            var v11 = 11; var v12 = 12; var v13 = 13; var v14 = 14; var v15 = 15;
            
            var closure = () -> {
                return v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11 + v12 + v13 + v14 + v15;
            };
            
            assert_eq(closure(), 120, "Expected closure with 15 upvalues to return sum of 1-15 = 120.");
        }
        test_upvalue_overflow();
        
        fn test_multiple_overflow_chunks() {
            var shared1 = 100; var shared2 = 200; var shared3 = 300;
            var a1 = 1; var a2 = 2; var a3 = 3; var a4 = 4; var a5 = 5;
            var b1 = 10; var b2 = 20; var b3 = 30; var b4 = 40; var b5 = 50;
            
            var closure1 = () -> shared1 + a1 + a2 + a3 + a4 + a5;
            var closure2 = () -> shared2 + b1 + b2 + b3 + b4 + b5;
            var closure3 = () -> shared3 + shared1 + shared2;
            
            assert_eq(closure1(), 115, "Expected closure1 to return 100 + 15 = 115.");
            assert_eq(closure2(), 350, "Expected closure2 to return 200 + 150 = 350.");
            assert_eq(closure3(), 600, "Expected closure3 to return 300 + 100 + 200 = 600.");
        }
        test_multiple_overflow_chunks();
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
            panic!("Failed with compiler errors.");
        }
    }
}

#[test]
fn test_nested_closures() {
    let source = r#"
        fn test_three_level_nested_closures() {
            // Test for deeply nested closures with proper upvalue resolution
            // This ensures variables are correctly captured through multiple closure levels
            var result = ((a) -> {
                return ((b) -> {
                return ((c) -> {
                    return a + b + c;
                })(4);
                })(3);
            })(2);
            
            assert_eq(result, 9, "Expected three-level closure to correctly capture variables: 2 + 3 + 4 = 9, but received " + to_string(result));
        }
        test_three_level_nested_closures();
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
            panic!("Failed with compiler errors.");
        }
    }
}
