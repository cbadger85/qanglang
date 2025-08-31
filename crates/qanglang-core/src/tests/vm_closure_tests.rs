use crate::{CompilerPipeline, HeapAllocator, SourceMap, Vm, disassemble_program};

#[test]
fn test_three_level_closure_capture_bug() {
    // BUG: Three-level nested closures have incorrect upvalue resolution
    //
    // ISSUE: In the innermost closure ((c) -> a + b + c), both variables 'a' and 'b'
    // are mapped to the same upvalue slot (index 0), causing 'a' to have the wrong value.
    //
    // EXPECTED BEHAVIOR:
    // - Level 1: a = 2 (parameter)
    // - Level 2: b = 3 (parameter), captured a = 2 (from Level 1)
    // - Level 3: c = 4 (parameter), captured b = 3 (from Level 2), captured a = 2 (from Level 1)
    // - Result: a + b + c = 2 + 3 + 4 = 9
    //
    // ACTUAL BEHAVIOR:
    // - Level 3: captured a = 3 (wrong! should be 2)
    // - Level 3: captured b = 3 (correct)
    // - Result: a + b + c = 3 + 3 + 4 = 10 (wrong!)
    //
    // ROOT CAUSE: The compiler's upvalue resolution incorrectly assigns both 'a' and 'b'
    // to upvalue index 0 in the innermost closure. The disassembly shows two
    // `OP_GET_UPVALUE 0` instructions where there should be `OP_GET_UPVALUE 0` and
    // `OP_GET_UPVALUE 1` for accessing different captured variables.
    //
    // This affects map expressions because they compile to IIFEs internally via
    // compile_map_as_iife(), so nested map expressions exhibit the same bug.

    // RESOURCES:
    //  - https://craftinginterpreters.com/closures.html#upvalues - The VM is based on the one from Crafting Intrepreters. This is how the upvalues work.
    //  - https://github.com/munificent/craftinginterpreters/blob/master/c/compiler.c - Crafting Intrepreters Compiler
    //  - https://github.com/munificent/craftinginterpreters/blob/master/c/vm.c - Crafting Intrepereters VM

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
fn test_upvalue_chain_linking() {
    let source = r#"
        // Test that overflow chunks properly link together
        var v1 = 1; var v2 = 2; var v3 = 3; var v4 = 4; var v5 = 5;
        var v6 = 6; var v7 = 7; var v8 = 8; var v9 = 9; var v10 = 10;
        var v11 = 11; var v12 = 12; var v13 = 13; var v14 = 14; var v15 = 15;
        var v16 = 16; var v17 = 17; var v18 = 18; var v19 = 19; var v20 = 20;
        
        // This closure captures 20 upvalues - needs multiple overflow chunks
        var closure = () -> {
            return v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 +
                v11 + v12 + v13 + v14 + v15 + v16 + v17 + v18 + v19 + v20;
        };
        
        assert_eq(closure(), 210, "Expected closure with 20 upvalues to return sum of 1-20 = 210.");
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
fn test_minimal_upvalue_overflow() {
    let source = r#"
        // Minimal test to isolate upvalue overflow issue
        var shared = 42;
        
        // Create exactly 5 closures - this should trigger overflow
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
            panic!("Failed with compiler errors.");
        }
    }
}

#[test]
fn test_isolated_upvalue_overflow_scenario() {
    let source = r#"
        // Test the exact scenario that's failing in function declarations
        fn test_upvalue_overflow() {
            // Create a function that captures many upvalues (more than 4 inline capacity)
            var v1 = 1; var v2 = 2; var v3 = 3; var v4 = 4; var v5 = 5;
            var v6 = 6; var v7 = 7; var v8 = 8; var v9 = 9; var v10 = 10;
            var v11 = 11; var v12 = 12; var v13 = 13; var v14 = 14; var v15 = 15;
            
            // This closure captures 15 upvalues - should trigger overflow chunks
            var closure = () -> {
                return v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11 + v12 + v13 + v14 + v15;
            };
            
            assert_eq(closure(), 120, "Expected closure with 15 upvalues to return sum of 1-15 = 120.");
        }
        test_upvalue_overflow();
        
        fn test_multiple_overflow_chunks() {
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
        test_multiple_overflow_chunks();
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
            panic!("Failed with compiler errors.");
        }
    }
}
