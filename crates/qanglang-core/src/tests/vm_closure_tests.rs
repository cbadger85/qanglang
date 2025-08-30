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
