use crate::{CompilerPipeline, HeapAllocator, SourceMap, Vm, disassemble_program};

// #[test]
fn test_simple_tail_recursion() {
    let source = r#"
        fn factorial(n, acc) {
            if (n <= 1) {
                return acc;
            } else {
                return factorial(n - 1, acc * n);
            }
        }
        
        var result = factorial(5, 1);
        assert_eq(result, 120);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(&source_map, &mut allocator) {
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

// #[test]
fn test_tail_call_different_function() {
    let source = r#"
        fn even(n) {
            if (n == 0) {
                return true;
            } else {
                return odd(n - 1);
            }
        }
        
        fn odd(n) {
            if (n == 0) {
                return false;
            } else {
                return even(n - 1);
            }
        }
        
        assert_eq(even(100), true);
        assert_eq(odd(99), true);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(&source_map, &mut allocator) {
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

// #[test]
fn test_non_tail_call_still_works() {
    let source = r#"
        fn factorial(n) {
            if (n <= 1) {
                return 1;
            } else {
                return n * factorial(n - 1); // Not a tail call
            }
        }
        
        var result = factorial(5);
        assert_eq(result, 120);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(&source_map, &mut allocator) {
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

// #[test]
fn test_tail_call_with_different_arity() {
    let source = r#"
        fn helper(a, b, c) {
            return a + b + c;
        }
        
        fn caller(x) {
            return helper(x, x * 2, x * 3);
        }
        
        var result = caller(10);
        assert_eq(result, 60); // 10 + 20 + 30
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(&source_map, &mut allocator) {
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

// #[test]
fn test_deep_tail_recursion() {
    // This test would stack overflow without tail call optimization
    let source = r#"
        fn countdown(n) {
            if (n <= 0) {
                return "done";
            } else {
                return countdown(n - 1);
            }
        }
        
        var result = countdown(1000);
        assert_eq(result, "done");
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(&source_map, &mut allocator) {
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
