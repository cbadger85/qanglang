use crate::{GlobalCompilerPipeline, HeapAllocator, Vm, disassemble_program};

/// Helper function to compile source code for tests
fn compile_test_source(source: &str) -> (crate::QangProgram, HeapAllocator) {
    let mut allocator = HeapAllocator::new();
    let program = GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator)
        .expect("Test compilation should succeed");
    (program, allocator)
}

#[test]
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

    let (program, allocator) = compile_test_source(source);
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

#[test]
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

    let (program, allocator) = compile_test_source(source);
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

#[test]
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

    let (program, allocator) = compile_test_source(source);
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

#[test]
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

    let (program, allocator) = compile_test_source(source);
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

#[test]
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

    let (program, allocator) = compile_test_source(source);
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

#[test]
fn test_class_instantiation_not_tail_optimized() {
    // Classes should NOT be tail-call optimized since they create instances
    let source = r#"
        class Counter {
            init(value) {
                this.value = value;
            }
        }

        fn makeCounter(n) {
            return Counter(n);
        }

        var c = makeCounter(42);
        assert_eq(c.value, 42);
    "#;

    let (program, allocator) = compile_test_source(source);
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

#[test]
fn test_indirect_call_not_tail_optimized() {
    // Indirect calls through variables should NOT be tail-call optimized
    // since we can't statically determine if they're functions or classes
    let source = r#"
        fn add(a, b) {
            return a + b;
        }

        fn indirectCall(f, x, y) {
            return f(x, y);
        }

        var result = indirectCall(add, 10, 20);
        assert_eq(result, 30);
    "#;

    let (program, allocator) = compile_test_source(source);
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
