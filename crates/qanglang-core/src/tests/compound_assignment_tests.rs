use crate::{CompilerPipeline, HeapAllocator, SourceMap, Vm};

#[test]
fn test_compound_assignment_operators() {
    let source = r#"
        // Test += with variables
        var a = 5;
        a += 3;
        assert_eq(a, 8);

        // Test -= with variables
        var b = 10;
        b -= 4;
        assert_eq(b, 6);

        // Test *= with variables
        var c = 3;
        c *= 4;
        assert_eq(c, 12);

        // Test /= with variables
        var d = 20;
        d /= 5;
        assert_eq(d, 4);

        // Test %= with variables
        var e = 17;
        e %= 5;
        assert_eq(e, 2);

        // Test with string concatenation
        var str = "Hello";
        str += " World";
        assert_eq(str, "Hello World");
    "#;

    let source_map = SourceMap::from_source(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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
            panic!("Failed with compiler errors.");
        }
    }
}

#[test]
fn test_compound_assignment_with_properties() {
    let source = r#"
        class TestObj {
            init() {
                this.value = 10;
            }
        }

        var obj = TestObj();
        
        // Test += with property
        obj.value += 5;
        assert_eq(obj.value, 15);

        // Test *= with property
        obj.value *= 2;
        assert_eq(obj.value, 30);

        // Test -= with property
        obj.value -= 10;
        assert_eq(obj.value, 20);
    "#;

    let source_map = SourceMap::from_source(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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
            panic!("Failed with compiler errors.");
        }
    }
}

#[test]
fn test_compound_assignment_with_arrays() {
    let source = r#"
        var arr = [1, 2, 3, 4, 5];
        
        // Test += with array element
        arr[0] += 10;
        assert_eq(arr[0], 11);

        // Test *= with array element
        arr[1] *= 3;
        assert_eq(arr[1], 6);

        // Test -= with array element
        arr[2] -= 1;
        assert_eq(arr[2], 2);
    "#;

    let source_map = SourceMap::from_source(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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
            panic!("Failed with compiler errors.");
        }
    }
}
