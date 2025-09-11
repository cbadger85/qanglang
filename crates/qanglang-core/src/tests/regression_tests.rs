use crate::{CompilerPipeline, HeapAllocator, SourceMap, Vm, disassemble_program};

#[test]
fn test_class_super_keyword() {
    let source = r#"
      fn test_class_super_keyword() {
        class TestClass {
          test_field = 42;
        }

        class OtherClass : TestClass {
          other_field;

          init() {
            this.other_field = super.test_field;
          }
        }  

        assert_eq(TestClass().test_field, OtherClass().other_field, "Expected fields to be equal.");
      }

      test_class_super_keyword();
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    // match CompilerPipeline::new(source_map, &mut allocator).run() {
    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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
fn test_class_inheritance() {
    let source = r#"
      fn test_class_inheritance() {
        class TestClass {
          test_field = 42;

          test_method() {
            return 42;
          }
        }

        class OtherClass : TestClass {}
        assert_eq(
          OtherClass().test_field, 
          42, 
          "Expected 42 but received " + (OtherClass().test_field |> to_string)
        );

        assert_eq(
          OtherClass().test_method(), 
          42, 
          "Expected 42 but received " + (OtherClass().test_method() |> to_string)
        );
        
        var test_method = OtherClass().test_method;
        assert_eq(
          test_method(), 
          42, 
          "Expected 42 but received " + (test_method() |> to_string)
        );
      }

      test_class_inheritance();
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    // match CompilerPipeline::new(source_map, &mut allocator).run() {
    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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
fn test_nested_loops_with_break() {
    let source = r#"
      fn test_nested_loops_with_break() {
        var outer_count = 0;
        var inner_count = 0;
        for (var i = 0; i < 5; i += 1) {
          outer_count = outer_count + 1;
          for (var j = 0; j < 5; j += 1) {
            inner_count = inner_count + 1;
            if (j == 2) {
              break;
            }
          }
        }
        assert_eq(outer_count, 5, "Expected outer loop to run 5 times.");
        assert_eq(inner_count, 15, "Expected inner loop to run 3 times per outer (5*3=15).");
      }

      test_nested_loops_with_break();
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
        // match CompilerPipeline::new(source_map, &mut allocator).run() {
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
fn test_nested_loops_with_continue() {
    let source = r#"
      fn test_nested_loops_with_continue() {
        var total = 0;
        for (var i = 0; i < 3; i = i + 1) {
          for (var j = 0; j < 5; j = j + 1) {
            if (j == 2) {
              continue;
            }
            total = total + 1;
          }
        }
        assert_eq(total, 12, "Expected 4 iterations per outer loop * 3 = 12.");
      }

      test_nested_loops_with_continue();
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
        // match CompilerPipeline::new(source_map, &mut allocator).run() {
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
fn test_nested_for_loops() {
    let source = r#"
      fn test_nested_for_loops() {
        var count = 0;
        for (var i = 0; i < 10; i += 1) {
          for (var j = 0; j < 10; j += 1) {
            count = count + 1;
          }
        }

        assert_eq(count, 100, "Expected count to equal 100 ");
      }

      test_nested_for_loops();
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
        // match CompilerPipeline::new(source_map, &mut allocator).run() {
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

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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
fn test_iterator_stdlib() {
    let source = r#"
        var arr = [1, 2, 3, 4];

        var new_arr = iter_array(arr) 
        |> iter_map((item) -> item * 3) 
        |> iter_filter((item) -> item % 2 == 0) 
        |> iter_collect();
        assert_eq(new_arr.length(), 2);
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    // match CompilerPipeline::new(source_map, &mut allocator).run() {
    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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
