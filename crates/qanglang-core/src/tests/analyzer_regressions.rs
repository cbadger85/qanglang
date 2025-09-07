use crate::{CompilerPipeline, HeapAllocator, SourceMap, Vm, compile, disassemble_program};

#[test]
fn test_calling_init_on_super_without_init() {
    let source = r#"
      fn test_calling_init_on_super_without_init() {
        class TestClass { }

        class OtherClass : TestClass {

          init() {
            super.init();
          }
        }  

        var test = OtherClass();
      }

      test_calling_init_on_super_without_init();
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    // match CompilerPipeline::new(source_map, &mut allocator).run() {
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
fn test_class_super_method_calls() {
    let source = r#"
      fn test_class_super_method_calls() {
        class TestClass {
          test_method() {
            return 42;
          }
        }

        class OtherClass : TestClass {
          test_method() {
            return super.test_method() - 42;
          }
        }  

        assert_eq(OtherClass().test_method(), 0, "Expected 0.");
      }
      
      test_class_super_method_calls();
    "#;

    let source_map = SourceMap::new(source.to_string());
    let mut allocator = HeapAllocator::new();

    // match CompilerPipeline::new(source_map, &mut allocator).run() {
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
