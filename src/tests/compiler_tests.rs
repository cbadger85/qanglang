use crate::{
    CompilerPipeline, KangFunction, ObjectHeap, SourceMap, Vm, debug::disassemble_chunk,
    disassemble_program,
};

#[test]
fn test_display() {
    let source = r#"
  1 * 2 / 3 + 4 - 5 + -1;
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_run() {
    let source = r#"
        var hello_world = "hello" + " " + "world!";
        println(hello_world);
        assert_eq(hello_world, "hello world!", "Expect \"hello world\" to equal \"hello world!\"");
        assert_eq("hello world", "hello world", "Expect \"hello world\" to equal \"hello world\"");
        var two = nil;
        assert_eq(two, nil);
        two = 2;
        assert_eq(two, 2);

        {
            var two = "2";
            assert_eq(two, "2");
            println(two);
        }   
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        match Vm::new(heap).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_while() {
    let source = r#"
        var a = 10;

        while (a > 0) {
            println(a);
            a = a - 1;
        } 
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            let function: &KangFunction = heap
                .get(program.into())
                .expect("expected function, found none.")
                .try_into()
                .expect("expected function, found none.");
            disassemble_chunk(&function.chunk, &heap, "script.ql");
            match Vm::new(heap).set_debug(false).interpret(program) {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", error.message)
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
fn test_subtraction() {
    let source = r#"
        var a = 3;
        a = 1 - a;
        println(a - -3);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            let function: &KangFunction = heap
                .get(program.into())
                .expect("expected function, found none.")
                .try_into()
                .expect("expected function, found none.");
            disassemble_chunk(&function.chunk, &heap, "script.ql");
            match Vm::new(heap).set_debug(false).interpret(program) {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", error.message)
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", error.message);
            }
        }
    }
}

#[test]
fn test_locals() {
    let source = r#"
        var two = "two";
        println(two);
        two = 2;
        {
            var two = "2";
            println(two);
            println("TEST");
        }
        println(two);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        let mut vm = Vm::new(heap);
        match vm.interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_conditionals() {
    let source = r#"
        if (true) {
            println("It's true!");
        } else {
            "This should not print" + true;
            }
            
        if (false) {
            "This should not print" + true;
        } else {
            println("It's false!"); 
        }
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        let mut vm = Vm::new(heap);
        match vm.interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_ternary_expressions() {
    let source = r#"
        var result1 = true ? "correct" : "wrong";
        println(result1);
        
        var result2 = false ? "wrong" : "correct";
        println(result2);
        
        var result3 = nil ? "wrong" : "correct";
        println(result3);

        var result4 = true ? "should do this" : false ? "don't do this" : "nope";
        println(result4);

        var result5 = false ? "should not do this" : true ? "do this" : "nope";
        println(result5);

        var result6 = false ? "should not do this" : false ? "don't do this" : "yup";
        println(result6);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        let mut vm = Vm::new(heap);
        match vm.interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_logical_expressions() {
    let source = r#"
        var result1 = true or false;
        println(result1);
        
        var result2 = false or true;
        println(result2);
        
        var result3 = false or false;
        println(result3);
        
        var result4 = true and false;
        println(result4);
        
        var result5 = true and true;
        println(result5);
        
        var result6 = false and true;
        println(result6);
        
        var result7 = "hello" or nil;
        println(result7);
        
        var result8 = nil and "world";
        println(result8);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        let mut vm = Vm::new(heap);
        match vm.interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn math_operations_test() {
    let source = r#"
  1 / 1 + 2 * (12 % 5);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        match Vm::new(heap).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn equality_operations_test() {
    let source = r#"
        "true" != "true";
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        match Vm::new(heap).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn comparison_operations_test() {
    let source = r#"
        println(10 >= 9);     // true
        println(10 >= 10);    // true
        println(10 >= 11);    // false
        println(10 > 9);      // true
        println(9 > 9);       // false
        println(10 > 11);     // false
        println(9 <= 10);     // true
        println(10 <= 10);    // true
        println(11 <= 10);    // false
        println(9 < 10);      // true
        println(9 < 9);       // false
        println(11 < 10);     // false
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        match Vm::new(heap).set_debug(false).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_runtime_error_with_source_span() {
    let source = r#"
  -"hello";
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        match Vm::new(heap).interpret(program) {
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
fn test_booleans() {
    let source = r#"
  !true;
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        match Vm::new(heap).interpret(program) {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", error.message)
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_targeted_type_error_spans() {
    let source = r#"
  42 + "hello";
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        match Vm::new(heap).interpret(program) {
            Ok(_) => {
                panic!("Expected runtime error for adding number and string")
            }
            Err(error) => {
                let error_message = error.message;
                assert!(error_message.contains("Cannot add number to string"));
                println!(
                    "Targeted error correctly points to string operand: {}",
                    error_message
                );
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_left_operand_error_span() {
    let source = r#"
  true + 5;
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    if let Ok(program) = CompilerPipeline::new(source_map, &mut heap).run() {
        let function: &KangFunction = heap
            .get(program.into())
            .expect("expected function, found none.")
            .try_into()
            .expect("expected function, found none.");
        disassemble_chunk(&function.chunk, &heap, "script.ql");
        match Vm::new(heap).interpret(program) {
            Ok(_) => {
                panic!("Expected runtime error for adding boolean and number")
            }
            Err(error) => {
                let error_message = error.message;
                assert!(error_message.contains("Cannot add boolean to number"));
                println!(
                    "Targeted error correctly points to boolean operand: {}",
                    error_message
                );
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_initializing_local_variable_with_itself() {
    let source = r#"
        {
            var a = "outer";
            {
                var a = a;
            }
        }
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = &errors.all()[0].message;
            assert!(
                error_message.contains("Cannot read local variable during its initialization.")
            );
        }
    }
}

#[test]
fn test_for_loop() {
    let source = r#"
        var sum = 0;
        for (var i = 1; i <= 3; i = i + 1) {
            println(i);
            sum = sum + i;
        }
        println(sum);
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            let function: &KangFunction = heap
                .get(program.into())
                .expect("expected function, found none.")
                .try_into()
                .expect("expected function, found none.");
            disassemble_chunk(&function.chunk, &heap, "for_loop_test.ql");
            match Vm::new(heap).set_debug(false).interpret(program) {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", error.message)
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
fn test_initializing_local_variable_with_same_name() {
    let source = r#"
        {
            var a = "a";
            var a = "b";
        }
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = &errors.all()[0].message;
            assert!(error_message.contains("Already a variable with this name in this scope."));
        }
    }
}

#[test]
fn test_native_functions() {
    let source = r#"
        assert(true, "This should not error.");
        assert(false, "This should error.");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            let function: &KangFunction = heap
                .get(program.into())
                .expect("expected function, found none.")
                .try_into()
                .expect("expected function, found none.");
            disassemble_chunk(&function.chunk, &heap, "script.ql");
            match Vm::new(heap).set_debug(false).interpret(program) {
                Ok(_) => {
                    panic!("Expected error but received none.")
                }
                Err(error) => {
                    assert!(error.message.contains("This should error."));
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
fn test_escape_sequences_in_print() {
    let source = r#"
        print("And then she said, \"Go to hell!\"");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            let function: &KangFunction = heap
                .get(program.into())
                .expect("expected function, found none.")
                .try_into()
                .expect("expected function, found none.");
            disassemble_chunk(&function.chunk, &heap, "script.ql");
            match Vm::new(heap).set_debug(false).interpret(program) {
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
fn test_comments_in_strings() {
    let source = r#"
        print("\\\\This should\n not care.");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            let function: &KangFunction = heap
                .get(program.into())
                .expect("expected function, found none.")
                .try_into()
                .expect("expected function, found none.");
            disassemble_chunk(&function.chunk, &heap, "script.ql");
            match Vm::new(heap).set_debug(false).interpret(program) {
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
fn test_function_calls() {
    let source = r#"
        fn this_is_a_test() {
            return "hello?";
        }

        assert_eq(this_is_a_test(), "hello?");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            disassemble_program(&heap);
            match Vm::new(heap).set_debug(false).interpret(program) {
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
fn test_nested_function_calls() {
    let source = r#"
        fn this_is_a_test() {
            fn inner() {
                return "hello?";
            }

            return inner();
        }

        assert_eq(this_is_a_test(), "hello?");
  "#;
    let source_map = SourceMap::new(source.to_string());
    let mut heap: ObjectHeap = ObjectHeap::new();

    match CompilerPipeline::new(source_map, &mut heap).run() {
        Ok(program) => {
            disassemble_program(&heap);
            match Vm::new(heap).set_debug(false).interpret(program) {
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
