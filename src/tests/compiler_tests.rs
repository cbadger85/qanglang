use crate::{CompilerPipeline, SourceMap, Vm, debug::disassemble_chunk, error::pretty_print_error};
use std::rc::Rc;

#[test]
fn test_display() {
    let source = r#"
  1 * 2 / 3 + 4 - 5 + -1;
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        disassemble_chunk(
            &artifact.source_map,
            &artifact.chunk,
            &artifact.heap,
            "script.ql",
        );
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_run() {
    let source = r#"
        var hello_world = "hello" + " " + "world!";
        print(hello_world);
        var two = nil;
        print(two);
        two = 2;
        print(two);

        {
            var two = "2";
            print(two);
        }   
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        disassemble_chunk(
            &artifact.source_map,
            &artifact.chunk,
            &artifact.heap,
            "script.ql",
        );
        match Vm::new(artifact).interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
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
            print(a);
            a = a - 1;
        } 
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    match CompilerPipeline::new((*source_map).clone()).run() {
        Ok(artifact) => {
            disassemble_chunk(
                &artifact.source_map,
                &artifact.chunk,
                &artifact.heap,
                "script.ql",
            );
            match Vm::new(artifact).set_debug(false).interpret() {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", pretty_print_error(&source_map, &error))
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", pretty_print_error(&source_map, error));
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
        print(a - -3);
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    match CompilerPipeline::new((*source_map).clone()).run() {
        Ok(artifact) => {
            disassemble_chunk(
                &artifact.source_map,
                &artifact.chunk,
                &artifact.heap,
                "script.ql",
            );
            match Vm::new(artifact).set_debug(false).interpret() {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", pretty_print_error(&source_map, &error))
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                pretty_print_error(&source_map, error);
            }
        }
    }
}

#[test]
fn test_locals() {
    let source = r#"
        var two = "two";
        print(two);
        two = 2;
        {
            var two = "2";
            print(two);
            print("TEST");
        }
        print(two);
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        disassemble_chunk(
            &artifact.source_map,
            &artifact.chunk,
            &artifact.heap,
            "script.ql",
        );
        let mut vm = Vm::new(artifact);
        match vm.interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
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
            print("It's true!");
        } else {
            "This should not print" + true;
            }
            
        if (false) {
            "This should not print" + true;
        } else {
            print("It's false!"); 
        }
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        disassemble_chunk(
            &artifact.source_map,
            &artifact.chunk,
            &artifact.heap,
            "script.ql",
        );
        let mut vm = Vm::new(artifact);
        match vm.interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
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
        print(result1);
        
        var result2 = false ? "wrong" : "correct";
        print(result2);
        
        var result3 = nil ? "wrong" : "correct";
        print(result3);

        var result4 = true ? "should do this" : false ? "don't do this" : "nope";
        print(result4);

        var result5 = false ? "should not do this" : true ? "do this" : "nope";
        print(result5);

        var result6 = false ? "should not do this" : false ? "don't do this" : "yup";
        print(result6);
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        disassemble_chunk(
            &artifact.source_map,
            &artifact.chunk,
            &artifact.heap,
            "script.ql",
        );
        let mut vm = Vm::new(artifact);
        match vm.interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
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
        print(result1);
        
        var result2 = false or true;
        print(result2);
        
        var result3 = false or false;
        print(result3);
        
        var result4 = true and false;
        print(result4);
        
        var result5 = true and true;
        print(result5);
        
        var result6 = false and true;
        print(result6);
        
        var result7 = "hello" or nil;
        print(result7);
        
        var result8 = nil and "world";
        print(result8);
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        disassemble_chunk(
            &artifact.source_map,
            &artifact.chunk,
            &artifact.heap,
            "script.ql",
        );
        let mut vm = Vm::new(artifact);
        match vm.interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
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
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        match Vm::new(artifact).interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
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
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        disassemble_chunk(
            &artifact.source_map,
            &artifact.chunk,
            &artifact.heap,
            "script.ql",
        );
        match Vm::new(artifact).interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn comparison_operations_test() {
    let source = r#"
        print(10 >= 9);     // true
        print(10 >= 10);    // true
        print(10 >= 11);    // false
        print(10 > 9);      // true
        print(9 > 9);       // false
        print(10 > 11);     // false
        print(9 <= 10);     // true
        print(10 <= 10);    // true
        print(11 <= 10);    // false
        print(9 < 10);      // true
        print(9 < 9);       // false
        print(11 < 10);     // false
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        disassemble_chunk(
            &artifact.source_map,
            &artifact.chunk,
            &artifact.heap,
            "script.ql",
        );
        match Vm::new(artifact).set_debug(false).interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
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
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        match Vm::new(artifact).interpret() {
            Ok(_) => {
                panic!("Expected runtime error for negating a string")
            }
            Err(error) => {
                let error_message = pretty_print_error(&source_map, &error);
                assert!(error_message.contains("Operand must be a number"));
                assert!(error_message.contains("hello"));
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
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        match Vm::new(artifact).interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_type_error_with_source_span_for_left_operand() {
    let source = r#"
  1 + "hello";
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        match Vm::new(artifact).interpret() {
            Ok(_) => {
                panic!("Expected runtime error for adding number and string")
            }
            Err(error) => {
                let error_message = pretty_print_error(&source_map, &error);
                assert!(error_message.contains("Cannot add number to string."));
                println!("Error correctly includes source span: {}", error_message);
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_type_error_with_source_span_for_right_operand() {
    let source = r#"
  "hello" + true;
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        match Vm::new(artifact).interpret() {
            Ok(_) => {
                panic!("Expected runtime error for adding string and boolean")
            }
            Err(error) => {
                let error_message = pretty_print_error(&source_map, &error);
                assert!(error_message.contains("Cannot add string to boolean."));
                println!("Error correctly includes source span: {}", error_message);
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
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        match Vm::new(artifact).interpret() {
            Ok(_) => {
                panic!("Expected runtime error for adding number and string")
            }
            Err(error) => {
                let error_message = pretty_print_error(&source_map, &error);
                assert!(error_message.contains("Cannot add number to string"));
                assert!(error_message.contains("hello"));
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
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        match Vm::new(artifact).interpret() {
            Ok(_) => {
                panic!("Expected runtime error for adding boolean and number")
            }
            Err(error) => {
                let error_message = pretty_print_error(&source_map, &error);
                assert!(error_message.contains("Cannot add boolean to number"));
                assert!(error_message.contains("true"));
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
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    match CompilerPipeline::new((*source_map).clone()).run() {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = pretty_print_error(&source_map, &errors.all()[0]);
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
            print(i);
            sum = sum + i;
        }
        print(sum);
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    match CompilerPipeline::new((*source_map).clone()).run() {
        Ok(artifact) => {
            disassemble_chunk(
                &artifact.source_map,
                &artifact.chunk,
                &artifact.heap,
                "for_loop_test.ql",
            );
            match Vm::new(artifact).set_debug(false).interpret() {
                Ok(_) => (),
                Err(error) => {
                    panic!("{}", pretty_print_error(&source_map, &error))
                }
            }
        }
        Err(errors) => {
            for error in errors.all() {
                println!("{}", pretty_print_error(&source_map, error));
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
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    match CompilerPipeline::new((*source_map).clone()).run() {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = pretty_print_error(&source_map, &errors.all()[0]);
            assert!(error_message.contains("Already a variable with this name in this scope."));
        }
    }
}
