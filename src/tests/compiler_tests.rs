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
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        match Vm::new(artifact).interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(source_map.clone(), &error))
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
                panic!("{}", pretty_print_error(source_map.clone(), &error))
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
                panic!("{}", pretty_print_error(source_map.clone(), &error))
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn comparison_operations_test() {
    let source = r#"
        10 >= 9;
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = CompilerPipeline::new((*source_map).clone()).run() {
        disassemble_chunk(
            &artifact.source_map,
            &artifact.chunk,
            &artifact.heap,
            "script.ql",
        );
        match Vm::new(artifact).set_debug(true).interpret() {
            Ok(_) => (),
            Err(error) => {
                panic!("{}", pretty_print_error(source_map.clone(), &error))
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
                let error_message = pretty_print_error(source_map.clone(), &error);
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
                panic!("{}", pretty_print_error(source_map.clone(), &error))
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
                let error_message = pretty_print_error(source_map.clone(), &error);
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
                let error_message = pretty_print_error(source_map.clone(), &error);
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
                let error_message = pretty_print_error(source_map.clone(), &error);
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
                let error_message = pretty_print_error(source_map.clone(), &error);
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
