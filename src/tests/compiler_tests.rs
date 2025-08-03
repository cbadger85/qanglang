use crate::{Compiler, SourceMap, Vm, debug::disassemble_chunk, error::pretty_print_error};
use std::rc::Rc;

#[test]
fn test_display() {
    let source = r#"
  1 * 2 / 3 + 4 - 5 + -1;
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = Compiler::new((*source_map).clone()).compile() {
        disassemble_chunk(&artifact, &source_map, "script.ql");
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_run() {
    let source = r#"
  "hello" + " " + "world!";
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = Compiler::new((*source_map).clone()).compile() {
        match Vm::new().interpret(artifact) {
            Ok((value, updated_artifact)) => {
                value.print(&updated_artifact.heap);
            }
            Err(error) => {
                panic!("{}", pretty_print_error(source_map.clone(), &error))
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn math_operations() {
    let source = r#"
  1 / 1 + 2 * (12 % 5);
  "#;
    let source_map = Rc::new(SourceMap::new(source.to_string()));

    if let Ok(artifact) = Compiler::new((*source_map).clone()).compile() {
        match Vm::new().interpret(artifact) {
            Ok((value, updated_artifact)) => {
                value.print(&updated_artifact.heap);
            }
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

    if let Ok(artifact) = Compiler::new((*source_map).clone()).compile() {
        match Vm::new().interpret(artifact) {
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

    if let Ok(artifact) = Compiler::new((*source_map).clone()).compile() {
        match Vm::new().interpret(artifact) {
            Ok((value, updated_artifact)) => {
                value.print(&updated_artifact.heap);
            }
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

    if let Ok(artifact) = Compiler::new((*source_map).clone()).compile() {
        match Vm::new().interpret(artifact) {
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

    if let Ok(artifact) = Compiler::new((*source_map).clone()).compile() {
        match Vm::new().interpret(artifact) {
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

    if let Ok(artifact) = Compiler::new((*source_map).clone()).compile() {
        match Vm::new().interpret(artifact) {
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

    if let Ok(artifact) = Compiler::new((*source_map).clone()).compile() {
        match Vm::new().interpret(artifact) {
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
