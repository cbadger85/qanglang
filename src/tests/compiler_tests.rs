use crate::{Compiler, SourceMap, Vm, debug::disassemble_chunk, error::pretty_print_error};

#[test]
fn test_display() {
    let source = r#"
  1 * 2 / 3 + 4 - 5 + -1;
  "#;
    let source_map = SourceMap::new(source.to_string());

    if let Ok(artifact) = Compiler::new(&source_map).compile() {
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
    let source_map = SourceMap::new(source.to_string());

    if let Ok(artifact) = Compiler::new(&source_map).compile() {
        match Vm::new(&source_map).interpret(artifact) {
            Ok((value, updated_artifact)) => {
                value.print(&updated_artifact.heap);
            }
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn math_operations() {
    let source = r#"
  -4 + 2 + 3;
  "#;
    let source_map = SourceMap::new(source.to_string());

    if let Ok(artifact) = Compiler::new(&source_map).compile() {
        match Vm::new(&source_map).interpret(artifact) {
            Ok((value, updated_artifact)) => {
                value.print(&updated_artifact.heap);
            }
            Err(error) => {
                panic!("{}", pretty_print_error(&source_map, &error))
            }
        }
    } else {
        panic!("Compiler errors.")
    }
}
