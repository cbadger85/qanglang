use crate::{Compiler, SourceMap, Vm, debug::disassemble_chunk};

#[test]
fn test_display() {
    let source = r#"
  1 * 2 / 3 + 4 - 5 + -1;
  "#;
    let source_map = SourceMap::new(source.to_string());

    if let Ok(artifact) = Compiler::new(&source_map).compile() {
        disassemble_chunk(&artifact.heap, &artifact.chunk, &source_map, "script.ql");
    } else {
        panic!("Compiler errors.")
    }
}

#[test]
fn test_run() {
    let source = r#"
  "hello world!";
  "#;
    let source_map = SourceMap::new(source.to_string());

    if let Ok(artifact) = Compiler::new(&source_map).compile() {
        if let Ok(value) = Vm::new(&source_map, artifact.chunk, artifact.heap.clone()).interpret() {
            value.print(&artifact.heap);
        } else {
            panic!("Runtime errors.")
        }
    } else {
        panic!("Compiler errors.")
    }
}
