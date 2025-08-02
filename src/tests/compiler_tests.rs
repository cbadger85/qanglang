use crate::{Compiler, SourceMap, debug::disassemble_chunk};

#[test]
fn test_display() {
    let source = r#"
  1 * 2 / 3 + 4 - 5 + -1 + "foo!" + "foo!";
  "#;
    let source_map = SourceMap::new(source.to_string());

    if let Ok(artifact) = Compiler::new(&source_map).compile() {
        disassemble_chunk(&artifact.heap, &artifact.chunk, &source_map, "script.ql");
    } else {
        panic!("Compiler errors.")
    }
}
