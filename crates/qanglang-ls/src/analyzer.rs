use qanglang_core::{Assembler, CompilerError, HeapAllocator, Parser, SourceMap, TypedNodeArena};

pub struct Analyzer<'a> {
    source_map: &'a SourceMap,
}

impl<'a> Analyzer<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Analyzer { source_map }
    }

    pub fn analyze(&self) -> Result<(), CompilerError> {
        let mut allocator = HeapAllocator::new();
        let nodes = TypedNodeArena::new();

        let mut parser = Parser::new(self.source_map, nodes, &mut allocator.strings);
        let program = parser.parse();
        let (errors, mut nodes) = parser.into_parts();

        let _ =
            Assembler::new(&mut allocator).compile(program, &mut nodes, self.source_map, errors)?;

        Ok(())
    }
}
