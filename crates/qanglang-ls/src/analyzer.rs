use qanglang_core::{CompilerError, CompilerVisitor, HeapAllocator, Parser, SourceMap};

pub struct Analyzer<'a> {
    source_map: &'a SourceMap,
}

impl<'a> Analyzer<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Analyzer { source_map }
    }

    pub fn analyze(&self) -> Result<(), CompilerError> {
        let mut allocator = HeapAllocator::new();

        let mut parser = Parser::new(self.source_map);
        let program = parser.parse();
        let errors = parser.into_reporter();

        let _ = CompilerVisitor::new(&mut allocator).compile(program, self.source_map, errors)?;

        Ok(())
    }
}
