use qanglang_core::{CompilerError, CompilerVisitor, ObjectHeap, Parser, SourceMap};

pub struct Analyzer<'a> {
    source_map: &'a SourceMap,
}

impl<'a> Analyzer<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Analyzer { source_map }
    }

    pub fn analyze(&self) -> Result<(), CompilerError> {
        let mut heap = ObjectHeap::new();

        let mut parser = Parser::new(self.source_map);
        let program = parser.parse();
        let errors = parser.into_reporter();

        let _ = CompilerVisitor::new(&mut heap).compile(program, self.source_map, errors)?;

        Ok(())
    }
}
