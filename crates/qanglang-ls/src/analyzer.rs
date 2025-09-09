use qanglang_core::{
    AnalysisPipeline, Parser, QangPipelineError, SourceMap, StringInterner, TypedNodeArena,
};

pub struct Analyzer<'a> {
    source_map: &'a SourceMap,
}

impl<'a> Analyzer<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Analyzer { source_map }
    }

    pub fn analyze(&self) -> Result<(), QangPipelineError> {
        let mut strings = StringInterner::new();
        let mut nodes = TypedNodeArena::new();
        let mut parser = Parser::new(self.source_map, &mut nodes, &mut strings);
        let program = parser.parse();

        let (mut errors, _modules) = parser.into_parts();

        let _ = AnalysisPipeline::new(&mut strings).analyze(
            program,
            self.source_map,
            &mut nodes,
            &mut errors,
        )?;

        Ok(())
    }
}
