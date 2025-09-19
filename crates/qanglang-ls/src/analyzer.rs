use std::sync::Arc;

use qanglang_core::{
    AnalysisPipeline, Parser, ParserConfig, QangPipelineError, SourceMap, StringInterner,
    TypedNodeArena,
};

pub fn analyze(source_map: Arc<SourceMap>) -> Result<(), QangPipelineError> {
    let mut strings = StringInterner::new();
    let mut nodes = TypedNodeArena::new();
    let mut parser = Parser::new(source_map, &mut nodes, &mut strings)
        .with_config(ParserConfig { skip_modules: true });
    let modules = parser.parse();

    let mut errors = parser.into_errors();

    // Parser now returns ModuleMap directly
    AnalysisPipeline::new(&mut strings).analyze(&modules, &mut nodes, &mut errors)?;

    Ok(())
}
