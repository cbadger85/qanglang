use std::sync::Arc;

use qanglang_core::{
    AnalysisPipeline, AstNodeArena, NodeId, Parser, ParserConfig, QangPipelineError, SourceMap,
    StringInterner,
};

/// Holds the results of successful analysis for use in LSP features
#[derive(Debug)]
pub struct AnalysisResult {
    pub nodes: AstNodeArena,
    pub strings: StringInterner,
    pub root_module_id: NodeId,
    pub source_map: Arc<SourceMap>,
}

pub fn analyze(source_map: Arc<SourceMap>) -> Result<AnalysisResult, QangPipelineError> {
    let mut strings = StringInterner::new();
    let mut nodes = AstNodeArena::new();
    let mut parser = Parser::new(source_map.clone(), &mut nodes, &mut strings)
        .with_config(ParserConfig { skip_modules: true });
    let modules = parser.parse();

    let mut errors = parser.into_errors();

    AnalysisPipeline::new(&mut strings).analyze(&modules, &mut nodes, &mut errors)?;

    // Get the root module ID - get main module from ModuleMap
    let root_module_id = modules.get_main()
        .map(|m| m.node)
        .ok_or_else(|| {
            QangPipelineError::new(vec![])
        })?;

    Ok(AnalysisResult {
        nodes,
        strings,
        root_module_id,
        source_map,
    })
}
