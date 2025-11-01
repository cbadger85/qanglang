use std::sync::Arc;

use qanglang_core::{
    AnalysisPipeline, AnalysisPipelineConfig, AstNodeArena, NodeId, Parser, ParserConfig,
    QangPipelineError, SourceMap, StringInterner,
    symbol_resolver::{SymbolResolver, SymbolTable},
};

/// Holds the results of analysis for use in LSP features.
/// Even with errors present, partial analysis results can still be used
/// for semantic highlighting, autocomplete, and other LSP features.
#[derive(Debug)]
pub struct AnalysisResult {
    pub nodes: AstNodeArena,
    pub strings: StringInterner,
    pub root_module_id: NodeId,
    pub source_map: Arc<SourceMap>,
    pub symbol_table: SymbolTable,
    pub errors: Vec<qanglang_core::QangCompilerError>,
}

pub fn analyze(source_map: Arc<SourceMap>) -> Result<AnalysisResult, QangPipelineError> {
    let mut strings = StringInterner::new();
    let mut nodes = AstNodeArena::new();
    let mut parser = Parser::new(source_map.clone(), &mut nodes, &mut strings)
        .with_config(ParserConfig { skip_modules: true });
    let mut modules = parser.parse();

    let mut errors = parser.into_errors();

    // Use non-strict mode to allow partial analysis even with errors
    // This enables LSP features like semantic highlighting and autocomplete
    // to continue working when there are syntax or semantic errors
    let analysis_config = AnalysisPipelineConfig { strict_mode: false };
    AnalysisPipeline::new(&mut strings)
        .with_config(analysis_config)
        .analyze(&mut modules, &mut nodes, &mut errors)?;

    // Get the root module ID - get main module from ModuleMap
    let root_module_id = modules
        .get_main()
        .map(|m| m.node)
        .ok_or_else(|| QangPipelineError::new(vec![]))?;

    // Run symbol resolution to map identifier references to declarations
    let module_node = nodes.get_program_node(root_module_id);
    let resolver = SymbolResolver::new(&nodes, &strings);
    let symbol_table = resolver.resolve_module(module_node);

    // Collect all errors (both parse and semantic analysis errors)
    let collected_errors = errors.take_errors();

    Ok(AnalysisResult {
        nodes,
        strings,
        root_module_id,
        source_map,
        symbol_table,
        errors: collected_errors,
    })
}
