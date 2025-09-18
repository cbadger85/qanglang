use std::sync::Arc;

use qanglang_core::{
    AnalysisPipeline, LegacyModuleMap, ModuleMap, Parser, ParserConfig, QangPipelineError, SourceMap, StringInterner,
    TypedNodeArena,
};

pub fn analyze(source_map: Arc<SourceMap>) -> Result<(), QangPipelineError> {
    let mut strings = StringInterner::new();
    let mut nodes = TypedNodeArena::new();
    let mut parser = Parser::new(source_map, &mut nodes, &mut strings)
        .with_config(ParserConfig { skip_modules: true });
    let modules = parser.parse();

    let mut errors = parser.into_errors();

    // Convert LegacyModuleMap to new ModuleMap for analysis
    let new_modules = convert_legacy_to_new_module_map(modules);
    let _ = AnalysisPipeline::new(&mut strings).analyze(&new_modules, &mut nodes, &mut errors)?;

    Ok(())
}

/// Temporary helper to convert LegacyModuleMap to new ModuleMap
fn convert_legacy_to_new_module_map(legacy: LegacyModuleMap) -> ModuleMap {
    let mut new_map = ModuleMap::new();

    // Add main module first
    let main_module = legacy.get_main();
    let main_path = main_module.source_map.get_path();
    new_map.insert(main_path, main_module.node, main_module.source_map.clone());
    new_map.add_main_module(main_path.to_path_buf());

    // Add other modules
    for (path, module) in legacy.iter() {
        new_map.insert(path, module.node, module.source_map.clone());
    }

    new_map
}
