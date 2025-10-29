use crate::{
    AstNodeArena, ErrorReporter, QangPipelineError,
    frontend::{
        module_map::ModuleMap, semantic_validator::SemanticValidator,
        symbol_resolver::SymbolResolver,
    },
    memory::StringInterner,
};

#[derive(Debug, Clone, Copy)]
pub struct AnalysisPipelineConfig {
    pub strict_mode: bool,
}

impl Default for AnalysisPipelineConfig {
    fn default() -> Self {
        Self { strict_mode: true }
    }
}

pub struct AnalysisPipeline<'a> {
    strings: &'a mut StringInterner,
    config: AnalysisPipelineConfig,
}

impl<'a> AnalysisPipeline<'a> {
    pub fn new(strings: &'a mut StringInterner) -> Self {
        Self {
            strings,
            config: AnalysisPipelineConfig::default(),
        }
    }

    pub fn with_config(mut self, config: AnalysisPipelineConfig) -> Self {
        self.config = config;
        self
    }

    pub fn analyze(
        self,
        modules: &mut ModuleMap,
        nodes: &mut AstNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), QangPipelineError> {
        let mut symbol_tables = Vec::new();
        for (path, module) in modules.iter() {
            let module_node = nodes.get_program_node(module.node);
            let resolver = SymbolResolver::new(nodes, self.strings);
            let symbol_table = resolver.resolve_module(module_node);
            symbol_tables.push((path.to_path_buf(), symbol_table));
        }

        for (path, symbol_table) in symbol_tables {
            if let Some(module) = modules.get(&path) {
                let node = module.node;
                let source_map = module.source_map.clone();
                modules.insert_with_symbols(&path, node, source_map, symbol_table);
            }
        }

        for (_, module) in modules.iter() {
            SemanticValidator::new(self.strings, module.source_map.clone()).analyze(
                module.node,
                nodes,
                errors,
            );
        }

        if self.config.strict_mode && errors.has_errors() {
            let errors = errors.take_errors();

            return Err(QangPipelineError::new(errors));
        }

        Ok(())
    }
}
