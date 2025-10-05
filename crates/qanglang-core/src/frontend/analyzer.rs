use crate::{
    ErrorReporter, QangPipelineError, AstNodeArena,
    frontend::{module_map::ModuleMap, semantic_validator::SemanticValidator},
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
        modules: &ModuleMap,
        nodes: &mut AstNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), QangPipelineError> {
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
