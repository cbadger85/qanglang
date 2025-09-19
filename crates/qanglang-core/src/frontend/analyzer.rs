use crate::{
    ErrorMessageFormat, ErrorReporter, QangPipelineError, TypedNodeArena,
    frontend::{module_map::ModuleMap, semantic_validator::SemanticValidator},
    memory::StringInterner,
};

#[derive(Debug, Clone, Copy)]
pub struct AnalysisPipelineConfig {
    pub error_message_format: ErrorMessageFormat,
    pub strict_mode: bool,
}

impl Default for AnalysisPipelineConfig {
    fn default() -> Self {
        Self {
            error_message_format: ErrorMessageFormat::Minimal,
            strict_mode: true,
        }
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
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), QangPipelineError> {
        for main_path in modules.get_main_modules() {
            if let Some(main_module) = modules.get(main_path) {
                SemanticValidator::new(self.strings).analyze(main_module.node, nodes, errors);
            }
        }

        for (path, module) in modules.iter() {
            if !modules.is_main_module(path) {
                SemanticValidator::new(self.strings).analyze(module.node, nodes, errors);
            }
        }

        if self.config.strict_mode && errors.has_errors() {
            let errors = errors.take_errors();

            // TODO this isn't going to work, the error reporter is collecting errors across modules.
            // Use the first main module's source map for formatting
            let main_source_map = modules
                .get_main_modules()
                .first()
                .and_then(|path| modules.get(path))
                .map(|module| &module.source_map);

            let formatted_errors = errors
                .into_iter()
                .map(|error| match self.config.error_message_format {
                    ErrorMessageFormat::Minimal => error,
                    ErrorMessageFormat::Compact => {
                        if let Some(source_map) = main_source_map {
                            error.into_short_formatted(source_map)
                        } else {
                            error
                        }
                    }
                    ErrorMessageFormat::Verbose => {
                        if let Some(source_map) = main_source_map {
                            error.into_formatted(source_map)
                        } else {
                            error
                        }
                    }
                })
                .collect();

            return Err(QangPipelineError::new(formatted_errors));
        }

        Ok(())
    }
}
