use crate::{
    ErrorMessageFormat, ErrorReporter, QangPipelineError, TypedNodeArena,
    frontend::{
        semantic_validator::SemanticValidator, source::ModuleMap,
        type_inference::TypeInferenceEngine,
    },
    memory::StringInterner,
};

#[derive(Debug, Clone, Copy)]
pub struct AnalysisPipelineConfig {
    pub error_message_format: ErrorMessageFormat,
    pub strict_mode: bool,
    pub enable_type_inference: bool,
    pub skip_modules: bool,
}

impl Default for AnalysisPipelineConfig {
    fn default() -> Self {
        Self {
            error_message_format: ErrorMessageFormat::Minimal,
            strict_mode: true,
            enable_type_inference: true,
            skip_modules: false,
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
        SemanticValidator::new(self.strings).analyze(modules.get_main().node, nodes, errors);

        for (_, module) in modules.iter() {
            SemanticValidator::new(self.strings).analyze(module.node, nodes, errors);
        }

        if self.config.strict_mode && errors.has_errors() {
            let errors = errors.take_errors();

            let formatted_errors = errors
                .into_iter()
                .map(|error| match self.config.error_message_format {
                    ErrorMessageFormat::Minimal => error,
                    ErrorMessageFormat::Compact => {
                        error.into_short_formatted(&modules.get_main().source_map)
                    }
                    ErrorMessageFormat::Verbose => {
                        error.into_formatted(&modules.get_main().source_map)
                    }
                })
                .collect();

            return Err(QangPipelineError::new(formatted_errors));
        }

        if self.config.enable_type_inference {
            let engine = TypeInferenceEngine::new(self.strings).with_native_types(&mut nodes.types);
            if self.config.skip_modules {
                engine.infer_types(modules.get_main().node, nodes, errors);
            } else {
                engine.infer_types_with_modules(modules.get_main().node, modules, nodes, errors);
            }

            if errors.has_errors() {
                let errors = errors.take_errors();

                let formatted_errors = errors
                    .into_iter()
                    .map(|error| match self.config.error_message_format {
                        ErrorMessageFormat::Minimal => error,
                        ErrorMessageFormat::Compact => {
                            error.into_short_formatted(&modules.get_main().source_map)
                        }
                        ErrorMessageFormat::Verbose => {
                            error.into_formatted(&modules.get_main().source_map)
                        }
                    })
                    .collect();

                return Err(QangPipelineError::new(formatted_errors));
            }
        }

        Ok(())
    }
}
