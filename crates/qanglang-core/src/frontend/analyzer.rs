use crate::{
    ErrorMessageFormat, ErrorReporter, QangPipelineError, TypedNodeArena,
    frontend::{
        scope_analysis::{ScopeAnalysis, ScopeAnalyzer},
        source::ModuleMap,
    },
    memory::StringInterner,
};

#[derive(Debug, Clone)]
pub struct AnalysisResults {
    pub scopes: ScopeAnalysis,
}

impl AnalysisResults {
    pub fn new(scopes: ScopeAnalysis) -> Self {
        Self { scopes }
    }
}

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
    ) -> Result<AnalysisResults, QangPipelineError> {
        let scope_analyzer = ScopeAnalyzer::new(self.strings);
        let mut scope_analysis = scope_analyzer.analyze(modules.get_main().node, nodes, errors);

        for (_, module) in modules.iter() {
            let scope_analyzer = ScopeAnalyzer::new(self.strings);
            scope_analysis =
                scope_analysis.merge_with(scope_analyzer.analyze(module.node, nodes, errors));
        }

        let result = AnalysisResults::new(scope_analysis);

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

        Ok(result)
    }
}
