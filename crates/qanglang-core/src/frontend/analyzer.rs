use crate::{
    ErrorMessageFormat, ErrorReporter, NodeId, QangPipelineError, SourceMap, TypedNodeArena,
    frontend::scope_analysis::{ScopeAnalysis, ScopeAnalyzer},
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

#[derive(Debug, Clone)]
pub struct AnalysisPipelineConfig {
    error_message_format: ErrorMessageFormat,
    strict_mode: bool,
}

impl Default for AnalysisPipelineConfig {
    fn default() -> Self {
        Self {
            error_message_format: ErrorMessageFormat::Minimal,
            strict_mode: false,
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
        program: NodeId,
        source_map: &SourceMap,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<AnalysisResults, QangPipelineError> {
        let scope_analyzer = ScopeAnalyzer::new(self.strings);
        let scope_analysis = scope_analyzer.analyze(program, nodes, errors);

        let result = AnalysisResults::new(scope_analysis);

        if self.config.strict_mode && errors.has_errors() {
            let errors = errors.take_errors();

            let formatted_errors = errors
                .into_iter()
                .map(|error| match self.config.error_message_format {
                    ErrorMessageFormat::Minimal => error,
                    ErrorMessageFormat::Compact => error.into_short_formatted(source_map),
                    ErrorMessageFormat::Verbose => error.into_formatted(source_map),
                })
                .collect();

            return Err(QangPipelineError::new(formatted_errors));
        }

        Ok(result)
    }
}
