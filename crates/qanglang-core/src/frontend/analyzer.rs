use crate::{
    ErrorReporter, NodeId, TypedNodeArena,
    frontend::scope_analysis::{ScopeAnalysis, ScopeAnalyzer},
    memory::StringInterner,
};

#[derive(Debug, Clone)]
pub struct AnalysisResults {
    pub scope: ScopeAnalysis,
}

impl AnalysisResults {
    pub fn new(scope: ScopeAnalysis) -> Self {
        Self { scope }
    }
}

pub struct AnalysisPipeline<'a> {
    strings: &'a mut StringInterner,
}

impl<'a> AnalysisPipeline<'a> {
    pub fn new(strings: &'a mut StringInterner) -> Self {
        Self { strings }
    }

    pub fn analyze(
        self,
        program: NodeId,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> AnalysisResults {
        let scope_analyzer = ScopeAnalyzer::new(self.strings);
        let scope_analysis = scope_analyzer.analyze(program, nodes, errors);

        AnalysisResults::new(scope_analysis)
    }
}
