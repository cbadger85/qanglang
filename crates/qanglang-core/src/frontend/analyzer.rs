use crate::{
    ErrorReporter, QangPipelineError, TypedNodeArena,
    frontend::{
        module_map::ModuleMap,
        semantic_validator::SemanticValidator,
        type_resolver::{TypeEnvironment, TypeResolver},
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
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), QangPipelineError> {
        for (_, module) in modules.iter() {
            SemanticValidator::new(self.strings, module.source_map.clone()).analyze(
                module.node,
                nodes,
                errors,
            );
        }

        if !self.config.strict_mode && errors.has_errors() {
            let errors = errors.take_errors();

            return Err(QangPipelineError::new(errors));
        }

        let gobal_types = TypeEnvironment::with_globals(nodes);

        let module_paths_to_resolve = modules
            .iter()
            .map(|(path, _)| path.to_path_buf())
            .collect::<Vec<_>>();

        for path in module_paths_to_resolve {
            let source_map = {
                let module = modules
                    .get(path.as_path())
                    .expect(format!("Expect module {} to be parsed.", path.display()).as_str());

                if module.is_resolved() {
                    continue;
                }

                module.source_map.clone()
            };
            TypeResolver::new(self.strings, source_map, path, modules, gobal_types.clone())
                .resolve(nodes, errors);
        }

        if errors.has_errors() {
            let errors = errors.take_errors();

            return Err(QangPipelineError::new(errors));
        }

        Ok(())
    }
}
