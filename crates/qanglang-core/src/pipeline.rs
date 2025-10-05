use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    AnalysisPipeline, AnalysisPipelineConfig, AstNodeArena, ErrorReporter, HeapAllocator, NodeId,
    Parser, QangCompilerError, QangPipelineError, QangProgram, SourceMap,
    backend::assembler::Assembler,
    frontend::{
        module_map::ModuleMap,
        node_visitor::{NodeVisitor, VisitorContext},
    },
    nodes::SourceSpan,
};

#[derive(Debug, Default, Clone)]
pub struct GlobalCompilerPipeline {
    pub modules: ModuleMap,
    pub nodes: AstNodeArena,
    imported_files: HashSet<PathBuf>,
    processed_files: HashSet<PathBuf>,
}

impl GlobalCompilerPipeline {
    pub fn new() -> Self {
        Self {
            modules: ModuleMap::new(),
            nodes: AstNodeArena::new(),
            imported_files: HashSet::new(),
            processed_files: HashSet::new(),
        }
    }

    pub fn parse_from_root(
        &mut self,
        file_path: PathBuf,
        allocator: &mut HeapAllocator,
    ) -> Result<(), QangPipelineError> {
        let canonical_path = file_path
            .canonicalize()
            .unwrap_or_else(|_| file_path.to_path_buf());

        self.processed_files.insert(canonical_path.clone());

        let source_map = match SourceMap::from_path(&canonical_path) {
            Ok(source_map) => Arc::new(source_map),
            Err(_) => {
                return Err(QangPipelineError::new(vec![
                    QangCompilerError::new_analysis_error(
                        format!(
                            "Unable to load module from path '{}'",
                            canonical_path.display()
                        ),
                        SourceSpan::default(),
                        Arc::new(SourceMap::default()),
                    ),
                ]));
            }
        };
        let mut parser = Parser::new(source_map.clone(), &mut self.nodes, &mut allocator.strings);

        self.modules = parser.parse();
        let mut errors = parser.into_errors();

        if errors.has_errors() {
            return Err(QangPipelineError::new(errors.take_errors()));
        }

        Ok(())
    }

    pub fn parse_files(
        &mut self,
        file_paths: Vec<PathBuf>,
        allocator: &mut HeapAllocator,
    ) -> Result<(), QangPipelineError> {
        // First pass: parse all files and track imports
        for file_path in &file_paths {
            self.parse_file_recursive(file_path, allocator)?;
        }

        // Second pass: determine main modules (files that aren't imported)
        for file_path in file_paths {
            let canonical_path = file_path
                .canonicalize()
                .unwrap_or_else(|_| file_path.clone());

            if !self.imported_files.contains(&canonical_path) {
                self.modules.set_main_module(canonical_path);
            }
        }

        Ok(())
    }

    fn parse_file_recursive(
        &mut self,
        file_path: &Path,
        allocator: &mut HeapAllocator,
    ) -> Result<NodeId, QangPipelineError> {
        let canonical_path = file_path
            .canonicalize()
            .unwrap_or_else(|_| file_path.to_path_buf());

        self.processed_files.insert(canonical_path.clone());

        if self.modules.has(&canonical_path) {
            return Ok(self.modules.get(&canonical_path).unwrap().node);
        }

        let source_map = match SourceMap::from_path(&canonical_path) {
            Ok(source_map) => Arc::new(source_map),
            Err(_) => {
                return Err(QangPipelineError::new(vec![
                    QangCompilerError::new_analysis_error(
                        format!(
                            "Unable to load module from path '{}'",
                            canonical_path.display()
                        ),
                        SourceSpan::default(),
                        Arc::new(SourceMap::default()),
                    ),
                ]));
            }
        };
        let mut parser = Parser::new(source_map.clone(), &mut self.nodes, &mut allocator.strings);
        let node_id = parser.parse_file();

        let mut errors = parser.into_errors();

        self.modules
            .insert(&canonical_path, node_id, source_map.clone());

        self.parse_module_dependencies(&canonical_path, node_id, allocator, &mut errors)?;

        if errors.has_errors() {
            return Err(QangPipelineError::new(errors.take_errors()));
        }

        Ok(node_id)
    }

    fn parse_module_dependencies(
        &mut self,
        module_path: &Path,
        module_node: NodeId,
        allocator: &mut HeapAllocator,
        errors: &mut ErrorReporter,
    ) -> Result<(), QangPipelineError> {
        let import_paths =
            { Self::extract_import_paths(module_node, module_path, &mut self.nodes, allocator) };

        for import_path in import_paths {
            self.imported_files.insert(import_path.clone());

            match self.parse_file_recursive(&import_path, allocator) {
                Ok(_) => {}
                Err(dep_error) => {
                    for error in dep_error.into_errors() {
                        errors.report_error(error);
                    }
                }
            }
        }

        Ok(())
    }

    pub fn process_files(
        &mut self,
        file_paths: Vec<PathBuf>,
        allocator: &mut HeapAllocator,
    ) -> Result<(), QangPipelineError> {
        self.parse_files(file_paths, allocator)?;

        self.analyze_all_modules(allocator)?;

        Ok(())
    }

    pub fn process_root(
        &mut self,
        file_path: PathBuf,
        allocator: &mut HeapAllocator,
    ) -> Result<(), QangPipelineError> {
        self.parse_from_root(file_path, allocator)?;

        self.analyze_all_modules(allocator)?;

        Ok(())
    }

    fn analyze_all_modules(
        &mut self,
        allocator: &mut HeapAllocator,
    ) -> Result<(), QangPipelineError> {
        let analysis_config = AnalysisPipelineConfig { strict_mode: true };

        let analyzer = AnalysisPipeline::new(&mut allocator.strings).with_config(analysis_config);
        let mut errors = ErrorReporter::new();
        analyzer.analyze(&self.modules, &mut self.nodes, &mut errors)?;

        Ok(())
    }

    pub fn compile_module(
        &mut self,
        module_path: &Path,
        alloc: &mut HeapAllocator,
    ) -> Result<QangProgram, QangPipelineError> {
        use crate::backend::module_resolver::RuntimeModule;
        use rustc_hash::{FxBuildHasher, FxHashMap};

        let module_source = self.modules.get(module_path).ok_or_else(|| {
            QangPipelineError::new(vec![QangCompilerError::new_analysis_error(
                format!("Module not found: {}", module_path.display()),
                SourceSpan::default(),
                Arc::new(SourceMap::default()),
            )])
        })?;

        let assembler = Assembler::new(module_source.source_map.clone(), alloc);
        let mut errors = ErrorReporter::new();
        let mut main_function =
            assembler.assemble(module_source.node, &mut self.nodes, &mut errors)?;

        let main_module_id = alloc
            .strings
            .intern(&module_source.source_map.get_path().to_string_lossy());
        main_function.name = main_module_id;

        let mut module_map = FxHashMap::with_hasher(FxBuildHasher);
        for (path, module) in self.modules.iter() {
            let path_str = path.to_string_lossy();
            let path_handle = alloc.strings.intern(&path_str);
            let assembler = Assembler::new(module.source_map.clone(), alloc);
            let mut compiled_module =
                assembler.assemble(module.node, &mut self.nodes, &mut errors)?;
            compiled_module.name = path_handle;
            let function_handle = alloc.allocate_function(compiled_module);
            module_map.insert(path_handle, RuntimeModule::new(function_handle));
        }

        if errors.has_errors() {
            return Err(QangPipelineError::new(errors.take_errors()));
        }

        let main_handle = alloc.allocate_function(main_function);
        Ok(QangProgram::new(main_handle, module_map.into()))
    }

    fn extract_import_paths(
        module_node: NodeId,
        module_path: &Path,
        nodes: &mut AstNodeArena,
        allocator: &mut HeapAllocator,
    ) -> Vec<PathBuf> {
        struct ModuleImportExtractor<'a> {
            import_paths: Vec<PathBuf>,
            module_path: &'a Path,
            allocator: &'a mut HeapAllocator,
        }

        impl<'a> NodeVisitor for ModuleImportExtractor<'a> {
            type Error = ();

            fn visit_import_module_declaration(
                &mut self,
                import_decl: crate::frontend::ast_node_arena::TypedNodeRef<
                    crate::nodes::ImportModuleDeclNode,
                >,
                _ctx: &mut VisitorContext,
            ) -> Result<(), Self::Error> {
                let import_path_str = self.allocator.strings.get_string(import_decl.node.path);

                let resolved_path = if let Some(parent_dir) = self.module_path.parent() {
                    parent_dir.join(import_path_str)
                } else {
                    PathBuf::from(import_path_str)
                };

                let final_path = resolved_path.canonicalize().unwrap_or(resolved_path);
                self.import_paths.push(final_path);
                Ok(())
            }
        }

        let mut extractor = ModuleImportExtractor {
            import_paths: Vec::new(),
            module_path,
            allocator,
        };

        let mut errors = ErrorReporter::new();
        let mut ctx = VisitorContext::new(nodes, &mut errors);

        let module_ref = ctx.nodes.get_program_node(module_node);
        let _ = extractor.visit_module(module_ref, &mut ctx);

        extractor.import_paths
    }

    pub fn compile_source(
        source: String,
        allocator: &mut HeapAllocator,
    ) -> Result<QangProgram, QangPipelineError> {
        let mut pipeline = Self::new();

        let source_map = Arc::new(SourceMap::from_source(source));

        let mut parser = Parser::new(
            source_map.clone(),
            &mut pipeline.nodes,
            &mut allocator.strings,
        );
        let modules = parser.parse();
        let mut errors = parser.into_errors();

        if errors.has_errors() {
            return Err(QangPipelineError::new(errors.take_errors()));
        }

        pipeline.modules = modules;

        pipeline.analyze_all_modules(allocator)?;

        let main_module_path = match pipeline.modules.get_main_path() {
            Some(path) => path.to_path_buf(),
            None => {
                return Err(QangPipelineError::new(vec![
                    QangCompilerError::new_analysis_error(
                        "No main module found during compilation".to_string(),
                        SourceSpan::default(),
                        Arc::new(SourceMap::default()),
                    ),
                ]));
            }
        };

        pipeline.compile_module(&main_module_path, allocator)
    }
}
