use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    AnalysisPipeline, AnalysisPipelineConfig, CompilerConfig, ErrorReporter, HeapAllocator, NodeId,
    Parser, QangCompilerError, QangPipelineError, QangProgram, SourceMap, TypedNodeArena,
    backend::compiler::Assembler,
    frontend::{
        module_map::ModuleMap,
        node_visitor::{NodeVisitor, VisitorContext},
    },
    nodes::SourceSpan,
};

#[derive(Debug, Default, Clone)]
pub struct GlobalCompilerPipeline {
    pub modules: ModuleMap,
    pub nodes: TypedNodeArena,
    imported_files: HashSet<PathBuf>,
    processed_files: HashSet<PathBuf>,
    config: CompilerConfig,
}

impl GlobalCompilerPipeline {
    pub fn new() -> Self {
        Self {
            modules: ModuleMap::new(),
            nodes: TypedNodeArena::new(),
            imported_files: HashSet::new(),
            processed_files: HashSet::new(),
            config: CompilerConfig::default(),
        }
    }

    pub fn with_config(mut self, config: CompilerConfig) -> Self {
        self.config = config;
        self
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
            // Try to canonicalize the path, but fall back to the original if it fails
            let canonical_path = file_path
                .canonicalize()
                .unwrap_or_else(|_| file_path.clone());

            // If this file wasn't imported by any other file, it's a main module
            if !self.imported_files.contains(&canonical_path) {
                self.modules.main_modules.push(canonical_path);
            }
        }

        Ok(())
    }

    fn parse_file_recursive(
        &mut self,
        file_path: &Path,
        allocator: &mut HeapAllocator,
    ) -> Result<NodeId, QangPipelineError> {
        // Try to canonicalize the path, but fall back to the original if it fails
        // (this supports in-memory test scenarios where files don't exist on disk)
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

            self.modules.add_dependency(module_path, &import_path);

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

    pub fn get_main_modules(&self) -> &[PathBuf] {
        &self.modules.main_modules
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

    /// Run analysis using the contained AnalysisPipeline
    fn analyze_all_modules(
        &mut self,
        allocator: &mut HeapAllocator,
    ) -> Result<(), QangPipelineError> {
        let analysis_config = AnalysisPipelineConfig {
            error_message_format: self.config.error_message_format,
            strict_mode: true,
        };

        // Use the new ModuleMap directly
        let analyzer = AnalysisPipeline::new(&mut allocator.strings).with_config(analysis_config);
        let mut errors = ErrorReporter::new();
        analyzer.analyze(&self.modules, &mut self.nodes, &mut errors)?;

        Ok(())
    }

    /// Compile a specific module to executable form
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
            )])
        })?;

        // Assemble the main module
        let assembler = Assembler::new(module_source.source_map.clone(), alloc);
        let mut errors = ErrorReporter::new();
        let mut main_function =
            assembler.assemble(module_source.node, &mut self.nodes, &mut errors)?;

        let main_module_id = alloc
            .strings
            .intern(&module_source.source_map.get_path().to_string_lossy());
        main_function.name = main_module_id;

        // Build ModuleResolver with all compiled modules
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

    /// Extract import paths from a parsed module using NodeVisitor for complete traversal
    fn extract_import_paths(
        module_node: NodeId,
        module_path: &Path,
        nodes: &mut TypedNodeArena,
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
                import_decl: crate::frontend::typed_node_arena::TypedNodeRef<
                    crate::nodes::ImportModuleDeclNode,
                >,
                _ctx: &mut VisitorContext,
            ) -> Result<(), Self::Error> {
                // Found an import declaration - extract the path
                let import_path_str = self.allocator.strings.get_string(import_decl.node.path);

                // Resolve relative to the current module's directory
                let resolved_path = if let Some(parent_dir) = self.module_path.parent() {
                    parent_dir.join(import_path_str)
                } else {
                    PathBuf::from(import_path_str)
                };

                // Try to canonicalize the path, but use the resolved path if it fails
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

    /// Compile source code directly from a string (for in-memory compilation)
    /// This is the primary method for tests and REPL usage
    pub fn compile_source(
        source: String,
        allocator: &mut HeapAllocator,
    ) -> Result<QangProgram, QangPipelineError> {
        let mut pipeline = Self::new();

        // Create an in-memory source map
        let source_map = Arc::new(SourceMap::from_source(source));

        // Parse the source using the internal parser approach
        let mut parser = Parser::new(
            source_map.clone(),
            &mut pipeline.nodes,
            &mut allocator.strings,
        );
        let modules = parser.parse();
        let mut errors = parser.into_errors();

        // Check for parse errors
        if errors.has_errors() {
            return Err(QangPipelineError::new(errors.take_errors()));
        }

        // Parser now returns ModuleMap directly
        pipeline.modules = modules;

        // Run semantic analysis
        pipeline.analyze_all_modules(allocator)?;

        // Get the main module path before borrowing pipeline mutably
        let main_module_path = {
            let main_modules = pipeline.get_main_modules();
            if main_modules.is_empty() {
                return Err(QangPipelineError::new(vec![
                    QangCompilerError::new_analysis_error(
                        "No main module found during compilation".to_string(),
                        SourceSpan::default(),
                    ),
                ]));
            }
            main_modules[0].clone()
        };

        // Compile the main module
        pipeline.compile_module(&main_module_path, allocator)
    }
}
