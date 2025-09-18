use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    AnalysisPipeline, AnalysisPipelineConfig, CompilerConfig, ErrorReporter, HeapAllocator, NodeId,
    Parser, QangCompilerError, QangPipelineError, QangProgram, SourceMap, StringInterner,
    TypedNodeArena,
    backend::compiler::Assembler,
    frontend::{module_map::{ModuleMap, ModuleSource, ModuleStatus}, source::LegacyModuleMap},
    nodes::{DeclNode, SourceSpan},
};

pub struct GlobalCompilerPipeline {
    pub modules: ModuleMap,
    pub nodes: TypedNodeArena,
    pub strings: StringInterner,
    imported_files: HashSet<PathBuf>,
    processed_files: HashSet<PathBuf>,
    config: CompilerConfig,
}

impl GlobalCompilerPipeline {
    pub fn new() -> Self {
        Self {
            modules: ModuleMap::new(),
            nodes: TypedNodeArena::new(),
            strings: StringInterner::new(),
            imported_files: HashSet::new(),
            processed_files: HashSet::new(),
            config: CompilerConfig::default(),
        }
    }

    pub fn with_string_interner(mut self, strings: StringInterner) -> Self {
        self.strings = strings;
        self
    }

    /// Parse files and automatically determine which are main modules
    pub fn parse_files_auto_main(
        &mut self,
        file_paths: Vec<PathBuf>,
    ) -> Result<(), QangPipelineError> {
        // First pass: parse all files and track imports
        for file_path in &file_paths {
            self.parse_file_recursive(file_path)?;
        }

        // Second pass: determine main modules (files that aren't imported)
        for file_path in file_paths {
            // Try to canonicalize the path, but fall back to the original if it fails
            let canonical_path = file_path.canonicalize().unwrap_or_else(|_| file_path.clone());

            // If this file wasn't imported by any other file, it's a main module
            if !self.imported_files.contains(&canonical_path) {
                self.modules.main_modules.push(canonical_path);
            }
        }

        Ok(())
    }

    fn parse_file_recursive(&mut self, file_path: &PathBuf) -> Result<NodeId, QangPipelineError> {
        // Try to canonicalize the path, but fall back to the original if it fails
        // (this supports in-memory test scenarios where files don't exist on disk)
        let canonical_path = file_path.canonicalize().unwrap_or_else(|_| file_path.clone());

        // Track that we've seen this file
        self.processed_files.insert(canonical_path.clone());

        // Return existing if already parsed
        if self.modules.has(&canonical_path) {
            return Ok(self.modules.get(&canonical_path).unwrap().node);
        }

        // Parse this file
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
        let mut parser = Parser::new(source_map.clone(), &mut self.nodes, &mut self.strings);
        let node_id = parser.parse_file();

        let mut errors = parser.into_errors();

        // Insert the module first to prevent infinite recursion
        self.modules
            .insert(&canonical_path, node_id, source_map.clone());

        // Now parse dependencies and mark them as imported
        self.parse_module_dependencies(&canonical_path, node_id, &mut errors)?;

        if errors.has_errors() {
            return Err(QangPipelineError::new(errors.take_errors()));
        }

        Ok(node_id)
    }

    fn parse_module_dependencies(
        &mut self,
        module_path: &Path,
        module_node: NodeId,
        errors: &mut ErrorReporter,
    ) -> Result<(), QangPipelineError> {
        let import_paths = self.extract_import_paths(module_node, module_path);

        for import_path in import_paths {
            // Mark this file as imported (it's not a main module)
            self.imported_files.insert(import_path.clone());

            // Add to dependency graph
            self.modules.add_dependency(module_path, &import_path);

            // Recursively parse the dependency
            match self.parse_file_recursive(&import_path) {
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

    /// Get all main modules (files that aren't imported by others)
    pub fn get_main_modules(&self) -> &[PathBuf] {
        &self.modules.main_modules
    }

    /// Check if a specific file is a main module
    pub fn is_main_module(&self, path: &Path) -> bool {
        let canonical = path.canonicalize().ok();
        canonical
            .as_ref()
            .map(|p| !self.imported_files.contains(p))
            .unwrap_or(false)
    }

    /// Full pipeline: Parse -> Analyze -> Ready for compilation
    pub fn process_files(&mut self, file_paths: Vec<PathBuf>) -> Result<(), QangPipelineError> {
        // Step 1: Parse all files and determine main modules
        self.parse_files_auto_main(file_paths)?;

        // Step 2: Run semantic analysis on all parsed modules
        self.analyze_all_modules()?;

        // Now everything is ready for compilation
        Ok(())
    }

    /// Run analysis using the contained AnalysisPipeline
    fn analyze_all_modules(&mut self) -> Result<(), QangPipelineError> {
        let analysis_config = AnalysisPipelineConfig {
            error_message_format: self.config.error_message_format,
            strict_mode: true,
        };

        // For now, analyze each main module and its dependencies using the legacy approach
        for main_path in self.modules.main_modules.clone() {
            if let Some(legacy_modules) = self.create_analysis_module_map(&main_path) {
                let analyzer = AnalysisPipeline::new(&mut self.strings).with_config(analysis_config);
                let mut errors = ErrorReporter::new();
                analyzer.analyze(&legacy_modules, &mut self.nodes, &mut errors)?;
            }
        }

        Ok(())
    }

    fn create_analysis_module_map(&self, main_path: &Path) -> Option<LegacyModuleMap> {
        self.modules.to_legacy_module_map(main_path)
    }

    /// Compile a specific module to executable form
    pub fn compile_module(
        &mut self,
        module_path: &Path,
        alloc: &mut HeapAllocator,
    ) -> Result<QangProgram, QangPipelineError> {
        use rustc_hash::{FxBuildHasher, FxHashMap};
        use crate::backend::module_resolver::RuntimeModule;

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
            let mut compiled_module = assembler.assemble(module.node, &mut self.nodes, &mut errors)?;
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

    /// Extract import paths from a parsed module
    fn extract_import_paths(&self, module_node: NodeId, module_path: &Path) -> Vec<PathBuf> {
        let mut import_paths = Vec::new();
        let module = self.nodes.get_program_node(module_node);
        let length = self.nodes.array.size(module.node.decls);

        for i in 0..length {
            if let Some(node_id) = self.nodes.array.get_node_id_at(module.node.decls, i) {
                let decl_node = self.nodes.get_decl_node(node_id);
                if let DeclNode::Module(import_decl) = decl_node.node {
                    // Get the import path string
                    let import_path_str = self.strings.get_string(import_decl.path);

                    // Resolve relative to the current module's directory
                    let resolved_path = if let Some(parent_dir) = module_path.parent() {
                        parent_dir.join(import_path_str)
                    } else {
                        PathBuf::from(import_path_str)
                    };

                    // Try to canonicalize the path, but use the resolved path if it fails
                    let final_path = resolved_path.canonicalize().unwrap_or(resolved_path);
                    import_paths.push(final_path);
                }
            }
        }

        import_paths
    }

    /// Get all parsed modules
    pub fn get_all_modules(&self) -> impl Iterator<Item = (&Path, &ModuleSource)> {
        self.modules.iter()
    }

    /// Get the total number of modules
    pub fn module_count(&self) -> usize {
        self.modules.iter().count()
    }

    /// Get modules with errors
    pub fn get_modules_with_errors(&self) -> Vec<&Path> {
        self.modules.modules_with_status(ModuleStatus::Error(Vec::new()))
    }

    /// Get the dependency graph
    pub fn get_dependencies(&self, path: &Path) -> Option<&Vec<PathBuf>> {
        self.modules.get_dependencies(path)
    }

    /// Get reverse dependencies (what depends on this module)
    pub fn get_dependents(&self, path: &Path) -> Option<&Vec<PathBuf>> {
        self.modules.get_dependents(path)
    }

    /// Check for circular dependencies
    pub fn detect_circular_dependencies(&self) -> Vec<Vec<PathBuf>> {
        self.modules.detect_cycles()
    }

    /// Get a module by path
    pub fn get_module(&self, path: &Path) -> Option<&ModuleSource> {
        self.modules.get(path)
    }

    /// Check if all modules have been successfully analyzed
    pub fn all_modules_analyzed(&self) -> bool {
        self.modules.iter().all(|(_, module)| {
            matches!(module.status, ModuleStatus::Analyzed)
        })
    }
}
