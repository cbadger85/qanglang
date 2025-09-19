use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{NodeId, SourceMap};

#[derive(Debug, Default, Clone)]
pub struct ModuleMap {
    modules: FxHashMap<PathBuf, ModuleSource>,
    pub main_modules: Vec<PathBuf>,
    dependency_graph: FxHashMap<PathBuf, Vec<PathBuf>>,
    reverse_dependencies: FxHashMap<PathBuf, Vec<PathBuf>>,
}

#[derive(Debug, Default, Clone)]
pub struct ModuleSource {
    pub node: NodeId,
    pub source_map: Arc<SourceMap>,
    pub dependencies: Vec<PathBuf>,
}

impl ModuleMap {
    pub fn new() -> Self {
        Self {
            modules: FxHashMap::with_hasher(FxBuildHasher),
            main_modules: Vec::new(),
            dependency_graph: FxHashMap::with_hasher(FxBuildHasher),
            reverse_dependencies: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn insert(&mut self, path: &Path, node: NodeId, source_map: Arc<SourceMap>) {
        let path_buf = path.to_path_buf();
        self.modules.insert(
            path_buf,
            ModuleSource {
                node,
                source_map,
                dependencies: Vec::new(),
            },
        );
    }

    /// Mark a module as a main module (not imported by others)
    pub fn add_main_module(&mut self, path: PathBuf) {
        if !self.main_modules.contains(&path) {
            self.main_modules.push(path);
        }
    }

    /// Add a dependency relationship
    pub fn add_dependency(&mut self, from: &Path, to: &Path) {
        let from_path = from.to_path_buf();
        let to_path = to.to_path_buf();

        // Add to dependency graph
        self.dependency_graph
            .entry(from_path.clone())
            .or_insert_with(Vec::new)
            .push(to_path.clone());

        // Add to reverse dependencies
        self.reverse_dependencies
            .entry(to_path.clone())
            .or_insert_with(Vec::new)
            .push(from_path.clone());

        // Update the module's dependency list
        if let Some(module) = self.modules.get_mut(&from_path) {
            if !module.dependencies.contains(&to_path) {
                module.dependencies.push(to_path);
            }
        }
    }

    /// Get all main modules
    pub fn get_main_modules(&self) -> &[PathBuf] {
        &self.main_modules
    }

    /// Get the first main module (for backward compatibility)
    pub fn get_main(&self) -> Option<&ModuleSource> {
        self.main_modules
            .first()
            .and_then(|path| self.modules.get(path))
    }

    /// Get a specific module
    pub fn get(&self, path: &Path) -> Option<&ModuleSource> {
        self.modules.get(path)
    }

    /// Check if a module exists
    pub fn has(&self, path: &Path) -> bool {
        self.modules.contains_key(path)
    }

    /// Check if a module is a main module (not imported by others)
    pub fn is_main_module(&self, path: &Path) -> bool {
        self.main_modules.contains(&path.to_path_buf())
    }

    /// Iterate over all modules
    pub fn iter(&self) -> impl Iterator<Item = (&Path, &ModuleSource)> {
        self.modules.iter().map(|(k, v)| (k.as_path(), v))
    }
}
