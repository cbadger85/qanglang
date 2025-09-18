use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    sync::Arc,
};

use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{NodeId, SourceMap};

// In source.rs
pub struct ModuleMap {
    modules: FxHashMap<PathBuf, ModuleSource>,
    pub main_modules: Vec<PathBuf>, // Multiple main modules now
    dependency_graph: FxHashMap<PathBuf, Vec<PathBuf>>, // who imports whom
    reverse_dependencies: FxHashMap<PathBuf, Vec<PathBuf>>, // who is imported by whom
}

pub struct ModuleSource {
    pub node: NodeId,
    pub source_map: Arc<SourceMap>,
    pub dependencies: Vec<PathBuf>, // What this module imports
    pub status: ModuleStatus,
}

#[derive(Debug, Clone)]
pub enum ModuleStatus {
    Parsing,            // Currently being parsed (for cycle detection)
    Parsed,             // Successfully parsed
    Analyzed,           // Passed semantic analysis
    Error(Vec<String>), // Parse or analysis errors
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

    // Remove the old constructor that took a main module
    // pub fn new(main_id: NodeId, source_map: Arc<SourceMap>) -> Self

    /// Insert a module
    pub fn insert(&mut self, path: &Path, node: NodeId, source_map: Arc<SourceMap>) {
        let path_buf = path.to_path_buf();
        self.modules.insert(
            path_buf,
            ModuleSource {
                node,
                source_map,
                dependencies: Vec::new(),
                status: ModuleStatus::Parsed,
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

    /// Get dependencies of a module
    pub fn get_dependencies(&self, path: &Path) -> Option<&Vec<PathBuf>> {
        self.dependency_graph.get(path)
    }

    /// Get modules that depend on this module
    pub fn get_dependents(&self, path: &Path) -> Option<&Vec<PathBuf>> {
        self.reverse_dependencies.get(path)
    }

    /// Check if a module is a main module (not imported by others)
    pub fn is_main_module(&self, path: &Path) -> bool {
        self.main_modules.contains(&path.to_path_buf())
    }

    /// Update module status
    pub fn set_module_status(&mut self, path: &Path, status: ModuleStatus) {
        if let Some(module) = self.modules.get_mut(path) {
            module.status = status;
        }
    }

    /// Get all modules with a specific status
    pub fn modules_with_status(&self, status: ModuleStatus) -> Vec<&Path> {
        self.modules
            .iter()
            .filter(|(_, module)| {
                std::mem::discriminant(&module.status) == std::mem::discriminant(&status)
            })
            .map(|(path, _)| path.as_path())
            .collect()
    }

    /// Iterate over all modules
    pub fn iter(&self) -> impl Iterator<Item = (&Path, &ModuleSource)> {
        self.modules.iter().map(|(k, v)| (k.as_path(), v))
    }

    /// Iterate over main modules only
    pub fn iter_main_modules(&self) -> impl Iterator<Item = (&Path, &ModuleSource)> {
        self.main_modules.iter().filter_map(move |path| {
            self.modules
                .get(path)
                .map(|module| (path.as_path(), module))
        })
    }


    /// Detect circular dependencies
    pub fn detect_cycles(&self) -> Vec<Vec<PathBuf>> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut cycles = Vec::new();

        for path in self.modules.keys() {
            if !visited.contains(path) {
                self.dfs_cycle_detection(
                    path,
                    &mut visited,
                    &mut rec_stack,
                    &mut cycles,
                    Vec::new(),
                );
            }
        }

        cycles
    }

    fn dfs_cycle_detection(
        &self,
        current: &PathBuf,
        visited: &mut HashSet<PathBuf>,
        rec_stack: &mut HashSet<PathBuf>,
        cycles: &mut Vec<Vec<PathBuf>>,
        mut current_path: Vec<PathBuf>,
    ) {
        visited.insert(current.clone());
        rec_stack.insert(current.clone());
        current_path.push(current.clone());

        if let Some(dependencies) = self.dependency_graph.get(current) {
            for dep in dependencies {
                if rec_stack.contains(dep) {
                    // Found a cycle
                    if let Some(cycle_start) = current_path.iter().position(|p| p == dep) {
                        let mut cycle = current_path[cycle_start..].to_vec();
                        cycle.push(dep.clone()); // Close the cycle
                        cycles.push(cycle);
                    }
                } else if !visited.contains(dep) {
                    self.dfs_cycle_detection(dep, visited, rec_stack, cycles, current_path.clone());
                }
            }
        }

        rec_stack.remove(current);
    }
}
