use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

use crate::{NodeId, SourceMap, StringHandle, frontend::types::TypeId};

#[derive(Debug, Default, Clone)]
pub struct ModuleSource {
    pub node: NodeId,
    pub source_map: Arc<SourceMap>,
    pub exported_types: FxHashMap<StringHandle, TypeId>,
    pub resolved_handles: FxHashSet<StringHandle>,
    pub resolving_handles: FxHashSet<StringHandle>,
}

impl ModuleSource {
    pub fn is_resolved(&self) -> bool {
        let keys = self
            .exported_types
            .keys()
            .copied()
            .collect::<FxHashSet<_>>();
        keys == self.resolved_handles
    }
}

#[derive(Debug, Default, Clone)]
pub struct ModuleMap {
    modules: FxHashMap<PathBuf, ModuleSource>,
    main_module: Option<PathBuf>,
}

impl ModuleMap {
    pub fn new() -> Self {
        Self {
            modules: FxHashMap::with_hasher(FxBuildHasher),
            main_module: None,
        }
    }

    pub fn insert(&mut self, path: &Path, node: NodeId, source_map: Arc<SourceMap>) {
        let path_buf = path.to_path_buf();
        self.modules.insert(
            path_buf,
            ModuleSource {
                node,
                source_map,
                exported_types: FxHashMap::with_hasher(FxBuildHasher),
                resolved_handles: FxHashSet::with_hasher(FxBuildHasher),
                resolving_handles: FxHashSet::with_hasher(FxBuildHasher),
            },
        );
    }

    pub fn add_main_module(&mut self, path: PathBuf) {
        self.main_module = Some(path);
    }

    pub fn get_main(&self) -> Option<&ModuleSource> {
        self.main_module
            .as_ref()
            .and_then(|path| self.modules.get(path))
    }

    pub fn get_main_path(&self) -> Option<&Path> {
        self.main_module.as_ref().map(|p| p.as_path())
    }

    pub fn get(&self, path: &Path) -> Option<&ModuleSource> {
        self.modules.get(path)
    }

    pub fn get_mut(&mut self, path: &Path) -> Option<&mut ModuleSource> {
        self.modules.get_mut(path)
    }

    pub fn has(&self, path: &Path) -> bool {
        self.modules.contains_key(path)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Path, &ModuleSource)> {
        self.modules.iter().map(|(k, v)| (k.as_path(), v))
    }
}
