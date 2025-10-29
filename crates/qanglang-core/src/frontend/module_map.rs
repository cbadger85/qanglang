use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{NodeId, SourceMap, frontend::symbol_resolver::SymbolTable};

#[derive(Debug, Default, Clone)]
pub struct ModuleMap {
    modules: FxHashMap<PathBuf, ModuleSource>,
    main_module: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct ModuleSource {
    pub node: NodeId,
    pub source_map: Arc<SourceMap>,
    pub symbol_table: SymbolTable,
}

impl Default for ModuleSource {
    fn default() -> Self {
        Self {
            node: NodeId::default(),
            source_map: Arc::new(SourceMap::default()),
            symbol_table: SymbolTable::default(),
        }
    }
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
        self.modules
            .insert(path_buf, ModuleSource { node, source_map, symbol_table: SymbolTable::default() });
    }

    pub fn insert_with_symbols(&mut self, path: &Path, node: NodeId, source_map: Arc<SourceMap>, symbol_table: SymbolTable) {
        let path_buf = path.to_path_buf();
        self.modules
            .insert(path_buf, ModuleSource { node, source_map, symbol_table });
    }

    pub fn set_main_module(&mut self, path: PathBuf) {
        self.main_module = Some(path);
    }

    pub fn get_main_path(&self) -> Option<&Path> {
        self.main_module.as_deref()
    }

    pub fn get_main(&self) -> Option<&ModuleSource> {
        self.main_module
            .as_ref()
            .and_then(|path| self.modules.get(path))
    }

    pub fn get(&self, path: &Path) -> Option<&ModuleSource> {
        self.modules.get(path)
    }

    pub fn has(&self, path: &Path) -> bool {
        self.modules.contains_key(path)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Path, &ModuleSource)> {
        self.modules.iter().map(|(k, v)| (k.as_path(), v))
    }
}
