use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{StringHandle, TypedNodeArena, frontend::types::TypeId};

#[derive(Debug, Clone, Default)]
pub struct TypeEnvironment {
    type_names: FxHashMap<StringHandle, TypeId>,
    variable_types: FxHashMap<StringHandle, TypeId>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            type_names: FxHashMap::with_hasher(FxBuildHasher),
            variable_types: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn with_globals(_nodes: &mut TypedNodeArena) -> Self {
        let type_names = FxHashMap::with_hasher(FxBuildHasher);

        // TODO insert globals from stdlib.ql

        Self {
            type_names,
            variable_types: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn declare_type(&mut self, name: StringHandle, type_id: TypeId) {
        self.type_names.insert(name, type_id);
    }

    fn lookup_type(&self, name: StringHandle) -> Option<TypeId> {
        self.type_names.get(&name).copied()
    }

    pub fn declare_variable(&mut self, name: StringHandle, type_id: TypeId) {
        self.variable_types.insert(name, type_id);
    }

    fn lookup_variable(&self, name: StringHandle) -> Option<TypeId> {
        self.variable_types.get(&name).copied()
    }
}

pub struct TypeScopeManager {
    scopes: Vec<TypeEnvironment>,
}

impl TypeScopeManager {
    pub fn new(global_types: TypeEnvironment) -> Self {
        Self {
            scopes: vec![global_types],
        }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(TypeEnvironment::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn get_scope_mut(&mut self) -> &mut TypeEnvironment {
        self.scopes
            .last_mut()
            .expect("Unexpected empty scope stack.")
    }

    pub fn lookup_type(&self, name: StringHandle) -> Option<TypeId> {
        for scope in self.scopes.iter().rev() {
            if let Some(type_id) = scope.lookup_type(name) {
                return Some(type_id);
            }
        }

        None
    }

    pub fn lookup_variable(&self, name: StringHandle) -> Option<TypeId> {
        for scope in self.scopes.iter().rev() {
            if let Some(type_id) = scope.lookup_variable(name) {
                return Some(type_id);
            }
        }

        None
    }

    pub fn is_global_scope(&self) -> bool {
        self.scopes.len() == 1
    }
}
