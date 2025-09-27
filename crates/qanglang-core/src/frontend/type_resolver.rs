use std::sync::Arc;

use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{
    ErrorReporter, NodeId, QangCompilerError, SourceMap, StringHandle, StringInterner,
    TypedNodeArena,
    frontend::{
        node_visitor::{NodeVisitor, VisitorContext},
        types::TypeId,
    },
};

#[derive(Debug, Clone, Default)]
pub struct TypeEnvironment {
    type_names: FxHashMap<StringHandle, TypeId>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            type_names: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn with_globals(_nodes: &mut TypedNodeArena) -> Self {
        let type_names = FxHashMap::with_hasher(FxBuildHasher);

        // TODO insert globals from stdlib.ql

        Self { type_names }
    }

    pub fn declare_type(&mut self, name: StringHandle, type_id: TypeId) {
        self.type_names.insert(name, type_id);
    }

    pub fn lookup_type(&self, name: StringHandle) -> Option<TypeId> {
        self.type_names.get(&name).copied()
    }
}

pub struct TypeResolver<'a> {
    strings: &'a mut StringInterner,
    source_map: Arc<SourceMap>,
    scopes: Vec<TypeEnvironment>,
    exported_types: FxHashMap<StringHandle, TypeId>,
}

impl<'a> TypeResolver<'a> {
    pub fn new(
        strings: &'a mut StringInterner,
        source_map: Arc<SourceMap>,
        global_types: TypeEnvironment,
    ) -> Self {
        Self {
            strings,
            source_map,
            scopes: vec![global_types],
            exported_types: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn collect(
        mut self,
        program: NodeId,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> FxHashMap<StringHandle, TypeId> {
        let mut ctx = VisitorContext::new(nodes, errors);
        let program_node = ctx.nodes.get_program_node(program);

        let _ = self.visit_module(program_node, &mut ctx);

        self.exported_types
    }

    fn start_scope(&mut self) {
        self.scopes.push(TypeEnvironment::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_scope_mut(&mut self) -> &mut TypeEnvironment {
        self.scopes
            .last_mut()
            .expect("Unexpected empty scope stack.")
    }

    fn lookup_type(&self, name: StringHandle) -> Option<TypeId> {
        for scope in self.scopes.iter().rev() {
            if let Some(type_id) = scope.lookup_type(name) {
                return Some(type_id);
            }
        }

        None
    }
}

impl<'a> NodeVisitor for TypeResolver<'a> {
    type Error = QangCompilerError;
}
