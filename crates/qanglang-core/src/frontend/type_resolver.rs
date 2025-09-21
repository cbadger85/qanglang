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
    /// Maps type names to their definitions
    type_names: FxHashMap<StringHandle, TypeId>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            type_names: FxHashMap::with_hasher(FxBuildHasher),
        }
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
}

impl<'a> TypeResolver<'a> {
    pub fn new(strings: &'a mut StringInterner, source_map: Arc<SourceMap>) -> Self {
        TypeResolver {
            strings,
            source_map,
        }
    }

    pub fn resolve(
        &mut self,
        program: NodeId,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) {
        let mut ctx = VisitorContext::new(nodes, errors);
        let program_node = ctx.nodes.get_program_node(program);

        let _ = self.visit_module(program_node, &mut ctx);
    }
}

impl<'a> NodeVisitor for TypeResolver<'a> {
    type Error = QangCompilerError;
}
