use std::sync::Arc;

use crate::{
    ErrorReporter, NodeId, QangCompilerError, SourceMap, StringInterner, TypedNodeArena,
    frontend::{
        node_visitor::{NodeVisitor, VisitorContext},
        type_scope_manager::{TypeEnvironment, TypeScopeManager},
    },
};

pub struct InferenceEngine<'a> {
    strings: &'a mut StringInterner,
    source_map: Arc<SourceMap>,
    scopes: TypeScopeManager,
}

impl<'a> InferenceEngine<'a> {
    pub fn new(
        strings: &'a mut StringInterner,
        source_map: Arc<SourceMap>,
        global_types: TypeEnvironment,
    ) -> Self {
        Self {
            strings,
            source_map,
            scopes: TypeScopeManager::new(global_types),
        }
    }

    pub fn infer(
        &mut self,
        module_node_id: NodeId,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) {
        let mut ctx = VisitorContext::new(nodes, errors);
        let module_node = ctx.nodes.get_program_node(module_node_id);

        let _ = self.visit_module(module_node, &mut ctx);
    }
}

impl<'a> NodeVisitor for InferenceEngine<'a> {
    type Error = QangCompilerError;
}
