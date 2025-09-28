use std::{path::PathBuf, sync::Arc};

use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};

use crate::{
    ErrorReporter, ModuleMap, NodeId, QangCompilerError, SourceMap, StringHandle, StringInterner,
    TypedNodeArena,
    frontend::{
        node_visitor::{NodeVisitor, VisitorContext},
        types::{TypeId, TypeNode},
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

    fn declare_type(&mut self, name: StringHandle, type_id: TypeId) {
        self.type_names.insert(name, type_id);
    }

    fn lookup_type(&self, name: StringHandle) -> Option<TypeId> {
        self.type_names.get(&name).copied()
    }
}

pub struct TypeResolver<'a> {
    strings: &'a mut StringInterner,
    source_map: Arc<SourceMap>,
    scopes: Vec<TypeEnvironment>,
    modules: &'a mut ModuleMap,
    module_path: PathBuf,
    unresolved_worklist: Vec<TypeId>,
    currently_resolving: FxHashSet<TypeId>,
}

impl<'a> TypeResolver<'a> {
    pub fn new(
        strings: &'a mut StringInterner,
        source_map: Arc<SourceMap>,
        module_path: PathBuf,
        modules: &'a mut ModuleMap,
        global_types: TypeEnvironment,
    ) -> Self {
        Self {
            strings,
            source_map,
            scopes: vec![global_types],
            modules,
            unresolved_worklist: Vec::new(),
            module_path,
            currently_resolving: FxHashSet::with_hasher(FxBuildHasher),
        }
    }

    pub fn resolve(mut self, nodes: &mut TypedNodeArena, errors: &mut ErrorReporter) {
        let module_node_id = self.modules.get(&self.module_path).map(|m| m.node).expect(
            format!(
                "Expected module {} to be parsed.",
                self.module_path.display()
            )
            .as_str(),
        );
        let mut ctx = VisitorContext::new(nodes, errors);
        let module_node = ctx.nodes.get_program_node(module_node_id);

        let _ = self.visit_module(module_node, &mut ctx);
    }

    fn begin_scope(&mut self) {
        self.scopes.push(TypeEnvironment::new());
    }

    fn end_scope(&mut self, ctx: &mut VisitorContext) {
        // Process the worklist until no more progress
        let mut progress = true;
        while progress && !self.unresolved_worklist.is_empty() {
            progress = false;

            // Use mem::take to avoid clone - takes ownership of the vec and replaces with empty
            let mut type_ids_to_check = std::mem::take(&mut self.unresolved_worklist);

            for type_id in type_ids_to_check.drain(..) {
                // Use try_resolve_type which handles circular references
                if self.try_resolve_type(type_id, ctx) {
                    progress = true;
                    // Successfully resolved - don't add back to worklist
                } else {
                    // Keep in worklist - either circular or not found yet
                    self.unresolved_worklist.push(type_id);
                }
            }
        }

        for &_type_id in &self.unresolved_worklist {
            // TODO Report as "type not found" since circular refs were already reported
        }

        self.unresolved_worklist.clear();
        self.scopes.pop();
    }

    fn is_global_scope(&self) -> bool {
        self.scopes.len() == 1
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

    fn try_resolve_type(&mut self, type_id: TypeId, ctx: &mut VisitorContext) -> bool {
        if self.currently_resolving.contains(&type_id) {
            // TODO report circular reference
            return false;
        }

        self.currently_resolving.insert(type_id);

        let type_info = ctx.nodes.type_table.get_type_info(type_id).unwrap();
        let result = if let TypeNode::UnresolvedReference { name, .. } = &type_info.type_node {
            if let Some(resolved_type_id) = self.lookup_type(*name) {
                // Recursively resolve the target (this catches A->B->A cycles)
                if self.try_resolve_type(resolved_type_id, ctx) {
                    // Replace with resolved type
                    let resolved_info = ctx
                        .nodes
                        .type_table
                        .get_type_info(resolved_type_id)
                        .unwrap()
                        .clone();
                    ctx.nodes.type_table.replace_type(type_id, resolved_info);
                    true
                } else {
                    false
                }
            } else {
                false // Not found
            }
        } else {
            true // Already resolved
        };

        self.currently_resolving.remove(&type_id);
        result
    }
}

impl<'a> NodeVisitor for TypeResolver<'a> {
    type Error = QangCompilerError;

    fn visit_declaration(
        &mut self,
        decl: super::typed_node_arena::TypedNodeRef<super::nodes::DeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if self.is_global_scope() {
            // TODO add to module exports
        }
        match decl.node {
            super::nodes::DeclNode::Class(class_decl) => self.visit_class_declaration(
                super::typed_node_arena::TypedNodeRef::new(decl.id, class_decl),
                ctx,
            ),
            super::nodes::DeclNode::Function(func_decl) => self.visit_function_declaration(
                super::typed_node_arena::TypedNodeRef::new(decl.id, func_decl),
                ctx,
            ),
            super::nodes::DeclNode::Lambda(lambda_decl) => self.visit_lambda_declaration(
                super::typed_node_arena::TypedNodeRef::new(decl.id, lambda_decl),
                ctx,
            ),
            super::nodes::DeclNode::Variable(var_decl) => self.visit_variable_declaration(
                super::typed_node_arena::TypedNodeRef::new(decl.id, var_decl),
                ctx,
            ),
            super::nodes::DeclNode::Module(import_decl) => self.visit_import_module_declaration(
                super::typed_node_arena::TypedNodeRef::new(decl.id, import_decl),
                ctx,
            ),
            super::nodes::DeclNode::Type(type_decl) => self.visit_type_alias_declaration(
                super::typed_node_arena::TypedNodeRef::new(decl.id, type_decl),
                ctx,
            ),
            super::nodes::DeclNode::Stmt(stmt) => self.visit_statement(
                super::typed_node_arena::TypedNodeRef::new(decl.id, stmt),
                ctx,
            ),
        }
    }

    fn visit_function_expression(
        &mut self,
        func_expr: super::typed_node_arena::TypedNodeRef<super::nodes::FunctionExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // TODO declare function type in current scope

        self.begin_scope();

        // TODO add the parameters types to current scope

        // TODO visit body to gather types

        // TODO visit body again to resolve types that were not declared yet

        self.end_scope(ctx);

        Ok(())
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: super::typed_node_arena::TypedNodeRef<super::nodes::LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        // TODO add the parameters types to current scope

        // TODO visit body to gather types

        // TODO visit body again to resolve types that were not declared yet

        self.end_scope(ctx);

        Ok(())
    }

    fn visit_map_expression(
        &mut self,
        map_expr: super::typed_node_arena::TypedNodeRef<super::nodes::MapExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        let body = ctx.nodes.get_expr_node(map_expr.node.body);
        self.visit_expression(body, ctx)?;

        // TODO visit expression again to resolve types that were not declared yet

        self.end_scope(ctx);

        Ok(())
    }

    fn visit_optional_map_expression(
        &mut self,
        map_expr: super::typed_node_arena::TypedNodeRef<super::nodes::OptionalMapExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        let body = ctx.nodes.get_expr_node(map_expr.node.body);
        self.visit_expression(body, ctx)?;

        // TODO visit expression again to resolve types that were not declared yet

        self.end_scope(ctx);

        Ok(())
    }

    fn visit_block_statement(
        &mut self,
        block_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::BlockStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        let decl_count = ctx.nodes.array.size(block_stmt.node.decls);
        for i in 0..decl_count {
            if let Some(decl_id) = ctx.nodes.array.get_node_id_at(block_stmt.node.decls, i) {
                let decl = ctx.nodes.get_decl_node(decl_id);
                self.visit_declaration(decl, ctx)?;
            }
        }

        // TODO visit statements again to resolve types that were not declared yet

        self.end_scope(ctx);
        Ok(())
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::ForStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        if let Some(initializer_id) = for_stmt.node.initializer {
            let initializer = ctx.nodes.get_for_initializer_node(initializer_id);
            self.visit_for_initializer(initializer, ctx)?;
        }

        if let Some(condition_id) = for_stmt.node.condition {
            self.visit_expression(ctx.nodes.get_expr_node(condition_id), ctx)?;
        }

        if let Some(increment_id) = for_stmt.node.increment {
            self.visit_expression(ctx.nodes.get_expr_node(increment_id), ctx)?;
        }

        self.visit_statement(ctx.nodes.get_stmt_node(for_stmt.node.body), ctx)?;

        self.end_scope(ctx);

        Ok(())
    }

    fn visit_class_declaration(
        &mut self,
        class_decl: super::typed_node_arena::TypedNodeRef<super::nodes::ClassDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // TODO declare class type in current scope

        self.begin_scope();

        let member_count = ctx.nodes.array.size(class_decl.node.members);
        for i in 0..member_count {
            if let Some(member_id) = ctx.nodes.array.get_node_id_at(class_decl.node.members, i) {
                let member = ctx.nodes.get_class_member_node(member_id);
                match member.node {
                    super::nodes::ClassMemberNode::Method(method) => {
                        let method_name = ctx.nodes.get_identifier_node(method.name);
                        let init_name = self.strings.intern("init");
                        let is_initializer = method_name.node.name == init_name;
                        let arity = ctx.nodes.array.size(method.parameters);

                        self.begin_scope();

                        for j in 0..arity {
                            if let Some(param_id) =
                                ctx.nodes.array.get_node_id_at(method.parameters, j)
                            {
                                // TODO add the parameters types to current scope
                            }
                        }

                        let body = ctx.nodes.get_block_stmt_node(method.body);
                        self.visit_block_statement(body, ctx)?;

                        // TODO visit body again to resolve types that were not declared yet

                        self.end_scope(ctx);
                    }
                    super::nodes::ClassMemberNode::Field(field) => {
                        if let Some(init_id) = field.initializer {
                            let initializer = ctx.nodes.get_expr_node(init_id);
                            self.visit_expression(initializer, ctx)?;
                        }
                    }
                }
            }
        }

        self.end_scope(ctx);

        // TODO update class type with method/field type data

        Ok(())
    }

    fn visit_import_module_declaration(
        &mut self,
        import_decl: super::typed_node_arena::TypedNodeRef<super::nodes::ImportModuleDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // TODO
        Ok(())
    }

    fn visit_generic_parameter(
        &mut self,
        _generic_param: super::typed_node_arena::TypedNodeRef<super::nodes::GenericParameterNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // TODO
        Ok(())
    }

    fn visit_parameter(
        &mut self,
        parameter: super::typed_node_arena::TypedNodeRef<super::nodes::ParameterNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // TODO
        Ok(())
    }

    fn visit_type_alias_declaration(
        &mut self,
        _type_decl: super::typed_node_arena::TypedNodeRef<super::nodes::TypeAliasDeclNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // TODO
        Ok(())
    }

    fn visit_type_cast_expression(
        &mut self,
        type_cast: super::typed_node_arena::TypedNodeRef<super::nodes::TypeCastExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // TODO
        Ok(())
    }
}
