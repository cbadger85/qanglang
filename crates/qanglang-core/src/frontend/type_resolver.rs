use std::{path::PathBuf, sync::Arc};

use rustc_hash::{FxBuildHasher, FxHashSet};

use crate::{
    ErrorReporter, ModuleMap, NodeId, QangCompilerError, SourceMap, StringHandle, StringInterner,
    TypedNodeArena,
    frontend::{
        node_visitor::{NodeVisitor, VisitorContext},
        type_scope_manager::{TypeEnvironment, TypeScopeManager},
        types::{FunctionType, GenericType, TypeId, TypeInfo, TypeNode, TypeOrigin},
    },
};

pub struct TypeResolver<'a> {
    strings: &'a mut StringInterner,
    source_map: Arc<SourceMap>,
    modules: &'a mut ModuleMap,
    module_path: PathBuf,
    unresolved_worklist: Vec<TypeId>,
    currently_resolving: FxHashSet<TypeId>,
    scopes: TypeScopeManager,
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
            scopes: TypeScopeManager::new(global_types),
            modules,
            unresolved_worklist: Vec::new(),
            module_path,
            currently_resolving: FxHashSet::with_hasher(FxBuildHasher),
        }
    }

    pub fn resolve(&mut self, nodes: &mut TypedNodeArena, errors: &mut ErrorReporter) {
        let module_node_id = self
            .modules
            .get(&self.module_path)
            .map(|m| m.node)
            .unwrap_or_else(|| {
                panic!(
                    "Expected module {} to be parsed.",
                    self.module_path.display()
                )
            });
        let mut ctx = VisitorContext::new(nodes, errors);
        let module_node = ctx.nodes.get_program_node(module_node_id);

        let _ = self.visit_module(module_node, &mut ctx);
    }

    fn begin_scope(&mut self) {
        self.scopes.begin_scope();
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

        // Report remaining unresolved types as "type not found"
        let remaining_unresolved = std::mem::take(&mut self.unresolved_worklist);
        for type_id in remaining_unresolved {
            self.report_type_not_found_error(type_id, ctx);
        }
        self.scopes.end_scope();
    }

    fn try_resolve_type(&mut self, type_id: TypeId, ctx: &mut VisitorContext) -> bool {
        if self.currently_resolving.contains(&type_id) {
            // Circular reference detected
            self.report_circular_reference_error(type_id, ctx);
            return false;
        }

        self.currently_resolving.insert(type_id);

        let type_info = ctx.nodes.type_table.get_type_info(type_id).unwrap().clone();
        let result = match &type_info.type_node {
            TypeNode::UnresolvedReference {
                name,
                identifier_node,
            } => self.resolve_unresolved_reference(*name, *identifier_node, type_id, ctx),
            TypeNode::TypeImport {
                module_path,
                type_name,
            } => self.resolve_type_import(*module_path, *type_name, type_id, ctx),
            TypeNode::Generic(generic_type) => {
                self.resolve_generic_type(generic_type, type_id, ctx)
            }
            TypeNode::Array(element_type_id) => {
                // Recursively resolve array element type
                if self.try_resolve_type(*element_type_id, ctx) {
                    true // Array type is resolved when element type is resolved
                } else {
                    false
                }
            }
            TypeNode::Function(func_type) => self.resolve_function_type(func_type, type_id, ctx),
            TypeNode::Union(type_ids) => {
                // Resolve all union member types
                let mut all_resolved = true;
                for &member_type_id in type_ids {
                    if !self.try_resolve_type(member_type_id, ctx) {
                        all_resolved = false;
                    }
                }
                all_resolved
            }
            TypeNode::ConstrainedTypeParameter { constraint, .. } => {
                // Resolve constraint type
                self.try_resolve_type(*constraint, ctx)
            }
            // These types are already resolved
            TypeNode::Primitive(_)
            | TypeNode::TypeParameter(_)
            | TypeNode::Object(_)
            | TypeNode::Class(_)
            | TypeNode::DynamicTop
            | TypeNode::DynamicNullable
            | TypeNode::Dynamic
            | TypeNode::Module(_) => true,
        };

        self.currently_resolving.remove(&type_id);
        result
    }

    fn resolve_unresolved_reference(
        &mut self,
        name: StringHandle,
        identifier_node: NodeId,
        type_id: TypeId,
        ctx: &mut VisitorContext,
    ) -> bool {
        // First try local scopes
        if let Some(resolved_type_id) = self.scopes.lookup_type(name) {
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
                return true;
            } else {
                return false;
            }
        }

        // If not found locally, try current module's exported types
        if let Some(module) = self.modules.get(&self.module_path)
            && let Some(&exported_type_id) = module.exported_types.get(&name)
            && self.try_resolve_type(exported_type_id, ctx)
        {
            let resolved_info = ctx
                .nodes
                .type_table
                .get_type_info(exported_type_id)
                .unwrap()
                .clone();
            ctx.nodes.type_table.replace_type(type_id, resolved_info);
            return true;
        }

        // Type not found - report error
        self.report_unresolved_type_error(name, identifier_node, ctx);
        false
    }

    fn resolve_type_import(
        &mut self,
        module_path: StringHandle,
        type_name: StringHandle,
        type_id: TypeId,
        ctx: &mut VisitorContext,
    ) -> bool {
        let module_path_str = self.strings.get_string(module_path);
        let module_path_buf = std::path::PathBuf::from(module_path_str);

        // Check if target module exists and has been parsed
        if let Some(target_module) = self.modules.get(&module_path_buf) {
            // Check for circular import prevention
            if target_module.resolving_handles.contains(&type_name) {
                self.report_circular_import_error(module_path, type_name, ctx);
                return false;
            }

            // Check if type is exported by target module
            if let Some(&exported_type_id) = target_module.exported_types.get(&type_name) {
                // Mark as resolving to prevent circular imports
                if let Some(target_module) = self.modules.get_mut(&module_path_buf) {
                    target_module.resolving_handles.insert(type_name);
                }

                // Recursively resolve the imported type
                let resolved = self.try_resolve_type(exported_type_id, ctx);

                // Mark as resolved and remove from resolving
                if let Some(target_module) = self.modules.get_mut(&module_path_buf) {
                    target_module.resolving_handles.remove(&type_name);
                    if resolved {
                        target_module.resolved_handles.insert(type_name);
                    }
                }

                if resolved {
                    // Replace TypeImport with resolved type
                    let resolved_info = ctx
                        .nodes
                        .type_table
                        .get_type_info(exported_type_id)
                        .unwrap()
                        .clone();
                    ctx.nodes.type_table.replace_type(type_id, resolved_info);
                    return true;
                }
            } else {
                // Type not exported by target module
                self.report_type_not_exported_error(module_path, type_name, ctx);
            }
        } else {
            // Target module not found
            self.report_module_not_found_error(module_path, ctx);
        }

        false
    }

    fn resolve_generic_type(
        &mut self,
        generic_type: &GenericType,
        type_id: TypeId,
        ctx: &mut VisitorContext,
    ) -> bool {
        // First resolve the base type
        let base_type_name = generic_type.base_name;
        if let Some(base_type_id) = self.scopes.lookup_type(base_type_name) {
            if !self.try_resolve_type(base_type_id, ctx) {
                return false;
            }
        } else {
            self.report_unresolved_base_type_error(base_type_name, ctx);
            return false;
        }

        // Resolve all type arguments
        let mut all_resolved = true;
        for &arg_type_id in &generic_type.type_arguments {
            if !self.try_resolve_type(arg_type_id, ctx) {
                all_resolved = false;
            }
        }

        if all_resolved {
            // All components resolved - update the type table with the resolved generic type
            // The generic type is now fully resolved (base type and all arguments are resolved)
            let resolved_generic_type = GenericType {
                base_name: generic_type.base_name,
                type_arguments: generic_type.type_arguments.clone(),
            };

            let resolved_type_info = TypeInfo {
                type_node: TypeNode::Generic(resolved_generic_type),
                origin: ctx
                    .nodes
                    .type_table
                    .get_type_info(type_id)
                    .unwrap()
                    .origin
                    .clone(),
            };

            ctx.nodes
                .type_table
                .replace_type(type_id, resolved_type_info);
        }

        all_resolved
    }

    fn resolve_function_type(
        &mut self,
        func_type: &FunctionType,
        type_id: TypeId,
        ctx: &mut VisitorContext,
    ) -> bool {
        // Resolve all parameter types
        let mut all_resolved = true;
        for &param_type_id in &func_type.parameters {
            if !self.try_resolve_type(param_type_id, ctx) {
                all_resolved = false;
            }
        }

        // Resolve return type
        if !self.try_resolve_type(func_type.return_type, ctx) {
            all_resolved = false;
        }

        if all_resolved {
            // All components resolved - update the type table with the resolved function type
            // The function type is now fully resolved (all parameters and return type are resolved)
            let resolved_function_type = FunctionType {
                generic_parameters: func_type.generic_parameters.clone(),
                parameters: func_type.parameters.clone(),
                return_type: func_type.return_type,
            };

            let resolved_type_info = TypeInfo {
                type_node: TypeNode::Function(resolved_function_type),
                origin: ctx
                    .nodes
                    .type_table
                    .get_type_info(type_id)
                    .unwrap()
                    .origin
                    .clone(),
            };

            ctx.nodes
                .type_table
                .replace_type(type_id, resolved_type_info);
        }

        all_resolved
    }

    // Helper methods
    fn extract_span_from_type(
        &self,
        type_id: TypeId,
        ctx: &VisitorContext,
    ) -> crate::nodes::SourceSpan {
        if let Some(type_info) = ctx.nodes.type_table.get_type_info(type_id) {
            match &type_info.origin {
                TypeOrigin::Annotation(node_id) | TypeOrigin::Inferred(node_id) => {
                    ctx.nodes.get_node(*node_id).span()
                }
                TypeOrigin::Builtin => crate::nodes::SourceSpan::default(),
            }
        } else {
            crate::nodes::SourceSpan::default()
        }
    }

    // Error reporting methods
    fn report_circular_reference_error(&mut self, type_id: TypeId, ctx: &mut VisitorContext) {
        let message = "Circular type reference detected".to_string();
        let span = self.extract_span_from_type(type_id, ctx);
        let error =
            crate::QangCompilerError::new_type_error(message, span, self.source_map.clone());
        ctx.errors.report_error(error);
    }

    fn report_unresolved_type_error(
        &mut self,
        name: StringHandle,
        identifier_node: NodeId,
        ctx: &mut VisitorContext,
    ) {
        let type_name = self.strings.get_string(name);
        let span = ctx.nodes.get_node(identifier_node).span();
        let message = format!("Unresolved type '{}'", type_name);
        let error =
            crate::QangCompilerError::new_type_error(message, span, self.source_map.clone());
        ctx.errors.report_error(error);
    }

    fn report_circular_import_error(
        &mut self,
        module_path: StringHandle,
        type_name: StringHandle,
        ctx: &mut VisitorContext,
    ) {
        let module_path_str = self.strings.get_string(module_path);
        let type_name_str = self.strings.get_string(type_name);
        let message = format!(
            "Circular type import detected for type '{}' from module '{}'",
            type_name_str, module_path_str
        );
        // For import errors, we use default span since we don't have the specific import node context
        let error = crate::QangCompilerError::new_type_error(
            message,
            crate::nodes::SourceSpan::default(),
            self.source_map.clone(),
        );
        ctx.errors.report_error(error);
    }

    fn report_type_not_exported_error(
        &mut self,
        module_path: StringHandle,
        type_name: StringHandle,
        ctx: &mut VisitorContext,
    ) {
        let module_path_str = self.strings.get_string(module_path);
        let type_name_str = self.strings.get_string(type_name);
        let message = format!(
            "Type '{}' is not exported by module '{}'",
            type_name_str, module_path_str
        );
        // For import errors, we use default span since we don't have the specific import node context
        let error = crate::QangCompilerError::new_type_error(
            message,
            crate::nodes::SourceSpan::default(),
            self.source_map.clone(),
        );
        ctx.errors.report_error(error);
    }

    fn report_module_not_found_error(
        &mut self,
        module_path: StringHandle,
        ctx: &mut VisitorContext,
    ) {
        let module_path_str = self.strings.get_string(module_path);
        let message = format!("Module '{}' not found", module_path_str);
        // For module errors, we use default span since we don't have the specific import node context
        let error = crate::QangCompilerError::new_type_error(
            message,
            crate::nodes::SourceSpan::default(),
            self.source_map.clone(),
        );
        ctx.errors.report_error(error);
    }

    fn report_unresolved_base_type_error(
        &mut self,
        base_type_name: StringHandle,
        ctx: &mut VisitorContext,
    ) {
        let type_name_str = self.strings.get_string(base_type_name);
        let message = format!("Unresolved base type '{}' in generic type", type_name_str);
        // For generic base type errors, we use default span since we don't have the specific context
        let error = crate::QangCompilerError::new_type_error(
            message,
            crate::nodes::SourceSpan::default(),
            self.source_map.clone(),
        );
        ctx.errors.report_error(error);
    }

    fn report_type_not_found_error(&mut self, type_id: TypeId, ctx: &mut VisitorContext) {
        let type_info = ctx.nodes.type_table.get_type_info(type_id).unwrap();
        let message = match &type_info.type_node {
            TypeNode::UnresolvedReference { name, .. } => {
                let type_name = self.strings.get_string(*name);
                format!("Type '{}' not found", type_name)
            }
            TypeNode::TypeImport {
                module_path,
                type_name,
            } => {
                let module_path_str = self.strings.get_string(*module_path);
                let type_name_str = self.strings.get_string(*type_name);
                format!(
                    "Type '{}' not found in module '{}'",
                    type_name_str, module_path_str
                )
            }
            TypeNode::Generic(generic_type) => {
                let base_name_str = self.strings.get_string(generic_type.base_name);
                format!(
                    "Generic type '{}' could not be fully resolved",
                    base_name_str
                )
            }
            _ => "Unresolved type".to_string(),
        };

        let span = self.extract_span_from_type(type_id, ctx);
        let error =
            crate::QangCompilerError::new_type_error(message, span, self.source_map.clone());
        ctx.errors.report_error(error);
    }
}

impl<'a> NodeVisitor for TypeResolver<'a> {
    type Error = QangCompilerError;

    fn visit_declaration(
        &mut self,
        decl: super::typed_node_arena::TypedNodeRef<super::nodes::DeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
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
        self.begin_scope();

        // Process generic parameters if present
        if let Some(generic_parameters) = func_expr.node.generic_parameters {
            let param_count = ctx.nodes.array.size(generic_parameters);
            for i in 0..param_count {
                if let Some(param_id) = ctx.nodes.array.get_node_id_at(generic_parameters, i) {
                    let param = ctx.nodes.get_generic_param_node(param_id);
                    self.visit_generic_parameter(param, ctx)?;
                }
            }
        }

        // Process function parameters
        let param_count = ctx.nodes.array.size(func_expr.node.parameters);
        for i in 0..param_count {
            if let Some(param_id) = ctx.nodes.array.get_node_id_at(func_expr.node.parameters, i) {
                let param = ctx.nodes.get_parameter_node(param_id);
                self.visit_parameter(param, ctx)?;
            }
        }

        // Process return type annotation if present
        if let Some(return_type_id) = func_expr.node.return_type {
            self.unresolved_worklist.push(return_type_id);
        }

        // Visit function body
        let body = ctx.nodes.get_block_stmt_node(func_expr.node.body);
        self.visit_block_statement(body, ctx)?;

        self.end_scope(ctx);

        Ok(())
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: super::typed_node_arena::TypedNodeRef<super::nodes::LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        // Process generic parameters if present
        if let Some(generic_parameters) = lambda_expr.node.generic_parameters {
            let param_count = ctx.nodes.array.size(generic_parameters);
            for i in 0..param_count {
                if let Some(param_id) = ctx.nodes.array.get_node_id_at(generic_parameters, i) {
                    let param = ctx.nodes.get_generic_param_node(param_id);
                    self.visit_generic_parameter(param, ctx)?;
                }
            }
        }

        // Process lambda parameters
        let param_count = ctx.nodes.array.size(lambda_expr.node.parameters);
        for i in 0..param_count {
            if let Some(param_id) = ctx
                .nodes
                .array
                .get_node_id_at(lambda_expr.node.parameters, i)
            {
                let param = ctx.nodes.get_parameter_node(param_id);
                self.visit_parameter(param, ctx)?;
            }
        }

        // Visit lambda body
        let body = ctx.nodes.get_lambda_body_node(lambda_expr.node.body);
        self.visit_lambda_body(body, ctx)?;

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
        // Get class name
        let class_name_node = ctx.nodes.get_identifier_node(class_decl.node.name);
        let class_name = class_name_node.node.name;

        // Handle superclass type if present
        let superclass_type = if let Some(superclass_id) = class_decl.node.superclass {
            let superclass_name_node = ctx.nodes.get_identifier_node(superclass_id);
            let superclass_name = superclass_name_node.node.name;

            // Create UnresolvedReference for superclass - will be resolved later
            let superclass_type_info = TypeInfo {
                type_node: TypeNode::UnresolvedReference {
                    name: superclass_name,
                    identifier_node: superclass_id,
                },
                origin: TypeOrigin::Annotation(superclass_id),
            };
            Some(ctx.nodes.type_table.create_type(superclass_type_info))
        } else {
            None
        };

        // Create ClassType and register it
        let class_type_id =
            ctx.nodes
                .type_table
                .create_class_type(class_name, superclass_type, class_decl.id);

        // Declare class type in current scope
        self.scopes
            .get_scope_mut()
            .declare_type(class_name, class_type_id);

        // If at global scope, add to module exports
        if self.scopes.is_global_scope()
            && let Some(module) = self.modules.get_mut(&self.module_path)
        {
            module.exported_types.insert(class_name, class_type_id);
        }

        // Add superclass to worklist if it needs resolution
        if let Some(superclass_type_id) = superclass_type {
            self.unresolved_worklist.push(superclass_type_id);
        }

        self.begin_scope();

        // Process class members
        let member_count = ctx.nodes.array.size(class_decl.node.members);
        for i in 0..member_count {
            if let Some(member_id) = ctx.nodes.array.get_node_id_at(class_decl.node.members, i) {
                let member = ctx.nodes.get_class_member_node(member_id);
                match member.node {
                    super::nodes::ClassMemberNode::Method(method) => {
                        let method_name_node = ctx.nodes.get_identifier_node(method.name);
                        let init_name = self.strings.intern("init");
                        let is_initializer = method_name_node.node.name == init_name;

                        self.begin_scope();

                        // Add method parameters to scope
                        let param_count = ctx.nodes.array.size(method.parameters);
                        for j in 0..param_count {
                            if let Some(param_id) =
                                ctx.nodes.array.get_node_id_at(method.parameters, j)
                            {
                                let param = ctx.nodes.get_parameter_node(param_id);
                                self.visit_parameter(param, ctx)?;
                            }
                        }

                        // Visit method body
                        let body = ctx.nodes.get_block_stmt_node(method.body);
                        self.visit_block_statement(body, ctx)?;

                        self.end_scope(ctx);

                        // Create function type for the method and add to class
                        let method_name = method_name_node.node.name;

                        // Collect parameter types
                        let param_count = ctx.nodes.array.size(method.parameters);
                        let mut param_types = Vec::new();
                        for j in 0..param_count {
                            if let Some(param_id) =
                                ctx.nodes.array.get_node_id_at(method.parameters, j)
                            {
                                let param = ctx.nodes.get_parameter_node(param_id);
                                if let Some(param_type_id) = param.node.parameter_type {
                                    param_types.push(param_type_id);
                                } else {
                                    // If no explicit type, use dynamic type
                                    let dyn_type = TypeInfo {
                                        type_node: TypeNode::DynamicNullable,
                                        origin: TypeOrigin::Builtin,
                                    };
                                    let dyn_type_id = ctx.nodes.type_table.create_type(dyn_type);
                                    param_types.push(dyn_type_id);
                                }
                            }
                        }

                        // Get return type
                        let return_type = if let Some(return_type_id) = method.return_type {
                            return_type_id
                        } else {
                            // If no explicit return type, use dynamic type
                            let dyn_type = TypeInfo {
                                type_node: TypeNode::DynamicNullable,
                                origin: TypeOrigin::Builtin,
                            };
                            ctx.nodes.type_table.create_type(dyn_type)
                        };

                        // Create function type for the method
                        let method_function_type_id = ctx.nodes.type_table.create_function_type(
                            param_types,
                            return_type,
                            member_id,
                        );

                        // Add method to class type with proper is_initializer flag
                        if let Err(e) = ctx.nodes.type_table.add_class_method(
                            class_type_id,
                            method_name,
                            method_function_type_id,
                            is_initializer,
                        ) {
                            // Log error but continue processing
                            eprintln!("Failed to add method to class: {}", e);
                        }
                    }
                    super::nodes::ClassMemberNode::Field(field) => {
                        // Get field name
                        let field_name_node = ctx.nodes.get_identifier_node(field.name);
                        let field_name = field_name_node.node.name;

                        // Get field type
                        let field_type = if let Some(field_type_id) = field.field_type {
                            // Add to worklist for resolution
                            self.unresolved_worklist.push(field_type_id);
                            field_type_id
                        } else {
                            // If no explicit type, use dynamic type
                            let dyn_type = TypeInfo {
                                type_node: TypeNode::DynamicNullable,
                                origin: TypeOrigin::Builtin,
                            };
                            ctx.nodes.type_table.create_type(dyn_type)
                        };

                        // Add field to class type
                        if let Err(e) = ctx.nodes.type_table.add_class_field(
                            class_type_id,
                            field_name,
                            field_type,
                        ) {
                            // Log error but continue processing
                            eprintln!("Failed to add field to class: {}", e);
                        }

                        // Visit field initializer if present
                        if let Some(init_id) = field.initializer {
                            let initializer = ctx.nodes.get_expr_node(init_id);
                            self.visit_expression(initializer, ctx)?;
                        }
                    }
                }
            }
        }

        self.end_scope(ctx);

        Ok(())
    }

    fn visit_import_module_declaration(
        &mut self,
        import_decl: super::typed_node_arena::TypedNodeRef<super::nodes::ImportModuleDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Get module name and path
        let module_name_node = ctx.nodes.get_identifier_node(import_decl.node.name);
        let module_name = module_name_node.node.name;
        let module_path = import_decl.node.path;

        // Convert string handle to PathBuf
        let module_path_str = self.strings.get_string(module_path);
        let module_path_buf = std::path::PathBuf::from(module_path_str);

        // Verify module exists and create a module type for this import
        if self.modules.has(&module_path_buf) {
            // Create a module type for this import
            let module_type_id = ctx
                .nodes
                .type_table
                .create_module_type(module_name, import_decl.id);

            // Register module in current scope
            self.scopes
                .get_scope_mut()
                .declare_type(module_name, module_type_id);

            // Note: Module types themselves don't need resolution, but their exported types
            // will be resolved when accessed via TypeImport nodes
        } else {
            // Report error if module not found (should be rare since modules are pre-parsed)
            let message = format!("Module '{}' not found during import", module_path_str);
            let span = ctx.nodes.get_node(import_decl.id).span();
            let error =
                crate::QangCompilerError::new_type_error(message, span, self.source_map.clone());
            ctx.errors.report_error(error);
        }

        Ok(())
    }

    fn visit_generic_parameter(
        &mut self,
        generic_param: super::typed_node_arena::TypedNodeRef<super::nodes::GenericParameterNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Get generic parameter name
        let param_name_node = ctx.nodes.get_identifier_node(generic_param.node.name);
        let param_name = param_name_node.node.name;

        // Create type parameter and register it in current scope
        let type_param_info = if let Some(constraint_type_id) = generic_param.node.constraint {
            // Add constraint to worklist for resolution
            self.unresolved_worklist.push(constraint_type_id);

            TypeInfo {
                type_node: TypeNode::ConstrainedTypeParameter {
                    name: param_name,
                    constraint: constraint_type_id,
                },
                origin: TypeOrigin::Annotation(generic_param.id),
            }
        } else {
            TypeInfo {
                type_node: TypeNode::TypeParameter(param_name),
                origin: TypeOrigin::Annotation(generic_param.id),
            }
        };

        let type_param_id = ctx.nodes.type_table.create_type(type_param_info);

        // Register type parameter in current scope
        self.scopes
            .get_scope_mut()
            .declare_type(param_name, type_param_id);

        Ok(())
    }

    fn visit_parameter(
        &mut self,
        parameter: super::typed_node_arena::TypedNodeRef<super::nodes::ParameterNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Process parameter type annotation if present
        if let Some(param_type_id) = parameter.node.parameter_type {
            // Add parameter type to worklist for resolution
            self.unresolved_worklist.push(param_type_id);
        }

        Ok(())
    }

    fn visit_type_alias_declaration(
        &mut self,
        type_decl: super::typed_node_arena::TypedNodeRef<super::nodes::TypeAliasDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Get type alias name
        let alias_name_node = ctx.nodes.get_identifier_node(type_decl.node.name);
        let alias_name = alias_name_node.node.name;

        // Get the type definition being aliased
        let type_definition_id = type_decl.node.type_definition;

        // Register type alias in current scope
        self.scopes
            .get_scope_mut()
            .declare_type(alias_name, type_definition_id);

        // If at global scope, add to module exports
        if self.scopes.is_global_scope()
            && let Some(module) = self.modules.get_mut(&self.module_path)
        {
            module.exported_types.insert(alias_name, type_definition_id);
        }

        // Add the aliased type to the worklist for resolution
        self.unresolved_worklist.push(type_definition_id);

        // Process generic parameters if present
        if let Some(generic_parameters) = type_decl.node.generic_parameters {
            let param_count = ctx.nodes.array.size(generic_parameters);
            for i in 0..param_count {
                if let Some(param_id) = ctx.nodes.array.get_node_id_at(generic_parameters, i) {
                    let param = ctx.nodes.get_generic_param_node(param_id);
                    self.visit_generic_parameter(param, ctx)?;
                }
            }
        }

        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        var_decl: super::typed_node_arena::TypedNodeRef<super::nodes::VariableDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Process variable type annotation if present
        if let Some(var_type_id) = var_decl.node.variable_type {
            // Add variable type to worklist for resolution
            self.unresolved_worklist.push(var_type_id);
        }

        // Visit initializer expression if present
        if let Some(init_id) = var_decl.node.initializer {
            let initializer = ctx.nodes.get_expr_node(init_id);
            self.visit_expression(initializer, ctx)?;
        }

        Ok(())
    }

    fn visit_type_cast_expression(
        &mut self,
        type_cast: super::typed_node_arena::TypedNodeRef<super::nodes::TypeCastExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Visit the expression being cast
        let expr = ctx.nodes.get_expr_node(type_cast.node.expr);
        self.visit_expression(expr, ctx)?;

        // The target type of the cast is stored in the TypeTable via set_node_type
        // We need to get it and add to worklist for resolution
        if let Some(cast_target_type_id) = ctx.nodes.type_table.get_node_type(type_cast.id) {
            self.unresolved_worklist.push(cast_target_type_id);
        }

        Ok(())
    }
}
