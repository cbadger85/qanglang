// crates/qanglang-core/src/frontend/type_inference.rs

use crate::{
    ErrorReporter, NodeId, QangCompilerError, StringHandle, TypedNodeArena,
    frontend::{
        node_visitor::{NodeVisitor, VisitorContext},
        nodes::*,
        source::ModuleMap,
        type_arena::{TypeArena, TypeInfo},
        typed_node_arena::{DeclNode, TypedNodeRef},
        types::{QangType, TypeId},
    },
    get_typed_node,
    memory::StringInterner,
};
use std::collections::HashMap;

/// Scope for tracking variable types
#[derive(Debug, Clone)]
struct TypeScope {
    variables: HashMap<crate::StringHandle, TypeId>,
    parent: Option<Box<TypeScope>>,
}

impl TypeScope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: TypeScope) -> Self {
        Self {
            variables: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    fn declare(&mut self, name: crate::StringHandle, ty: TypeId) {
        self.variables.insert(name, ty);
    }

    fn lookup(&self, name: crate::StringHandle) -> Option<TypeId> {
        self.variables
            .get(&name)
            .copied()
            .or_else(|| self.parent.as_ref().and_then(|p| p.lookup(name)))
    }
}

pub struct TypeInferenceEngine<'a> {
    strings: &'a StringInterner,
    current_scope: TypeScope,
    /// Stack of function return types for return statement checking
    function_stack: Vec<TypeId>,
    /// Current function being analyzed (for tail call detection)
    current_function: Option<NodeId>,
    /// Available modules for this analysis
    modules: Option<&'a ModuleMap>,
    /// All return types encountered in current function
    return_types_seen: Vec<TypeId>,
    /// Whether we've seen a nil return
    has_nil_return: bool,
}

impl<'a> TypeInferenceEngine<'a> {
    pub fn new(strings: &'a StringInterner) -> Self {
        Self {
            strings,
            current_scope: TypeScope::new(),
            function_stack: Vec::new(),
            current_function: None,
            modules: None,
            return_types_seen: Vec::new(),
            has_nil_return: false,
        }
    }

    pub fn infer_types(
        mut self,
        program: NodeId,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) {
        let mut ctx = VisitorContext::new(nodes, errors);
        let program_node = ctx.nodes.get_module_node(program);

        let _ = self.visit_module(program_node, &mut ctx);
    }

    pub fn infer_types_with_modules(
        mut self,
        program: NodeId,
        modules: &'a ModuleMap,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) {
        self.modules = Some(modules);
        let mut ctx = VisitorContext::new(nodes, errors);
        let program_node = ctx.nodes.get_module_node(program);

        let _ = self.visit_module(program_node, &mut ctx);
    }

    fn begin_scope(&mut self) {
        let parent = std::mem::replace(&mut self.current_scope, TypeScope::new());
        self.current_scope = TypeScope::with_parent(parent);
    }

    fn end_scope(&mut self) {
        if let Some(parent) = self.current_scope.parent.take() {
            self.current_scope = *parent;
        }
    }

    fn begin_function(&mut self, return_type: TypeId, function_node: NodeId) {
        self.function_stack.push(return_type);
        self.current_function = Some(function_node);
    }

    fn end_function(&mut self) {
        self.function_stack.pop();
        self.current_function = None;
    }

    fn current_function_return_type(&self) -> Option<TypeId> {
        self.function_stack.last().copied()
    }

    /// Check if two types are compatible for assignment
    fn are_types_compatible(&self, target: TypeId, source: TypeId, type_arena: &TypeArena) -> bool {
        if target == source {
            return true;
        }

        // Special cases for null safety
        match (type_arena.get_type(target), type_arena.get_type(source)) {
            // Can assign non-optional to optional
            (QangType::Optional(inner), _) if *inner == source => true,
            // Can assign anything to unknown (during inference)
            (QangType::Unknown, _) | (_, QangType::Unknown) => true,
            // Unit types are compatible
            (QangType::Unit, QangType::Unit) => true,
            _ => false,
        }
    }

    /// Detect if a call expression is a tail call
    fn is_tail_call(&self, call_expr: NodeId, ctx: &VisitorContext) -> bool {
        if self.current_function.is_none() {
            return false;
        }

        let call = get_typed_node!(ctx.nodes, call_expr, AstNode::CallExpr);
        let callee_type = ctx.nodes.get_node_type_id(call.node.callee);

        ctx.nodes.types.is_function(callee_type) && !ctx.nodes.types.is_class(callee_type)
    }

    /// Analyze a module and infer its exported types
    fn analyze_imported_module(
        &mut self,
        module_path: StringHandle,
        ctx: &mut VisitorContext,
    ) -> TypeId {
        if let Some(modules) = self.modules {
            let module_node = *modules
                .nodes
                .get(&module_path)
                .expect("Expected module but found none.");

            let exports = self.extract_module_exports(module_node, ctx);

            return ctx
                .nodes
                .types
                .make_module(module_path, exports, module_node);
        }

        panic!("Expected modules but found none.")
    }

    /// Extract exports from a parsed module
    fn extract_module_exports(
        &mut self,
        module_node: NodeId,
        ctx: &mut VisitorContext,
    ) -> Vec<(crate::StringHandle, TypeId)> {
        let mut exports = Vec::new();

        let module = ctx.nodes.get_module_node(module_node);
        let decl_count = ctx.nodes.array.size(module.node.decls);

        for i in 0..decl_count {
            if let Some(decl_id) = ctx.nodes.array.get_node_id_at(module.node.decls, i) {
                let decl = ctx.nodes.get_decl_node(decl_id);

                match decl.node {
                    DeclNode::Function(func_decl) => {
                        let func_expr = ctx.nodes.get_func_expr_node(func_decl.function);
                        let name_node = ctx.nodes.get_identifier_node(func_expr.node.name);

                        let param_count = ctx.nodes.array.size(func_expr.node.parameters);
                        let param_types = vec![TypeArena::UNKNOWN; param_count];
                        let func_type = ctx
                            .nodes
                            .types
                            .make_function(param_types, TypeArena::UNKNOWN);

                        exports.push((name_node.node.name, func_type));
                    }
                    DeclNode::Variable(var_decl) => {
                        let identifier = ctx.nodes.get_identifier_node(var_decl.target);
                        exports.push((identifier.node.name, TypeArena::UNKNOWN));
                    }
                    DeclNode::Class(class_decl) => {
                        let name_node = ctx.nodes.get_identifier_node(class_decl.name);
                        let class_type = ctx.nodes.types.make_class(name_node.node.name, decl_id);

                        exports.push((name_node.node.name, class_type));
                    }
                    DeclNode::Module(import_decl) => {
                        let name_node = ctx.nodes.get_identifier_node(import_decl.name);
                        exports.push((name_node.node.name, TypeArena::UNKNOWN));
                    }
                    _ => {}
                }
            }
        }

        exports
    }

    fn handle_map_operation(
        &mut self,
        operation: TypedNodeRef<super::typed_node_arena::CallOperationNode>,
        operand_type: TypeId,
        ctx: &mut VisitorContext,
    ) -> Result<TypeId, QangCompilerError> {
        if let super::typed_node_arena::CallOperationNode::Map(map_expr) = operation.node {
            self.begin_scope();

            let param = ctx.nodes.get_identifier_node(map_expr.parameter);
            self.current_scope.declare(param.node.name, operand_type);
            ctx.nodes
                .set_node_type(param.id, TypeInfo::new(operand_type));

            let body = ctx.nodes.get_expr_node(map_expr.body);
            self.visit_expression(body, ctx)?;

            self.end_scope();

            let body_type = ctx.nodes.get_node_type_id(map_expr.body);
            let result_type = if ctx.nodes.types.is_optional(operand_type) {
                ctx.nodes.types.make_optional(body_type)
            } else {
                body_type
            };

            ctx.nodes
                .set_node_type(operation.id, TypeInfo::new(result_type));
            Ok(result_type)
        } else {
            Err(QangCompilerError::new_analysis_error(
                "Expected map operation".to_string(),
                SourceSpan::default(),
            ))
        }
    }

    fn handle_optional_map_operation(
        &mut self,
        operation: TypedNodeRef<super::typed_node_arena::CallOperationNode>,
        operand_type: TypeId,
        ctx: &mut VisitorContext,
    ) -> Result<TypeId, QangCompilerError> {
        if let super::typed_node_arena::CallOperationNode::OptionalMap(opt_map_expr) =
            operation.node
        {
            self.begin_scope();

            let param_type = if ctx.nodes.types.is_optional(operand_type) {
                ctx.nodes.types.unwrap_optional(operand_type)
            } else {
                operand_type
            };

            let param = ctx.nodes.get_identifier_node(opt_map_expr.parameter);
            self.current_scope.declare(param.node.name, param_type);
            ctx.nodes.set_node_type(param.id, TypeInfo::new(param_type));

            let body = ctx.nodes.get_expr_node(opt_map_expr.body);
            self.visit_expression(body, ctx)?;

            self.end_scope();

            let body_type = ctx.nodes.get_node_type_id(opt_map_expr.body);
            let result_type = ctx.nodes.types.make_optional(body_type);

            ctx.nodes
                .set_node_type(operation.id, TypeInfo::new(result_type));
            Ok(result_type)
        } else {
            Err(QangCompilerError::new_analysis_error(
                "Expected optional map operation".to_string(),
                SourceSpan::default(),
            ))
        }
    }

    fn track_return_type(&mut self, return_type: TypeId) {
        // In a full implementation, you'd track all return types
        // and unify them to get the function's actual return type
        // For now, we just update the current function's expected return type
        if let Some(current_return) = self.function_stack.last_mut() {
            if *current_return == TypeArena::UNKNOWN {
                *current_return = return_type;
            }
            // Could add logic here to unify multiple return types
        }
    }

    /// Unify two return types, handling nil specially
    fn unify_return_types(&self, type1: TypeId, type2: TypeId, type_arena: &TypeArena) -> TypeId {
        if type1 == type2 {
            return type1;
        }

        // If either is unknown, use the other
        if type1 == TypeArena::UNKNOWN {
            return type2;
        }
        if type2 == TypeArena::UNKNOWN {
            return type1;
        }

        // Handle nil returns specially
        let type1_is_nil_optional = matches!(type_arena.get_type(type1), QangType::Optional(inner) if *inner == TypeArena::UNKNOWN);
        let type2_is_nil_optional = matches!(type_arena.get_type(type2), QangType::Optional(inner) if *inner == TypeArena::UNKNOWN);

        match (type1_is_nil_optional, type2_is_nil_optional) {
            // nil + concrete type = Optional<concrete type>
            (true, false) => {
                // type1 is nil, type2 is concrete - result is Optional<type2>
                if type_arena.is_optional(type2) {
                    type2 // Already optional
                } else {
                    // Make type2 optional - but we need mutable access to type_arena
                    // This is a design issue - we'd need to restructure to handle this properly
                    TypeArena::UNKNOWN // Placeholder - see below for better approach
                }
            }
            (false, true) => {
                // type2 is nil, type1 is concrete - result is Optional<type1>
                if type_arena.is_optional(type1) {
                    type1
                } else {
                    TypeArena::UNKNOWN // Placeholder
                }
            }
            (true, true) => {
                // Both are nil - result is nil (Optional<Unknown>)
                type1
            }
            (false, false) => {
                // Neither is nil - they should be compatible or it's an error
                // For now, return unknown to indicate error
                TypeArena::UNKNOWN
            }
        }
    }

    /// Track return type with nil handling
    fn track_return_type_with_nil(&mut self, return_type: TypeId, ctx: &mut VisitorContext) {
        // Get current function's return type
        if let Some(current_return) = self.function_stack.last_mut() {
            if *current_return == TypeArena::UNKNOWN {
                // First return - just set it
                *current_return = return_type;
            } else {
                // Subsequent return - unify with previous
                let unified = self.unify_return_types_with_arena(*current_return, return_type, ctx);
                *current_return = unified;
            }
        }
    }

    /// Unify return types with mutable arena access
    fn unify_return_types_with_arena(
        &mut self,
        type1: TypeId,
        type2: TypeId,
        ctx: &mut VisitorContext,
    ) -> TypeId {
        if type1 == type2 {
            return type1;
        }

        if type1 == TypeArena::UNKNOWN {
            return type2;
        }
        if type2 == TypeArena::UNKNOWN {
            return type1;
        }

        let type_arena = &ctx.nodes.types;

        // Check if either type represents nil (Optional<Unknown>)
        let type1_is_nil = matches!(type_arena.get_type(type1), QangType::Optional(inner) if *inner == TypeArena::UNKNOWN);
        let type2_is_nil = matches!(type_arena.get_type(type2), QangType::Optional(inner) if *inner == TypeArena::UNKNOWN);

        match (type1_is_nil, type2_is_nil) {
            (true, false) => {
                // nil + concrete type = Optional<concrete_type>
                if type_arena.is_optional(type2) {
                    type2 // Already optional
                } else {
                    // Make concrete type optional
                    ctx.nodes.types.make_optional(type2)
                }
            }
            (false, true) => {
                // concrete type + nil = Optional<concrete_type>
                if type_arena.is_optional(type1) {
                    type1 // Already optional
                } else {
                    ctx.nodes.types.make_optional(type1)
                }
            }
            (true, true) => {
                // nil + nil = nil
                type1
            }
            (false, false) => {
                // Two different concrete types - this is an error, but we'll try to make them compatible
                if type_arena.is_optional(type1) && !type_arena.is_optional(type2) {
                    // Optional<T1> + T2 - check if T2 is compatible with T1's inner type
                    let inner1 = type_arena.unwrap_optional(type1);
                    if self.are_types_compatible(inner1, type2, type_arena) {
                        type1 // Keep the optional
                    } else {
                        TypeArena::UNKNOWN // Incompatible types
                    }
                } else if !type_arena.is_optional(type1) && type_arena.is_optional(type2) {
                    let inner2 = type_arena.unwrap_optional(type2);
                    if self.are_types_compatible(type1, inner2, type_arena) {
                        type2 // Keep the optional
                    } else {
                        TypeArena::UNKNOWN
                    }
                } else {
                    // Both concrete and incompatible
                    TypeArena::UNKNOWN
                }
            }
        }
    }
}

impl<'a> NodeVisitor for TypeInferenceEngine<'a> {
    type Error = QangCompilerError;

    fn visit_return_statement(
        &mut self,
        return_stmt: TypedNodeRef<ReturnStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let return_type = if let Some(value_id) = return_stmt.node.value {
            let value = ctx.nodes.get_expr_node(value_id);
            self.visit_expression(value, ctx)?;
            ctx.nodes.get_node_type_id(value_id)
        } else {
            TypeArena::UNIT
        };

        // Track this return type for function inference
        self.track_return_type(return_type);

        // Check compatibility with previously inferred return type
        if let Some(expected_return_type) = self.current_function_return_type() {
            if expected_return_type != TypeArena::UNKNOWN
                && !self.are_types_compatible(expected_return_type, return_type, &ctx.nodes.types)
            {
                ctx.errors
                    .report_error(QangCompilerError::new_analysis_error(
                        "Inconsistent return types in function".to_string(),
                        return_stmt.node.span,
                    ));
            }
        }

        ctx.nodes
            .set_node_type(return_stmt.id, TypeInfo::new(TypeArena::UNIT));
        Ok(())
    }

    fn visit_number_literal(
        &mut self,
        number: TypedNodeRef<NumberLiteralNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        ctx.nodes
            .set_node_type(number.id, TypeInfo::new(TypeArena::NUMBER));
        Ok(())
    }

    fn visit_string_literal(
        &mut self,
        string: TypedNodeRef<StringLiteralNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        ctx.nodes
            .set_node_type(string.id, TypeInfo::new(TypeArena::STRING));
        Ok(())
    }

    fn visit_boolean_literal(
        &mut self,
        boolean: TypedNodeRef<BooleanLiteralNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        ctx.nodes
            .set_node_type(boolean.id, TypeInfo::new(TypeArena::BOOLEAN));
        Ok(())
    }

    fn visit_nil_literal(
        &mut self,
        nil: TypedNodeRef<NilLiteralNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let nil_type = ctx.nodes.types.make_optional(TypeArena::UNKNOWN);
        ctx.nodes
            .set_node_type(nil.id, TypeInfo::new_nullable(nil_type));
        Ok(())
    }

    fn visit_identifier(
        &mut self,
        identifier: TypedNodeRef<IdentifierNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if let Some(var_type) = self.current_scope.lookup(identifier.node.name) {
            ctx.nodes
                .set_node_type(identifier.id, TypeInfo::new(var_type));
        } else {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    format!(
                        "Undefined variable '{}'",
                        self.strings.get_string(identifier.node.name)
                    ),
                    identifier.node.span,
                ));
            ctx.nodes
                .set_node_type(identifier.id, TypeInfo::new(TypeArena::UNKNOWN));
        }
        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        var_decl: TypedNodeRef<VariableDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(var_decl.node.target);

        let var_type = if let Some(initializer_id) = var_decl.node.initializer {
            let initializer = ctx.nodes.get_expr_node(initializer_id);
            self.visit_expression(initializer, ctx)?;
            ctx.nodes.get_node_type_id(initializer_id)
        } else {
            TypeArena::UNKNOWN
        };

        self.current_scope.declare(identifier.node.name, var_type);
        ctx.nodes
            .set_node_type(identifier.id, TypeInfo::new(var_type));

        Ok(())
    }

    fn visit_lambda_declaration(
        &mut self,
        lambda_decl: TypedNodeRef<LambdaDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(lambda_decl.node.name);
        let lambda_expr = ctx.nodes.get_lambda_expr_node(lambda_decl.node.lambda);

        self.visit_lambda_expression(lambda_expr, ctx)?;
        let lambda_type = ctx.nodes.get_node_type_id(lambda_decl.node.lambda);

        self.current_scope
            .declare(identifier.node.name, lambda_type);
        ctx.nodes
            .set_node_type(identifier.id, TypeInfo::new(lambda_type));
        ctx.nodes
            .set_node_type(lambda_decl.id, TypeInfo::new(lambda_type));

        Ok(())
    }

    fn visit_import_module_declaration(
        &mut self,
        import_decl: TypedNodeRef<ImportModuleDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(import_decl.node.name);

        let module_type = self.analyze_imported_module(import_decl.node.path, ctx);

        self.current_scope
            .declare(identifier.node.name, module_type);
        ctx.nodes
            .set_node_type(identifier.id, TypeInfo::new(module_type));

        Ok(())
    }

    fn visit_assignment_expression(
        &mut self,
        assignment: TypedNodeRef<AssignmentExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let value = ctx.nodes.get_expr_node(assignment.node.value);
        self.visit_expression(value, ctx)?;

        let target = ctx.nodes.get_assignment_target_node(assignment.node.target);
        self.visit_assignment_target(target, ctx)?;

        let value_type = ctx.nodes.get_node_type_id(assignment.node.value);
        let target_type = ctx.nodes.get_node_type_id(assignment.node.target);

        if !self.are_types_compatible(target_type, value_type, &ctx.nodes.types) {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Type mismatch in assignment".to_string(),
                    assignment.node.span,
                ));
        }

        ctx.nodes
            .set_node_type(assignment.id, TypeInfo::new(value_type));

        Ok(())
    }

    fn visit_pipe_expression(
        &mut self,
        pipe: TypedNodeRef<PipeExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Pipe operator: left |> right (partial application)
        let left = ctx.nodes.get_expr_node(pipe.node.left);
        self.visit_expression(left, ctx)?;
        let left_type = ctx.nodes.get_node_type_id(pipe.node.left);

        let right = ctx.nodes.get_expr_node(pipe.node.right);
        self.visit_expression(right, ctx)?;
        let right_type = ctx.nodes.get_node_type_id(pipe.node.right);

        let result_type = match ctx.nodes.types.get_type(right_type) {
            QangType::Function {
                params,
                return_type,
            } => {
                if params.is_empty() {
                    ctx.errors
                        .report_error(QangCompilerError::new_analysis_error(
                            "Cannot pipe to function with no parameters".to_string(),
                            pipe.node.span,
                        ));
                    TypeArena::UNKNOWN
                } else {
                    let first_param_type = params[0];
                    if !self.are_types_compatible(first_param_type, left_type, &ctx.nodes.types) {
                        ctx.errors
                            .report_error(QangCompilerError::new_analysis_error(
                                "Type mismatch in pipe operation".to_string(),
                                pipe.node.span,
                            ));
                    }

                    if params.len() > 1 {
                        // Partial application - create new function type
                        let remaining_params = params[1..].to_vec();
                        ctx.nodes
                            .types
                            .make_function(remaining_params, *return_type)
                    } else {
                        // Fully applied
                        *return_type
                    }
                }
            }
            _ => {
                ctx.errors
                    .report_error(QangCompilerError::new_analysis_error(
                        "Right side of pipe must be a function".to_string(),
                        pipe.node.span,
                    ));
                TypeArena::UNKNOWN
            }
        };

        ctx.nodes.set_node_type(pipe.id, TypeInfo::new(result_type));

        Ok(())
    }

    fn visit_call_expression(
        &mut self,
        call: TypedNodeRef<CallExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let callee = ctx.nodes.get_expr_node(call.node.callee);
        self.visit_expression(callee, ctx)?;
        let callee_type = ctx.nodes.get_node_type_id(call.node.callee);

        let operation = ctx.nodes.get_call_operation_node(call.node.operation);
        let result_type = match operation.node {
            super::typed_node_arena::CallOperationNode::Property(property) => {
                let property_identifier = ctx.nodes.get_identifier_node(property.identifier);

                match ctx.nodes.types.get_type(callee_type) {
                    QangType::Module { .. } => ctx
                        .nodes
                        .types
                        .lookup_module_export(callee_type, property_identifier.node.name)
                        .unwrap_or(TypeArena::UNKNOWN),
                    QangType::Object { fields } => fields
                        .iter()
                        .find(|(name, _)| *name == property_identifier.node.name)
                        .map(|(_, ty)| *ty)
                        .unwrap_or(TypeArena::UNKNOWN),
                    QangType::Class { .. } => TypeArena::UNKNOWN,
                    _ => {
                        ctx.errors
                            .report_error(QangCompilerError::new_analysis_error(
                                "Cannot access property on non-object type".to_string(),
                                call.node.span,
                            ));
                        TypeArena::UNKNOWN
                    }
                }
            }
            super::typed_node_arena::CallOperationNode::Call(_) => {
                match ctx.nodes.types.get_type(callee_type) {
                    QangType::Function { return_type, .. } => *return_type,
                    QangType::Class { .. } => callee_type,
                    _ => {
                        ctx.errors
                            .report_error(QangCompilerError::new_analysis_error(
                                "Cannot call non-function value".to_string(),
                                call.node.span,
                            ));
                        TypeArena::UNKNOWN
                    }
                }
            }
            super::typed_node_arena::CallOperationNode::Map(_) => {
                self.handle_map_operation(operation, callee_type, ctx)?
            }
            super::typed_node_arena::CallOperationNode::OptionalMap(_) => {
                self.handle_optional_map_operation(operation, callee_type, ctx)?
            }
            _ => {
                self.visit_call_operation(operation, ctx)?;
                TypeArena::UNKNOWN
            }
        };

        if !matches!(
            operation.node,
            super::typed_node_arena::CallOperationNode::Map(_)
                | super::typed_node_arena::CallOperationNode::OptionalMap(_)
        ) {
            self.visit_call_operation(operation, ctx)?;
        }

        ctx.nodes.set_node_type(call.id, TypeInfo::new(result_type));

        Ok(())
    }

    fn visit_function_expression(
        &mut self,
        func_expr: TypedNodeRef<FunctionExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        let mut param_types = Vec::new();
        let param_count = ctx.nodes.array.size(func_expr.node.parameters);

        for i in 0..param_count {
            if let Some(param_id) = ctx.nodes.array.get_node_id_at(func_expr.node.parameters, i) {
                let param = ctx.nodes.get_identifier_node(param_id);

                let param_type = TypeArena::UNKNOWN;
                param_types.push(param_type);

                self.current_scope.declare(param.node.name, param_type);
                ctx.nodes.set_node_type(param.id, TypeInfo::new(param_type));
            }
        }

        // Start with unknown return type - will be inferred from return statements
        let return_type = TypeArena::UNKNOWN;
        self.begin_function(return_type, func_expr.id);

        let body = ctx.nodes.get_block_stmt_node(func_expr.node.body);
        self.visit_block_statement(body, ctx)?;

        // After visiting body, get the inferred return type
        // For now, we'll use UNKNOWN, but this could be enhanced to track actual returns
        let inferred_return_type = self
            .current_function_return_type()
            .unwrap_or(TypeArena::UNIT);

        self.end_function();
        self.end_scope();

        let func_type = ctx
            .nodes
            .types
            .make_function(param_types, inferred_return_type);
        ctx.nodes
            .set_node_type(func_expr.id, TypeInfo::new(func_type));

        Ok(())
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: TypedNodeRef<LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        let mut param_types = Vec::new();
        let param_count = ctx.nodes.array.size(lambda_expr.node.parameters);

        for i in 0..param_count {
            if let Some(param_id) = ctx
                .nodes
                .array
                .get_node_id_at(lambda_expr.node.parameters, i)
            {
                let param = ctx.nodes.get_identifier_node(param_id);

                let param_type = TypeArena::UNKNOWN;
                param_types.push(param_type);

                self.current_scope.declare(param.node.name, param_type);
                ctx.nodes.set_node_type(param.id, TypeInfo::new(param_type));
            }
        }

        let body = ctx.nodes.get_lambda_body_node(lambda_expr.node.body);

        let return_type = match body.node {
            super::typed_node_arena::LambdaBodyNode::Block(_) => {
                // For block bodies, track return statements
                self.begin_function(TypeArena::UNKNOWN, lambda_expr.id);
                self.visit_lambda_body(body, ctx)?;
                let inferred_return = self
                    .current_function_return_type()
                    .unwrap_or(TypeArena::UNIT);
                self.end_function();
                inferred_return
            }
            super::typed_node_arena::LambdaBodyNode::Expr(_) => {
                // Expression body - return type is the expression type
                self.visit_lambda_body(body, ctx)?;
                ctx.nodes.get_node_type_id(lambda_expr.node.body)
            }
        };

        self.end_scope();

        let lambda_type = ctx.nodes.types.make_function(param_types, return_type);
        ctx.nodes
            .set_node_type(lambda_expr.id, TypeInfo::new(lambda_type));

        Ok(())
    }

    fn visit_lambda_body(
        &mut self,
        body: TypedNodeRef<super::typed_node_arena::LambdaBodyNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match body.node {
            super::typed_node_arena::LambdaBodyNode::Block(block) => {
                let block_node = TypedNodeRef::new(body.id, block);
                self.visit_block_statement(block_node, ctx)
            }
            super::typed_node_arena::LambdaBodyNode::Expr(expr) => {
                let expr_node = TypedNodeRef::new(body.id, expr);
                self.visit_expression(expr_node, ctx)
            }
        }
    }

    fn visit_block_statement(
        &mut self,
        block_stmt: TypedNodeRef<BlockStmtNode>,
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

        self.end_scope();

        ctx.nodes
            .set_node_type(block_stmt.id, TypeInfo::new(TypeArena::UNIT));

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_scope() {
        let mut scope = TypeScope::new();
        let name = 0;

        scope.declare(name, TypeArena::NUMBER);
        assert_eq!(scope.lookup(name), Some(TypeArena::NUMBER));

        let child = TypeScope::with_parent(scope);
        assert_eq!(child.lookup(name), Some(TypeArena::NUMBER));
    }
}
