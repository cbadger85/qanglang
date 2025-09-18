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
    strings: &'a mut StringInterner,
    current_scope: TypeScope,
    /// Stack of function return types for return statement checking
    function_stack: Vec<TypeId>,
    /// Current function being analyzed (for tail call detection)
    current_function: Option<NodeId>,
    /// Available modules for this analysis
    modules: Option<&'a ModuleMap>,
    /// Current scope depth (0 = global scope)
    scope_depth: usize,
}

impl<'a> TypeInferenceEngine<'a> {
    pub fn new(strings: &'a mut StringInterner) -> Self {
        let engine = Self {
            strings,
            current_scope: TypeScope::new(),
            function_stack: Vec::new(),
            current_function: None,
            modules: None,
            scope_depth: 0,
        };

        engine
    }

    pub fn with_native_types(mut self, type_arena: &mut TypeArena) -> Self {
        self.inject_native_function_types(type_arena);
        self
    }

    fn inject_native_function_types(&mut self, type_arena: &mut TypeArena) {
        // Create array type for use in function signatures
        let array_type = type_arena.make_array(TypeArena::UNKNOWN);

        // Pre-create optional string type to avoid multiple mutable borrows
        let optional_string = type_arena.make_optional(TypeArena::STRING);

        // Global native functions
        let assert_handle = self.strings.intern("assert");
        let assert_type =
            type_arena.make_function(vec![TypeArena::BOOLEAN, optional_string], TypeArena::UNIT);
        self.current_scope.declare(assert_handle, assert_type);

        let assert_eq_handle = self.strings.intern("assert_eq");
        let assert_eq_type = type_arena.make_function(
            vec![TypeArena::UNKNOWN, TypeArena::UNKNOWN, optional_string],
            TypeArena::UNIT,
        );
        self.current_scope.declare(assert_eq_handle, assert_eq_type);

        let assert_throws_handle = self.strings.intern("assert_throws");
        let function_type = type_arena.make_function(vec![], TypeArena::UNKNOWN);
        let assert_throws_type =
            type_arena.make_function(vec![function_type, optional_string], TypeArena::UNIT);
        self.current_scope
            .declare(assert_throws_handle, assert_throws_type);

        let print_handle = self.strings.intern("print");
        let print_type = type_arena.make_function(vec![TypeArena::UNKNOWN], TypeArena::UNIT);
        self.current_scope.declare(print_handle, print_type);

        let println_handle = self.strings.intern("println");
        let println_type = type_arena.make_function(vec![TypeArena::UNKNOWN], TypeArena::UNIT);
        self.current_scope.declare(println_handle, println_type);

        let typeof_handle = self.strings.intern("typeof");
        let typeof_type = type_arena.make_function(vec![TypeArena::UNKNOWN], TypeArena::STRING);
        self.current_scope.declare(typeof_handle, typeof_type);

        let system_time_handle = self.strings.intern("system_time");
        let system_time_type = type_arena.make_function(vec![], TypeArena::NUMBER);
        self.current_scope
            .declare(system_time_handle, system_time_type);

        let to_string_handle = self.strings.intern("to_string");
        let to_string_type = type_arena.make_function(vec![TypeArena::UNKNOWN], TypeArena::STRING);
        self.current_scope.declare(to_string_handle, to_string_type);

        let hash_handle = self.strings.intern("hash");
        let hash_type = type_arena.make_function(vec![TypeArena::UNKNOWN], TypeArena::NUMBER);
        self.current_scope.declare(hash_handle, hash_type);

        let array_of_length_handle = self.strings.intern("array_of_length");
        let array_of_length_type = type_arena.make_function(vec![TypeArena::NUMBER], array_type);
        self.current_scope
            .declare(array_of_length_handle, array_of_length_type);

        let array_of_length_handle = self.strings.intern("array_of");
        let item_creator = type_arena.create_type(QangType::Function {
            params: vec![TypeArena::NUMBER],
            return_type: TypeArena::UNKNOWN,
        });
        let nullable_item_creator = type_arena.make_optional(item_creator);
        let array_of_length_type =
            type_arena.make_function(vec![TypeArena::NUMBER, nullable_item_creator], array_type);
        self.current_scope
            .declare(array_of_length_handle, array_of_length_type);

        // Runtime type constants for 'is' operator
        let array_const_handle = self.strings.intern("ARRAY");
        self.current_scope.declare(array_const_handle, TypeArena::UNKNOWN);

        let object_const_handle = self.strings.intern("OBJECT");
        self.current_scope.declare(object_const_handle, TypeArena::UNKNOWN);

        let string_const_handle = self.strings.intern("STRING");
        self.current_scope.declare(string_const_handle, TypeArena::UNKNOWN);

        let number_const_handle = self.strings.intern("NUMBER");
        self.current_scope.declare(number_const_handle, TypeArena::UNKNOWN);

        let boolean_const_handle = self.strings.intern("BOOLEAN");
        self.current_scope.declare(boolean_const_handle, TypeArena::UNKNOWN);

        let nil_const_handle = self.strings.intern("NIL");
        self.current_scope.declare(nil_const_handle, TypeArena::UNKNOWN);

        let function_const_handle = self.strings.intern("FUNCTION");
        self.current_scope.declare(function_const_handle, TypeArena::UNKNOWN);

        let class_const_handle = self.strings.intern("CLASS");
        self.current_scope.declare(class_const_handle, TypeArena::UNKNOWN);
    }

    /// Get the type of a string intrinsic method
    fn get_string_intrinsic_type(
        &self,
        method_name: StringHandle,
        strings: &StringInterner,
        type_arena: &mut TypeArena,
    ) -> TypeId {
        let method_str = strings.get_string(method_name);
        match method_str {
            "to_uppercase" | "to_lowercase" => {
                // String methods that return string
                type_arena.make_function(vec![], TypeArena::STRING)
            }
            _ => TypeArena::UNKNOWN,
        }
    }

    /// Get the type of an array intrinsic method
    fn get_array_intrinsic_type(
        &self,
        method_name: StringHandle,
        array_type: TypeId,
        strings: &StringInterner,
        type_arena: &mut TypeArena,
    ) -> TypeId {
        let method_str = strings.get_string(method_name);
        let element_type = match type_arena.get_type(array_type) {
            QangType::Array(elem_type) => *elem_type,
            _ => TypeArena::UNKNOWN,
        };

        match method_str {
            "length" => {
                // () -> number
                type_arena.make_function(vec![], TypeArena::NUMBER)
            }
            "push" => {
                // (element) -> unit
                type_arena.make_function(vec![element_type], TypeArena::UNIT)
            }
            "pop" => {
                // () -> element
                type_arena.make_function(vec![], element_type)
            }
            "reverse" => {
                // () -> unit
                type_arena.make_function(vec![], TypeArena::UNIT)
            }
            "slice" => {
                // (number?, number?) -> array
                let optional_number = type_arena.make_optional(TypeArena::NUMBER);
                type_arena.make_function(vec![optional_number, optional_number], array_type)
            }
            "get" => {
                // (number) -> element
                type_arena.make_function(vec![TypeArena::NUMBER], element_type)
            }
            "concat" => {
                // (array) -> array
                type_arena.make_function(vec![array_type], array_type)
            }
            _ => TypeArena::UNKNOWN,
        }
    }

    /// Get the type of a function intrinsic method
    fn get_function_intrinsic_type(
        &self,
        method_name: StringHandle,
        return_type: TypeId,
        strings: &StringInterner,
        type_arena: &mut TypeArena,
    ) -> TypeId {
        let method_str = strings.get_string(method_name);
        let array_type = type_arena.make_array(TypeArena::UNKNOWN);

        match method_str {
            "call" => {
                // (...args) -> return_type (variadic function)
                type_arena.make_function(vec![], return_type)
            }
            "apply" => {
                // (array) -> return_type
                type_arena.make_function(vec![array_type], return_type)
            }
            _ => TypeArena::UNKNOWN,
        }
    }

    /// Handle a regular (non-intrinsic) function call
    fn handle_regular_call(
        &self,
        callee_type: TypeId,
        call: TypedNodeRef<CallExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<TypeId, QangCompilerError> {
        match ctx.nodes.types.get_type(callee_type) {
            QangType::Function { return_type, .. } => Ok(*return_type),
            QangType::Class { .. } => Ok(callee_type),
            QangType::Unknown => Ok(TypeArena::UNKNOWN),
            _ => {
                ctx.errors
                    .report_error(QangCompilerError::new_analysis_error(
                        "Cannot call non-function value".to_string(),
                        call.node.span,
                    ));
                Ok(TypeArena::UNKNOWN)
            }
        }
    }

    #[allow(dead_code)]
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
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        if let Some(parent) = self.current_scope.parent.take() {
            self.current_scope = *parent;
        }
        if self.scope_depth > 0 {
            self.scope_depth -= 1;
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

    #[allow(dead_code)]
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

    /// Track return type with nil handling
    fn track_return_type_with_nil(&mut self, return_type: TypeId, ctx: &mut VisitorContext) {
        // Get current function's return type
        if let Some(&mut current_type) = self.function_stack.last_mut() {
            if current_type == TypeArena::UNKNOWN {
                // First return - just set it
                *self.function_stack.last_mut().unwrap() = return_type;
            } else {
                // Subsequent return - unify with previous
                let unified = self.unify_return_types(current_type, return_type, ctx);
                *self.function_stack.last_mut().unwrap() = unified;
            }
        }
    }

    /// Look up a field or method in a class definition
    fn lookup_class_member(
        &self,
        class_node: NodeId,
        member_name: StringHandle,
        ctx: &VisitorContext,
    ) -> TypeId {
        let class_decl = ctx.nodes.get_decl_node(class_node);

        // Ensure this is actually a class declaration
        if let DeclNode::Class(class_info) = class_decl.node {
            let member_count = ctx.nodes.array.size(class_info.members);

            for i in 0..member_count {
                if let Some(member_id) = ctx.nodes.array.get_node_id_at(class_info.members, i) {
                    let member = ctx.nodes.get_class_member_node(member_id);

                    match member.node {
                        super::typed_node_arena::ClassMemberNode::Field(field) => {
                            let field_identifier = ctx.nodes.get_identifier_node(field.name);
                            if field_identifier.node.name == member_name {
                                // Return the field's type if it has been inferred
                                return ctx.nodes.get_node_type_id(field_identifier.id);
                            }
                        }
                        super::typed_node_arena::ClassMemberNode::Method(method) => {
                            let method_identifier = ctx.nodes.get_identifier_node(method.name);
                            if method_identifier.node.name == member_name {
                                // Return the method's function type
                                return ctx.nodes.get_node_type_id(member_id);
                            }
                        }
                    }
                }
            }
        }

        // Member not found - this will result in an error
        TypeArena::UNKNOWN
    }

    /// Unify two return types, handling nil specially
    fn unify_return_types(
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
                // Two different concrete types - this is an error
                if type_arena.is_optional(type1) && !type_arena.is_optional(type2) {
                    // Optional<T1> + T2 - check if T2 is compatible with T1's inner type
                    let inner1 = type_arena.unwrap_optional(type1);
                    if self.are_types_compatible(inner1, type2, type_arena) {
                        type1 // Keep the optional
                    } else {
                        // Report error for incompatible return types
                        ctx.errors
                            .report_error(QangCompilerError::new_analysis_error(
                                "Inconsistent return types in function".to_string(),
                                SourceSpan::default(),
                            ));
                        TypeArena::UNKNOWN
                    }
                } else if !type_arena.is_optional(type1) && type_arena.is_optional(type2) {
                    let inner2 = type_arena.unwrap_optional(type2);
                    if self.are_types_compatible(type1, inner2, type_arena) {
                        type2 // Keep the optional
                    } else {
                        // Report error for incompatible return types
                        ctx.errors
                            .report_error(QangCompilerError::new_analysis_error(
                                "Inconsistent return types in function".to_string(),
                                SourceSpan::default(),
                            ));
                        TypeArena::UNKNOWN
                    }
                } else {
                    // Both concrete and incompatible - report error
                    ctx.errors
                        .report_error(QangCompilerError::new_analysis_error(
                            "Inconsistent return types in function".to_string(),
                            SourceSpan::default(),
                        ));
                    TypeArena::UNKNOWN
                }
            }
        }
    }
}

impl<'a> NodeVisitor for TypeInferenceEngine<'a> {
    type Error = QangCompilerError;

    fn visit_module(
        &mut self,
        program: TypedNodeRef<Module>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(program.node.decls);

        // First pass: declare all global functions to handle forward references
        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(program.node.decls, i) {
                let decl_node = ctx.nodes.get_decl_node(node_id);
                match decl_node.node {
                    DeclNode::Function(func_decl) => {
                        // Only declare the function name and type, don't visit the body yet
                        let func_expr = ctx.nodes.get_func_expr_node(func_decl.function);
                        let name_node = ctx.nodes.get_identifier_node(func_expr.node.name);

                        // Create a function type with unknown parameter and return types
                        let param_count = ctx.nodes.array.size(func_expr.node.parameters);
                        let param_types = vec![TypeArena::UNKNOWN; param_count];
                        let func_type = ctx
                            .nodes
                            .types
                            .make_function(param_types, TypeArena::UNKNOWN);

                        // Declare the function in the current scope
                        self.current_scope.declare(name_node.node.name, func_type);
                        ctx.nodes
                            .set_node_type(name_node.id, TypeInfo::new(func_type));
                    }
                    DeclNode::Class(class_decl) => {
                        // Declare classes in first pass too for forward references
                        let name_node = ctx.nodes.get_identifier_node(class_decl.name);
                        let class_type = ctx.nodes.types.make_class(name_node.node.name, decl_node.id);
                        self.current_scope.declare(name_node.node.name, class_type);
                        ctx.nodes.set_node_type(name_node.id, TypeInfo::new(class_type));
                    }
                    _ => {} // Other declarations will be handled in second pass
                }
            }
        }

        // Second pass: visit all declarations normally (including function bodies)
        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(program.node.decls, i) {
                let decl_node = ctx.nodes.get_decl_node(node_id);
                self.visit_declaration(decl_node, ctx)?;
            }
        }

        Ok(())
    }

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

        // Track this return type with nil handling
        self.track_return_type_with_nil(return_type, ctx);

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

    fn visit_array_literal(
        &mut self,
        array: TypedNodeRef<ArrayLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(array.node.elements);
        let mut element_types = Vec::new();

        // Visit all elements and collect their types
        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(array.node.elements, i) {
                let element = ctx.nodes.get_expr_node(node_id);
                self.visit_expression(element, ctx)?;
                let element_type = ctx.nodes.get_node_type_id(node_id);
                element_types.push(element_type);
            }
        }

        // Determine the common element type
        let element_type = if element_types.is_empty() {
            // Empty array - use unknown element type
            TypeArena::UNKNOWN
        } else if element_types.len() == 1 {
            // Single element - use its type
            element_types[0]
        } else {
            // Multiple elements - try to find a common type
            let first_type = element_types[0];
            let all_same = element_types.iter().all(|&t| t == first_type);

            if all_same {
                first_type
            } else {
                // Mixed types - use unknown for now
                // In a more sophisticated system, you might want to find a common supertype
                TypeArena::UNKNOWN
            }
        };

        // Create array type
        let array_type = ctx.nodes.types.make_array(element_type);
        ctx.nodes.set_node_type(array.id, TypeInfo::new(array_type));

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
            // For dynamic languages, only report undefined variable errors for local scope
            // Global variables can be dynamically created at runtime
            if self.scope_depth > 0 {
                ctx.errors
                    .report_error(QangCompilerError::new_analysis_error(
                        format!(
                            "Undefined variable '{}'",
                            self.strings.get_string(identifier.node.name)
                        ),
                        identifier.node.span,
                    ));
            }
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
                    // Be more permissive with type checking for now to handle cases like .apply
                    // which are handled specially at runtime
                    if !self.are_types_compatible(first_param_type, left_type, &ctx.nodes.types)
                        && left_type != TypeArena::UNKNOWN
                        && first_param_type != TypeArena::UNKNOWN
                    {
                        // Only report type mismatches when both types are known and incompatible
                        // This allows for runtime resolution of special cases like .apply
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
            QangType::Unknown => {
                // For unknown types, assume it might be a function at runtime
                TypeArena::UNKNOWN
            }
            _ => {
                // Check the actual right-side expression to determine if this is acceptable
                match &right.node {
                    super::typed_node_arena::ExprNode::Call(_) => {
                        // Call expressions like arr.concat() are OK - they might resolve to functions
                        TypeArena::UNKNOWN
                    }
                    _ => {
                        // For other non-function types like strings, numbers, etc., report an error
                        ctx.errors
                            .report_error(QangCompilerError::new_analysis_error(
                                "Right side of pipe must be a function".to_string(),
                                pipe.node.span,
                            ));
                        TypeArena::UNKNOWN
                    }
                }
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
        let operation = ctx.nodes.get_call_operation_node(call.node.operation);

        // Check if this is an intrinsic method call first
        if let super::typed_node_arena::CallOperationNode::Call(_) = operation.node {
            if let super::typed_node_arena::ExprNode::Call(inner_call) =
                ctx.nodes.get_expr_node(call.node.callee).node
            {
                if let super::typed_node_arena::CallOperationNode::Property(property) =
                    ctx.nodes.get_call_operation_node(inner_call.operation).node
                {
                    // This is a method call - handle intrinsics specially
                    let inner_callee = ctx.nodes.get_expr_node(inner_call.callee);
                    self.visit_expression(inner_callee, ctx)?;
                    let inner_callee_type = ctx.nodes.get_node_type_id(inner_call.callee);

                    let property_identifier = ctx.nodes.get_identifier_node(property.identifier);

                    // Handle intrinsic method calls without visiting the property identifier
                    let result_type = match ctx.nodes.types.get_type(inner_callee_type) {
                        QangType::String => {
                            let method_type = self.get_string_intrinsic_type(
                                property_identifier.node.name,
                                self.strings,
                                &mut ctx.nodes.types,
                            );
                            // Set the property identifier type to the method type
                            ctx.nodes
                                .set_node_type(property_identifier.id, TypeInfo::new(method_type));
                            // Set the inner call type to the method type
                            ctx.nodes
                                .set_node_type(inner_call.callee, TypeInfo::new(method_type));
                            // Return the method's return type
                            if let QangType::Function { return_type, .. } =
                                ctx.nodes.types.get_type(method_type)
                            {
                                *return_type
                            } else {
                                TypeArena::UNKNOWN
                            }
                        }
                        QangType::Array(_) => {
                            let method_type = self.get_array_intrinsic_type(
                                property_identifier.node.name,
                                inner_callee_type,
                                self.strings,
                                &mut ctx.nodes.types,
                            );
                            ctx.nodes
                                .set_node_type(property_identifier.id, TypeInfo::new(method_type));
                            ctx.nodes
                                .set_node_type(inner_call.callee, TypeInfo::new(method_type));
                            if let QangType::Function { return_type, .. } =
                                ctx.nodes.types.get_type(method_type)
                            {
                                *return_type
                            } else {
                                TypeArena::UNKNOWN
                            }
                        }
                        QangType::Function {
                            return_type: func_return_type,
                            ..
                        } => {
                            let method_type = self.get_function_intrinsic_type(
                                property_identifier.node.name,
                                *func_return_type,
                                self.strings,
                                &mut ctx.nodes.types,
                            );
                            ctx.nodes
                                .set_node_type(property_identifier.id, TypeInfo::new(method_type));
                            ctx.nodes
                                .set_node_type(inner_call.callee, TypeInfo::new(method_type));
                            if let QangType::Function { return_type, .. } =
                                ctx.nodes.types.get_type(method_type)
                            {
                                *return_type
                            } else {
                                TypeArena::UNKNOWN
                            }
                        }
                        _ => {
                            // Not an intrinsic - fall back to regular processing
                            self.visit_expression(ctx.nodes.get_expr_node(call.node.callee), ctx)?;
                            let callee_type = ctx.nodes.get_node_type_id(call.node.callee);
                            self.handle_regular_call(callee_type, call, ctx)?
                        }
                    };

                    // Visit the call operation arguments
                    self.visit_call_operation(operation, ctx)?;
                    ctx.nodes.set_node_type(call.id, TypeInfo::new(result_type));
                    return Ok(());
                }
            }
        }

        // Regular call processing
        let callee = ctx.nodes.get_expr_node(call.node.callee);
        self.visit_expression(callee, ctx)?;
        let callee_type = ctx.nodes.get_node_type_id(call.node.callee);

        let result_type = match operation.node {
            super::typed_node_arena::CallOperationNode::Property(property) => {
                let property_identifier = ctx.nodes.get_identifier_node(property.identifier);

                // Set the property identifier type based on the result, don't visit it as a variable
                let property_type = match ctx.nodes.types.get_type(callee_type) {
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
                    QangType::Class { class_node, .. } => {
                        // Look up the field/method in the class definition
                        self.lookup_class_member(
                            *class_node,
                            property_identifier.node.name,
                            ctx,
                        )
                    }
                    QangType::String => {
                        // Handle string intrinsic methods
                        self.get_string_intrinsic_type(
                            property_identifier.node.name,
                            self.strings,
                            &mut ctx.nodes.types,
                        )
                    }
                    QangType::Array(_) => {
                        // Handle array intrinsic methods
                        self.get_array_intrinsic_type(
                            property_identifier.node.name,
                            callee_type,
                            self.strings,
                            &mut ctx.nodes.types,
                        )
                    }
                    QangType::Function { return_type, .. } => {
                        // Handle function intrinsic methods
                        self.get_function_intrinsic_type(
                            property_identifier.node.name,
                            *return_type,
                            self.strings,
                            &mut ctx.nodes.types,
                        )
                    }
                    QangType::Unknown => {
                        // For unknown types, assume property access might be valid at runtime
                        TypeArena::UNKNOWN
                    }
                    _ => {
                        ctx.errors
                            .report_error(QangCompilerError::new_analysis_error(
                                "Cannot access property on non-object type".to_string(),
                                call.node.span,
                            ));
                        TypeArena::UNKNOWN
                    }
                };

                // Set the property identifier type and return the property type
                ctx.nodes.set_node_type(property_identifier.id, TypeInfo::new(property_type));
                property_type
            }
            super::typed_node_arena::CallOperationNode::Call(_) => {
                // Check if this is calling an intrinsic method (e.g., arr.length())
                if let super::typed_node_arena::ExprNode::Call(inner_call) =
                    ctx.nodes.get_expr_node(call.node.callee).node
                {
                    if let super::typed_node_arena::CallOperationNode::Property(property) =
                        ctx.nodes.get_call_operation_node(inner_call.operation).node
                    {
                        let inner_callee_type = ctx.nodes.get_node_type_id(inner_call.callee);
                        let property_identifier =
                            ctx.nodes.get_identifier_node(property.identifier);

                        // Handle intrinsic method calls
                        match ctx.nodes.types.get_type(inner_callee_type) {
                            QangType::String => {
                                let method_type = self.get_string_intrinsic_type(
                                    property_identifier.node.name,
                                    self.strings,
                                    &mut ctx.nodes.types,
                                );
                                // For method calls, return the return type of the method
                                if let QangType::Function { return_type, .. } =
                                    ctx.nodes.types.get_type(method_type)
                                {
                                    *return_type
                                } else {
                                    TypeArena::UNKNOWN
                                }
                            }
                            QangType::Array(_) => {
                                let method_type = self.get_array_intrinsic_type(
                                    property_identifier.node.name,
                                    inner_callee_type,
                                    self.strings,
                                    &mut ctx.nodes.types,
                                );
                                if let QangType::Function { return_type, .. } =
                                    ctx.nodes.types.get_type(method_type)
                                {
                                    *return_type
                                } else {
                                    TypeArena::UNKNOWN
                                }
                            }
                            QangType::Function {
                                return_type: func_return_type,
                                ..
                            } => {
                                let method_type = self.get_function_intrinsic_type(
                                    property_identifier.node.name,
                                    *func_return_type,
                                    self.strings,
                                    &mut ctx.nodes.types,
                                );
                                if let QangType::Function { return_type, .. } =
                                    ctx.nodes.types.get_type(method_type)
                                {
                                    *return_type
                                } else {
                                    TypeArena::UNKNOWN
                                }
                            }
                            _ => {
                                // Fall back to regular function call handling
                                match ctx.nodes.types.get_type(callee_type) {
                                    QangType::Function { return_type, .. } => *return_type,
                                    QangType::Class { .. } => callee_type,
                                    QangType::Unknown => TypeArena::UNKNOWN,
                                    _ => {
                                        ctx.errors.report_error(
                                            QangCompilerError::new_analysis_error(
                                                "Cannot call non-function value".to_string(),
                                                call.node.span,
                                            ),
                                        );
                                        TypeArena::UNKNOWN
                                    }
                                }
                            }
                        }
                    } else {
                        // Regular function call
                        match ctx.nodes.types.get_type(callee_type) {
                            QangType::Function { return_type, .. } => *return_type,
                            QangType::Class { .. } => callee_type,
                            QangType::Unknown => TypeArena::UNKNOWN,
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
                } else {
                    // Regular function call
                    match ctx.nodes.types.get_type(callee_type) {
                        QangType::Function { return_type, .. } => *return_type,
                        QangType::Class { .. } => callee_type,
                        QangType::Unknown => TypeArena::UNKNOWN,
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

    fn visit_function_declaration(
        &mut self,
        func_decl: TypedNodeRef<FunctionDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let func_expr = ctx.nodes.get_func_expr_node(func_decl.node.function);
        let name_node = ctx.nodes.get_identifier_node(func_expr.node.name);

        // Check if this function was already declared in the first pass (global scope)
        let already_declared = self.scope_depth == 0 && self.current_scope.lookup(name_node.node.name).is_some();

        if !already_declared {
            // First, create a function type with unknown parameter and return types
            let param_count = ctx.nodes.array.size(func_expr.node.parameters);
            let param_types = vec![TypeArena::UNKNOWN; param_count];
            let func_type = ctx
                .nodes
                .types
                .make_function(param_types, TypeArena::UNKNOWN);

            // Declare the function in the current scope
            self.current_scope.declare(name_node.node.name, func_type);
            ctx.nodes
                .set_node_type(name_node.id, TypeInfo::new(func_type));
        }

        // Now visit the function expression to infer its actual types
        self.visit_function_expression(func_expr, ctx)?;

        // Update the function declaration's type
        let inferred_func_type = ctx.nodes.get_node_type_id(func_decl.node.function);
        ctx.nodes
            .set_node_type(func_decl.id, TypeInfo::new(inferred_func_type));

        // Update the function name's type in scope
        self.current_scope
            .declare(name_node.node.name, inferred_func_type);
        ctx.nodes
            .set_node_type(name_node.id, TypeInfo::new(inferred_func_type));

        Ok(())
    }

    fn visit_class_declaration(
        &mut self,
        class_decl: TypedNodeRef<ClassDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let name_node = ctx.nodes.get_identifier_node(class_decl.node.name);

        // Check if this class was already declared in the first pass (global scope)
        let already_declared = self.scope_depth == 0 && self.current_scope.lookup(name_node.node.name).is_some();

        if !already_declared {
            // Create a class type
            let class_type = ctx
                .nodes
                .types
                .make_class(name_node.node.name, class_decl.id);

            // Declare the class in the current scope
            self.current_scope.declare(name_node.node.name, class_type);
            ctx.nodes
                .set_node_type(name_node.id, TypeInfo::new(class_type));
            ctx.nodes
                .set_node_type(class_decl.id, TypeInfo::new(class_type));
        }

        // Visit the superclass if it exists
        if let Some(superclass_id) = class_decl.node.superclass {
            let superclass = ctx.nodes.get_identifier_node(superclass_id);
            self.visit_identifier(superclass, ctx)?;
        }

        // Visit all class members (methods and field declarations)
        let member_count = ctx.nodes.array.size(class_decl.node.members);
        for i in 0..member_count {
            if let Some(member_id) = ctx.nodes.array.get_node_id_at(class_decl.node.members, i) {
                let member = ctx.nodes.get_class_member_node(member_id);
                self.visit_class_member(member, ctx)?;
            }
        }

        Ok(())
    }

    fn visit_class_member(
        &mut self,
        member: TypedNodeRef<super::typed_node_arena::ClassMemberNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match member.node {
            super::typed_node_arena::ClassMemberNode::Method(method) => {
                // For class methods, we need to handle 'this' context
                self.begin_scope();

                // TODO: Add 'this' to the scope with the class type
                // For now, we'll just visit the method like a regular function
                let func_expr = TypedNodeRef::new(member.id, method);
                self.visit_function_expression(func_expr, ctx)?;

                // Set the member node type to the function type
                let method_type = ctx.nodes.get_node_type_id(member.id);
                ctx.nodes.set_node_type(member.id, TypeInfo::new(method_type));

                self.end_scope();
                Ok(())
            }
            super::typed_node_arena::ClassMemberNode::Field(field) => {
                let identifier = ctx.nodes.get_identifier_node(field.name);

                let field_type = if let Some(initializer_id) = field.initializer {
                    let initializer = ctx.nodes.get_expr_node(initializer_id);
                    self.visit_expression(initializer, ctx)?;
                    ctx.nodes.get_node_type_id(initializer_id)
                } else {
                    // Fields without initializers default to optional unknown (nil)
                    ctx.nodes.types.make_optional(TypeArena::UNKNOWN)
                };

                // Set both the identifier and member types
                ctx.nodes
                    .set_node_type(identifier.id, TypeInfo::new(field_type));
                ctx.nodes
                    .set_node_type(member.id, TypeInfo::new(field_type));
                Ok(())
            }
        }
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

    fn visit_object_literal(
        &mut self,
        object: TypedNodeRef<ObjectLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let mut fields = Vec::new();
        let length = ctx.nodes.array.size(object.node.entries);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(object.node.entries, i) {
                let entry = ctx.nodes.get_obj_entry_node(node_id);

                // Extract field information from the entry
                // ObjectEntryNode has key and value fields
                let key_identifier = ctx.nodes.get_identifier_node(entry.node.key);
                let field_name = key_identifier.node.name;

                // Visit the value expression to infer its type
                let value_expr = ctx.nodes.get_expr_node(entry.node.value);
                self.visit_expression(value_expr, ctx)?;
                let field_type = ctx.nodes.get_node_type_id(entry.node.value);

                fields.push((field_name, field_type));
            }
        }

        let object_type = ctx.nodes.types.make_object(fields);
        ctx.nodes.set_node_type(object.id, TypeInfo::new(object_type));

        Ok(())
    }

    fn visit_object_entry(
        &mut self,
        entry: TypedNodeRef<ObjectEntryNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // For object literal entries, the key is a field name, not a variable reference
        // So we don't need to visit it as an identifier (which would cause variable lookup)
        // Just visit the value expression
        let value_expr = ctx.nodes.get_expr_node(entry.node.value);
        self.visit_expression(value_expr, ctx)?;

        Ok(())
    }

    fn visit_property_assignment(
        &mut self,
        property: TypedNodeRef<PropertyAssignmentNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Visit the object being assigned to
        let object = ctx.nodes.get_expr_node(property.node.object);
        self.visit_expression(object, ctx)?;
        let _object_type = ctx.nodes.get_node_type_id(property.node.object);

        // For property assignment, we need to handle the property identifier
        let property_identifier = ctx.nodes.get_identifier_node(property.node.property);

        // Set the type of the property based on the assignment target type
        // For now, we'll set it to unknown and let assignment expression handle the real type
        ctx.nodes.set_node_type(property_identifier.id, TypeInfo::new(TypeArena::UNKNOWN));
        ctx.nodes.set_node_type(property.id, TypeInfo::new(TypeArena::UNKNOWN));

        Ok(())
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

    fn visit_call_operation(
        &mut self,
        operation: TypedNodeRef<super::typed_node_arena::CallOperationNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match operation.node {
            super::typed_node_arena::CallOperationNode::Call(call) => {
                // Visit function call arguments
                let length = ctx.nodes.array.size(call.args);
                for i in 0..length {
                    if let Some(node_id) = ctx.nodes.array.get_node_id_at(call.args, i) {
                        let arg = ctx.nodes.get_expr_node(node_id);
                        self.visit_expression(arg, ctx)?;
                    }
                }
                Ok(())
            }
            super::typed_node_arena::CallOperationNode::Property(_) => {
                // For property access, don't visit the property identifier as a variable
                // The property type is already set in the call expression handler
                Ok(())
            }
            super::typed_node_arena::CallOperationNode::OptionalProperty(_) => {
                // Similar to property access, don't visit as variable
                Ok(())
            }
            super::typed_node_arena::CallOperationNode::Index(index) => {
                // Visit the index expression
                let index_expr = ctx.nodes.get_expr_node(index.index);
                self.visit_expression(index_expr, ctx)
            }
            super::typed_node_arena::CallOperationNode::Map(_) => {
                // Map operations are handled specially in visit_call_expression
                Ok(())
            }
            super::typed_node_arena::CallOperationNode::OptionalMap(_) => {
                // Optional map operations are handled specially in visit_call_expression
                Ok(())
            }
        }
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
