use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{
    ErrorReporter, NodeId, QangCompilerError, StringHandle, TypedNodeArena,
    frontend::node_visitor::{NodeVisitor, VisitorContext},
    memory::StringInterner,
    nodes::SourceSpan,
};

#[derive(Debug, Clone, PartialEq)]
pub enum VariableKind {
    Local { scope_depth: usize, slot: usize },
    Global,
    Upvalue { index: usize, is_local: bool },
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableInfo {
    pub name: StringHandle,
    pub kind: VariableKind,
    pub is_captured: bool,
    pub span: SourceSpan,
    pub declaring_scope: Option<NodeId>, // NEW: Which scope declared this variable
    pub declaration_order: usize,        // NEW: Order within scope (for LIFO cleanup)
}

#[derive(Debug, Clone, PartialEq)]
pub struct UpvalueInfo {
    pub name: StringHandle,
    pub index: usize,
    pub is_local: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {
    pub name: StringHandle,
    pub arity: usize,
    pub upvalues: Vec<UpvalueInfo>,
    pub max_locals: usize,
    pub span: SourceSpan,
    pub kind: FunctionKind,
}

#[derive(Debug, Clone)]
pub struct ClassInheritanceInfo {
    pub entry_scope_depth: usize,
    pub super_scope_depth: usize,
    pub super_slot: usize,
}

#[derive(Debug, Clone)]
pub struct BreakContinueInfo {
    pub target_loop: NodeId,
    pub statement_type: BreakContinueType,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BreakContinueType {
    Break,
    Continue,
}

// NEW: Unified scope information for all scope types
#[derive(Debug, Clone)]
pub struct ScopeInfo {
    pub scope_type: ScopeType,
    pub declared_variables: Vec<NodeId>, // Variables declared directly in this scope (LIFO order)
    pub scope_depth: usize,
    pub parent_scope: Option<NodeId>, // For nested scope queries
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub enum ScopeType {
    Block(NodeId),     // Block statements
    ForLoop(NodeId),   // For loop bodies
    WhileLoop(NodeId), // While loop bodies
    Function(NodeId),  // Function bodies
    Class(NodeId),     // Class bodies
    Lambda(NodeId),    // Lambda expressions
}

#[derive(Debug, Clone)]
pub struct ScopeAnalysis {
    pub variables: FxHashMap<NodeId, VariableInfo>,
    pub functions: FxHashMap<NodeId, FunctionInfo>,
    pub upvalue_captures: FxHashMap<NodeId, Vec<UpvalueInfo>>,
    pub class_inheritance: FxHashMap<NodeId, ClassInheritanceInfo>,
    pub break_continue_statements: FxHashMap<NodeId, BreakContinueInfo>,
    pub scopes: FxHashMap<NodeId, ScopeInfo>, // NEW: Unified scope tracking
}

impl Default for ScopeAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeAnalysis {
    pub fn new() -> Self {
        Self {
            variables: FxHashMap::with_hasher(FxBuildHasher),
            functions: FxHashMap::with_hasher(FxBuildHasher),
            upvalue_captures: FxHashMap::with_hasher(FxBuildHasher),
            class_inheritance: FxHashMap::with_hasher(FxBuildHasher),
            break_continue_statements: FxHashMap::with_hasher(FxBuildHasher),
            scopes: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn merge_with(mut self, other: Self) -> Self {
        self.variables.extend(other.variables);
        self.functions.extend(other.functions);
        self.upvalue_captures.extend(other.upvalue_captures);
        self.class_inheritance.extend(other.class_inheritance);
        self.break_continue_statements
            .extend(other.break_continue_statements);
        self.scopes.extend(other.scopes);
        self
    }

    // NEW: Helper methods for scope queries
    // TODO turn this into an iterator and do not create a new vec
    pub fn get_scope_variables(&self, scope_id: NodeId) -> Vec<NodeId> {
        if let Some(scope_info) = self.scopes.get(&scope_id) {
            scope_info.declared_variables.clone()
        } else {
            Vec::new()
        }
    }

    pub fn get_variable_scope(&self, var_id: NodeId) -> Option<NodeId> {
        if let Some(var_info) = self.variables.get(&var_id) {
            var_info.declaring_scope
        } else {
            None
        }
    }

    pub fn get_nested_scopes(&self, scope_id: NodeId) -> Vec<NodeId> {
        self.scopes
            .iter()
            .filter_map(|(id, scope_info)| {
                if scope_info.parent_scope == Some(scope_id) {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
struct Scope {
    variables: FxHashMap<StringHandle, VariableInfo>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: FxHashMap::with_hasher(FxBuildHasher),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionKind {
    Global,
    Function,
    Method,
    Initializer,
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub name: StringHandle,
    pub slot: usize,
    pub is_captured: bool,
    pub scope_depth: usize,
    pub is_initialized: bool,
}

#[derive(Debug, Clone)]
struct FunctionContext {
    pub name: StringHandle,
    pub arity: usize,
    pub kind: FunctionKind,
    pub locals: Vec<LocalVariable>,
    pub local_count: usize,
    pub upvalues: Vec<UpvalueInfo>,
    pub span: SourceSpan,
}

impl FunctionContext {
    fn new(
        handle: StringHandle,
        arity: usize,
        kind: FunctionKind,
        blank_handle: StringHandle,
        span: SourceSpan,
    ) -> Self {
        let mut locals = Vec::with_capacity(u8::MAX as usize);

        // For methods and initializers, slot 0 is reserved for 'this'
        let initial_name = match kind {
            FunctionKind::Method | FunctionKind::Initializer => handle, // Use method name as placeholder
            _ => blank_handle,
        };

        locals.push(LocalVariable {
            name: initial_name,
            scope_depth: 0,
            slot: 0,
            is_captured: false,
            is_initialized: true, // Function names are immediately initialized
        });

        Self {
            name: handle,
            arity,
            locals,
            local_count: 1,
            upvalues: Vec::with_capacity(u8::MAX as usize),
            span,
            kind,
        }
    }

    fn add_local(&mut self, name: StringHandle, scope_depth: usize, is_initialized: bool) -> usize {
        let slot = self.local_count;
        let local_index = self.locals.len();

        self.locals.push(LocalVariable {
            name,
            scope_depth,
            slot,
            is_captured: false,
            is_initialized,
        });

        self.local_count += 1;

        local_index
    }

    fn mark_initialized(&mut self, local_index: usize) {
        if let Some(local) = self.locals.get_mut(local_index) {
            local.is_initialized = true;
        }
    }

    fn find_local(&self, name: StringHandle, current_scope_depth: usize) -> Option<usize> {
        // Search backwards to find the most recent (innermost scope) variable with this name
        // that is still within the current scope depth
        self.locals.iter().enumerate().rev().find_map(|(i, local)| {
            if local.name == name && local.scope_depth <= current_scope_depth {
                Some(i)
            } else {
                None
            }
        })
    }

    fn add_upvalue(&mut self, name: StringHandle, index: usize, is_local: bool) -> usize {
        let upvalue_index = self.upvalues.len();

        self.upvalues.push(UpvalueInfo {
            name,
            index,
            is_local,
        });

        upvalue_index
    }
}

pub struct ScopeAnalyzer<'a> {
    strings: &'a mut StringInterner,
    scopes: Vec<Scope>,
    functions: Vec<FunctionContext>,
    results: ScopeAnalysis,
    blank_handle: StringHandle,
    scope_stack: Vec<NodeId>, // Stack of current scope node IDs for unified tracking
    declaration_order_counter: usize, // Global counter for declaration order
}

impl<'a> ScopeAnalyzer<'a> {
    pub fn new(strings: &'a mut StringInterner) -> Self {
        let handle = strings.intern("(script)");
        let blank_handle = strings.intern("");
        Self {
            blank_handle,
            strings,
            scopes: vec![Scope::new()],
            functions: vec![FunctionContext::new(
                handle,
                0,
                FunctionKind::Global,
                blank_handle,
                SourceSpan::default(),
            )],
            results: ScopeAnalysis::new(),
            scope_stack: Vec::new(),
            declaration_order_counter: 0,
        }
    }

    pub fn analyze(
        mut self,
        program: NodeId,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> ScopeAnalysis {
        let mut ctx = VisitorContext::new(nodes, errors);
        let program_node = ctx.nodes.get_program_node(program);

        let _ = self.visit_module(program_node, &mut ctx);

        self.results
    }

    fn current_scope_depth(&self) -> usize {
        self.scopes.len() - 1
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    // NEW: Unified scope tracking methods
    fn begin_tracked_scope(&mut self, scope_type: ScopeType, scope_id: NodeId, span: SourceSpan) {
        let scope_depth = self.current_scope_depth();
        let parent_scope = self.scope_stack.last().copied();

        let scope_info = ScopeInfo {
            scope_type,
            declared_variables: Vec::new(),
            scope_depth,
            parent_scope,
            span,
        };

        self.results.scopes.insert(scope_id, scope_info);
        self.scope_stack.push(scope_id);
        self.begin_scope();
    }

    fn end_tracked_scope(&mut self) {
        self.scope_stack.pop();
        self.end_scope();
    }

    fn declare_variable_in_current_scope(&mut self, var_id: NodeId) {
        if let Some(&current_scope_id) = self.scope_stack.last() {
            // Add to scope's declared_variables list
            if let Some(scope_info) = self.results.scopes.get_mut(&current_scope_id) {
                scope_info.declared_variables.push(var_id);
            }

            // Update variable's declaring_scope and declaration_order
            if let Some(var_info) = self.results.variables.get_mut(&var_id) {
                var_info.declaring_scope = Some(current_scope_id);
                var_info.declaration_order = self.declaration_order_counter;
                self.declaration_order_counter += 1;
            }
        }
    }

    fn declare_variable(
        &mut self,
        identifier: crate::frontend::typed_node_arena::TypedNodeRef<
            crate::frontend::nodes::IdentifierNode,
        >,
    ) -> Result<(), QangCompilerError> {
        self.declare_variable_with_init(identifier, true)?; // Default to initialized
        Ok(())
    }

    fn declare_variable_with_init(
        &mut self,
        identifier: crate::frontend::typed_node_arena::TypedNodeRef<
            crate::frontend::nodes::IdentifierNode,
        >,
        is_initialized: bool,
    ) -> Result<Option<usize>, QangCompilerError> {
        let name = identifier.node.name;
        let span = identifier.node.span;

        // Check for duplicate variables in current scope (only for local scopes)
        if self.current_scope_depth() > 0
            && let Some(current_scope) = self.scopes.last()
            && current_scope.variables.contains_key(&name)
        {
            return Err(QangCompilerError::new_analysis_error(
                "Already a variable with this name in this scope.".to_string(),
                span,
            ));
        }

        let (variable_info, local_index) = if self.current_scope_depth() == 0 {
            // Global scope
            let variable_info = VariableInfo {
                name,
                kind: VariableKind::Global,
                is_captured: false,
                span,
                declaring_scope: None, // Global variables don't have a scope
                declaration_order: 0,
            };
            (variable_info, None)
        } else {
            // Local scope - add to current function's locals
            let scope_depth = self.current_scope_depth();
            let (local_idx, slot) = if let Some(function) = self.functions.last_mut() {
                let slot = function.local_count; // Get slot before adding
                let local_idx = function.add_local(name, scope_depth, is_initialized);
                (local_idx, slot)
            } else {
                (0, 0)
            };

            let variable_info = VariableInfo {
                name,
                kind: VariableKind::Local { scope_depth, slot },
                is_captured: false,
                span,
                declaring_scope: None, // Will be set by declare_variable_in_current_scope
                declaration_order: 0,  // Will be set by declare_variable_in_current_scope
            };
            (variable_info, Some(local_idx))
        };

        // Add to current scope
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.variables.insert(name, variable_info.clone());
        }

        // Add to results
        self.results.variables.insert(identifier.id, variable_info);

        // Track in current scope if it's a local variable
        if self.current_scope_depth() > 0 {
            self.declare_variable_in_current_scope(identifier.id);
        }

        Ok(local_index)
    }

    fn resolve_variable(
        &mut self,
        name: StringHandle,
        span: SourceSpan,
        node_id: NodeId,
    ) -> Result<VariableKind, QangCompilerError> {
        // First try to resolve as local variable in current function
        if let Some(function) = self.functions.last() {
            let current_scope_depth = self.current_scope_depth();
            if let Some(local_index) = function.find_local(name, current_scope_depth) {
                let local = &function.locals[local_index];

                // Check if the local variable is initialized
                if !local.is_initialized {
                    return Err(QangCompilerError::new_analysis_error(
                        "Cannot read local variable during its initialization.".to_string(),
                        span,
                    ));
                }

                let variable_kind = VariableKind::Local {
                    scope_depth: local.scope_depth,
                    slot: local.slot,
                };

                // Add to results
                let variable_info = VariableInfo {
                    name,
                    kind: variable_kind.clone(),
                    is_captured: local.is_captured,
                    span,
                    declaring_scope: None, // Variable references don't have declaring scope info
                    declaration_order: 0,
                };
                self.results.variables.insert(node_id, variable_info);

                return Ok(variable_kind);
            }
        }

        // Try to resolve as upvalue
        if let Some(upvalue_kind) = self.resolve_upvalue(name, span)? {
            let variable_info = VariableInfo {
                name,
                kind: upvalue_kind.clone(),
                is_captured: false,
                span,
                declaring_scope: None, // Variable references don't have declaring scope info
                declaration_order: 0,
            };
            self.results.variables.insert(node_id, variable_info);
            return Ok(upvalue_kind);
        }

        // Default to global variable
        let variable_kind = VariableKind::Global;
        let variable_info = VariableInfo {
            name,
            kind: variable_kind.clone(),
            is_captured: false,
            span,
            declaring_scope: None, // Variable references don't have declaring scope info
            declaration_order: 0,
        };
        self.results.variables.insert(node_id, variable_info);

        Ok(variable_kind)
    }

    fn resolve_upvalue(
        &mut self,
        name: StringHandle,
        _span: SourceSpan,
    ) -> Result<Option<VariableKind>, QangCompilerError> {
        if self.functions.len() < 2 {
            return Ok(None); // No enclosing function to capture from
        }

        // We need to look at functions in reverse order (excluding current)
        let current_func_idx = self.functions.len() - 1;

        // Check if it's a local in the immediately enclosing function
        if let Some(enclosing_func) = self.functions.get_mut(current_func_idx - 1) {
            // For upvalue resolution, we want to find any variable in the enclosing function
            // regardless of scope depth, since it will be captured
            if let Some(local_index) = enclosing_func.find_local(name, usize::MAX) {
                // Mark the local as captured
                enclosing_func.locals[local_index].is_captured = true;

                // Add upvalue to current function
                if let Some(current_func) = self.functions.get_mut(current_func_idx) {
                    let upvalue_index = current_func.add_upvalue(name, local_index, true);
                    return Ok(Some(VariableKind::Upvalue {
                        index: upvalue_index,
                        is_local: true,
                    }));
                }
            }
        }

        // Recursively check outer scopes by temporarily removing current function
        let current_function = self.functions.pop().unwrap();
        let upvalue_result = self.resolve_upvalue(name, _span)?;
        self.functions.push(current_function);

        if let Some(VariableKind::Upvalue { index, .. }) = upvalue_result {
            // Add upvalue reference to current function
            if let Some(current_func) = self.functions.last_mut() {
                let upvalue_index = current_func.add_upvalue(name, index, false);
                return Ok(Some(VariableKind::Upvalue {
                    index: upvalue_index,
                    is_local: false,
                }));
            }
        }

        Ok(None)
    }

    fn current_loop(&self) -> Option<NodeId> {
        // Find the most recent loop scope in the scope stack, but stop at function boundaries
        for &scope_id in self.scope_stack.iter().rev() {
            if let Some(scope_info) = self.results.scopes.get(&scope_id) {
                match scope_info.scope_type {
                    ScopeType::ForLoop(_) | ScopeType::WhileLoop(_) => return Some(scope_id),
                    // Stop at function boundaries - break/continue cannot cross function boundaries
                    ScopeType::Function(_) | ScopeType::Lambda(_) => return None,
                    _ => continue,
                }
            }
        }
        None
    }

    fn begin_function(
        &mut self,
        name: StringHandle,
        arity: usize,
        span: SourceSpan,
        kind: FunctionKind,
        errors: &mut ErrorReporter,
        function_id: NodeId,
    ) {
        self.begin_function_with_scope_type(
            name,
            arity,
            span,
            kind,
            errors,
            function_id,
            ScopeType::Function(function_id),
        );
    }

    fn begin_function_with_scope_type(
        &mut self,
        name: StringHandle,
        arity: usize,
        span: SourceSpan,
        kind: FunctionKind,
        errors: &mut ErrorReporter,
        function_id: NodeId,
        scope_type: ScopeType,
    ) {
        if arity > 255 {
            errors.report_error(QangCompilerError::new_analysis_error(
                "Cannot have more than 255 parameters.".to_string(),
                span,
            ));
        }

        let mut function = FunctionContext::new(name, arity, kind, self.blank_handle, span);

        // For methods and initializers, properly set up 'this' at slot 0
        if matches!(kind, FunctionKind::Method | FunctionKind::Initializer) {
            let this_handle = self.strings.intern("this");
            function.locals[0].name = this_handle;
        }

        self.functions.push(function);

        // Track function scope using unified tracking
        self.begin_tracked_scope(scope_type, function_id, span);

        // Note: Loops don't cross function boundaries - scope stack automatically handles this
    }

    fn end_function(&mut self, node_id: NodeId) -> Result<(), QangCompilerError> {
        self.end_tracked_scope();

        let function = self
            .functions
            .pop()
            .expect("Expect function stack not to be empty.");

        let analysis = FunctionInfo {
            name: function.name,
            arity: function.arity,
            upvalues: function.upvalues.clone(),
            max_locals: function.local_count,
            span: function.span,
            kind: function.kind,
        };
        self.results.functions.insert(node_id, analysis);
        self.results
            .upvalue_captures
            .insert(node_id, function.upvalues);

        Ok(())
    }

    fn get_identifier_name(&self, nodes: &TypedNodeArena, node_id: NodeId) -> StringHandle {
        nodes.get_identifier_node(node_id).node.name
    }
}

impl<'a> NodeVisitor for ScopeAnalyzer<'a> {
    type Error = QangCompilerError;

    fn visit_identifier(
        &mut self,
        identifier: super::typed_node_arena::TypedNodeRef<super::nodes::IdentifierNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if let Err(error) =
            self.resolve_variable(identifier.node.name, identifier.node.span, identifier.id)
        {
            ctx.errors.report_error(error);
        }
        Ok(())
    }

    fn visit_array_literal(
        &mut self,
        array: super::typed_node_arena::TypedNodeRef<super::nodes::ArrayLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let size = ctx.nodes.array.size(array.node.elements);

        if size > u8::MAX as usize {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Array literal cannot have more than 256 elements.".to_string(),
                    array.node.span,
                ));
        }

        for i in 0..size {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(array.node.elements, i) {
                let expr = ctx.nodes.get_expr_node(node_id);
                self.visit_expression(expr, ctx)?;
            }
        }

        Ok(())
    }

    fn visit_object_literal(
        &mut self,
        object: super::typed_node_arena::TypedNodeRef<super::nodes::ObjectLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let size = ctx.nodes.array.size(object.node.entries);

        if size > u8::MAX as usize {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Object literal cannot have more than 256 entries.".to_string(),
                    object.node.span,
                ));
        }

        // Visit each object entry to analyze the value expressions
        for i in 0..size {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(object.node.entries, i) {
                let entry = ctx.nodes.get_obj_entry_node(node_id);
                self.visit_object_entry(entry, ctx)?;
            }
        }

        Ok(())
    }

    fn visit_function_expression(
        &mut self,
        func_expr: super::typed_node_arena::TypedNodeRef<super::nodes::FunctionExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let arity = ctx.nodes.array.size(func_expr.node.parameters);

        let identifier = ctx.nodes.get_identifier_node(func_expr.node.name);

        if let Err(error) = self.declare_variable(identifier) {
            ctx.errors.report_error(error);
        }

        if arity > u8::MAX as usize {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Function call cannot have more than 256 arguments.".to_string(),
                    func_expr.node.span,
                ));
        }

        self.begin_function(
            self.get_identifier_name(ctx.nodes, func_expr.node.name),
            arity,
            func_expr.node.span,
            FunctionKind::Function,
            ctx.errors,
            func_expr.id,
        );

        for i in 0..arity {
            if let Some(param_id) = ctx.nodes.array.get_node_id_at(func_expr.node.parameters, i) {
                let param = ctx.nodes.get_identifier_node(param_id);
                if let Err(error) = self.declare_variable(param) {
                    ctx.errors.report_error(error);
                };
            }
        }

        // Visit body
        let body = ctx.nodes.get_block_stmt_node(func_expr.node.body);
        self.visit_block_statement(body, ctx)?;

        self.end_function(func_expr.id)?;

        Ok(())
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: super::typed_node_arena::TypedNodeRef<super::nodes::LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let arity = ctx.nodes.array.size(lambda_expr.node.parameters);

        if arity > u8::MAX as usize {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Function call cannot have more than 256 arguments.".to_string(),
                    lambda_expr.node.span,
                ));
        }

        let anonymous_name = self.strings.intern("<lambda>");

        self.begin_function_with_scope_type(
            anonymous_name,
            arity,
            lambda_expr.node.span,
            FunctionKind::Function,
            ctx.errors,
            lambda_expr.id,
            ScopeType::Lambda(lambda_expr.id),
        );

        for i in 0..arity {
            if let Some(param_id) = ctx
                .nodes
                .array
                .get_node_id_at(lambda_expr.node.parameters, i)
            {
                let param = ctx.nodes.get_identifier_node(param_id);
                if let Err(error) = self.declare_variable(param) {
                    ctx.errors.report_error(error);
                };
            }
        }

        // Visit body
        let body = ctx.nodes.get_lambda_body_node(lambda_expr.node.body);
        self.visit_lambda_body(body, ctx)?;

        self.end_function(lambda_expr.id)?;

        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        var_decl: super::typed_node_arena::TypedNodeRef<super::nodes::VariableDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(var_decl.node.target);

        // First declare the variable (but leave it uninitialized for locals)
        let local_index = match self.declare_variable_with_init(identifier, false) {
            Ok(index) => index,
            Err(error) => {
                ctx.errors.report_error(error);
                None // Continue analysis but without local index
            }
        };

        // Then analyze initializer (if present)
        if let Some(initializer_id) = var_decl.node.initializer {
            let initializer = ctx.nodes.get_expr_node(initializer_id);
            self.visit_expression(initializer, ctx)?;
        }

        // Finally, mark the variable as initialized (for locals only)
        if let Some(local_idx) = local_index
            && let Some(function) = self.functions.last_mut()
        {
            function.mark_initialized(local_idx);
        }

        Ok(())
    }

    fn visit_import_module_declaration(
        &mut self,
        import_decl: super::typed_node_arena::TypedNodeRef<super::nodes::ImportModuleDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(import_decl.node.name);
        if let Err(error) = self.declare_variable(identifier) {
            ctx.errors.report_error(error);
        }
        Ok(())
    }

    fn visit_call_expression(
        &mut self,
        call: super::typed_node_arena::TypedNodeRef<super::nodes::CallExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Visit callee
        let callee = ctx.nodes.get_expr_node(call.node.callee);
        self.visit_expression(callee, ctx)?;

        // Visit operation
        let operation = ctx.nodes.get_call_operation_node(call.node.operation);
        match operation.node {
            super::typed_node_arena::CallOperationNode::Call(call_node) => {
                let arg_count = ctx.nodes.array.size(call_node.args);

                if arg_count > u8::MAX as usize {
                    ctx.errors
                        .report_error(QangCompilerError::new_analysis_error(
                            "Function call cannot have more than 256 arguments.".to_string(),
                            call_node.span,
                        ));
                }

                let arg_count = ctx.nodes.array.size(call_node.args);
                for i in 0..arg_count {
                    if let Some(arg_id) = ctx.nodes.array.get_node_id_at(call_node.args, i) {
                        let arg = ctx.nodes.get_expr_node(arg_id);
                        self.visit_expression(arg, ctx)?;
                    }
                }
            }
            _ => {
                self.visit_call_operation(operation, ctx)?;
            }
        }

        Ok(())
    }

    fn visit_class_declaration(
        &mut self,
        class_decl: super::typed_node_arena::TypedNodeRef<super::nodes::ClassDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let name_node = ctx.nodes.get_identifier_node(class_decl.node.name);
        if let Err(error) = self.declare_variable(name_node) {
            ctx.errors.report_error(error);
        }

        // Handle inheritance setup
        let has_superclass = class_decl.node.superclass.is_some();
        if has_superclass {
            // Visit superclass identifier
            if let Some(superclass_id) = class_decl.node.superclass {
                let superclass = ctx.nodes.get_identifier_node(superclass_id);
                self.visit_identifier(superclass, ctx)?;
            }

            // Begin new scope for super variable - this creates a scope that methods can capture from
            self.begin_scope();

            // Create a synthetic local for super by manipulating the current function's local slot allocation
            // This mimics how the working assembler allocates super as a local in the class compilation context
            let super_handle = self.strings.intern("super");
            let scope_depth = self.current_scope_depth();

            // Add super to current function's locals to properly reserve a slot
            let super_slot = if let Some(func) = self.functions.last_mut() {
                let slot_index = func.add_local(super_handle, scope_depth, true); // Super should be immediately initialized
                func.locals[slot_index].slot
            } else {
                ctx.errors
                    .report_error(QangCompilerError::new_analysis_error(
                        "No function context for super variable".to_string(),
                        class_decl.node.span,
                    ));

                return Ok(());
            };

            // Add to scope so methods can resolve 'super' as an upvalue
            let var_info = VariableInfo {
                name: super_handle,
                kind: VariableKind::Local {
                    scope_depth,
                    slot: super_slot,
                },
                is_captured: false, // Will be marked as captured when methods access it
                span: class_decl.node.span,
                declaring_scope: None, // Super is a special synthetic variable
                declaration_order: 0,
            };

            if let Some(current_scope) = self.scopes.last_mut() {
                current_scope.variables.insert(super_handle, var_info);
            }

            // Record class inheritance information for the compiler backend
            let inheritance_info = ClassInheritanceInfo {
                entry_scope_depth: scope_depth - 1, // Scope depth before begin_scope()
                super_scope_depth: scope_depth,     // Scope depth where super is allocated
                super_slot,                         // Slot where super should be stored
            };

            self.results
                .class_inheritance
                .insert(class_decl.id, inheritance_info);
        }

        // Visit members
        let member_count = ctx.nodes.array.size(class_decl.node.members);
        for i in 0..member_count {
            if let Some(member_id) = ctx.nodes.array.get_node_id_at(class_decl.node.members, i) {
                let member = ctx.nodes.get_class_member_node(member_id);
                match member.node {
                    super::typed_node_arena::ClassMemberNode::Method(method) => {
                        let method_name = ctx.nodes.get_identifier_node(method.name);
                        let init_name = self.strings.intern("init");
                        let is_initializer = method_name.node.name == init_name;
                        let arity = ctx.nodes.array.size(method.parameters);

                        if arity > u8::MAX as usize {
                            ctx.errors
                                .report_error(QangCompilerError::new_analysis_error(
                                    "Function call cannot have more than 256 arguments."
                                        .to_string(),
                                    method.span,
                                ));
                        }

                        self.begin_function(
                            method_name.node.name,
                            arity,
                            method.span,
                            if is_initializer {
                                FunctionKind::Initializer
                            } else {
                                FunctionKind::Method
                            },
                            ctx.errors,
                            member_id,
                        );

                        // Visit parameters
                        for j in 0..arity {
                            if let Some(param_id) =
                                ctx.nodes.array.get_node_id_at(method.parameters, j)
                            {
                                let param = ctx.nodes.get_identifier_node(param_id);
                                if let Err(error) = self.declare_variable(param) {
                                    ctx.errors.report_error(error);
                                };
                            }
                        }

                        // Visit body
                        let body = ctx.nodes.get_block_stmt_node(method.body);
                        self.visit_block_statement(body, ctx)?;

                        self.end_function(member_id)?;
                    }
                    super::typed_node_arena::ClassMemberNode::Field(field) => {
                        // Field names are not variables to be resolved, they are property names
                        // Only visit the initializer if present
                        if let Some(init_id) = field.initializer {
                            let initializer = ctx.nodes.get_expr_node(init_id);
                            self.visit_expression(initializer, ctx)?;
                        }
                    }
                }
            }
        }

        // End the superclass scope if we created one
        if has_superclass {
            self.end_scope();
        }

        Ok(())
    }

    fn visit_block_statement(
        &mut self,
        block_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::BlockStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Use unified scope tracking
        self.begin_tracked_scope(
            ScopeType::Block(block_stmt.id),
            block_stmt.id,
            block_stmt.node.span,
        );

        let decl_count = ctx.nodes.array.size(block_stmt.node.decls);
        for i in 0..decl_count {
            if let Some(decl_id) = ctx.nodes.array.get_node_id_at(block_stmt.node.decls, i) {
                let decl = ctx.nodes.get_decl_node(decl_id);
                self.visit_declaration(decl, ctx)?;
            }
        }

        self.end_tracked_scope();
        Ok(())
    }

    fn visit_super_expression(
        &mut self,
        super_expr: super::typed_node_arena::TypedNodeRef<super::nodes::SuperExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Check if we're in a method or initializer context
        if let Some(current_function) = &self.functions.last() {
            if !matches!(
                current_function.kind,
                FunctionKind::Method | FunctionKind::Initializer
            ) {
                ctx.errors
                    .report_error(QangCompilerError::new_analysis_error(
                        "Cannot use 'super' outside of a class method.".to_string(),
                        super_expr.node.span,
                    ));
            }
        } else {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Cannot use 'super' outside of a class method.".to_string(),
                    super_expr.node.span,
                ));
        }

        // Resolve the 'super' variable access
        let super_handle = self.strings.intern("super");
        if let Err(error) = self.resolve_variable(super_handle, super_expr.node.span, super_expr.id)
        {
            ctx.errors.report_error(error);
        }

        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::WhileStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Visit condition
        let condition = ctx.nodes.get_expr_node(while_stmt.node.condition);
        self.visit_expression(condition, ctx)?;

        // Enter loop context and visit body using unified scope tracking
        self.begin_tracked_scope(
            ScopeType::WhileLoop(while_stmt.id),
            while_stmt.id,
            while_stmt.node.span,
        );
        let body = ctx.nodes.get_stmt_node(while_stmt.node.body);
        self.visit_statement(body, ctx)?;
        self.end_tracked_scope();

        Ok(())
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::ForStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Use unified scope tracking for for loops
        self.begin_tracked_scope(
            ScopeType::ForLoop(for_stmt.id),
            for_stmt.id,
            for_stmt.node.span,
        );

        // Handle initializer (variables automatically tracked by unified scope system)
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

        self.end_tracked_scope();
        Ok(())
    }

    fn visit_break_statement(
        &mut self,
        break_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::BreakStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if let Some(target_loop) = self.current_loop() {
            // Record this break statement and which loop it targets
            let break_info = BreakContinueInfo {
                target_loop,
                statement_type: BreakContinueType::Break,
                span: break_stmt.node.span,
            };
            self.results
                .break_continue_statements
                .insert(break_stmt.id, break_info);
        } else {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "'break' can only be used inside loops.".to_string(),
                    break_stmt.node.span,
                ));
        }
        Ok(())
    }

    fn visit_continue_statement(
        &mut self,
        continue_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::ContinueStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if let Some(target_loop) = self.current_loop() {
            // Record this continue statement and which loop it targets
            let continue_info = BreakContinueInfo {
                target_loop,
                statement_type: BreakContinueType::Continue,
                span: continue_stmt.node.span,
            };
            self.results
                .break_continue_statements
                .insert(continue_stmt.id, continue_info);
        } else {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "'continue' can only be used inside loops.".to_string(),
                    continue_stmt.node.span,
                ));
        }
        Ok(())
    }

    fn visit_map_expression(
        &mut self,
        map_expr: super::typed_node_arena::TypedNodeRef<super::nodes::MapExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Map expressions are essentially lambda expressions, so treat them the same way
        let anonymous_name = self.strings.intern("<map>");

        self.begin_function_with_scope_type(
            anonymous_name,
            1, // Map expressions always take exactly one parameter
            map_expr.node.span,
            FunctionKind::Function,
            ctx.errors,
            map_expr.id,
            ScopeType::Lambda(map_expr.id),
        );

        // Declare the parameter
        let param = ctx.nodes.get_identifier_node(map_expr.node.parameter);
        if let Err(error) = self.declare_variable(param) {
            ctx.errors.report_error(error);
        };

        // Visit the body expression
        let body = ctx.nodes.get_expr_node(map_expr.node.body);
        self.visit_expression(body, ctx)?;

        self.end_function(map_expr.id)?;

        Ok(())
    }

    fn visit_optional_map_expression(
        &mut self,
        opt_map_expr: super::typed_node_arena::TypedNodeRef<super::nodes::OptionalMapExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Optional map expressions work the same as regular map expressions
        let anonymous_name = self.strings.intern("<optional_map>");

        self.begin_function_with_scope_type(
            anonymous_name,
            1, // Optional map expressions also take exactly one parameter
            opt_map_expr.node.span,
            FunctionKind::Function,
            ctx.errors,
            opt_map_expr.id,
            ScopeType::Lambda(opt_map_expr.id),
        );

        // Declare the parameter
        let param = ctx.nodes.get_identifier_node(opt_map_expr.node.parameter);
        if let Err(error) = self.declare_variable(param) {
            ctx.errors.report_error(error);
        };

        // Visit the body expression
        let body = ctx.nodes.get_expr_node(opt_map_expr.node.body);
        self.visit_expression(body, ctx)?;

        self.end_function(opt_map_expr.id)?;

        Ok(())
    }

    fn visit_call_operation(
        &mut self,
        operation: super::typed_node_arena::TypedNodeRef<
            super::typed_node_arena::CallOperationNode,
        >,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        use super::typed_node_arena::CallOperationNode;

        match operation.node {
            CallOperationNode::Call(call) => {
                let arg_count = ctx.nodes.array.size(call.args);
                for i in 0..arg_count {
                    if let Some(arg_id) = ctx.nodes.array.get_node_id_at(call.args, i) {
                        let arg = ctx.nodes.get_expr_node(arg_id);
                        self.visit_expression(arg, ctx)?;
                    }
                }
                Ok(())
            }
            CallOperationNode::Property(_property) => {
                // Property names should not be resolved as variables - they are property identifiers
                // used for property lookup at runtime, not variable resolution at compile time
                Ok(())
            }
            CallOperationNode::OptionalProperty(_optional_property) => {
                // Same as regular properties - don't resolve property names as variables
                Ok(())
            }
            CallOperationNode::Index(index) => {
                self.visit_expression(ctx.nodes.get_expr_node(index.index), ctx)
            }
            CallOperationNode::Map(map) => self.visit_map_expression(
                super::typed_node_arena::TypedNodeRef::new(operation.id, map),
                ctx,
            ),
            CallOperationNode::OptionalMap(map) => self.visit_optional_map_expression(
                super::typed_node_arena::TypedNodeRef::new(operation.id, map),
                ctx,
            ),
        }
    }
}
