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
pub struct ScopeAnalysis {
    pub variables: FxHashMap<NodeId, VariableInfo>,
    pub functions: FxHashMap<NodeId, FunctionInfo>,
    pub upvalue_captures: FxHashMap<NodeId, Vec<UpvalueInfo>>,
}

impl ScopeAnalysis {
    pub fn new() -> Self {
        Self {
            variables: FxHashMap::with_hasher(FxBuildHasher),
            functions: FxHashMap::with_hasher(FxBuildHasher),
            upvalue_captures: FxHashMap::with_hasher(FxBuildHasher),
        }
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
    Function,
    Method,
    Initializer,
}

#[derive(Debug, Clone)]
struct FunctionScope {
    name: StringHandle,
    arity: usize,
    upvalues: Vec<UpvalueInfo>,
    local_count: usize,
    span: SourceSpan,
    kind: FunctionKind,
    enclosing: Option<Box<FunctionScope>>,
}

impl FunctionScope {
    fn new(name: StringHandle, arity: usize, span: SourceSpan, kind: FunctionKind) -> Self {
        // All functions reserve slot 0 - for 'this' in methods, blank in regular functions
        let local_count = 1;

        Self {
            name,
            arity,
            upvalues: Vec::new(),
            local_count,
            span,
            kind,
            enclosing: None,
        }
    }

    fn resolve_upvalue(
        self: &mut FunctionScope,
        name: StringHandle,
        span: SourceSpan,
    ) -> Result<Option<VariableKind>, QangCompilerError> {
        // Check if this upvalue already exists in the function scope
        for (index, upvalue) in self.upvalues.iter().enumerate() {
            if upvalue.name == name {
                return Ok(Some(VariableKind::Upvalue {
                    index,
                    is_local: upvalue.is_local,
                }));
            }
        }

        if let Some(enclosing) = &mut self.enclosing {
            // Recursively check the next outer function
            if let Some(outer_upvalue) = Self::resolve_upvalue(enclosing, name, span)? {
                if let VariableKind::Upvalue { index, .. } = outer_upvalue {
                    // Add to current function scope
                    let upvalue_index = self.upvalues.len();
                    self.upvalues.push(UpvalueInfo {
                        name,
                        index,
                        is_local: false,
                    });

                    return Ok(Some(VariableKind::Upvalue {
                        index: upvalue_index,
                        is_local: false,
                    }));
                }
            }
        }

        Ok(None)
    }
}

pub struct ScopeAnalyzer<'a> {
    strings: &'a mut StringInterner,
    scopes: Vec<Scope>,
    current_function: Option<FunctionScope>,
    results: ScopeAnalysis,
}

impl<'a> ScopeAnalyzer<'a> {
    pub fn new(strings: &'a mut StringInterner) -> Self {
        Self {
            strings,
            scopes: vec![Scope::new()],
            current_function: None,
            results: ScopeAnalysis::new(),
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

        let _ = self.visit_program(program_node, &mut ctx);

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

    fn declare_variable(
        &mut self,
        identifier: crate::frontend::typed_node_arena::TypedNodeRef<
            crate::frontend::nodes::IdentifierNode,
        >,
    ) -> Result<(), QangCompilerError> {
        if self.current_scope_depth() == 0 {
            let var_info = VariableInfo {
                name: identifier.node.name,
                kind: VariableKind::Global,
                is_captured: false,
                span: identifier.node.span,
            };
            self.results.variables.insert(identifier.id, var_info);
            return Ok(());
        }

        if let Some(current_scope) = self.scopes.last() {
            if current_scope.variables.contains_key(&identifier.node.name) {
                return Err(QangCompilerError::new_analysis_error(
                    "Variable with this name already declared in this scope.".to_string(),
                    identifier.node.span,
                ));
            }
        }

        let slot = if let Some(func) = &mut self.current_function {
            let slot = func.local_count;
            func.local_count += 1;
            slot
        } else {
            0
        };

        let var_info = VariableInfo {
            name: identifier.node.name,
            kind: VariableKind::Local {
                scope_depth: self.current_scope_depth(),
                slot,
            },
            is_captured: false,
            span: identifier.node.span,
        };

        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope
                .variables
                .insert(identifier.node.name, var_info.clone());
        }

        self.results.variables.insert(identifier.id, var_info);
        Ok(())
    }

    fn resolve_variable(
        &mut self,
        name: StringHandle,
        span: SourceSpan,
        node_id: NodeId,
    ) -> Result<VariableKind, QangCompilerError> {
        // Search scopes from innermost to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(var_info) = scope.variables.get(&name) {
                let var_info_copy = VariableInfo {
                    name,
                    kind: var_info.kind.clone(),
                    is_captured: var_info.is_captured,
                    span,
                };
                self.results.variables.insert(node_id, var_info_copy);
                return Ok(var_info.kind.clone());
            }
        }

        // Try to resolve as upvalue
        if let Some(upvalue_kind) = self.resolve_upvalue(name, span)? {
            let var_info = VariableInfo {
                name,
                kind: upvalue_kind.clone(),
                is_captured: false,
                span,
            };
            self.results.variables.insert(node_id, var_info);
            return Ok(upvalue_kind);
        }

        // Global variable
        let var_info = VariableInfo {
            name,
            kind: VariableKind::Global,
            is_captured: false,
            span,
        };
        self.results.variables.insert(node_id, var_info);
        Ok(VariableKind::Global)
    }

    fn resolve_upvalue(
        &mut self,
        name: StringHandle,
        span: SourceSpan,
    ) -> Result<Option<VariableKind>, QangCompilerError> {
        // We need an enclosing function to have upvalues
        if self.current_function.is_none() {
            return Ok(None);
        }

        // Take ownership of current_function temporarily to avoid borrowing conflicts
        let mut current_function = self.current_function.take().unwrap();

        if let Some(enclosing) = &mut current_function.enclosing {
            // First check if it's a local variable in the immediately enclosing function's scopes
            // Search in current scopes for the variable
            for scope in self.scopes.iter_mut().rev() {
                if let Some(var_info) = scope.variables.get_mut(&name) {
                    // Mark the variable as captured
                    var_info.is_captured = true;

                    // Add upvalue to current function with is_local=true
                    let upvalue_index = current_function.upvalues.len();
                    if let VariableKind::Local { slot, .. } = var_info.kind {
                        current_function.upvalues.push(UpvalueInfo {
                            name,
                            index: slot,
                            is_local: true,
                        });

                        // Restore current_function
                        self.current_function = Some(current_function);

                        return Ok(Some(VariableKind::Upvalue {
                            index: upvalue_index,
                            is_local: true,
                        }));
                    }
                }
            }

            // If not found as local, recursively check outer scopes for upvalues
            if let Some(upvalue_kind) = enclosing.resolve_upvalue(name, span)? {
                if let VariableKind::Upvalue { index, .. } = upvalue_kind {
                    // Add upvalue to current function with is_local=false
                    let upvalue_index = current_function.upvalues.len();
                    current_function.upvalues.push(UpvalueInfo {
                        name,
                        index,
                        is_local: false,
                    });

                    // Restore current_function
                    self.current_function = Some(current_function);

                    return Ok(Some(VariableKind::Upvalue {
                        index: upvalue_index,
                        is_local: false,
                    }));
                }
            }
        }

        // Restore current_function
        self.current_function = Some(current_function);
        Ok(None)
    }

    fn begin_function(
        &mut self,
        name: StringHandle,
        arity: usize,
        span: SourceSpan,
        kind: FunctionKind,
        errors: &mut ErrorReporter,
    ) {
        if arity > 255 {
            errors.report_error(QangCompilerError::new_analysis_error(
                "Cannot have more than 255 parameters.".to_string(),
                span,
            ));
        }

        let mut new_function = FunctionScope::new(name, arity, span, kind);

        if let Some(current) = self.current_function.take() {
            new_function.enclosing = Some(Box::new(current));
        }

        self.current_function = Some(new_function);
        self.begin_scope();
    }

    fn end_function(&mut self, node_id: NodeId) -> Result<(), QangCompilerError> {
        self.end_scope();

        if let Some(mut function) = self.current_function.take() {
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

            // Restore enclosing function
            if let Some(enclosing) = function.enclosing.take() {
                self.current_function = Some(*enclosing);
            }
        }

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
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.resolve_variable(identifier.node.name, identifier.node.span, identifier.id)?;
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

        Ok(())
    }

    fn visit_function_expression(
        &mut self,
        func_expr: super::typed_node_arena::TypedNodeRef<super::nodes::FunctionExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let arity = ctx.nodes.array.size(func_expr.node.parameters);
        let identifier = ctx.nodes.get_identifier_node(func_expr.node.name);
        self.declare_variable(identifier)?;

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
        );

        for i in 0..arity {
            if let Some(param_id) = ctx.nodes.array.get_node_id_at(func_expr.node.parameters, i) {
                let param = ctx.nodes.get_identifier_node(param_id);
                self.declare_variable(param)?;
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

        self.begin_function(
            anonymous_name,
            arity,
            lambda_expr.node.span,
            FunctionKind::Function,
            ctx.errors,
        );

        for i in 0..arity {
            if let Some(param_id) = ctx
                .nodes
                .array
                .get_node_id_at(lambda_expr.node.parameters, i)
            {
                let param = ctx.nodes.get_identifier_node(param_id);
                self.declare_variable(param)?;
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

        // Analyze initializer first (if present)
        if let Some(initializer_id) = var_decl.node.initializer {
            let initializer = ctx.nodes.get_expr_node(initializer_id);
            self.visit_expression(initializer, ctx)?;
        }

        // Then declare the variable
        self.declare_variable(identifier)?;

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
        self.declare_variable(name_node)?;

        // Visit superclass if present
        if let Some(superclass_id) = class_decl.node.superclass {
            let superclass = ctx.nodes.get_identifier_node(superclass_id);
            self.visit_identifier(superclass, ctx)?;
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
                        );

                        // Visit parameters
                        for j in 0..arity {
                            if let Some(param_id) =
                                ctx.nodes.array.get_node_id_at(method.parameters, j)
                            {
                                let param = ctx.nodes.get_identifier_node(param_id);
                                self.declare_variable(param)?;
                            }
                        }

                        // Visit body
                        let body = ctx.nodes.get_block_stmt_node(method.body);
                        self.visit_block_statement(body, ctx)?;

                        self.end_function(member_id)?;
                    }
                    super::typed_node_arena::ClassMemberNode::Field(field) => {
                        let field_name = ctx.nodes.get_identifier_node(field.name);
                        self.visit_identifier(field_name, ctx)?;

                        if let Some(init_id) = field.initializer {
                            let initializer = ctx.nodes.get_expr_node(init_id);
                            self.visit_expression(initializer, ctx)?;
                        }
                    }
                }
            }
        }

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

        self.end_scope();
        Ok(())
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::ForStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();
        if let Some(initializer_id) = for_stmt.node.initializer {
            self.visit_for_initializer(ctx.nodes.get_for_initializer_node(initializer_id), ctx)?;
        }

        if let Some(condition_id) = for_stmt.node.condition {
            self.visit_expression(ctx.nodes.get_expr_node(condition_id), ctx)?;
        }

        if let Some(increment_id) = for_stmt.node.increment {
            self.visit_expression(ctx.nodes.get_expr_node(increment_id), ctx)?;
        }

        self.visit_statement(ctx.nodes.get_stmt_node(for_stmt.node.body), ctx)?;
        self.end_scope();
        Ok(())
    }
}
