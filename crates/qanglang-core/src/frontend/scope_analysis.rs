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
        locals.push(LocalVariable {
            name: blank_handle,
            scope_depth: 0,
            slot: 0,
            is_captured: false,
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

    fn add_local(&mut self, name: StringHandle, scope_depth: usize) -> usize {
        let slot = self.local_count;
        let local_index = self.locals.len();

        self.locals.push(LocalVariable {
            name,
            scope_depth,
            slot,
            is_captured: false,
        });

        self.local_count += 1;

        local_index
    }

    fn find_local(&self, name: StringHandle) -> Option<usize> {
        self.locals
            .iter()
            .enumerate()
            .find_map(|(i, local)| if local.name == name { Some(i) } else { None })
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
    this_handle: StringHandle,
}

impl<'a> ScopeAnalyzer<'a> {
    pub fn new(strings: &'a mut StringInterner) -> Self {
        let handle = strings.intern("(script)");
        let blank_handle = strings.intern("");
        let this_handle = strings.intern("this");
        Self {
            blank_handle,
            this_handle,
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
        // TODO add variable to function scope
        // TODO add variable to variable results
        todo!()
    }

    fn resolve_variable(
        &mut self,
        name: StringHandle,
        span: SourceSpan,
        node_id: NodeId,
    ) -> Result<VariableKind, QangCompilerError> {
        todo!()
    }

    fn resolve_upvalue(
        &mut self,
        name: StringHandle,
        span: SourceSpan,
    ) -> Result<Option<VariableKind>, QangCompilerError> {
        todo!()
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

        let function = FunctionContext::new(name, arity, kind, self.blank_handle, span);

        self.functions.push(function);
        self.begin_scope();
    }

    fn end_function(&mut self, node_id: NodeId) -> Result<(), QangCompilerError> {
        self.end_scope();

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

        // Handle inheritance setup
        let has_superclass = class_decl.node.superclass.is_some();
        if has_superclass {
            // Visit superclass identifier
            if let Some(superclass_id) = class_decl.node.superclass {
                let superclass = ctx.nodes.get_identifier_node(superclass_id);
                self.visit_identifier(superclass, ctx)?;
            }

            // Begin new scope for super variable
            self.begin_scope();

            // Declare "super" as a local variable at slot 1 (slot 0 is reserved for 'this' in methods)
            let super_handle = self.strings.intern("super");

            // Special handling for super variable - assign it to slot 1
            let var_info = VariableInfo {
                name: super_handle,
                kind: VariableKind::Local {
                    scope_depth: self.current_scope_depth(),
                    slot: 1, // Hardcode slot 1 for super variable
                },
                is_captured: false,
                span: class_decl.node.span,
            };

            if let Some(current_scope) = self.scopes.last_mut() {
                current_scope
                    .variables
                    .insert(super_handle, var_info.clone());
            }

            // Also ensure current function's local_count accounts for slot 1
            if let Some(func) = self.functions.last_mut() {
                func.local_count = std::cmp::max(func.local_count, 2); // Ensure slots 0 and 1 are allocated
            }

            self.results.variables.insert(class_decl.id, var_info);
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

    fn visit_super_expression(
        &mut self,
        super_expr: super::typed_node_arena::TypedNodeRef<super::nodes::SuperExprNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Check if we're in a method or initializer context
        if let Some(current_function) = &self.functions.last() {
            if !matches!(
                current_function.kind,
                FunctionKind::Method | FunctionKind::Initializer
            ) {
                return Err(QangCompilerError::new_analysis_error(
                    "Cannot use 'super' outside of a class method.".to_string(),
                    super_expr.node.span,
                ));
            }
        } else {
            return Err(QangCompilerError::new_analysis_error(
                "Cannot use 'super' outside of a class method.".to_string(),
                super_expr.node.span,
            ));
        }

        // Resolve the 'super' variable access
        let super_handle = self.strings.intern("super");
        self.resolve_variable(super_handle, super_expr.node.span, super_expr.id)?;

        Ok(())
    }
}
