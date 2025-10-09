use std::sync::Arc;

use crate::{
    AstNodeArena, ErrorReporter, NodeId, QangCompilerError, SourceMap,
    frontend::node_visitor::{NodeVisitor, VisitorContext},
    memory::StringInterner,
    nodes::SourceSpan,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionKind {
    Script,
    Function,
    Method,
    Initializer,
}

#[derive(Debug, Clone)]
struct Local {
    name: crate::StringHandle,
    depth: Option<usize>,
}

#[derive(Debug, Clone)]
struct FunctionContext {
    pub kind: FunctionKind,
    pub local_count: usize,
    pub locals: Vec<Local>,
}

impl FunctionContext {
    fn new(kind: FunctionKind, blank_handle: crate::StringHandle, this_handle: crate::StringHandle, scope_depth: usize) -> Self {
        let mut locals = Vec::with_capacity(256);
        let is_method = matches!(kind, FunctionKind::Method | FunctionKind::Initializer);

        if is_method {
            // For methods, slot 0 is "this" and is initialized
            locals.push(Local {
                name: this_handle,
                depth: Some(scope_depth),
            });
        } else {
            // For functions and scripts, slot 0 is a blank slot (not initialized initially)
            locals.push(Local {
                name: blank_handle,
                depth: None,
            });
        }

        Self {
            kind,
            local_count: 1,
            locals,
        }
    }

    fn add_local(&mut self, name: crate::StringHandle, depth: Option<usize>) {
        if self.local_count < self.locals.len() {
            self.locals[self.local_count] = Local { name, depth };
        } else {
            self.locals.push(Local { name, depth });
        }
        self.local_count += 1;
    }

    fn mark_initialized(&mut self, scope_depth: usize) {
        if self.local_count > 0 {
            if let Some(local) = self.locals.get_mut(self.local_count - 1) {
                local.depth = Some(scope_depth);
            }
        }
    }

    fn has_local_in_current_scope(&self, name: crate::StringHandle, scope_depth: usize) -> bool {
        for i in (0..self.local_count).rev() {
            if let Some(local) = self.locals.get(i) {
                if local
                    .depth
                    .map(|local_depth| local_depth < scope_depth)
                    .unwrap_or(false)
                {
                    break;
                }

                if local.name == name {
                    return true;
                }
            }
        }
        false
    }
}

pub struct SemanticValidator<'a> {
    source_map: Arc<SourceMap>,
    strings: &'a mut StringInterner,
    scope_depth: usize,
    functions: Vec<FunctionContext>,
    in_loop: bool,
    blank_handle: crate::StringHandle,
    this_handle: crate::StringHandle,
}

impl<'a> SemanticValidator<'a> {
    pub fn new(strings: &'a mut StringInterner, source_map: Arc<SourceMap>) -> Self {
        let blank_handle = strings.intern("");
        let this_handle = strings.intern("this");

        Self {
            scope_depth: 0,
            functions: vec![FunctionContext::new(FunctionKind::Script, blank_handle, this_handle, 0)],
            in_loop: false,
            source_map,
            strings,
            blank_handle,
            this_handle,
        }
    }

    pub fn analyze(
        mut self,
        program: NodeId,
        nodes: &mut AstNodeArena,
        errors: &mut ErrorReporter,
    ) {
        let mut ctx = VisitorContext::new(nodes, errors);
        let program_node = ctx.nodes.get_program_node(program);

        let _ = self.visit_module(program_node, &mut ctx);
    }

    fn current_scope_depth(&self) -> usize {
        self.scope_depth
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        if self.scope_depth > 0 {
            // Before decrementing scope_depth, remove locals from the current scope
            if let Some(function) = self.functions.last_mut() {
                let mut locals_to_remove = 0;

                for i in (0..function.local_count).rev() {
                    if let Some(local) = function.locals.get(i) {
                        if let Some(depth) = local.depth {
                            if depth >= self.scope_depth {
                                locals_to_remove += 1;
                            } else {
                                break;
                            }
                        } else {
                            // Uninitialized local - shouldn't happen at end_scope but handle it
                            break;
                        }
                    } else {
                        break;
                    }
                }

                function.local_count -= locals_to_remove;
            }

            self.scope_depth -= 1;
        }
    }

    fn begin_loop(&mut self) {
        self.in_loop = true;
    }

    fn end_loop(&mut self) {
        self.in_loop = false;
    }

    fn declare_variable(
        &mut self,
        identifier: crate::frontend::ast_node_arena::TypedNodeRef<
            crate::frontend::nodes::IdentifierNode,
        >,
    ) -> Result<(), QangCompilerError> {
        self.declare_variable_with_init(identifier, true)?;
        Ok(())
    }

    fn declare_variable_with_init(
        &mut self,
        identifier: crate::frontend::ast_node_arena::TypedNodeRef<
            crate::frontend::nodes::IdentifierNode,
        >,
        is_initialized: bool,
    ) -> Result<(), QangCompilerError> {
        // Only check for duplicates in local scope
        if self.current_scope_depth() > 0 {
            // Check for duplicate declarations in the current scope
            if let Some(function) = self.functions.last() {
                if function.has_local_in_current_scope(identifier.node.name, self.scope_depth) {
                    return Err(QangCompilerError::new_analysis_error(
                        "Already a variable with this name in this scope.".to_string(),
                        identifier.node.span,
                        self.source_map.clone(),
                    ));
                }
            }

            // Add the local variable
            if let Some(function) = self.functions.last_mut() {
                let depth = if is_initialized {
                    Some(self.scope_depth)
                } else {
                    None
                };
                function.add_local(identifier.node.name, depth);
            }
        }

        Ok(())
    }

    fn mark_initialized(&mut self) {
        if self.current_scope_depth() > 0 {
            if let Some(function) = self.functions.last_mut() {
                function.mark_initialized(self.scope_depth);
            }
        }
    }

    fn in_current_loop(&self) -> bool {
        self.in_loop
    }

    fn begin_function(
        &mut self,
        arity: usize,
        span: SourceSpan,
        kind: FunctionKind,
        errors: &mut ErrorReporter,
    ) {
        if arity > 256 {
            errors.report_error(QangCompilerError::new_analysis_error(
                "Cannot have more than 255 parameters.".to_string(),
                span,
                self.source_map.clone(),
            ));
        }

        self.in_loop = false;

        // Create new function context with scope_depth 0 (mimicking assembler's State::push)
        let function = FunctionContext::new(kind, self.blank_handle, self.this_handle, 0);
        self.functions.push(function);

        // Then increment scope depth for the function body
        self.begin_scope();
    }

    fn end_function(&mut self, _node_id: NodeId) -> Result<(), QangCompilerError> {
        self.end_scope();

        self.functions
            .pop()
            .expect("Expect function stack not to be empty.");

        Ok(())
    }
}

impl<'a> NodeVisitor for SemanticValidator<'a> {
    type Error = QangCompilerError;

    fn visit_array_literal(
        &mut self,
        array: super::ast_node_arena::TypedNodeRef<super::nodes::ArrayLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let size = ctx.nodes.array.size(array.node.elements);

        if size > u8::MAX as usize {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Array literal cannot have more than 256 elements.".to_string(),
                    array.node.span,
                    self.source_map.clone(),
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
        object: super::ast_node_arena::TypedNodeRef<super::nodes::ObjectLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let size = ctx.nodes.array.size(object.node.entries);

        if size > u8::MAX as usize {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Object literal cannot have more than 256 entries.".to_string(),
                    object.node.span,
                    self.source_map.clone(),
                ));
        }

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
        func_expr: super::ast_node_arena::TypedNodeRef<super::nodes::FunctionExprNode>,
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
                    self.source_map.clone(),
                ));
        }

        self.begin_function(
            arity,
            func_expr.node.span,
            FunctionKind::Function,
            ctx.errors,
        );

        for i in 0..arity {
            if let Some(param_id) = ctx.nodes.array.get_node_id_at(func_expr.node.parameters, i) {
                let param = ctx.nodes.get_identifier_node(param_id);
                if let Err(error) = self.declare_variable(param) {
                    ctx.errors.report_error(error);
                };
            }
        }

        let body = ctx.nodes.get_block_stmt_node(func_expr.node.body);
        self.visit_block_statement(body, ctx)?;

        self.end_function(func_expr.id)?;

        Ok(())
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: super::ast_node_arena::TypedNodeRef<super::nodes::LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let arity = ctx.nodes.array.size(lambda_expr.node.parameters);

        if arity > u8::MAX as usize {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Function call cannot have more than 256 arguments.".to_string(),
                    lambda_expr.node.span,
                    self.source_map.clone(),
                ));
        }

        self.begin_function(
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
                if let Err(error) = self.declare_variable(param) {
                    ctx.errors.report_error(error);
                };
            }
        }

        let body = ctx.nodes.get_lambda_body_node(lambda_expr.node.body);
        self.visit_lambda_body(body, ctx)?;

        self.end_function(lambda_expr.id)?;

        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        var_decl: super::ast_node_arena::TypedNodeRef<super::nodes::VariableDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(var_decl.node.target);

        // Declare the variable as uninitialized first
        if let Err(error) = self.declare_variable_with_init(identifier, false) {
            ctx.errors.report_error(error);
        }

        // Visit the initializer expression (if present)
        if let Some(initializer_id) = var_decl.node.initializer {
            let initializer = ctx.nodes.get_expr_node(initializer_id);
            self.visit_expression(initializer, ctx)?;

            // Mark the variable as initialized after visiting the initializer
            self.mark_initialized();
        } else {
            // Even without an initializer, we need to mark it as initialized
            // (the variable exists but has an undefined value)
            self.mark_initialized();
        }

        Ok(())
    }

    fn visit_import_module_declaration(
        &mut self,
        import_decl: super::ast_node_arena::TypedNodeRef<super::nodes::ImportModuleDeclNode>,
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
        call: super::ast_node_arena::TypedNodeRef<super::nodes::CallExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let callee = ctx.nodes.get_expr_node(call.node.callee);
        self.visit_expression(callee, ctx)?;

        let operation = ctx.nodes.get_call_operation_node(call.node.operation);
        match operation.node {
            super::nodes::CallOperationNode::Call(call_node) => {
                let arg_count = ctx.nodes.array.size(call_node.args);

                if arg_count > u8::MAX as usize {
                    ctx.errors
                        .report_error(QangCompilerError::new_analysis_error(
                            "Function call cannot have more than 256 arguments.".to_string(),
                            call_node.span,
                            self.source_map.clone(),
                        ));
                }

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
        class_decl: super::ast_node_arena::TypedNodeRef<super::nodes::ClassDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let name_node = ctx.nodes.get_identifier_node(class_decl.node.name);
        if let Err(error) = self.declare_variable(name_node) {
            ctx.errors.report_error(error);
        }

        let has_superclass = class_decl.node.superclass.is_some();
        if has_superclass {
            if let Some(superclass_id) = class_decl.node.superclass {
                let superclass = ctx.nodes.get_identifier_node(superclass_id);
                self.visit_identifier(superclass, ctx)?;
            }

            self.begin_scope();

            if let Some(func) = self.functions.last_mut() {
                let super_handle = self.strings.intern("super");
                func.add_local(super_handle, Some(self.scope_depth));
            }
        }

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

                        if arity > u8::MAX as usize {
                            ctx.errors
                                .report_error(QangCompilerError::new_analysis_error(
                                    "Function call cannot have more than 256 arguments."
                                        .to_string(),
                                    method.span,
                                    self.source_map.clone(),
                                ));
                        }

                        self.begin_function(
                            arity,
                            method.span,
                            if is_initializer {
                                FunctionKind::Initializer
                            } else {
                                FunctionKind::Method
                            },
                            ctx.errors,
                        );

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

                        let body = ctx.nodes.get_block_stmt_node(method.body);
                        self.visit_block_statement(body, ctx)?;

                        self.end_function(member_id)?;
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

        if has_superclass {
            self.end_scope();
        }

        Ok(())
    }

    fn visit_block_statement(
        &mut self,
        block_stmt: super::ast_node_arena::TypedNodeRef<super::nodes::BlockStmtNode>,
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

    fn visit_super_expression(
        &mut self,
        super_expr: super::ast_node_arena::TypedNodeRef<super::nodes::SuperExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if let Some(current_function) = &self.functions.last() {
            if !matches!(
                current_function.kind,
                FunctionKind::Method | FunctionKind::Initializer
            ) {
                ctx.errors
                    .report_error(QangCompilerError::new_analysis_error(
                        "Cannot use 'super' outside of a class method.".to_string(),
                        super_expr.node.span,
                        self.source_map.clone(),
                    ));
            }
        } else {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Cannot use 'super' outside of a class method.".to_string(),
                    super_expr.node.span,
                    self.source_map.clone(),
                ));
        }

        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: super::ast_node_arena::TypedNodeRef<super::nodes::WhileStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let condition = ctx.nodes.get_expr_node(while_stmt.node.condition);
        self.visit_expression(condition, ctx)?;

        self.begin_loop();
        let body = ctx.nodes.get_stmt_node(while_stmt.node.body);
        self.visit_statement(body, ctx)?;
        self.end_loop();

        Ok(())
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: super::ast_node_arena::TypedNodeRef<super::nodes::ForStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_loop();
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

        self.end_scope();
        self.end_loop();
        Ok(())
    }

    fn visit_break_statement(
        &mut self,
        break_stmt: super::ast_node_arena::TypedNodeRef<super::nodes::BreakStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if !self.in_current_loop() {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "'break' can only be used inside loops.".to_string(),
                    break_stmt.node.span,
                    self.source_map.clone(),
                ));
        }
        Ok(())
    }

    fn visit_continue_statement(
        &mut self,
        continue_stmt: super::ast_node_arena::TypedNodeRef<super::nodes::ContinueStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if !self.in_current_loop() {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "'continue' can only be used inside loops.".to_string(),
                    continue_stmt.node.span,
                    self.source_map.clone(),
                ));
        }
        Ok(())
    }

    fn visit_map_expression(
        &mut self,
        map_expr: super::ast_node_arena::TypedNodeRef<super::nodes::MapExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_function(1, map_expr.node.span, FunctionKind::Function, ctx.errors);

        let param = ctx.nodes.get_identifier_node(map_expr.node.parameter);
        if let Err(error) = self.declare_variable(param) {
            ctx.errors.report_error(error);
        };

        let body = ctx.nodes.get_expr_node(map_expr.node.body);
        self.visit_expression(body, ctx)?;

        self.end_function(map_expr.id)?;

        Ok(())
    }

    fn visit_optional_map_expression(
        &mut self,
        opt_map_expr: super::ast_node_arena::TypedNodeRef<super::nodes::OptionalMapExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_function(
            1,
            opt_map_expr.node.span,
            FunctionKind::Function,
            ctx.errors,
        );

        let param = ctx.nodes.get_identifier_node(opt_map_expr.node.parameter);
        if let Err(error) = self.declare_variable(param) {
            ctx.errors.report_error(error);
        };

        let body = ctx.nodes.get_expr_node(opt_map_expr.node.body);
        self.visit_expression(body, ctx)?;

        self.end_function(opt_map_expr.id)?;

        Ok(())
    }

    fn visit_call_operation(
        &mut self,
        operation: super::ast_node_arena::TypedNodeRef<super::nodes::CallOperationNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        use super::nodes::CallOperationNode;

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
            CallOperationNode::Property(_property) => Ok(()),
            CallOperationNode::OptionalProperty(_optional_property) => Ok(()),
            CallOperationNode::Index(index) => {
                self.visit_expression(ctx.nodes.get_expr_node(index.index), ctx)
            }
            CallOperationNode::Map(map) => self.visit_map_expression(
                super::ast_node_arena::TypedNodeRef::new(operation.id, map),
                ctx,
            ),
            CallOperationNode::OptionalMap(map) => self.visit_optional_map_expression(
                super::ast_node_arena::TypedNodeRef::new(operation.id, map),
                ctx,
            ),
        }
    }

    fn visit_identifier(
        &mut self,
        identifier: super::ast_node_arena::TypedNodeRef<super::nodes::IdentifierNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Check if we're trying to read a variable during its own initialization
        if self.current_scope_depth() > 0 {
            if let Some(function) = self.functions.last() {
                for i in (0..function.local_count).rev() {
                    if let Some(local) = function.locals.get(i) {
                        if local.name == identifier.node.name {
                            if local.depth.is_none() {
                                ctx.errors.report_error(QangCompilerError::new_analysis_error(
                                    "Cannot read local variable during its initialization.".to_string(),
                                    identifier.node.span,
                                    self.source_map.clone(),
                                ));
                            }
                            break;
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
