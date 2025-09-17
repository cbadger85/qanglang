use crate::{
    ErrorReporter, NodeId, QangCompilerError, TypedNodeArena,
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
struct FunctionContext {
    pub kind: FunctionKind,
    pub local_count: usize,
}

impl FunctionContext {
    fn new(kind: FunctionKind) -> Self {
        let local_count = 1;

        Self { kind, local_count }
    }

    fn add_local(&mut self) {
        self.local_count += 1;
    }
}

pub struct ScopeAnalyzer<'a> {
    strings: &'a mut StringInterner,
    scope_depth: usize,
    functions: Vec<FunctionContext>,
    in_loop: bool,
}

impl<'a> ScopeAnalyzer<'a> {
    pub fn new(strings: &'a mut StringInterner) -> Self {
        Self {
            strings,
            scope_depth: 0,
            functions: vec![FunctionContext::new(FunctionKind::Script)],
            in_loop: false,
        }
    }

    pub fn analyze(
        mut self,
        program: NodeId,
        nodes: &mut TypedNodeArena,
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
        identifier: crate::frontend::typed_node_arena::TypedNodeRef<
            crate::frontend::nodes::IdentifierNode,
        >,
    ) -> Result<(), QangCompilerError> {
        self.declare_variable_with_init(identifier, true)?;
        Ok(())
    }

    fn declare_variable_with_init(
        &mut self,
        _identifier: crate::frontend::typed_node_arena::TypedNodeRef<
            crate::frontend::nodes::IdentifierNode,
        >,
        _is_initialized: bool,
    ) -> Result<(), QangCompilerError> {
        if self.current_scope_depth() > 0
            && let Some(function) = self.functions.last_mut()
        {
            function.add_local();
        }

        Ok(())
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
            ));
        }

        let function = FunctionContext::new(kind);
        self.functions.push(function);

        self.in_loop = false;
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

impl<'a> NodeVisitor for ScopeAnalyzer<'a> {
    type Error = QangCompilerError;

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
        var_decl: super::typed_node_arena::TypedNodeRef<super::nodes::VariableDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(var_decl.node.target);

        if let Err(error) = self.declare_variable_with_init(identifier, false) {
            ctx.errors.report_error(error);
        }

        if let Some(initializer_id) = var_decl.node.initializer {
            let initializer = ctx.nodes.get_expr_node(initializer_id);
            self.visit_expression(initializer, ctx)?;
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
        let callee = ctx.nodes.get_expr_node(call.node.callee);
        self.visit_expression(callee, ctx)?;

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

        let has_superclass = class_decl.node.superclass.is_some();
        if has_superclass {
            if let Some(superclass_id) = class_decl.node.superclass {
                let superclass = ctx.nodes.get_identifier_node(superclass_id);
                self.visit_identifier(superclass, ctx)?;
            }

            self.begin_scope();

            if let Some(func) = self.functions.last_mut() {
                func.add_local();
            }
        }

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
                    super::typed_node_arena::ClassMemberNode::Field(field) => {
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

    fn visit_super_expression(
        &mut self,
        super_expr: super::typed_node_arena::TypedNodeRef<super::nodes::SuperExprNode>,
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
                    ));
            }
        } else {
            ctx.errors
                .report_error(QangCompilerError::new_analysis_error(
                    "Cannot use 'super' outside of a class method.".to_string(),
                    super_expr.node.span,
                ));
        }

        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::WhileStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let condition = ctx.nodes.get_expr_node(while_stmt.node.condition);
        self.visit_expression(condition, ctx)?;

        self.begin_loop();
        self.begin_scope();
        let body = ctx.nodes.get_stmt_node(while_stmt.node.body);
        self.visit_statement(body, ctx)?;
        self.end_scope();
        self.end_loop();

        Ok(())
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::ForStmtNode>,
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
        break_stmt: super::typed_node_arena::TypedNodeRef<super::nodes::BreakStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if !self.in_current_loop() {
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
        if !self.in_current_loop() {
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
        opt_map_expr: super::typed_node_arena::TypedNodeRef<super::nodes::OptionalMapExprNode>,
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
            CallOperationNode::Property(_property) => Ok(()),
            CallOperationNode::OptionalProperty(_optional_property) => Ok(()),
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
