use crate::{
    ErrorReporter,
    frontend::{
        ast_node_arena::{AstNodeArena, TypedNodeRef},
        nodes::*,
    },
};

pub struct VisitorContext<'a> {
    pub nodes: &'a mut AstNodeArena,
    pub errors: &'a mut ErrorReporter,
}

impl<'a> VisitorContext<'a> {
    pub fn new(nodes: &'a mut AstNodeArena, errors: &'a mut ErrorReporter) -> Self {
        Self { nodes, errors }
    }
}

pub trait NodeVisitor {
    type Error;

    fn visit_module(
        &mut self,
        program: TypedNodeRef<Module>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(program.node.decls);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(program.node.decls, i) {
                let decl_node = ctx.nodes.get_decl_node(node_id);
                self.visit_declaration(decl_node, ctx)?;
            }
        }

        Ok(())
    }

    fn visit_declaration(
        &mut self,
        decl: TypedNodeRef<DeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match decl.node {
            DeclNode::Class(class_decl) => {
                self.visit_class_declaration(TypedNodeRef::new(decl.id, class_decl), ctx)
            }
            DeclNode::Function(func_decl) => {
                self.visit_function_declaration(TypedNodeRef::new(decl.id, func_decl), ctx)
            }
            DeclNode::Lambda(lambda_decl) => {
                self.visit_lambda_declaration(TypedNodeRef::new(decl.id, lambda_decl), ctx)
            }
            DeclNode::Variable(var_decl) => {
                self.visit_variable_declaration(TypedNodeRef::new(decl.id, var_decl), ctx)
            }
            DeclNode::Module(import_decl) => {
                self.visit_import_module_declaration(TypedNodeRef::new(decl.id, import_decl), ctx)
            }
            DeclNode::Stmt(stmt) => self.visit_statement(TypedNodeRef::new(decl.id, stmt), ctx),
        }
    }

    fn visit_import_module_declaration(
        &mut self,
        import_decl: TypedNodeRef<ImportModuleDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(import_decl.node.name), ctx)
    }

    fn visit_class_declaration(
        &mut self,
        class_decl: TypedNodeRef<ClassDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(class_decl.node.name), ctx)?;

        if let Some(superclass_id) = class_decl.node.superclass {
            self.visit_identifier(ctx.nodes.get_identifier_node(superclass_id), ctx)?;
        }

        let length = ctx.nodes.array.size(class_decl.node.members);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(class_decl.node.members, i) {
                let class_member = ctx.nodes.get_class_member_node(node_id);
                self.visit_class_member(class_member, ctx)?;
            }
        }

        Ok(())
    }

    fn visit_class_member(
        &mut self,
        member: TypedNodeRef<ClassMemberNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match member.node {
            ClassMemberNode::Method(method) => {
                self.visit_function_expression(TypedNodeRef::new(member.id, method), ctx)
            }
            ClassMemberNode::Field(field) => {
                self.visit_field_declaration(TypedNodeRef::new(member.id, field), ctx)
            }
        }
    }

    fn visit_field_declaration(
        &mut self,
        field_decl: TypedNodeRef<FieldDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(field_decl.node.name), ctx)?;

        if let Some(initializer_id) = field_decl.node.initializer {
            self.visit_expression(ctx.nodes.get_expr_node(initializer_id), ctx)?;
        }

        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: TypedNodeRef<FunctionDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_function_expression(ctx.nodes.get_func_expr_node(func_decl.node.function), ctx)
    }

    fn visit_function_expression(
        &mut self,
        func_expr: TypedNodeRef<FunctionExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(func_expr.node.name), ctx)?;

        let length = ctx.nodes.array.size(func_expr.node.parameters);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(func_expr.node.parameters, i) {
                let param = ctx.nodes.get_identifier_node(node_id);
                self.visit_identifier(param, ctx)?;
            }
        }

        self.visit_block_statement(ctx.nodes.get_block_stmt_node(func_expr.node.body), ctx)
    }

    fn visit_lambda_declaration(
        &mut self,
        lambda_decl: TypedNodeRef<LambdaDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(lambda_decl.node.name), ctx)?;
        self.visit_lambda_expression(ctx.nodes.get_lambda_expr_node(lambda_decl.node.lambda), ctx)
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: TypedNodeRef<LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(lambda_expr.node.parameters);

        for i in 0..length {
            if let Some(node_id) = ctx
                .nodes
                .array
                .get_node_id_at(lambda_expr.node.parameters, i)
            {
                let param = ctx.nodes.get_identifier_node(node_id);
                self.visit_identifier(param, ctx)?;
            }
        }

        self.visit_lambda_body(ctx.nodes.get_lambda_body_node(lambda_expr.node.body), ctx)
    }

    fn visit_lambda_body(
        &mut self,
        body: TypedNodeRef<LambdaBodyNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match body.node {
            LambdaBodyNode::Block(block) => {
                self.visit_block_statement(TypedNodeRef::new(body.id, block), ctx)
            }
            LambdaBodyNode::Expr(expr) => {
                self.visit_expression(TypedNodeRef::new(body.id, expr), ctx)
            }
        }
    }

    fn visit_variable_declaration(
        &mut self,
        var_decl: TypedNodeRef<VariableDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(var_decl.node.target), ctx)?;

        if let Some(initializer_id) = var_decl.node.initializer {
            self.visit_expression(ctx.nodes.get_expr_node(initializer_id), ctx)?;
        }

        Ok(())
    }

    fn visit_statement(
        &mut self,
        stmt: TypedNodeRef<StmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match stmt.node {
            StmtNode::Expr(expr_stmt) => {
                self.visit_expression_statement(TypedNodeRef::new(stmt.id, expr_stmt), ctx)
            }
            StmtNode::Block(block_stmt) => {
                self.visit_block_statement(TypedNodeRef::new(stmt.id, block_stmt), ctx)
            }
            StmtNode::If(if_stmt) => {
                self.visit_if_statement(TypedNodeRef::new(stmt.id, if_stmt), ctx)
            }
            StmtNode::While(while_stmt) => {
                self.visit_while_statement(TypedNodeRef::new(stmt.id, while_stmt), ctx)
            }
            StmtNode::For(for_stmt) => {
                self.visit_for_statement(TypedNodeRef::new(stmt.id, for_stmt), ctx)
            }
            StmtNode::Break(break_stmt) => {
                self.visit_break_statement(TypedNodeRef::new(stmt.id, break_stmt), ctx)
            }
            StmtNode::Continue(continue_stmt) => {
                self.visit_continue_statement(TypedNodeRef::new(stmt.id, continue_stmt), ctx)
            }
            StmtNode::Return(return_stmt) => {
                self.visit_return_statement(TypedNodeRef::new(stmt.id, return_stmt), ctx)
            }
        }
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: TypedNodeRef<ExprStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(expr_stmt.node.expr), ctx)
    }

    fn visit_block_statement(
        &mut self,
        block_stmt: TypedNodeRef<BlockStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(block_stmt.node.decls);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(block_stmt.node.decls, i) {
                let decl = ctx.nodes.get_decl_node(node_id);
                self.visit_declaration(decl, ctx)?;
            }
        }

        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        if_stmt: TypedNodeRef<IfStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(if_stmt.node.condition), ctx)?;
        self.visit_statement(ctx.nodes.get_stmt_node(if_stmt.node.then_branch), ctx)?;

        if let Some(else_branch_id) = if_stmt.node.else_branch {
            self.visit_statement(ctx.nodes.get_stmt_node(else_branch_id), ctx)?;
        }

        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: TypedNodeRef<WhileStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(while_stmt.node.condition), ctx)?;
        self.visit_statement(ctx.nodes.get_stmt_node(while_stmt.node.body), ctx)
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: TypedNodeRef<ForStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if let Some(initializer_id) = for_stmt.node.initializer {
            self.visit_for_initializer(ctx.nodes.get_for_initializer_node(initializer_id), ctx)?;
        }

        if let Some(condition_id) = for_stmt.node.condition {
            self.visit_expression(ctx.nodes.get_expr_node(condition_id), ctx)?;
        }

        if let Some(increment_id) = for_stmt.node.increment {
            self.visit_expression(ctx.nodes.get_expr_node(increment_id), ctx)?;
        }

        self.visit_statement(ctx.nodes.get_stmt_node(for_stmt.node.body), ctx)
    }

    fn visit_for_initializer(
        &mut self,
        initializer: TypedNodeRef<ForInitializerNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match initializer.node {
            ForInitializerNode::VarDecl(var_decl) => {
                self.visit_variable_declaration(TypedNodeRef::new(initializer.id, var_decl), ctx)
            }
            ForInitializerNode::Expr(expr) => {
                self.visit_expression(TypedNodeRef::new(initializer.id, expr), ctx)
            }
        }
    }

    fn visit_break_statement(
        &mut self,
        _break_stmt: TypedNodeRef<BreakStmtNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_continue_statement(
        &mut self,
        _continue_stmt: TypedNodeRef<ContinueStmtNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: TypedNodeRef<ReturnStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if let Some(value_id) = return_stmt.node.value {
            self.visit_expression(ctx.nodes.get_expr_node(value_id), ctx)?;
        }
        Ok(())
    }

    fn visit_expression(
        &mut self,
        expr: TypedNodeRef<ExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match expr.node {
            ExprNode::Assignment(assignment) => {
                self.visit_assignment_expression(TypedNodeRef::new(expr.id, assignment), ctx)
            }
            ExprNode::Pipe(pipe) => {
                self.visit_pipe_expression(TypedNodeRef::new(expr.id, pipe), ctx)
            }
            ExprNode::Ternary(ternary) => {
                self.visit_ternary_expression(TypedNodeRef::new(expr.id, ternary), ctx)
            }
            ExprNode::LogicalOr(logical_or) => {
                self.visit_logical_or_expression(TypedNodeRef::new(expr.id, logical_or), ctx)
            }
            ExprNode::LogicalAnd(logical_and) => {
                self.visit_logical_and_expression(TypedNodeRef::new(expr.id, logical_and), ctx)
            }
            ExprNode::Equality(equality) => {
                self.visit_equality_expression(TypedNodeRef::new(expr.id, equality), ctx)
            }
            ExprNode::Comparison(comparison) => {
                self.visit_comparison_expression(TypedNodeRef::new(expr.id, comparison), ctx)
            }
            ExprNode::Term(term) => {
                self.visit_term_expression(TypedNodeRef::new(expr.id, term), ctx)
            }
            ExprNode::Factor(factor) => {
                self.visit_factor_expression(TypedNodeRef::new(expr.id, factor), ctx)
            }
            ExprNode::Unary(unary) => {
                self.visit_unary_expression(TypedNodeRef::new(expr.id, unary), ctx)
            }
            ExprNode::Call(call) => {
                self.visit_call_expression(TypedNodeRef::new(expr.id, call), ctx)
            }
            ExprNode::Primary(primary) => {
                self.visit_primary_expression(TypedNodeRef::new(expr.id, primary), ctx)
            }
        }
    }

    fn visit_assignment_expression(
        &mut self,
        assignment: TypedNodeRef<AssignmentExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_assignment_target(
            ctx.nodes.get_assignment_target_node(assignment.node.target),
            ctx,
        )?;
        self.visit_expression(ctx.nodes.get_expr_node(assignment.node.value), ctx)
    }

    fn visit_assignment_target(
        &mut self,
        target: TypedNodeRef<AssignmentTargetNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match target.node {
            AssignmentTargetNode::Identifier(identifier) => {
                self.visit_identifier(TypedNodeRef::new(target.id, identifier), ctx)
            }
            AssignmentTargetNode::Property(property) => {
                self.visit_property_assignment(TypedNodeRef::new(target.id, property), ctx)
            }
            AssignmentTargetNode::Index(index) => {
                self.visit_index_assignment(TypedNodeRef::new(target.id, index), ctx)
            }
        }
    }

    fn visit_property_assignment(
        &mut self,
        property: TypedNodeRef<PropertyAssignmentNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(property.node.object), ctx)
    }

    fn visit_index_assignment(
        &mut self,
        index: TypedNodeRef<IndexAssignmentNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(index.node.object), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(index.node.index), ctx)
    }

    fn visit_pipe_expression(
        &mut self,
        pipe: TypedNodeRef<PipeExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(pipe.node.left), ctx)?;

        self.visit_expression(ctx.nodes.get_expr_node(pipe.node.right), ctx)?;

        Ok(())
    }

    fn visit_ternary_expression(
        &mut self,
        ternary: TypedNodeRef<TernaryExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(ternary.node.condition), ctx)?;

        self.visit_expression(ctx.nodes.get_expr_node(ternary.node.then_expr), ctx)?;

        self.visit_expression(ctx.nodes.get_expr_node(ternary.node.else_expr), ctx)?;

        Ok(())
    }

    fn visit_logical_or_expression(
        &mut self,
        logical_or: TypedNodeRef<LogicalOrExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(logical_or.node.left), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(logical_or.node.right), ctx)
    }

    fn visit_logical_and_expression(
        &mut self,
        logical_and: TypedNodeRef<LogicalAndExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(logical_and.node.left), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(logical_and.node.right), ctx)
    }

    fn visit_equality_expression(
        &mut self,
        equality: TypedNodeRef<EqualityExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(equality.node.left), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(equality.node.right), ctx)
    }

    fn visit_comparison_expression(
        &mut self,
        comparison: TypedNodeRef<ComparisonExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(comparison.node.left), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(comparison.node.right), ctx)
    }

    fn visit_term_expression(
        &mut self,
        term: TypedNodeRef<TermExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(term.node.left), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(term.node.right), ctx)
    }

    fn visit_factor_expression(
        &mut self,
        factor: TypedNodeRef<FactorExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(factor.node.left), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(factor.node.right), ctx)
    }

    fn visit_unary_expression(
        &mut self,
        unary: TypedNodeRef<UnaryExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(unary.node.operand), ctx)
    }

    fn visit_call_expression(
        &mut self,
        call: TypedNodeRef<CallExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(call.node.callee), ctx)?;
        self.visit_call_operation(ctx.nodes.get_call_operation_node(call.node.operation), ctx)
    }

    fn visit_call_operation(
        &mut self,
        operation: TypedNodeRef<CallOperationNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match operation.node {
            CallOperationNode::Call(call) => {
                let length = ctx.nodes.array.size(call.args);

                for i in 0..length {
                    if let Some(node_id) = ctx.nodes.array.get_node_id_at(call.args, i) {
                        let arg = ctx.nodes.get_expr_node(node_id);
                        self.visit_expression(arg, ctx)?;
                    }
                }

                Ok(())
            }
            CallOperationNode::Property(property) => {
                self.visit_identifier(ctx.nodes.get_identifier_node(property.identifier), ctx)
            }
            CallOperationNode::OptionalProperty(optional_property) => self.visit_identifier(
                ctx.nodes.get_identifier_node(optional_property.identifier),
                ctx,
            ),
            CallOperationNode::Index(index) => {
                self.visit_expression(ctx.nodes.get_expr_node(index.index), ctx)
            }
            CallOperationNode::Map(map) => {
                self.visit_map_expression(TypedNodeRef::new(operation.id, map), ctx)
            }
            CallOperationNode::OptionalMap(map) => {
                self.visit_optional_map_expression(TypedNodeRef::new(operation.id, map), ctx)
            }
        }
    }

    fn visit_primary_expression(
        &mut self,
        primary: TypedNodeRef<PrimaryNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        match primary.node {
            PrimaryNode::Number(number) => {
                self.visit_number_literal(TypedNodeRef::new(primary.id, number), ctx)
            }
            PrimaryNode::String(string) => {
                self.visit_string_literal(TypedNodeRef::new(primary.id, string), ctx)
            }
            PrimaryNode::Boolean(boolean) => {
                self.visit_boolean_literal(TypedNodeRef::new(primary.id, boolean), ctx)
            }
            PrimaryNode::Nil(nil) => {
                self.visit_nil_literal(TypedNodeRef::new(primary.id, nil), ctx)
            }
            PrimaryNode::This(this_expr) => {
                self.visit_this_expression(TypedNodeRef::new(primary.id, this_expr), ctx)
            }
            PrimaryNode::Super(super_expr) => {
                self.visit_super_expression(TypedNodeRef::new(primary.id, super_expr), ctx)
            }
            PrimaryNode::Identifier(identifier) => {
                self.visit_identifier(TypedNodeRef::new(primary.id, identifier), ctx)
            }
            PrimaryNode::Grouping(grouping) => {
                self.visit_grouping_expression(TypedNodeRef::new(primary.id, grouping), ctx)
            }
            PrimaryNode::Lambda(lambda) => {
                self.visit_lambda_expression(TypedNodeRef::new(primary.id, lambda), ctx)
            }
            PrimaryNode::Array(array) => {
                self.visit_array_literal(TypedNodeRef::new(primary.id, array), ctx)
            }
            PrimaryNode::Object(object) => {
                self.visit_object_literal(TypedNodeRef::new(primary.id, object), ctx)
            }
        }
    }

    fn visit_number_literal(
        &mut self,
        _number: TypedNodeRef<NumberLiteralNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_string_literal(
        &mut self,
        _string: TypedNodeRef<StringLiteralNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_boolean_literal(
        &mut self,
        _boolean: TypedNodeRef<BooleanLiteralNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_nil_literal(
        &mut self,
        _nil: TypedNodeRef<NilLiteralNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_this_expression(
        &mut self,
        _this_expr: TypedNodeRef<ThisExprNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_super_expression(
        &mut self,
        super_expr: TypedNodeRef<SuperExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(super_expr.node.method), ctx)
    }

    fn visit_identifier(
        &mut self,
        _identifier: TypedNodeRef<IdentifierNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_grouping_expression(
        &mut self,
        grouping: TypedNodeRef<GroupingExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_expression(ctx.nodes.get_expr_node(grouping.node.expr), ctx)
    }

    fn visit_array_literal(
        &mut self,
        array: TypedNodeRef<ArrayLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(array.node.elements);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(array.node.elements, i) {
                let element = ctx.nodes.get_expr_node(node_id);
                self.visit_expression(element, ctx)?;
            }
        }

        Ok(())
    }

    fn visit_object_literal(
        &mut self,
        object: TypedNodeRef<ObjectLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(object.node.entries);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(object.node.entries, i) {
                let entry = ctx.nodes.get_obj_entry_node(node_id);
                self.visit_object_entry(entry, ctx)?;
            }
        }

        Ok(())
    }

    fn visit_object_entry(
        &mut self,
        entry: TypedNodeRef<ObjectEntryNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(entry.node.key), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(entry.node.value), ctx)
    }

    fn visit_map_expression(
        &mut self,
        map: TypedNodeRef<MapExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(map.node.parameter), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(map.node.body), ctx)
    }

    fn visit_optional_map_expression(
        &mut self,
        map: TypedNodeRef<OptionalMapExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(ctx.nodes.get_identifier_node(map.node.parameter), ctx)?;
        self.visit_expression(ctx.nodes.get_expr_node(map.node.body), ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nodes::SourceSpan;

    struct IdentifierCounter {
        count: usize,
    }

    impl IdentifierCounter {
        fn new() -> Self {
            Self { count: 0 }
        }
    }

    impl NodeVisitor for IdentifierCounter {
        type Error = ();

        fn visit_identifier(
            &mut self,
            _identifier: TypedNodeRef<IdentifierNode>,
            _ctx: &mut VisitorContext,
        ) -> Result<(), Self::Error> {
            self.count += 1;
            Ok(())
        }
    }

    #[test]
    fn test_node_visitor() {
        let mut arena = AstNodeArena::new();
        let identifier = IdentifierNode {
            name: 0,
            span: SourceSpan::new(0, 4),
        };
        let id = arena.create_node(AstNode::Identifier(identifier));
        let mut counter = IdentifierCounter::new();
        let mut errors = ErrorReporter::new();
        let mut ctx = VisitorContext::new(&mut arena, &mut errors);

        let typed_ref = ctx.nodes.get_identifier_node(id);

        counter.visit_identifier(typed_ref, &mut ctx).unwrap();
        assert_eq!(counter.count, 1);
    }
}
