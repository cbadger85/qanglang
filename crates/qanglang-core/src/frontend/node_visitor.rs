use crate::{
    ErrorReporter,
    frontend::{
        nodes::*,
        typed_node_arena::{
            AssignmentTargetNode, CallOperationNode, ClassMember, DeclNode, ExprNode,
            ForInitializerNode, LambdaBodyNode, NodeId, PrimaryNode, StmtNode, TypedNodeArena,
            TypedNodeRef,
        },
    },
};

pub trait NodeVisitor {
    type Error;

    fn visit_program(
        &mut self,
        program: TypedNodeRef<ProgramNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for decl in arena.iter_decl_nodes(program.node.decls) {
            self.visit_declaration(decl, arena, errors)?;
        }
        Ok(())
    }

    fn visit_declaration(
        &mut self,
        decl: TypedNodeRef<DeclNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match decl.node {
            DeclNode::Class(class_decl) => {
                self.visit_class_declaration(TypedNodeRef::new(decl.id, class_decl), arena, errors)
            }
            DeclNode::Function(func_decl) => self.visit_function_declaration(
                TypedNodeRef::new(decl.id, func_decl),
                arena,
                errors,
            ),
            DeclNode::Lambda(lambda_decl) => self.visit_lambda_declaration(
                TypedNodeRef::new(decl.id, lambda_decl),
                arena,
                errors,
            ),
            DeclNode::Variable(var_decl) => {
                self.visit_variable_declaration(TypedNodeRef::new(decl.id, var_decl), arena, errors)
            }
            DeclNode::Stmt(stmt) => {
                self.visit_statement(TypedNodeRef::new(decl.id, stmt), arena, errors)
            }
        }
    }

    fn visit_class_declaration(
        &mut self,
        class_decl: TypedNodeRef<ClassDeclNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(
            arena.get_identifier_node(class_decl.node.name),
            arena,
            errors,
        )?;

        if let Some(superclass_id) = class_decl.node.superclass {
            self.visit_identifier(arena.get_identifier_node(superclass_id), arena, errors)?;
        }

        for member in arena.iter_class_member_nodes(class_decl.node.members) {
            self.visit_class_member(member, arena, errors)?;
        }

        Ok(())
    }

    fn visit_class_member(
        &mut self,
        member: TypedNodeRef<ClassMember>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match member.node {
            ClassMember::Method(method) => {
                self.visit_function_expression(TypedNodeRef::new(member.id, method), arena, errors)
            }
            ClassMember::Field(field) => {
                self.visit_field_declaration(TypedNodeRef::new(member.id, field), arena, errors)
            }
        }
    }

    fn visit_field_declaration(
        &mut self,
        field_decl: TypedNodeRef<FieldDeclNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(
            arena.get_identifier_node(field_decl.node.name),
            arena,
            errors,
        )?;

        if let Some(initializer_id) = field_decl.node.initializer {
            self.visit_expression(arena.get_expr_node(initializer_id), arena, errors)?;
        }

        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: TypedNodeRef<FunctionDeclNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_function_expression(
            arena.get_func_expr_node(func_decl.node.function),
            arena,
            errors,
        )
    }

    fn visit_function_expression(
        &mut self,
        func_expr: TypedNodeRef<FunctionExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(
            arena.get_identifier_node(func_expr.node.name),
            arena,
            errors,
        )?;

        for param in arena.iter_identifier_nodes(func_expr.node.parameters) {
            self.visit_identifier(param, arena, errors)?;
        }

        self.visit_block_statement(
            arena.get_block_stmt_node(func_expr.node.body),
            arena,
            errors,
        )
    }

    fn visit_lambda_declaration(
        &mut self,
        lambda_decl: TypedNodeRef<LambdaDeclNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(
            arena.get_identifier_node(lambda_decl.node.name),
            arena,
            errors,
        )?;
        self.visit_lambda_expression(
            arena.get_lambda_expr_node(lambda_decl.node.lambda),
            arena,
            errors,
        )
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: TypedNodeRef<LambdaExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for param in arena.iter_identifier_nodes(lambda_expr.node.parameters) {
            self.visit_identifier(param, arena, errors)?;
        }

        self.visit_lambda_body(
            arena.get_lambda_body_node(lambda_expr.node.body),
            arena,
            errors,
        )
    }

    fn visit_lambda_body(
        &mut self,
        body: TypedNodeRef<LambdaBodyNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match body.node {
            LambdaBodyNode::Block(block) => {
                self.visit_block_statement(TypedNodeRef::new(body.id, block), arena, errors)
            }
            LambdaBodyNode::Expr(expr) => {
                self.visit_expression(TypedNodeRef::new(body.id, expr), arena, errors)
            }
        }
    }

    fn visit_variable_declaration(
        &mut self,
        var_decl: TypedNodeRef<VariableDeclNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(
            arena.get_identifier_node(var_decl.node.target),
            arena,
            errors,
        )?;

        if let Some(initializer_id) = var_decl.node.initializer {
            self.visit_expression(arena.get_expr_node(initializer_id), arena, errors)?;
        }

        Ok(())
    }

    fn visit_statement(
        &mut self,
        stmt: TypedNodeRef<StmtNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match stmt.node {
            StmtNode::Expr(expr_stmt) => self.visit_expression_statement(
                TypedNodeRef::new(stmt.id, expr_stmt),
                arena,
                errors,
            ),
            StmtNode::Block(block_stmt) => {
                self.visit_block_statement(TypedNodeRef::new(stmt.id, block_stmt), arena, errors)
            }
            StmtNode::If(if_stmt) => {
                self.visit_if_statement(TypedNodeRef::new(stmt.id, if_stmt), arena, errors)
            }
            StmtNode::While(while_stmt) => {
                self.visit_while_statement(TypedNodeRef::new(stmt.id, while_stmt), arena, errors)
            }
            StmtNode::For(for_stmt) => {
                self.visit_for_statement(TypedNodeRef::new(stmt.id, for_stmt), arena, errors)
            }
            StmtNode::Break(break_stmt) => {
                self.visit_break_statement(TypedNodeRef::new(stmt.id, break_stmt), arena, errors)
            }
            StmtNode::Continue(continue_stmt) => self.visit_continue_statement(
                TypedNodeRef::new(stmt.id, continue_stmt),
                arena,
                errors,
            ),
            StmtNode::Return(return_stmt) => {
                self.visit_return_statement(TypedNodeRef::new(stmt.id, return_stmt), arena, errors)
            }
        }
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: TypedNodeRef<ExprStmtNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(expr_stmt.node.expr), arena, errors)
    }

    fn visit_block_statement(
        &mut self,
        block_stmt: TypedNodeRef<BlockStmtNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for decl in arena.iter_decl_nodes(block_stmt.node.decls) {
            self.visit_declaration(decl, arena, errors)?;
        }
        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        if_stmt: TypedNodeRef<IfStmtNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(if_stmt.node.condition), arena, errors)?;
        self.visit_statement(arena.get_stmt_node(if_stmt.node.then_branch), arena, errors)?;

        if let Some(else_branch_id) = if_stmt.node.else_branch {
            self.visit_statement(arena.get_stmt_node(else_branch_id), arena, errors)?;
        }

        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: TypedNodeRef<WhileStmtNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(
            arena.get_expr_node(while_stmt.node.condition),
            arena,
            errors,
        )?;
        self.visit_statement(arena.get_stmt_node(while_stmt.node.body), arena, errors)
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: TypedNodeRef<ForStmtNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if let Some(initializer_id) = for_stmt.node.initializer {
            self.visit_for_initializer(
                arena.get_for_initializer_node(initializer_id),
                arena,
                errors,
            )?;
        }

        if let Some(condition_id) = for_stmt.node.condition {
            self.visit_expression(arena.get_expr_node(condition_id), arena, errors)?;
        }

        if let Some(increment_id) = for_stmt.node.increment {
            self.visit_expression(arena.get_expr_node(increment_id), arena, errors)?;
        }

        self.visit_statement(arena.get_stmt_node(for_stmt.node.body), arena, errors)
    }

    fn visit_for_initializer(
        &mut self,
        initializer: TypedNodeRef<ForInitializerNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match initializer.node {
            ForInitializerNode::VarDecl(var_decl) => self.visit_variable_declaration(
                TypedNodeRef::new(initializer.id, var_decl),
                arena,
                errors,
            ),
            ForInitializerNode::Expr(expr) => {
                self.visit_expression(TypedNodeRef::new(initializer.id, expr), arena, errors)
            }
        }
    }

    fn visit_break_statement(
        &mut self,
        _break_stmt: TypedNodeRef<BreakStmtNode>,
        _arena: &TypedNodeArena,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_continue_statement(
        &mut self,
        _continue_stmt: TypedNodeRef<ContinueStmtNode>,
        _arena: &TypedNodeArena,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: TypedNodeRef<ReturnStmtNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if let Some(value_id) = return_stmt.node.value {
            self.visit_expression(arena.get_expr_node(value_id), arena, errors)?;
        }
        Ok(())
    }

    fn visit_expression(
        &mut self,
        expr: TypedNodeRef<ExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match expr.node {
            ExprNode::Assignment(assignment) => self.visit_assignment_expression(
                TypedNodeRef::new(expr.id, assignment),
                arena,
                errors,
            ),
            ExprNode::Pipe(pipe) => {
                self.visit_pipe_expression(TypedNodeRef::new(expr.id, pipe), arena, errors)
            }
            ExprNode::Ternary(ternary) => {
                self.visit_ternary_expression(TypedNodeRef::new(expr.id, ternary), arena, errors)
            }
            ExprNode::LogicalOr(logical_or) => self.visit_logical_or_expression(
                TypedNodeRef::new(expr.id, logical_or),
                arena,
                errors,
            ),
            ExprNode::LogicalAnd(logical_and) => self.visit_logical_and_expression(
                TypedNodeRef::new(expr.id, logical_and),
                arena,
                errors,
            ),
            ExprNode::Equality(equality) => {
                self.visit_equality_expression(TypedNodeRef::new(expr.id, equality), arena, errors)
            }
            ExprNode::Comparison(comparison) => self.visit_comparison_expression(
                TypedNodeRef::new(expr.id, comparison),
                arena,
                errors,
            ),
            ExprNode::Term(term) => {
                self.visit_term_expression(TypedNodeRef::new(expr.id, term), arena, errors)
            }
            ExprNode::Factor(factor) => {
                self.visit_factor_expression(TypedNodeRef::new(expr.id, factor), arena, errors)
            }
            ExprNode::Unary(unary) => {
                self.visit_unary_expression(TypedNodeRef::new(expr.id, unary), arena, errors)
            }
            ExprNode::Call(call) => {
                self.visit_call_expression(TypedNodeRef::new(expr.id, call), arena, errors)
            }
            ExprNode::Primary(primary) => {
                self.visit_primary_expression(TypedNodeRef::new(expr.id, primary), arena, errors)
            }
        }
    }

    fn visit_assignment_expression(
        &mut self,
        assignment: TypedNodeRef<AssignmentExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_assignment_target(
            arena.get_assignment_target_node(assignment.node.target),
            arena,
            errors,
        )?;
        self.visit_expression(arena.get_expr_node(assignment.node.value), arena, errors)
    }

    fn visit_assignment_target(
        &mut self,
        target: TypedNodeRef<AssignmentTargetNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match target.node {
            AssignmentTargetNode::Identifier(identifier) => {
                self.visit_identifier(TypedNodeRef::new(target.id, identifier), arena, errors)
            }
            AssignmentTargetNode::Property(property) => self.visit_property_assignment(
                TypedNodeRef::new(target.id, property),
                arena,
                errors,
            ),
            AssignmentTargetNode::Index(index) => {
                self.visit_index_assignment(TypedNodeRef::new(target.id, index), arena, errors)
            }
        }
    }

    fn visit_property_assignment(
        &mut self,
        property: TypedNodeRef<PropertyAssignmentNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(property.node.object), arena, errors)
    }

    fn visit_index_assignment(
        &mut self,
        index: TypedNodeRef<IndexAssignmentNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(index.node.object), arena, errors)?;
        self.visit_expression(arena.get_expr_node(index.node.index), arena, errors)
    }

    fn visit_pipe_expression(
        &mut self,
        pipe: TypedNodeRef<PipeExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(pipe.node.left), arena, errors)?;

        if let Some(right_id) = pipe.node.right {
            self.visit_expression(arena.get_expr_node(right_id), arena, errors)?;
        }

        Ok(())
    }

    fn visit_ternary_expression(
        &mut self,
        ternary: TypedNodeRef<TernaryExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(ternary.node.condition), arena, errors)?;

        if let Some(then_expr_id) = ternary.node.then_expr {
            self.visit_expression(arena.get_expr_node(then_expr_id), arena, errors)?;
        }

        if let Some(else_expr_id) = ternary.node.else_expr {
            self.visit_expression(arena.get_expr_node(else_expr_id), arena, errors)?;
        }

        Ok(())
    }

    fn visit_logical_or_expression(
        &mut self,
        logical_or: TypedNodeRef<LogicalOrExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(logical_or.node.left), arena, errors)?;
        self.visit_expression(arena.get_expr_node(logical_or.node.right), arena, errors)
    }

    fn visit_logical_and_expression(
        &mut self,
        logical_and: TypedNodeRef<LogicalAndExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(logical_and.node.left), arena, errors)?;
        self.visit_expression(arena.get_expr_node(logical_and.node.right), arena, errors)
    }

    fn visit_equality_expression(
        &mut self,
        equality: TypedNodeRef<EqualityExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(equality.node.left), arena, errors)?;
        self.visit_expression(arena.get_expr_node(equality.node.right), arena, errors)
    }

    fn visit_comparison_expression(
        &mut self,
        comparison: TypedNodeRef<ComparisonExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(comparison.node.left), arena, errors)?;
        self.visit_expression(arena.get_expr_node(comparison.node.right), arena, errors)
    }

    fn visit_term_expression(
        &mut self,
        term: TypedNodeRef<TermExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(term.node.left), arena, errors)?;
        self.visit_expression(arena.get_expr_node(term.node.right), arena, errors)
    }

    fn visit_factor_expression(
        &mut self,
        factor: TypedNodeRef<FactorExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(factor.node.left), arena, errors)?;
        self.visit_expression(arena.get_expr_node(factor.node.right), arena, errors)
    }

    fn visit_unary_expression(
        &mut self,
        unary: TypedNodeRef<UnaryExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(unary.node.operand), arena, errors)
    }

    fn visit_call_expression(
        &mut self,
        call: TypedNodeRef<CallExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(call.node.callee), arena, errors)?;
        self.visit_call_operation(
            arena.get_call_operation_node(call.node.operation),
            arena,
            errors,
        )
    }

    fn visit_call_operation(
        &mut self,
        operation: TypedNodeRef<CallOperationNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match operation.node {
            CallOperationNode::Call(call) => {
                for arg in arena.iter_expr_nodes(call.args) {
                    self.visit_expression(arg, arena, errors)?;
                }
                Ok(())
            }
            CallOperationNode::Property(property) => self.visit_identifier(
                arena.get_identifier_node(property.identifier),
                arena,
                errors,
            ),
            CallOperationNode::OptionalProperty(optional_property) => self.visit_identifier(
                arena.get_identifier_node(optional_property.identifier),
                arena,
                errors,
            ),
            CallOperationNode::Index(index) => {
                self.visit_expression(arena.get_expr_node(index.index), arena, errors)
            }
            CallOperationNode::Map(map) => {
                self.visit_map_expression(TypedNodeRef::new(operation.id, map), arena, errors)
            }
            CallOperationNode::OptionalMap(map) => self.visit_optional_map_expression(
                TypedNodeRef::new(operation.id, map),
                arena,
                errors,
            ),
        }
    }

    fn visit_primary_expression(
        &mut self,
        primary: TypedNodeRef<PrimaryNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match primary.node {
            PrimaryNode::Number(number) => {
                self.visit_number_literal(TypedNodeRef::new(primary.id, number), arena, errors)
            }
            PrimaryNode::String(string) => {
                self.visit_string_literal(TypedNodeRef::new(primary.id, string), arena, errors)
            }
            PrimaryNode::Boolean(boolean) => {
                self.visit_boolean_literal(TypedNodeRef::new(primary.id, boolean), arena, errors)
            }
            PrimaryNode::Nil(nil) => {
                self.visit_nil_literal(TypedNodeRef::new(primary.id, nil), arena, errors)
            }
            PrimaryNode::This(this_expr) => {
                self.visit_this_expression(TypedNodeRef::new(primary.id, this_expr), arena, errors)
            }
            PrimaryNode::Super(super_expr) => self.visit_super_expression(
                TypedNodeRef::new(primary.id, super_expr),
                arena,
                errors,
            ),
            PrimaryNode::Identifier(identifier) => {
                self.visit_identifier(TypedNodeRef::new(primary.id, identifier), arena, errors)
            }
            PrimaryNode::Grouping(grouping) => self.visit_grouping_expression(
                TypedNodeRef::new(primary.id, grouping),
                arena,
                errors,
            ),
            PrimaryNode::Lambda(lambda) => {
                self.visit_lambda_expression(TypedNodeRef::new(primary.id, lambda), arena, errors)
            }
            PrimaryNode::Array(array) => {
                self.visit_array_literal(TypedNodeRef::new(primary.id, array), arena, errors)
            }
            PrimaryNode::Object(object) => {
                self.visit_object_literal(TypedNodeRef::new(primary.id, object), arena, errors)
            }
        }
    }

    fn visit_number_literal(
        &mut self,
        _number: TypedNodeRef<NumberLiteralNode>,
        _arena: &TypedNodeArena,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_string_literal(
        &mut self,
        _string: TypedNodeRef<StringLiteralNode>,
        _arena: &TypedNodeArena,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_boolean_literal(
        &mut self,
        _boolean: TypedNodeRef<BooleanLiteralNode>,
        _arena: &TypedNodeArena,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_nil_literal(
        &mut self,
        _nil: TypedNodeRef<NilLiteralNode>,
        _arena: &TypedNodeArena,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_this_expression(
        &mut self,
        _this_expr: TypedNodeRef<ThisExprNode>,
        _arena: &TypedNodeArena,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_super_expression(
        &mut self,
        super_expr: TypedNodeRef<SuperExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(
            arena.get_identifier_node(super_expr.node.method),
            arena,
            errors,
        )
    }

    fn visit_identifier(
        &mut self,
        _identifier: TypedNodeRef<IdentifierNode>,
        _arena: &TypedNodeArena,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_grouping_expression(
        &mut self,
        grouping: TypedNodeRef<GroupingExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(arena.get_expr_node(grouping.node.expr), arena, errors)
    }

    fn visit_array_literal(
        &mut self,
        array: TypedNodeRef<ArrayLiteralExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for element in arena.iter_expr_nodes(array.node.elements) {
            self.visit_expression(element, arena, errors)?;
        }
        Ok(())
    }

    fn visit_object_literal(
        &mut self,
        object: TypedNodeRef<ObjectLiteralExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for entry in arena.iter_object_entry_nodes(object.node.entries) {
            self.visit_object_entry(entry, arena, errors)?;
        }
        Ok(())
    }

    fn visit_object_entry(
        &mut self,
        entry: TypedNodeRef<ObjectEntryNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(arena.get_identifier_node(entry.node.key), arena, errors)?;
        self.visit_expression(arena.get_expr_node(entry.node.value), arena, errors)
    }

    fn visit_map_expression(
        &mut self,
        map: TypedNodeRef<MapExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(arena.get_identifier_node(map.node.parameter), arena, errors)?;
        self.visit_expression(arena.get_expr_node(map.node.body), arena, errors)
    }

    fn visit_optional_map_expression(
        &mut self,
        map: TypedNodeRef<OptionalMapExprNode>,
        arena: &TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(arena.get_identifier_node(map.node.parameter), arena, errors)?;
        self.visit_expression(arena.get_expr_node(map.node.body), arena, errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::SourceSpan;

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
            _arena: &TypedNodeArena,
            _errors: &mut ErrorReporter,
        ) -> Result<(), Self::Error> {
            self.count += 1;
            Ok(())
        }
    }

    #[test]
    fn test_node_visitor() {
        let mut arena = TypedNodeArena::new();
        let mut counter = IdentifierCounter::new();
        let mut errors = ErrorReporter::new();

        // Create a simple identifier node
        let identifier = IdentifierNode {
            name: 0,
            span: SourceSpan::new(0, 4),
        };
        let id = arena.insert_node(AstNode::Identifier(identifier));
        let typed_ref = arena.get_identifier_node(id);

        counter
            .visit_identifier(typed_ref, &arena, &mut errors)
            .unwrap();
        assert_eq!(counter.count, 1);
    }
}
