use crate::{
    CompilerError, ErrorReporter, FunctionObject, HeapAllocator, NodeId, Parser, QangCompilerError,
    QangProgram, SourceLocation, SourceMap, TypedNodeArena, Value,
    backend::chunk::{Chunk, OpCode},
    frontend::{
        analyzer::{AnalysisPipeline, AnalysisResults},
        node_visitor::{NodeVisitor, VisitorContext},
        scope_analysis::{FunctionInfo, FunctionKind},
        typed_node_arena::TypedNodeRef,
    },
    nodes::*,
};

pub fn compile(source_map: &SourceMap) -> Result<QangProgram, CompilerError> {
    let mut alloc = HeapAllocator::new();
    let mut parser = Parser::new(source_map, TypedNodeArena::new(), &mut alloc.strings);
    let program = parser.parse();

    let (mut errors, mut nodes) = parser.into_parts();

    let result =
        AnalysisPipeline::new(&mut alloc.strings).analyze(program, &mut nodes, &mut errors);

    if errors.has_errors() {
        return Err(CompilerError::new(errors.take_errors()));
    }

    let assembler = Assembler::new(source_map, &mut alloc, &result);
    let main_function = assembler.assemble(program, &mut nodes, &mut errors)?;

    Ok(QangProgram::new(alloc.allocate_function(main_function)))
}

#[derive(Debug, Clone, PartialEq, Default)]
struct LoopContext {
    continue_jumps: Vec<usize>,
    break_jumps: Vec<usize>,
}

struct Assembler<'a> {
    source_map: &'a SourceMap,
    allocator: &'a mut HeapAllocator,
    analysis: &'a AnalysisResults,
    current_function: FunctionObject,
    current_function_info: Option<FunctionInfo>,
    loop_contexts: Vec<LoopContext>,
}

impl<'a> Assembler<'a> {
    pub fn new(
        source_map: &'a SourceMap,
        allocator: &'a mut HeapAllocator,
        analysis: &'a AnalysisResults,
    ) -> Self {
        let handle = allocator.strings.intern(&source_map.name);
        Self {
            source_map,
            allocator,
            analysis,
            current_function: FunctionObject::new(handle, 0),
            current_function_info: None,
            loop_contexts: Vec::new(),
        }
    }

    pub fn assemble(
        mut self,
        program: NodeId,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<FunctionObject, CompilerError> {
        let mut ctx = VisitorContext::new(nodes, errors);

        let program_node = ctx.nodes.get_program_node(program);

        self.visit_program(program_node, &mut ctx)
            .map_err(|err| CompilerError::new(vec![err]))?;

        Ok(self.current_function)
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.current_function.chunk
    }

    fn emit_opcode(&mut self, opcode: OpCode, span: SourceSpan) {
        let line = self.source_map.get_line_number(span.start);
        let col = self.source_map.get_column_number(span.start);
        self.current_chunk_mut()
            .write_opcode(opcode, SourceLocation::new(line, col));
    }

    fn emit_byte(&mut self, byte: u8, span: SourceSpan) {
        let line = self.source_map.get_line_number(span.start);
        let col = self.source_map.get_column_number(span.start);
        self.current_chunk_mut()
            .write(byte, SourceLocation::new(line, col));
    }

    fn emit_jump(&mut self, opcode: OpCode, span: SourceSpan) -> usize {
        self.emit_opcode(opcode, span);
        self.emit_byte(0xff, span);
        self.emit_byte(0xff, span);
        self.current_chunk_mut().code.len() - 2
    }

    fn patch_jump(&mut self, offset: usize, span: SourceSpan) -> Result<(), QangCompilerError> {
        let jump = self.current_chunk_mut().code.len() - offset - 2;

        if jump > u16::MAX as usize {
            return Err(QangCompilerError::new_syntax_error(
                "Too much code to jump over.".to_string(),
                span,
            ));
        }

        self.current_chunk_mut().code[offset] = ((jump >> 8) & 0xff) as u8;
        self.current_chunk_mut().code[offset + 1] = (jump & 0xff) as u8;

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize, span: SourceSpan) -> Result<(), QangCompilerError> {
        self.emit_opcode(OpCode::Loop, span);
        let offset = self.current_chunk_mut().code.len() - loop_start + 2;
        if offset > u16::MAX as usize {
            return Err(QangCompilerError::new_syntax_error(
                "Loop body too large.".to_string(),
                span,
            ));
        }

        self.emit_byte((offset >> 8 & 0xff) as u8, span);
        self.emit_byte((offset & 0xff) as u8, span);

        Ok(())
    }

    fn emit_opcode_and_byte(&mut self, opcode: OpCode, byte: u8, span: SourceSpan) {
        self.emit_opcode(opcode, span);
        self.emit_byte(byte, span);
    }

    fn emit_constant(&mut self, value: Value, span: SourceSpan) -> Result<(), QangCompilerError> {
        let index = self.current_chunk_mut().add_constant(value);

        if index <= u8::MAX as usize {
            self.emit_opcode_and_byte(OpCode::Constant, index as u8, span);
        } else if index <= u16::MAX as usize {
            self.emit_opcode(OpCode::Constant16, span);
            self.emit_byte((index >> 8) as u8, span);
            self.emit_byte((index & 0xff) as u8, span);
        } else {
            return Err(QangCompilerError::new_assembler_error(
                "Too many constants in function".to_string(),
                span,
            ));
        }

        Ok(())
    }

    fn make_constant(&mut self, value: Value, span: SourceSpan) -> Result<u8, QangCompilerError> {
        let index = self.current_chunk_mut().add_constant(value);

        if index > u8::MAX as usize {
            Err(QangCompilerError::new_assembler_error(
                "Too many constants in function (max 256)".to_string(),
                span,
            ))
        } else {
            Ok(index as u8)
        }
    }

    fn emit_constant_opcode(
        &mut self,
        opcode_8: OpCode,
        opcode_16: OpCode,
        value: Value,
        span: SourceSpan,
    ) -> Result<(), QangCompilerError> {
        let index = self.current_chunk_mut().add_constant(value);

        if index <= u8::MAX as usize {
            self.emit_opcode_and_byte(opcode_8, index as u8, span);
        } else if index <= u16::MAX as usize {
            self.emit_opcode(opcode_16, span);
            self.emit_byte((index >> 8) as u8, span);
            self.emit_byte((index & 0xff) as u8, span);
        } else {
            return Err(QangCompilerError::new_assembler_error(
                "Too many constants in function (max 65536)".to_string(),
                span,
            ));
        }

        Ok(())
    }

    fn emit_return(&mut self, function_kind: FunctionKind, span: SourceSpan) {
        let is_init = self
            .current_function_info
            .as_ref()
            .map(|f| matches!(f.kind, FunctionKind::Initializer))
            .unwrap_or(false);
        if is_init {
            self.emit_opcode_and_byte(OpCode::GetLocal, 0, span);
        } else {
            self.emit_opcode(OpCode::Nil, span);
        }
        self.emit_opcode(OpCode::Return, span);
    }
}

impl<'a> NodeVisitor for Assembler<'a> {
    type Error = QangCompilerError;

    fn visit_return_statement(
        &mut self,
        return_stmt: TypedNodeRef<ReturnStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let is_init = self
            .current_function_info
            .as_ref()
            .map(|f| matches!(f.kind, FunctionKind::Initializer))
            .unwrap_or(false);

        if let Some(expr) = return_stmt.node.value {
            // TODO: Tail call optimization disabled until static analysis is implemented
            // to avoid issues with class instantiation
            // if self.is_tail_call(expr)
            //     && let ast::Expr::Call(call_expr) = expr
            // {
            //     // Handle call operation to get arguments
            //     if let ast::CallOperation::Call(args) = &*call_expr.operation {
            //         // Emit callee first (like regular calls)
            //         self.visit_expression(&call_expr.callee, errors)?;
            //         // Then emit arguments
            //         for arg in args {
            //             self.visit_expression(arg, errors)?;
            //         }
            //         // Emit tail call instead of regular call + return
            //         self.emit_tail_call(args.len() as u8, call_expr.span);
            //         return Ok(());
            //     }
            // }
            let expr_node = ctx.nodes.get_expr_node(expr);
            self.visit_expression(expr_node, ctx)?;
        } else if is_init {
            self.emit_opcode_and_byte(OpCode::GetLocal, 0, return_stmt.node.span);
        } else {
            self.emit_opcode(OpCode::Nil, return_stmt.node.span);
        }

        self.emit_opcode(OpCode::Return, return_stmt.node.span);
        Ok(())
    }

    fn visit_number_literal(
        &mut self,
        number: TypedNodeRef<NumberLiteralNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.emit_constant(number.node.value.into(), number.node.span)
    }

    fn visit_string_literal(
        &mut self,
        string: TypedNodeRef<StringLiteralNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.emit_constant(Value::String(string.node.value), string.node.span)
    }

    fn visit_boolean_literal(
        &mut self,
        boolean: TypedNodeRef<BooleanLiteralNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.emit_opcode(boolean.node.value.into(), boolean.node.span);
        Ok(())
    }

    fn visit_nil_literal(
        &mut self,
        nil: TypedNodeRef<NilLiteralNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.emit_opcode(OpCode::Nil, nil.node.span);
        Ok(())
    }

    fn visit_this_expression(
        &mut self,
        _this_expr: TypedNodeRef<ThisExprNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_super_expression(
        &mut self,
        super_expr: TypedNodeRef<SuperExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_comparison_expression(
        &mut self,
        comparison: TypedNodeRef<ComparisonExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let left = ctx.nodes.get_expr_node(comparison.node.left);
        self.visit_expression(left, ctx)?;
        let right = ctx.nodes.get_expr_node(comparison.node.right);
        self.visit_expression(right, ctx)?;
        self.emit_opcode(comparison.node.operator.into(), comparison.node.span);
        Ok(())
    }

    fn visit_equality_expression(
        &mut self,
        equality: TypedNodeRef<EqualityExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let left = ctx.nodes.get_expr_node(equality.node.left);
        self.visit_expression(left, ctx)?;
        let right = ctx.nodes.get_expr_node(equality.node.right);
        self.visit_expression(right, ctx)?;
        self.emit_opcode(equality.node.operator.into(), equality.node.span);

        if let EqualityOperator::NotEqual = equality.node.operator {
            self.emit_opcode(OpCode::Not, equality.node.span);
        }
        Ok(())
    }

    fn visit_term_expression(
        &mut self,
        term: TypedNodeRef<TermExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let left = ctx.nodes.get_expr_node(term.node.left);
        self.visit_expression(left, ctx)?;
        let right = ctx.nodes.get_expr_node(term.node.right);
        self.visit_expression(right, ctx)?;
        self.emit_opcode(term.node.operator.into(), term.node.span);
        Ok(())
    }

    fn visit_factor_expression(
        &mut self,
        factor: TypedNodeRef<FactorExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let left = ctx.nodes.get_expr_node(factor.node.left);
        self.visit_expression(left, ctx)?;
        let right = ctx.nodes.get_expr_node(factor.node.right);
        self.visit_expression(right, ctx)?;
        self.emit_opcode(factor.node.operator.into(), factor.node.span);
        Ok(())
    }

    fn visit_unary_expression(
        &mut self,
        unary: TypedNodeRef<UnaryExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let operand = ctx.nodes.get_expr_node(unary.node.operand);
        self.visit_expression(operand, ctx)?;
        self.emit_opcode(unary.node.operator.into(), unary.node.span);
        Ok(())
    }

    fn visit_assignment_expression(
        &mut self,
        assignment: TypedNodeRef<AssignmentExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: TypedNodeRef<ExprStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let expr_node = ctx.nodes.get_expr_node(expr_stmt.node.expr);
        self.visit_expression(expr_node, ctx)?;
        self.emit_opcode(OpCode::Pop, expr_stmt.node.span);
        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        var_decl: TypedNodeRef<VariableDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_identifier(
        &mut self,
        _identifier: TypedNodeRef<IdentifierNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_if_statement(
        &mut self,
        if_stmt: TypedNodeRef<IfStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let condition = ctx.nodes.get_expr_node(if_stmt.node.condition);
        self.visit_expression(condition, ctx)?;

        let then_stmt = ctx.nodes.get_stmt_node(if_stmt.node.then_branch);
        let then_jump = self.emit_jump(OpCode::JumpIfFalse, then_stmt.node.span());
        self.emit_opcode(OpCode::Pop, then_stmt.node.span());
        self.visit_statement(then_stmt, ctx)?;

        let else_jump = self.emit_jump(OpCode::Jump, if_stmt.node.span);
        self.patch_jump(then_jump, if_stmt.node.span)?;
        self.emit_opcode(OpCode::Pop, if_stmt.node.span);

        if let Some(else_branch) = if_stmt.node.else_branch {
            let else_branch = ctx.nodes.get_stmt_node(else_branch);
            self.visit_statement(else_branch, ctx)?;
        }
        self.patch_jump(else_jump, if_stmt.node.span)?;

        Ok(())
    }

    fn visit_ternary_expression(
        &mut self,
        ternary: TypedNodeRef<TernaryExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let condition = ctx.nodes.get_expr_node(ternary.node.condition);
        self.visit_expression(condition, ctx)?;

        let then_jump = self.emit_jump(OpCode::JumpIfFalse, ternary.node.span);
        self.emit_opcode(OpCode::Pop, ternary.node.span);

        let then_expr = ctx.nodes.get_expr_node(ternary.node.then_expr);
        self.visit_expression(then_expr, ctx)?;

        let else_jump = self.emit_jump(OpCode::Jump, ternary.node.span);
        self.patch_jump(then_jump, ternary.node.span)?;
        self.emit_opcode(OpCode::Pop, ternary.node.span);

        let else_expr = ctx.nodes.get_expr_node(ternary.node.else_expr);
        self.visit_expression(else_expr, ctx)?;

        self.patch_jump(else_jump, ternary.node.span)?;

        Ok(())
    }

    fn visit_logical_or_expression(
        &mut self,
        logical_or: TypedNodeRef<LogicalOrExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let left = ctx.nodes.get_expr_node(logical_or.node.left);
        let right = ctx.nodes.get_expr_node(logical_or.node.right);
        self.visit_expression(left, ctx)?;

        let else_jump = self.emit_jump(OpCode::JumpIfFalse, logical_or.node.span);
        let end_jump = self.emit_jump(OpCode::Jump, logical_or.node.span);

        self.patch_jump(else_jump, logical_or.node.span)?;
        self.emit_opcode(OpCode::Pop, logical_or.node.span);

        self.visit_expression(right, ctx)?;
        self.patch_jump(end_jump, logical_or.node.span)?;

        Ok(())
    }

    fn visit_logical_and_expression(
        &mut self,
        logical_and: TypedNodeRef<LogicalAndExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let left = ctx.nodes.get_expr_node(logical_and.node.left);
        let right = ctx.nodes.get_expr_node(logical_and.node.right);
        self.visit_expression(left, ctx)?;

        let end_jump = self.emit_jump(OpCode::JumpIfFalse, logical_and.node.span);
        self.emit_opcode(OpCode::Pop, logical_and.node.span);

        self.visit_expression(right, ctx)?;
        self.patch_jump(end_jump, logical_and.node.span)?;

        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: TypedNodeRef<WhileStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.loop_contexts.push(LoopContext::default());

        let loop_start = self.current_chunk_mut().code.len();
        let condition = ctx.nodes.get_expr_node(while_stmt.node.condition);
        self.visit_expression(condition, ctx)?;

        let body = ctx.nodes.get_stmt_node(while_stmt.node.body);
        let exit_jump = self.emit_jump(OpCode::JumpIfFalse, body.node.span());
        self.emit_opcode(OpCode::Pop, body.node.span());
        self.visit_statement(body, ctx)?;

        let loop_context = self.loop_contexts.pop().expect("Loop context should exist");

        for continue_position in loop_context.continue_jumps {
            let jump_distance = continue_position - loop_start + 3;

            if jump_distance > u16::MAX as usize {
                return Err(QangCompilerError::new_syntax_error(
                    "Continue jump too large.".to_string(),
                    while_stmt.node.span,
                ));
            }

            let chunk = self.current_chunk_mut();
            chunk.code.insert(continue_position, OpCode::Loop as u8);
            chunk
                .code
                .insert(continue_position + 1, ((jump_distance >> 8) & 0xff) as u8);
            chunk
                .code
                .insert(continue_position + 2, (jump_distance & 0xff) as u8);
        }

        self.emit_loop(loop_start, body.node.span())?;

        self.patch_jump(exit_jump, body.node.span())?;
        self.emit_opcode(OpCode::Pop, while_stmt.node.span);

        for break_jump in loop_context.break_jumps {
            self.patch_jump(break_jump, while_stmt.node.span)?;
        }

        Ok(())
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: TypedNodeRef<ForStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: TypedNodeRef<FunctionDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_break_statement(
        &mut self,
        break_stmt: TypedNodeRef<BreakStmtNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if !self.loop_contexts.is_empty() {
            let jump = self.emit_jump(OpCode::Jump, break_stmt.node.span);
            if let Some(loop_context) = self.loop_contexts.last_mut() {
                loop_context.break_jumps.push(jump);
            }
            Ok(())
        } else {
            Err(QangCompilerError::new_syntax_error(
                "'break' can only be used inside loops.".to_string(),
                break_stmt.node.span,
            ))
        }
    }

    fn visit_continue_statement(
        &mut self,
        continue_stmt: TypedNodeRef<ContinueStmtNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if !self.loop_contexts.is_empty() {
            let continue_position = self.current_chunk_mut().code.len();
            if let Some(loop_context) = self.loop_contexts.last_mut() {
                loop_context.continue_jumps.push(continue_position);
            }
            Ok(())
        } else {
            Err(QangCompilerError::new_syntax_error(
                "'continue' can only be used inside loops.".to_string(),
                continue_stmt.node.span,
            ))
        }
    }

    fn visit_lambda_declaration(
        &mut self,
        lambda_decl: TypedNodeRef<LambdaDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: TypedNodeRef<LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_call_expression(
        &mut self,
        call: TypedNodeRef<CallExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_pipe_expression(
        &mut self,
        pipe: TypedNodeRef<PipeExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_class_declaration(
        &mut self,
        class_decl: TypedNodeRef<ClassDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        todo!()
    }

    fn visit_array_literal(
        &mut self,
        array: TypedNodeRef<ArrayLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(array.node.elements);

        if length > u8::MAX.into() {
            return Err(QangCompilerError::new_syntax_error(
                "An array literal cannot be initialized with more than 256 elements.".to_string(),
                array.node.span,
            ));
        }

        for i in (0..length).rev() {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(array.node.elements, i) {
                let expr = ctx.nodes.get_expr_node(node_id);
                self.visit_expression(expr, ctx)?;
            }
        }

        self.emit_opcode_and_byte(OpCode::ArrayLiteral, length as u8, array.node.span);

        Ok(())
    }

    fn visit_object_literal(
        &mut self,
        object: TypedNodeRef<ObjectLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(object.node.entries);

        if length > u8::MAX.into() {
            return Err(QangCompilerError::new_syntax_error(
                "An object literal cannot be initialized with more than 256 entries.".to_string(),
                object.node.span,
            ));
        }

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(object.node.entries, i) {
                let entry = ctx.nodes.get_obj_entry_node(node_id);
                self.visit_object_entry(entry, ctx)?;
            }
        }

        self.emit_opcode_and_byte(OpCode::ObjectLiteral, length as u8, object.node.span);
        Ok(())
    }

    fn visit_object_entry(
        &mut self,
        entry: TypedNodeRef<ObjectEntryNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier_node = ctx.nodes.get_identifier_node(entry.node.key).node;
        let identifier_handle = identifier_node.name;
        self.emit_constant(Value::String(identifier_handle), identifier_node.span)?;
        let value = ctx.nodes.get_expr_node(entry.node.value);
        self.visit_expression(value, ctx)?;
        Ok(())
    }
}
