use std::sync::Arc;

use crate::{
    AnalysisPipelineConfig, ErrorMessageFormat, ErrorReporter, FunctionHandle, FunctionObject,
    HeapAllocator, NodeId, Parser, QangCompilerError, QangPipelineError, SourceLocation, SourceMap,
    TypedNodeArena, Value,
    backend::chunk::{Chunk, OpCode},
    frontend::{
        analyzer::{AnalysisPipeline, AnalysisResults},
        node_visitor::{NodeVisitor, VisitorContext},
        scope_analysis::{FunctionInfo, FunctionKind, VariableKind},
        typed_node_arena::{AssignmentTargetNode, ClassMemberNode, TypedNodeRef},
    },
    nodes::*,
};

pub const FRAME_MAX: usize = 64;
pub const STACK_MAX: usize = FRAME_MAX * 256;

#[derive(Debug, Clone, PartialEq)]
pub struct QangProgram(FunctionHandle);

impl QangProgram {
    pub fn new(handle: FunctionHandle) -> Self {
        Self(handle)
    }

    pub fn into_handle(self) -> FunctionHandle {
        self.0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CompilerConfig {
    pub error_message_format: ErrorMessageFormat,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            error_message_format: ErrorMessageFormat::Compact,
        }
    }
}

impl From<CompilerConfig> for AnalysisPipelineConfig {
    fn from(value: CompilerConfig) -> Self {
        Self {
            error_message_format: value.error_message_format,
            ..Default::default()
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompilerPipeline {
    config: CompilerConfig,
}

impl Default for CompilerPipeline {
    fn default() -> Self {
        Self::new()
    }
}

impl CompilerPipeline {
    pub fn new() -> Self {
        Self {
            config: CompilerConfig::default(),
        }
    }

    pub fn with_config(mut self, config: CompilerConfig) -> Self {
        self.config = config;
        self
    }

    pub fn compile(
        &self,
        source_map: SourceMap,
        alloc: &mut HeapAllocator,
    ) -> Result<QangProgram, QangPipelineError> {
        let mut nodes = TypedNodeArena::new();
        let mut parser = Parser::new(Arc::new(source_map), &mut nodes, &mut alloc.strings);
        let modules = parser.parse();

        let mut errors = parser.into_errors();

        let result = AnalysisPipeline::new(&mut alloc.strings)
            .with_config(self.config.into())
            .analyze(
                modules.get_main().module_id,
                modules.get_main().source_map.clone(),
                &mut nodes,
                &mut errors,
            )?;

        let assembler = Assembler::new(modules.get_main().source_map.clone(), alloc, &result);
        let main_function =
            assembler.assemble(modules.get_main().module_id, &mut nodes, &mut errors)?;

        Ok(QangProgram::new(alloc.allocate_function(main_function)))
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
struct LoopContext {
    continue_jumps: Vec<usize>,
    break_jumps: Vec<usize>,
    node_id: NodeId,
}

struct Assembler<'a> {
    source_map: Arc<SourceMap>,
    allocator: &'a mut HeapAllocator,
    analysis: &'a AnalysisResults,
    current_function_id: Option<NodeId>,
    current_function: FunctionObject,
    loop_contexts: Vec<LoopContext>,
}

impl<'a> Assembler<'a> {
    pub fn new(
        source_map: Arc<SourceMap>,
        allocator: &'a mut HeapAllocator,
        analysis: &'a AnalysisResults,
    ) -> Self {
        let handle = allocator.strings.intern(&source_map.name);
        Self {
            source_map,
            allocator,
            analysis,
            current_function: FunctionObject::new(handle, 0),
            current_function_id: None,
            loop_contexts: Vec::new(),
        }
    }

    pub fn assemble(
        mut self,
        program: NodeId,
        nodes: &mut TypedNodeArena,
        errors: &mut ErrorReporter,
    ) -> Result<FunctionObject, QangPipelineError> {
        let mut ctx = VisitorContext::new(nodes, errors);

        let program_node = ctx.nodes.get_program_node(program);

        self.visit_module(program_node, &mut ctx)
            .map_err(|err| QangPipelineError::new(vec![err]))?;

        self.emit_opcode(OpCode::Nil, SourceSpan::default());
        self.emit_opcode(OpCode::Return, SourceSpan::default());

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
            return Err(QangCompilerError::new_assembler_error(
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
            return Err(QangCompilerError::new_assembler_error(
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

    fn emit_return(&mut self, span: SourceSpan) {
        let is_init = self
            .current_function_id
            .map(|id| matches!(self.get_function_info(id).kind, FunctionKind::Initializer))
            .unwrap_or(false);
        if is_init {
            self.emit_opcode_and_byte(OpCode::GetLocal, 0, span);
        } else {
            self.emit_opcode(OpCode::Nil, span);
        }
        self.emit_opcode(OpCode::Return, span);
    }

    fn emit_variable_access(
        &mut self,
        node_id: NodeId,
        span: SourceSpan,
        is_assignment: bool,
    ) -> Result<(), QangCompilerError> {
        let var_info = self
            .analysis
            .scopes
            .variables
            .get(&node_id)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    format!(
                        "Variable not found in analysis results for node_id: {:?}",
                        node_id
                    ),
                    span,
                )
            })?;

        match &var_info.kind {
            VariableKind::Local { slot, .. } => {
                let opcode = if is_assignment {
                    OpCode::SetLocal
                } else {
                    OpCode::GetLocal
                };
                self.emit_opcode_and_byte(opcode, *slot as u8, span);
            }
            VariableKind::Global => {
                self.emit_global_variable_access(var_info.name, is_assignment, span)?;
            }
            VariableKind::Upvalue { index, .. } => {
                let opcode = if is_assignment {
                    OpCode::SetUpvalue
                } else {
                    OpCode::GetUpvalue
                };
                self.emit_opcode_and_byte(opcode, *index as u8, span);
            }
        }

        Ok(())
    }

    fn emit_global_variable_access(
        &mut self,
        handle: crate::StringHandle,
        is_assignment: bool,
        span: SourceSpan,
    ) -> Result<(), QangCompilerError> {
        if is_assignment {
            self.emit_constant_opcode(
                OpCode::SetGlobal,
                OpCode::SetGlobal16,
                Value::String(handle),
                span,
            )
        } else {
            self.emit_constant_opcode(
                OpCode::GetGlobal,
                OpCode::GetGlobal16,
                Value::String(handle),
                span,
            )
        }
    }

    fn compile_function(
        &mut self,
        func_node_id: NodeId,
        func_expr: FunctionExprNode,
        ctx: &mut VisitorContext,
    ) -> Result<(), QangCompilerError> {
        // Get function info from analysis
        let func_info = self.get_function_info(func_node_id);

        // Create new function object
        let identifier = ctx.nodes.get_identifier_node(func_expr.name).node;
        let function = FunctionObject::new(identifier.name, func_info.arity);

        // Store current function and switch to new one
        let old_function = std::mem::replace(&mut self.current_function, function);

        // Store and reset loop contexts - loops don't cross function boundaries
        let old_loop_contexts = std::mem::take(&mut self.loop_contexts);

        let old_func_id = self.current_function_id;
        self.current_function_id = Some(func_node_id);

        // Compile function body
        let body_node = ctx.nodes.get_block_stmt_node(func_expr.body);
        self.visit_block_statement(body_node, ctx)?;

        // Emit return
        self.emit_return(func_expr.span);

        // Get upvalue info for this function
        let upvalue_captures = self
            .analysis
            .scopes
            .upvalue_captures
            .get(&func_node_id)
            .cloned()
            .unwrap_or_default();

        // Store compiled function and set upvalue count
        let mut compiled_function = std::mem::replace(&mut self.current_function, old_function);
        compiled_function.upvalue_count = upvalue_captures.len();

        // Restore the old loop contexts
        self.loop_contexts = old_loop_contexts;

        self.current_function_id = old_func_id;

        // Allocate function and emit closure
        let function_handle = self.allocator.allocate_function(compiled_function);
        self.emit_constant_opcode(
            OpCode::Closure,
            OpCode::Closure16,
            Value::FunctionDecl(function_handle),
            func_expr.span,
        )?;

        // Emit upvalue information
        for upvalue in upvalue_captures {
            let is_local_byte = if upvalue.is_local { 1 } else { 0 };
            self.emit_byte(is_local_byte, func_expr.span);
            self.emit_byte(upvalue.index as u8, func_expr.span);
        }

        Ok(())
    }

    fn get_function_info(&self, node_id: NodeId) -> &FunctionInfo {
        self.analysis
            .scopes
            .functions
            .get(&node_id)
            .expect("Function not found in analysis results")
    }

    fn get_current_function_info(&self) -> Option<&FunctionInfo> {
        self.current_function_id
            .map(|id| self.get_function_info(id))
    }

    fn emit_compound_assignment_op(
        &mut self,
        operator: AssignmentOperator,
        span: SourceSpan,
    ) -> Result<(), QangCompilerError> {
        let opcode = match operator {
            AssignmentOperator::Assign => unreachable!(),
            AssignmentOperator::AddAssign => OpCode::Add,
            AssignmentOperator::SubtractAssign => OpCode::Subtract,
            AssignmentOperator::MultiplyAssign => OpCode::Multiply,
            AssignmentOperator::DivideAssign => OpCode::Divide,
            AssignmentOperator::ModuloAssign => OpCode::Modulo,
        };

        self.emit_opcode(opcode, span);
        Ok(())
    }

    fn emit_nested_loop_variable_resets(
        &mut self,
        for_loop_id: NodeId,
        ctx: &mut VisitorContext,
    ) -> Result<(), QangCompilerError> {
        // Find nested for loops within this loop's body and reset their variables
        if let Some(child_loop_ids) = self.analysis.scopes.loop_children.get(&for_loop_id) {
            // Look for nested loops that need their variables reset
            for &child_loop_id in child_loop_ids {
                if let Some(nested_for_info) = self.analysis.scopes.for_loops.get(&child_loop_id) {
                    for var_info in &nested_for_info.scope_info.initializer_variables {
                        if let Some(initializer_node) = var_info.initializer_node {
                            let initializer_expr = ctx.nodes.get_expr_node(initializer_node);
                            self.visit_expression(initializer_expr, ctx)?;
                            self.emit_opcode_and_byte(
                                OpCode::SetLocal,
                                var_info.slot as u8,
                                initializer_expr.node.span(),
                            );
                            self.emit_opcode(OpCode::Pop, initializer_expr.node.span());
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn handle_map_expression(
        &mut self,
        callee: TypedNodeRef<crate::frontend::typed_node_arena::ExprNode>,
        map_expr: MapExprNode,
        map_node_id: NodeId, // Add this parameter to identify the map expression node
        ctx: &mut VisitorContext,
    ) -> Result<(), QangCompilerError> {
        // Compile the map expression as a closure using static analysis
        self.compile_map_expression(map_node_id, map_expr, ctx)?;

        // Visit the callee (the value being mapped over)
        self.visit_expression(callee, ctx)?;

        // Call the lambda with one argument (the callee result)
        self.emit_opcode_and_byte(OpCode::Call, 1, map_expr.span);
        Ok(())
    }

    fn compile_map_expression(
        &mut self,
        map_node_id: NodeId,
        map_expr: MapExprNode,
        ctx: &mut VisitorContext,
    ) -> Result<(), QangCompilerError> {
        // Get function info from analysis
        let map_name_handle = self.allocator.strings.intern("<map>");
        let func_info = self.get_function_info(map_node_id);

        // Create new function object for the map expression
        let function = FunctionObject::new(map_name_handle, func_info.arity);

        // Store current function and switch to new one
        let old_function = std::mem::replace(&mut self.current_function, function);

        // Store and reset loop contexts - loops don't cross function boundaries
        let old_loop_contexts = std::mem::take(&mut self.loop_contexts);

        let old_func_id = self.current_function_id;
        self.current_function_id = Some(map_node_id);

        // For map expressions, the body is always an expression that gets returned
        let return_node = ReturnStmtNode {
            value: Some(map_expr.body),
            span: map_expr.span,
        };
        self.visit_return_statement(TypedNodeRef::new(map_expr.body, return_node), ctx)?;

        // Get upvalue info for this map expression
        let upvalue_captures = self
            .analysis
            .scopes
            .upvalue_captures
            .get(&map_node_id)
            .cloned()
            .unwrap_or_default();

        // Store compiled function and set upvalue count
        let mut compiled_function = std::mem::replace(&mut self.current_function, old_function);
        compiled_function.upvalue_count = upvalue_captures.len();

        // Restore the old loop contexts
        self.loop_contexts = old_loop_contexts;

        self.current_function_id = old_func_id;

        // Allocate function and emit closure
        let function_handle = self.allocator.allocate_function(compiled_function);
        self.emit_constant_opcode(
            OpCode::Closure,
            OpCode::Closure16,
            Value::FunctionDecl(function_handle),
            map_expr.span,
        )?;

        // Emit upvalue information for the VM
        for upvalue in &upvalue_captures {
            let is_local_byte = if upvalue.is_local { 1 } else { 0 };
            self.emit_byte(is_local_byte, map_expr.span);
            self.emit_byte(upvalue.index as u8, map_expr.span);
        }

        Ok(())
    }
}

impl<'a> NodeVisitor for Assembler<'a> {
    type Error = QangCompilerError;

    fn visit_return_statement(
        &mut self,
        return_stmt: TypedNodeRef<ReturnStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
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
        } else {
            let is_init = self
                .get_current_function_info()
                .map(|f| matches!(f.kind, FunctionKind::Initializer))
                .unwrap_or(false);

            if is_init {
                self.emit_opcode_and_byte(OpCode::GetLocal, 0, return_stmt.node.span);
            } else {
                self.emit_opcode(OpCode::Nil, return_stmt.node.span);
            }
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
        this_expr: TypedNodeRef<ThisExprNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if matches!(
            self.get_current_function_info().map(|f| f.kind),
            Some(FunctionKind::Method | FunctionKind::Initializer)
        ) {
            self.emit_opcode_and_byte(OpCode::GetLocal, 0, this_expr.node.span);
            Ok(())
        } else {
            Err(QangCompilerError::new_assembler_error(
                "Cannot use 'this' outside of a class method.".to_string(),
                this_expr.node.span,
            ))
        }
    }

    fn visit_super_expression(
        &mut self,
        super_expr: TypedNodeRef<SuperExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        // Check if we're in a method or initializer context
        let current_function_kind = self
            .get_current_function_info()
            .map(|f| f.kind)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    "Cannot use 'super' outside of a class method.".to_string(),
                    super_expr.node.span,
                )
            })?;

        if !matches!(
            current_function_kind,
            FunctionKind::Method | FunctionKind::Initializer
        ) {
            return Err(QangCompilerError::new_assembler_error(
                "Cannot use 'super' outside of a class method.".to_string(),
                super_expr.node.span,
            ));
        }

        // Load 'this' onto the stack first
        // 'this' is always at local slot 0 for methods/initializers
        self.emit_opcode_and_byte(OpCode::GetLocal, 0, super_expr.node.span);

        // Load 'super' variable using static analysis results
        // The scope analysis should have resolved the 'super' variable access
        self.emit_variable_access(super_expr.id, super_expr.node.span, false)?;

        // Get the method name and emit GetSuper instruction
        let method_identifier = ctx.nodes.get_identifier_node(super_expr.node.method);
        self.emit_constant_opcode(
            OpCode::GetSuper,
            OpCode::GetSuper16,
            Value::String(method_identifier.node.name),
            super_expr.node.span,
        )?;

        Ok(())
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
        let assignment_target = ctx.nodes.get_assignment_target_node(assignment.node.target);
        let value = ctx.nodes.get_expr_node(assignment.node.value);

        match assignment.node.operator {
            AssignmentOperator::Assign => match assignment_target.node {
                AssignmentTargetNode::Identifier(identifier) => {
                    // Emit the value first
                    self.visit_expression(value, ctx)?;
                    // Then store it in the variable
                    self.emit_variable_access(assignment_target.id, identifier.span, true)?;
                }
                AssignmentTargetNode::Property(property) => {
                    // Emit object
                    self.visit_expression(ctx.nodes.get_expr_node(property.object), ctx)?;
                    // Emit value
                    self.visit_expression(value, ctx)?;
                    // Emit property assignment
                    let identifier_handle =
                        ctx.nodes.get_identifier_node(property.property).node.name;
                    self.emit_constant_opcode(
                        OpCode::SetProperty,
                        OpCode::SetProperty16,
                        Value::String(identifier_handle),
                        property.span,
                    )?;
                }
                AssignmentTargetNode::Index(index) => {
                    // Emit array
                    let array = ctx.nodes.get_expr_node(index.object);
                    self.visit_expression(array, ctx)?;
                    // Emit index
                    let index_expr = ctx.nodes.get_expr_node(index.index);
                    self.visit_expression(index_expr, ctx)?;
                    // Emit value
                    self.visit_expression(value, ctx)?;
                    // Emit index assignment
                    self.emit_opcode(OpCode::SetArrayIndex, index.span);
                }
            },
            _ => match assignment_target.node {
                AssignmentTargetNode::Identifier(identifier) => {
                    // For compound assignment: load current value, compute, store
                    self.emit_variable_access(assignment_target.id, identifier.span, false)?;
                    self.visit_expression(value, ctx)?;
                    self.emit_compound_assignment_op(
                        assignment.node.operator,
                        assignment.node.span,
                    )?;
                    self.emit_variable_access(assignment_target.id, identifier.span, true)?;
                }
                AssignmentTargetNode::Property(property) => {
                    // For compound property assignment
                    let property_object = ctx.nodes.get_expr_node(property.object);
                    self.visit_expression(property_object, ctx)?;
                    self.visit_expression(property_object, ctx)?;
                    let identifier_handle =
                        ctx.nodes.get_identifier_node(property.property).node.name;
                    self.emit_constant_opcode(
                        OpCode::GetProperty,
                        OpCode::GetProperty16,
                        Value::String(identifier_handle),
                        property.span,
                    )?;

                    self.visit_expression(value, ctx)?;
                    self.emit_compound_assignment_op(
                        assignment.node.operator,
                        assignment.node.span,
                    )?;

                    self.emit_constant_opcode(
                        OpCode::SetProperty,
                        OpCode::SetProperty16,
                        Value::String(identifier_handle),
                        property.span,
                    )?;
                }
                AssignmentTargetNode::Index(index_node) => {
                    // For compound index assignment
                    let array = ctx.nodes.get_expr_node(index_node.object);
                    let index = ctx.nodes.get_expr_node(index_node.index);

                    // Load array and index for getting current value
                    self.visit_expression(array, ctx)?;
                    self.visit_expression(index, ctx)?;

                    // Duplicate array and index for setting later
                    self.visit_expression(array, ctx)?;
                    self.visit_expression(index, ctx)?;

                    // Get current value
                    self.emit_opcode(OpCode::GetArrayIndex, index.node.span());

                    // Emit the right-hand value
                    self.visit_expression(value, ctx)?;

                    // Perform the compound operation
                    self.emit_compound_assignment_op(
                        assignment.node.operator,
                        assignment.node.span,
                    )?;

                    // Set the result
                    self.emit_opcode(OpCode::SetArrayIndex, index_node.span);
                }
            },
        }

        Ok(())
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
        let identifier = ctx.nodes.get_identifier_node(var_decl.node.target);

        // Emit initializer
        if let Some(expr) = var_decl
            .node
            .initializer
            .map(|id| ctx.nodes.get_expr_node(id))
        {
            self.visit_expression(expr, ctx)?;
        } else {
            self.emit_opcode(OpCode::Nil, var_decl.node.span);
        }

        // Use static analysis to determine how to define the variable
        let var_info = self
            .analysis
            .scopes
            .variables
            .get(&identifier.id)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    "Variable not found in analysis results".to_string(),
                    identifier.node.span,
                )
            })?;

        match &var_info.kind {
            VariableKind::Global => {
                self.emit_constant_opcode(
                    OpCode::DefineGlobal,
                    OpCode::DefineGlobal16,
                    Value::String(identifier.node.name),
                    var_decl.node.span,
                )?;
            }
            VariableKind::Local { .. } => {
                // Local variables are automatically placed on the stack
                // No additional instruction needed for definition
            }
            VariableKind::Upvalue { .. } => {
                // This shouldn't happen for declarations
                return Err(QangCompilerError::new_assembler_error(
                    "Cannot declare upvalue variable".to_string(),
                    var_decl.node.span,
                ));
            }
        }

        Ok(())
    }

    fn visit_identifier(
        &mut self,
        identifier: TypedNodeRef<IdentifierNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.emit_variable_access(identifier.id, identifier.node.span, false)
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
        let mut loop_context = LoopContext {
            continue_jumps: Vec::new(),
            break_jumps: Vec::new(),
            node_id: while_stmt.id,
        };
        self.loop_contexts.push(loop_context);

        let loop_start = self.current_chunk_mut().code.len();
        let condition = ctx.nodes.get_expr_node(while_stmt.node.condition);
        self.visit_expression(condition, ctx)?;

        let body = ctx.nodes.get_stmt_node(while_stmt.node.body);
        let exit_jump = self.emit_jump(OpCode::JumpIfFalse, body.node.span());
        self.emit_opcode(OpCode::Pop, body.node.span());
        self.visit_statement(body, ctx)?;

        loop_context = self.loop_contexts.pop().expect("Loop context should exist");

        for continue_position in loop_context.continue_jumps {
            let jump_distance = continue_position - loop_start + 3;

            if jump_distance > u16::MAX as usize {
                return Err(QangCompilerError::new_assembler_error(
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

        // Patch break jumps to the same location as the exit jump
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
        // Handle initializer
        if let Some(initializer_id) = for_stmt.node.initializer {
            let initializer = ctx.nodes.get_for_initializer_node(initializer_id);
            match initializer.node {
                crate::frontend::typed_node_arena::ForInitializerNode::VarDecl(var_decl) => {
                    self.visit_variable_declaration(
                        TypedNodeRef::new(initializer.id, var_decl),
                        ctx,
                    )?;
                }
                crate::frontend::typed_node_arena::ForInitializerNode::Expr(expr) => {
                    self.visit_expression(TypedNodeRef::new(initializer.id, expr), ctx)?;
                    self.emit_opcode(OpCode::Pop, expr.span());
                }
            }
        }

        let mut loop_start = self.current_chunk_mut().code.len();
        let mut exit_jump: Option<usize> = None;
        let increment_start: Option<usize>;

        // Create loop context AFTER initializer but BEFORE body
        let loop_context = LoopContext {
            continue_jumps: Vec::new(),
            break_jumps: Vec::new(),
            node_id: for_stmt.id,
        };
        self.loop_contexts.push(loop_context);

        // Handle condition and body
        if let Some(condition_id) = for_stmt.node.condition {
            let condition = ctx.nodes.get_expr_node(condition_id);
            let condition_jump = self.emit_jump(OpCode::Jump, condition.node.span());
            loop_start = self.current_chunk_mut().code.len();

            // Reset nested loop variables if this is a nested for loop
            self.emit_nested_loop_variable_resets(for_stmt.id, ctx)?;

            let body = ctx.nodes.get_stmt_node(for_stmt.node.body);
            self.visit_statement(body, ctx)?;

            increment_start = Some(self.current_chunk_mut().code.len());

            if let Some(increment_id) = for_stmt.node.increment {
                let increment = ctx.nodes.get_expr_node(increment_id);
                self.visit_expression(increment, ctx)?;
                self.emit_opcode(OpCode::Pop, increment.node.span());
            }

            self.patch_jump(condition_jump, condition.node.span())?;
            self.visit_expression(condition, ctx)?;
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse, condition.node.span()));
            self.emit_opcode(OpCode::Pop, condition.node.span());
            self.emit_loop(loop_start, body.node.span())?;
        } else {
            // Reset nested loop variables if this is a nested for loop
            self.emit_nested_loop_variable_resets(for_stmt.id, ctx)?;

            let body = ctx.nodes.get_stmt_node(for_stmt.node.body);
            self.visit_statement(body, ctx)?;

            increment_start = Some(self.current_chunk_mut().code.len());

            if let Some(increment_id) = for_stmt.node.increment {
                let increment = ctx.nodes.get_expr_node(increment_id);
                self.visit_expression(increment, ctx)?;
                self.emit_opcode(OpCode::Pop, increment.node.span());
            }

            self.emit_loop(loop_start, body.node.span())?;
        }

        let loop_context = self.loop_contexts.pop().expect("Loop context should exist");

        // Handle continue jumps
        let continue_target = increment_start.unwrap_or(loop_start);
        for continue_jump in loop_context.continue_jumps {
            if continue_target > continue_jump {
                let jump_distance = continue_target - continue_jump - 3;
                if jump_distance > u16::MAX as usize {
                    return Err(QangCompilerError::new_assembler_error(
                        "Continue jump too large.".to_string(),
                        for_stmt.node.span,
                    ));
                }
                let chunk = self.current_chunk_mut();
                chunk.code[continue_jump + 1] = ((jump_distance >> 8) & 0xff) as u8;
                chunk.code[continue_jump + 2] = (jump_distance & 0xff) as u8;
            } else {
                let jump_distance = continue_jump - continue_target + 2;
                if jump_distance > u16::MAX as usize {
                    return Err(QangCompilerError::new_assembler_error(
                        "Continue jump too large.".to_string(),
                        for_stmt.node.span,
                    ));
                }
                let chunk = self.current_chunk_mut();
                chunk.code[continue_jump] = OpCode::Loop as u8;
                chunk.code[continue_jump + 1] = ((jump_distance >> 8) & 0xff) as u8;
                chunk.code[continue_jump + 2] = (jump_distance & 0xff) as u8;
            }
        }

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump, for_stmt.node.span)?;
            self.emit_opcode(OpCode::Pop, for_stmt.node.span);
        }

        // Patch break jumps to current position (after scope cleanup)
        for break_jump in loop_context.break_jumps {
            self.patch_jump(break_jump, for_stmt.node.span)?;
        }

        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: TypedNodeRef<FunctionDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let func_expr = ctx.nodes.get_func_expr_node(func_decl.node.function);

        // Declare the function variable first (for global functions)
        let identifier = ctx.nodes.get_identifier_node(func_expr.node.name);
        let var_info = self
            .analysis
            .scopes
            .variables
            .get(&identifier.id)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    "Function variable not found in analysis results".to_string(),
                    identifier.node.span,
                )
            })?;

        // Compile the function
        self.compile_function(func_decl.node.function, func_expr.node, ctx)?;

        // Define the function variable
        match &var_info.kind {
            VariableKind::Global => {
                self.emit_constant_opcode(
                    OpCode::DefineGlobal,
                    OpCode::DefineGlobal16,
                    Value::String(identifier.node.name),
                    func_decl.node.span,
                )?;
            }
            VariableKind::Local { .. } => {
                // Local function, no additional instruction needed
            }
            VariableKind::Upvalue { .. } => {
                return Err(QangCompilerError::new_assembler_error(
                    "Cannot declare upvalue function".to_string(),
                    func_decl.node.span,
                ));
            }
        }

        Ok(())
    }

    fn visit_break_statement(
        &mut self,
        break_stmt: TypedNodeRef<BreakStmtNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let jump = self.emit_jump(OpCode::Jump, break_stmt.node.span);

        // Use scope analysis to find the target loop for this break statement
        let break_info = self
            .analysis
            .scopes
            .break_continue_statements
            .get(&break_stmt.id)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    "Break statement not found in scope analysis results".to_string(),
                    break_stmt.node.span,
                )
            })?;

        // Find the loop context that matches the target loop from scope analysis
        let loop_context = self
            .loop_contexts
            .iter_mut()
            .rev()
            .find(|ctx| ctx.node_id == break_info.target_loop)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    "Break statement target loop not found in compiler context".to_string(),
                    break_stmt.node.span,
                )
            })?;

        loop_context.break_jumps.push(jump);
        Ok(())
    }

    fn visit_continue_statement(
        &mut self,
        continue_stmt: TypedNodeRef<ContinueStmtNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let continue_position = self.current_chunk_mut().code.len();

        // Use scope analysis to find the target loop for this continue statement
        let continue_info = self
            .analysis
            .scopes
            .break_continue_statements
            .get(&continue_stmt.id)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    "Continue statement not found in scope analysis results".to_string(),
                    continue_stmt.node.span,
                )
            })?;

        // Find the loop context that matches the target loop from scope analysis
        let loop_context = self
            .loop_contexts
            .iter_mut()
            .rev()
            .find(|ctx| ctx.node_id == continue_info.target_loop)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    "Continue statement target loop not found in compiler context".to_string(),
                    continue_stmt.node.span,
                )
            })?;

        loop_context.continue_jumps.push(continue_position);
        Ok(())
    }

    fn visit_lambda_declaration(
        &mut self,
        lambda_decl: TypedNodeRef<LambdaDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(lambda_decl.node.name);
        let lambda_expr = ctx.nodes.get_lambda_expr_node(lambda_decl.node.lambda);

        // Compile the lambda expression
        self.visit_lambda_expression(lambda_expr, ctx)?;

        // Define the lambda variable using static analysis
        let var_info = self
            .analysis
            .scopes
            .variables
            .get(&identifier.id)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    "Lambda variable not found in analysis results".to_string(),
                    identifier.node.span,
                )
            })?;

        match &var_info.kind {
            VariableKind::Global => {
                self.emit_constant_opcode(
                    OpCode::DefineGlobal,
                    OpCode::DefineGlobal16,
                    Value::String(identifier.node.name),
                    lambda_decl.node.span,
                )?;
            }
            VariableKind::Local { .. } => {
                // Local lambda, no additional instruction needed
            }
            VariableKind::Upvalue { .. } => {
                return Err(QangCompilerError::new_assembler_error(
                    "Cannot declare upvalue lambda".to_string(),
                    lambda_decl.node.span,
                ));
            }
        }

        Ok(())
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: TypedNodeRef<LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let lambda_name_handle = self.allocator.strings.intern("<anonymous>");
        let func_info = self.get_function_info(lambda_expr.id);

        // Create new function object for lambda
        let function = FunctionObject::new(lambda_name_handle, func_info.arity);

        // Store current function and switch to new one
        let old_function = std::mem::replace(&mut self.current_function, function);

        // Store and reset loop contexts - loops don't cross function boundaries
        let old_loop_contexts = std::mem::take(&mut self.loop_contexts);

        let old_function_id = self.current_function_id;
        self.current_function_id = Some(lambda_expr.id);

        // Compile lambda body
        let lambda_body = ctx.nodes.get_lambda_body_node(lambda_expr.node.body);
        match lambda_body.node {
            crate::frontend::typed_node_arena::LambdaBodyNode::Block(body) => {
                self.visit_block_statement(TypedNodeRef::new(lambda_body.id, body), ctx)?;
                self.emit_return(lambda_expr.node.span);
            }
            crate::frontend::typed_node_arena::LambdaBodyNode::Expr(_) => {
                // For expression lambdas, create a return statement
                let return_node = ReturnStmtNode {
                    value: Some(lambda_body.id),
                    span: lambda_body.node.span(),
                };
                self.visit_return_statement(TypedNodeRef::new(lambda_body.id, return_node), ctx)?;
            }
        }

        // Get upvalue info for this lambda
        let upvalue_captures = self
            .analysis
            .scopes
            .upvalue_captures
            .get(&lambda_expr.id)
            .cloned()
            .unwrap_or_default();

        // Store compiled function and set upvalue count
        let mut compiled_function = std::mem::replace(&mut self.current_function, old_function);
        compiled_function.upvalue_count = upvalue_captures.len();

        // Restore the old loop contexts
        self.loop_contexts = old_loop_contexts;

        self.current_function_id = old_function_id;

        // Allocate function and emit closure
        let function_handle = self.allocator.allocate_function(compiled_function);
        self.emit_constant_opcode(
            OpCode::Closure,
            OpCode::Closure16,
            Value::FunctionDecl(function_handle),
            lambda_expr.node.span,
        )?;

        // Emit upvalue information for the VM
        for upvalue in &upvalue_captures {
            let is_local_byte = if upvalue.is_local { 1 } else { 0 };
            self.emit_byte(is_local_byte, lambda_expr.node.span);
            self.emit_byte(upvalue.index as u8, lambda_expr.node.span);
        }

        Ok(())
    }

    fn visit_call_expression(
        &mut self,
        call: TypedNodeRef<CallExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let callee = ctx.nodes.get_expr_node(call.node.callee);
        let call_operation = ctx.nodes.get_call_operation_node(call.node.operation);

        match call_operation.node {
            crate::frontend::typed_node_arena::CallOperationNode::Call(args) => {
                let arg_length = ctx.nodes.array.size(args.args);
                if arg_length > u8::MAX as usize {
                    return Err(QangCompilerError::new_assembler_error(
                        "Functions may only take up to 256 arguments.".to_string(),
                        call.node.span,
                    ));
                }

                // Handle super calls
                if let crate::frontend::typed_node_arena::ExprNode::Primary(
                    crate::frontend::typed_node_arena::PrimaryNode::Super(super_expr),
                ) = callee.node
                {
                    // Check if we're in a method context
                    let current_function_kind = self
                        .get_current_function_info()
                        .map(|f| f.kind)
                        .ok_or_else(|| {
                            QangCompilerError::new_assembler_error(
                                "Cannot use 'super' outside of a class method.".to_string(),
                                super_expr.span,
                            )
                        })?;

                    if !matches!(
                        current_function_kind,
                        FunctionKind::Method | FunctionKind::Initializer
                    ) {
                        return Err(QangCompilerError::new_assembler_error(
                            "Cannot use 'super' outside of a class method.".to_string(),
                            super_expr.span,
                        ));
                    }

                    // Load 'this' (always at local slot 0)
                    self.emit_opcode_and_byte(OpCode::GetLocal, 0, super_expr.span);

                    // Emit arguments
                    for i in 0..arg_length {
                        if let Some(node_id) = ctx.nodes.array.get_node_id_at(args.args, i) {
                            let expr = ctx.nodes.get_expr_node(node_id);
                            self.visit_expression(expr, ctx)?;
                        } else {
                            self.emit_opcode(OpCode::Nil, call.node.span);
                        }
                    }

                    // Load 'super' using static analysis results
                    // Find the super identifier node (we need to find the node ID for the super expression)
                    self.emit_variable_access(callee.id, super_expr.span, false)?;

                    // Get method name and emit SuperInvoke
                    let method = ctx.nodes.get_identifier_node(super_expr.method);
                    self.emit_constant_opcode(
                        OpCode::SuperInvoke,
                        OpCode::SuperInvoke16,
                        Value::String(method.node.name),
                        call.node.span,
                    )?;
                    self.emit_byte(arg_length as u8, call.node.span);

                    return Ok(());
                }

                // Handle method calls (optimization for obj.method(args))
                if let crate::frontend::typed_node_arena::ExprNode::Call(property_call) =
                    callee.node
                    && let crate::frontend::typed_node_arena::CallOperationNode::Property(
                        method_name,
                    ) = ctx
                        .nodes
                        .get_call_operation_node(property_call.operation)
                        .node
                {
                    // Emit the object
                    let expr = ctx.nodes.get_expr_node(property_call.callee);
                    self.visit_expression(expr, ctx)?;

                    // Get method handle
                    let method_handle = ctx
                        .nodes
                        .get_identifier_node(method_name.identifier)
                        .node
                        .name;

                    // Emit arguments
                    for i in 0..arg_length {
                        if let Some(node_id) = ctx.nodes.array.get_node_id_at(args.args, i) {
                            let expr = ctx.nodes.get_expr_node(node_id);
                            self.visit_expression(expr, ctx)?;
                        } else {
                            self.emit_opcode(OpCode::Nil, call.node.span);
                        }
                    }

                    // Emit optimized invoke
                    self.emit_constant_opcode(
                        OpCode::Invoke,
                        OpCode::Invoke16,
                        Value::String(method_handle),
                        call.node.span,
                    )?;
                    self.emit_byte(arg_length as u8, call.node.span);

                    return Ok(());
                }

                // Regular function call
                self.visit_expression(callee, ctx)?;

                // Emit arguments
                for i in 0..arg_length {
                    if let Some(node_id) = ctx.nodes.array.get_node_id_at(args.args, i) {
                        let expr = ctx.nodes.get_expr_node(node_id);
                        self.visit_expression(expr, ctx)?;
                    } else {
                        self.emit_opcode(OpCode::Nil, call.node.span);
                    }
                }

                self.emit_opcode_and_byte(OpCode::Call, arg_length as u8, call.node.span);
                Ok(())
            }
            crate::frontend::typed_node_arena::CallOperationNode::Property(identifier) => {
                self.visit_expression(callee, ctx)?;
                let identifier_handle = ctx
                    .nodes
                    .get_identifier_node(identifier.identifier)
                    .node
                    .name;
                self.emit_constant_opcode(
                    OpCode::GetProperty,
                    OpCode::GetProperty16,
                    Value::String(identifier_handle),
                    call.node.span,
                )?;
                Ok(())
            }
            crate::frontend::typed_node_arena::CallOperationNode::Index(expr) => {
                self.visit_expression(callee, ctx)?;
                let expr = ctx.nodes.get_expr_node(expr.index);
                self.visit_expression(expr, ctx)?;
                self.emit_opcode(OpCode::GetArrayIndex, call.node.span);
                Ok(())
            }
            crate::frontend::typed_node_arena::CallOperationNode::OptionalProperty(identifier) => {
                self.visit_expression(callee, ctx)?;
                let identifier_handle = ctx
                    .nodes
                    .get_identifier_node(identifier.identifier)
                    .node
                    .name;
                self.emit_constant_opcode(
                    OpCode::GetOptionalProperty,
                    OpCode::GetOptionalProperty16,
                    Value::String(identifier_handle),
                    call.node.span,
                )?;
                Ok(())
            }
            crate::frontend::typed_node_arena::CallOperationNode::Map(map_expr) => {
                self.handle_map_expression(callee, map_expr, call_operation.id, ctx)
            }
            crate::frontend::typed_node_arena::CallOperationNode::OptionalMap(map_expr) => {
                self.visit_expression(callee, ctx)?;
                let nil_jump = self.emit_jump(OpCode::JumpIfNil, map_expr.span);
                self.emit_opcode(OpCode::Pop, map_expr.span);
                self.handle_map_expression(
                    callee,
                    MapExprNode {
                        body: map_expr.body,
                        parameter: map_expr.parameter,
                        span: map_expr.span,
                    },
                    call_operation.id, // Pass the node ID here
                    ctx,
                )?;
                self.patch_jump(nil_jump, map_expr.span)?;
                Ok(())
            }
        }
    }

    fn visit_pipe_expression(
        &mut self,
        pipe: TypedNodeRef<PipeExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let left = ctx.nodes.get_expr_node(pipe.node.left);
        let right = ctx.nodes.get_expr_node(pipe.node.right);
        let call_handle = self.allocator.strings.intern("call");

        match right.node {
            crate::frontend::typed_node_arena::ExprNode::Call(call_expr) => {
                match ctx.nodes.get_call_operation_node(call_expr.operation).node {
                    crate::frontend::typed_node_arena::CallOperationNode::Call(arguments) => {
                        // Check if this is a special .call() method invocation
                        if let crate::frontend::typed_node_arena::ExprNode::Call(inner_call) =
                            ctx.nodes.get_expr_node(call_expr.callee).node
                            && let crate::frontend::typed_node_arena::CallOperationNode::Property(
                                method_name,
                            ) = ctx.nodes.get_call_operation_node(inner_call.operation).node
                            && ctx
                                .nodes
                                .get_identifier_node(method_name.identifier)
                                .node
                                .name
                                == call_handle
                        {
                            // This is obj.call(args) piped - use invoke optimization
                            self.visit_expression(ctx.nodes.get_expr_node(call_expr.callee), ctx)?;
                            self.visit_expression(left, ctx)?;
                            let method_handle = ctx
                                .nodes
                                .get_identifier_node(method_name.identifier)
                                .node
                                .name;
                            self.emit_constant_opcode(
                                OpCode::Invoke,
                                OpCode::Invoke16,
                                Value::String(method_handle),
                                pipe.node.span,
                            )?;
                            self.emit_byte(1, pipe.node.span);
                            return Ok(());
                        }

                        // Regular function call with pipe
                        self.visit_expression(ctx.nodes.get_expr_node(call_expr.callee), ctx)?;

                        // The left side of the pipe becomes the first argument
                        self.visit_expression(left, ctx)?;

                        // Add the additional arguments from the call
                        let arg_length = ctx.nodes.array.size(arguments.args);
                        for i in 0..arg_length {
                            if let Some(node_id) = ctx.nodes.array.get_node_id_at(arguments.args, i)
                            {
                                let expr = ctx.nodes.get_expr_node(node_id);
                                self.visit_expression(expr, ctx)?;
                            } else {
                                self.emit_opcode(OpCode::Nil, pipe.node.span);
                            }
                        }

                        let total_args = 1 + arg_length;
                        if total_args > u8::MAX as usize {
                            return Err(QangCompilerError::new_assembler_error(
                                "Functions may only take up to 256 arguments.".to_string(),
                                pipe.node.span,
                            ));
                        }

                        self.emit_opcode_and_byte(OpCode::Call, total_args as u8, pipe.node.span);
                    }
                    crate::frontend::typed_node_arena::CallOperationNode::Property(method_name) => {
                        // Method call with pipe: obj.method |> value becomes value.method()
                        self.visit_expression(ctx.nodes.get_expr_node(call_expr.callee), ctx)?;
                        self.visit_expression(left, ctx)?;
                        let method_handle = ctx
                            .nodes
                            .get_identifier_node(method_name.identifier)
                            .node
                            .name;
                        self.emit_constant_opcode(
                            OpCode::Invoke,
                            OpCode::Invoke16,
                            Value::String(method_handle),
                            pipe.node.span,
                        )?;
                        self.emit_byte(1, pipe.node.span);
                    }
                    _ => {
                        return Err(QangCompilerError::new_assembler_error(
                            "Pipe expression with non-call operation not supported.".to_string(),
                            call_expr.span,
                        ));
                    }
                }
            }
            _ => {
                // Simple pipe: left |> right becomes right(left)
                self.visit_expression(right, ctx)?;
                self.visit_expression(left, ctx)?;
                self.emit_opcode_and_byte(OpCode::Call, 1, pipe.node.span);
            }
        }

        Ok(())
    }

    fn visit_class_declaration(
        &mut self,
        class_decl: TypedNodeRef<ClassDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let name = ctx.nodes.get_identifier_node(class_decl.node.name);
        let superclass = class_decl
            .node
            .superclass
            .map(|superclass| ctx.nodes.get_identifier_node(superclass));

        // Emit the class constant and create the class
        let class_handle = name.node.name;
        self.emit_constant_opcode(
            OpCode::Class,
            OpCode::Class16,
            Value::String(class_handle),
            name.node.span,
        )?;

        // Define the class variable using static analysis
        let var_info = self
            .analysis
            .scopes
            .variables
            .get(&name.id)
            .ok_or_else(|| {
                QangCompilerError::new_assembler_error(
                    "Class variable not found in analysis results".to_string(),
                    name.node.span,
                )
            })?;

        match &var_info.kind {
            VariableKind::Global => {
                self.emit_constant_opcode(
                    OpCode::DefineGlobal,
                    OpCode::DefineGlobal16,
                    Value::String(class_handle),
                    name.node.span,
                )?;
            }
            VariableKind::Local { .. } => {
                // Local class, no additional instruction needed for definition
            }
            VariableKind::Upvalue { .. } => {
                return Err(QangCompilerError::new_assembler_error(
                    "Cannot declare upvalue class".to_string(),
                    class_decl.node.span,
                ));
            }
        }

        // Load the class for method/field definitions
        // The class should be loaded from the same location where it was defined
        match &var_info.kind {
            VariableKind::Global => {
                self.emit_constant_opcode(
                    OpCode::GetGlobal,
                    OpCode::GetGlobal16,
                    Value::String(class_handle),
                    name.node.span,
                )?;
            }
            VariableKind::Local { slot, .. } => {
                self.emit_opcode_and_byte(OpCode::GetLocal, *slot as u8, name.node.span);
            }
            VariableKind::Upvalue { .. } => {
                return Err(QangCompilerError::new_assembler_error(
                    "Cannot access upvalue class for member definition".to_string(),
                    name.node.span,
                ));
            }
        }

        // Handle inheritance using class inheritance analysis
        if let Some(superclass) = superclass {
            // Check for self-inheritance
            if class_handle == superclass.node.name {
                return Err(QangCompilerError::new_assembler_error(
                    "A class cannot inherit from itself.".to_string(),
                    superclass.node.span,
                ));
            }

            // Get the inheritance information from static analysis
            let inheritance_info = self
                .analysis
                .scopes
                .class_inheritance
                .get(&class_decl.id)
                .ok_or_else(|| {
                    QangCompilerError::new_assembler_error(
                        "Class inheritance info not found in analysis results".to_string(),
                        class_decl.node.span,
                    )
                })?;

            // Load the subclass again (for inheritance)
            // The class should be loaded from the same location where it was defined
            match &var_info.kind {
                VariableKind::Global => {
                    self.emit_constant_opcode(
                        OpCode::GetGlobal,
                        OpCode::GetGlobal16,
                        Value::String(class_handle),
                        name.node.span,
                    )?;
                }
                VariableKind::Local { slot, .. } => {
                    self.emit_opcode_and_byte(OpCode::GetLocal, *slot as u8, name.node.span);
                }
                VariableKind::Upvalue { .. } => {
                    return Err(QangCompilerError::new_assembler_error(
                        "Cannot access upvalue class for inheritance".to_string(),
                        name.node.span,
                    ));
                }
            }

            // Load the superclass
            self.emit_variable_access(superclass.id, superclass.node.span, false)?;

            // Store the superclass in the "super" local variable using the slot from analysis
            self.emit_opcode_and_byte(
                OpCode::SetLocal,
                inheritance_info.super_slot as u8,
                superclass.node.span,
            );

            // Now emit inheritance instruction
            self.emit_opcode(OpCode::Inherit, superclass.node.span);
        }

        // Process class members
        let length = ctx.nodes.array.size(class_decl.node.members);
        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(class_decl.node.members, i) {
                let member = ctx.nodes.get_class_member_node(node_id);

                match member.node {
                    ClassMemberNode::Method(function) => {
                        let method_name = ctx.nodes.get_identifier_node(function.name);

                        // Compile the method function
                        self.compile_function(node_id, function, ctx)?;

                        // Add the method to the class
                        self.emit_constant_opcode(
                            OpCode::Method,
                            OpCode::Method16,
                            Value::String(method_name.node.name),
                            function.span,
                        )?;
                    }
                    ClassMemberNode::Field(field) => {
                        let field_name = ctx.nodes.get_identifier_node(field.name);

                        // Emit the field initializer (or nil if none)
                        if let Some(init_id) = field.initializer {
                            let initializer = ctx.nodes.get_expr_node(init_id);
                            self.visit_expression(initializer, ctx)?;
                        } else {
                            self.emit_opcode(OpCode::Nil, field.span);
                        }

                        // Add the field to the class
                        self.emit_constant_opcode(
                            OpCode::InitField,
                            OpCode::InitField16,
                            Value::String(field_name.node.name),
                            field_name.node.span,
                        )?;
                    }
                }
            }
        }

        // Pop the class from the stack
        self.emit_opcode(OpCode::Pop, class_decl.node.span);

        Ok(())
    }

    fn visit_array_literal(
        &mut self,
        array: TypedNodeRef<ArrayLiteralExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let length = ctx.nodes.array.size(array.node.elements);

        if length > u8::MAX.into() {
            return Err(QangCompilerError::new_assembler_error(
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
            return Err(QangCompilerError::new_assembler_error(
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
