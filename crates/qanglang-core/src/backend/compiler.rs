use std::sync::Arc;

use crate::{
    AnalysisPipelineConfig, ErrorMessageFormat, ErrorReporter, FunctionHandle, FunctionObject,
    HeapAllocator, NodeId, QangCompilerError, QangPipelineError, SourceLocation, SourceMap,
    TypedNodeArena, Value,
    backend::{
        chunk::{Chunk, OpCode},
        module_resolver::ModuleResolver,
    },
    frontend::{
        node_visitor::{NodeVisitor, VisitorContext},
        semantic_validator::FunctionKind,
        typed_node_arena::TypedNodeRef,
    },
    nodes::*,
};

pub const FRAME_MAX: usize = 64;
pub const STACK_MAX: usize = FRAME_MAX * 256;

#[derive(Debug, Clone, PartialEq)]
pub struct QangProgram {
    function: FunctionHandle,
    modules: ModuleResolver,
}

impl QangProgram {
    pub fn new(handle: FunctionHandle, modules: ModuleResolver) -> Self {
        Self {
            function: handle,
            modules,
        }
    }

    pub fn get_handle(&self) -> FunctionHandle {
        self.function
    }

    pub fn into_modules(self) -> ModuleResolver {
        self.modules
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

#[derive(Debug, Clone, PartialEq, Default)]
struct LoopContext {
    continue_jumps: Vec<usize>,
    break_jumps: Vec<usize>,
    node_id: NodeId,
}

#[derive(Debug, Clone)]
struct Local {
    name: crate::StringHandle,
    depth: Option<usize>,
    is_captured: bool,
}

impl Local {
    fn new(name: crate::StringHandle) -> Self {
        Self {
            name,
            depth: None,
            is_captured: false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Upvalue {
    index: u8,
    is_local: bool,
}

#[derive(Debug, Clone)]
struct CompilerState {
    kind: FunctionKind,
    locals: Vec<Local>,
    local_count: usize,
    scope_depth: usize,
    upvalues: Vec<Upvalue>,
    enclosing: Option<Box<CompilerState>>,
    has_superclass: bool,
    blank_handle: crate::StringHandle,
    this_handle: crate::StringHandle,
}

impl CompilerState {
    fn new(blank_handle: crate::StringHandle, this_handle: crate::StringHandle) -> Self {
        let mut locals = Vec::with_capacity(u8::MAX as usize);
        locals.push(Local::new(blank_handle));
        Self {
            kind: FunctionKind::Script,
            locals,
            local_count: 1,
            scope_depth: 0,
            upvalues: Vec::with_capacity(u8::MAX as usize),
            enclosing: None,
            has_superclass: false,
            blank_handle,
            this_handle,
        }
    }

    fn push(&mut self, has_superclass: bool, kind: FunctionKind) -> &mut Self {
        let mut locals = Vec::with_capacity(u8::MAX as usize);
        let is_method = matches!(kind, FunctionKind::Method | FunctionKind::Initializer);
        if is_method {
            let mut this_local = Local::new(self.this_handle);
            this_local.depth = Some(0);
            locals.push(this_local);
        } else {
            locals.push(Local::new(self.blank_handle));
        }
        let previous = std::mem::replace(
            self,
            Self {
                kind,
                locals,
                local_count: 1,
                scope_depth: 0,
                upvalues: Vec::with_capacity(u8::MAX as usize),
                enclosing: None,
                has_superclass,
                blank_handle: self.blank_handle,
                this_handle: self.this_handle,
            },
        );

        self.enclosing = Some(Box::new(previous));
        self
    }

    fn pop(&mut self) -> Option<Self> {
        if let Some(mut previous) = self.enclosing.take() {
            std::mem::swap(&mut *previous, self);
            Some(*previous)
        } else {
            None
        }
    }

    fn resolve_local_variable(
        &self,
        handle: crate::StringHandle,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangCompilerError> {
        for i in (0..self.local_count).rev() {
            if let Some(local) = self.locals.get(i)
                && local.name == handle
            {
                if local.depth.is_none() {
                    return Err(QangCompilerError::new_assembler_error(
                        "Cannot read local variable during its initialization.".to_string(),
                        span,
                    ));
                }
                return Ok(Some(i));
            }
        }

        Ok(None)
    }

    fn resolve_upvalue(
        &mut self,
        handle: crate::StringHandle,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangCompilerError> {
        if let Some(enclosing) = self.enclosing.as_mut() {
            if let Some(local_index) = enclosing.resolve_local_variable(handle, span)? {
                if let Some(local) = enclosing.locals.get_mut(local_index) {
                    local.is_captured = true;
                }
                return self.add_upvalue(local_index, true, span).map(Some);
            }

            if let Some(upvalue_index) = enclosing.resolve_upvalue(handle, span)? {
                return self.add_upvalue(upvalue_index, false, span).map(Some);
            }
        }

        Ok(None)
    }

    fn add_upvalue(
        &mut self,
        index: usize,
        is_local: bool,
        span: SourceSpan,
    ) -> Result<usize, QangCompilerError> {
        let upvalue_count = self.upvalues.len();

        for i in 0..upvalue_count {
            let upvalue = self.upvalues[i];
            if upvalue.index == index as u8 && upvalue.is_local == is_local {
                return Ok(i);
            }
        }

        if upvalue_count == u8::MAX as usize {
            return Err(QangCompilerError::new_assembler_error(
                "Can only close over up to 256 variables.".to_string(),
                span,
            ));
        }

        self.upvalues.push(Upvalue {
            index: index as u8,
            is_local,
        });

        Ok(upvalue_count)
    }
}

pub struct Assembler<'a> {
    source_map: Arc<SourceMap>,
    allocator: &'a mut HeapAllocator,
    current_function_id: Option<NodeId>,
    current_function: FunctionObject,
    loop_contexts: Vec<LoopContext>,
    compiler_state: CompilerState,
}

impl<'a> Assembler<'a> {
    pub fn new(source_map: Arc<SourceMap>, allocator: &'a mut HeapAllocator) -> Self {
        let handle = allocator.strings.intern("");
        let blank_handle = allocator.strings.intern("");
        let this_handle = allocator.strings.intern("this");
        Self {
            source_map,
            allocator,
            current_function: FunctionObject::new(handle, 0),
            current_function_id: None,
            loop_contexts: Vec::new(),
            compiler_state: CompilerState::new(blank_handle, this_handle),
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
        self.emit_opcode(OpCode::ModuleReturn, SourceSpan::default());

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

    fn begin_scope(&mut self) {
        self.compiler_state.scope_depth += 1;
    }

    fn end_scope(&mut self, span: SourceSpan) {
        let current = &mut self.compiler_state;
        current.scope_depth -= 1;

        let mut instructions = Vec::new();

        for i in (0..current.local_count).rev() {
            if let Some(local) = current.locals.get(i) {
                if let Some(depth) = local.depth {
                    if depth > current.scope_depth {
                        let instruction = if local.is_captured {
                            OpCode::CloseUpvalue
                        } else {
                            OpCode::Pop
                        };
                        instructions.push(instruction);
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        current.local_count -= instructions.len();

        for instruction in instructions {
            self.emit_opcode(instruction, span);
        }
    }

    fn add_local(
        &mut self,
        handle: crate::StringHandle,
        span: SourceSpan,
    ) -> Result<(), QangCompilerError> {
        let current = &mut self.compiler_state;
        if current.local_count >= STACK_MAX {
            Err(QangCompilerError::new_assembler_error(
                "Too many local variables in scope.".to_string(),
                span,
            ))
        } else {
            let local = Local::new(handle);
            if current.local_count < current.locals.len() {
                current.locals[current.local_count] = local;
            } else {
                current.locals.push(local);
            }
            current.local_count += 1;
            Ok(())
        }
    }

    fn declare_variable(
        &mut self,
        handle: crate::StringHandle,
        span: SourceSpan,
    ) -> Result<(), QangCompilerError> {
        let current = &self.compiler_state;
        if current.scope_depth == 0 {
            return Ok(());
        }
        for i in (0..current.local_count).rev() {
            if let Some(local) = current.locals.get(i) {
                if local
                    .depth
                    .map(|local_depth| local_depth < current.scope_depth)
                    .unwrap_or(false)
                {
                    break;
                }

                if local.name == handle {
                    return Err(QangCompilerError::new_assembler_error(
                        "Already a variable with this name in this scope.".to_string(),
                        span,
                    ));
                }
            }
        }

        self.add_local(handle, span)?;
        Ok(())
    }

    fn define_variable(
        &mut self,
        handle: Option<crate::StringHandle>,
        span: SourceSpan,
    ) -> Result<(), QangCompilerError> {
        if self.compiler_state.scope_depth > 0 {
            self.mark_initialized();
            return Ok(());
        }
        let handle = handle.expect("Expected an object handle when defining global variables.");

        self.emit_constant_opcode(
            OpCode::DefineGlobal,
            OpCode::DefineGlobal16,
            Value::string(handle),
            span,
        )?;

        Ok(())
    }

    fn mark_initialized(&mut self) {
        let current = &mut self.compiler_state;
        if current.scope_depth == 0 {
            return;
        }
        if current.local_count > 0
            && let Some(local) = current.locals.get_mut(current.local_count - 1)
        {
            local.depth = Some(current.scope_depth)
        }
    }

    fn parse_variable(
        &mut self,
        identifier: crate::StringHandle,
        span: SourceSpan,
    ) -> Result<bool, QangCompilerError> {
        self.declare_variable(identifier, span)?;

        if self.compiler_state.scope_depth > 0 {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn handle_variable(
        &mut self,
        handle: crate::StringHandle,
        span: SourceSpan,
        is_assignment: bool,
    ) -> Result<(), QangCompilerError> {
        let (index, get_op, set_op) = {
            if let Some(index) = self.resolve_local_variable(handle, span)? {
                (index as u8, OpCode::GetLocal, OpCode::SetLocal)
            } else if let Some(index) = self.resolve_upvalue(handle, span)? {
                (index as u8, OpCode::GetUpvalue, OpCode::SetUpvalue)
            } else {
                let index = self.current_chunk_mut().add_constant(Value::string(handle));
                if index <= u8::MAX as usize {
                    (index as u8, OpCode::GetGlobal, OpCode::SetGlobal)
                } else {
                    return self.emit_global_variable_access(handle, is_assignment, span);
                }
            }
        };

        let op = if is_assignment { set_op } else { get_op };
        self.emit_opcode_and_byte(op, index, span);

        Ok(())
    }

    fn resolve_upvalue(
        &mut self,
        name: crate::StringHandle,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangCompilerError> {
        self.compiler_state.resolve_upvalue(name, span)
    }

    fn resolve_local_variable(
        &self,
        handle: crate::StringHandle,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangCompilerError> {
        self.compiler_state.resolve_local_variable(handle, span)
    }

    fn emit_return(&mut self, span: SourceSpan) {
        let is_init = matches!(self.compiler_state.kind, FunctionKind::Initializer);
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
        ctx: &VisitorContext,
    ) -> Result<(), QangCompilerError> {
        let variable_name = match ctx.nodes.get_node(node_id) {
            AstNode::Identifier(identifier) => identifier.name,
            AstNode::SuperExpr(_) => self.allocator.strings.intern("super"),
            _ => {
                return Err(QangCompilerError::new_assembler_error(
                    format!(
                        "Unexpected node type for variable access: {:?}",
                        ctx.nodes.get_node(node_id)
                    ),
                    span,
                ));
            }
        };

        self.handle_variable(variable_name, span, is_assignment)
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
                Value::string(handle),
                span,
            )
        } else {
            self.emit_constant_opcode(
                OpCode::GetGlobal,
                OpCode::GetGlobal16,
                Value::string(handle),
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
        self.compile_function_with_superclass(
            func_node_id,
            func_expr,
            ctx,
            false,
            FunctionKind::Function,
        )
    }

    fn compile_function_with_superclass(
        &mut self,
        func_node_id: NodeId,
        func_expr: FunctionExprNode,
        ctx: &mut VisitorContext,
        has_superclass: bool,
        func_kind: FunctionKind,
    ) -> Result<(), QangCompilerError> {
        let func_arity = ctx.nodes.array.size(func_expr.parameters);

        let identifier = ctx.nodes.get_identifier_node(func_expr.name).node;
        let function = FunctionObject::new(identifier.name, func_arity);

        let old_function = std::mem::replace(&mut self.current_function, function);

        let old_loop_contexts = std::mem::take(&mut self.loop_contexts);

        let old_func_id = self.current_function_id;
        self.current_function_id = Some(func_node_id);

        self.compiler_state.push(has_superclass, func_kind);
        self.begin_scope();

        let param_count = ctx.nodes.array.size(func_expr.parameters);
        for i in 0..param_count {
            if let Some(param_id) = ctx.nodes.array.get_node_id_at(func_expr.parameters, i) {
                let param = ctx.nodes.get_identifier_node(param_id);
                let is_local = self.parse_variable(param.node.name, param.node.span)?;
                self.define_variable(
                    if is_local {
                        None
                    } else {
                        Some(param.node.name)
                    },
                    param.node.span,
                )?;
            }
        }

        let body_node = ctx.nodes.get_block_stmt_node(func_expr.body);
        self.visit_block_statement(body_node, ctx)?;

        self.emit_return(func_expr.span);

        let old_state = self
            .compiler_state
            .pop()
            .expect("Function state should exist");

        let mut compiled_function = std::mem::replace(&mut self.current_function, old_function);
        compiled_function.upvalue_count = old_state.upvalues.len();

        self.loop_contexts = old_loop_contexts;

        self.current_function_id = old_func_id;

        let function_handle = self.allocator.allocate_function(compiled_function);
        self.emit_constant_opcode(
            OpCode::Closure,
            OpCode::Closure16,
            Value::function(function_handle),
            func_expr.span,
        )?;

        for upvalue in &old_state.upvalues {
            let is_local_byte = if upvalue.is_local { 1 } else { 0 };
            self.emit_byte(is_local_byte, func_expr.span);
            self.emit_byte(upvalue.index, func_expr.span);
        }

        Ok(())
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

    fn handle_map_expression(
        &mut self,
        callee: TypedNodeRef<ExprNode>,
        map_expr: MapExprNode,
        map_node_id: NodeId,
        ctx: &mut VisitorContext,
    ) -> Result<(), QangCompilerError> {
        self.compile_map_expression(map_node_id, map_expr, ctx)?;

        self.visit_expression(callee, ctx)?;

        self.emit_opcode_and_byte(OpCode::Call, 1, map_expr.span);
        Ok(())
    }

    fn compile_map_expression(
        &mut self,
        map_node_id: NodeId,
        map_expr: MapExprNode,
        ctx: &mut VisitorContext,
    ) -> Result<(), QangCompilerError> {
        let map_name_handle = self.allocator.strings.intern("<map>");
        let func_arity = 1;

        let function = FunctionObject::new(map_name_handle, func_arity);

        let old_function = std::mem::replace(&mut self.current_function, function);

        let old_loop_contexts = std::mem::take(&mut self.loop_contexts);

        let old_func_id = self.current_function_id;
        self.current_function_id = Some(map_node_id);

        self.compiler_state.push(false, FunctionKind::Function);
        self.begin_scope();

        let param_identifier = ctx.nodes.get_identifier_node(map_expr.parameter);
        let param_handle = param_identifier.node.name;
        let is_local = self.parse_variable(param_handle, param_identifier.node.span)?;
        self.define_variable(
            if is_local { None } else { Some(param_handle) },
            param_identifier.node.span,
        )?;

        let return_node = ReturnStmtNode {
            value: Some(map_expr.body),
            span: map_expr.span,
        };
        self.visit_return_statement(TypedNodeRef::new(map_expr.body, return_node), ctx)?;

        self.end_scope(map_expr.span);
        let old_state = self
            .compiler_state
            .pop()
            .expect("Map expression state should exist");

        let mut compiled_function = std::mem::replace(&mut self.current_function, old_function);
        compiled_function.upvalue_count = old_state.upvalues.len();

        self.loop_contexts = old_loop_contexts;

        self.current_function_id = old_func_id;

        let function_handle = self.allocator.allocate_function(compiled_function);
        self.emit_constant_opcode(
            OpCode::Closure,
            OpCode::Closure16,
            Value::function(function_handle),
            map_expr.span,
        )?;

        for upvalue in &old_state.upvalues {
            let is_local_byte = if upvalue.is_local { 1 } else { 0 };
            self.emit_byte(is_local_byte, map_expr.span);
            self.emit_byte(upvalue.index, map_expr.span);
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
            let is_init = matches!(self.compiler_state.kind, FunctionKind::Initializer);

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
        self.emit_constant(Value::string(string.node.value), string.node.span)
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
            self.compiler_state.kind,
            FunctionKind::Method | FunctionKind::Initializer
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
        if !matches!(
            self.compiler_state.kind,
            FunctionKind::Method | FunctionKind::Initializer
        ) {
            return Err(QangCompilerError::new_assembler_error(
                "Cannot use 'super' outside of a class method.".to_string(),
                super_expr.node.span,
            ));
        }

        if !self.compiler_state.has_superclass {
            return Err(QangCompilerError::new_assembler_error(
                "Can't use 'super' in a class with no superclass.".to_string(),
                super_expr.node.span,
            ));
        }

        self.emit_opcode_and_byte(OpCode::GetLocal, 0, super_expr.node.span);

        self.emit_variable_access(super_expr.id, super_expr.node.span, false, ctx)?;

        let method_identifier = ctx.nodes.get_identifier_node(super_expr.node.method);
        self.emit_constant_opcode(
            OpCode::GetSuper,
            OpCode::GetSuper16,
            Value::string(method_identifier.node.name),
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
                    self.visit_expression(value, ctx)?;
                    self.emit_variable_access(assignment_target.id, identifier.span, true, ctx)?;
                }
                AssignmentTargetNode::Property(property) => {
                    self.visit_expression(ctx.nodes.get_expr_node(property.object), ctx)?;
                    self.visit_expression(value, ctx)?;
                    let identifier_handle =
                        ctx.nodes.get_identifier_node(property.property).node.name;
                    self.emit_constant_opcode(
                        OpCode::SetProperty,
                        OpCode::SetProperty16,
                        Value::string(identifier_handle),
                        property.span,
                    )?;
                }
                AssignmentTargetNode::Index(index) => {
                    let array = ctx.nodes.get_expr_node(index.object);
                    self.visit_expression(array, ctx)?;
                    let index_expr = ctx.nodes.get_expr_node(index.index);
                    self.visit_expression(index_expr, ctx)?;
                    self.visit_expression(value, ctx)?;
                    self.emit_opcode(OpCode::SetArrayIndex, index.span);
                }
            },
            _ => match assignment_target.node {
                AssignmentTargetNode::Identifier(identifier) => {
                    self.emit_variable_access(assignment_target.id, identifier.span, false, ctx)?;
                    self.visit_expression(value, ctx)?;
                    self.emit_compound_assignment_op(
                        assignment.node.operator,
                        assignment.node.span,
                    )?;
                    self.emit_variable_access(assignment_target.id, identifier.span, true, ctx)?;
                }
                AssignmentTargetNode::Property(property) => {
                    let property_object = ctx.nodes.get_expr_node(property.object);
                    self.visit_expression(property_object, ctx)?;
                    self.visit_expression(property_object, ctx)?;
                    let identifier_handle =
                        ctx.nodes.get_identifier_node(property.property).node.name;
                    self.emit_constant_opcode(
                        OpCode::GetProperty,
                        OpCode::GetProperty16,
                        Value::string(identifier_handle),
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
                        Value::string(identifier_handle),
                        property.span,
                    )?;
                }
                AssignmentTargetNode::Index(index_node) => {
                    let array = ctx.nodes.get_expr_node(index_node.object);
                    let index = ctx.nodes.get_expr_node(index_node.index);

                    self.visit_expression(array, ctx)?;
                    self.visit_expression(index, ctx)?;

                    self.visit_expression(array, ctx)?;
                    self.visit_expression(index, ctx)?;

                    self.emit_opcode(OpCode::GetArrayIndex, index.node.span());

                    self.visit_expression(value, ctx)?;

                    self.emit_compound_assignment_op(
                        assignment.node.operator,
                        assignment.node.span,
                    )?;

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

        let is_local = self.parse_variable(identifier.node.name, identifier.node.span)?;

        if let Some(expr) = var_decl
            .node
            .initializer
            .map(|id| ctx.nodes.get_expr_node(id))
        {
            self.visit_expression(expr, ctx)?;
        } else {
            self.emit_opcode(OpCode::Nil, var_decl.node.span);
        }

        if is_local {
            self.define_variable(None, var_decl.node.span)?;
        } else {
            self.define_variable(Some(identifier.node.name), var_decl.node.span)?;
        }

        Ok(())
    }

    fn visit_import_module_declaration(
        &mut self,
        import_decl: TypedNodeRef<ImportModuleDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(import_decl.node.name);

        let is_local = self.parse_variable(identifier.node.name, identifier.node.span)?;

        self.emit_constant_opcode(
            OpCode::Module,
            OpCode::Module16,
            Value::string(import_decl.node.path),
            import_decl.node.span,
        )?;

        if is_local {
            self.define_variable(None, import_decl.node.span)?;
        } else {
            self.define_variable(Some(identifier.node.name), import_decl.node.span)?;
        }

        Ok(())
    }

    fn visit_identifier(
        &mut self,
        identifier: TypedNodeRef<IdentifierNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.emit_variable_access(identifier.id, identifier.node.span, false, ctx)
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
        self.begin_scope();

        if let Some(initializer_id) = for_stmt.node.initializer {
            let initializer = ctx.nodes.get_for_initializer_node(initializer_id);
            match initializer.node {
                ForInitializerNode::VarDecl(var_decl) => {
                    self.visit_variable_declaration(
                        TypedNodeRef::new(initializer.id, var_decl),
                        ctx,
                    )?;
                }
                ForInitializerNode::Expr(expr) => {
                    self.visit_expression(TypedNodeRef::new(initializer.id, expr), ctx)?;
                    self.emit_opcode(OpCode::Pop, expr.span());
                }
            }
        }

        let mut loop_start = self.current_chunk_mut().code.len();
        let mut exit_jump: Option<usize> = None;
        let increment_start: Option<usize>;

        let loop_context = LoopContext {
            continue_jumps: Vec::new(),
            break_jumps: Vec::new(),
            node_id: for_stmt.id,
        };
        self.loop_contexts.push(loop_context);

        if let Some(condition_id) = for_stmt.node.condition {
            let condition = ctx.nodes.get_expr_node(condition_id);
            let condition_jump = self.emit_jump(OpCode::Jump, condition.node.span());
            loop_start = self.current_chunk_mut().code.len();

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

        for break_jump in loop_context.break_jumps {
            self.patch_jump(break_jump, for_stmt.node.span)?;
        }

        self.end_scope(for_stmt.node.span);

        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: TypedNodeRef<FunctionDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let func_expr = ctx.nodes.get_func_expr_node(func_decl.node.function);

        let identifier = ctx.nodes.get_identifier_node(func_expr.node.name);
        let is_local = self.parse_variable(identifier.node.name, identifier.node.span)?;

        self.compile_function(func_decl.node.function, func_expr.node, ctx)?;

        self.define_variable(
            if is_local {
                None
            } else {
                Some(identifier.node.name)
            },
            func_decl.node.span,
        )?;

        Ok(())
    }

    fn visit_break_statement(
        &mut self,
        break_stmt: TypedNodeRef<BreakStmtNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if self.loop_contexts.is_empty() {
            return Err(QangCompilerError::new_assembler_error(
                "'break' can only be used inside loops.".to_string(),
                break_stmt.node.span,
            ));
        }

        let jump = self.emit_jump(OpCode::Jump, break_stmt.node.span);

        if let Some(loop_context) = self.loop_contexts.last_mut() {
            loop_context.break_jumps.push(jump);
        }

        Ok(())
    }

    fn visit_continue_statement(
        &mut self,
        continue_stmt: TypedNodeRef<ContinueStmtNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if self.loop_contexts.is_empty() {
            return Err(QangCompilerError::new_assembler_error(
                "'continue' can only be used inside loops.".to_string(),
                continue_stmt.node.span,
            ));
        }

        let continue_position = self.current_chunk_mut().code.len();

        if let Some(loop_context) = self.loop_contexts.last_mut() {
            loop_context.continue_jumps.push(continue_position);
        }

        Ok(())
    }

    fn visit_block_statement(
        &mut self,
        block_stmt: TypedNodeRef<BlockStmtNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        let length = ctx.nodes.array.size(block_stmt.node.decls);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(block_stmt.node.decls, i) {
                let decl = ctx.nodes.get_decl_node(node_id);
                self.visit_declaration(decl, ctx)?;
            }
        }

        self.end_scope(block_stmt.node.span);

        Ok(())
    }

    fn visit_lambda_declaration(
        &mut self,
        lambda_decl: TypedNodeRef<LambdaDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier = ctx.nodes.get_identifier_node(lambda_decl.node.name);
        let lambda_expr = ctx.nodes.get_lambda_expr_node(lambda_decl.node.lambda);

        let is_local = self.parse_variable(identifier.node.name, identifier.node.span)?;

        self.visit_lambda_expression(lambda_expr, ctx)?;

        self.define_variable(
            if is_local {
                None
            } else {
                Some(identifier.node.name)
            },
            lambda_decl.node.span,
        )?;

        Ok(())
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: TypedNodeRef<LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let lambda_name_handle = self.allocator.strings.intern("<anonymous>");
        let func_arity = ctx.nodes.array.size(lambda_expr.node.parameters);

        let function = FunctionObject::new(lambda_name_handle, func_arity);

        let old_function = std::mem::replace(&mut self.current_function, function);

        let old_loop_contexts = std::mem::take(&mut self.loop_contexts);

        let old_function_id = self.current_function_id;
        self.current_function_id = Some(lambda_expr.id);

        self.compiler_state.push(false, FunctionKind::Function);
        self.begin_scope();

        let param_count = ctx.nodes.array.size(lambda_expr.node.parameters);
        for i in 0..param_count {
            if let Some(param_id) = ctx
                .nodes
                .array
                .get_node_id_at(lambda_expr.node.parameters, i)
            {
                let param = ctx.nodes.get_identifier_node(param_id);
                let is_local = self.parse_variable(param.node.name, param.node.span)?;
                self.define_variable(
                    if is_local {
                        None
                    } else {
                        Some(param.node.name)
                    },
                    param.node.span,
                )?;
            }
        }

        let lambda_body = ctx.nodes.get_lambda_body_node(lambda_expr.node.body);
        match lambda_body.node {
            LambdaBodyNode::Block(body) => {
                self.visit_block_statement(TypedNodeRef::new(lambda_body.id, body), ctx)?;
                self.emit_return(lambda_expr.node.span);
            }
            LambdaBodyNode::Expr(_) => {
                let return_node = ReturnStmtNode {
                    value: Some(lambda_body.id),
                    span: lambda_body.node.span(),
                };
                self.visit_return_statement(TypedNodeRef::new(lambda_body.id, return_node), ctx)?;
            }
        }

        self.end_scope(lambda_expr.node.span);
        let old_state = self
            .compiler_state
            .pop()
            .expect("Lambda state should exist");

        let mut compiled_function = std::mem::replace(&mut self.current_function, old_function);
        compiled_function.upvalue_count = old_state.upvalues.len();

        self.loop_contexts = old_loop_contexts;

        self.current_function_id = old_function_id;

        let function_handle = self.allocator.allocate_function(compiled_function);
        self.emit_constant_opcode(
            OpCode::Closure,
            OpCode::Closure16,
            Value::function(function_handle),
            lambda_expr.node.span,
        )?;

        for upvalue in &old_state.upvalues {
            let is_local_byte = if upvalue.is_local { 1 } else { 0 };
            self.emit_byte(is_local_byte, lambda_expr.node.span);
            self.emit_byte(upvalue.index, lambda_expr.node.span);
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
            CallOperationNode::Call(args) => {
                let arg_length = ctx.nodes.array.size(args.args);
                if arg_length > u8::MAX as usize {
                    return Err(QangCompilerError::new_assembler_error(
                        "Functions may only take up to 256 arguments.".to_string(),
                        call.node.span,
                    ));
                }

                if let ExprNode::Primary(PrimaryNode::Super(super_expr)) = callee.node {
                    if !matches!(
                        self.compiler_state.kind,
                        FunctionKind::Method | FunctionKind::Initializer
                    ) {
                        return Err(QangCompilerError::new_assembler_error(
                            "Cannot use 'super' outside of a class method.".to_string(),
                            super_expr.span,
                        ));
                    }

                    self.emit_opcode_and_byte(OpCode::GetLocal, 0, super_expr.span);

                    for i in 0..arg_length {
                        if let Some(node_id) = ctx.nodes.array.get_node_id_at(args.args, i) {
                            let expr = ctx.nodes.get_expr_node(node_id);
                            self.visit_expression(expr, ctx)?;
                        } else {
                            self.emit_opcode(OpCode::Nil, call.node.span);
                        }
                    }

                    self.emit_variable_access(callee.id, super_expr.span, false, ctx)?;

                    let method = ctx.nodes.get_identifier_node(super_expr.method);
                    self.emit_constant_opcode(
                        OpCode::SuperInvoke,
                        OpCode::SuperInvoke16,
                        Value::string(method.node.name),
                        call.node.span,
                    )?;
                    self.emit_byte(arg_length as u8, call.node.span);

                    return Ok(());
                }

                if let ExprNode::Call(property_call) = callee.node
                    && let CallOperationNode::Property(method_name) = ctx
                        .nodes
                        .get_call_operation_node(property_call.operation)
                        .node
                {
                    let expr = ctx.nodes.get_expr_node(property_call.callee);
                    self.visit_expression(expr, ctx)?;

                    let method_handle = ctx
                        .nodes
                        .get_identifier_node(method_name.identifier)
                        .node
                        .name;

                    for i in 0..arg_length {
                        if let Some(node_id) = ctx.nodes.array.get_node_id_at(args.args, i) {
                            let expr = ctx.nodes.get_expr_node(node_id);
                            self.visit_expression(expr, ctx)?;
                        } else {
                            self.emit_opcode(OpCode::Nil, call.node.span);
                        }
                    }

                    self.emit_constant_opcode(
                        OpCode::Invoke,
                        OpCode::Invoke16,
                        Value::string(method_handle),
                        call.node.span,
                    )?;
                    self.emit_byte(arg_length as u8, call.node.span);

                    return Ok(());
                }

                self.visit_expression(callee, ctx)?;

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
            CallOperationNode::Property(identifier) => {
                self.visit_expression(callee, ctx)?;
                let identifier_handle = ctx
                    .nodes
                    .get_identifier_node(identifier.identifier)
                    .node
                    .name;
                self.emit_constant_opcode(
                    OpCode::GetProperty,
                    OpCode::GetProperty16,
                    Value::string(identifier_handle),
                    call.node.span,
                )?;
                Ok(())
            }
            CallOperationNode::Index(expr) => {
                self.visit_expression(callee, ctx)?;
                let expr = ctx.nodes.get_expr_node(expr.index);
                self.visit_expression(expr, ctx)?;
                self.emit_opcode(OpCode::GetArrayIndex, call.node.span);
                Ok(())
            }
            CallOperationNode::OptionalProperty(identifier) => {
                self.visit_expression(callee, ctx)?;
                let identifier_handle = ctx
                    .nodes
                    .get_identifier_node(identifier.identifier)
                    .node
                    .name;
                self.emit_constant_opcode(
                    OpCode::GetOptionalProperty,
                    OpCode::GetOptionalProperty16,
                    Value::string(identifier_handle),
                    call.node.span,
                )?;
                Ok(())
            }
            CallOperationNode::Map(map_expr) => {
                self.handle_map_expression(callee, map_expr, call_operation.id, ctx)
            }
            CallOperationNode::OptionalMap(map_expr) => {
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
                    call_operation.id,
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
            ExprNode::Call(call_expr) => {
                match ctx.nodes.get_call_operation_node(call_expr.operation).node {
                    CallOperationNode::Call(arguments) => {
                        if let ExprNode::Call(inner_call) =
                            ctx.nodes.get_expr_node(call_expr.callee).node
                            && let CallOperationNode::Property(method_name) =
                                ctx.nodes.get_call_operation_node(inner_call.operation).node
                            && ctx
                                .nodes
                                .get_identifier_node(method_name.identifier)
                                .node
                                .name
                                == call_handle
                        {
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
                                Value::string(method_handle),
                                pipe.node.span,
                            )?;
                            self.emit_byte(1, pipe.node.span);
                            return Ok(());
                        }

                        self.visit_expression(ctx.nodes.get_expr_node(call_expr.callee), ctx)?;

                        self.visit_expression(left, ctx)?;

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
                    CallOperationNode::Property(method_name) => {
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
                            Value::string(method_handle),
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

        let class_handle = name.node.name;
        self.emit_constant_opcode(
            OpCode::Class,
            OpCode::Class16,
            Value::string(class_handle),
            name.node.span,
        )?;

        let is_local = self.parse_variable(class_handle, name.node.span)?;
        self.define_variable(
            if is_local { None } else { Some(class_handle) },
            name.node.span,
        )?;

        self.handle_variable(class_handle, name.node.span, false)?;

        if let Some(superclass) = superclass {
            if class_handle == superclass.node.name {
                return Err(QangCompilerError::new_assembler_error(
                    "A class cannot inherit from itself.".to_string(),
                    superclass.node.span,
                ));
            }

            self.begin_scope();

            self.handle_variable(class_handle, name.node.span, false)?;

            self.handle_variable(superclass.node.name, superclass.node.span, false)?;

            let super_handle = self.allocator.strings.intern("super");
            self.add_local(super_handle, class_decl.node.span)?;
            self.define_variable(None, class_decl.node.span)?;

            self.handle_variable(super_handle, superclass.node.span, true)?;

            self.emit_opcode(OpCode::Inherit, superclass.node.span);
        }

        let length = ctx.nodes.array.size(class_decl.node.members);
        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(class_decl.node.members, i) {
                let member = ctx.nodes.get_class_member_node(node_id);

                match member.node {
                    ClassMemberNode::Method(function) => {
                        let method_name = ctx.nodes.get_identifier_node(function.name);
                        let init_name = self.allocator.strings.intern("init");
                        let is_initializer = method_name.node.name == init_name;
                        let func_kind = if is_initializer {
                            FunctionKind::Initializer
                        } else {
                            FunctionKind::Method
                        };

                        let has_superclass = class_decl.node.superclass.is_some();
                        self.compile_function_with_superclass(
                            node_id,
                            function,
                            ctx,
                            has_superclass,
                            func_kind,
                        )?;

                        self.emit_constant_opcode(
                            OpCode::Method,
                            OpCode::Method16,
                            Value::string(method_name.node.name),
                            function.span,
                        )?;
                    }
                    ClassMemberNode::Field(field) => {
                        let field_name = ctx.nodes.get_identifier_node(field.name);

                        if let Some(init_id) = field.initializer {
                            let initializer = ctx.nodes.get_expr_node(init_id);
                            self.visit_expression(initializer, ctx)?;
                        } else {
                            self.emit_opcode(OpCode::Nil, field.span);
                        }

                        self.emit_constant_opcode(
                            OpCode::InitField,
                            OpCode::InitField16,
                            Value::string(field_name.node.name),
                            field_name.node.span,
                        )?;
                    }
                }
            }
        }

        self.emit_opcode(OpCode::Pop, class_decl.node.span);

        if class_decl.node.superclass.is_some() {
            self.end_scope(class_decl.node.span);
        }

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
        self.emit_constant(Value::string(identifier_handle), identifier_node.span)?;
        let value = ctx.nodes.get_expr_node(entry.node.value);
        self.visit_expression(value, ctx)?;
        Ok(())
    }
}
