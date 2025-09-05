use crate::{
    ErrorReporter, FunctionHandle, QangCompilerError, SourceMap, Value,
    backend::chunk::{Chunk, OpCode, SourceLocation},
    error::{CompilerError, ErrorMessageFormat},
    frontend::{
        node_visitor::{NodeVisitor, VisitorContext},
        nodes::*,
        parse::Parser,
        source::DEFALT_SOURCE_MAP,
        typed_node_arena::{
            AssignmentTargetNode, CallOperationNode, ClassMemberNode, DeclNode, ExprNode,
            ForInitializerNode, LambdaBodyNode, NodeId, PrimaryNode, TypedNodeArena, TypedNodeRef,
        },
    },
    memory::{FunctionObject, HeapAllocator, StringHandle},
};

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

#[derive(Debug, Clone, PartialEq, Default)]
struct LoopContext {
    continue_jumps: Vec<usize>,
    break_jumps: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Default)]
struct AssemblerState {
    kind: AssemblerKind,
    function: FunctionObject,
    locals: Vec<Local>,
    local_count: usize,
    scope_depth: usize,
    upvalues: Vec<Upvalue>,
    enclosing: Option<Box<AssemblerState>>,
    has_superclass: bool,
    loop_contexts: Vec<LoopContext>,
    blank_handle: StringHandle,
    this_handle: StringHandle,
}

impl AssemblerState {
    fn new(handle: StringHandle, blank_handle: StringHandle, this_handle: StringHandle) -> Self {
        let mut locals = Vec::with_capacity(u8::MAX as usize);
        locals.push(Local::new(blank_handle));
        Self {
            kind: AssemblerKind::Script,
            function: FunctionObject::new(handle, 0),
            locals,
            local_count: 1,
            scope_depth: 0,
            upvalues: Vec::with_capacity(u8::MAX as usize),
            enclosing: None,
            has_superclass: false,
            loop_contexts: Vec::new(),
            blank_handle,
            this_handle,
        }
    }

    fn push(
        &mut self,
        handle: StringHandle,
        arity: usize,
        kind: AssemblerKind,
        has_superclass: bool,
    ) -> &mut Self {
        let mut locals = Vec::with_capacity(u8::MAX as usize);
        if matches!(kind, AssemblerKind::Method | AssemblerKind::Initializer) {
            let mut this_local = Local::new(self.this_handle);
            this_local.depth = Some(0); // Initialize 'this' as available immediately
            locals.push(this_local);
        } else {
            locals.push(Local::new(self.blank_handle));
        }
        let previous = std::mem::replace(
            self,
            Self {
                kind,
                function: FunctionObject::new(handle, arity),
                locals,
                local_count: 1,
                scope_depth: 0,
                upvalues: Vec::with_capacity(u8::MAX as usize),
                enclosing: None,
                has_superclass,
                loop_contexts: Vec::new(),
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
        handle: StringHandle,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangCompilerError> {
        for i in (0..self.local_count).rev() {
            if let Some(local) = self.locals.get(i)
                && local.name == handle
            {
                if local.depth.is_none() {
                    return Err(QangCompilerError::new_syntax_error(
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
        handle: StringHandle,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangCompilerError> {
        if let Some(enclosing) = self.enclosing.as_mut() {
            // First check if it's a local variable in the immediately enclosing scope
            if let Some(local_index) = enclosing.resolve_local_variable(handle, span)? {
                if let Some(local) = enclosing.locals.get_mut(local_index) {
                    local.is_captured = true;
                }
                // Add as upvalue with is_local=true
                return self.add_upvalue(local_index, true, span).map(Some);
            }

            // If not found as local, recursively check outer scopes
            if let Some(upvalue_index) = enclosing.resolve_upvalue(handle, span)? {
                // Add as upvalue with is_local=false (it's an upvalue in enclosing scope)
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
        let upvalue_count = self.function.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = self.upvalues[i];
            if upvalue.index == index as u8 && upvalue.is_local == is_local {
                return Ok(i);
            }
        }

        if upvalue_count == u8::MAX as usize {
            return Err(QangCompilerError::new_syntax_error(
                "Can only close over up to 256 variables.".to_string(),
                span,
            ));
        }

        self.upvalues.push(Upvalue {
            index: index as u8,
            is_local,
        });
        self.function.upvalue_count += 1;

        Ok(upvalue_count)
    }
}

pub struct CompilerPipeline<'a> {
    source_map: SourceMap,
    allocator: &'a mut HeapAllocator,
    error_message_format: ErrorMessageFormat,
}

impl<'a> CompilerPipeline<'a> {
    pub fn new(source_map: SourceMap, allocator: &'a mut HeapAllocator) -> Self {
        Self {
            source_map,
            allocator,
            error_message_format: ErrorMessageFormat::Minimal,
        }
    }

    pub fn error_message_format(mut self, format: ErrorMessageFormat) -> Self {
        self.error_message_format = format;

        self
    }

    pub fn run(self) -> Result<QangProgram, CompilerError> {
        let nodes = TypedNodeArena::new();
        let mut parser = Parser::new(&self.source_map, nodes, &mut self.allocator.strings);
        let program = parser.parse();
        let (errors, mut nodes) = parser.into_parts();
        match Assembler::new(self.allocator).compile(program, &mut nodes, &self.source_map, errors)
        {
            Ok(program) => Ok(QangProgram::new(self.allocator.allocate_function(program))),
            Err(error) => Err(CompilerError::new(
                error
                    .all()
                    .iter()
                    .cloned()
                    .map(|e| e.into_formatted(&self.source_map))
                    .collect(),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
struct Local {
    name: StringHandle,
    depth: Option<usize>,
    is_captured: bool,
}

impl Local {
    fn new(name: StringHandle) -> Self {
        Self {
            name,
            depth: None,
            is_captured: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default, Copy)]
struct Upvalue {
    index: u8,
    is_local: bool,
}

pub const FRAME_MAX: usize = 64;
pub const STACK_MAX: usize = FRAME_MAX * 256;

#[derive(Debug, Clone, Copy, PartialEq)]
enum AssemblerKind {
    Script,
    Function,
    Method,
    Initializer,
}

impl Default for AssemblerKind {
    fn default() -> Self {
        Self::Script
    }
}

pub struct Assembler<'a> {
    source_map: &'a SourceMap,
    allocator: &'a mut HeapAllocator,
    state: AssemblerState,
}

impl<'a> Assembler<'a> {
    pub fn new(allocator: &'a mut HeapAllocator) -> Self {
        let handle = allocator.strings.intern("(script)");
        let blank_handle = allocator.strings.intern("");
        let this_handle = allocator.strings.intern("this");

        Self {
            source_map: &DEFALT_SOURCE_MAP,
            allocator,
            state: AssemblerState::new(handle, blank_handle, this_handle),
        }
    }

    fn reset(&mut self) {
        let handle = self.allocator.strings.intern("(script)");
        self.source_map = &DEFALT_SOURCE_MAP;
        self.state = AssemblerState::new(handle, self.state.blank_handle, self.state.this_handle);
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.state.function.chunk
    }

    pub fn compile(
        &mut self,
        program: NodeId,
        nodes: &mut TypedNodeArena,
        source_map: &'a SourceMap,
        mut errors: ErrorReporter,
    ) -> Result<FunctionObject, CompilerError> {
        self.source_map = source_map;
        let handle = self.allocator.strings.intern(&source_map.name);
        self.state = AssemblerState::new(handle, self.state.blank_handle, self.state.this_handle);
        let mut ctx = VisitorContext::new(nodes, &mut errors);

        let program_node = ctx.nodes.get_program_node(program);

        self.visit_program(program_node, &mut ctx)
            .map_err(|err| CompilerError::new(vec![err]))?;

        self.emit_opcode(OpCode::Nil, SourceSpan::default());
        self.emit_opcode(OpCode::Return, SourceSpan::default());

        if errors.has_errors() {
            Err(CompilerError::new(errors.take_errors()))
        } else {
            let artifacts = std::mem::take(&mut self.state);
            self.reset();

            Ok(artifacts.function)
        }
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
            return Err(QangCompilerError::new_syntax_error(
                "Too many constants in function".to_string(),
                span,
            ));
        }

        Ok(())
    }

    fn make_constant(&mut self, value: Value, span: SourceSpan) -> Result<u8, QangCompilerError> {
        let index = self.current_chunk_mut().add_constant(value);

        if index > u8::MAX as usize {
            Err(QangCompilerError::new_syntax_error(
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
            return Err(QangCompilerError::new_syntax_error(
                "Too many constants in function (max 65536)".to_string(),
                span,
            ));
        }

        Ok(())
    }

    fn emit_return(&mut self, span: SourceSpan) {
        if matches!(self.state.kind, AssemblerKind::Initializer) {
            self.emit_opcode_and_byte(OpCode::GetLocal, 0, span);
        } else {
            self.emit_opcode(OpCode::Nil, span);
        }
        self.emit_opcode(OpCode::Return, span);
    }

    // fn is_tail_call(&self, expr: ExprNode) -> bool {
    //     matches!(expr, ExprNode::Call(_))
    // }

    // fn emit_tail_call(&mut self, arg_count: u8, span: SourceSpan) {
    //     self.emit_opcode_and_byte(OpCode::TailCall, arg_count, span);
    // }

    fn begin_scope(&mut self) {
        self.state.scope_depth += 1;
    }

    fn end_scope(&mut self, span: SourceSpan) {
        let current = &mut self.state;
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
        handle: StringHandle,
        span: SourceSpan,
    ) -> Result<(), QangCompilerError> {
        let current = &mut self.state;
        if current.local_count >= STACK_MAX {
            Err(QangCompilerError::new_syntax_error(
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
        handle: StringHandle,
        span: SourceSpan,
    ) -> Result<(), QangCompilerError> {
        let current = &self.state;
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
                    return Err(QangCompilerError::new_syntax_error(
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
        handle: Option<StringHandle>,
        span: SourceSpan,
    ) -> Result<(), QangCompilerError> {
        if self.state.scope_depth > 0 {
            self.mark_initialized();

            return Ok(());
        }
        let handle = handle.expect("Expected an object handle when defining global variables.");

        self.emit_constant_opcode(
            OpCode::DefineGlobal,
            OpCode::DefineGlobal16,
            Value::String(handle),
            span,
        )?;

        Ok(())
    }

    fn mark_initialized(&mut self) {
        let current = &mut self.state;
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
        identifer: StringHandle,
        span: SourceSpan,
    ) -> Result<bool, QangCompilerError> {
        self.declare_variable(identifer, span)?;

        if self.state.scope_depth > 0 {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn handle_variable(
        &mut self,
        handle: StringHandle,
        span: SourceSpan,
        is_assignment: bool,
    ) -> Result<(), QangCompilerError> {
        let (index, get_op, set_op) = {
            if let Some(index) = self.resolve_local_variable(handle, span)? {
                (index as u8, OpCode::GetLocal, OpCode::SetLocal)
            } else if let Some(index) = self.resolve_upvalue(handle, span)? {
                (index as u8, OpCode::GetUpvalue, OpCode::SetUpvalue)
            } else {
                let index = self.current_chunk_mut().add_constant(Value::String(handle));
                if index <= u8::MAX as usize {
                    (index as u8, OpCode::GetGlobal, OpCode::SetGlobal)
                } else {
                    // For 16-bit indices, we'll handle this differently
                    return self.emit_global_variable_access(handle, is_assignment, span);
                }
            }
        };

        let op = if is_assignment { set_op } else { get_op };
        self.emit_opcode_and_byte(op, index, span);

        Ok(())
    }

    fn emit_global_variable_access(
        &mut self,
        handle: StringHandle,
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

    fn resolve_upvalue(
        &mut self,
        name: StringHandle,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangCompilerError> {
        self.state.resolve_upvalue(name, span)
    }

    fn resolve_local_variable(
        &self,
        handle: StringHandle,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangCompilerError> {
        self.state.resolve_local_variable(handle, span)
    }

    fn handle_function(
        &mut self,
        kind: AssemblerKind,
        func_expr: FunctionExprNode,
        has_superclass: bool,
        ctx: &mut VisitorContext,
    ) -> Result<(), QangCompilerError> {
        let identifier = ctx.nodes.get_identifier_node(func_expr.name).node;
        if matches!(kind, AssemblerKind::Function) {
            self.parse_variable(identifier.name, func_expr.span)?;
        }

        let function_name_handle = identifier.name;
        let arity = ctx.nodes.array.size(func_expr.parameters);
        self.state
            .push(function_name_handle, arity, kind, has_superclass);
        self.begin_scope();

        if arity > 255 {
            return Err(QangCompilerError::new_syntax_error(
                "Cannot have more than 255 parameters.".to_string(),
                identifier.span,
            ));
        }

        let length = ctx.nodes.array.size(func_expr.parameters);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(func_expr.parameters, i) {
                let node = ctx.nodes.get_identifier_node(node_id).node;
                let handle = if self.parse_variable(node.name, node.span)? {
                    Some(node.name)
                } else {
                    None
                };

                self.define_variable(handle, node.span)?;
            }
        }

        let body_node = ctx.nodes.get_block_stmt_node(func_expr.body);
        self.visit_block_statement(body_node, ctx)?;
        self.emit_return(func_expr.span);

        let compiler = self
            .state
            .pop()
            .expect("Expected AssemblerState stack to not be empty.");
        let function = compiler.function;
        let upvalue_count = function.upvalue_count;

        let function_handle = self.allocator.allocate_function(function);
        self.emit_constant_opcode(
            OpCode::Closure,
            OpCode::Closure16,
            Value::FunctionDecl(function_handle),
            identifier.span,
        )?;

        for i in 0..upvalue_count {
            let upvalue = compiler.upvalues[i];
            let is_local_byte = if upvalue.is_local { 1 } else { 0 };

            self.emit_byte(is_local_byte, identifier.span);
            self.emit_byte(upvalue.index, identifier.span);
        }

        if matches!(kind, AssemblerKind::Function) {
            self.define_variable(Some(identifier.name), identifier.span)?;
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
        map_expr: TypedNodeRef<MapExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), QangCompilerError> {
        let parameters = ctx.nodes.array.create();
        ctx.nodes.array.push(parameters, map_expr.node.parameter);
        let lambda_expr = LambdaExprNode {
            parameters,
            body: map_expr.node.body,
            span: map_expr.node.span,
        };
        self.visit_lambda_expression(TypedNodeRef::new(map_expr.id, lambda_expr), ctx)?;
        self.visit_expression(callee, ctx)?;
        self.emit_opcode_and_byte(OpCode::Call, 1, map_expr.node.span);
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
        } else if matches!(self.state.kind, AssemblerKind::Initializer) {
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
        this_expr: TypedNodeRef<ThisExprNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if matches!(
            self.state.kind,
            AssemblerKind::Method | AssemblerKind::Initializer
        ) {
            self.emit_opcode_and_byte(OpCode::GetLocal, 0, this_expr.node.span);
            Ok(())
        } else {
            Err(QangCompilerError::new_syntax_error(
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
        if !self.state.has_superclass {
            return Err(QangCompilerError::new_syntax_error(
                "Can't use 'super' in a class with no superclass.".to_string(),
                super_expr.node.span,
            ));
        }

        if !matches!(
            self.state.kind,
            AssemblerKind::Method | AssemblerKind::Initializer
        ) {
            return Err(QangCompilerError::new_syntax_error(
                "Can't use 'super' outside of a class method.".to_string(),
                super_expr.node.span,
            ));
        }

        let this_handle = self.allocator.strings.intern("this");
        self.handle_variable(this_handle, super_expr.node.span, false)?;
        let super_handle = self.allocator.strings.intern("super");
        self.handle_variable(super_handle, super_expr.node.span, false)?;

        let method_identifer = ctx.nodes.get_identifier_node(super_expr.node.method);
        self.emit_constant_opcode(
            OpCode::GetSuper,
            OpCode::GetSuper16,
            Value::String(method_identifer.node.name),
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
        let assignment_target = ctx
            .nodes
            .get_assignment_target_node(assignment.node.target)
            .node;
        let value = ctx.nodes.get_expr_node(assignment.node.value);
        match assignment.node.operator {
            AssignmentOperator::Assign => match assignment_target {
                AssignmentTargetNode::Identifier(identifier) => {
                    self.visit_expression(value, ctx)?;
                    self.handle_variable(identifier.name, identifier.span, true)?;
                }
                AssignmentTargetNode::Property(property) => {
                    self.visit_expression(ctx.nodes.get_expr_node(property.object), ctx)?;
                    let identifier_handle =
                        ctx.nodes.get_identifier_node(property.property).node.name;

                    self.visit_expression(value, ctx)?;
                    self.emit_constant_opcode(
                        OpCode::SetProperty,
                        OpCode::SetProperty16,
                        Value::String(identifier_handle),
                        property.span,
                    )?;
                }
                AssignmentTargetNode::Index(index) => {
                    let array = ctx.nodes.get_expr_node(index.object);
                    self.visit_expression(array, ctx)?;
                    let index = ctx.nodes.get_expr_node(index.index);
                    let index_span = index.node.span();
                    self.visit_expression(index, ctx)?;
                    self.visit_expression(value, ctx)?;
                    self.emit_opcode(OpCode::SetArrayIndex, index_span);
                }
            },
            _ => match assignment_target {
                AssignmentTargetNode::Identifier(identifier) => {
                    self.handle_variable(identifier.name, identifier.span, false)?;
                    self.visit_expression(value, ctx)?;
                    self.emit_compound_assignment_op(
                        assignment.node.operator,
                        assignment.node.span,
                    )?;
                    self.handle_variable(identifier.name, identifier.span, true)?;
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
        let handle = if is_local {
            None
        } else {
            Some(identifier.node.name)
        };
        self.define_variable(handle, identifier.node.span)?;

        if is_local {
            self.handle_variable(identifier.node.name, var_decl.node.span, true)?;
        }

        Ok(())
    }

    fn visit_identifier(
        &mut self,
        identifier: TypedNodeRef<IdentifierNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        self.handle_variable(identifier.node.name, identifier.node.span, false)?;

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
                self.visit_declaration(ctx.nodes.get_decl_node(node_id), ctx)?;
            }
        }

        self.end_scope(block_stmt.node.span);
        Ok(())
    }

    fn visit_declaration(
        &mut self,
        decl: TypedNodeRef<DeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let result = match decl.node {
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
            DeclNode::Stmt(stmt) => self.visit_statement(TypedNodeRef::new(decl.id, stmt), ctx),
        };
        if let Err(error) = result {
            ctx.errors.report_error(error);
        }
        Ok(())
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
        self.state.loop_contexts.push(LoopContext::default());

        let loop_start = self.current_chunk_mut().code.len();
        let condition = ctx.nodes.get_expr_node(while_stmt.node.condition);
        self.visit_expression(condition, ctx)?;

        let body = ctx.nodes.get_stmt_node(while_stmt.node.body);
        let exit_jump = self.emit_jump(OpCode::JumpIfFalse, body.node.span());
        self.emit_opcode(OpCode::Pop, body.node.span());
        self.visit_statement(body, ctx)?;

        let loop_context = self
            .state
            .loop_contexts
            .pop()
            .expect("Loop context should exist");

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
        self.begin_scope();

        self.state.loop_contexts.push(LoopContext::default());

        let initializer = for_stmt
            .node
            .initializer
            .map(|init| ctx.nodes.get_for_initializer_node(init));
        let condition = for_stmt
            .node
            .condition
            .map(|cond| ctx.nodes.get_expr_node(cond));
        let increment = for_stmt
            .node
            .increment
            .map(|inc| ctx.nodes.get_expr_node(inc));
        let body = ctx.nodes.get_stmt_node(for_stmt.node.body);
        if let Some(initializer) = initializer {
            match initializer.node {
                ForInitializerNode::VarDecl(var_decl) => self
                    .visit_variable_declaration(TypedNodeRef::new(initializer.id, var_decl), ctx)?,
                ForInitializerNode::Expr(expr) => {
                    self.visit_expression(TypedNodeRef::new(initializer.id, expr), ctx)?;
                    self.emit_opcode(OpCode::Pop, expr.span());
                }
            }
        }

        let mut loop_start = self.current_chunk_mut().code.len();
        let mut exit_jump: Option<usize> = None;
        let increment_start: Option<usize>;

        if let Some(condition) = condition {
            let condition_jump = self.emit_jump(OpCode::Jump, condition.node.span());
            loop_start = self.current_chunk_mut().code.len();
            self.visit_statement(body, ctx)?;

            increment_start = Some(self.current_chunk_mut().code.len());

            if let Some(increment) = increment {
                self.visit_expression(increment, ctx)?;
                self.emit_opcode(OpCode::Pop, increment.node.span());
            }

            self.patch_jump(condition_jump, condition.node.span())?;
            self.visit_expression(condition, ctx)?;
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse, condition.node.span()));
            self.emit_opcode(OpCode::Pop, condition.node.span());
            self.emit_loop(loop_start, body.node.span())?;
        } else {
            self.visit_statement(body, ctx)?;

            increment_start = Some(self.current_chunk_mut().code.len());

            if let Some(increment) = increment {
                self.visit_expression(increment, ctx)?;
                self.emit_opcode(OpCode::Pop, increment.node.span());
            }

            self.emit_loop(loop_start, body.node.span())?;
        }

        let loop_context = self
            .state
            .loop_contexts
            .pop()
            .expect("Loop context should exist");

        let continue_target = increment_start.unwrap_or(loop_start);
        for continue_jump in loop_context.continue_jumps {
            if continue_target > continue_jump {
                let jump_distance = continue_target - continue_jump - 3;
                if jump_distance > u16::MAX as usize {
                    return Err(QangCompilerError::new_syntax_error(
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
                    return Err(QangCompilerError::new_syntax_error(
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
        self.handle_function(AssemblerKind::Function, func_expr.node, false, ctx)
    }

    fn visit_break_statement(
        &mut self,
        break_stmt: TypedNodeRef<BreakStmtNode>,
        _ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        if !self.state.loop_contexts.is_empty() {
            let jump = self.emit_jump(OpCode::Jump, break_stmt.node.span);
            if let Some(loop_context) = self.state.loop_contexts.last_mut() {
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
        if !self.state.loop_contexts.is_empty() {
            let continue_position = self.current_chunk_mut().code.len();
            if let Some(loop_context) = self.state.loop_contexts.last_mut() {
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
        lamdba_decl: TypedNodeRef<LambdaDeclNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let identifier_node = ctx.nodes.get_identifier_node(lamdba_decl.node.name).node;
        let is_local = self.parse_variable(identifier_node.name, lamdba_decl.node.span)?;

        let lambda_expr = ctx.nodes.get_lambda_expr_node(lamdba_decl.node.lambda);
        self.visit_lambda_expression(lambda_expr, ctx)?;

        let lambda_identifier_handle = if is_local {
            None
        } else {
            Some(identifier_node.name)
        };

        self.define_variable(lambda_identifier_handle, identifier_node.span)?;

        Ok(())
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: TypedNodeRef<LambdaExprNode>,
        ctx: &mut VisitorContext,
    ) -> Result<(), Self::Error> {
        let lambda_name_handle = self.allocator.strings.intern("<anonymous>");

        let arity = ctx.nodes.array.size(lambda_expr.node.parameters);

        self.state
            .push(lambda_name_handle, arity, AssemblerKind::Function, false);
        self.begin_scope();

        if arity > 255 {
            return Err(QangCompilerError::new_syntax_error(
                "Cannot have more than 255 parameters.".to_string(),
                lambda_expr.node.span,
            ));
        }

        for i in 0..arity {
            if let Some(parameter) = ctx
                .nodes
                .array
                .get_node_id_at(lambda_expr.node.parameters, i)
            {
                let identifier = ctx.nodes.get_identifier_node(parameter);
                let is_local = self.parse_variable(identifier.node.name, identifier.node.span)?;

                let handle = if is_local {
                    None
                } else {
                    Some(identifier.node.name)
                };

                self.define_variable(handle, identifier.node.span)?;
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

        let compiler = self.state.pop().expect("Unexpected end of artifact stack.");
        let function = compiler.function;
        let upvalue_count = function.upvalue_count;

        let function_handle = self.allocator.allocate_function(function);
        self.emit_constant_opcode(
            OpCode::Closure,
            OpCode::Closure16,
            Value::FunctionDecl(function_handle),
            lambda_expr.node.span,
        )?;

        for i in 0..upvalue_count {
            let upvalue = compiler.upvalues[i];
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
                    return Err(QangCompilerError::new_syntax_error(
                        "Functions may only take up to 256 arguments.".to_string(),
                        call.node.span,
                    ));
                }

                if let ExprNode::Primary(PrimaryNode::Super(super_expr)) = callee.node {
                    if !self.state.has_superclass {
                        return Err(QangCompilerError::new_syntax_error(
                            "Can't use 'super' in a class with no superclass.".to_string(),
                            super_expr.span,
                        ));
                    }

                    if !matches!(
                        self.state.kind,
                        AssemblerKind::Method | AssemblerKind::Initializer
                    ) {
                        return Err(QangCompilerError::new_syntax_error(
                            "Can't use 'super' outside of a class method.".to_string(),
                            super_expr.span,
                        ));
                    }
                    let this_handle = self.allocator.strings.intern("this");
                    self.handle_variable(this_handle, super_expr.span, false)?;

                    for i in 0..arg_length {
                        let node_id = ctx.nodes.array.get_node_id_at(args.args, i);
                        if let Some(node_id) = node_id {
                            let expr = ctx.nodes.get_expr_node(node_id);
                            self.visit_expression(expr, ctx)?;
                        } else {
                            self.emit_opcode(OpCode::Nil, call.node.span); // Should be unreachable
                        }
                    }

                    let super_handle = self.allocator.strings.intern("super");
                    self.handle_variable(super_handle, super_expr.span, false)?;

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
                        let node_id = ctx.nodes.array.get_node_id_at(args.args, i);
                        if let Some(node_id) = node_id {
                            let expr = ctx.nodes.get_expr_node(node_id);
                            self.visit_expression(expr, ctx)?;
                        } else {
                            self.emit_opcode(OpCode::Nil, call.node.span); // Should be unreachable
                        }
                    }

                    self.emit_constant_opcode(
                        OpCode::Invoke,
                        OpCode::Invoke16,
                        Value::String(method_handle),
                        call.node.span,
                    )?;
                    self.emit_byte(arg_length as u8, call.node.span);

                    return Ok(());
                }

                self.visit_expression(callee, ctx)?;

                for i in 0..arg_length {
                    let node_id = ctx.nodes.array.get_node_id_at(args.args, i);
                    if let Some(node_id) = node_id {
                        let expr = ctx.nodes.get_expr_node(node_id);
                        self.visit_expression(expr, ctx)?;
                    } else {
                        self.emit_opcode(OpCode::Nil, call.node.span); // Should be unreachable
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
                    Value::String(identifier_handle),
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
                    Value::String(identifier_handle),
                    call.node.span,
                )?;
                Ok(())
            }
            CallOperationNode::Map(map_expr) => self.handle_map_expression(
                callee,
                TypedNodeRef::new(call_operation.id, map_expr),
                ctx,
            ),
            CallOperationNode::OptionalMap(map_expr) => {
                self.visit_expression(callee, ctx)?;
                let nil_jump = self.emit_jump(OpCode::JumpIfNil, map_expr.span);
                self.emit_opcode(OpCode::Pop, map_expr.span);
                self.handle_map_expression(
                    callee,
                    TypedNodeRef::new(
                        call_operation.id,
                        MapExprNode {
                            parameter: map_expr.parameter,
                            body: map_expr.body,
                            span: map_expr.span,
                        },
                    ),
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
                                Value::String(method_handle),
                                pipe.node.span,
                            )?;
                            self.emit_byte(1, pipe.node.span);
                        } else {
                            self.visit_expression(ctx.nodes.get_expr_node(call_expr.callee), ctx)?;

                            self.visit_expression(left, ctx)?;

                            let arg_length = ctx.nodes.array.size(arguments.args);

                            for i in 0..arg_length {
                                if let Some(node_id) =
                                    ctx.nodes.array.get_node_id_at(arguments.args, i)
                                {
                                    let expr = ctx.nodes.get_expr_node(node_id);
                                    self.visit_expression(expr, ctx)?;
                                } else {
                                    self.emit_opcode(OpCode::Nil, pipe.node.span); // should be unreachable.
                                }
                            }

                            let total_args = 1 + arg_length;
                            if total_args > u8::MAX as usize {
                                return Err(QangCompilerError::new_syntax_error(
                                    "Functions may only take up to 256 arguments.".to_string(),
                                    pipe.node.span,
                                ));
                            }

                            self.emit_opcode_and_byte(
                                OpCode::Call,
                                total_args as u8,
                                pipe.node.span,
                            );
                        }
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
                            Value::String(method_handle),
                            pipe.node.span,
                        )?;
                        self.emit_byte(1, pipe.node.span);
                    }
                    _ => {
                        return Err(QangCompilerError::new_syntax_error(
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
        self.declare_variable(name.node.name, name.node.span)?;
        let class_handle = name.node.name;
        // TODO this needs a Class16 variant as well
        let byte = self.make_constant(Value::String(class_handle), name.node.span)?;
        self.emit_opcode_and_byte(OpCode::Class, byte, name.node.span);
        self.define_variable(Some(class_handle), name.node.span)?;

        self.handle_variable(class_handle, name.node.span, false)?;

        if let Some(superclass) = superclass {
            self.handle_variable(class_handle, name.node.span, false)?;

            if class_handle == superclass.node.name {
                return Err(QangCompilerError::new_syntax_error(
                    "A class cannot inherit from itself.".to_string(),
                    superclass.node.span,
                ));
            }

            self.begin_scope();

            let super_handle = self.allocator.strings.intern("super");
            self.add_local(super_handle, class_decl.node.span)?;
            self.define_variable(Some(super_handle), name.node.span)?;

            self.handle_variable(superclass.node.name, superclass.node.span, false)?;

            self.handle_variable(super_handle, class_decl.node.span, true)?;

            self.emit_opcode(OpCode::Inherit, superclass.node.span);
        }

        let length = ctx.nodes.array.size(class_decl.node.members);

        for i in 0..length {
            if let Some(node_id) = ctx.nodes.array.get_node_id_at(class_decl.node.members, i) {
                let member = ctx.nodes.get_class_member_node(node_id);

                match member.node {
                    ClassMemberNode::Method(function) => {
                        let name = ctx.nodes.get_identifier_node(function.name);
                        let init_handle = self.allocator.strings.intern("init");
                        let compiler_kind = if name.node.name == init_handle {
                            AssemblerKind::Initializer
                        } else {
                            AssemblerKind::Method
                        };
                        let handle_identifier = name.node.name;

                        self.handle_function(
                            compiler_kind,
                            function,
                            class_decl.node.superclass.is_some(),
                            ctx,
                        )?;

                        self.emit_constant_opcode(
                            OpCode::Method,
                            OpCode::Method16,
                            Value::String(handle_identifier),
                            function.span,
                        )?;
                    }
                    ClassMemberNode::Field(field) => {
                        let name = ctx.nodes.get_identifier_node(field.name);

                        let initializer = field
                            .initializer
                            .map(|init| ctx.nodes.get_expr_node(init))
                            .unwrap_or_else(|| {
                                let node_id =
                                    ctx.nodes.create_node(AstNode::NilLiteral(NilLiteralNode {
                                        span: field.span,
                                    }));
                                let nil_node =
                                    ExprNode::Primary(PrimaryNode::Nil(NilLiteralNode {
                                        span: name.node.span,
                                    }));
                                TypedNodeRef::new(node_id, nil_node)
                            });

                        self.visit_expression(initializer, ctx)?;
                        let handle_identifier = name.node.name;
                        self.emit_constant_opcode(
                            OpCode::InitField,
                            OpCode::InitField16,
                            Value::String(handle_identifier),
                            name.node.span,
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
