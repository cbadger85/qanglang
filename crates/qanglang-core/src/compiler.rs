use crate::{
    ErrorReporter, FunctionHandle, QangSyntaxError, SourceMap, Value,
    ast::{self, AstVisitor, NilLiteral, SourceSpan},
    chunk::{Chunk, OpCode, SourceLocation},
    memory::{FunctionObject, HeapAllocator, StringHandle},
    parser::Parser,
    source::DEFALT_SOURCE_MAP,
    value::{
        ARRAY_TYPE_STRING, BOOLEAN_TYPE_STRING, CLASS_TYPE_STRING, FUNCTION_TYPE_STRING,
        NIL_TYPE_STRING, NUMBER_TYPE_STRING, OBJECT_TYPE_STRING, STRING_TYPE_STRING,
    },
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ErrorMessageFormat {
    Verbose,
    Compact,
    Minimal,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompilerError(Vec<QangSyntaxError>);

impl CompilerError {
    pub fn all(&self) -> &[QangSyntaxError] {
        &self.0
    }

    pub fn into_errors(self) -> Vec<QangSyntaxError> {
        self.0
    }
}

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
struct Compiler {
    kind: CompilerKind,
    function: FunctionObject,
    locals: Vec<Local>,
    local_count: usize,
    scope_depth: usize,
    upvalues: Vec<Upvalue>,
    enclosing: Option<Box<Compiler>>,
    has_superclass: bool,
    loop_contexts: Vec<LoopContext>,
}

impl Compiler {
    fn new(handle: StringHandle) -> Self {
        let mut locals = Vec::with_capacity(u8::MAX as usize);
        locals.push(Local::new("".into()));
        Self {
            kind: CompilerKind::Script,
            function: FunctionObject::new(handle, 0),
            locals,
            local_count: 1,
            scope_depth: 0,
            upvalues: Vec::with_capacity(u8::MAX as usize),
            enclosing: None,
            has_superclass: false,
            loop_contexts: Vec::new(),
        }
    }

    fn push(
        &mut self,
        handle: StringHandle,
        arity: usize,
        kind: CompilerKind,
        has_superclass: bool,
    ) -> &mut Self {
        let mut locals = Vec::with_capacity(u8::MAX as usize);
        if matches!(kind, CompilerKind::Method | CompilerKind::Initializer) {
            let mut this_local = Local::new("this".into());
            this_local.depth = Some(0); // Initialize 'this' as available immediately
            locals.push(this_local);
        } else {
            locals.push(Local::new("".into()));
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
        handle: &str,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangSyntaxError> {
        for i in (0..self.local_count).rev() {
            if let Some(local) = self.locals.get(i)
                && *local.name == *handle
            {
                if local.depth.is_none() {
                    return Err(QangSyntaxError::new(
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
        handle: &str,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangSyntaxError> {
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
    ) -> Result<usize, QangSyntaxError> {
        let upvalue_count = self.function.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = self.upvalues[i];
            if upvalue.index == index as u8 && upvalue.is_local == is_local {
                return Ok(i);
            }
        }

        if upvalue_count == u8::MAX as usize {
            return Err(QangSyntaxError::new(
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
        let mut parser = Parser::new(&self.source_map);
        let program = parser.parse();
        let errors = parser.into_reporter();

        match CompilerVisitor::new(self.allocator).compile(program, &self.source_map, errors) {
            Ok(program) => {
                self.allocator.strings.intern("NIL");
                self.allocator.strings.intern(NIL_TYPE_STRING);
                self.allocator.strings.intern("BOOLEAN");
                self.allocator.strings.intern(BOOLEAN_TYPE_STRING);
                self.allocator.strings.intern("NUMBER");
                self.allocator.strings.intern(NUMBER_TYPE_STRING);
                self.allocator.strings.intern("STRING");
                self.allocator.strings.intern(STRING_TYPE_STRING);
                self.allocator.strings.intern("FUNCTION");
                self.allocator.strings.intern(FUNCTION_TYPE_STRING);
                self.allocator.strings.intern("CLASS");
                self.allocator.strings.intern(CLASS_TYPE_STRING);
                self.allocator.strings.intern("OBJECT");
                self.allocator.strings.intern(OBJECT_TYPE_STRING);
                self.allocator.strings.intern("ARRAY");
                self.allocator.strings.intern(ARRAY_TYPE_STRING);
                Ok(QangProgram(self.allocator.allocate_function(program)))
            }
            Err(error) => Err(CompilerError(
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
    name: Box<str>,
    depth: Option<usize>,
    is_captured: bool,
}

impl Local {
    fn new(name: Box<str>) -> Self {
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
enum CompilerKind {
    Script,
    Function,
    Method,
    Initializer,
}

impl Default for CompilerKind {
    fn default() -> Self {
        Self::Script
    }
}

pub struct CompilerVisitor<'a> {
    source_map: &'a SourceMap,
    allocator: &'a mut HeapAllocator,
    compiler: Compiler,
}

impl<'a> CompilerVisitor<'a> {
    pub fn new(allocator: &'a mut HeapAllocator) -> Self {
        let handle = allocator.strings.intern("(script)");

        Self {
            source_map: &DEFALT_SOURCE_MAP,
            allocator,
            compiler: Compiler::new(handle),
        }
    }

    fn reset(&mut self) {
        let handle = self.allocator.strings.intern("(script)");
        self.source_map = &DEFALT_SOURCE_MAP;
        self.compiler = Compiler::new(handle);
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.compiler.function.chunk
    }

    pub fn compile(
        &mut self,
        program: ast::Program,
        source_map: &'a SourceMap,
        mut errors: ErrorReporter,
    ) -> Result<FunctionObject, CompilerError> {
        self.source_map = source_map;
        let handle = self.allocator.strings.intern(&source_map.name);
        self.compiler = Compiler::new(handle);

        self.visit_program(&program, &mut errors)
            .map_err(|err| CompilerError(vec![err]))?;

        self.emit_opcode(OpCode::Nil, SourceSpan::default());
        self.emit_opcode(OpCode::Return, SourceSpan::default());

        if errors.has_errors() {
            Err(CompilerError(errors.take_errors()))
        } else {
            let artifacts = std::mem::take(&mut self.compiler);
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

    fn patch_jump(&mut self, offset: usize, span: SourceSpan) -> Result<(), QangSyntaxError> {
        let jump = self.current_chunk_mut().code.len() - offset - 2;

        if jump > u16::MAX as usize {
            return Err(QangSyntaxError::new(
                "Too much code to jump over.".to_string(),
                span,
            ));
        }

        self.current_chunk_mut().code[offset] = ((jump >> 8) & 0xff) as u8;
        self.current_chunk_mut().code[offset + 1] = (jump & 0xff) as u8;

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize, span: SourceSpan) -> Result<(), QangSyntaxError> {
        self.emit_opcode(OpCode::Loop, span);
        let offset = self.current_chunk_mut().code.len() - loop_start + 2;
        if offset > u16::MAX as usize {
            return Err(QangSyntaxError::new(
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

    fn emit_constant(&mut self, value: Value, span: SourceSpan) -> Result<(), QangSyntaxError> {
        let index = self.current_chunk_mut().add_constant(value);

        if index <= u8::MAX as usize {
            self.emit_opcode_and_byte(OpCode::Constant, index as u8, span);
        } else if index <= u16::MAX as usize {
            self.emit_opcode(OpCode::Constant16, span);
            self.emit_byte((index >> 8) as u8, span);
            self.emit_byte((index & 0xff) as u8, span);
        } else {
            return Err(QangSyntaxError::new(
                "Too many constants in function".to_string(),
                span,
            ));
        }

        Ok(())
    }

    fn make_constant(&mut self, value: Value, span: SourceSpan) -> Result<u8, QangSyntaxError> {
        let index = self.current_chunk_mut().add_constant(value);

        if index > u8::MAX as usize {
            Err(QangSyntaxError::new(
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
    ) -> Result<(), QangSyntaxError> {
        let index = self.current_chunk_mut().add_constant(value);

        if index <= u8::MAX as usize {
            self.emit_opcode_and_byte(opcode_8, index as u8, span);
        } else if index <= u16::MAX as usize {
            self.emit_opcode(opcode_16, span);
            self.emit_byte((index >> 8) as u8, span);
            self.emit_byte((index & 0xff) as u8, span);
        } else {
            return Err(QangSyntaxError::new(
                "Too many constants in function (max 65536)".to_string(),
                span,
            ));
        }

        Ok(())
    }

    fn emit_return(&mut self, span: SourceSpan) {
        if matches!(self.compiler.kind, CompilerKind::Initializer) {
            self.emit_opcode_and_byte(OpCode::GetLocal, 0, span);
        } else {
            self.emit_opcode(OpCode::Nil, span);
        }
        self.emit_opcode(OpCode::Return, span);
    }

    fn is_tail_call(&self, expr: &ast::Expr) -> bool {
        matches!(expr, ast::Expr::Call(_))
    }

    fn emit_tail_call(&mut self, arg_count: u8, span: SourceSpan) {
        self.emit_opcode_and_byte(OpCode::TailCall, arg_count, span);
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self, span: SourceSpan) {
        let current = &mut self.compiler;
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

    fn add_local(&mut self, handle: &str, span: SourceSpan) -> Result<(), QangSyntaxError> {
        let current = &mut self.compiler;
        if current.local_count >= STACK_MAX {
            Err(QangSyntaxError::new(
                "Too many local variables in scope.".to_string(),
                span,
            ))
        } else {
            let local = Local::new(handle.into());
            if current.local_count < current.locals.len() {
                current.locals[current.local_count] = local;
            } else {
                current.locals.push(local);
            }
            current.local_count += 1;
            Ok(())
        }
    }

    fn declare_variable(&mut self, handle: &str, span: SourceSpan) -> Result<(), QangSyntaxError> {
        let current = &self.compiler;
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

                if *local.name == *handle {
                    return Err(QangSyntaxError::new(
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
    ) -> Result<(), QangSyntaxError> {
        if self.compiler.scope_depth > 0 {
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
        let current = &mut self.compiler;
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
        identifer: &str,
        span: SourceSpan,
    ) -> Result<Option<StringHandle>, QangSyntaxError> {
        self.declare_variable(identifer, span)?;

        if self.compiler.scope_depth > 0 {
            Ok(None)
        } else {
            Ok(Some(self.allocator.strings.intern(identifer)))
        }
    }

    fn handle_variable(
        &mut self,
        handle: &str,
        span: SourceSpan,
        is_assignment: bool,
    ) -> Result<(), QangSyntaxError> {
        let (index, get_op, set_op) = {
            if let Some(index) = self.resolve_local_variable(handle, span)? {
                (index as u8, OpCode::GetLocal, OpCode::SetLocal)
            } else if let Some(index) = self.resolve_upvalue(handle, span)? {
                (index as u8, OpCode::GetUpvalue, OpCode::SetUpvalue)
            } else {
                let string_handle = self.allocator.strings.intern(handle);
                let index = self
                    .current_chunk_mut()
                    .add_constant(Value::String(string_handle));
                if index <= u8::MAX as usize {
                    (index as u8, OpCode::GetGlobal, OpCode::SetGlobal)
                } else {
                    // For 16-bit indices, we'll handle this differently
                    return self.emit_global_variable_access(string_handle, is_assignment, span);
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
    ) -> Result<(), QangSyntaxError> {
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
        name: &str,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangSyntaxError> {
        self.compiler.resolve_upvalue(name, span)
    }

    fn resolve_local_variable(
        &self,
        handle: &str,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangSyntaxError> {
        self.compiler.resolve_local_variable(handle, span)
    }

    fn handle_function(
        &mut self,
        kind: CompilerKind,
        func_expr: &ast::FunctionExpr,
        has_superclass: bool,
        errors: &mut ErrorReporter,
    ) -> Result<(), QangSyntaxError> {
        let function_identifier_handle = if matches!(kind, CompilerKind::Function) {
            self.parse_variable(&func_expr.name.name, func_expr.span)?
        } else {
            None
        };

        let function_name_handle = function_identifier_handle
            .unwrap_or_else(|| self.allocator.strings.intern(&func_expr.name.name));

        self.compiler.push(
            function_name_handle,
            func_expr.parameters.len(),
            kind,
            has_superclass,
        );
        self.begin_scope();

        let parameter_span = SourceSpan::combine(
            func_expr
                .parameters
                .first()
                .map(|p| p.span())
                .unwrap_or(func_expr.span),
            func_expr
                .parameters
                .last()
                .map(|p| p.span())
                .unwrap_or(func_expr.span),
        );

        if func_expr.parameters.len() > 255 {
            return Err(QangSyntaxError::new(
                "Cannot have more than 255 parameters.".to_string(),
                parameter_span,
            ));
        }

        for parameter in &func_expr.parameters {
            match parameter {
                ast::Parameter::Identifier(identifier) => {
                    let handle = self.parse_variable(&identifier.name, identifier.span)?;
                    self.define_variable(handle, identifier.span)?;
                }
                ast::Parameter::Destructure(_pattern) => {
                    // TODO: Implement destructuring parameters
                    return Err(QangSyntaxError::new(
                        "Destructuring parameters not yet implemented.".to_string(),
                        parameter.span(),
                    ));
                }
            }
        }

        self.visit_block_statement(&func_expr.body, errors)?;
        self.emit_return(func_expr.span);

        let compiler = self
            .compiler
            .pop()
            .expect("Unexpected end of artifact stack.");
        let function = compiler.function;
        let upvalue_count = function.upvalue_count;

        let function_handle = self.allocator.allocate_function(function);
        let constant_index = self
            .current_chunk_mut()
            .add_constant(Value::FunctionDecl(function_handle));
        if constant_index <= u8::MAX as usize {
            self.emit_opcode_and_byte(OpCode::Closure, constant_index as u8, func_expr.name.span);
        } else {
            return Err(QangSyntaxError::new(
                "Function constant index too large".to_string(),
                func_expr.name.span,
            ));
        }

        for i in 0..upvalue_count {
            let upvalue = compiler.upvalues[i];
            let is_local_byte = if upvalue.is_local { 1 } else { 0 };

            self.emit_byte(is_local_byte, func_expr.name.span);
            self.emit_byte(upvalue.index, func_expr.name.span);
        }

        if matches!(kind, CompilerKind::Function) {
            self.define_variable(function_identifier_handle, func_expr.name.span)?;
        }
        Ok(())
    }

    fn emit_compound_assignment_op(
        &mut self,
        operator: ast::AssignmentOperator,
        span: SourceSpan,
    ) -> Result<(), QangSyntaxError> {
        let opcode = match operator {
            ast::AssignmentOperator::Assign => unreachable!(),
            ast::AssignmentOperator::AddAssign => OpCode::Add,
            ast::AssignmentOperator::SubtractAssign => OpCode::Subtract,
            ast::AssignmentOperator::MultiplyAssign => OpCode::Multiply,
            ast::AssignmentOperator::DivideAssign => OpCode::Divide,
            ast::AssignmentOperator::ModuloAssign => OpCode::Modulo,
        };

        self.emit_opcode(opcode, span);
        Ok(())
    }

    fn handle_map_expression(
        &mut self,
        callee: &ast::Expr,
        map_expr: &ast::MapExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), QangSyntaxError> {
        let lambda_expr = ast::LambdaExpr {
            parameters: vec![map_expr.parameter.clone()],
            body: Box::new(ast::LambdaBody::Expr(map_expr.body.clone())),
            span: map_expr.span,
        };
        self.visit_lambda_expression(&lambda_expr, errors)?;
        self.visit_expression(callee, errors)?;
        self.emit_opcode_and_byte(OpCode::Call, 1, map_expr.span);
        Ok(())
    }
}

impl<'a> AstVisitor for CompilerVisitor<'a> {
    type Error = QangSyntaxError;

    fn visit_return_statement(
        &mut self,
        return_stmt: &ast::ReturnStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if let Some(expr) = &return_stmt.value {
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

            self.visit_expression(expr, errors)?;
        } else if matches!(self.compiler.kind, CompilerKind::Initializer) {
            self.emit_opcode_and_byte(OpCode::GetLocal, 0, return_stmt.span);
        } else {
            self.emit_opcode(OpCode::Nil, return_stmt.span);
        }

        self.emit_opcode(OpCode::Return, return_stmt.span);
        Ok(())
    }

    fn visit_number_literal(
        &mut self,
        number: &ast::NumberLiteral,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.emit_constant(number.value.into(), number.span)
    }

    fn visit_string_literal(
        &mut self,
        string: &ast::StringLiteral,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let handle = self.allocator.strings.intern(&string.value);
        self.emit_constant(Value::String(handle), string.span)
    }

    fn visit_boolean_literal(
        &mut self,
        boolean: &ast::BooleanLiteral,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.emit_opcode(boolean.value.into(), boolean.span);
        Ok(())
    }

    fn visit_nil_literal(
        &mut self,
        nil: &ast::NilLiteral,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.emit_opcode(OpCode::Nil, nil.span);
        Ok(())
    }

    fn visit_this_expression(
        &mut self,
        this_expr: &ast::ThisExpr,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if matches!(
            self.compiler.kind,
            CompilerKind::Method | CompilerKind::Initializer
        ) {
            self.emit_opcode_and_byte(OpCode::GetLocal, 0, this_expr.span);
            Ok(())
        } else {
            Err(QangSyntaxError::new(
                "Cannot use 'this' outside of a class method.".to_string(),
                this_expr.span,
            ))
        }
    }

    fn visit_super_expression(
        &mut self,
        super_expr: &ast::SuperExpr,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if !self.compiler.has_superclass {
            return Err(QangSyntaxError::new(
                "Can't use 'super' in a class with no superclass.".to_string(),
                super_expr.span,
            ));
        }

        if !matches!(
            self.compiler.kind,
            CompilerKind::Method | CompilerKind::Initializer
        ) {
            return Err(QangSyntaxError::new(
                "Can't use 'super' outside of a class method.".to_string(),
                super_expr.span,
            ));
        }

        self.handle_variable("this", super_expr.span, false)?;
        self.handle_variable("super", super_expr.span, false)?;

        let method_handle = self.allocator.strings.intern(&super_expr.method.name);
        self.emit_constant_opcode(
            OpCode::GetSuper,
            OpCode::GetSuper16,
            Value::String(method_handle),
            super_expr.span,
        )?;

        Ok(())
    }

    fn visit_comparison_expression(
        &mut self,
        comparison: &ast::ComparisonExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&comparison.left, errors)?;
        self.visit_expression(&comparison.right, errors)?;
        self.emit_opcode(comparison.operator.into(), comparison.span);
        Ok(())
    }

    fn visit_equality_expression(
        &mut self,
        equality: &ast::EqualityExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&equality.left, errors)?;
        self.visit_expression(&equality.right, errors)?;
        self.emit_opcode(equality.operator.into(), equality.span);

        if let ast::EqualityOperator::NotEqual = equality.operator {
            self.emit_opcode(OpCode::Not, equality.span);
        }
        Ok(())
    }

    fn visit_term_expression(
        &mut self,
        term: &ast::TermExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&term.left, errors)?;
        self.visit_expression(&term.right, errors)?;
        self.emit_opcode(term.operator.into(), term.span);
        Ok(())
    }

    fn visit_factor_expression(
        &mut self,
        factor: &ast::FactorExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&factor.left, errors)?;
        self.visit_expression(&factor.right, errors)?;
        self.emit_opcode(factor.operator.into(), factor.span);
        Ok(())
    }

    fn visit_unary_expression(
        &mut self,
        unary: &ast::UnaryExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&unary.operand, errors)?;
        self.emit_opcode(unary.operator.into(), unary.span);
        Ok(())
    }

    fn visit_assignment_expression(
        &mut self,
        assignment: &ast::AssignmentExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match assignment.operator {
            ast::AssignmentOperator::Assign => match &assignment.target {
                ast::AssignmentTarget::Identifier(identifier) => {
                    self.visit_expression(&assignment.value, errors)?;
                    self.handle_variable(&identifier.name, identifier.span, true)?;
                }
                ast::AssignmentTarget::Property(property) => {
                    self.visit_expression(&property.object, errors)?;
                    let identifier_handle = self.allocator.strings.intern(&property.property.name);
                    self.visit_expression(&assignment.value, errors)?;
                    self.emit_constant_opcode(
                        OpCode::SetProperty,
                        OpCode::SetProperty16,
                        Value::String(identifier_handle),
                        property.span,
                    )?;
                }
                ast::AssignmentTarget::Index(index) => {
                    self.visit_expression(&index.object, errors)?;
                    self.visit_expression(&index.index, errors)?;
                    self.visit_expression(&assignment.value, errors)?;
                    self.emit_opcode(OpCode::SetArrayIndex, index.span);
                }
            },
            _ => match &assignment.target {
                ast::AssignmentTarget::Identifier(identifier) => {
                    self.handle_variable(&identifier.name, identifier.span, false)?;
                    self.visit_expression(&assignment.value, errors)?;
                    self.emit_compound_assignment_op(assignment.operator, assignment.span)?;
                    self.handle_variable(&identifier.name, identifier.span, true)?;
                }
                ast::AssignmentTarget::Property(property) => {
                    self.visit_expression(&property.object, errors)?;
                    self.visit_expression(&property.object, errors)?;
                    let identifier_handle = self.allocator.strings.intern(&property.property.name);
                    self.emit_constant_opcode(
                        OpCode::GetProperty,
                        OpCode::GetProperty16,
                        Value::String(identifier_handle),
                        property.span,
                    )?;

                    self.visit_expression(&assignment.value, errors)?;
                    self.emit_compound_assignment_op(assignment.operator, assignment.span)?;

                    let identifier_handle = self.allocator.strings.intern(&property.property.name);
                    self.emit_constant_opcode(
                        OpCode::SetProperty,
                        OpCode::SetProperty16,
                        Value::String(identifier_handle),
                        property.span,
                    )?;
                }
                ast::AssignmentTarget::Index(index) => {
                    self.visit_expression(&index.object, errors)?;
                    self.visit_expression(&index.index, errors)?;

                    self.visit_expression(&index.object, errors)?;
                    self.visit_expression(&index.index, errors)?;
                    self.emit_opcode(OpCode::GetArrayIndex, index.span);

                    self.visit_expression(&assignment.value, errors)?;
                    self.emit_compound_assignment_op(assignment.operator, assignment.span)?;

                    self.emit_opcode(OpCode::SetArrayIndex, index.span);
                }
            },
        }

        Ok(())
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &ast::ExprStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&expr_stmt.expr, errors)?;
        self.emit_opcode(OpCode::Pop, expr_stmt.span);
        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        var_decl: &ast::VariableDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match &var_decl.target {
            ast::VariableTarget::Identifier(identifier) => {
                let global = self.parse_variable(&identifier.name, identifier.span)?;

                if let Some(expr) = &var_decl.initializer {
                    self.visit_expression(expr, errors)?;
                } else {
                    self.emit_opcode(OpCode::Nil, var_decl.span);
                }

                self.define_variable(global, identifier.span)?;
                if global.is_none() {
                    self.handle_variable(&identifier.name, var_decl.span, true)?;
                }
            }
            ast::VariableTarget::Destructure(_pattern) => {
                // TODO: Implement destructuring assignment
                return Err(QangSyntaxError::new(
                    "Destructuring assignment not yet implemented.".to_string(),
                    var_decl.span,
                ));
            }
        }

        Ok(())
    }

    fn visit_identifier(
        &mut self,
        identifier: &ast::Identifier,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.handle_variable(&identifier.name, identifier.span, false)?;

        Ok(())
    }

    fn visit_block_statement(
        &mut self,
        block_stmt: &ast::BlockStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.begin_scope();
        for decl in &block_stmt.decls {
            self.visit_declaration(decl, errors)?;
        }
        self.end_scope(block_stmt.span);
        Ok(())
    }

    fn visit_declaration(
        &mut self,
        decl: &ast::Decl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let result = match decl {
            ast::Decl::Class(class_decl) => self.visit_class_declaration(class_decl, errors),
            ast::Decl::Function(func_decl) => self.visit_function_declaration(func_decl, errors),
            ast::Decl::Lambda(lambda_decl) => self.visit_lambda_declaration(lambda_decl, errors),
            ast::Decl::Variable(var_decl) => self.visit_variable_declaration(var_decl, errors),
            ast::Decl::Stmt(stmt) => self.visit_statement(stmt, errors),
        };
        if let Err(error) = result {
            errors.report_error(error);
        }
        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        if_stmt: &ast::IfStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&if_stmt.condition, errors)?;

        let then_jump = self.emit_jump(OpCode::JumpIfFalse, if_stmt.then_branch.span());
        self.emit_opcode(OpCode::Pop, if_stmt.then_branch.span());
        self.visit_statement(&if_stmt.then_branch, errors)?;

        let else_jump = self.emit_jump(OpCode::Jump, if_stmt.span);
        self.patch_jump(then_jump, if_stmt.then_branch.span())?;
        self.emit_opcode(OpCode::Pop, if_stmt.span);

        if let Some(else_branch) = if_stmt.else_branch.as_ref() {
            self.visit_statement(else_branch, errors)?;
        }
        self.patch_jump(else_jump, if_stmt.span)?;

        Ok(())
    }

    fn visit_ternary_expression(
        &mut self,
        ternary: &ast::TernaryExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&ternary.condition, errors)?;

        let then_jump = self.emit_jump(OpCode::JumpIfFalse, ternary.span);
        self.emit_opcode(OpCode::Pop, ternary.span);

        if let Some(then_expr) = &ternary.then_expr {
            self.visit_expression(then_expr, errors)?;
        } else {
            self.emit_opcode(OpCode::Nil, ternary.span);
        }

        let else_jump = self.emit_jump(OpCode::Jump, ternary.span);
        self.patch_jump(then_jump, ternary.span)?;
        self.emit_opcode(OpCode::Pop, ternary.span);

        if let Some(else_expr) = &ternary.else_expr {
            self.visit_expression(else_expr, errors)?;
        } else {
            self.emit_opcode(OpCode::Nil, ternary.span);
        }

        self.patch_jump(else_jump, ternary.span)?;

        Ok(())
    }

    fn visit_logical_or_expression(
        &mut self,
        logical_or: &ast::LogicalOrExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&logical_or.left, errors)?;

        let else_jump = self.emit_jump(OpCode::JumpIfFalse, logical_or.span);
        let end_jump = self.emit_jump(OpCode::Jump, logical_or.span);

        self.patch_jump(else_jump, logical_or.span)?;
        self.emit_opcode(OpCode::Pop, logical_or.span);

        self.visit_expression(&logical_or.right, errors)?;
        self.patch_jump(end_jump, logical_or.span)?;

        Ok(())
    }

    fn visit_logical_and_expression(
        &mut self,
        logical_and: &ast::LogicalAndExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&logical_and.left, errors)?;

        let end_jump = self.emit_jump(OpCode::JumpIfFalse, logical_and.span);
        self.emit_opcode(OpCode::Pop, logical_and.span);

        self.visit_expression(&logical_and.right, errors)?;
        self.patch_jump(end_jump, logical_and.span)?;

        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: &ast::WhileStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.compiler.loop_contexts.push(LoopContext::default());

        let loop_start = self.current_chunk_mut().code.len();
        self.visit_expression(&while_stmt.condition, errors)?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse, while_stmt.body.span());
        self.emit_opcode(OpCode::Pop, while_stmt.body.span());
        self.visit_statement(&while_stmt.body, errors)?;

        let loop_context = self
            .compiler
            .loop_contexts
            .pop()
            .expect("Loop context should exist");

        for continue_position in loop_context.continue_jumps {
            let jump_distance = continue_position - loop_start + 3;

            if jump_distance > u16::MAX as usize {
                return Err(QangSyntaxError::new(
                    "Continue jump too large.".to_string(),
                    while_stmt.span,
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

        self.emit_loop(loop_start, while_stmt.body.span())?;

        self.patch_jump(exit_jump, while_stmt.body.span())?;
        self.emit_opcode(OpCode::Pop, while_stmt.span);

        for break_jump in loop_context.break_jumps {
            self.patch_jump(break_jump, while_stmt.span)?;
        }

        Ok(())
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: &ast::ForStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

        self.compiler.loop_contexts.push(LoopContext::default());

        if let Some(initializer) = &for_stmt.initializer {
            match initializer {
                ast::ForInitializer::Variable(var_decl) => {
                    self.visit_variable_declaration(var_decl, errors)?
                }
                ast::ForInitializer::Expr(expr) => {
                    self.visit_expression(expr, errors)?;
                    self.emit_opcode(OpCode::Pop, expr.span());
                }
            }
        }

        let mut loop_start = self.current_chunk_mut().code.len();
        let mut exit_jump: Option<usize> = None;
        let increment_start: Option<usize>;

        if let Some(condition) = &for_stmt.condition {
            let condition_jump = self.emit_jump(OpCode::Jump, condition.span());
            loop_start = self.current_chunk_mut().code.len();
            self.visit_statement(&for_stmt.body, errors)?;

            increment_start = Some(self.current_chunk_mut().code.len());
            if let Some(increment) = &for_stmt.increment {
                self.visit_expression(increment, errors)?;
                self.emit_opcode(OpCode::Pop, increment.span());
            }

            self.patch_jump(condition_jump, condition.span())?;
            self.visit_expression(condition, errors)?;
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse, condition.span()));
            self.emit_opcode(OpCode::Pop, condition.span());
            self.emit_loop(loop_start, for_stmt.body.span())?;
        } else {
            self.visit_statement(&for_stmt.body, errors)?;

            increment_start = Some(self.current_chunk_mut().code.len());
            if let Some(increment) = &for_stmt.increment {
                self.visit_expression(increment, errors)?;
                self.emit_opcode(OpCode::Pop, increment.span());
            }

            self.emit_loop(loop_start, for_stmt.body.span())?;
        }

        let loop_context = self
            .compiler
            .loop_contexts
            .pop()
            .expect("Loop context should exist");

        let continue_target = increment_start.unwrap_or(loop_start);
        for continue_jump in loop_context.continue_jumps {
            if continue_target > continue_jump {
                let jump_distance = continue_target - continue_jump - 3;
                if jump_distance > u16::MAX as usize {
                    return Err(QangSyntaxError::new(
                        "Continue jump too large.".to_string(),
                        for_stmt.span,
                    ));
                }
                let chunk = self.current_chunk_mut();
                chunk.code[continue_jump + 1] = ((jump_distance >> 8) & 0xff) as u8;
                chunk.code[continue_jump + 2] = (jump_distance & 0xff) as u8;
            } else {
                let jump_distance = continue_jump - continue_target + 2;
                if jump_distance > u16::MAX as usize {
                    return Err(QangSyntaxError::new(
                        "Continue jump too large.".to_string(),
                        for_stmt.span,
                    ));
                }
                let chunk = self.current_chunk_mut();
                chunk.code[continue_jump] = OpCode::Loop as u8;
                chunk.code[continue_jump + 1] = ((jump_distance >> 8) & 0xff) as u8;
                chunk.code[continue_jump + 2] = (jump_distance & 0xff) as u8;
            }
        }

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump, for_stmt.span)?;
            self.emit_opcode(OpCode::Pop, for_stmt.span);
        }

        for break_jump in loop_context.break_jumps {
            self.patch_jump(break_jump, for_stmt.span)?;
        }

        self.end_scope(for_stmt.span);

        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: &ast::FunctionDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.handle_function(CompilerKind::Function, &func_decl.function, false, errors)
    }

    fn visit_break_statement(
        &mut self,
        break_stmt: &ast::BreakStmt,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if !self.compiler.loop_contexts.is_empty() {
            let jump = self.emit_jump(OpCode::Jump, break_stmt.span);
            if let Some(loop_context) = self.compiler.loop_contexts.last_mut() {
                loop_context.break_jumps.push(jump);
            }
            Ok(())
        } else {
            Err(QangSyntaxError::new(
                "'break' can only be used inside loops.".to_string(),
                break_stmt.span,
            ))
        }
    }

    fn visit_continue_statement(
        &mut self,
        continue_stmt: &ast::ContinueStmt,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if !self.compiler.loop_contexts.is_empty() {
            let continue_position = self.current_chunk_mut().code.len();
            if let Some(loop_context) = self.compiler.loop_contexts.last_mut() {
                loop_context.continue_jumps.push(continue_position);
            }
            Ok(())
        } else {
            Err(QangSyntaxError::new(
                "'continue' can only be used inside loops.".to_string(),
                continue_stmt.span,
            ))
        }
    }

    fn visit_lambda_declaration(
        &mut self,
        lamdba_decl: &ast::LambdaDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let lambda_identifier_handle =
            self.parse_variable(&lamdba_decl.name.name, lamdba_decl.span)?;

        self.visit_lambda_expression(&lamdba_decl.lambda, errors)?;

        self.define_variable(lambda_identifier_handle, lamdba_decl.name.span)?;

        Ok(())
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: &ast::LambdaExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let lambda_name_handle = self.allocator.strings.intern("<anonymous>");

        self.compiler.push(
            lambda_name_handle,
            lambda_expr.parameters.len(),
            CompilerKind::Function,
            false,
        );
        self.begin_scope();

        let parameter_span = SourceSpan::combine(
            lambda_expr
                .parameters
                .first()
                .map(|p| p.span())
                .unwrap_or(lambda_expr.span),
            lambda_expr
                .parameters
                .last()
                .map(|p| p.span())
                .unwrap_or(lambda_expr.span),
        );

        if lambda_expr.parameters.len() > 255 {
            return Err(QangSyntaxError::new(
                "Cannot have more than 255 parameters.".to_string(),
                parameter_span,
            ));
        }

        for parameter in &lambda_expr.parameters {
            match parameter {
                ast::Parameter::Identifier(identifier) => {
                    let handle = self.parse_variable(&identifier.name, identifier.span)?;
                    self.define_variable(handle, identifier.span)?;
                }
                ast::Parameter::Destructure(_pattern) => {
                    // TODO: Implement destructuring parameters
                    return Err(QangSyntaxError::new(
                        "Destructuring parameters not yet implemented.".to_string(),
                        parameter.span(),
                    ));
                }
            }
        }

        match lambda_expr.body.as_ref() {
            ast::LambdaBody::Block(body) => {
                self.visit_block_statement(body, errors)?;
                self.emit_return(lambda_expr.span);
            }
            ast::LambdaBody::Expr(expr) => {
                self.visit_return_statement(
                    &ast::ReturnStmt {
                        value: Some(*expr.clone()),
                        span: expr.span(),
                    },
                    errors,
                )?;
            }
        }

        let compiler = self
            .compiler
            .pop()
            .expect("Unexpected end of artifact stack.");
        let function = compiler.function;
        let upvalue_count = function.upvalue_count;

        let function_handle = self.allocator.allocate_function(function);
        let constant_index = self
            .current_chunk_mut()
            .add_constant(Value::FunctionDecl(function_handle));
        if constant_index <= u8::MAX as usize {
            self.emit_opcode_and_byte(OpCode::Closure, constant_index as u8, lambda_expr.span);
        } else {
            return Err(QangSyntaxError::new(
                "Function constant index too large".to_string(),
                lambda_expr.span,
            ));
        }

        for i in 0..upvalue_count {
            let upvalue = compiler.upvalues[i];
            let is_local_byte = if upvalue.is_local { 1 } else { 0 };

            self.emit_byte(is_local_byte, lambda_expr.span);
            self.emit_byte(upvalue.index, lambda_expr.span);
        }

        Ok(())
    }

    fn visit_call_expression(
        &mut self,
        call: &ast::CallExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match call.operation.as_ref() {
            ast::CallOperation::Call(args) => {
                if args.len() > u8::MAX as usize {
                    return Err(QangSyntaxError::new(
                        "Functions may only take up to 256 arguments.".to_string(),
                        call.span,
                    ));
                }

                if let ast::Expr::Primary(ast::PrimaryExpr::Super(super_expr)) =
                    call.callee.as_ref()
                {
                    if !self.compiler.has_superclass {
                        return Err(QangSyntaxError::new(
                            "Can't use 'super' in a class with no superclass.".to_string(),
                            super_expr.span,
                        ));
                    }

                    if !matches!(
                        self.compiler.kind,
                        CompilerKind::Method | CompilerKind::Initializer
                    ) {
                        return Err(QangSyntaxError::new(
                            "Can't use 'super' outside of a class method.".to_string(),
                            super_expr.span,
                        ));
                    }

                    self.handle_variable("this", super_expr.span, false)?;

                    for arg in args {
                        self.visit_expression(arg, errors)?;
                    }

                    self.handle_variable("super", super_expr.span, false)?;

                    let method_handle = self.allocator.strings.intern(&super_expr.method.name);
                    self.emit_constant_opcode(
                        OpCode::SuperInvoke,
                        OpCode::SuperInvoke16,
                        Value::String(method_handle),
                        call.span,
                    )?;
                    self.emit_byte(args.len() as u8, call.span);

                    return Ok(());
                }

                if let ast::Expr::Call(property_call) = call.callee.as_ref()
                    && let ast::CallOperation::Property(method_name) =
                        property_call.operation.as_ref()
                {
                    self.visit_expression(&property_call.callee, errors)?;

                    let method_handle = self.allocator.strings.intern(&method_name.name);

                    for arg in args {
                        self.visit_expression(arg, errors)?;
                    }

                    self.emit_constant_opcode(
                        OpCode::Invoke,
                        OpCode::Invoke16,
                        Value::String(method_handle),
                        call.span,
                    )?;
                    self.emit_byte(args.len() as u8, call.span);

                    return Ok(());
                }

                self.visit_expression(call.callee.as_ref(), errors)?;

                for arg in args {
                    self.visit_expression(arg, errors)?;
                }

                self.emit_opcode_and_byte(OpCode::Call, args.len() as u8, call.span);

                Ok(())
            }
            ast::CallOperation::Property(identifier) => {
                self.visit_expression(call.callee.as_ref(), errors)?;
                let identifier_handle = self.allocator.strings.intern(&identifier.name);
                self.emit_constant_opcode(
                    OpCode::GetProperty,
                    OpCode::GetProperty16,
                    Value::String(identifier_handle),
                    call.span,
                )?;
                Ok(())
            }
            ast::CallOperation::Index(expr) => {
                self.visit_expression(call.callee.as_ref(), errors)?;
                self.visit_expression(expr, errors)?;

                self.emit_opcode(OpCode::GetArrayIndex, call.span);
                Ok(())
            }
            ast::CallOperation::OptionalProperty(identifier) => {
                self.visit_expression(call.callee.as_ref(), errors)?;
                let identifier_handle = self.allocator.strings.intern(&identifier.name);
                self.emit_constant_opcode(
                    OpCode::GetOptionalProperty,
                    OpCode::GetOptionalProperty16,
                    Value::String(identifier_handle),
                    call.span,
                )?;
                Ok(())
            }
            ast::CallOperation::Map(map_expr) => {
                self.handle_map_expression(call.callee.as_ref(), map_expr, errors)
            }
            ast::CallOperation::OptionalMap(map_expr) => {
                self.visit_expression(&call.callee, errors)?;
                let nil_jump = self.emit_jump(OpCode::JumpIfNil, map_expr.span);
                self.emit_opcode(OpCode::Pop, map_expr.span);
                self.handle_map_expression(call.callee.as_ref(), &map_expr.clone().into(), errors)?;
                self.patch_jump(nil_jump, map_expr.span)?;
                Ok(())
            }
        }
    }

    fn visit_pipe_expression(
        &mut self,
        pipe: &ast::PipeExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if let Some(right) = &pipe.right {
            match right.as_ref() {
                ast::Expr::Call(call_expr) => match call_expr.operation.as_ref() {
                    ast::CallOperation::Call(arguments) => {
                        if let ast::Expr::Call(inner_call) = call_expr.callee.as_ref()
                            && let ast::CallOperation::Property(method_name) =
                                inner_call.operation.as_ref()
                            && method_name.name == "call".into()
                        {
                            self.visit_expression(&call_expr.callee, errors)?;
                            self.visit_expression(&pipe.left, errors)?;
                            let method_handle = self.allocator.strings.intern(&method_name.name);
                            self.emit_constant_opcode(
                                OpCode::Invoke,
                                OpCode::Invoke16,
                                Value::String(method_handle),
                                pipe.span,
                            )?;
                            self.emit_byte(1, pipe.span);
                        } else {
                            self.visit_expression(&call_expr.callee, errors)?;

                            self.visit_expression(&pipe.left, errors)?;

                            for arg in arguments {
                                self.visit_expression(arg, errors)?;
                            }

                            let total_args = 1 + arguments.len();
                            if total_args > u8::MAX as usize {
                                return Err(QangSyntaxError::new(
                                    "Functions may only take up to 256 arguments.".to_string(),
                                    pipe.span,
                                ));
                            }

                            self.emit_opcode_and_byte(OpCode::Call, total_args as u8, pipe.span);
                        }
                    }
                    ast::CallOperation::Property(method_name) => {
                        self.visit_expression(&call_expr.callee, errors)?;
                        self.visit_expression(&pipe.left, errors)?;
                        let method_handle = self.allocator.strings.intern(&method_name.name);
                        self.emit_constant_opcode(
                            OpCode::Invoke,
                            OpCode::Invoke16,
                            Value::String(method_handle),
                            pipe.span,
                        )?;
                        self.emit_byte(1, pipe.span);
                    }
                    _ => {
                        return Err(QangSyntaxError::new(
                            "Pipe expression with non-call operation not supported.".to_string(),
                            call_expr.span,
                        ));
                    }
                },

                _ => {
                    self.visit_expression(right, errors)?;

                    self.visit_expression(&pipe.left, errors)?;

                    self.emit_opcode_and_byte(OpCode::Call, 1, pipe.span);
                }
            }
        } else {
            return Err(QangSyntaxError::new(
                "Pipe expression missing right operand.".to_string(),
                pipe.span,
            ));
        }

        Ok(())
    }

    fn visit_class_declaration(
        &mut self,
        class_decl: &ast::ClassDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.declare_variable(&class_decl.name.name, class_decl.name.span)?;
        let class_handle = self.allocator.strings.intern(&class_decl.name.name);
        let byte = self.make_constant(Value::String(class_handle), class_decl.name.span)?;
        self.emit_opcode_and_byte(OpCode::Class, byte, class_decl.name.span);
        self.define_variable(Some(class_handle), class_decl.name.span)?;

        self.handle_variable(&class_decl.name.name, class_decl.name.span, false)?;

        if let Some(superclass) = &class_decl.superclass {
            self.handle_variable(&class_decl.name.name, class_decl.name.span, false)?;

            if class_decl.name.name == superclass.name {
                return Err(QangSyntaxError::new(
                    "A class cannot inherit from itself.".to_string(),
                    superclass.span,
                ));
            }

            self.begin_scope();
            let zuper = "super";
            let super_handle = self.allocator.strings.intern(zuper);
            self.add_local(zuper, class_decl.span)?;
            self.define_variable(Some(super_handle), class_decl.name.span)?;

            self.handle_variable(&superclass.name, superclass.span, false)?;

            self.handle_variable(zuper, class_decl.span, true)?;

            self.emit_opcode(OpCode::Inherit, superclass.span);
        }

        for member in &class_decl.members {
            match &member {
                ast::ClassMember::Method(function) => {
                    let compiler_kind = if function.name.name == "init".into() {
                        CompilerKind::Initializer
                    } else {
                        CompilerKind::Method
                    };
                    let handle_identifier = self.allocator.strings.intern(&function.name.name);

                    self.handle_function(
                        compiler_kind,
                        function,
                        class_decl.superclass.is_some(),
                        errors,
                    )?;

                    self.emit_constant_opcode(
                        OpCode::Method,
                        OpCode::Method16,
                        Value::String(handle_identifier),
                        function.span,
                    )?;
                }
                ast::ClassMember::Field(field) => {
                    let default =
                        ast::Expr::Primary(ast::PrimaryExpr::Nil(NilLiteral { span: field.span }));
                    let initializer = field.initializer.as_ref().unwrap_or(&default);
                    self.visit_expression(initializer, errors)?;
                    let handle_identifier = self.allocator.strings.intern(&field.name.name);
                    self.emit_constant_opcode(
                        OpCode::InitField,
                        OpCode::InitField16,
                        Value::String(handle_identifier),
                        field.name.span,
                    )?;
                }
            }
        }

        self.emit_opcode(OpCode::Pop, class_decl.span);
        if class_decl.superclass.is_some() {
            self.end_scope(class_decl.span);
        }

        Ok(())
    }

    fn visit_array_literal(
        &mut self,
        array: &ast::ArrayLiteral,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let length = array.elements.len();

        if length > u8::MAX.into() {
            return Err(QangSyntaxError::new(
                "An array literal cannot be initialized with more than 256 elements.".to_string(),
                array.span,
            ));
        }

        for expr in array.elements.iter().rev() {
            self.visit_expression(expr, errors)?;
        }

        self.emit_opcode_and_byte(OpCode::ArrayLiteral, length as u8, array.span);

        Ok(())
    }

    fn visit_object_literal(
        &mut self,
        object: &ast::ObjectLiteral,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let length = object.entries.len();

        if length > u8::MAX.into() {
            return Err(QangSyntaxError::new(
                "An object literal cannot be initialized with more than 256 entries.".to_string(),
                object.span,
            ));
        }

        for entry in object.entries.iter() {
            self.visit_object_entry(entry, errors)?;
        }

        self.emit_opcode_and_byte(OpCode::ObjectLiteral, length as u8, object.span);
        Ok(())
    }

    fn visit_object_entry(
        &mut self,
        entry: &ast::ObjectEntry,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let identifier_handle = self.allocator.strings.intern(&entry.key.name);
        self.emit_constant(Value::String(identifier_handle), entry.key.span)?;
        self.visit_expression(&entry.value, errors)?;
        Ok(())
    }
}
