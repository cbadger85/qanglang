use std::rc::Rc;

use crate::{
    ErrorReporter, QangSyntaxError, SourceMap, Value,
    ast::{self, AstVisitor, SourceSpan},
    chunk::{Chunk, OpCode, SourceLocation},
    heap::{FunctionObject, ObjectHandle, ObjectHeap},
    parser::Parser,
    source::DEFALT_SOURCE_MAP,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ErrorMessageFormat {
    Verbose,
    Compact,
    Minimal,
}

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
pub struct QangProgram(Rc<FunctionObject>);

impl QangProgram {
    pub fn new(function: FunctionObject) -> Self {
        Self(Rc::new(function))
    }

    pub fn into_function(self) -> Rc<FunctionObject> {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
struct Artifact {
    kind: CompilerKind,
    function: FunctionObject,
    previous: Option<Box<Artifact>>,
}

impl Artifact {
    fn new_script(handle: ObjectHandle) -> Self {
        Self {
            kind: CompilerKind::Script,
            function: FunctionObject::new(handle, 0),
            previous: None,
        }
    }

    fn new_function(handle: ObjectHandle, arity: usize) -> Self {
        Self {
            kind: CompilerKind::Function,
            function: FunctionObject::new(handle, arity),
            previous: None,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct ArtifactStack {
    artifacts: Vec<Artifact>,
}

impl ArtifactStack {
    fn new(handle: ObjectHandle) -> Self {
        Self {
            artifacts: vec![Artifact::new_script(handle)],
        }
    }

    fn push(&mut self, handle: ObjectHandle, arity: usize) {
        self.artifacts.push(Artifact::new_function(handle, arity));
    }

    fn pop(&mut self) -> Option<FunctionObject> {
        if self.artifacts.len() > 1 {
            self.artifacts.pop().map(|a| a.function)
        } else {
            None
        }
    }

    fn compiler_kind(&self) -> CompilerKind {
        self.artifacts[self.artifacts.len() - 1].kind
    }

    #[allow(dead_code)]
    fn function(&self) -> &FunctionObject {
        &self.artifacts[self.artifacts.len() - 1].function
    }

    fn function_mut(&mut self) -> &mut FunctionObject {
        let len = self.artifacts.len();
        &mut self.artifacts[len - 1].function
    }

    #[allow(dead_code)]
    fn iter(&self) -> impl Iterator<Item = &Artifact> {
        self.artifacts.iter().rev()
    }

    #[allow(dead_code)]
    fn iter_mut(&mut self) -> impl Iterator<Item = &mut Artifact> {
        self.artifacts.iter_mut().rev()
    }

    fn into_function(mut self) -> FunctionObject {
        self.artifacts.pop().unwrap().function
    }
}

pub struct CompilerPipeline<'a> {
    source_map: SourceMap,
    heap: &'a mut ObjectHeap,
    error_message_format: ErrorMessageFormat,
}

impl<'a> CompilerPipeline<'a> {
    pub fn new(source_map: SourceMap, heap: &'a mut ObjectHeap) -> Self {
        Self {
            source_map,
            heap,
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

        match Compiler::new(self.heap).compile(program, &self.source_map, errors) {
            Ok(program) => {
                let program = Rc::new(program);
                self.heap
                    .allocate_object(crate::HeapObject::Function(program.clone()));
                Ok(QangProgram(program.clone()))
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

#[derive(Debug, Clone)]
struct Local {
    name: Box<str>,
    depth: Option<usize>,
}

impl Local {
    fn new(name: Box<str>) -> Self {
        Self { name, depth: None }
    }
}

pub const FRAME_MAX: usize = 64;
pub const STACK_MAX: usize = FRAME_MAX * 256;

#[derive(Debug, Clone, Copy, PartialEq)]
enum CompilerKind {
    Script,
    Function,
}

impl Default for CompilerKind {
    fn default() -> Self {
        Self::Script
    }
}

pub struct Compiler<'a> {
    source_map: &'a SourceMap,
    heap: &'a mut ObjectHeap,
    locals: Vec<Local>,
    local_count: usize,
    scope_depth: usize,
    artifacts: ArtifactStack,
}

impl<'a> Compiler<'a> {
    pub fn new(heap: &'a mut ObjectHeap) -> Self {
        let handle = heap.intern_string("<script>".to_string().into_boxed_str());
        let locals = Vec::with_capacity(u8::MAX as usize);

        Self {
            source_map: &DEFALT_SOURCE_MAP,
            heap,
            locals,
            local_count: 0,
            scope_depth: 0,
            artifacts: ArtifactStack::new(handle),
        }
    }

    fn reset(&mut self) {
        self.source_map = &DEFALT_SOURCE_MAP;
        self.locals = Vec::with_capacity(STACK_MAX);
        self.local_count = 0;
        self.scope_depth = 0;
    }

    fn get_current_chunk(&mut self) -> &mut Chunk {
        &mut self.artifacts.function_mut().chunk
    }

    pub fn compile(
        &mut self,
        program: ast::Program,
        source_map: &'a SourceMap,
        mut errors: ErrorReporter,
    ) -> Result<FunctionObject, CompilerError> {
        self.source_map = source_map;

        self.visit_program(&program, &mut errors)
            .map_err(|err| CompilerError(vec![err]))?;

        self.emit_opcode(OpCode::Nil, SourceSpan::default());
        self.emit_opcode(OpCode::Return, SourceSpan::default());

        if errors.has_errors() {
            Err(CompilerError(errors.take_errors()))
        } else {
            let artifacts = std::mem::take(&mut self.artifacts);
            self.reset();

            Ok(artifacts.into_function())
        }
    }

    fn emit_opcode(&mut self, opcode: OpCode, span: SourceSpan) {
        let line = self.source_map.get_line_number(span.start);
        let col = self.source_map.get_column_number(span.start);
        self.get_current_chunk()
            .write_opcode(opcode, SourceLocation::new(line, col));
    }

    fn emit_byte(&mut self, byte: u8, span: SourceSpan) {
        let line = self.source_map.get_line_number(span.start);
        let col = self.source_map.get_column_number(span.start);
        self.get_current_chunk()
            .write(byte, SourceLocation::new(line, col));
    }

    fn emit_jump(&mut self, opcode: OpCode, span: SourceSpan) -> usize {
        self.emit_opcode(opcode, span);
        self.emit_byte(0xff, span);
        self.emit_byte(0xff, span);
        self.get_current_chunk().count() - 2
    }

    fn patch_jump(&mut self, offset: usize, span: SourceSpan) -> Result<(), QangSyntaxError> {
        let jump = self.get_current_chunk().count() - offset - 2;

        if jump > u16::MAX as usize {
            return Err(QangSyntaxError::new(
                "Too much code to jump over.".to_string(),
                span,
            ));
        }

        self.get_current_chunk().code_mut()[offset] = ((jump >> 8) & 0xff) as u8;
        self.get_current_chunk().code_mut()[offset + 1] = (jump & 0xff) as u8;

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize, span: SourceSpan) -> Result<(), QangSyntaxError> {
        self.emit_opcode(OpCode::Loop, span);
        let offset = self.get_current_chunk().count() - loop_start + 2;
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
        let constant = self.make_constant(value, span)?;
        self.emit_opcode_and_byte(OpCode::Constant, constant, span);

        Ok(())
    }

    fn make_constant(&mut self, value: Value, span: SourceSpan) -> Result<u8, QangSyntaxError> {
        let index = self.get_current_chunk().add_constant(value);

        if index > u8::MAX as usize {
            Err(QangSyntaxError::new(
                "Constant index out of range".to_string(),
                span,
            ))
        } else {
            Ok(index as u8)
        }
    }

    fn emit_return(&mut self, span: SourceSpan) {
        self.emit_opcode(OpCode::Nil, span);
        self.emit_opcode(OpCode::Return, span);
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, span: SourceSpan) {
        self.scope_depth -= 1;

        while self.local_count > 0
            && self
                .locals
                .get(self.local_count - 1)
                .and_then(|l| l.depth)
                .map(|local_depth| local_depth > self.scope_depth)
                .unwrap_or(false)
        {
            self.emit_opcode(OpCode::Pop, span);
            self.local_count -= 1;
        }
    }

    fn add_local(&mut self, handle: &str, span: SourceSpan) -> Result<(), QangSyntaxError> {
        if self.local_count >= STACK_MAX {
            Err(QangSyntaxError::new(
                "Too many local variables in scope.".to_string(),
                span,
            ))
        } else {
            let local = Local::new(handle.into());
            if self.local_count < self.locals.len() {
                // Reuse inactive local slot
                self.locals[self.local_count] = local;
            } else {
                // Grow vec
                self.locals.push(local);
            }
            self.local_count += 1;
            Ok(())
        }
    }

    fn declare_variable(&mut self, handle: &str, span: SourceSpan) -> Result<(), QangSyntaxError> {
        if self.scope_depth == 0 {
            return Ok(());
        }
        for i in (0..self.local_count).rev() {
            if let Some(local) = self.locals.get(i) {
                if local
                    .depth
                    .map(|local_depth| local_depth < self.scope_depth)
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
        handle: Option<ObjectHandle>,
        span: SourceSpan,
    ) -> Result<(), QangSyntaxError> {
        if self.scope_depth > 0 {
            self.mark_initialized();

            return Ok(());
        }
        let handle = handle.expect("Expected an object handle when defining global variables.");

        let index = self.make_constant(Value::String(handle), span)?;
        self.emit_opcode_and_byte(OpCode::DefineGlobal, index as u8, span);

        Ok(())
    }

    fn resolve_local_variable(
        &mut self,
        handle: &str,
        span: SourceSpan,
    ) -> Result<Option<usize>, QangSyntaxError> {
        if self.scope_depth == 0 {
            return Ok(None);
        }
        for i in (0..self.local_count).rev() {
            if let Some(local) = self.locals.get(i) {
                if *local.name == *handle {
                    if local.depth.is_none() {
                        return Err(QangSyntaxError::new(
                            "Cannot read local variable during its initialization.".to_string(),
                            span,
                        ));
                    }
                    return Ok(Some(i));
                }
            }
        }

        Ok(None)
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }
        if self.local_count > 0 {
            if let Some(local) = self.locals.get_mut(self.local_count - 1) {
                local.depth = Some(self.scope_depth)
            }
        }
    }

    fn parse_variable(
        &mut self,
        identifer: &str,
        span: SourceSpan,
    ) -> Result<Option<ObjectHandle>, QangSyntaxError> {
        self.declare_variable(identifer, span)?;

        if self.scope_depth > 0 {
            Ok(None)
        } else {
            Ok(Some(self.heap.intern_string(identifer.into())))
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
            } else {
                let handle = self.heap.intern_string(handle.into());
                let index = self.make_constant(Value::String(handle), span)?;
                (index, OpCode::GetGlobal, OpCode::SetGlobal)
            }
        };

        let op = if is_assignment { set_op } else { get_op };
        self.emit_opcode_and_byte(op, index as u8, span);

        Ok(())
    }
}

impl<'a> AstVisitor for Compiler<'a> {
    type Error = QangSyntaxError;

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
        let handle = self.heap.intern_string(string.value.to_owned());
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
        self.visit_expression(&assignment.value, errors)?;

        match &assignment.target {
            ast::AssignmentTarget::Identifier(identifier) => {
                self.handle_variable(&identifier.name, identifier.span, true)?;
            }
            _ => {
                return Err(QangSyntaxError::new(
                    "Invalid expression.".to_string(),
                    assignment.target.span(),
                ));
            }
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
        let global = self.parse_variable(&var_decl.name.name, var_decl.name.span)?;

        if let Some(expr) = &var_decl.initializer {
            self.visit_expression(expr, errors)?;
        } else {
            self.emit_opcode(OpCode::Nil, var_decl.span);
        }

        self.define_variable(global, var_decl.name.span)?;
        if global.is_none() {
            self.handle_variable(&var_decl.name.name, var_decl.span, true)?;
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

    fn visit_lambda_declaration(
        &mut self,
        _lambda_decl: &ast::LambdaDecl,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: &ast::WhileStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let loop_start = self.get_current_chunk().count();
        self.visit_expression(&while_stmt.condition, errors)?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse, while_stmt.body.span());
        self.emit_opcode(OpCode::Pop, while_stmt.body.span());
        self.visit_statement(&while_stmt.body, errors)?;
        self.emit_loop(loop_start, while_stmt.body.span())?;

        self.patch_jump(exit_jump, while_stmt.body.span())?;
        self.emit_opcode(OpCode::Pop, while_stmt.span);

        Ok(())
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: &ast::ForStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.begin_scope();

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

        let mut loop_start = self.get_current_chunk().count();
        let mut exit_jump: Option<usize> = None;

        if let Some(condition) = &for_stmt.condition {
            let condition_jump = self.emit_jump(OpCode::Jump, condition.span());
            loop_start = self.get_current_chunk().count();
            self.visit_statement(&for_stmt.body, errors)?;

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

            if let Some(increment) = &for_stmt.increment {
                self.visit_expression(increment, errors)?;
                self.emit_opcode(OpCode::Pop, increment.span());
            }

            self.emit_loop(loop_start, for_stmt.body.span())?;
        }

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump, for_stmt.span)?;
            self.emit_opcode(OpCode::Pop, for_stmt.span);
        }

        self.end_scope(for_stmt.span);

        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: &ast::FunctionDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let function_identifier_handle =
            self.parse_variable(&func_decl.function.name.name, func_decl.function.span)?;

        let function_name_handle = function_identifier_handle.unwrap_or_else(|| {
            self.heap
                .intern_string(func_decl.function.name.name.to_owned())
        });

        self.artifacts
            .push(function_name_handle, func_decl.function.parameters.len());
        let previous_locals = std::mem::take(&mut self.locals);
        let previous_local_count = self.local_count;
        let previous_scope_depth = self.scope_depth;

        // Reset state for function compilation
        self.locals = Vec::with_capacity(STACK_MAX);
        self.local_count = 0;
        self.scope_depth = 0;
        self.begin_scope();

        let parameter_span = SourceSpan::combine(
            func_decl
                .function
                .parameters
                .first()
                .map(|p| p.span)
                .unwrap_or(func_decl.span),
            func_decl
                .function
                .parameters
                .last()
                .map(|p| p.span)
                .unwrap_or(func_decl.span),
        );

        if func_decl.function.parameters.len() > 255 {
            return Err(QangSyntaxError::new(
                "Cannot have more than 255 parameters.".to_string(),
                parameter_span,
            ));
        }

        for parameter in &func_decl.function.parameters {
            let handle = self.parse_variable(&parameter.name, parameter.span)?;

            self.define_variable(handle, parameter.span)?;
        }

        self.visit_block_statement(&func_decl.function.body, errors)?;
        self.emit_return(func_decl.span);

        let function = self
            .artifacts
            .pop()
            .expect("Unexpected end of artifact stack.");

        let function_handle = self.heap.allocate_object(function.into());
        let constant_index =
            self.make_constant(Value::FunctionDecl(function_handle), func_decl.span)?;
        self.emit_opcode_and_byte(OpCode::Closure, constant_index, func_decl.span);

        // Restore previous compiler state
        self.locals = previous_locals;
        self.local_count = previous_local_count;
        self.scope_depth = previous_scope_depth;

        self.define_variable(function_identifier_handle, func_decl.span)?;
        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &ast::ReturnStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if self.artifacts.compiler_kind() == CompilerKind::Script {
            return Err(QangSyntaxError::new(
                "Cannot return from top-level code.".to_string(),
                return_stmt.span,
            ));
        }

        if let Some(value) = &return_stmt.value {
            self.visit_expression(value, errors)?;
        } else {
            self.emit_opcode(OpCode::Nil, return_stmt.span);
        }

        self.emit_opcode(OpCode::Return, return_stmt.span);
        Ok(())
    }

    fn visit_call_expression(
        &mut self,
        call: &ast::CallExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match call.callee.as_ref() {
            ast::Expr::Primary(ast::PrimaryExpr::Identifier(identifier)) => {
                match call.operation.as_ref() {
                    ast::CallOperation::Call(args) => {
                        if args.len() > u8::MAX as usize {
                            return Err(QangSyntaxError::new(
                                "Functions may only take up to 256 arguments.".to_string(),
                                call.span,
                            ));
                        }

                        self.visit_identifier(identifier, errors)?;

                        for arg in args {
                            self.visit_expression(arg, errors)?;
                        }

                        self.emit_opcode_and_byte(OpCode::Call, args.len() as u8, call.span);

                        Ok(())
                    }
                    _ => Err(QangSyntaxError::new(
                        "Expected function call.".to_string(),
                        call.span,
                    )),
                }
            }
            _ => Ok(()),
        }
    }

    fn visit_class_declaration(
        &mut self,
        _class_decl: &ast::ClassDecl,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}
