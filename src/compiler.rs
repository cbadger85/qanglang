use crate::{
    AstVisitor, ErrorReporter, QangError, QangErrors, QangResult, SourceMap, Value, ast,
    chunk::{Chunk, OpCode},
    heap::ObjectHeap,
    parser::Parser,
};

pub trait CompilerMiddleware {
    fn run(&mut self, program: ast::Program, _errors: &mut ErrorReporter) -> ast::Program {
        program
    }
}

pub struct Compiler<'a> {
    source_map: &'a SourceMap,
    errors: ErrorReporter<'a>,
    current_span: ast::SourceSpan,
    current_chunk: Chunk,
    middleware: Vec<Box<dyn CompilerMiddleware>>,
    is_silent: bool,
    heap: ObjectHeap,
}

impl<'a> Compiler<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            current_chunk: Chunk::new(),
            current_span: ast::SourceSpan::default(),
            errors: ErrorReporter::new(source_map),
            middleware: Vec::new(),
            is_silent: false,
            heap: ObjectHeap::new(),
        }
    }

    pub fn add_middleware<T>(mut self, middleware: T) -> Self
    where
        T: CompilerMiddleware + 'static,
    {
        self.middleware.push(Box::new(middleware));

        self
    }

    pub fn set_silent(mut self, is_silent: bool) -> Self {
        self.is_silent = is_silent;

        self
    }

    pub fn compile(mut self) -> QangResult<Chunk> {
        let mut parser = Parser::new(self.source_map);
        let mut program = parser.parse();
        let mut errors = parser.into_reporter();

        for middleware in &mut self.middleware {
            program = middleware.as_mut().run(program, &mut errors);
        }

        self.errors = errors;

        self.visit_program(&program)
            .map_err(|err| QangErrors(vec![err]))?;

        if self.errors.has_errors() {
            Err(self.errors.into_errors())
        } else {
            Ok(self.current_chunk)
        }
    }

    fn emit_opcode(&mut self, opcode: OpCode) {
        if !self.is_silent {
            self.current_chunk.write_opcode(opcode, self.current_span);
        }
    }

    fn emit_byte(&mut self, byte: u8) {
        if !self.is_silent {
            self.current_chunk.write(byte, self.current_span);
        }
    }

    fn emit_opcode_and_byte(&mut self, opcode: OpCode, byte: u8) {
        self.emit_opcode(opcode);
        self.emit_byte(byte);
    }

    fn emit_constant(&mut self, value: Value) -> Result<(), QangError> {
        let constant = self.make_constant(value)?;

        self.emit_opcode_and_byte(OpCode::Constant, constant);

        Ok(())
    }

    fn make_constant(&mut self, value: Value) -> Result<u8, QangError> {
        let index = self.current_chunk.add_constant(value);

        if index > u8::MAX as usize {
            Err(QangError::parse_error(
                "Constant index out of range",
                self.current_span,
            ))
        } else {
            Ok(index as u8)
        }
    }
}

impl<'a> AstVisitor<()> for Compiler<'a> {
    type Error = QangError;

    fn get_span(&self) -> ast::SourceSpan {
        self.current_span
    }

    fn set_span(&mut self, span: ast::SourceSpan) {
        self.current_span = span;
    }

    fn visit_number_literal(&mut self, _number: &ast::NumberLiteral) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_assignment_expression(
        &mut self,
        _assignment: &ast::AssignmentExpr,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        _var_decl: &ast::VariableDecl,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_lambda_declaration(
        &mut self,
        _lambda_decl: &ast::LambdaDecl,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        _func_decl: &ast::FunctionDecl,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_class_declaration(&mut self, _class_decl: &ast::ClassDecl) -> Result<(), Self::Error> {
        Ok(())
    }

    fn default_result(&self) -> Result<(), Self::Error> {
        Ok(())
    }
}
