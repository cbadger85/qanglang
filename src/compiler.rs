use crate::{
    ErrorReporter, QangError, QangErrors, QangResult, SourceMap, Value,
    ast::{self, AstTransformer, AstVisitor, SourceSpan},
    chunk::{Chunk, OpCode},
    heap::ObjectHeap,
    parser::Parser,
};

trait TransformerMiddleware {
    fn run(&mut self, program: ast::Program) -> ast::Program {
        program
    }
}

struct TransformerAdapter<T> {
    transformer: T,
}

impl<T> TransformerAdapter<T> {
    pub fn new(transformer: T) -> Self {
        Self { transformer }
    }
}

impl<T> TransformerMiddleware for TransformerAdapter<T>
where
    T: AstTransformer,
{
    fn run(&mut self, program: ast::Program) -> ast::Program {
        self.transformer.transform_program(program)
    }
}

pub trait AnalysisMiddleware {
    fn analyze(&mut self, program: &ast::Program, errors: &mut ErrorReporter);
}

pub struct VisitorAdapter<T> {
    visitor: T,
}

impl<T> VisitorAdapter<T> {
    pub fn new(visitor: T) -> Self {
        Self { visitor }
    }
}

impl<T> AnalysisMiddleware for VisitorAdapter<T>
where
    T: AstVisitor<Error = QangError>,
{
    fn analyze(&mut self, program: &ast::Program, errors: &mut ErrorReporter) {
        if let Err(error) = self.visitor.visit_program(program, errors) {
            errors.report_error(error);
        }
    }
}

const STACK_MAX: usize = 256;

pub struct CompilerArtifact {
    pub source_map: SourceMap,
    pub chunk: Chunk,
    pub heap: ObjectHeap,
    pub ip: usize,
    pub stack_top: usize,
    pub stack: [Value; STACK_MAX],
}

impl CompilerArtifact {
    pub fn new(source_map: SourceMap, chunk: Chunk, heap: ObjectHeap) -> Self {
        Self {
            source_map,
            chunk,
            heap,
            ip: 0,
            stack_top: 0,
            stack: std::array::from_fn(|_| Value::default()),
        }
    }

    pub fn get_current_span(&self) -> SourceSpan {
        self.chunk.spans()[self.ip]
    }
}

pub struct Compiler<'a> {
    source_map: &'a SourceMap,
    current_chunk: Chunk,
    transformers: Vec<Box<dyn TransformerMiddleware>>,
    analyzers: Vec<Box<dyn AnalysisMiddleware>>,
    is_silent: bool,
    heap: ObjectHeap,
}

impl<'a> Compiler<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            current_chunk: Chunk::new(),
            transformers: Vec::new(),
            analyzers: Vec::new(),
            is_silent: false,
            heap: ObjectHeap::new(),
        }
    }

    pub fn add_analyzer<T>(mut self, analyzer: T) -> Self
    where
        T: AstVisitor<Error = QangError> + 'static,
    {
        self.analyzers.push(Box::new(VisitorAdapter::new(analyzer)));

        self
    }

    pub fn add_transformer<T>(mut self, transformer: T) -> Self
    where
        T: AstTransformer + 'static,
    {
        self.transformers
            .push(Box::new(TransformerAdapter::new(transformer)));

        self
    }

    pub fn set_silent(mut self, is_silent: bool) -> Self {
        self.is_silent = is_silent;

        self
    }

    pub fn compile(mut self) -> QangResult<CompilerArtifact> {
        let mut parser = Parser::new(self.source_map);
        let mut program = parser.parse();
        let mut errors = parser.into_reporter();

        for analyzer in &mut self.analyzers {
            analyzer.analyze(&program, &mut errors);
        }

        for transformer in &mut self.transformers {
            program = transformer.as_mut().run(program);
        }

        self.visit_program(&program, &mut errors)
            .map_err(|err| QangErrors(vec![err]))?;

        if errors.has_errors() {
            Err(errors.into_errors())
        } else {
            self.emit_opcode(
                OpCode::Return,
                SourceSpan::new(
                    self.source_map.get_source().len(),
                    self.source_map.get_source().len(),
                ),
            );
            Ok(CompilerArtifact::new(
                self.source_map.to_owned(),
                self.current_chunk,
                self.heap,
            ))
        }
    }

    fn emit_opcode(&mut self, opcode: OpCode, span: SourceSpan) {
        if !self.is_silent {
            self.current_chunk.write_opcode(opcode, span);
        }
    }

    fn emit_byte(&mut self, byte: u8, span: SourceSpan) {
        if !self.is_silent {
            self.current_chunk.write(byte, span);
        }
    }

    fn emit_opcode_and_byte(&mut self, opcode: OpCode, byte: u8, span: SourceSpan) {
        self.emit_opcode(opcode, span);
        self.emit_byte(byte, span);
    }

    fn emit_constant(&mut self, value: Value, span: SourceSpan, errors: &mut ErrorReporter) {
        let constant = self.make_constant(value, span, errors);

        self.emit_opcode_and_byte(OpCode::Constant, constant, span);
    }

    fn make_constant(&mut self, value: Value, span: SourceSpan, errors: &mut ErrorReporter) -> u8 {
        let index = self.current_chunk.add_constant(value);

        if index > u8::MAX as usize {
            self.is_silent = true;
            errors.report_parse_error("Constant index out of range", span);
            0
        } else {
            index as u8
        }
    }
}

impl<'a> AstVisitor for Compiler<'a> {
    type Error = QangError;

    fn visit_number_literal(
        &mut self,
        number: &ast::NumberLiteral,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.emit_constant(number.value.into(), number.span, errors);
        Ok(())
    }

    fn visit_string_literal(
        &mut self,
        string: &ast::StringLiteral,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        let handle = self.heap.intern_string(string.value.to_owned());
        self.emit_constant(Value::String(handle), string.span, errors);
        Ok(())
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
        self.emit_opcode(comparison.operator.into(), comparison.span);
        self.visit_expression(&comparison.right, errors)?;
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
        self.emit_opcode(factor.operator.into(), factor.span);
        self.visit_expression(&factor.right, errors)?;
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
        _assignment: &ast::AssignmentExpr,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        _var_decl: &ast::VariableDecl,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_lambda_declaration(
        &mut self,
        _lambda_decl: &ast::LambdaDecl,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        _func_decl: &ast::FunctionDecl,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_class_declaration(
        &mut self,
        _class_decl: &ast::ClassDecl,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}
