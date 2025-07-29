use crate::{
    ErrorReporter, QangError, QangErrors, QangResult, SourceMap, Value,
    ast::{self, AstTransformer, AstVisitor},
    chunk::{Chunk, OpCode},
    heap::ObjectHeap,
    parser::Parser,
};

trait TransformerMiddleware {
    fn run(&mut self, program: ast::Program, _errors: &mut ErrorReporter) -> ast::Program {
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
    T: AstTransformer<Error = QangError>,
{
    fn run(&mut self, program: ast::Program, errors: &mut ErrorReporter) -> ast::Program {
        match self.transformer.transform_program(program) {
            Ok(transformed) => transformed,
            Err(error) => {
                errors.report_error(error);
                // Return original or empty program on error
                ast::Program::new(Vec::new(), ast::SourceSpan::default())
            }
        }
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
    T: AstVisitor<(), Error = QangError>,
{
    fn analyze(&mut self, program: &ast::Program, errors: &mut ErrorReporter) {
        if let Err(error) = self.visitor.visit_program(program) {
            errors.report_error(error);
        }
    }
}

pub struct Compiler<'a> {
    source_map: &'a SourceMap,
    errors: ErrorReporter<'a>,
    current_span: ast::SourceSpan,
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
            current_span: ast::SourceSpan::default(),
            errors: ErrorReporter::new(source_map),
            transformers: Vec::new(),
            analyzers: Vec::new(),
            is_silent: false,
            heap: ObjectHeap::new(),
        }
    }

    pub fn add_analyzer<T>(mut self, analyzer: T) -> Self
    where
        T: AstVisitor<(), Error = QangError> + 'static,
    {
        self.analyzers.push(Box::new(VisitorAdapter::new(analyzer)));

        self
    }

    pub fn add_transformer<T>(mut self, transformer: T) -> Self
    where
        T: AstTransformer<Error = QangError> + 'static,
    {
        self.transformers
            .push(Box::new(TransformerAdapter::new(transformer)));

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

        for transformer in &mut self.transformers {
            program = transformer.as_mut().run(program, &mut errors);
        }

        for analyzer in &mut self.analyzers {
            analyzer.analyze(&program, &mut errors);
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

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);

        self.emit_opcode_and_byte(OpCode::Constant, constant);
    }

    fn report_error(&mut self, message: &str) {
        self.is_silent = true;
        self.errors.report_parse_error(message, self.current_span);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let index = self.current_chunk.add_constant(value);

        if index > u8::MAX as usize {
            self.report_error("Constant index out of range");
            0
        } else {
            index as u8
        }
    }
}

impl<'a> AstVisitor<()> for Compiler<'a> {
    type Error = QangError;

    fn visit_number_literal(&mut self, number: &ast::NumberLiteral) -> Result<(), Self::Error> {
        self.emit_constant(number.value.into());
        Ok(())
    }

    fn visit_string_literal(&mut self, string: &ast::StringLiteral) -> Result<(), Self::Error> {
        let handle = self.heap.intern_string(string.value.to_owned());
        self.emit_constant(Value::String(handle));
        Ok(())
    }

    fn visit_boolean_literal(&mut self, boolean: &ast::BooleanLiteral) -> Result<(), Self::Error> {
        self.emit_opcode(boolean.value.into());
        Ok(())
    }

    fn visit_nil_literal(&mut self, _nil: &ast::NilLiteral) -> Result<(), Self::Error> {
        self.emit_opcode(OpCode::Nil);
        Ok(())
    }

    fn visit_comparison_expression(
        &mut self,
        comparison: &ast::ComparisonExpr,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&comparison.left)?;
        self.emit_opcode(comparison.operator.into());
        self.visit_expression(&comparison.right)?;
        Ok(())
    }

    fn visit_term_expression(&mut self, term: &ast::TermExpr) -> Result<(), Self::Error> {
        self.visit_expression(&term.left)?;
        self.emit_opcode(term.operator.into());
        self.visit_expression(&term.right)?;
        Ok(())
    }

    fn visit_factor_expression(&mut self, factor: &ast::FactorExpr) -> Result<(), Self::Error> {
        self.visit_expression(&factor.left)?;
        self.emit_opcode(factor.operator.into());
        self.visit_expression(&factor.right)?;
        Ok(())
    }

    fn visit_unary_expression(&mut self, unary: &ast::UnaryExpr) -> Result<(), Self::Error> {
        self.emit_opcode(unary.operator.into());
        self.visit_expression(&unary.operand)?;
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
