use crate::{QangError, tokenizer::Token};

/// Represents a position in the source code for error reporting and debugging
#[derive(Debug, Clone, PartialEq, Default, Copy)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn from_token(token: &Token) -> Self {
        Self {
            start: token.start,
            end: token.end,
        }
    }

    pub fn combine(start: SourceSpan, end: SourceSpan) -> Self {
        Self {
            start: start.start,
            end: end.end,
        }
    }
}

/// Root AST node representing a complete program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub span: SourceSpan,
}

/// Top-level declarations in the program
#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Class(ClassDecl),
    Function(FunctionDecl),
    Lambda(LambdaDecl),
    Variable(VariableDecl),
    Stmt(Stmt),
}

impl Decl {
    pub fn span(&self) -> SourceSpan {
        match self {
            Decl::Class(decl) => decl.span,
            Decl::Function(decl) => decl.span,
            Decl::Lambda(decl) => decl.span,
            Decl::Variable(decl) => decl.span,
            Decl::Stmt(stmt) => stmt.span(),
        }
    }
}

/// Class declaration: class IDENTIFIER ( : IDENTIFIER )? { classMember* }
#[derive(Debug, Clone, PartialEq)]
pub struct ClassDecl {
    pub name: Identifier,
    pub superclass: Option<Identifier>,
    pub members: Vec<ClassMember>,
    pub span: SourceSpan,
}

/// Members that can appear inside a class
#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
    Method(FunctionExpr),
    Field(FieldDecl),
}

impl ClassMember {
    pub fn span(&self) -> SourceSpan {
        match self {
            ClassMember::Method(method) => method.span,
            ClassMember::Field(field) => field.span,
        }
    }
}

/// Field declaration: IDENTIFIER ( = expression )? ;
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDecl {
    pub name: Identifier,
    pub initializer: Option<Expr>,
    pub span: SourceSpan,
}

/// Function declaration: fn function
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub function: FunctionExpr,
    pub span: SourceSpan,
}

/// Function expression: IDENTIFIER ( parameters? ) block
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExpr {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: BlockStmt,
    pub span: SourceSpan,
}

/// Lambda declaration: var IDENTIFIER = lambda
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaDecl {
    pub name: Identifier,
    pub lambda: LambdaExpr,
    pub span: SourceSpan,
}

/// Lambda expression: ( parameters? ) -> ( block | expression )
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaExpr {
    pub parameters: Vec<Identifier>,
    pub body: Box<LambdaBody>,
    pub span: SourceSpan,
}

/// Body of a lambda can be either a block or a single expression
#[derive(Debug, Clone, PartialEq)]
pub enum LambdaBody {
    Block(BlockStmt),
    Expr(Box<Expr>),
}

impl LambdaBody {
    pub fn span(&self) -> SourceSpan {
        match self {
            LambdaBody::Block(block) => block.span,
            LambdaBody::Expr(expr) => expr.span(),
        }
    }
}

/// Variable declaration: var IDENTIFIER ( = expression )? ;
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDecl {
    pub name: Identifier,
    pub initializer: Option<Expr>,
    pub span: SourceSpan,
}

/// All possible statements
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(ExprStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Return(ReturnStmt),
    Throw(ThrowStmt),
    Try(TryStmt),
}

impl Stmt {
    pub fn span(&self) -> SourceSpan {
        match self {
            Stmt::Expr(stmt) => stmt.span,
            Stmt::Block(stmt) => stmt.span,
            Stmt::If(stmt) => stmt.span,
            Stmt::While(stmt) => stmt.span,
            Stmt::For(stmt) => stmt.span,
            Stmt::Break(stmt) => stmt.span,
            Stmt::Continue(stmt) => stmt.span,
            Stmt::Return(stmt) => stmt.span,
            Stmt::Throw(stmt) => stmt.span,
            Stmt::Try(stmt) => stmt.span,
        }
    }
}

/// Expression statement: expression ;
#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: SourceSpan,
}

/// Block statement: { declaration* }
#[derive(Debug, Clone, PartialEq)]
pub struct BlockStmt {
    pub decls: Vec<Decl>,
    pub span: SourceSpan,
}

/// If statement: if ( expression ) statement ( else statement )?
#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
    pub span: SourceSpan,
}

/// While statement: while ( expression ) statement
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
    pub span: SourceSpan,
}

/// For statement: for ( ( varDecl | exprStmt | ; ) expression? ; expression? ) statement
#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub initializer: Option<ForInitializer>,
    pub condition: Option<Expr>,
    pub increment: Option<Expr>,
    pub body: Box<Stmt>,
    pub span: SourceSpan,
}

/// Initializer for a for loop
#[derive(Debug, Clone, PartialEq)]
pub enum ForInitializer {
    Variable(VariableDecl),
    Expr(Expr),
}

impl ForInitializer {
    pub fn span(&self) -> SourceSpan {
        match self {
            ForInitializer::Variable(var) => var.span,
            ForInitializer::Expr(expr) => expr.span(),
        }
    }
}

/// Break statement: break ;
#[derive(Debug, Clone, PartialEq)]
pub struct BreakStmt {
    pub span: SourceSpan,
}

/// Continue statement: continue ;
#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStmt {
    pub span: SourceSpan,
}

/// Return statement: return expression? ;
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: SourceSpan,
}

/// Throw statement: throw expression? ;
#[derive(Debug, Clone, PartialEq)]
pub struct ThrowStmt {
    pub value: Option<Expr>,
    pub span: SourceSpan,
}

/// Try statement: try block catchFinally
#[derive(Debug, Clone, PartialEq)]
pub struct TryStmt {
    pub try_block: BlockStmt,
    pub catch_clause: Option<CatchClause>,
    pub finally_block: Option<BlockStmt>,
    pub span: SourceSpan,
}

/// Catch clause: catch ( IDENTIFIER )? block
#[derive(Debug, Clone, PartialEq)]
pub struct CatchClause {
    pub parameter: Option<Identifier>,
    pub body: BlockStmt,
    pub span: SourceSpan,
}

/// All possible expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assignment(AssignmentExpr),
    Pipe(PipeExpr),
    Ternary(TernaryExpr),
    LogicalOr(LogicalOrExpr),
    LogicalAnd(LogicalAndExpr),
    Equality(EqualityExpr),
    Comparison(ComparisonExpr),
    Term(TermExpr),
    Factor(FactorExpr),
    Unary(UnaryExpr),
    Call(CallExpr),
    Primary(PrimaryExpr),
}

impl Expr {
    pub fn span(&self) -> SourceSpan {
        match self {
            Expr::Assignment(expr) => expr.span,
            Expr::Pipe(expr) => expr.span,
            Expr::Ternary(expr) => expr.span,
            Expr::LogicalOr(expr) => expr.span,
            Expr::LogicalAnd(expr) => expr.span,
            Expr::Equality(expr) => expr.span,
            Expr::Comparison(expr) => expr.span,
            Expr::Term(expr) => expr.span,
            Expr::Factor(expr) => expr.span,
            Expr::Unary(expr) => expr.span,
            Expr::Call(expr) => expr.span,
            Expr::Primary(expr) => expr.span(),
        }
    }
}

/// Assignment expression: ( call . IDENTIFIER | IDENTIFIER ) = assignment | pipe
#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpr {
    pub target: AssignmentTarget,
    pub value: Box<Expr>,
    pub span: SourceSpan,
}

/// Target of an assignment
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTarget {
    Identifier(Identifier),
    Property(PropertyAccess),
}

impl AssignmentTarget {
    pub fn span(&self) -> SourceSpan {
        match self {
            AssignmentTarget::Identifier(id) => id.span,
            AssignmentTarget::Property(prop) => prop.span,
        }
    }
}

/// Property access for assignment: call . IDENTIFIER
#[derive(Debug, Clone, PartialEq)]
pub struct PropertyAccess {
    pub object: Box<Expr>,
    pub property: Identifier,
    pub span: SourceSpan,
}

/// Pipe expression: ternary ( |> pipe )?
#[derive(Debug, Clone, PartialEq)]
pub struct PipeExpr {
    pub left: Box<Expr>,
    pub right: Option<Box<Expr>>,
    pub span: SourceSpan,
}

/// Ternary expression: logicOr ( ? expression : ternary )?
#[derive(Debug, Clone, PartialEq)]
pub struct TernaryExpr {
    pub condition: Box<Expr>,
    pub then_expr: Option<Box<Expr>>,
    pub else_expr: Option<Box<Expr>>,
    pub span: SourceSpan,
}

/// Logical OR expression: logicAnd ( or logicAnd )*
#[derive(Debug, Clone, PartialEq)]
pub struct LogicalOrExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub span: SourceSpan,
}

/// Logical AND expression: equality ( and equality )*
#[derive(Debug, Clone, PartialEq)]
pub struct LogicalAndExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub span: SourceSpan,
}

/// Equality expression: comparison ( ( != | == ) comparison )*
#[derive(Debug, Clone, PartialEq)]
pub struct EqualityExpr {
    pub left: Box<Expr>,
    pub operator: EqualityOperator,
    pub right: Box<Expr>,
    pub span: SourceSpan,
}

/// Equality operators
#[derive(Debug, Clone, PartialEq)]
pub enum EqualityOperator {
    Equal,    // ==
    NotEqual, // !=
}

/// Comparison expression: term ( ( > | >= | < | <= ) term )*
#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonExpr {
    pub left: Box<Expr>,
    pub operator: ComparisonOperator,
    pub right: Box<Expr>,
    pub span: SourceSpan,
}

/// Comparison operators
#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOperator {
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=
}

/// Term expression: factor ( ( + | - ) factor )*
#[derive(Debug, Clone, PartialEq)]
pub struct TermExpr {
    pub left: Box<Expr>,
    pub operator: TermOperator,
    pub right: Box<Expr>,
    pub span: SourceSpan,
}

/// Term operators
#[derive(Debug, Clone, PartialEq)]
pub enum TermOperator {
    Add,      // +
    Subtract, // -
}

/// Factor expression: unary ( ( / | * | % ) unary )*
#[derive(Debug, Clone, PartialEq)]
pub struct FactorExpr {
    pub left: Box<Expr>,
    pub operator: FactorOperator,
    pub right: Box<Expr>,
    pub span: SourceSpan,
}

/// Factor operators
#[derive(Debug, Clone, PartialEq)]
pub enum FactorOperator {
    Divide,   // /
    Multiply, // *
    Modulo,   // %
}

/// Unary expression: ( - | ! ) unary | call
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub operand: Box<Expr>,
    pub span: SourceSpan,
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus, // -
    Not,   // !
}

/// Call expression: primary ( ( arguments? ) | . IDENTIFIER | .? IDENTIFIER | [ expression ] )*
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub operation: Box<CallOperation>,
    pub span: SourceSpan,
}

/// Operations that can be chained on a call expression
#[derive(Debug, Clone, PartialEq)]
pub enum CallOperation {
    Call(Vec<Expr>),              // ( arguments? )
    Property(Identifier),         // . IDENTIFIER
    OptionalProperty(Identifier), // .? IDENTIFIER
    Index(Expr),                  // [ expression ]
}

impl CallOperation {
    pub fn span(&self) -> SourceSpan {
        match self {
            CallOperation::Call(args) => {
                if args.is_empty() {
                    // This should be handled by the parser to provide proper span
                    SourceSpan::new(0, 0)
                } else {
                    SourceSpan::combine(args[0].span(), args[args.len() - 1].span())
                }
            }
            CallOperation::Property(id) => id.span,
            CallOperation::OptionalProperty(id) => id.span,
            CallOperation::Index(expr) => expr.span(),
        }
    }
}

/// Primary expressions
#[derive(Debug, Clone, PartialEq)]
pub enum PrimaryExpr {
    Number(NumberLiteral),
    String(StringLiteral),
    Boolean(BooleanLiteral),
    Nil(NilLiteral),
    This(ThisExpr),
    Super(SuperExpr),
    Identifier(Identifier),
    Grouping(GroupingExpr),
    Lambda(Box<LambdaExpr>),
    Array(ArrayLiteral),
}

impl PrimaryExpr {
    pub fn span(&self) -> SourceSpan {
        match self {
            PrimaryExpr::Number(lit) => lit.span,
            PrimaryExpr::String(lit) => lit.span,
            PrimaryExpr::Boolean(lit) => lit.span,
            PrimaryExpr::Nil(lit) => lit.span,
            PrimaryExpr::This(expr) => expr.span,
            PrimaryExpr::Super(expr) => expr.span,
            PrimaryExpr::Identifier(id) => id.span,
            PrimaryExpr::Grouping(expr) => expr.span,
            PrimaryExpr::Lambda(lambda) => lambda.span,
            PrimaryExpr::Array(array) => array.span,
        }
    }
}

/// Number literal
#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteral {
    pub value: f64,
    pub span: SourceSpan,
}

/// String literal
#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
    pub span: SourceSpan,
}

/// Boolean literal
#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
    pub span: SourceSpan,
}

/// Nil literal
#[derive(Debug, Clone, PartialEq)]
pub struct NilLiteral {
    pub span: SourceSpan,
}

/// This expression
#[derive(Debug, Clone, PartialEq)]
pub struct ThisExpr {
    pub span: SourceSpan,
}

/// Super expression: "super"
#[derive(Debug, Clone, PartialEq)]
pub struct SuperExpr {
    pub span: SourceSpan,
}

/// Super constructor call: "super"
#[derive(Debug, Clone, PartialEq)]
pub struct SuperConstructor {
    pub span: SourceSpan,
}

/// Super method call: "super" "." IDENTIFIER
#[derive(Debug, Clone, PartialEq)]
pub struct SuperMethod {
    pub method: Identifier,
    pub span: SourceSpan,
}

/// Identifier
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub span: SourceSpan,
}

/// Grouping expression: ( expression )
#[derive(Debug, Clone, PartialEq)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
    pub span: SourceSpan,
}

/// Array literal: [ ( expression ( , expression )* )? ]
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Vec<Expr>,
    pub span: SourceSpan,
}

/// Utility functions for working with the AST
impl Program {
    pub fn new(decls: Vec<Decl>, span: SourceSpan) -> Self {
        Self { decls, span }
    }
}

impl Identifier {
    pub fn new(name: String, span: SourceSpan) -> Self {
        Self { name, span }
    }
}

/// A visitor trait for walking the AST and producing results
///
/// This trait provides default implementations that recursively visit child nodes.
/// Implementors can override specific methods to customize behavior for particular node types.
pub trait AstVisitor<T> {
    type Error;

    fn set_span(&mut self, _span: SourceSpan) {}

    fn get_span(&self) -> SourceSpan {
        SourceSpan::default()
    }

    fn visit_program(&mut self, program: &Program) -> Result<T, Self::Error> {
        self.set_span(program.span);
        for decl in &program.decls {
            self.visit_declaration(decl)?;
        }
        self.default_result()
    }

    fn visit_declaration(&mut self, decl: &Decl) -> Result<T, Self::Error> {
        self.set_span(decl.span());
        match decl {
            Decl::Class(class_decl) => self.visit_class_declaration(class_decl),
            Decl::Function(func_decl) => self.visit_function_declaration(func_decl),
            Decl::Lambda(lambda_decl) => self.visit_lambda_declaration(lambda_decl),
            Decl::Variable(var_decl) => self.visit_variable_declaration(var_decl),
            Decl::Stmt(stmt) => self.visit_statement(stmt),
        }
    }

    fn visit_class_declaration(&mut self, class_decl: &ClassDecl) -> Result<T, Self::Error> {
        self.set_span(class_decl.span);

        self.visit_identifier(&class_decl.name)?;

        if let Some(superclass) = &class_decl.superclass {
            self.visit_identifier(superclass)?;
        }

        for member in &class_decl.members {
            self.visit_class_member(member)?;
        }

        self.default_result()
    }

    fn visit_class_member(&mut self, member: &ClassMember) -> Result<T, Self::Error> {
        self.set_span(member.span());
        match member {
            ClassMember::Method(method) => self.visit_function_expression(method),
            ClassMember::Field(field) => self.visit_field_declaration(field),
        }
    }

    fn visit_field_declaration(&mut self, field_decl: &FieldDecl) -> Result<T, Self::Error> {
        self.set_span(field_decl.span);
        self.visit_identifier(&field_decl.name)?;

        if let Some(initializer) = &field_decl.initializer {
            self.visit_expression(initializer)?;
        }

        self.default_result()
    }

    fn visit_function_declaration(&mut self, func_decl: &FunctionDecl) -> Result<T, Self::Error> {
        self.set_span(func_decl.span);
        self.visit_function_expression(&func_decl.function)
    }

    fn visit_function_expression(&mut self, func_expr: &FunctionExpr) -> Result<T, Self::Error> {
        self.set_span(func_expr.span);
        self.visit_identifier(&func_expr.name)?;

        for param in &func_expr.parameters {
            self.visit_identifier(param)?;
        }

        self.visit_block_statement(&func_expr.body)
    }

    fn visit_lambda_declaration(&mut self, lambda_decl: &LambdaDecl) -> Result<T, Self::Error> {
        self.set_span(lambda_decl.span);
        self.visit_identifier(&lambda_decl.name)?;
        self.visit_lambda_expression(&lambda_decl.lambda)
    }

    fn visit_lambda_expression(&mut self, lambda_expr: &LambdaExpr) -> Result<T, Self::Error> {
        self.set_span(lambda_expr.span);
        for param in &lambda_expr.parameters {
            self.visit_identifier(param)?;
        }

        self.visit_lambda_body(&lambda_expr.body)
    }

    fn visit_lambda_body(&mut self, body: &LambdaBody) -> Result<T, Self::Error> {
        self.set_span(body.span());
        match body {
            LambdaBody::Block(block) => self.visit_block_statement(block),
            LambdaBody::Expr(expr) => self.visit_expression(expr),
        }
    }

    fn visit_variable_declaration(&mut self, var_decl: &VariableDecl) -> Result<T, Self::Error> {
        self.set_span(var_decl.span);
        self.visit_identifier(&var_decl.name)?;

        if let Some(initializer) = &var_decl.initializer {
            self.visit_expression(initializer)?;
        }

        self.default_result()
    }

    fn visit_statement(&mut self, stmt: &Stmt) -> Result<T, Self::Error> {
        self.set_span(stmt.span());
        match stmt {
            Stmt::Expr(expr_stmt) => self.visit_expression_statement(expr_stmt),
            Stmt::Block(block_stmt) => self.visit_block_statement(block_stmt),
            Stmt::If(if_stmt) => self.visit_if_statement(if_stmt),
            Stmt::While(while_stmt) => self.visit_while_statement(while_stmt),
            Stmt::For(for_stmt) => self.visit_for_statement(for_stmt),
            Stmt::Break(break_stmt) => self.visit_break_statement(break_stmt),
            Stmt::Continue(continue_stmt) => self.visit_continue_statement(continue_stmt),
            Stmt::Return(return_stmt) => self.visit_return_statement(return_stmt),
            Stmt::Throw(throw_stmt) => self.visit_throw_statement(throw_stmt),
            Stmt::Try(try_stmt) => self.visit_try_statement(try_stmt),
        }
    }

    fn visit_expression_statement(&mut self, expr_stmt: &ExprStmt) -> Result<T, Self::Error> {
        self.set_span(expr_stmt.span);
        self.visit_expression(&expr_stmt.expr)
    }

    fn visit_block_statement(&mut self, block_stmt: &BlockStmt) -> Result<T, Self::Error> {
        self.set_span(block_stmt.span);
        for decl in &block_stmt.decls {
            self.visit_declaration(decl)?;
        }
        self.default_result()
    }

    fn visit_if_statement(&mut self, if_stmt: &IfStmt) -> Result<T, Self::Error> {
        self.set_span(if_stmt.span);
        self.visit_expression(&if_stmt.condition)?;
        self.visit_statement(&if_stmt.then_branch)?;

        if let Some(else_branch) = &if_stmt.else_branch {
            self.visit_statement(else_branch)?;
        }

        self.default_result()
    }

    fn visit_while_statement(&mut self, while_stmt: &WhileStmt) -> Result<T, Self::Error> {
        self.set_span(while_stmt.span);
        self.visit_expression(&while_stmt.condition)?;
        self.visit_statement(&while_stmt.body)
    }

    fn visit_for_statement(&mut self, for_stmt: &ForStmt) -> Result<T, Self::Error> {
        self.set_span(for_stmt.span);
        if let Some(initializer) = &for_stmt.initializer {
            self.visit_for_initializer(initializer)?;
        }

        if let Some(condition) = &for_stmt.condition {
            self.visit_expression(condition)?;
        }

        if let Some(increment) = &for_stmt.increment {
            self.visit_expression(increment)?;
        }

        self.visit_statement(&for_stmt.body)
    }

    fn visit_for_initializer(&mut self, initializer: &ForInitializer) -> Result<T, Self::Error> {
        self.set_span(initializer.span());
        match initializer {
            ForInitializer::Variable(var_decl) => self.visit_variable_declaration(var_decl),
            ForInitializer::Expr(expr) => self.visit_expression(expr),
        }
    }

    fn visit_break_statement(&mut self, break_stmt: &BreakStmt) -> Result<T, Self::Error> {
        self.set_span(break_stmt.span);
        self.default_result()
    }

    fn visit_continue_statement(&mut self, continue_stmt: &ContinueStmt) -> Result<T, Self::Error> {
        self.set_span(continue_stmt.span);
        self.default_result()
    }

    fn visit_return_statement(&mut self, return_stmt: &ReturnStmt) -> Result<T, Self::Error> {
        self.set_span(return_stmt.span);
        if let Some(value) = &return_stmt.value {
            self.visit_expression(value)?;
        }
        self.default_result()
    }

    fn visit_throw_statement(&mut self, throw_stmt: &ThrowStmt) -> Result<T, Self::Error> {
        self.set_span(throw_stmt.span);
        if let Some(value) = &throw_stmt.value {
            self.visit_expression(value)?;
        }
        self.default_result()
    }

    fn visit_try_statement(&mut self, try_stmt: &TryStmt) -> Result<T, Self::Error> {
        self.set_span(try_stmt.span);
        self.visit_block_statement(&try_stmt.try_block)?;

        if let Some(catch_clause) = &try_stmt.catch_clause {
            self.visit_catch_clause(catch_clause)?;
        }

        if let Some(finally_block) = &try_stmt.finally_block {
            self.visit_block_statement(finally_block)?;
        }

        self.default_result()
    }

    fn visit_catch_clause(&mut self, catch_clause: &CatchClause) -> Result<T, Self::Error> {
        self.set_span(catch_clause.span);
        if let Some(parameter) = &catch_clause.parameter {
            self.visit_identifier(parameter)?;
        }

        self.visit_block_statement(&catch_clause.body)
    }

    fn visit_expression(&mut self, expr: &Expr) -> Result<T, Self::Error> {
        self.set_span(expr.span());
        match expr {
            Expr::Assignment(assignment) => self.visit_assignment_expression(assignment),
            Expr::Pipe(pipe) => self.visit_pipe_expression(pipe),
            Expr::Ternary(ternary) => self.visit_ternary_expression(ternary),
            Expr::LogicalOr(logical_or) => self.visit_logical_or_expression(logical_or),
            Expr::LogicalAnd(logical_and) => self.visit_logical_and_expression(logical_and),
            Expr::Equality(equality) => self.visit_equality_expression(equality),
            Expr::Comparison(comparison) => self.visit_comparison_expression(comparison),
            Expr::Term(term) => self.visit_term_expression(term),
            Expr::Factor(factor) => self.visit_factor_expression(factor),
            Expr::Unary(unary) => self.visit_unary_expression(unary),
            Expr::Call(call) => self.visit_call_expression(call),
            Expr::Primary(primary) => self.visit_primary_expression(primary),
        }
    }

    fn visit_assignment_expression(
        &mut self,
        assignment: &AssignmentExpr,
    ) -> Result<T, Self::Error> {
        self.set_span(assignment.span);
        self.visit_assignment_target(&assignment.target)?;
        self.visit_expression(&assignment.value)
    }

    fn visit_assignment_target(&mut self, target: &AssignmentTarget) -> Result<T, Self::Error> {
        self.set_span(target.span());
        match target {
            AssignmentTarget::Identifier(identifier) => self.visit_identifier(identifier),
            AssignmentTarget::Property(property) => self.visit_property_access(property),
        }
    }

    fn visit_property_access(&mut self, property: &PropertyAccess) -> Result<T, Self::Error> {
        self.set_span(property.span);
        self.visit_expression(&property.object)?;
        self.visit_identifier(&property.property)
    }

    fn visit_pipe_expression(&mut self, pipe: &PipeExpr) -> Result<T, Self::Error> {
        self.set_span(pipe.span);
        self.visit_expression(&pipe.left)?;

        if let Some(right) = &pipe.right {
            self.visit_expression(right)?;
        }

        self.default_result()
    }

    fn visit_ternary_expression(&mut self, ternary: &TernaryExpr) -> Result<T, Self::Error> {
        self.set_span(ternary.span);
        self.visit_expression(&ternary.condition)?;

        if let Some(then_expr) = &ternary.then_expr {
            self.visit_expression(then_expr)?;
        }

        if let Some(else_expr) = &ternary.else_expr {
            self.visit_expression(else_expr)?;
        }

        self.default_result()
    }

    fn visit_logical_or_expression(
        &mut self,
        logical_or: &LogicalOrExpr,
    ) -> Result<T, Self::Error> {
        self.set_span(logical_or.span);
        self.visit_expression(&logical_or.left)?;
        self.visit_expression(&logical_or.right)
    }

    fn visit_logical_and_expression(
        &mut self,
        logical_and: &LogicalAndExpr,
    ) -> Result<T, Self::Error> {
        self.set_span(logical_and.span);
        self.visit_expression(&logical_and.left)?;
        self.visit_expression(&logical_and.right)
    }

    fn visit_equality_expression(&mut self, equality: &EqualityExpr) -> Result<T, Self::Error> {
        self.set_span(equality.span);
        self.visit_expression(&equality.left)?;
        self.visit_expression(&equality.right)
    }

    fn visit_comparison_expression(
        &mut self,
        comparison: &ComparisonExpr,
    ) -> Result<T, Self::Error> {
        self.set_span(comparison.span);
        self.visit_expression(&comparison.left)?;
        self.visit_expression(&comparison.right)
    }

    fn visit_term_expression(&mut self, term: &TermExpr) -> Result<T, Self::Error> {
        self.set_span(term.span);
        self.visit_expression(&term.left)?;
        self.visit_expression(&term.right)
    }

    fn visit_factor_expression(&mut self, factor: &FactorExpr) -> Result<T, Self::Error> {
        self.set_span(factor.span);
        self.visit_expression(&factor.left)?;
        self.visit_expression(&factor.right)
    }

    fn visit_unary_expression(&mut self, unary: &UnaryExpr) -> Result<T, Self::Error> {
        self.set_span(unary.span);
        self.visit_expression(&unary.operand)
    }

    fn visit_call_expression(&mut self, call: &CallExpr) -> Result<T, Self::Error> {
        self.set_span(call.span);
        self.visit_expression(&call.callee)?;
        self.visit_call_operation(&call.operation)
    }

    fn visit_call_operation(&mut self, operation: &CallOperation) -> Result<T, Self::Error> {
        self.set_span(operation.span());
        match operation {
            CallOperation::Call(args) => {
                for arg in args {
                    self.visit_expression(arg)?;
                }
                self.default_result()
            }
            CallOperation::Property(identifier) => self.visit_identifier(identifier),
            CallOperation::OptionalProperty(identifier) => self.visit_identifier(identifier),
            CallOperation::Index(expr) => self.visit_expression(expr),
        }
    }

    fn visit_primary_expression(&mut self, primary: &PrimaryExpr) -> Result<T, Self::Error> {
        self.set_span(primary.span());
        match primary {
            PrimaryExpr::Number(number) => self.visit_number_literal(number),
            PrimaryExpr::String(string) => self.visit_string_literal(string),
            PrimaryExpr::Boolean(boolean) => self.visit_boolean_literal(boolean),
            PrimaryExpr::Nil(nil) => self.visit_nil_literal(nil),
            PrimaryExpr::This(this_expr) => self.visit_this_expression(this_expr),
            PrimaryExpr::Super(super_expr) => self.visit_super_expression(super_expr),
            PrimaryExpr::Identifier(identifier) => self.visit_identifier(identifier),
            PrimaryExpr::Grouping(grouping) => self.visit_grouping_expression(grouping),
            PrimaryExpr::Lambda(lambda) => self.visit_lambda_expression(lambda),
            PrimaryExpr::Array(array) => self.visit_array_literal(array),
        }
    }

    fn visit_number_literal(&mut self, number: &NumberLiteral) -> Result<T, Self::Error> {
        self.set_span(number.span);
        self.default_result()
    }

    fn visit_string_literal(&mut self, string: &StringLiteral) -> Result<T, Self::Error> {
        self.set_span(string.span);
        self.default_result()
    }

    fn visit_boolean_literal(&mut self, boolean: &BooleanLiteral) -> Result<T, Self::Error> {
        self.set_span(boolean.span);
        self.default_result()
    }

    fn visit_nil_literal(&mut self, nil: &NilLiteral) -> Result<T, Self::Error> {
        self.set_span(nil.span);
        self.default_result()
    }

    fn visit_this_expression(&mut self, this_expr: &ThisExpr) -> Result<T, Self::Error> {
        self.set_span(this_expr.span);
        self.default_result()
    }

    fn visit_super_expression(&mut self, super_expr: &SuperExpr) -> Result<T, Self::Error> {
        self.set_span(super_expr.span);
        self.default_result()
    }

    fn visit_identifier(&mut self, identifier: &Identifier) -> Result<T, Self::Error> {
        self.set_span(identifier.span);
        self.default_result()
    }

    fn visit_grouping_expression(&mut self, grouping: &GroupingExpr) -> Result<T, Self::Error> {
        self.set_span(grouping.span);
        self.visit_expression(&grouping.expr)
    }

    fn visit_array_literal(&mut self, array: &ArrayLiteral) -> Result<T, Self::Error> {
        self.set_span(array.span);
        for element in &array.elements {
            self.visit_expression(element)?;
        }
        self.default_result()
    }

    /// Default result to return when a visit method doesn't produce a specific value
    /// This should be implemented by each visitor
    fn default_result(&self) -> Result<T, Self::Error>;
}

/// A concrete implementation using QangError for errors
pub trait QangAstVisitor<T>: AstVisitor<T, Error = QangError> {}

/// Blanket implementation for any visitor that uses QangError
impl<T, V> QangAstVisitor<T> for V where V: AstVisitor<T, Error = QangError> {}

#[cfg(test)]
mod tests {
    use super::*;

    pub struct IdentifierCollector {
        pub identifiers: Vec<String>,
    }

    impl IdentifierCollector {
        pub fn new() -> Self {
            Self {
                identifiers: Vec::new(),
            }
        }
    }

    impl AstVisitor<()> for IdentifierCollector {
        type Error = QangError;

        fn visit_identifier(&mut self, identifier: &Identifier) -> Result<(), Self::Error> {
            self.identifiers.push(identifier.name.clone());
            self.default_result()
        }

        fn default_result(&self) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    #[test]
    fn test_identifier_collector() {
        let mut collector = IdentifierCollector::new();

        let identifier = Identifier {
            name: "test_var".to_string(),
            span: SourceSpan::new(0, 8),
        };

        collector.visit_identifier(&identifier).unwrap();

        assert_eq!(collector.identifiers.len(), 1);
        assert_eq!(collector.identifiers[0], "test_var");
    }

    #[test]
    fn test_expression_visitor() {
        let mut collector = IdentifierCollector::new();

        let expr = Expr::Primary(PrimaryExpr::Number(NumberLiteral {
            value: 42.0,
            span: SourceSpan::new(0, 2),
        }));

        collector.visit_expression(&expr).unwrap();

        // Should not collect any identifiers from a number literal
        assert_eq!(collector.identifiers.len(), 0);
    }

    pub struct AstValidator {
        pub errors: Vec<QangError>,
    }

    impl AstValidator {
        pub fn new() -> Self {
            Self { errors: Vec::new() }
        }

        pub fn has_errors(&self) -> bool {
            !self.errors.is_empty()
        }
    }

    impl AstVisitor<()> for AstValidator {
        type Error = QangError;

        fn visit_return_statement(&mut self, return_stmt: &ReturnStmt) -> Result<(), Self::Error> {
            // Example validation: check if return statement is in valid context
            // This would need additional context tracking in a real implementation

            if let Some(value) = &return_stmt.value {
                self.visit_expression(value)?;
            }

            self.default_result()
        }

        fn visit_break_statement(&mut self, _break_stmt: &BreakStmt) -> Result<(), Self::Error> {
            // Example validation: check if break is inside a loop
            // This would need loop context tracking in a real implementation

            self.default_result()
        }

        fn default_result(&self) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    #[test]
    fn test_validator() {
        let mut validator = AstValidator::new();

        let return_stmt = ReturnStmt {
            value: None,
            span: SourceSpan::new(0, 6),
        };

        validator.visit_return_statement(&return_stmt).unwrap();

        assert!(!validator.has_errors());
    }
}
