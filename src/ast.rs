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
#[derive(Debug, Clone, PartialEq, Copy)]
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
#[derive(Debug, Clone, PartialEq, Copy)]
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
#[derive(Debug, Clone, PartialEq, Copy)]
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
#[derive(Debug, Clone, PartialEq, Copy)]
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

pub trait AstVisitor<T> {
    type Error;

    fn visit_program(&mut self, program: &Program) -> Result<T, Self::Error> {
        for decl in &program.decls {
            self.visit_declaration(decl)?;
        }
        self.default_result()
    }

    fn visit_declaration(&mut self, decl: &Decl) -> Result<T, Self::Error> {
        match decl {
            Decl::Class(class_decl) => self.visit_class_declaration(class_decl),
            Decl::Function(func_decl) => self.visit_function_declaration(func_decl),
            Decl::Lambda(lambda_decl) => self.visit_lambda_declaration(lambda_decl),
            Decl::Variable(var_decl) => self.visit_variable_declaration(var_decl),
            Decl::Stmt(stmt) => self.visit_statement(stmt),
        }
    }

    fn visit_class_declaration(&mut self, class_decl: &ClassDecl) -> Result<T, Self::Error> {
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
        match member {
            ClassMember::Method(method) => self.visit_function_expression(method),
            ClassMember::Field(field) => self.visit_field_declaration(field),
        }
    }

    fn visit_field_declaration(&mut self, field_decl: &FieldDecl) -> Result<T, Self::Error> {
        self.visit_identifier(&field_decl.name)?;

        if let Some(initializer) = &field_decl.initializer {
            self.visit_expression(initializer)?;
        }

        self.default_result()
    }

    fn visit_function_declaration(&mut self, func_decl: &FunctionDecl) -> Result<T, Self::Error> {
        self.visit_function_expression(&func_decl.function)
    }

    fn visit_function_expression(&mut self, func_expr: &FunctionExpr) -> Result<T, Self::Error> {
        self.visit_identifier(&func_expr.name)?;

        for param in &func_expr.parameters {
            self.visit_identifier(param)?;
        }

        self.visit_block_statement(&func_expr.body)
    }

    fn visit_lambda_declaration(&mut self, lambda_decl: &LambdaDecl) -> Result<T, Self::Error> {
        self.visit_identifier(&lambda_decl.name)?;
        self.visit_lambda_expression(&lambda_decl.lambda)
    }

    fn visit_lambda_expression(&mut self, lambda_expr: &LambdaExpr) -> Result<T, Self::Error> {
        for param in &lambda_expr.parameters {
            self.visit_identifier(param)?;
        }

        self.visit_lambda_body(&lambda_expr.body)
    }

    fn visit_lambda_body(&mut self, body: &LambdaBody) -> Result<T, Self::Error> {
        match body {
            LambdaBody::Block(block) => self.visit_block_statement(block),
            LambdaBody::Expr(expr) => self.visit_expression(expr),
        }
    }

    fn visit_variable_declaration(&mut self, var_decl: &VariableDecl) -> Result<T, Self::Error> {
        self.visit_identifier(&var_decl.name)?;

        if let Some(initializer) = &var_decl.initializer {
            self.visit_expression(initializer)?;
        }

        self.default_result()
    }

    fn visit_statement(&mut self, stmt: &Stmt) -> Result<T, Self::Error> {
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
        self.visit_expression(&expr_stmt.expr)
    }

    fn visit_block_statement(&mut self, block_stmt: &BlockStmt) -> Result<T, Self::Error> {
        for decl in &block_stmt.decls {
            self.visit_declaration(decl)?;
        }
        self.default_result()
    }

    fn visit_if_statement(&mut self, if_stmt: &IfStmt) -> Result<T, Self::Error> {
        self.visit_expression(&if_stmt.condition)?;
        self.visit_statement(&if_stmt.then_branch)?;

        if let Some(else_branch) = &if_stmt.else_branch {
            self.visit_statement(else_branch)?;
        }

        self.default_result()
    }

    fn visit_while_statement(&mut self, while_stmt: &WhileStmt) -> Result<T, Self::Error> {
        self.visit_expression(&while_stmt.condition)?;
        self.visit_statement(&while_stmt.body)
    }

    fn visit_for_statement(&mut self, for_stmt: &ForStmt) -> Result<T, Self::Error> {
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
        match initializer {
            ForInitializer::Variable(var_decl) => self.visit_variable_declaration(var_decl),
            ForInitializer::Expr(expr) => self.visit_expression(expr),
        }
    }

    fn visit_break_statement(&mut self, break_stmt: &BreakStmt) -> Result<T, Self::Error> {
        self.default_result()
    }

    fn visit_continue_statement(&mut self, continue_stmt: &ContinueStmt) -> Result<T, Self::Error> {
        self.default_result()
    }

    fn visit_return_statement(&mut self, return_stmt: &ReturnStmt) -> Result<T, Self::Error> {
        if let Some(value) = &return_stmt.value {
            self.visit_expression(value)?;
        }
        self.default_result()
    }

    fn visit_throw_statement(&mut self, throw_stmt: &ThrowStmt) -> Result<T, Self::Error> {
        if let Some(value) = &throw_stmt.value {
            self.visit_expression(value)?;
        }
        self.default_result()
    }

    fn visit_try_statement(&mut self, try_stmt: &TryStmt) -> Result<T, Self::Error> {
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
        if let Some(parameter) = &catch_clause.parameter {
            self.visit_identifier(parameter)?;
        }

        self.visit_block_statement(&catch_clause.body)
    }

    fn visit_expression(&mut self, expr: &Expr) -> Result<T, Self::Error> {
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
        self.visit_assignment_target(&assignment.target)?;
        self.visit_expression(&assignment.value)
    }

    fn visit_assignment_target(&mut self, target: &AssignmentTarget) -> Result<T, Self::Error> {
        match target {
            AssignmentTarget::Identifier(identifier) => self.visit_identifier(identifier),
            AssignmentTarget::Property(property) => self.visit_property_access(property),
        }
    }

    fn visit_property_access(&mut self, property: &PropertyAccess) -> Result<T, Self::Error> {
        self.visit_expression(&property.object)?;
        self.visit_identifier(&property.property)
    }

    fn visit_pipe_expression(&mut self, pipe: &PipeExpr) -> Result<T, Self::Error> {
        self.visit_expression(&pipe.left)?;

        if let Some(right) = &pipe.right {
            self.visit_expression(right)?;
        }

        self.default_result()
    }

    fn visit_ternary_expression(&mut self, ternary: &TernaryExpr) -> Result<T, Self::Error> {
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
        self.visit_expression(&logical_or.left)?;
        self.visit_expression(&logical_or.right)
    }

    fn visit_logical_and_expression(
        &mut self,
        logical_and: &LogicalAndExpr,
    ) -> Result<T, Self::Error> {
        self.visit_expression(&logical_and.left)?;
        self.visit_expression(&logical_and.right)
    }

    fn visit_equality_expression(&mut self, equality: &EqualityExpr) -> Result<T, Self::Error> {
        self.visit_expression(&equality.left)?;
        self.visit_expression(&equality.right)
    }

    fn visit_comparison_expression(
        &mut self,
        comparison: &ComparisonExpr,
    ) -> Result<T, Self::Error> {
        self.visit_expression(&comparison.left)?;
        self.visit_expression(&comparison.right)
    }

    fn visit_term_expression(&mut self, term: &TermExpr) -> Result<T, Self::Error> {
        self.visit_expression(&term.left)?;
        self.visit_expression(&term.right)
    }

    fn visit_factor_expression(&mut self, factor: &FactorExpr) -> Result<T, Self::Error> {
        self.visit_expression(&factor.left)?;
        self.visit_expression(&factor.right)
    }

    fn visit_unary_expression(&mut self, unary: &UnaryExpr) -> Result<T, Self::Error> {
        self.visit_expression(&unary.operand)
    }

    fn visit_call_expression(&mut self, call: &CallExpr) -> Result<T, Self::Error> {
        self.visit_expression(&call.callee)?;
        self.visit_call_operation(&call.operation)
    }

    fn visit_call_operation(&mut self, operation: &CallOperation) -> Result<T, Self::Error> {
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
        self.default_result()
    }

    fn visit_string_literal(&mut self, string: &StringLiteral) -> Result<T, Self::Error> {
        self.default_result()
    }

    fn visit_boolean_literal(&mut self, boolean: &BooleanLiteral) -> Result<T, Self::Error> {
        self.default_result()
    }

    fn visit_nil_literal(&mut self, nil: &NilLiteral) -> Result<T, Self::Error> {
        self.default_result()
    }

    fn visit_this_expression(&mut self, this_expr: &ThisExpr) -> Result<T, Self::Error> {
        self.default_result()
    }

    fn visit_super_expression(&mut self, super_expr: &SuperExpr) -> Result<T, Self::Error> {
        self.default_result()
    }

    fn visit_identifier(&mut self, identifier: &Identifier) -> Result<T, Self::Error> {
        self.default_result()
    }

    fn visit_grouping_expression(&mut self, grouping: &GroupingExpr) -> Result<T, Self::Error> {
        self.visit_expression(&grouping.expr)
    }

    fn visit_array_literal(&mut self, array: &ArrayLiteral) -> Result<T, Self::Error> {
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

/// A practical AST transformer that balances flexibility with simplicity
pub trait AstTransformer {
    type Error;

    fn transform_program(&mut self, program: Program) -> Result<Program, Self::Error> {
        let mut transformed_decls = Vec::new();
        for decl in program.decls {
            match self.transform_declaration(decl)? {
                DeclarationResult::Keep(decl) => transformed_decls.push(decl),
                DeclarationResult::Replace(new_decls) => transformed_decls.extend(new_decls),
                DeclarationResult::Remove => {} // Skip this declaration
            }
        }
        Ok(Program::new(transformed_decls, program.span))
    }

    fn transform_identifier(&mut self, identifier: Identifier) -> Result<Identifier, Self::Error> {
        Ok(identifier)
    }

    fn transform_block_statement(
        &mut self,
        block_stmt: BlockStmt,
    ) -> Result<BlockStmt, Self::Error> {
        let mut transformed_decls = Vec::new();
        for decl in block_stmt.decls {
            match self.transform_declaration(decl)? {
                DeclarationResult::Keep(decl) => transformed_decls.push(decl),
                DeclarationResult::Replace(new_decls) => transformed_decls.extend(new_decls),
                DeclarationResult::Remove => {}
            }
        }
        Ok(BlockStmt {
            decls: transformed_decls,
            span: block_stmt.span,
        })
    }

    fn transform_function_expression(
        &mut self,
        func_expr: FunctionExpr,
    ) -> Result<FunctionExpr, Self::Error> {
        let mut transformed_params = Vec::new();
        for param in func_expr.parameters {
            transformed_params.push(self.transform_identifier(param)?);
        }

        Ok(FunctionExpr {
            name: self.transform_identifier(func_expr.name)?,
            parameters: transformed_params,
            body: self.transform_block_statement(func_expr.body)?,
            span: func_expr.span,
        })
    }

    fn transform_declaration(&mut self, decl: Decl) -> Result<DeclarationResult, Self::Error> {
        match decl {
            Decl::Class(class_decl) => self.transform_class_declaration(class_decl),
            Decl::Function(func_decl) => self.transform_function_declaration(func_decl),
            Decl::Lambda(lambda_decl) => self.transform_lambda_declaration(lambda_decl),
            Decl::Variable(var_decl) => self.transform_variable_declaration(var_decl),
            Decl::Stmt(stmt) => {
                // Convert statement result to declaration result
                match self.transform_statement(stmt)? {
                    StatementResult::Keep(stmt) => Ok(DeclarationResult::Keep(Decl::Stmt(stmt))),
                    StatementResult::Replace(stmts) => {
                        let decls = stmts.into_iter().map(Decl::Stmt).collect();
                        Ok(DeclarationResult::Replace(decls))
                    }
                    StatementResult::Remove => Ok(DeclarationResult::Remove),
                }
            }
        }
    }

    fn transform_statement(&mut self, stmt: Stmt) -> Result<StatementResult, Self::Error> {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                let transformed_expr = self.transform_expression(expr_stmt.expr)?;
                Ok(StatementResult::Keep(Stmt::Expr(ExprStmt {
                    expr: transformed_expr,
                    span: expr_stmt.span,
                })))
            }
            Stmt::Block(block_stmt) => {
                let transformed_block = self.transform_block_statement(block_stmt)?;
                Ok(StatementResult::Keep(Stmt::Block(transformed_block)))
            }
            Stmt::If(if_stmt) => self.transform_if_statement(if_stmt),
            Stmt::While(while_stmt) => self.transform_while_statement(while_stmt),
            Stmt::For(for_stmt) => self.transform_for_statement(for_stmt),
            Stmt::Break(break_stmt) => Ok(StatementResult::Keep(Stmt::Break(break_stmt))),
            Stmt::Continue(continue_stmt) => {
                Ok(StatementResult::Keep(Stmt::Continue(continue_stmt)))
            }
            Stmt::Return(return_stmt) => {
                let transformed_value = match return_stmt.value {
                    Some(value) => Some(self.transform_expression(value)?),
                    None => None,
                };
                Ok(StatementResult::Keep(Stmt::Return(ReturnStmt {
                    value: transformed_value,
                    span: return_stmt.span,
                })))
            }
            Stmt::Throw(throw_stmt) => {
                let transformed_value = match throw_stmt.value {
                    Some(value) => Some(self.transform_expression(value)?),
                    None => None,
                };
                Ok(StatementResult::Keep(Stmt::Throw(ThrowStmt {
                    value: transformed_value,
                    span: throw_stmt.span,
                })))
            }
            Stmt::Try(try_stmt) => {
                let transformed_try = TryStmt {
                    try_block: self.transform_block_statement(try_stmt.try_block)?,
                    catch_clause: match try_stmt.catch_clause {
                        Some(catch) => Some(CatchClause {
                            parameter: match catch.parameter {
                                Some(param) => Some(self.transform_identifier(param)?),
                                None => None,
                            },
                            body: self.transform_block_statement(catch.body)?,
                            span: catch.span,
                        }),
                        None => None,
                    },
                    finally_block: match try_stmt.finally_block {
                        Some(finally) => Some(self.transform_block_statement(finally)?),
                        None => None,
                    },
                    span: try_stmt.span,
                };
                Ok(StatementResult::Keep(Stmt::Try(transformed_try)))
            }
        }
    }

    fn transform_expression(&mut self, expr: Expr) -> Result<Expr, Self::Error> {
        match expr {
            Expr::Assignment(assignment) => self.transform_assignment_expression(assignment),
            Expr::Pipe(pipe) => self.transform_pipe_expression(pipe),
            Expr::Ternary(ternary) => self.transform_ternary_expression(ternary),
            Expr::LogicalOr(logical_or) => self.transform_logical_or_expression(logical_or),
            Expr::LogicalAnd(logical_and) => self.transform_logical_and_expression(logical_and),
            Expr::Equality(equality) => self.transform_equality_expression(equality),
            Expr::Comparison(comparison) => self.transform_comparison_expression(comparison),
            Expr::Term(term) => self.transform_term_expression(term),
            Expr::Factor(factor) => self.transform_factor_expression(factor),
            Expr::Unary(unary) => self.transform_unary_expression(unary),
            Expr::Call(call) => self.transform_call_expression(call),
            Expr::Primary(primary) => self.transform_primary_expression(primary),
        }
    }

    fn transform_class_declaration(
        &mut self,
        class_decl: ClassDecl,
    ) -> Result<DeclarationResult, Self::Error> {
        let mut transformed_members = Vec::new();
        for member in class_decl.members {
            transformed_members.push(match member {
                ClassMember::Method(method) => {
                    ClassMember::Method(self.transform_function_expression(method)?)
                }
                ClassMember::Field(field) => ClassMember::Field(FieldDecl {
                    name: self.transform_identifier(field.name)?,
                    initializer: match field.initializer {
                        Some(init) => Some(self.transform_expression(init)?),
                        None => None,
                    },
                    span: field.span,
                }),
            });
        }

        Ok(DeclarationResult::Keep(Decl::Class(ClassDecl {
            name: self.transform_identifier(class_decl.name)?,
            superclass: match class_decl.superclass {
                Some(superclass) => Some(self.transform_identifier(superclass)?),
                None => None,
            },
            members: transformed_members,
            span: class_decl.span,
        })))
    }

    fn transform_function_declaration(
        &mut self,
        func_decl: FunctionDecl,
    ) -> Result<DeclarationResult, Self::Error> {
        Ok(DeclarationResult::Keep(Decl::Function(FunctionDecl {
            function: self.transform_function_expression(func_decl.function)?,
            span: func_decl.span,
        })))
    }

    fn transform_lambda_declaration(
        &mut self,
        lambda_decl: LambdaDecl,
    ) -> Result<DeclarationResult, Self::Error> {
        let transformed_lambda = LambdaExpr {
            parameters: {
                let mut params = Vec::new();
                for param in lambda_decl.lambda.parameters {
                    params.push(self.transform_identifier(param)?);
                }
                params
            },
            body: Box::new(match *lambda_decl.lambda.body {
                LambdaBody::Block(block) => {
                    LambdaBody::Block(self.transform_block_statement(block)?)
                }
                LambdaBody::Expr(expr) => {
                    LambdaBody::Expr(Box::new(self.transform_expression(*expr)?))
                }
            }),
            span: lambda_decl.lambda.span,
        };

        Ok(DeclarationResult::Keep(Decl::Lambda(LambdaDecl {
            name: self.transform_identifier(lambda_decl.name)?,
            lambda: transformed_lambda,
            span: lambda_decl.span,
        })))
    }

    fn transform_variable_declaration(
        &mut self,
        var_decl: VariableDecl,
    ) -> Result<DeclarationResult, Self::Error> {
        Ok(DeclarationResult::Keep(Decl::Variable(VariableDecl {
            name: self.transform_identifier(var_decl.name)?,
            initializer: match var_decl.initializer {
                Some(init) => Some(self.transform_expression(init)?),
                None => None,
            },
            span: var_decl.span,
        })))
    }

    fn transform_if_statement(&mut self, if_stmt: IfStmt) -> Result<StatementResult, Self::Error> {
        let condition = self.transform_expression(if_stmt.condition)?;
        let then_branch = Box::new(self.transform_statement_to_single(*if_stmt.then_branch)?);
        let else_branch = match if_stmt.else_branch {
            Some(else_stmt) => Some(Box::new(self.transform_statement_to_single(*else_stmt)?)),
            None => None,
        };

        Ok(StatementResult::Keep(Stmt::If(IfStmt {
            condition,
            then_branch,
            else_branch,
            span: if_stmt.span,
        })))
    }

    fn transform_while_statement(
        &mut self,
        while_stmt: WhileStmt,
    ) -> Result<StatementResult, Self::Error> {
        let condition = self.transform_expression(while_stmt.condition)?;
        let body = Box::new(self.transform_statement_to_single(*while_stmt.body)?);

        Ok(StatementResult::Keep(Stmt::While(WhileStmt {
            condition,
            body,
            span: while_stmt.span,
        })))
    }

    fn transform_for_statement(
        &mut self,
        for_stmt: ForStmt,
    ) -> Result<StatementResult, Self::Error> {
        let initializer = match for_stmt.initializer {
            Some(ForInitializer::Variable(var_decl)) => {
                match self.transform_variable_declaration(var_decl)? {
                    DeclarationResult::Keep(Decl::Variable(var_decl)) => {
                        Some(ForInitializer::Variable(var_decl))
                    }
                    _ => None,
                }
            }
            Some(ForInitializer::Expr(expr)) => {
                Some(ForInitializer::Expr(self.transform_expression(expr)?))
            }
            None => None,
        };

        let condition = match for_stmt.condition {
            Some(cond) => Some(self.transform_expression(cond)?),
            None => None,
        };

        let increment = match for_stmt.increment {
            Some(inc) => Some(self.transform_expression(inc)?),
            None => None,
        };

        let body = Box::new(self.transform_statement_to_single(*for_stmt.body)?);

        Ok(StatementResult::Keep(Stmt::For(ForStmt {
            initializer,
            condition,
            increment,
            body,
            span: for_stmt.span,
        })))
    }

    fn transform_pipe_expression(&mut self, pipe: PipeExpr) -> Result<Expr, Self::Error> {
        let left = self.transform_expression(*pipe.left)?;
        let right = match pipe.right {
            Some(right) => Some(Box::new(self.transform_expression(*right)?)),
            None => None,
        };

        Ok(Expr::Pipe(PipeExpr {
            left: Box::new(left),
            right,
            span: pipe.span,
        }))
    }

    /// Transform ternary expressions - can become any expression
    fn transform_ternary_expression(&mut self, ternary: TernaryExpr) -> Result<Expr, Self::Error> {
        Ok(Expr::Ternary(TernaryExpr {
            condition: Box::new(self.transform_expression(*ternary.condition)?),
            then_expr: match ternary.then_expr {
                Some(then_expr) => Some(Box::new(self.transform_expression(*then_expr)?)),
                None => None,
            },
            else_expr: match ternary.else_expr {
                Some(else_expr) => Some(Box::new(self.transform_expression(*else_expr)?)),
                None => None,
            },
            span: ternary.span,
        }))
    }

    // ===== HELPER METHODS =====

    /// Helper to convert StatementResult to a single statement (wraps multiple in block)
    fn transform_statement_to_single(&mut self, stmt: Stmt) -> Result<Stmt, Self::Error> {
        match self.transform_statement(stmt)? {
            StatementResult::Keep(stmt) => Ok(stmt),
            StatementResult::Replace(stmts) => {
                // Wrap multiple statements in a block
                Ok(Stmt::Block(BlockStmt {
                    decls: stmts.into_iter().map(Decl::Stmt).collect(),
                    span: SourceSpan::default(), // You might want better span handling
                }))
            }
            StatementResult::Remove => {
                // Return empty block for removed statements
                Ok(Stmt::Block(BlockStmt {
                    decls: Vec::new(),
                    span: SourceSpan::default(),
                }))
            }
        }
    }

    // ===== DEFAULT IMPLEMENTATIONS FOR COMMON EXPRESSIONS =====

    fn transform_assignment_expression(
        &mut self,
        assignment: AssignmentExpr,
    ) -> Result<Expr, Self::Error> {
        let target = match assignment.target {
            AssignmentTarget::Identifier(id) => {
                AssignmentTarget::Identifier(self.transform_identifier(id)?)
            }
            AssignmentTarget::Property(prop) => AssignmentTarget::Property(PropertyAccess {
                object: Box::new(self.transform_expression(*prop.object)?),
                property: self.transform_identifier(prop.property)?,
                span: prop.span,
            }),
        };

        Ok(Expr::Assignment(AssignmentExpr {
            target,
            value: Box::new(self.transform_expression(*assignment.value)?),
            span: assignment.span,
        }))
    }

    fn transform_logical_or_expression(
        &mut self,
        logical_or: LogicalOrExpr,
    ) -> Result<Expr, Self::Error> {
        Ok(Expr::LogicalOr(LogicalOrExpr {
            left: Box::new(self.transform_expression(*logical_or.left)?),
            right: Box::new(self.transform_expression(*logical_or.right)?),
            span: logical_or.span,
        }))
    }

    fn transform_logical_and_expression(
        &mut self,
        logical_and: LogicalAndExpr,
    ) -> Result<Expr, Self::Error> {
        Ok(Expr::LogicalAnd(LogicalAndExpr {
            left: Box::new(self.transform_expression(*logical_and.left)?),
            right: Box::new(self.transform_expression(*logical_and.right)?),
            span: logical_and.span,
        }))
    }

    fn transform_equality_expression(
        &mut self,
        equality: EqualityExpr,
    ) -> Result<Expr, Self::Error> {
        Ok(Expr::Equality(EqualityExpr {
            left: Box::new(self.transform_expression(*equality.left)?),
            operator: equality.operator,
            right: Box::new(self.transform_expression(*equality.right)?),
            span: equality.span,
        }))
    }

    fn transform_comparison_expression(
        &mut self,
        comparison: ComparisonExpr,
    ) -> Result<Expr, Self::Error> {
        Ok(Expr::Comparison(ComparisonExpr {
            left: Box::new(self.transform_expression(*comparison.left)?),
            operator: comparison.operator,
            right: Box::new(self.transform_expression(*comparison.right)?),
            span: comparison.span,
        }))
    }

    fn transform_term_expression(&mut self, term: TermExpr) -> Result<Expr, Self::Error> {
        Ok(Expr::Term(TermExpr {
            left: Box::new(self.transform_expression(*term.left)?),
            operator: term.operator,
            right: Box::new(self.transform_expression(*term.right)?),
            span: term.span,
        }))
    }

    fn transform_factor_expression(&mut self, factor: FactorExpr) -> Result<Expr, Self::Error> {
        Ok(Expr::Factor(FactorExpr {
            left: Box::new(self.transform_expression(*factor.left)?),
            operator: factor.operator,
            right: Box::new(self.transform_expression(*factor.right)?),
            span: factor.span,
        }))
    }

    fn transform_unary_expression(&mut self, unary: UnaryExpr) -> Result<Expr, Self::Error> {
        Ok(Expr::Unary(UnaryExpr {
            operator: unary.operator,
            operand: Box::new(self.transform_expression(*unary.operand)?),
            span: unary.span,
        }))
    }

    fn transform_call_expression(&mut self, call: CallExpr) -> Result<Expr, Self::Error> {
        let operation = match *call.operation {
            CallOperation::Call(args) => {
                let mut transformed_args = Vec::new();
                for arg in args {
                    transformed_args.push(self.transform_expression(arg)?);
                }
                CallOperation::Call(transformed_args)
            }
            CallOperation::Property(id) => CallOperation::Property(self.transform_identifier(id)?),
            CallOperation::OptionalProperty(id) => {
                CallOperation::OptionalProperty(self.transform_identifier(id)?)
            }
            CallOperation::Index(expr) => CallOperation::Index(self.transform_expression(expr)?),
        };

        Ok(Expr::Call(CallExpr {
            callee: Box::new(self.transform_expression(*call.callee)?),
            operation: Box::new(operation),
            span: call.span,
        }))
    }

    fn transform_primary_expression(&mut self, primary: PrimaryExpr) -> Result<Expr, Self::Error> {
        match primary {
            PrimaryExpr::Number(num) => Ok(Expr::Primary(PrimaryExpr::Number(num))),
            PrimaryExpr::String(str) => Ok(Expr::Primary(PrimaryExpr::String(str))),
            PrimaryExpr::Boolean(bool) => Ok(Expr::Primary(PrimaryExpr::Boolean(bool))),
            PrimaryExpr::Nil(nil) => Ok(Expr::Primary(PrimaryExpr::Nil(nil))),
            PrimaryExpr::This(this) => Ok(Expr::Primary(PrimaryExpr::This(this))),
            PrimaryExpr::Super(sup) => Ok(Expr::Primary(PrimaryExpr::Super(sup))),
            PrimaryExpr::Identifier(id) => Ok(Expr::Primary(PrimaryExpr::Identifier(
                self.transform_identifier(id)?,
            ))),
            PrimaryExpr::Grouping(group) => {
                Ok(Expr::Primary(PrimaryExpr::Grouping(GroupingExpr {
                    expr: Box::new(self.transform_expression(*group.expr)?),
                    span: group.span,
                })))
            }
            PrimaryExpr::Lambda(lambda) => {
                let transformed_lambda = LambdaExpr {
                    parameters: {
                        let mut params = Vec::new();
                        for param in lambda.parameters {
                            params.push(self.transform_identifier(param)?);
                        }
                        params
                    },
                    body: Box::new(match *lambda.body {
                        LambdaBody::Block(block) => {
                            LambdaBody::Block(self.transform_block_statement(block)?)
                        }
                        LambdaBody::Expr(expr) => {
                            LambdaBody::Expr(Box::new(self.transform_expression(*expr)?))
                        }
                    }),
                    span: lambda.span,
                };
                Ok(Expr::Primary(PrimaryExpr::Lambda(Box::new(
                    transformed_lambda,
                ))))
            }
            PrimaryExpr::Array(array) => {
                let mut transformed_elements = Vec::new();
                for element in array.elements {
                    transformed_elements.push(self.transform_expression(element)?);
                }
                Ok(Expr::Primary(PrimaryExpr::Array(ArrayLiteral {
                    elements: transformed_elements,
                    span: array.span,
                })))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclarationResult {
    Keep(Decl),         // Keep the declaration as-is (after transformation)
    Replace(Vec<Decl>), // Replace with multiple declarations
    Remove,             // Remove the declaration entirely
}

#[derive(Debug, Clone)]
pub enum StatementResult {
    Keep(Stmt),         // Keep the statement as-is (after transformation)
    Replace(Vec<Stmt>), // Replace with multiple statements
    Remove,             // Remove the statement entirely
}

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
