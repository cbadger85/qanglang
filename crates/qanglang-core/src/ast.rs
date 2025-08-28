use crate::{ErrorReporter, tokenizer::Token};

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
    Stmt(Box<Stmt>),
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
    pub parameters: Vec<Parameter>,
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
    pub parameters: Vec<Parameter>,
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

/// Variable target for declarations  
#[derive(Debug, Clone, PartialEq)]
pub enum VariableTarget {
    Identifier(Identifier),
    Destructure(DestructurePattern),
}

impl VariableTarget {
    pub fn span(&self) -> SourceSpan {
        match self {
            VariableTarget::Identifier(id) => id.span,
            VariableTarget::Destructure(pattern) => pattern.span,
        }
    }
}

/// Variable declaration: var ( IDENTIFIER | destructurePattern ) ( = expression )? ;
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDecl {
    pub target: VariableTarget,
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
    For(Box<ForStmt>),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Return(ReturnStmt),
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

/// All possible expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assignment(AssignmentExpr),
    Pipe(PipeExpr),
    Map(MapCallExpr),
    OptionalMap(OptionalMapCallExpr),
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
            Expr::Map(expr) => expr.span,
            Expr::OptionalMap(expr) => expr.span,
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

/// Assignment expression: ( call . IDENTIFIER | IDENTIFIER ) ( = | += | -= | *= | /= | %= ) assignment | pipe
#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpr {
    pub target: AssignmentTarget,
    pub operator: AssignmentOperator,
    pub value: Box<Expr>,
    pub span: SourceSpan,
}

/// Assignment operators
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AssignmentOperator {
    Assign,         // =
    AddAssign,      // +=
    SubtractAssign, // -=
    MultiplyAssign, // *=
    DivideAssign,   // /=
    ModuloAssign,   // %=
}

/// Target of an assignment
#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentTarget {
    Identifier(Identifier),
    Property(PropertyAccess),
    Index(IndexAccess),
}

impl AssignmentTarget {
    pub fn span(&self) -> SourceSpan {
        match self {
            AssignmentTarget::Identifier(id) => id.span,
            AssignmentTarget::Property(prop) => prop.span,
            AssignmentTarget::Index(index) => index.span,
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

/// Index access for assignment: call [ expression ]
#[derive(Debug, Clone, PartialEq)]
pub struct IndexAccess {
    pub object: Box<Expr>,
    pub index: Box<Expr>,
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
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum EqualityOperator {
    Equal,    // ==
    NotEqual, // !=
    Is,       // is
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
    OptionalProperty(Identifier), // ?. IDENTIFIER
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
    ObjectLiteral(ObjectLiteral),
    Map(MapExpr),
    OptionalMap(OptionalMapExpr),
}

impl PrimaryExpr {
    pub fn span(&self) -> SourceSpan {
        match self {
            PrimaryExpr::Number(lit) => lit.span,
            PrimaryExpr::String(lit) => lit.span,
            PrimaryExpr::Boolean(lit) => lit.span,
            PrimaryExpr::Nil(lit) => lit.span,
            PrimaryExpr::This(expr) => expr.span,
            PrimaryExpr::Super(expr) => expr.span(),
            PrimaryExpr::Identifier(id) => id.span,
            PrimaryExpr::Grouping(expr) => expr.span,
            PrimaryExpr::Lambda(lambda) => lambda.span,
            PrimaryExpr::Array(array) => array.span,
            PrimaryExpr::ObjectLiteral(object) => object.span,
            PrimaryExpr::Map(map) => map.span,
            PrimaryExpr::OptionalMap(map) => map.span,
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
    pub value: Box<str>,
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

/// Super expression variants
#[derive(Debug, Clone, PartialEq)]
pub enum SuperExpr {
    /// "super" - bare super keyword (this will be an error)
    Bare(SuperBare),
    /// "super.method" - super method access
    Method(SuperMethod),
}

impl SuperExpr {
    fn span(&self) -> SourceSpan {
        match self {
            Self::Bare(bare_super) => bare_super.span,
            Self::Method(super_method) => super_method.span,
        }
    }
}

/// Bare super expression (invalid)
#[derive(Debug, Clone, PartialEq)]
pub struct SuperBare {
    pub span: SourceSpan,
}

/// Super method access: "super.method"
#[derive(Debug, Clone, PartialEq)]
pub struct SuperMethod {
    pub method: Identifier,
    pub span: SourceSpan,
}

/// Identifier
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: Box<str>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectLiteral {
    pub entries: Vec<ObjectEntry>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectEntry {
    pub key: Identifier,
    pub value: Box<Expr>,
    pub span: SourceSpan,
}

/// Parameters can be either identifiers or destructuring patterns
#[derive(Debug, Clone, PartialEq)]
pub enum Parameter {
    Identifier(Identifier),
    Destructure(DestructurePattern),
}

impl Parameter {
    pub fn span(&self) -> SourceSpan {
        match self {
            Parameter::Identifier(id) => id.span,
            Parameter::Destructure(pattern) => pattern.span,
        }
    }
}

/// Destructuring pattern: ( IDENTIFIER ( , IDENTIFIER )* ( , .. IDENTIFIER )? )
#[derive(Debug, Clone, PartialEq)]
pub struct DestructurePattern {
    pub elements: Vec<Identifier>,
    pub rest: Option<Identifier>,
    pub span: SourceSpan,
}

/// Map expression: || parameters? -> expression |
#[derive(Debug, Clone, PartialEq)]
pub struct MapExpr {
    pub parameters: Vec<Parameter>,
    pub body: Box<Expr>,
    pub span: SourceSpan,
}

/// Optional map expression: ?| parameters? -> expression |
#[derive(Debug, Clone, PartialEq)]
pub struct OptionalMapExpr {
    pub parameters: Vec<Parameter>,
    pub body: Box<Expr>,
    pub span: SourceSpan,
}

/// Map call expression: target||parameters? -> expression|
#[derive(Debug, Clone, PartialEq)]
pub struct MapCallExpr {
    pub target: Box<Expr>,
    pub parameters: Vec<Parameter>,
    pub body: Box<Expr>,
    pub span: SourceSpan,
}

/// Optional map call expression: target?|parameters? -> expression|
#[derive(Debug, Clone, PartialEq)]
pub struct OptionalMapCallExpr {
    pub target: Box<Expr>,
    pub parameters: Vec<Parameter>,
    pub body: Box<Expr>,
    pub span: SourceSpan,
}

/// Utility functions for working with the AST
impl Program {
    pub fn new(decls: Vec<Decl>, span: SourceSpan) -> Self {
        Self { decls, span }
    }
}

impl Identifier {
    pub fn new(name: Box<str>, span: SourceSpan) -> Self {
        Self { name, span }
    }
}

pub trait AstVisitor {
    type Error;

    fn visit_program(
        &mut self,
        program: &Program,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for decl in &program.decls {
            self.visit_declaration(decl, errors)?;
        }
        Ok(())
    }

    fn visit_declaration(
        &mut self,
        decl: &Decl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match decl {
            Decl::Class(class_decl) => self.visit_class_declaration(class_decl, errors),
            Decl::Function(func_decl) => self.visit_function_declaration(func_decl, errors),
            Decl::Lambda(lambda_decl) => self.visit_lambda_declaration(lambda_decl, errors),
            Decl::Variable(var_decl) => self.visit_variable_declaration(var_decl, errors),
            Decl::Stmt(stmt) => self.visit_statement(stmt, errors),
        }
    }

    fn visit_class_declaration(
        &mut self,
        class_decl: &ClassDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(&class_decl.name, errors)?;

        if let Some(superclass) = &class_decl.superclass {
            self.visit_identifier(superclass, errors)?;
        }

        for member in &class_decl.members {
            self.visit_class_member(member, errors)?;
        }

        Ok(())
    }

    fn visit_class_member(
        &mut self,
        member: &ClassMember,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match member {
            ClassMember::Method(method) => self.visit_function_expression(method, errors),
            ClassMember::Field(field) => self.visit_field_declaration(field, errors),
        }
    }

    fn visit_field_declaration(
        &mut self,
        field_decl: &FieldDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(&field_decl.name, errors)?;

        if let Some(initializer) = &field_decl.initializer {
            self.visit_expression(initializer, errors)?;
        }

        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        func_decl: &FunctionDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_function_expression(&func_decl.function, errors)
    }

    fn visit_function_expression(
        &mut self,
        func_expr: &FunctionExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(&func_expr.name, errors)?;

        for param in &func_expr.parameters {
            self.visit_parameter(param, errors)?;
        }

        self.visit_block_statement(&func_expr.body, errors)
    }

    fn visit_lambda_declaration(
        &mut self,
        lambda_decl: &LambdaDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(&lambda_decl.name, errors)?;
        self.visit_lambda_expression(&lambda_decl.lambda, errors)
    }

    fn visit_lambda_expression(
        &mut self,
        lambda_expr: &LambdaExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for param in &lambda_expr.parameters {
            self.visit_parameter(param, errors)?;
        }

        self.visit_lambda_body(&lambda_expr.body, errors)
    }

    fn visit_lambda_body(
        &mut self,
        body: &LambdaBody,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match body {
            LambdaBody::Block(block) => self.visit_block_statement(block, errors),
            LambdaBody::Expr(expr) => self.visit_expression(expr, errors),
        }
    }

    fn visit_variable_declaration(
        &mut self,
        var_decl: &VariableDecl,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_variable_target(&var_decl.target, errors)?;

        if let Some(initializer) = &var_decl.initializer {
            self.visit_expression(initializer, errors)?;
        }

        Ok(())
    }

    fn visit_statement(
        &mut self,
        stmt: &Stmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match stmt {
            Stmt::Expr(expr_stmt) => self.visit_expression_statement(expr_stmt, errors),
            Stmt::Block(block_stmt) => self.visit_block_statement(block_stmt, errors),
            Stmt::If(if_stmt) => self.visit_if_statement(if_stmt, errors),
            Stmt::While(while_stmt) => self.visit_while_statement(while_stmt, errors),
            Stmt::For(for_stmt) => self.visit_for_statement(for_stmt, errors),
            Stmt::Break(break_stmt) => self.visit_break_statement(break_stmt, errors),
            Stmt::Continue(continue_stmt) => self.visit_continue_statement(continue_stmt, errors),
            Stmt::Return(return_stmt) => self.visit_return_statement(return_stmt, errors),
        }
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &ExprStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&expr_stmt.expr, errors)
    }

    fn visit_block_statement(
        &mut self,
        block_stmt: &BlockStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for decl in &block_stmt.decls {
            self.visit_declaration(decl, errors)?;
        }
        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        if_stmt: &IfStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&if_stmt.condition, errors)?;
        self.visit_statement(&if_stmt.then_branch, errors)?;

        if let Some(else_branch) = &if_stmt.else_branch {
            self.visit_statement(else_branch, errors)?;
        }

        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: &WhileStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&while_stmt.condition, errors)?;
        self.visit_statement(&while_stmt.body, errors)
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: &ForStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if let Some(initializer) = &for_stmt.initializer {
            self.visit_for_initializer(initializer, errors)?;
        }

        if let Some(condition) = &for_stmt.condition {
            self.visit_expression(condition, errors)?;
        }

        if let Some(increment) = &for_stmt.increment {
            self.visit_expression(increment, errors)?;
        }

        self.visit_statement(&for_stmt.body, errors)
    }

    fn visit_for_initializer(
        &mut self,
        initializer: &ForInitializer,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match initializer {
            ForInitializer::Variable(var_decl) => self.visit_variable_declaration(var_decl, errors),
            ForInitializer::Expr(expr) => self.visit_expression(expr, errors),
        }
    }

    fn visit_break_statement(
        &mut self,
        _break_stmt: &BreakStmt,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_continue_statement(
        &mut self,
        _continue_stmt: &ContinueStmt,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &ReturnStmt,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        if let Some(value) = &return_stmt.value {
            self.visit_expression(value, errors)?;
        }
        Ok(())
    }

    fn visit_expression(
        &mut self,
        expr: &Expr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match expr {
            Expr::Assignment(assignment) => self.visit_assignment_expression(assignment, errors),
            Expr::Pipe(pipe) => self.visit_pipe_expression(pipe, errors),
            Expr::Map(map) => self.visit_map_call_expression(map, errors),
            Expr::OptionalMap(map) => self.visit_optional_map_call_expression(map, errors),
            Expr::Ternary(ternary) => self.visit_ternary_expression(ternary, errors),
            Expr::LogicalOr(logical_or) => self.visit_logical_or_expression(logical_or, errors),
            Expr::LogicalAnd(logical_and) => self.visit_logical_and_expression(logical_and, errors),
            Expr::Equality(equality) => self.visit_equality_expression(equality, errors),
            Expr::Comparison(comparison) => self.visit_comparison_expression(comparison, errors),
            Expr::Term(term) => self.visit_term_expression(term, errors),
            Expr::Factor(factor) => self.visit_factor_expression(factor, errors),
            Expr::Unary(unary) => self.visit_unary_expression(unary, errors),
            Expr::Call(call) => self.visit_call_expression(call, errors),
            Expr::Primary(primary) => self.visit_primary_expression(primary, errors),
        }
    }

    fn visit_assignment_expression(
        &mut self,
        assignment: &AssignmentExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_assignment_target(&assignment.target, errors)?;
        self.visit_expression(&assignment.value, errors)
    }

    fn visit_assignment_target(
        &mut self,
        target: &AssignmentTarget,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match target {
            AssignmentTarget::Identifier(identifier) => self.visit_identifier(identifier, errors),
            AssignmentTarget::Property(property) => self.visit_property_access(property, errors),
            AssignmentTarget::Index(index) => self.visit_index_access(index, errors),
        }
    }

    fn visit_property_access(
        &mut self,
        property: &PropertyAccess,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&property.object, errors)?;
        self.visit_identifier(&property.property, errors)
    }

    fn visit_index_access(
        &mut self,
        index: &IndexAccess,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&index.object, errors)?;
        self.visit_expression(&index.index, errors)
    }

    fn visit_pipe_expression(
        &mut self,
        pipe: &PipeExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&pipe.left, errors)?;

        if let Some(right) = &pipe.right {
            self.visit_expression(right, errors)?;
        }

        Ok(())
    }

    fn visit_ternary_expression(
        &mut self,
        ternary: &TernaryExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&ternary.condition, errors)?;

        if let Some(then_expr) = &ternary.then_expr {
            self.visit_expression(then_expr, errors)?;
        }

        if let Some(else_expr) = &ternary.else_expr {
            self.visit_expression(else_expr, errors)?;
        }

        Ok(())
    }

    fn visit_logical_or_expression(
        &mut self,
        logical_or: &LogicalOrExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&logical_or.left, errors)?;
        self.visit_expression(&logical_or.right, errors)
    }

    fn visit_logical_and_expression(
        &mut self,
        logical_and: &LogicalAndExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&logical_and.left, errors)?;
        self.visit_expression(&logical_and.right, errors)
    }

    fn visit_equality_expression(
        &mut self,
        equality: &EqualityExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&equality.left, errors)?;
        self.visit_expression(&equality.right, errors)
    }

    fn visit_comparison_expression(
        &mut self,
        comparison: &ComparisonExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&comparison.left, errors)?;
        self.visit_expression(&comparison.right, errors)
    }

    fn visit_term_expression(
        &mut self,
        term: &TermExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&term.left, errors)?;
        self.visit_expression(&term.right, errors)
    }

    fn visit_factor_expression(
        &mut self,
        factor: &FactorExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&factor.left, errors)?;
        self.visit_expression(&factor.right, errors)
    }

    fn visit_unary_expression(
        &mut self,
        unary: &UnaryExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&unary.operand, errors)
    }

    fn visit_call_expression(
        &mut self,
        call: &CallExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&call.callee, errors)?;
        self.visit_call_operation(&call.operation, errors)
    }

    fn visit_call_operation(
        &mut self,
        operation: &CallOperation,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match operation {
            CallOperation::Call(args) => {
                for arg in args {
                    self.visit_expression(arg, errors)?;
                }
                Ok(())
            }
            CallOperation::Property(identifier) => self.visit_identifier(identifier, errors),
            CallOperation::OptionalProperty(identifier) => {
                self.visit_identifier(identifier, errors)
            }
            CallOperation::Index(expr) => self.visit_expression(expr, errors),
        }
    }

    fn visit_primary_expression(
        &mut self,
        primary: &PrimaryExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match primary {
            PrimaryExpr::Number(number) => self.visit_number_literal(number, errors),
            PrimaryExpr::String(string) => self.visit_string_literal(string, errors),
            PrimaryExpr::Boolean(boolean) => self.visit_boolean_literal(boolean, errors),
            PrimaryExpr::Nil(nil) => self.visit_nil_literal(nil, errors),
            PrimaryExpr::This(this_expr) => self.visit_this_expression(this_expr, errors),
            PrimaryExpr::Super(super_expr) => self.visit_super_expression(super_expr, errors),
            PrimaryExpr::Identifier(identifier) => self.visit_identifier(identifier, errors),
            PrimaryExpr::Grouping(grouping) => self.visit_grouping_expression(grouping, errors),
            PrimaryExpr::Lambda(lambda) => self.visit_lambda_expression(lambda, errors),
            PrimaryExpr::Array(array) => self.visit_array_literal(array, errors),
            PrimaryExpr::ObjectLiteral(object) => self.visit_object_literal(object, errors),
            PrimaryExpr::Map(map) => self.visit_map_expression(map, errors),
            PrimaryExpr::OptionalMap(map) => self.visit_optional_map_expression(map, errors),
        }
    }

    fn visit_number_literal(
        &mut self,
        _number: &NumberLiteral,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_string_literal(
        &mut self,
        _string: &StringLiteral,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_boolean_literal(
        &mut self,
        _boolean: &BooleanLiteral,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_nil_literal(
        &mut self,
        _nil: &NilLiteral,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_this_expression(
        &mut self,
        _this_expr: &ThisExpr,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_super_expression(
        &mut self,
        super_expr: &SuperExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match super_expr {
            SuperExpr::Bare(bare) => self.visit_super_bare(bare, errors),
            SuperExpr::Method(method) => self.visit_super_method(method, errors),
        }
    }

    fn visit_super_bare(
        &mut self,
        _super_bare: &SuperBare,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_super_method(
        &mut self,
        super_method: &SuperMethod,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(&super_method.method, errors)
    }

    fn visit_identifier(
        &mut self,
        _identifier: &Identifier,
        _errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_grouping_expression(
        &mut self,
        grouping: &GroupingExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&grouping.expr, errors)
    }

    fn visit_array_literal(
        &mut self,
        array: &ArrayLiteral,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for element in &array.elements {
            self.visit_expression(element, errors)?;
        }
        Ok(())
    }

    fn visit_object_literal(
        &mut self,
        object: &ObjectLiteral,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for entry in &object.entries {
            self.visit_object_entry(entry, errors)?;
        }

        Ok(())
    }

    fn visit_object_entry(
        &mut self,
        entry: &ObjectEntry,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_identifier(&entry.key, errors)?;
        self.visit_expression(&entry.value, errors)?;

        Ok(())
    }

    fn visit_parameter(
        &mut self,
        parameter: &Parameter,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match parameter {
            Parameter::Identifier(identifier) => self.visit_identifier(identifier, errors),
            Parameter::Destructure(pattern) => self.visit_destructure_pattern(pattern, errors),
        }
    }

    fn visit_destructure_pattern(
        &mut self,
        pattern: &DestructurePattern,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for element in &pattern.elements {
            self.visit_identifier(element, errors)?;
        }

        if let Some(rest) = &pattern.rest {
            self.visit_identifier(rest, errors)?;
        }

        Ok(())
    }

    fn visit_variable_target(
        &mut self,
        target: &VariableTarget,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        match target {
            VariableTarget::Identifier(identifier) => self.visit_identifier(identifier, errors),
            VariableTarget::Destructure(pattern) => self.visit_destructure_pattern(pattern, errors),
        }
    }

    fn visit_map_expression(
        &mut self,
        map: &MapExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for parameter in &map.parameters {
            self.visit_parameter(parameter, errors)?;
        }

        self.visit_expression(&map.body, errors)
    }

    fn visit_optional_map_expression(
        &mut self,
        map: &OptionalMapExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        for parameter in &map.parameters {
            self.visit_parameter(parameter, errors)?;
        }

        self.visit_expression(&map.body, errors)
    }

    fn visit_map_call_expression(
        &mut self,
        map: &MapCallExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&map.target, errors)?;
        for parameter in &map.parameters {
            self.visit_parameter(parameter, errors)?;
        }
        self.visit_expression(&map.body, errors)
    }

    fn visit_optional_map_call_expression(
        &mut self,
        map: &OptionalMapCallExpr,
        errors: &mut ErrorReporter,
    ) -> Result<(), Self::Error> {
        self.visit_expression(&map.target, errors)?;
        for parameter in &map.parameters {
            self.visit_parameter(parameter, errors)?;
        }
        self.visit_expression(&map.body, errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub struct IdentifierCollector {
        pub identifiers: Vec<Box<str>>,
    }

    impl IdentifierCollector {
        pub fn new() -> Self {
            Self {
                identifiers: Vec::new(),
            }
        }
    }

    impl AstVisitor for IdentifierCollector {
        type Error = crate::QangSyntaxError;

        fn visit_identifier(
            &mut self,
            identifier: &Identifier,
            _errors: &mut ErrorReporter,
        ) -> Result<(), Self::Error> {
            self.identifiers.push(identifier.name.clone());
            Ok(())
        }
    }

    #[test]
    fn test_identifier_collector() {
        let mut collector = IdentifierCollector::new();

        let identifier = Identifier::new("test_var".into(), SourceSpan::new(0, 8));

        let mut errors = ErrorReporter::new();
        collector
            .visit_identifier(&identifier, &mut errors)
            .unwrap();

        assert_eq!(collector.identifiers.len(), 1);
        assert_eq!(collector.identifiers[0].as_ref(), "test_var");
    }

    #[test]
    fn test_expression_visitor() {
        let mut collector = IdentifierCollector::new();

        let expr = Expr::Primary(PrimaryExpr::Number(NumberLiteral {
            value: 42.0,
            span: SourceSpan::new(0, 2),
        }));

        let mut errors = ErrorReporter::new();

        collector.visit_expression(&expr, &mut errors).unwrap();

        // Should not collect any identifiers from a number literal
        assert_eq!(collector.identifiers.len(), 0);
    }

    pub struct AstValidator {
        pub errors: Vec<crate::QangSyntaxError>,
    }

    impl AstValidator {
        pub fn new() -> Self {
            Self { errors: Vec::new() }
        }

        pub fn has_errors(&self) -> bool {
            !self.errors.is_empty()
        }
    }

    impl AstVisitor for AstValidator {
        type Error = crate::QangSyntaxError;

        fn visit_return_statement(
            &mut self,
            return_stmt: &ReturnStmt,
            errors: &mut ErrorReporter,
        ) -> Result<(), Self::Error> {
            // Example validation: check if return statement is in valid context
            // This would need additional context tracking in a real implementation

            if let Some(value) = &return_stmt.value {
                self.visit_expression(value, errors)?;
            }

            Ok(())
        }

        fn visit_break_statement(
            &mut self,
            _break_stmt: &BreakStmt,
            _errors: &mut ErrorReporter,
        ) -> Result<(), Self::Error> {
            // Example validation: check if break is inside a loop
            // This would need loop context tracking in a real implementation

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

        let mut errors = ErrorReporter::new();
        validator
            .visit_return_statement(&return_stmt, &mut errors)
            .unwrap();

        assert!(!validator.has_errors());
    }
}
