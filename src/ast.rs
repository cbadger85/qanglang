use crate::tokenizer::Token;

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
    pub fn span(&self) -> &SourceSpan {
        match self {
            ClassMember::Method(method) => &method.span,
            ClassMember::Field(field) => &field.span,
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
    pub fn span(&self) -> &SourceSpan {
        match self {
            AssignmentTarget::Identifier(id) => &id.span,
            AssignmentTarget::Property(prop) => &prop.span,
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
