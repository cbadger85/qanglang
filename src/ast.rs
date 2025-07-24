use crate::tokenizer::Token;

/// Represents a position in the source code for error reporting and debugging
#[derive(Debug, Clone, PartialEq, Default)]
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

    pub fn combine(start: &SourceSpan, end: &SourceSpan) -> Self {
        Self {
            start: start.start,
            end: end.end,
        }
    }
}

/// Root AST node representing a complete program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
    pub span: SourceSpan,
}

/// Top-level declarations in the program
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Class(ClassDeclaration),
    Function(FunctionDeclaration),
    Lambda(LambdaDeclaration),
    Variable(VariableDeclaration),
    Statement(Statement),
}

impl Declaration {
    pub fn span(&self) -> &SourceSpan {
        match self {
            Declaration::Class(decl) => &decl.span,
            Declaration::Function(decl) => &decl.span,
            Declaration::Lambda(decl) => &decl.span,
            Declaration::Variable(decl) => &decl.span,
            Declaration::Statement(stmt) => stmt.span(),
        }
    }
}

/// Class declaration: class IDENTIFIER ( : IDENTIFIER )? { classMember* }
#[derive(Debug, Clone, PartialEq)]
pub struct ClassDeclaration {
    pub name: Identifier,
    pub superclass: Option<Identifier>,
    pub members: Vec<ClassMember>,
    pub span: SourceSpan,
}

/// Members that can appear inside a class
#[derive(Debug, Clone, PartialEq)]
pub enum ClassMember {
    Method(FunctionExpression),
    Field(FieldDeclaration),
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
pub struct FieldDeclaration {
    pub name: Identifier,
    pub initializer: Option<Expression>,
    pub span: SourceSpan,
}

/// Function declaration: fn function
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub function: FunctionExpression,
    pub span: SourceSpan,
}

/// Function expression: IDENTIFIER ( parameters? ) block
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExpression {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub span: SourceSpan,
}

/// Lambda declaration: var IDENTIFIER = lambda
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaDeclaration {
    pub name: Identifier,
    pub lambda: LambdaExpression,
    pub span: SourceSpan,
}

/// Lambda expression: ( parameters? ) -> ( block | expression )
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaExpression {
    pub parameters: Vec<Identifier>,
    pub body: Box<LambdaBody>,
    pub span: SourceSpan,
}

/// Body of a lambda can be either a block or a single expression
#[derive(Debug, Clone, PartialEq)]
pub enum LambdaBody {
    Block(BlockStatement),
    Expression(Box<Expression>),
}

impl LambdaBody {
    pub fn span(&self) -> &SourceSpan {
        match self {
            LambdaBody::Block(block) => &block.span,
            LambdaBody::Expression(expr) => expr.span(),
        }
    }
}

/// Variable declaration: var IDENTIFIER ( = expression )? ;
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub initializer: Option<Expression>,
    pub span: SourceSpan,
}

/// All possible statements
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(ExpressionStatement),
    Block(BlockStatement),
    If(IfStatement),
    While(WhileStatement),
    For(ForStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    Return(ReturnStatement),
    Throw(ThrowStatement),
    Try(TryStatement),
}

impl Statement {
    pub fn span(&self) -> &SourceSpan {
        match self {
            Statement::Expression(stmt) => &stmt.span,
            Statement::Block(stmt) => &stmt.span,
            Statement::If(stmt) => &stmt.span,
            Statement::While(stmt) => &stmt.span,
            Statement::For(stmt) => &stmt.span,
            Statement::Break(stmt) => &stmt.span,
            Statement::Continue(stmt) => &stmt.span,
            Statement::Return(stmt) => &stmt.span,
            Statement::Throw(stmt) => &stmt.span,
            Statement::Try(stmt) => &stmt.span,
        }
    }
}

/// Expression statement: expression ;
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub span: SourceSpan,
}

/// Block statement: { declaration* }
#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub declarations: Vec<Declaration>,
    pub span: SourceSpan,
}

/// If statement: if ( expression ) statement ( else statement )?
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
    pub span: SourceSpan,
}

/// While statement: while ( expression ) statement
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
    pub span: SourceSpan,
}

/// For statement: for ( ( varDecl | exprStmt | ; ) expression? ; expression? ) statement
#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub initializer: Option<ForInitializer>,
    pub condition: Option<Expression>,
    pub increment: Option<Expression>,
    pub body: Box<Statement>,
    pub span: SourceSpan,
}

/// Initializer for a for loop
#[derive(Debug, Clone, PartialEq)]
pub enum ForInitializer {
    Variable(VariableDeclaration),
    Expression(Expression),
}

impl ForInitializer {
    pub fn span(&self) -> &SourceSpan {
        match self {
            ForInitializer::Variable(var) => &var.span,
            ForInitializer::Expression(expr) => expr.span(),
        }
    }
}

/// Break statement: break ;
#[derive(Debug, Clone, PartialEq)]
pub struct BreakStatement {
    pub span: SourceSpan,
}

/// Continue statement: continue ;
#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStatement {
    pub span: SourceSpan,
}

/// Return statement: return expression? ;
#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
    pub span: SourceSpan,
}

/// Throw statement: throw expression? ;
#[derive(Debug, Clone, PartialEq)]
pub struct ThrowStatement {
    pub value: Option<Expression>,
    pub span: SourceSpan,
}

/// Try statement: try block catchFinally
#[derive(Debug, Clone, PartialEq)]
pub struct TryStatement {
    pub try_block: BlockStatement,
    pub catch_clause: Option<CatchClause>,
    pub finally_block: Option<BlockStatement>,
    pub span: SourceSpan,
}

/// Catch clause: catch ( IDENTIFIER )? block
#[derive(Debug, Clone, PartialEq)]
pub struct CatchClause {
    pub parameter: Option<Identifier>,
    pub body: BlockStatement,
    pub span: SourceSpan,
}

/// All possible expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Assignment(AssignmentExpression),
    Pipe(PipeExpression),
    Ternary(TernaryExpression),
    LogicalOr(LogicalOrExpression),
    LogicalAnd(LogicalAndExpression),
    Equality(EqualityExpression),
    Comparison(ComparisonExpression),
    Term(TermExpression),
    Factor(FactorExpression),
    Unary(UnaryExpression),
    Call(CallExpression),
    Primary(PrimaryExpression),
}

impl Expression {
    pub fn span(&self) -> &SourceSpan {
        match self {
            Expression::Assignment(expr) => &expr.span,
            Expression::Pipe(expr) => &expr.span,
            Expression::Ternary(expr) => &expr.span,
            Expression::LogicalOr(expr) => &expr.span,
            Expression::LogicalAnd(expr) => &expr.span,
            Expression::Equality(expr) => &expr.span,
            Expression::Comparison(expr) => &expr.span,
            Expression::Term(expr) => &expr.span,
            Expression::Factor(expr) => &expr.span,
            Expression::Unary(expr) => &expr.span,
            Expression::Call(expr) => &expr.span,
            Expression::Primary(expr) => expr.span(),
        }
    }
}

/// Assignment expression: ( call . IDENTIFIER | IDENTIFIER ) = assignment | pipe
#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub target: AssignmentTarget,
    pub value: Box<Expression>,
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
    pub object: Box<Expression>,
    pub property: Identifier,
    pub span: SourceSpan,
}

/// Pipe expression: ternary ( |> pipe )?
#[derive(Debug, Clone, PartialEq)]
pub struct PipeExpression {
    pub left: Box<Expression>,
    pub right: Option<Box<Expression>>,
    pub span: SourceSpan,
}

/// Ternary expression: logicOr ( ? expression : ternary )?
#[derive(Debug, Clone, PartialEq)]
pub struct TernaryExpression {
    pub condition: Box<Expression>,
    pub then_expr: Option<Box<Expression>>,
    pub else_expr: Option<Box<Expression>>,
    pub span: SourceSpan,
}

/// Logical OR expression: logicAnd ( or logicAnd )*
#[derive(Debug, Clone, PartialEq)]
pub struct LogicalOrExpression {
    pub left: Box<Expression>,
    pub right: Vec<Expression>,
    pub span: SourceSpan,
}

/// Logical AND expression: equality ( and equality )*
#[derive(Debug, Clone, PartialEq)]
pub struct LogicalAndExpression {
    pub left: Box<Expression>,
    pub right: Vec<Expression>,
    pub span: SourceSpan,
}

/// Equality expression: comparison ( ( != | == ) comparison )*
#[derive(Debug, Clone, PartialEq)]
pub struct EqualityExpression {
    pub left: Box<Expression>,
    pub operations: Vec<(EqualityOperator, Expression)>,
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
pub struct ComparisonExpression {
    pub left: Box<Expression>,
    pub operations: Vec<(ComparisonOperator, Expression)>,
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
pub struct TermExpression {
    pub left: Box<Expression>,
    pub operations: Vec<(TermOperator, Expression)>,
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
pub struct FactorExpression {
    pub left: Box<Expression>,
    pub operations: Vec<(FactorOperator, Expression)>,
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
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
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
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub operations: Vec<CallOperation>,
    pub span: SourceSpan,
}

/// Operations that can be chained on a call expression
#[derive(Debug, Clone, PartialEq)]
pub enum CallOperation {
    Call(Vec<Expression>),        // ( arguments? )
    Property(Identifier),         // . IDENTIFIER
    OptionalProperty(Identifier), // .? IDENTIFIER
    Index(Expression),            // [ expression ]
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
            CallOperation::Property(id) => id.span.clone(),
            CallOperation::OptionalProperty(id) => id.span.clone(),
            CallOperation::Index(expr) => expr.span().clone(),
        }
    }
}

/// Primary expressions
#[derive(Debug, Clone, PartialEq)]
pub enum PrimaryExpression {
    Number(NumberLiteral),
    String(StringLiteral),
    Boolean(BooleanLiteral),
    Nil(NilLiteral),
    This(ThisExpression),
    Super(SuperExpression),
    Identifier(Identifier),
    Grouping(GroupingExpression),
    Lambda(Box<LambdaExpression>),
    Array(ArrayLiteral),
}

impl PrimaryExpression {
    pub fn span(&self) -> &SourceSpan {
        match self {
            PrimaryExpression::Number(lit) => &lit.span,
            PrimaryExpression::String(lit) => &lit.span,
            PrimaryExpression::Boolean(lit) => &lit.span,
            PrimaryExpression::Nil(lit) => &lit.span,
            PrimaryExpression::This(expr) => &expr.span,
            PrimaryExpression::Super(expr) => &expr.span(),
            PrimaryExpression::Identifier(id) => &id.span,
            PrimaryExpression::Grouping(expr) => &expr.span,
            PrimaryExpression::Lambda(lambda) => &lambda.span,
            PrimaryExpression::Array(array) => &array.span,
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
pub struct ThisExpression {
    pub span: SourceSpan,
}

/// Super expression: "super" | "super" "." IDENTIFIER
#[derive(Debug, Clone, PartialEq)]
pub enum SuperExpression {
    /// Just "super" (for constructor calls)
    Constructor(SuperConstructor),
    /// "super.method" (for method calls)  
    Method(SuperMethod),
}

impl SuperExpression {
    pub fn span(&self) -> &SourceSpan {
        match self {
            SuperExpression::Constructor(sup) => &sup.span,
            SuperExpression::Method(sup) => &sup.span,
        }
    }
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
pub struct GroupingExpression {
    pub expression: Box<Expression>,
    pub span: SourceSpan,
}

/// Array literal: [ ( expression ( , expression )* )? ]
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
    pub span: SourceSpan,
}

/// Utility functions for working with the AST
impl Program {
    pub fn new(declarations: Vec<Declaration>, span: SourceSpan) -> Self {
        Self { declarations, span }
    }
}

impl Identifier {
    pub fn new(name: String, span: SourceSpan) -> Self {
        Self { name, span }
    }
}
