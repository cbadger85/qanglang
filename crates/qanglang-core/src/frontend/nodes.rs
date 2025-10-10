use crate::{
    StringHandle, Token,
    frontend::{ast_node_arena::NodeId, node_array_arena::NodeArrayId},
};
use std::convert::TryFrom;

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

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ComparisonOperator {
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum EqualityOperator {
    Equal,    // ==
    NotEqual, // !=
    Is,       // is
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum FactorOperator {
    Divide,   // /
    Multiply, // *
    Modulo,   // %
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum TermOperator {
    Add,      // +
    Subtract, // -
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UnaryOperator {
    Minus, // -
    Not,   // !
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AstNode {
    Module(Module),
    Class(ClassDeclNode),
    Function(FunctionDeclNode),
    Identifier(IdentifierNode),
    FieldDecl(FieldDeclNode),
    FunctionDecl(FunctionDeclNode),
    FunctionExpr(FunctionExprNode),
    NumberLiteral(NumberLiteralNode),
    StringLiteral(StringLiteralNode),
    BooleanLiteral(BooleanLiteralNode),
    NilLiteral(NilLiteralNode),
    ThisExpr(ThisExprNode),
    SuperExpr(SuperExprNode),
    AssignmentExpr(AssignmentExprNode),
    PipeExpr(PipeExprNode),
    TernaryExpr(TernaryExprNode),
    LogicalOrExpr(LogicalOrExprNode),
    LogicalAndExpr(LogicalAndExprNode),
    EqualityExpr(EqualityExprNode),
    ComparisonExpr(ComparisonExprNode),
    TermExpr(TermExprNode),
    FactorExpr(FactorExprNode),
    UnaryExpr(UnaryExprNode),
    CallExpr(CallExprNode),
    CallOperation(CallNode),
    PropertyAccess(PropertyNode),
    PropertyAssignment(PropertyAssignmentNode),
    OptionalPropertyAccess(OptionalPropertyNode),
    IndexAccess(IndexNode),
    IndexAssignment(IndexAssignmentNode),
    ArrayLiteralExpr(ArrayLiteralExprNode),
    ObjectLiteralExpr(ObjectLiteralExprNode),
    ObjectEntry(ObjectEntryNode),
    MapExpr(MapExprNode),
    OptionalMapExpr(OptionalMapExprNode),
    WhenExpr(WhenExprNode),
    WhenBranch(WhenBranchNode),
    ExprStmt(ExprStmtNode),
    GroupingExpr(GroupingExprNode),
    LambdaDecl(LambdaDeclNode),
    LambdaExpr(LambdaExprNode),
    ReturnStmt(ReturnStmtNode),
    BreakStmt(BreakStmtNode),
    ContinueStmt(ContinueStmtNode),
    BlockStmt(BlockStmtNode),
    IfStmt(IfStmtNode),
    WhileStmt(WhileStmtNode),
    ForStmt(ForStmtNode),
    VariableDecl(VariableDeclNode),
    ImportModuleDecl(ImportModuleDeclNode),
}

impl AstNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            AstNode::Module(node) => node.span,
            AstNode::Class(node) => node.span,
            AstNode::Function(node) => node.span,
            AstNode::Identifier(node) => node.span,
            AstNode::FieldDecl(node) => node.span,
            AstNode::FunctionDecl(node) => node.span,
            AstNode::FunctionExpr(node) => node.span,
            AstNode::NumberLiteral(node) => node.span,
            AstNode::StringLiteral(node) => node.span,
            AstNode::BooleanLiteral(node) => node.span,
            AstNode::NilLiteral(node) => node.span,
            AstNode::ThisExpr(node) => node.span,
            AstNode::SuperExpr(node) => node.span,
            AstNode::AssignmentExpr(node) => node.span,
            AstNode::PipeExpr(node) => node.span,
            AstNode::TernaryExpr(node) => node.span,
            AstNode::LogicalOrExpr(node) => node.span,
            AstNode::LogicalAndExpr(node) => node.span,
            AstNode::EqualityExpr(node) => node.span,
            AstNode::ComparisonExpr(node) => node.span,
            AstNode::TermExpr(node) => node.span,
            AstNode::FactorExpr(node) => node.span,
            AstNode::UnaryExpr(node) => node.span,
            AstNode::CallExpr(node) => node.span,
            AstNode::CallOperation(node) => node.span,
            AstNode::PropertyAccess(node) => node.span,
            AstNode::PropertyAssignment(node) => node.span,
            AstNode::OptionalPropertyAccess(node) => node.span,
            AstNode::IndexAccess(node) => node.span,
            AstNode::IndexAssignment(node) => node.span,
            AstNode::ArrayLiteralExpr(node) => node.span,
            AstNode::ObjectLiteralExpr(node) => node.span,
            AstNode::ObjectEntry(node) => node.span,
            AstNode::MapExpr(node) => node.span,
            AstNode::OptionalMapExpr(node) => node.span,
            AstNode::WhenExpr(node) => node.span,
            AstNode::WhenBranch(node) => node.span,
            AstNode::ExprStmt(node) => node.span,
            AstNode::GroupingExpr(node) => node.span,
            AstNode::LambdaDecl(node) => node.span,
            AstNode::LambdaExpr(node) => node.span,
            AstNode::ReturnStmt(node) => node.span,
            AstNode::BreakStmt(node) => node.span,
            AstNode::ContinueStmt(node) => node.span,
            AstNode::BlockStmt(node) => node.span,
            AstNode::IfStmt(node) => node.span,
            AstNode::WhileStmt(node) => node.span,
            AstNode::ForStmt(node) => node.span,
            AstNode::VariableDecl(node) => node.span,
            AstNode::ImportModuleDecl(node) => node.span,
        }
    }
}

/// Root AST node representing a complete program
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Module {
    pub decls: NodeArrayId, // [DeclNode]
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct IdentifierNode {
    pub name: StringHandle,
    pub span: SourceSpan,
}

/// Number literal
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct NumberLiteralNode {
    pub value: f64,
    pub span: SourceSpan,
}

/// String literal
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct StringLiteralNode {
    pub value: StringHandle,
    pub span: SourceSpan,
}

/// Boolean literal
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct BooleanLiteralNode {
    pub value: bool,
    pub span: SourceSpan,
}

/// Nil literal
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct NilLiteralNode {
    pub span: SourceSpan,
}

/// This expression
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ThisExprNode {
    pub span: SourceSpan,
}

/// Super method access: "super.method"
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct SuperExprNode {
    pub method: NodeId, // IdentifierNode
    pub span: SourceSpan,
}

/// Grouping expression: ( expression )
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct GroupingExprNode {
    pub expr: NodeId, // ExprNode
    pub span: SourceSpan,
}

/// Array literal: [ ( expression ( , expression )* )? ]
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ArrayLiteralExprNode {
    pub elements: NodeArrayId, // [ExprNode]
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ObjectLiteralExprNode {
    pub entries: NodeArrayId, // [ObjectEntryNode]
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ObjectEntryNode {
    pub key: NodeId,   // IdentifierNode
    pub value: NodeId, // ExprNode
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ImportModuleDeclNode {
    pub path: StringHandle,
    pub name: NodeId, // IdentifierNode
    pub span: SourceSpan,
}

/// Class declaration: class IDENTIFIER ( : IDENTIFIER )? { classMember* }
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ClassDeclNode {
    pub name: NodeId,               // IdentifierNode
    pub superclass: Option<NodeId>, // IdentifierNode
    pub members: NodeArrayId,       // [FunctionExprNode | FieldDeclNode]
    pub span: SourceSpan,
}

/// Field declaration: IDENTIFIER ( = expression )? ;
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct FieldDeclNode {
    pub name: NodeId,                // IdentifierNode
    pub initializer: Option<NodeId>, // ExprNode
    pub span: SourceSpan,
}

/// Function declaration: fn function
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct FunctionDeclNode {
    pub function: NodeId, // FunctionExprNode
    pub span: SourceSpan,
}

/// Function expression: IDENTIFIER ( parameters? ) block
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct FunctionExprNode {
    pub name: NodeId,            // IdentifierNode,
    pub parameters: NodeArrayId, // [IdentifierNode]
    pub body: NodeId,            // BlockStmtNode
    pub span: SourceSpan,
}

/// Assignment expression: ( call . IDENTIFIER | IDENTIFIER ) ( = | += | -= | *= | /= | %= ) assignment | pipe
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct AssignmentExprNode {
    pub target: NodeId, // AssignmentTargetNode
    pub operator: AssignmentOperator,
    pub value: NodeId, // ExprNode
    pub span: SourceSpan,
}

/// Property access for assignment: call . IDENTIFIER
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct PropertyAssignmentNode {
    pub property: NodeId, // IdentifierNode
    pub object: NodeId,   // ExprNode
    pub span: SourceSpan,
}

/// Index access for assignment: call [ expression ]
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct IndexAssignmentNode {
    pub object: NodeId, // ExprNode
    pub index: NodeId,  // ExprNode
    pub span: SourceSpan,
}

/// Pipe expression: ternary ( |> pipe )?
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct PipeExprNode {
    pub left: NodeId,  // ExprNode
    pub right: NodeId, //ExprNode
    pub span: SourceSpan,
}

/// Ternary expression: logicOr ( ? expression : ternary )?
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct TernaryExprNode {
    pub condition: NodeId, // ExprNode
    pub then_expr: NodeId, // ExprNode
    pub else_expr: NodeId, //ExprNode
    pub span: SourceSpan,
}

/// Logical OR expression: logicAnd ( or logicAnd )*
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct LogicalOrExprNode {
    pub left: NodeId,  // ExprNode
    pub right: NodeId, // ExprNode
    pub span: SourceSpan,
}

/// Logical AND expression: equality ( and equality )*
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct LogicalAndExprNode {
    pub left: NodeId,  // ExprNode
    pub right: NodeId, // ExprNode
    pub span: SourceSpan,
}

/// Equality expression: comparison ( ( != | == ) comparison )*
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct EqualityExprNode {
    pub left: NodeId, // ExprNode
    pub operator: EqualityOperator,
    pub right: NodeId, // ExprNode
    pub span: SourceSpan,
}

/// Comparison expression: term ( ( > | >= | < | <= ) term )*
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ComparisonExprNode {
    pub left: NodeId, // ExprNode
    pub operator: ComparisonOperator,
    pub right: NodeId, // ExprNode
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct TermExprNode {
    pub left: NodeId, // ExprNode
    pub operator: TermOperator,
    pub right: NodeId, // ExprNode
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct FactorExprNode {
    pub left: NodeId, // ExprNode
    pub operator: FactorOperator,
    pub right: NodeId, // ExprNode
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct UnaryExprNode {
    pub operator: UnaryOperator,
    pub operand: NodeId, //ExprNode
    pub span: SourceSpan,
}

/// Call expression: primary ( ( arguments? ) | . IDENTIFIER | .? IDENTIFIER | [ expression ] )*
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct CallExprNode {
    pub callee: NodeId,    // ExprNode
    pub operation: NodeId, // CallOperationNode
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct CallNode {
    pub args: NodeArrayId, // [ExprNode]
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct PropertyNode {
    pub identifier: NodeId, // IdentifierNode
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct OptionalPropertyNode {
    pub identifier: NodeId, // IdentifierNode
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct IndexNode {
    pub index: NodeId, // ExprNode
    pub span: SourceSpan,
}

/// Map call expression: target||parameter -> expression|
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct MapExprNode {
    pub parameter: NodeId, // IdentifierNode
    pub body: NodeId,      // ExprNode
    pub span: SourceSpan,
}

/// Optional map call expression: target?|parameter -> expression|
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct OptionalMapExprNode {
    pub parameter: NodeId, // IdentifierNode
    pub body: NodeId,      //ExprNode
    pub span: SourceSpan,
}

/// When expression: when ( value )? { branch* ( else => expression )? }
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct WhenExprNode {
    pub value: Option<NodeId>,    // Optional value being matched
    pub branches: NodeArrayId,    // [WhenBranchNode]
    pub else_branch: Option<NodeId>, // Optional else expression
    pub span: SourceSpan,
}

/// When branch: condition => expression
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct WhenBranchNode {
    pub condition: NodeId,   // ExprNode - the left side (expression or pattern)
    pub is_type_check: bool, // whether this uses the `is` operator
    pub body: NodeId,        // ExprNode - the right side expression
    pub span: SourceSpan,
}

/// Lambda declaration: var IDENTIFIER = lambda
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct LambdaDeclNode {
    pub name: NodeId,   // IdentifierNode
    pub lambda: NodeId, // LambdaExprNode
    pub span: SourceSpan,
}

/// Lambda expression: ( parameters? ) -> ( block | expression )
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct LambdaExprNode {
    pub parameters: NodeArrayId, // [IdentifierNode]
    pub body: NodeId,            // BlockStmtNode | ExprNode
    pub span: SourceSpan,
}

/// Variable declaration: var ( IDENTIFIER | destructurePattern ) ( = expression )? ;
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct VariableDeclNode {
    pub target: NodeId,              // IdentifierNode
    pub initializer: Option<NodeId>, // ExprNode
    pub span: SourceSpan,
}

/// Expression statement: expression ;
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ExprStmtNode {
    pub expr: NodeId, // ExprNode
    pub span: SourceSpan,
}

/// Block statement: { declaration* }
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct BlockStmtNode {
    pub decls: NodeArrayId, // [DeclNode]
    pub span: SourceSpan,
}

/// If statement: if ( expression ) statement ( else statement )?
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct IfStmtNode {
    pub condition: NodeId,           // ExprNode
    pub then_branch: NodeId,         // StmtNode
    pub else_branch: Option<NodeId>, // StmtNode
    pub span: SourceSpan,
}

/// While statement: while ( expression ) statement
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct WhileStmtNode {
    pub condition: NodeId, // ExprNode
    pub body: NodeId,      // StmtNode
    pub span: SourceSpan,
}

/// For statement: for ( ( varDecl | exprStmt | ; ) expression? ; expression? ) statement
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ForStmtNode {
    pub initializer: Option<NodeId>, // VariableDeclNode | ExprNode
    pub condition: Option<NodeId>,   // ExprNode
    pub increment: Option<NodeId>,   // ExprNode
    pub body: NodeId,                // StmtNode
    pub span: SourceSpan,
}

/// Break statement: break ;
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct BreakStmtNode {
    pub span: SourceSpan,
}

/// Continue statement: continue ;
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ContinueStmtNode {
    pub span: SourceSpan,
}

/// Return statement: return expression? ;
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ReturnStmtNode {
    pub value: Option<NodeId>, // ExprNode
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NodeConversionError(String);

#[derive(Debug, Clone, PartialEq, Copy)]

pub enum DeclNode {
    Class(ClassDeclNode),
    Function(FunctionDeclNode),
    Lambda(LambdaDeclNode),
    Variable(VariableDeclNode),
    Stmt(StmtNode),
    Module(ImportModuleDeclNode),
}

impl DeclNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Class(class_decl) => class_decl.span,
            Self::Function(func_decl) => func_decl.span,
            Self::Lambda(lambda_decl) => lambda_decl.span,
            Self::Variable(variable_decl) => variable_decl.span,
            Self::Stmt(stmt) => stmt.span(),
            Self::Module(import_decl) => import_decl.span,
        }
    }
}

impl TryFrom<AstNode> for DeclNode {
    type Error = NodeConversionError;

    fn try_from(value: AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::Class(class_decl) => Ok(DeclNode::Class(class_decl)),
            AstNode::FunctionDecl(func_decl) => Ok(DeclNode::Function(func_decl)),
            AstNode::LambdaDecl(lambda_decl) => Ok(DeclNode::Lambda(lambda_decl)),
            AstNode::VariableDecl(var_decl) => Ok(DeclNode::Variable(var_decl)),
            AstNode::ImportModuleDecl(imprt_decl) => Ok(DeclNode::Module(imprt_decl)),
            _ => Ok(DeclNode::Stmt(StmtNode::try_from(value)?)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]

pub enum StmtNode {
    Expr(ExprStmtNode),
    Block(BlockStmtNode),
    If(IfStmtNode),
    While(WhileStmtNode),
    For(ForStmtNode),
    Break(BreakStmtNode),
    Continue(ContinueStmtNode),
    Return(ReturnStmtNode),
}

impl StmtNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Expr(expr_stmt) => expr_stmt.span,
            Self::Block(block_stmt) => block_stmt.span,
            Self::If(if_stmt) => if_stmt.span,
            Self::While(while_stmt) => while_stmt.span,
            Self::For(for_stmt) => for_stmt.span,
            Self::Break(break_stmt) => break_stmt.span,
            Self::Continue(continue_stmt) => continue_stmt.span,
            Self::Return(return_stmt) => return_stmt.span,
        }
    }
}

impl TryFrom<AstNode> for StmtNode {
    type Error = NodeConversionError;

    fn try_from(value: AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::ExprStmt(expr_stmt) => Ok(StmtNode::Expr(expr_stmt)),
            AstNode::BlockStmt(block_stmt) => Ok(StmtNode::Block(block_stmt)),
            AstNode::IfStmt(if_stmt) => Ok(StmtNode::If(if_stmt)),
            AstNode::WhileStmt(while_stmt) => Ok(StmtNode::While(while_stmt)),
            AstNode::ForStmt(for_stmt) => Ok(StmtNode::For(for_stmt)),
            AstNode::BreakStmt(break_stmt) => Ok(StmtNode::Break(break_stmt)),
            AstNode::ContinueStmt(continue_stmt) => Ok(StmtNode::Continue(continue_stmt)),
            AstNode::ReturnStmt(return_stmt) => Ok(StmtNode::Return(return_stmt)),
            _ => Err(NodeConversionError(format!(
                "Cannot convert {:?} to StmtNode",
                value
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ExprNode {
    Assignment(AssignmentExprNode),
    Pipe(PipeExprNode),
    Ternary(TernaryExprNode),
    LogicalOr(LogicalOrExprNode),
    LogicalAnd(LogicalAndExprNode),
    Equality(EqualityExprNode),
    Comparison(ComparisonExprNode),
    Term(TermExprNode),
    Factor(FactorExprNode),
    Unary(UnaryExprNode),
    Call(CallExprNode),
    Primary(PrimaryNode),
}

impl ExprNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Assignment(assignment_expr) => assignment_expr.span,
            Self::Pipe(pipe_expr) => pipe_expr.span,
            Self::Ternary(ternary_expr) => ternary_expr.span,
            Self::LogicalOr(logical_or_expr) => logical_or_expr.span,
            Self::LogicalAnd(logical_and_expr) => logical_and_expr.span,
            Self::Equality(equality_expr) => equality_expr.span,
            Self::Comparison(comparison_expr) => comparison_expr.span,
            Self::Term(term_expr) => term_expr.span,
            Self::Factor(factor_expr) => factor_expr.span,
            Self::Unary(unary_expr) => unary_expr.span,
            Self::Call(call_expr) => call_expr.span,
            Self::Primary(primary) => primary.span(),
        }
    }
}

impl TryFrom<AstNode> for ExprNode {
    type Error = NodeConversionError;

    fn try_from(value: AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::AssignmentExpr(assignment_expr) => Ok(ExprNode::Assignment(assignment_expr)),
            AstNode::PipeExpr(pipe_expr) => Ok(ExprNode::Pipe(pipe_expr)),
            AstNode::TernaryExpr(ternary_expr) => Ok(ExprNode::Ternary(ternary_expr)),
            AstNode::LogicalOrExpr(logical_or_expr) => Ok(ExprNode::LogicalOr(logical_or_expr)),
            AstNode::LogicalAndExpr(logical_and_expr) => Ok(ExprNode::LogicalAnd(logical_and_expr)),
            AstNode::EqualityExpr(equality_expr) => Ok(ExprNode::Equality(equality_expr)),
            AstNode::ComparisonExpr(comparison_expr) => Ok(ExprNode::Comparison(comparison_expr)),
            AstNode::TermExpr(term_expr) => Ok(ExprNode::Term(term_expr)),
            AstNode::FactorExpr(factor_expr) => Ok(ExprNode::Factor(factor_expr)),
            AstNode::UnaryExpr(unary_expr) => Ok(ExprNode::Unary(unary_expr)),
            AstNode::CallExpr(call_expr) => Ok(ExprNode::Call(call_expr)),
            _ => Ok(ExprNode::Primary(PrimaryNode::try_from(value)?)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CallOperationNode {
    Call(CallNode),
    Property(PropertyNode),
    OptionalProperty(OptionalPropertyNode),
    Index(IndexNode),
    Map(MapExprNode),
    OptionalMap(OptionalMapExprNode),
}

impl CallOperationNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Call(call) => call.span,
            Self::Property(property) => property.span,
            Self::OptionalProperty(optional_property) => optional_property.span,
            Self::Index(index) => index.span,
            Self::Map(map_expr) => map_expr.span,
            Self::OptionalMap(optional_map_expr) => optional_map_expr.span,
        }
    }
}

impl TryFrom<AstNode> for CallOperationNode {
    type Error = NodeConversionError;

    fn try_from(value: AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::CallOperation(call) => Ok(CallOperationNode::Call(call)),
            AstNode::PropertyAccess(property) => Ok(CallOperationNode::Property(property)),
            AstNode::OptionalPropertyAccess(optional_property) => {
                Ok(CallOperationNode::OptionalProperty(optional_property))
            }
            AstNode::IndexAccess(index_access) => Ok(CallOperationNode::Index(index_access)),
            AstNode::MapExpr(map_expr) => Ok(CallOperationNode::Map(map_expr)),
            AstNode::OptionalMapExpr(optional_map_expr) => {
                Ok(CallOperationNode::OptionalMap(optional_map_expr))
            }
            _ => Err(NodeConversionError(format!(
                "Cannot convert {:?} to CallOperationNode",
                value
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AssignmentTargetNode {
    Identifier(IdentifierNode),
    Property(PropertyAssignmentNode),
    Index(IndexAssignmentNode),
}

impl AssignmentTargetNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Identifier(identifier) => identifier.span,
            Self::Property(property) => property.span,
            Self::Index(index) => index.span,
        }
    }
}

impl TryFrom<AstNode> for AssignmentTargetNode {
    type Error = NodeConversionError;

    fn try_from(value: AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::PropertyAssignment(property) => Ok(AssignmentTargetNode::Property(property)),
            AstNode::IndexAssignment(index) => Ok(AssignmentTargetNode::Index(index)),
            AstNode::Identifier(identifier) => Ok(AssignmentTargetNode::Identifier(identifier)),
            _ => Err(NodeConversionError(format!(
                "Cannot convert {:?} to AssignmentTargetNode",
                value
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum PrimaryNode {
    Number(NumberLiteralNode),
    String(StringLiteralNode),
    Boolean(BooleanLiteralNode),
    Nil(NilLiteralNode),
    Identifier(IdentifierNode),
    This(ThisExprNode),
    Super(SuperExprNode),
    Grouping(GroupingExprNode),
    Lambda(LambdaExprNode),
    Array(ArrayLiteralExprNode),
    Object(ObjectLiteralExprNode),
    When(WhenExprNode),
}

impl PrimaryNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Array(array_literal) => array_literal.span,
            Self::Boolean(boolean_literal) => boolean_literal.span,
            Self::Grouping(grouping_expr) => grouping_expr.span,
            Self::Identifier(identifier) => identifier.span,
            Self::Lambda(lambda_expr) => lambda_expr.span,
            Self::Nil(nil_literal) => nil_literal.span,
            Self::Number(number_literal) => number_literal.span,
            Self::Object(object_literal) => object_literal.span,
            Self::String(string_literal) => string_literal.span,
            Self::Super(super_expr) => super_expr.span,
            Self::This(this_expr) => this_expr.span,
            Self::When(when_expr) => when_expr.span,
        }
    }
}

impl TryFrom<AstNode> for PrimaryNode {
    type Error = NodeConversionError;

    fn try_from(value: AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::NumberLiteral(number_literal) => Ok(PrimaryNode::Number(number_literal)),
            AstNode::StringLiteral(string_literal) => Ok(PrimaryNode::String(string_literal)),
            AstNode::BooleanLiteral(boolean_literal) => Ok(PrimaryNode::Boolean(boolean_literal)),
            AstNode::NilLiteral(nil_literal) => Ok(PrimaryNode::Nil(nil_literal)),
            AstNode::Identifier(identifier_literal) => {
                Ok(PrimaryNode::Identifier(identifier_literal))
            }
            AstNode::ThisExpr(this_expr) => Ok(PrimaryNode::This(this_expr)),
            AstNode::SuperExpr(super_expr) => Ok(PrimaryNode::Super(super_expr)),
            AstNode::GroupingExpr(grouping_expr) => Ok(PrimaryNode::Grouping(grouping_expr)),
            AstNode::LambdaExpr(lambda_expr) => Ok(PrimaryNode::Lambda(lambda_expr)),
            AstNode::ArrayLiteralExpr(array_expr) => Ok(PrimaryNode::Array(array_expr)),
            AstNode::ObjectLiteralExpr(object_expr) => Ok(PrimaryNode::Object(object_expr)),
            AstNode::WhenExpr(when_expr) => Ok(PrimaryNode::When(when_expr)),
            _ => Err(NodeConversionError(format!(
                "Cannot convert {:?} to PrimaryNode",
                value
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ClassMemberNode {
    Method(FunctionExprNode),
    Field(FieldDeclNode),
}

impl ClassMemberNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Method(method) => method.span,
            Self::Field(field) => field.span,
        }
    }
}

impl TryFrom<AstNode> for ClassMemberNode {
    type Error = NodeConversionError;

    fn try_from(value: AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::FieldDecl(field) => Ok(ClassMemberNode::Field(field)),
            AstNode::FunctionExpr(method) => Ok(ClassMemberNode::Method(method)),
            _ => Err(NodeConversionError(format!(
                "Cannot convert {:?} to ClassMember",
                value
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum LambdaBodyNode {
    Block(BlockStmtNode),
    Expr(ExprNode),
}

impl LambdaBodyNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Block(block) => block.span,
            Self::Expr(expr) => expr.span(),
        }
    }
}

impl TryFrom<AstNode> for LambdaBodyNode {
    type Error = NodeConversionError;

    fn try_from(value: AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::BlockStmt(block) => Ok(LambdaBodyNode::Block(block)),
            _ => Ok(LambdaBodyNode::Expr(value.try_into()?)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum ForInitializerNode {
    VarDecl(VariableDeclNode),
    Expr(ExprNode),
}

impl ForInitializerNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::VarDecl(var_decl) => var_decl.span,
            Self::Expr(expr) => expr.span(),
        }
    }
}

impl TryFrom<AstNode> for ForInitializerNode {
    type Error = NodeConversionError;

    fn try_from(value: AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::VariableDecl(var_decl) => Ok(ForInitializerNode::VarDecl(var_decl)),
            _ => Ok(ForInitializerNode::Expr(value.try_into()?)),
        }
    }
}
