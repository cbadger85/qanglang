use crate::{
    StringHandle,
    ast::{
        AssignmentOperator, ComparisonOperator, EqualityOperator, FactorOperator, SourceSpan,
        TermOperator, UnaryOperator,
    },
    frontend::{node_array_arena::NodeArrayId, typed_node_arena::NodeId},
};

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum AstNode {
    Program(ProgramNode),
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
}

/// Root AST node representing a complete program
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ProgramNode {
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
    pub object: NodeId, // ExprNode
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
    pub left: NodeId,          // ExprNode
    pub right: Option<NodeId>, //ExprNode
    pub span: SourceSpan,
}

/// Ternary expression: logicOr ( ? expression : ternary )?
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct TernaryExprNode {
    pub condition: NodeId,         // ExprNode
    pub then_expr: Option<NodeId>, // ExprNode
    pub else_expr: Option<NodeId>, //ExprNode
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
