use crate::{
    arena::{Arena, Index},
    frontend::{node_array_arena::NodeArrayArena, nodes::*},
};
use std::convert::TryFrom;

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq, Hash)]
pub struct NodeId(Index);

impl NodeId {
    pub fn new(index: Index) -> Self {
        Self(index)
    }

    pub fn get(&self) -> Index {
        self.0
    }
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
}

impl DeclNode {
    pub fn span(&self) -> SourceSpan {
        match self {
            Self::Class(class_decl) => class_decl.span,
            Self::Function(func_decl) => func_decl.span,
            Self::Lambda(lambda_decl) => lambda_decl.span,
            Self::Variable(variable_decl) => variable_decl.span,
            Self::Stmt(stmt) => stmt.span(),
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

#[derive(Debug, Clone, Copy)]
pub struct TypedNodeRef<T: Clone + Copy> {
    pub id: NodeId,
    pub node: T,
}

impl<T: Clone + Copy> TypedNodeRef<T> {
    pub fn new(id: NodeId, node: T) -> Self {
        Self { id, node }
    }
}

#[derive(Default, Clone)]
pub struct TypedNodeArena {
    nodes: Arena<AstNode>,
    pub array: NodeArrayArena,
}

impl TypedNodeArena {
    pub fn new() -> Self {
        Self {
            nodes: Arena::new(),
            array: NodeArrayArena::new(),
        }
    }

    pub fn create_node(&mut self, node: AstNode) -> NodeId {
        NodeId::new(self.nodes.insert(node))
    }

    pub fn get_node(&self, node_id: NodeId) -> &AstNode {
        &self.nodes[node_id.get()]
    }

    pub fn get_decl_node(&self, node_id: NodeId) -> TypedNodeRef<DeclNode> {
        let node = self.nodes[node_id.get()];
        TypedNodeRef::new(node_id, node.try_into().unwrap())
    }

    pub fn get_stmt_node(&self, node_id: NodeId) -> TypedNodeRef<StmtNode> {
        let node = self.nodes[node_id.get()];
        TypedNodeRef::new(node_id, node.try_into().unwrap())
    }

    pub fn get_expr_node(&self, node_id: NodeId) -> TypedNodeRef<ExprNode> {
        let node = self.nodes[node_id.get()];
        TypedNodeRef::new(node_id, node.try_into().unwrap())
    }

    pub fn get_call_operation_node(&self, node_id: NodeId) -> TypedNodeRef<CallOperationNode> {
        let node = self.nodes[node_id.get()];
        TypedNodeRef::new(node_id, node.try_into().unwrap())
    }

    pub fn get_assignment_target_node(
        &self,
        node_id: NodeId,
    ) -> TypedNodeRef<AssignmentTargetNode> {
        let node = self.nodes[node_id.get()];
        TypedNodeRef::new(node_id, node.try_into().unwrap())
    }

    pub fn get_primary_node(&self, node_id: NodeId) -> TypedNodeRef<PrimaryNode> {
        let node = self.nodes[node_id.get()];
        TypedNodeRef::new(node_id, node.try_into().unwrap())
    }

    pub fn get_var_decl_node(&self, node_id: NodeId) -> TypedNodeRef<VariableDeclNode> {
        let node = self.nodes[node_id.get()];

        match node {
            AstNode::VariableDecl(decl) => TypedNodeRef::new(node_id, decl),
            _ => panic!("Expected VariableDeclNode."),
        }
    }

    pub fn get_program_node(&self, node_id: NodeId) -> TypedNodeRef<Module> {
        let node = self.nodes[node_id.get()];

        match node {
            AstNode::Module(program) => TypedNodeRef::new(node_id, program),
            _ => panic!("Expected ProgramNode"),
        }
    }

    pub fn get_identifier_node(&self, node_id: NodeId) -> TypedNodeRef<IdentifierNode> {
        let node = self.nodes[node_id.get()];
        match node {
            AstNode::Identifier(identifier) => TypedNodeRef::new(node_id, identifier),
            _ => panic!("Expected IdentifierNode"),
        }
    }

    pub fn get_func_expr_node(&self, node_id: NodeId) -> TypedNodeRef<FunctionExprNode> {
        let node = self.nodes[node_id.get()];

        match node {
            AstNode::FunctionExpr(func) => TypedNodeRef::new(node_id, func),
            _ => panic!("Expected FunctionExprNode"),
        }
    }

    pub fn get_lambda_expr_node(&self, node_id: NodeId) -> TypedNodeRef<LambdaExprNode> {
        let node = self.nodes[node_id.get()];

        match node {
            AstNode::LambdaExpr(lambda) => TypedNodeRef::new(node_id, lambda),
            _ => panic!("Expected LambdaExprNode"),
        }
    }

    pub fn get_block_stmt_node(&self, node_id: NodeId) -> TypedNodeRef<BlockStmtNode> {
        let node = self.nodes[node_id.get()];

        match node {
            AstNode::BlockStmt(block) => TypedNodeRef::new(node_id, block),
            _ => panic!("Expected BlockStmtNode."),
        }
    }

    pub fn get_obj_entry_node(&self, node_id: NodeId) -> TypedNodeRef<ObjectEntryNode> {
        let node = self.nodes[node_id.get()];

        match node {
            AstNode::ObjectEntry(entry) => TypedNodeRef::new(node_id, entry),
            _ => panic!("Expected ObjectEntryNode."),
        }
    }

    pub fn get_lambda_body_node(&self, node_id: NodeId) -> TypedNodeRef<LambdaBodyNode> {
        let node = self.nodes[node_id.get()];
        TypedNodeRef::new(node_id, node.try_into().unwrap())
    }

    pub fn get_for_initializer_node(&self, node_id: NodeId) -> TypedNodeRef<ForInitializerNode> {
        let node = self.nodes[node_id.get()];
        TypedNodeRef::new(node_id, node.try_into().unwrap())
    }

    pub fn get_class_member_node(&self, node_id: NodeId) -> TypedNodeRef<ClassMemberNode> {
        let node = self.nodes[node_id.get()];
        TypedNodeRef::new(node_id, node.try_into().unwrap())
    }
}
