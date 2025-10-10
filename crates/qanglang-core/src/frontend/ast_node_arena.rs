use crate::{
    arena::{Arena, Index},
    frontend::{node_array_arena::NodeArrayArena, nodes::*},
};

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

#[derive(Default, Clone, Debug)]
pub struct AstNodeArena {
    nodes: Arena<AstNode>,
    pub array: NodeArrayArena,
}

impl AstNodeArena {
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

    pub fn get_when_expr_node(&self, node_id: NodeId) -> TypedNodeRef<WhenExprNode> {
        let node = self.nodes[node_id.get()];

        match node {
            AstNode::WhenExpr(when_expr) => TypedNodeRef::new(node_id, when_expr),
            _ => panic!("Expected WhenExprNode"),
        }
    }

    pub fn get_when_branch_node(&self, node_id: NodeId) -> TypedNodeRef<WhenBranchNode> {
        let node = self.nodes[node_id.get()];

        match node {
            AstNode::WhenBranch(when_branch) => TypedNodeRef::new(node_id, when_branch),
            _ => panic!("Expected WhenBranchNode"),
        }
    }
}
