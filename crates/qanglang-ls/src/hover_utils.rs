use qanglang_core::nodes::*;
use qanglang_core::{AstNodeArena, NodeId, SourceMap, StringInterner, TypedNodeRef};
use tower_lsp::lsp_types::{Position, Range};

use crate::analyzer::AnalysisResult;

#[derive(Debug, Clone)]
pub struct NodeInfo {
    #[allow(dead_code)]
    node_id: NodeId, // Might be useful later when doc comments are added
    pub range: Range,
    pub kind: NodeKind,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Class(String),
    Function(String),
    Variable(String),
    Field(String),
    Parameter(String),
}

/// Find the AST node at the given byte offset
pub fn find_node_at_offset(analysis: &AnalysisResult, offset: usize) -> Option<NodeInfo> {
    let module = analysis.nodes.get_program_node(analysis.root_module_id);

    let mut finder = NodeFinder {
        offset,
        best_match: None,
        nodes: &analysis.nodes,
        strings: &analysis.strings,
        source_map: &analysis.source_map,
    };

    finder.find_in_module(module);

    finder.best_match
}

struct NodeFinder<'a> {
    offset: usize,
    best_match: Option<NodeInfo>,
    nodes: &'a AstNodeArena,
    strings: &'a StringInterner,
    source_map: &'a SourceMap,
}

impl<'a> NodeFinder<'a> {
    fn find_in_module(&mut self, module: TypedNodeRef<Module>) {
        let length = self.nodes.array.size(module.node.decls);

        for i in 0..length {
            if let Some(node_id) = self.nodes.array.get_node_id_at(module.node.decls, i) {
                let decl = self.nodes.get_decl_node(node_id);
                self.check_decl(decl);
            }
        }
    }

    fn check_decl(&mut self, decl: TypedNodeRef<DeclNode>) {
        match decl.node {
            DeclNode::Class(class) => self.check_class(TypedNodeRef::new(decl.id, class)),
            DeclNode::Function(func) => self.check_function(TypedNodeRef::new(decl.id, func)),
            DeclNode::Variable(var) => self.check_variable(TypedNodeRef::new(decl.id, var)),
            _ => {}
        }
    }

    fn check_class(&mut self, class: TypedNodeRef<ClassDeclNode>) {
        // Check if the class name identifier contains the offset
        let name_node = self.nodes.get_identifier_node(class.node.name);
        if self.span_contains(name_node.node.span) {
            if self.is_better_match(name_node.node.span) {
                let name = self.strings.get_string(name_node.node.name).to_string();

                self.best_match = Some(NodeInfo {
                    node_id: class.id,
                    range: self.span_to_range(name_node.node.span),
                    kind: NodeKind::Class(name),
                });
            }
        }

        // Check class members
        let members_length = self.nodes.array.size(class.node.members);
        for i in 0..members_length {
            if let Some(member_id) = self.nodes.array.get_node_id_at(class.node.members, i) {
                let member = self.nodes.get_class_member_node(member_id);
                match member.node {
                    ClassMemberNode::Field(field) => {
                        self.check_field(TypedNodeRef::new(member.id, field))
                    }
                    ClassMemberNode::Method(method) => {
                        self.check_function_expr(TypedNodeRef::new(member.id, method))
                    }
                }
            }
        }
    }

    fn check_function(&mut self, func: TypedNodeRef<FunctionDeclNode>) {
        // FunctionDeclNode wraps a FunctionExprNode
        let func_expr = self.nodes.get_func_expr_node(func.node.function);
        self.check_function_expr(func_expr);
    }

    fn check_function_expr(&mut self, func: TypedNodeRef<FunctionExprNode>) {
        // Check function name
        let name_node = self.nodes.get_identifier_node(func.node.name);
        if self.span_contains(name_node.node.span) {
            if self.is_better_match(name_node.node.span) {
                let name = self.strings.get_string(name_node.node.name).to_string();

                self.best_match = Some(NodeInfo {
                    node_id: func.id,
                    range: self.span_to_range(name_node.node.span),
                    kind: NodeKind::Function(name),
                });
            }
        }

        // Check parameters
        let params_length = self.nodes.array.size(func.node.parameters);
        for i in 0..params_length {
            if let Some(param_id) = self.nodes.array.get_node_id_at(func.node.parameters, i) {
                let param = self.nodes.get_identifier_node(param_id);
                self.check_parameter(param);
            }
        }

        // Check function body
        let body = self.nodes.get_block_stmt_node(func.node.body);
        self.check_block(body);
    }

    fn check_block(&mut self, block: TypedNodeRef<BlockStmtNode>) {
        let decls_length = self.nodes.array.size(block.node.decls);
        for i in 0..decls_length {
            if let Some(decl_id) = self.nodes.array.get_node_id_at(block.node.decls, i) {
                let decl = self.nodes.get_decl_node(decl_id);
                self.check_decl(decl);
            }
        }
    }

    fn check_variable(&mut self, var: TypedNodeRef<VariableDeclNode>) {
        let name_node = self.nodes.get_identifier_node(var.node.target);
        if self.span_contains(name_node.node.span) {
            if self.is_better_match(name_node.node.span) {
                let name = self.strings.get_string(name_node.node.name).to_string();

                self.best_match = Some(NodeInfo {
                    node_id: var.id,
                    range: self.span_to_range(name_node.node.span),
                    kind: NodeKind::Variable(name),
                });
            }
        }
    }

    fn check_field(&mut self, field: TypedNodeRef<FieldDeclNode>) {
        let name_node = self.nodes.get_identifier_node(field.node.name);
        if self.span_contains(name_node.node.span) {
            if self.is_better_match(name_node.node.span) {
                let name = self.strings.get_string(name_node.node.name).to_string();

                self.best_match = Some(NodeInfo {
                    node_id: field.id,
                    range: self.span_to_range(name_node.node.span),
                    kind: NodeKind::Field(name),
                });
            }
        }
    }

    fn check_parameter(&mut self, param: TypedNodeRef<IdentifierNode>) {
        if self.span_contains(param.node.span) {
            if self.is_better_match(param.node.span) {
                let name = self.strings.get_string(param.node.name).to_string();

                self.best_match = Some(NodeInfo {
                    node_id: param.id,
                    range: self.span_to_range(param.node.span),
                    kind: NodeKind::Parameter(name),
                });
            }
        }
    }

    fn span_contains(&self, span: SourceSpan) -> bool {
        self.offset >= span.start && self.offset < span.end
    }

    fn is_better_match(&self, span: SourceSpan) -> bool {
        match &self.best_match {
            None => true,
            Some(current) => {
                // Prefer smaller spans (more specific nodes)
                let current_size = current.range.end.character - current.range.start.character;
                let new_size = (span.end - span.start) as u32;
                new_size < current_size
            }
        }
    }

    fn span_to_range(&self, span: SourceSpan) -> Range {
        Range {
            start: Position {
                line: self.source_map.get_line_number(span.start) - 1,
                character: self.source_map.get_column_number(span.start) - 1,
            },
            end: Position {
                line: self.source_map.get_line_number(span.end) - 1,
                character: self.source_map.get_column_number(span.end) - 1,
            },
        }
    }
}

/// Format hover information for display
pub fn format_hover_info(_analysis: &AnalysisResult, info: &NodeInfo) -> String {
    match &info.kind {
        NodeKind::Class(name) => format!("```qanglang\nclass {}\n```", name),
        NodeKind::Function(name) => format!("```qanglang\nfn {}\n```", name),
        NodeKind::Variable(name) => format!("```qanglang\nvar {}\n```", name),
        NodeKind::Field(name) => format!("```qanglang\nfield {}\n```", name),
        NodeKind::Parameter(name) => format!("```qanglang\nparam {}\n```", name),
    }
}
