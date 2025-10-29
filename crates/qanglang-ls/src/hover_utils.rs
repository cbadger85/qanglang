use qanglang_core::nodes::*;
use qanglang_core::symbol_resolver::SymbolKind;
use qanglang_core::{AstNodeArena, NodeId, SourceMap, StringInterner, TypedNodeRef};
use tower_lsp::lsp_types::{Position, Range};

use crate::analyzer::AnalysisResult;
use crate::builtins;
use crate::stdlib_analyzer;

#[derive(Debug, Clone)]
pub struct NodeInfo {
    pub node_id: NodeId,
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
    Module(String),
    ImportPath(String), // Absolute path to the imported module file
    NativeFunction(String), // Native function name
    IntrinsicMethod(String), // Intrinsic method name
    StdlibClass(String), // Stdlib class name
    StdlibFunction(String), // Stdlib function name
    StdlibMethod(String), // Stdlib method name
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
        symbol_table: &analysis.symbol_table,
        current_class: None,
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
    symbol_table: &'a qanglang_core::symbol_resolver::SymbolTable,
    /// Current class context (None if not inside a class, Some(class_decl_node_id) if inside)
    current_class: Option<NodeId>,
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
            DeclNode::Lambda(lambda) => {
                // Check lambda name
                let name_node = self.nodes.get_identifier_node(lambda.name);
                if self.span_contains(name_node.node.span) {
                    if self.is_better_match(name_node.node.span) {
                        let name = self.strings.get_string(name_node.node.name).to_string();
                        self.best_match = Some(NodeInfo {
                            node_id: decl.id,
                            range: self.span_to_range(name_node.node.span),
                            kind: NodeKind::Function(name),
                        });
                    }
                }

                // Check lambda body
                let lambda_expr = self.nodes.get_lambda_expr_node(lambda.lambda);
                let params_length = self.nodes.array.size(lambda_expr.node.parameters);
                for i in 0..params_length {
                    if let Some(param_id) = self.nodes.array.get_node_id_at(lambda_expr.node.parameters, i) {
                        let param = self.nodes.get_identifier_node(param_id);
                        self.check_parameter(param);
                    }
                }

                let node = self.nodes.get_node(lambda_expr.node.body);
                if matches!(node, AstNode::BlockStmt(_)) {
                    let block = self.nodes.get_block_stmt_node(lambda_expr.node.body);
                    self.check_block(block);
                } else {
                    let expr = self.nodes.get_expr_node(lambda_expr.node.body);
                    self.check_expression(expr);
                }
            }
            DeclNode::Stmt(stmt) => {
                self.check_statement(TypedNodeRef::new(decl.id, stmt));
            }
            DeclNode::Module(module_decl) => {
                self.check_module(TypedNodeRef::new(decl.id, module_decl));
            }
        }
    }

    fn check_module(&mut self, module: TypedNodeRef<ImportModuleDeclNode>) {
        // Check if the import path string literal contains the offset
        if self.span_contains(module.node.path_literal_span) {
            if self.is_better_match(module.node.path_literal_span) {
                let path = self.strings.get_string(module.node.path).to_string();
                self.best_match = Some(NodeInfo {
                    node_id: module.id,
                    range: self.span_to_range(module.node.path_literal_span),
                    kind: NodeKind::ImportPath(path),
                });
            }
        }

        // Check if the module name identifier contains the offset
        let name_node = self.nodes.get_identifier_node(module.node.name);
        if self.span_contains(name_node.node.span) {
            if self.is_better_match(name_node.node.span) {
                let name = self.strings.get_string(name_node.node.name).to_string();
                self.best_match = Some(NodeInfo {
                    node_id: module.id,
                    range: self.span_to_range(name_node.node.span),
                    kind: NodeKind::Module(name),
                });
            }
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

        // Check superclass identifier if present (e.g., TestClass in "class Foo : TestClass")
        if let Some(superclass_id) = class.node.superclass {
            let superclass_ident = self.nodes.get_identifier_node(superclass_id);
            self.check_identifier_reference(superclass_ident);
        }

        // Set current class context before checking members
        let previous_class = self.current_class;
        self.current_class = Some(class.id);

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

        // Restore previous class context
        self.current_class = previous_class;
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

    fn check_statement(&mut self, stmt: TypedNodeRef<StmtNode>) {
        match stmt.node {
            StmtNode::Expr(expr_stmt) => {
                let expr = self.nodes.get_expr_node(expr_stmt.expr);
                self.check_expression(expr);
            }
            StmtNode::Block(block) => {
                self.check_block(TypedNodeRef::new(stmt.id, block));
            }
            StmtNode::If(if_stmt) => {
                let cond = self.nodes.get_expr_node(if_stmt.condition);
                self.check_expression(cond);

                let then_branch = self.nodes.get_stmt_node(if_stmt.then_branch);
                self.check_statement(then_branch);

                if let Some(else_id) = if_stmt.else_branch {
                    let else_branch = self.nodes.get_stmt_node(else_id);
                    self.check_statement(else_branch);
                }
            }
            StmtNode::While(while_stmt) => {
                let cond = self.nodes.get_expr_node(while_stmt.condition);
                self.check_expression(cond);

                let body = self.nodes.get_stmt_node(while_stmt.body);
                self.check_statement(body);
            }
            StmtNode::For(for_stmt) => {
                if let Some(init_id) = for_stmt.initializer {
                    let node = self.nodes.get_node(init_id);
                    if matches!(node, AstNode::VariableDecl(_)) {
                        let var_decl = self.nodes.get_var_decl_node(init_id);
                        self.check_variable(var_decl);
                    } else {
                        let expr = self.nodes.get_expr_node(init_id);
                        self.check_expression(expr);
                    }
                }

                if let Some(cond_id) = for_stmt.condition {
                    let cond = self.nodes.get_expr_node(cond_id);
                    self.check_expression(cond);
                }

                if let Some(after_id) = for_stmt.afterthought {
                    let after = self.nodes.get_expr_node(after_id);
                    self.check_expression(after);
                }

                let body = self.nodes.get_stmt_node(for_stmt.body);
                self.check_statement(body);
            }
            StmtNode::Return(ret_stmt) => {
                if let Some(value_id) = ret_stmt.value {
                    let value = self.nodes.get_expr_node(value_id);
                    self.check_expression(value);
                }
            }
            StmtNode::Break(_) | StmtNode::Continue(_) => {}
        }
    }

    fn check_expression(&mut self, expr: TypedNodeRef<ExprNode>) {
        match expr.node {
            ExprNode::Primary(primary) => {
                self.check_primary(TypedNodeRef::new(expr.id, primary));
            }
            ExprNode::Call(call) => {
                let callee = self.nodes.get_expr_node(call.callee);
                self.check_expression(callee);

                let op = self.nodes.get_call_operation_node(call.operation);
                match op.node {
                    CallOperationNode::Call(call_node) => {
                        let length = self.nodes.array.size(call_node.args);
                        for i in 0..length {
                            if let Some(arg_id) = self.nodes.array.get_node_id_at(call_node.args, i) {
                                let arg = self.nodes.get_expr_node(arg_id);
                                self.check_expression(arg);
                            }
                        }
                    }
                    CallOperationNode::Property(prop) => {
                        let name_node = self.nodes.get_identifier_node(prop.identifier);
                        let name = self.strings.get_string(name_node.node.name).to_string();

                        if self.span_contains(name_node.node.span) {
                            if self.is_better_match(name_node.node.span) {

                                // Check if this is an intrinsic method first
                                if builtins::get_string_method(&name).is_some()
                                    || builtins::get_array_method(&name).is_some()
                                    || builtins::get_number_method(&name).is_some()
                                    || builtins::get_function_method(&name).is_some()
                                {
                                    self.best_match = Some(NodeInfo {
                                        node_id: name_node.id,
                                        range: self.span_to_range(name_node.node.span),
                                        kind: NodeKind::IntrinsicMethod(name.clone()),
                                    });
                                } else if let Some(stdlib_analyzer::StdlibSymbol::Method { method_name, .. }) =
                                    stdlib_analyzer::get_stdlib_cache().get(&name)
                                {
                                    // Check if this is a stdlib method
                                    self.best_match = Some(NodeInfo {
                                        node_id: name_node.id,
                                        range: self.span_to_range(name_node.node.span),
                                        kind: NodeKind::StdlibMethod(method_name.clone()),
                                    });
                                } else {
                                    // Check if callee is 'super' - if so, look up in superclass
                                    let kind = if matches!(callee.node, ExprNode::Primary(PrimaryNode::Super(_))) {
                                        let kind = self.find_super_property_kind(&name);
                                        kind
                                    } else {
                                        // Try to determine if this is a method or field
                                        // Look up the callee to find its type/class, then check the member
                                        self.find_property_kind(callee, &name)
                                    };

                                    self.best_match = Some(NodeInfo {
                                        node_id: name_node.id,
                                        range: self.span_to_range(name_node.node.span),
                                        kind,
                                    });
                                }
                            } else {
                            }
                        } else {
                        }
                    }
                    CallOperationNode::Index(index) => {
                        let index_expr = self.nodes.get_expr_node(index.index);
                        self.check_expression(index_expr);
                    }
                    _ => {}
                }
            }
            ExprNode::Assignment(assign) => {
                let target = self.nodes.get_assignment_target_node(assign.target);
                self.check_assignment_target(target);

                let value = self.nodes.get_expr_node(assign.value);
                self.check_expression(value);
            }
            ExprNode::Unary(unary) => {
                let operand = self.nodes.get_expr_node(unary.operand);
                self.check_expression(operand);
            }
            ExprNode::Equality(equality) => {
                let left = self.nodes.get_expr_node(equality.left);
                self.check_expression(left);

                let right = self.nodes.get_expr_node(equality.right);
                self.check_expression(right);
            }
            ExprNode::Comparison(comparison) => {
                let left = self.nodes.get_expr_node(comparison.left);
                self.check_expression(left);

                let right = self.nodes.get_expr_node(comparison.right);
                self.check_expression(right);
            }
            ExprNode::Term(term) => {
                let left = self.nodes.get_expr_node(term.left);
                self.check_expression(left);

                let right = self.nodes.get_expr_node(term.right);
                self.check_expression(right);
            }
            ExprNode::Factor(factor) => {
                let left = self.nodes.get_expr_node(factor.left);
                self.check_expression(left);

                let right = self.nodes.get_expr_node(factor.right);
                self.check_expression(right);
            }
            ExprNode::Pipe(pipe) => {
                let left = self.nodes.get_expr_node(pipe.left);
                self.check_expression(left);

                let right = self.nodes.get_expr_node(pipe.right);
                self.check_expression(right);
            }
            ExprNode::LogicalOr(logical) => {
                let left = self.nodes.get_expr_node(logical.left);
                self.check_expression(left);

                let right = self.nodes.get_expr_node(logical.right);
                self.check_expression(right);
            }
            ExprNode::LogicalAnd(logical) => {
                let left = self.nodes.get_expr_node(logical.left);
                self.check_expression(left);

                let right = self.nodes.get_expr_node(logical.right);
                self.check_expression(right);
            }
            ExprNode::Ternary(ternary) => {
                let condition = self.nodes.get_expr_node(ternary.condition);
                self.check_expression(condition);

                let then_expr = self.nodes.get_expr_node(ternary.then_expr);
                self.check_expression(then_expr);

                let else_expr = self.nodes.get_expr_node(ternary.else_expr);
                self.check_expression(else_expr);
            }
        }
    }

    fn check_assignment_target(&mut self, target: TypedNodeRef<AssignmentTargetNode>) {
        match target.node {
            AssignmentTargetNode::Identifier(ident) => {
                self.check_identifier_reference(TypedNodeRef::new(target.id, ident));
            }
            AssignmentTargetNode::Property(prop) => {
                // Check the object part (e.g., "this" in "this.name")
                let object = self.nodes.get_expr_node(prop.object);
                self.check_expression(object);

                // Check the property name itself (e.g., "name" in "this.name")
                let property_node = self.nodes.get_identifier_node(prop.property);
                if self.span_contains(property_node.node.span) {
                    if self.is_better_match(property_node.node.span) {
                        let name = self.strings.get_string(property_node.node.name).to_string();

                        // Check if this is an intrinsic method first
                        if builtins::get_string_method(&name).is_some()
                            || builtins::get_array_method(&name).is_some()
                            || builtins::get_number_method(&name).is_some()
                            || builtins::get_function_method(&name).is_some()
                        {
                            self.best_match = Some(NodeInfo {
                                node_id: property_node.id,
                                range: self.span_to_range(property_node.node.span),
                                kind: NodeKind::IntrinsicMethod(name.clone()),
                            });
                        } else if let Some(stdlib_analyzer::StdlibSymbol::Method { method_name, .. }) =
                            stdlib_analyzer::get_stdlib_cache().get(&name)
                        {
                            // Check if this is a stdlib method
                            self.best_match = Some(NodeInfo {
                                node_id: property_node.id,
                                range: self.span_to_range(property_node.node.span),
                                kind: NodeKind::StdlibMethod(method_name.clone()),
                            });
                        } else {
                            // Try to determine if this is a method or field
                            // by looking up the class definition
                            let kind = self.find_property_kind(object, &name);

                            self.best_match = Some(NodeInfo {
                                node_id: property_node.id,
                                range: self.span_to_range(property_node.node.span),
                                kind,
                            });
                        }
                    }
                }
            }
            AssignmentTargetNode::Index(index) => {
                let object = self.nodes.get_expr_node(index.object);
                self.check_expression(object);

                let index_expr = self.nodes.get_expr_node(index.index);
                self.check_expression(index_expr);
            }
        }
    }

    fn check_primary(&mut self, primary: TypedNodeRef<PrimaryNode>) {
        match primary.node {
            PrimaryNode::Identifier(ident) => {
                self.check_identifier_reference(TypedNodeRef::new(primary.id, ident));
            }
            PrimaryNode::Array(arr) => {
                let length = self.nodes.array.size(arr.elements);
                for i in 0..length {
                    if let Some(elem_id) = self.nodes.array.get_node_id_at(arr.elements, i) {
                        let elem = self.nodes.get_expr_node(elem_id);
                        self.check_expression(elem);
                    }
                }
            }
            PrimaryNode::Object(obj) => {
                let length = self.nodes.array.size(obj.entries);
                for i in 0..length {
                    if let Some(entry_id) = self.nodes.array.get_node_id_at(obj.entries, i) {
                        let entry = self.nodes.get_obj_entry_node(entry_id);
                        let value = self.nodes.get_expr_node(entry.node.value);
                        self.check_expression(value);
                    }
                }
            }
            PrimaryNode::Grouping(group) => {
                let inner = self.nodes.get_expr_node(group.expr);
                self.check_expression(inner);
            }
            PrimaryNode::Lambda(lambda) => {
                let params_length = self.nodes.array.size(lambda.parameters);
                for i in 0..params_length {
                    if let Some(param_id) = self.nodes.array.get_node_id_at(lambda.parameters, i) {
                        let param = self.nodes.get_identifier_node(param_id);
                        self.check_parameter(param);
                    }
                }

                let node = self.nodes.get_node(lambda.body);
                if matches!(node, AstNode::BlockStmt(_)) {
                    let block = self.nodes.get_block_stmt_node(lambda.body);
                    self.check_block(block);
                } else {
                    let expr = self.nodes.get_expr_node(lambda.body);
                    self.check_expression(expr);
                }
            }
            PrimaryNode::When(when) => {
                if let Some(value_id) = when.value {
                    let subject = self.nodes.get_expr_node(value_id);
                    self.check_expression(subject);
                }

                let branch_length = self.nodes.array.size(when.branches);
                for i in 0..branch_length {
                    if let Some(branch_id) = self.nodes.array.get_node_id_at(when.branches, i) {
                        let branch = self.nodes.get_when_branch_node(branch_id);
                        let cond = self.nodes.get_expr_node(branch.node.condition);
                        self.check_expression(cond);

                        let body = self.nodes.get_expr_node(branch.node.body);
                        self.check_expression(body);
                    }
                }

                if let Some(else_id) = when.else_branch {
                    let else_expr = self.nodes.get_expr_node(else_id);
                    self.check_expression(else_expr);
                }
            }
            PrimaryNode::Super(super_expr) => {
                // super.field or super.method() - check the method/field identifier
                let method_ident = self.nodes.get_identifier_node(super_expr.method);
                if self.span_contains(method_ident.node.span) {
                    if self.is_better_match(method_ident.node.span) {
                        let name = self.strings.get_string(method_ident.node.name).to_string();
                        let kind = self.find_super_property_kind(&name);

                        self.best_match = Some(NodeInfo {
                            node_id: method_ident.id,
                            range: self.span_to_range(method_ident.node.span),
                            kind,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    fn check_identifier_reference(&mut self, identifier: TypedNodeRef<IdentifierNode>) {
        if !self.span_contains(identifier.node.span) {
            return;
        }

        if !self.is_better_match(identifier.node.span) {
            return;
        }

        let name = self.strings.get_string(identifier.node.name);

        // Check if this is a native function
        if let Some(_func_info) = builtins::get_native_function(name) {
            self.best_match = Some(NodeInfo {
                node_id: identifier.id,
                range: self.span_to_range(identifier.node.span),
                kind: NodeKind::NativeFunction(name.to_string()),
            });
            return;
        }

        // Check if this is a stdlib symbol
        let stdlib = stdlib_analyzer::get_stdlib_cache();
        if let Some(symbol) = stdlib.get(name) {
            let kind = match symbol {
                stdlib_analyzer::StdlibSymbol::Class { name } => NodeKind::StdlibClass(name.clone()),
                stdlib_analyzer::StdlibSymbol::Function { name, .. } => NodeKind::StdlibFunction(name.clone()),
                stdlib_analyzer::StdlibSymbol::Method { method_name, .. } => NodeKind::StdlibMethod(method_name.clone()),
            };
            self.best_match = Some(NodeInfo {
                node_id: identifier.id,
                range: self.span_to_range(identifier.node.span),
                kind,
            });
            return;
        }

        // Try to resolve this identifier using the symbol table
        if let Some(symbol_info) = self.symbol_table.resolve(identifier.id) {
            let kind = match symbol_info.kind {
                SymbolKind::Variable => NodeKind::Variable(name.to_string()),
                SymbolKind::Parameter => NodeKind::Parameter(name.to_string()),
                SymbolKind::Function => NodeKind::Function(name.to_string()),
                SymbolKind::Class => NodeKind::Class(name.to_string()),
                SymbolKind::Field => NodeKind::Field(name.to_string()),
                SymbolKind::Module => NodeKind::Module(name.to_string()),
            };

            self.best_match = Some(NodeInfo {
                node_id: symbol_info.decl_node_id,
                range: self.span_to_range(identifier.node.span),
                kind,
            });
        }
    }

    /// Determine if a property access is a method or field by finding the class definition
    fn find_property_kind(
        &self,
        callee: TypedNodeRef<ExprNode>,
        property_name: &str,
    ) -> NodeKind {
        // Try to find the class definition by resolving the callee
        if let Some(class_node_id) = self.find_class_of_expression(callee) {
            let decl = self.nodes.get_decl_node(class_node_id);

            // Check if it's actually a class declaration
            if let DeclNode::Class(class_decl) = decl.node {
                // Search through class members for this property
                let members_length = self.nodes.array.size(class_decl.members);
                for i in 0..members_length {
                    if let Some(member_id) =
                        self.nodes.array.get_node_id_at(class_decl.members, i)
                    {
                        let member = self.nodes.get_class_member_node(member_id);

                        match member.node {
                            ClassMemberNode::Method(method) => {
                                let name_node = self.nodes.get_identifier_node(method.name);
                                let member_name =
                                    self.strings.get_string(name_node.node.name);
                                if member_name == property_name {
                                    return NodeKind::Function(property_name.to_string());
                                }
                            }
                            ClassMemberNode::Field(field) => {
                                let name_node = self.nodes.get_identifier_node(field.name);
                                let member_name =
                                    self.strings.get_string(name_node.node.name);
                                if member_name == property_name {
                                    return NodeKind::Field(property_name.to_string());
                                }
                            }
                        }
                    }
                }
            }
        }

        // Default to Field if we can't determine
        NodeKind::Field(property_name.to_string())
    }

    /// Find the class definition for an expression (if it's a class instance)
    fn find_class_of_expression(&self, expr: TypedNodeRef<ExprNode>) -> Option<NodeId> {
        match expr.node {
            ExprNode::Primary(PrimaryNode::Identifier(_ident)) => {
                // Resolve the identifier to its declaration
                if let Some(symbol_info) = self.symbol_table.resolve(expr.id) {
                    // If it's a variable, we need to find its initializer to get the type
                    // For now, we'll check if the declaration is a class
                    if matches!(symbol_info.kind, SymbolKind::Class) {
                        return Some(symbol_info.decl_node_id);
                    }

                    // Try to find the variable's type by looking at its initializer
                    let decl_node = self.nodes.get_node(symbol_info.decl_node_id);
                    if let AstNode::VariableDecl(var_decl) = decl_node {
                        if let Some(init_id) = var_decl.initializer {
                            return self.find_class_from_initializer(init_id);
                        }
                    }
                }
                None
            }
            ExprNode::Call(_) => {
                // If it's a call expression, the result type depends on what's being called
                // For now, we don't track return types, so we can't determine this
                None
            }
            _ => None,
        }
    }

    /// Find the class from a variable initializer (e.g., `var user = Person()`)
    fn find_class_from_initializer(&self, init_id: NodeId) -> Option<NodeId> {
        let init_expr = self.nodes.get_expr_node(init_id);

        match init_expr.node {
            ExprNode::Call(call) => {
                // Check if the callee is a class constructor call
                let callee = self.nodes.get_expr_node(call.callee);
                if let ExprNode::Primary(PrimaryNode::Identifier(_)) = callee.node {
                    // Resolve the identifier
                    if let Some(symbol_info) = self.symbol_table.resolve(callee.id) {
                        if matches!(symbol_info.kind, SymbolKind::Class) {
                            return Some(symbol_info.decl_node_id);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Find a property in the superclass of the current class (for super.property)
    fn find_super_property_kind(&self, property_name: &str) -> NodeKind {

        // Get the current class we're in
        if let Some(current_class_id) = self.current_class {
            let decl = self.nodes.get_decl_node(current_class_id);

            if let DeclNode::Class(class_decl) = decl.node {
                // Get the superclass if it exists
                if let Some(superclass_id) = class_decl.superclass {
                    let superclass_ident = self.nodes.get_identifier_node(superclass_id);

                    // Resolve the superclass identifier to its declaration
                    if let Some(symbol_info) = self.symbol_table.resolve(superclass_ident.id) {
                        if matches!(symbol_info.kind, SymbolKind::Class) {
                            let superclass_decl = self.nodes.get_decl_node(symbol_info.decl_node_id);

                            if let DeclNode::Class(superclass) = superclass_decl.node {
                                // Search for the property in the superclass members
                                let members_length = self.nodes.array.size(superclass.members);
                                for i in 0..members_length {
                                    if let Some(member_id) = self.nodes.array.get_node_id_at(superclass.members, i) {
                                        let member = self.nodes.get_class_member_node(member_id);

                                        match member.node {
                                            ClassMemberNode::Method(method) => {
                                                let name_node = self.nodes.get_identifier_node(method.name);
                                                let member_name = self.strings.get_string(name_node.node.name);
                                                if member_name == property_name {
                                                    return NodeKind::Function(property_name.to_string());
                                                }
                                            }
                                            ClassMemberNode::Field(field) => {
                                                let name_node = self.nodes.get_identifier_node(field.name);
                                                let member_name = self.strings.get_string(name_node.node.name);
                                                if member_name == property_name {
                                                    return NodeKind::Field(property_name.to_string());
                                                }
                                            }
                                        }
                                    }
                                }
                            } else {
                            }
                        } else {
                        }
                    } else {
                    }
                } else {
                }
            } else {
            }
        } else {
        }

        // Default to Field if we can't determine
        NodeKind::Field(property_name.to_string())
    }

    fn check_variable(&mut self, var: TypedNodeRef<VariableDeclNode>) {
        // Check the variable name itself
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

        // Also check the variable's initializer expression (e.g., Person(...) in var user = Person(...))
        if let Some(init_id) = var.node.initializer {
            let init_expr = self.nodes.get_expr_node(init_id);
            self.check_expression(init_expr);
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

/// Get the identifier span (range) for a declaration node
/// This is used by "Go to Definition" to get the declaration's actual location
pub fn get_declaration_range(
    analysis: &AnalysisResult,
    decl_node_id: NodeId,
) -> Option<Range> {
    let node = analysis.nodes.get_node(decl_node_id);

    let identifier_id = match node {
        AstNode::VariableDecl(var_decl) => var_decl.target,
        AstNode::FunctionDecl(func_decl) => {
            let func_expr = analysis.nodes.get_func_expr_node(func_decl.function);
            func_expr.node.name
        }
        AstNode::Class(class_decl) => class_decl.name,
        AstNode::FieldDecl(field_decl) => field_decl.name,
        AstNode::LambdaDecl(lambda_decl) => lambda_decl.name,
        AstNode::ImportModuleDecl(module_decl) => module_decl.name,
        AstNode::Identifier(_) => {
            // If it's already an identifier (e.g., parameter), use it directly
            decl_node_id
        }
        _ => return None,
    };

    // Get the identifier node and convert its span to a Range
    let identifier = analysis.nodes.get_identifier_node(identifier_id);
    let span = identifier.node.span;

    Some(Range {
        start: Position {
            line: analysis.source_map.get_line_number(span.start) - 1,
            character: analysis.source_map.get_column_number(span.start) - 1,
        },
        end: Position {
            line: analysis.source_map.get_line_number(span.end) - 1,
            character: analysis.source_map.get_column_number(span.end) - 1,
        },
    })
}

/// Format hover information for display
pub fn format_hover_info(_analysis: &AnalysisResult, info: &NodeInfo) -> String {
    match &info.kind {
        NodeKind::Class(name) => format!("```qanglang\nclass {}\n```", name),
        NodeKind::Function(name) => format!("```qanglang\nfn {}\n```", name),
        NodeKind::Variable(name) => format!("```qanglang\nvar {}\n```", name),
        NodeKind::Field(name) => format!("```qanglang\nfield {}\n```", name),
        NodeKind::Parameter(name) => format!("```qanglang\nparam {}\n```", name),
        NodeKind::Module(name) => format!("```qanglang\nmod {}\n```", name),
        NodeKind::ImportPath(path) => format!("```qanglang\nimport(\"{}\")\n```", path),
        NodeKind::NativeFunction(name) => format!("```qanglang\nfn {}\n```", name),
        NodeKind::IntrinsicMethod(name) => format!("```qanglang\nfn {}\n```", name),
        NodeKind::StdlibClass(name) => format!("```qanglang\nclass {}\n```", name),
        NodeKind::StdlibFunction(name) => format!("```qanglang\nfn {}\n```", name),
        NodeKind::StdlibMethod(name) => format!("```qanglang\nfn {}\n```", name),
    }
}
