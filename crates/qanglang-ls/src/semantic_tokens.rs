use crate::analyzer::AnalysisResult;
use qanglang_core::nodes::*;
use qanglang_core::symbol_resolver::{SymbolKind, SymbolTable};
use qanglang_core::{AstNodeArena, NodeArrayId, SourceMap, TypedNodeRef};
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

/// Collect semantic tokens from analysis result
pub fn collect_semantic_tokens(analysis: &AnalysisResult) -> Vec<SemanticToken> {
    let module = analysis.nodes.get_program_node(analysis.root_module_id);

    let mut collector = SemanticTokenCollector {
        tokens: Vec::new(),
        nodes: &analysis.nodes,
        symbol_table: &analysis.symbol_table,
    };

    collector.collect_from_module(module);

    // Sort tokens by position (required by LSP spec)
    collector.tokens.sort_by_key(|t| t.start);

    // Convert to delta-encoded format
    delta_encode_tokens(&collector.tokens, &analysis.source_map)
}

/// Raw token with absolute position (before delta encoding)
#[derive(Debug, Clone)]
struct RawToken {
    start: usize,    // byte offset in source
    length: usize,   // token length
    token_type: u32, // index into LEGEND_TYPE
    modifiers: u32,  // bitfield of token modifiers
}

struct SemanticTokenCollector<'a> {
    tokens: Vec<RawToken>,
    nodes: &'a AstNodeArena,
    symbol_table: &'a SymbolTable,
}

impl<'a> SemanticTokenCollector<'a> {
    fn collect_from_module(&mut self, module: TypedNodeRef<Module>) {
        let length = self.nodes.array.size(module.node.decls);

        for i in 0..length {
            if let Some(node_id) = self.nodes.array.get_node_id_at(module.node.decls, i) {
                let decl = self.nodes.get_decl_node(node_id);
                self.process_decl(decl);
            }
        }
    }

    fn process_decl(&mut self, decl: TypedNodeRef<DeclNode>) {
        match decl.node {
            DeclNode::Class(class) => self.process_class(TypedNodeRef::new(decl.id, class)),
            DeclNode::Function(func) => {
                self.process_function_decl(TypedNodeRef::new(decl.id, func))
            }
            DeclNode::Variable(var) => self.process_variable(TypedNodeRef::new(decl.id, var)),
            DeclNode::Lambda(lambda) => {
                self.process_lambda_decl(TypedNodeRef::new(decl.id, lambda))
            }
            DeclNode::Stmt(stmt) => {
                // Process statements wrapped in declarations
                self.process_statement(TypedNodeRef::new(decl.id, stmt));
            }
            DeclNode::Module(_) => {
                // Import declarations - could highlight import keyword + module name in future
            }
        }
    }

    fn process_class(&mut self, class: TypedNodeRef<ClassDeclNode>) {
        // Add token for class name
        let name_node = self.nodes.get_identifier_node(class.node.name);
        self.add_token(
            name_node.node.span.start,
            name_node.node.span.end - name_node.node.span.start,
            SemanticTokenType::CLASS,
        );

        // Process superclass if present
        if let Some(superclass_id) = class.node.superclass {
            let superclass_node = self.nodes.get_identifier_node(superclass_id);
            self.add_token(
                superclass_node.node.span.start,
                superclass_node.node.span.end - superclass_node.node.span.start,
                SemanticTokenType::CLASS,
            );
        }

        // Process class members
        let length = self.nodes.array.size(class.node.members);
        for i in 0..length {
            if let Some(member_id) = self.nodes.array.get_node_id_at(class.node.members, i) {
                let member = self.nodes.get_class_member_node(member_id);
                self.process_class_member(member);
            }
        }
    }

    fn process_class_member(&mut self, member: TypedNodeRef<ClassMemberNode>) {
        match member.node {
            ClassMemberNode::Method(method) => {
                self.process_method(TypedNodeRef::new(member.id, method))
            }
            ClassMemberNode::Field(field) => {
                self.process_field(TypedNodeRef::new(member.id, field))
            }
        }
    }

    fn process_method(&mut self, method: TypedNodeRef<FunctionExprNode>) {
        // Method name
        let name_node = self.nodes.get_identifier_node(method.node.name);
        self.add_token(
            name_node.node.span.start,
            name_node.node.span.end - name_node.node.span.start,
            SemanticTokenType::METHOD,
        );

        // Parameters
        self.process_parameters(method.node.parameters);

        // Body - we'll process the block
        let body = self.nodes.get_block_stmt_node(method.node.body);
        self.process_block(body);
    }

    fn process_field(&mut self, field: TypedNodeRef<FieldDeclNode>) {
        let name_node = self.nodes.get_identifier_node(field.node.name);
        self.add_token(
            name_node.node.span.start,
            name_node.node.span.end - name_node.node.span.start,
            SemanticTokenType::PROPERTY,
        );

        // Process initializer if present
        if let Some(init_id) = field.node.initializer {
            let init_expr = self.nodes.get_expr_node(init_id);
            self.process_expression(init_expr);
        }
    }

    fn process_function_decl(&mut self, func: TypedNodeRef<FunctionDeclNode>) {
        let func_expr = self.nodes.get_func_expr_node(func.node.function);

        // Function name
        let name_node = self.nodes.get_identifier_node(func_expr.node.name);
        self.add_token(
            name_node.node.span.start,
            name_node.node.span.end - name_node.node.span.start,
            SemanticTokenType::FUNCTION,
        );

        // Parameters
        self.process_parameters(func_expr.node.parameters);

        // Body
        let body = self.nodes.get_block_stmt_node(func_expr.node.body);
        self.process_block(body);
    }

    fn process_lambda_decl(&mut self, lambda: TypedNodeRef<LambdaDeclNode>) {
        // Lambda name
        let name_node = self.nodes.get_identifier_node(lambda.node.name);
        self.add_token(
            name_node.node.span.start,
            name_node.node.span.end - name_node.node.span.start,
            SemanticTokenType::FUNCTION,
        );

        // Process the lambda expression itself
        let lambda_expr = self.nodes.get_lambda_expr_node(lambda.node.lambda);
        self.process_parameters(lambda_expr.node.parameters);

        // Body is an expression (not a statement!)
        let body = self.nodes.get_expr_node(lambda_expr.node.body);
        self.process_expression(body);
    }

    fn process_variable(&mut self, var: TypedNodeRef<VariableDeclNode>) {
        let name_node = self.nodes.get_identifier_node(var.node.target);
        self.add_token(
            name_node.node.span.start,
            name_node.node.span.end - name_node.node.span.start,
            SemanticTokenType::VARIABLE,
        );

        // Process initializer
        if let Some(init_id) = var.node.initializer {
            let init_expr = self.nodes.get_expr_node(init_id);
            self.process_expression(init_expr);
        }
    }

    fn process_parameters(&mut self, parameters: NodeArrayId) {
        let length = self.nodes.array.size(parameters);
        for i in 0..length {
            if let Some(param_id) = self.nodes.array.get_node_id_at(parameters, i) {
                let param_node = self.nodes.get_identifier_node(param_id);
                self.add_token(
                    param_node.node.span.start,
                    param_node.node.span.end - param_node.node.span.start,
                    SemanticTokenType::PARAMETER,
                );
            }
        }
    }

    fn process_block(&mut self, block: TypedNodeRef<BlockStmtNode>) {
        let length = self.nodes.array.size(block.node.decls);
        for i in 0..length {
            if let Some(decl_id) = self.nodes.array.get_node_id_at(block.node.decls, i) {
                let decl = self.nodes.get_decl_node(decl_id);
                self.process_decl(decl);
            }
        }
    }

    fn process_statement(&mut self, stmt: TypedNodeRef<StmtNode>) {
        match stmt.node {
            StmtNode::Expr(expr_stmt) => {
                let expr = self.nodes.get_expr_node(expr_stmt.expr);
                self.process_expression(expr);
            }
            StmtNode::Block(block) => {
                self.process_block(TypedNodeRef::new(stmt.id, block));
            }
            StmtNode::If(if_stmt) => {
                // Condition
                let cond = self.nodes.get_expr_node(if_stmt.condition);
                self.process_expression(cond);

                // Then branch
                let then_branch = self.nodes.get_stmt_node(if_stmt.then_branch);
                self.process_statement(then_branch);

                // Else branch
                if let Some(else_id) = if_stmt.else_branch {
                    let else_branch = self.nodes.get_stmt_node(else_id);
                    self.process_statement(else_branch);
                }
            }
            StmtNode::While(while_stmt) => {
                let cond = self.nodes.get_expr_node(while_stmt.condition);
                self.process_expression(cond);

                let body = self.nodes.get_stmt_node(while_stmt.body);
                self.process_statement(body);
            }
            StmtNode::For(for_stmt) => {
                // Initializer can be either VariableDeclNode or ExprNode
                if let Some(init_id) = for_stmt.initializer {
                    self.process_for_initializer(init_id);
                }
                // Condition is an expression
                if let Some(cond_id) = for_stmt.condition {
                    let cond = self.nodes.get_expr_node(cond_id);
                    self.process_expression(cond);
                }
                // Afterthought is an expression
                if let Some(after_id) = for_stmt.afterthought {
                    let after = self.nodes.get_expr_node(after_id);
                    self.process_expression(after);
                }
                // Body is a statement
                let body = self.nodes.get_stmt_node(for_stmt.body);
                self.process_statement(body);
            }
            StmtNode::Return(ret_stmt) => {
                if let Some(value_id) = ret_stmt.value {
                    let value = self.nodes.get_expr_node(value_id);
                    self.process_expression(value);
                }
            }
            StmtNode::Break(_) | StmtNode::Continue(_) => {
                // No tokens to add
            }
        }
    }

    fn process_expression(&mut self, expr: TypedNodeRef<ExprNode>) {
        match expr.node {
            ExprNode::Primary(primary) => {
                self.process_primary(TypedNodeRef::new(expr.id, primary));
            }
            ExprNode::Call(call) => {
                // Process callee
                let callee = self.nodes.get_expr_node(call.callee);
                self.process_expression(callee);

                // Process call operation which may contain arguments
                let op = self.nodes.get_call_operation_node(call.operation);
                match op.node {
                    CallOperationNode::Call(call_node) => {
                        let length = self.nodes.array.size(call_node.args);
                        for i in 0..length {
                            if let Some(arg_id) = self.nodes.array.get_node_id_at(call_node.args, i)
                            {
                                let arg = self.nodes.get_expr_node(arg_id);
                                self.process_expression(arg);
                            }
                        }
                    }
                    CallOperationNode::Property(prop) => {
                        // Property access - highlight the property
                        let name_node = self.nodes.get_identifier_node(prop.identifier);
                        self.add_token(
                            name_node.node.span.start,
                            name_node.node.span.end - name_node.node.span.start,
                            SemanticTokenType::PROPERTY,
                        );
                    }
                    CallOperationNode::Index(index) => {
                        // Index access
                        let index_expr = self.nodes.get_expr_node(index.index);
                        self.process_expression(index_expr);
                    }
                    _ => {
                        // Skip other call operation types for now
                    }
                }
            }
            ExprNode::Assignment(assign) => {
                // Process target
                let target = self.nodes.get_assignment_target_node(assign.target);
                self.process_assignment_target(target);

                // Process value
                let value = self.nodes.get_expr_node(assign.value);
                self.process_expression(value);
            }
            ExprNode::Unary(unary) => {
                let operand = self.nodes.get_expr_node(unary.operand);
                self.process_expression(operand);
            }
            ExprNode::LogicalOr(logical) => {
                let left = self.nodes.get_expr_node(logical.left);
                self.process_expression(left);

                let right = self.nodes.get_expr_node(logical.right);
                self.process_expression(right);
            }
            ExprNode::LogicalAnd(logical) => {
                let left = self.nodes.get_expr_node(logical.left);
                self.process_expression(left);

                let right = self.nodes.get_expr_node(logical.right);
                self.process_expression(right);
            }
            ExprNode::Ternary(ternary) => {
                let condition = self.nodes.get_expr_node(ternary.condition);
                self.process_expression(condition);

                let then_expr = self.nodes.get_expr_node(ternary.then_expr);
                self.process_expression(then_expr);

                let else_expr = self.nodes.get_expr_node(ternary.else_expr);
                self.process_expression(else_expr);
            }
            _ => {
                // Skip other expression types for now
            }
        }
    }

    fn process_assignment_target(&mut self, target: TypedNodeRef<AssignmentTargetNode>) {
        match target.node {
            AssignmentTargetNode::Identifier(ident) => {
                // Resolve identifier using symbol table
                if let Some(symbol_info) = self.symbol_table.resolve(target.id) {
                    let token_type = match symbol_info.kind {
                        SymbolKind::Variable => SemanticTokenType::VARIABLE,
                        SymbolKind::Parameter => SemanticTokenType::PARAMETER,
                        SymbolKind::Function => SemanticTokenType::FUNCTION,
                        SymbolKind::Class => SemanticTokenType::CLASS,
                        SymbolKind::Field => SemanticTokenType::PROPERTY,
                    };

                    self.add_token(
                        ident.span.start,
                        ident.span.end - ident.span.start,
                        token_type,
                    );
                }
            }
            AssignmentTargetNode::Property(prop) => {
                // Process the object part
                let object = self.nodes.get_expr_node(prop.object);
                self.process_expression(object);
                // Property name is already handled as PROPERTY in earlier code
            }
            AssignmentTargetNode::Index(index) => {
                // Process object and index expressions
                let object = self.nodes.get_expr_node(index.object);
                self.process_expression(object);

                let index_expr = self.nodes.get_expr_node(index.index);
                self.process_expression(index_expr);
            }
        }
    }

    fn process_primary(&mut self, primary: TypedNodeRef<PrimaryNode>) {
        match primary.node {
            PrimaryNode::Number(num) => {
                self.add_token(
                    num.span.start,
                    num.span.end - num.span.start,
                    SemanticTokenType::NUMBER,
                );
            }
            PrimaryNode::String(s) => {
                self.add_token(
                    s.span.start,
                    s.span.end - s.span.start,
                    SemanticTokenType::STRING,
                );
            }
            PrimaryNode::Boolean(b) => {
                self.add_token(
                    b.span.start,
                    b.span.end - b.span.start,
                    SemanticTokenType::KEYWORD,
                );
            }
            PrimaryNode::Nil(n) => {
                self.add_token(
                    n.span.start,
                    n.span.end - n.span.start,
                    SemanticTokenType::KEYWORD,
                );
            }
            PrimaryNode::Identifier(ident) => {
                // Resolve identifier using symbol table to determine correct token type
                if let Some(symbol_info) = self.symbol_table.resolve(primary.id) {
                    let token_type = match symbol_info.kind {
                        SymbolKind::Variable => SemanticTokenType::VARIABLE,
                        SymbolKind::Parameter => SemanticTokenType::PARAMETER,
                        SymbolKind::Function => SemanticTokenType::FUNCTION,
                        SymbolKind::Class => SemanticTokenType::CLASS,
                        SymbolKind::Field => SemanticTokenType::PROPERTY,
                    };

                    self.add_token(
                        ident.span.start,
                        ident.span.end - ident.span.start,
                        token_type,
                    );
                }
                // If unresolved, skip (could be builtin or error)
            }
            PrimaryNode::This(this) => {
                self.add_token(
                    this.span.start,
                    this.span.end - this.span.start,
                    SemanticTokenType::KEYWORD,
                );
            }
            PrimaryNode::Super(sup) => {
                // super.method or super.field
                // The span covers the whole expression, but we need to tokenize:
                // 1. "super" keyword
                // 2. method/field identifier

                // Get the method/field identifier
                let method_ident = self.nodes.get_identifier_node(sup.method);

                // Calculate the length of "super" keyword
                // The identifier starts after "super.", so we can determine where "super" ends
                let super_keyword_length = method_ident.node.span.start - sup.span.start - 1; // -1 for the '.'

                // Add token for "super" keyword
                self.add_token(
                    sup.span.start,
                    super_keyword_length,
                    SemanticTokenType::KEYWORD,
                );

                // Add token for the method/field identifier as PROPERTY
                // (we can't distinguish method vs field without type information)
                self.add_token(
                    method_ident.node.span.start,
                    method_ident.node.span.end - method_ident.node.span.start,
                    SemanticTokenType::PROPERTY,
                );
            }
            PrimaryNode::Array(arr) => {
                // Process array elements
                let length = self.nodes.array.size(arr.elements);
                for i in 0..length {
                    if let Some(elem_id) = self.nodes.array.get_node_id_at(arr.elements, i) {
                        let elem = self.nodes.get_expr_node(elem_id);
                        self.process_expression(elem);
                    }
                }
            }
            PrimaryNode::Object(obj) => {
                // Process object properties
                let length = self.nodes.array.size(obj.entries);
                for i in 0..length {
                    if let Some(entry_id) = self.nodes.array.get_node_id_at(obj.entries, i) {
                        let entry = self.nodes.get_obj_entry_node(entry_id);
                        self.process_object_entry(entry);
                    }
                }
            }
            PrimaryNode::Grouping(group) => {
                let inner = self.nodes.get_expr_node(group.expr);
                self.process_expression(inner);
            }
            PrimaryNode::Lambda(lambda) => {
                // Process lambda parameters
                self.process_parameters(lambda.parameters);

                // Process lambda body - can be either BlockStmt or Expr
                // We need to handle both cases
                self.process_lambda_body(lambda.body);
            }
            PrimaryNode::When(when) => {
                // Process when expression subject if present
                if let Some(value_id) = when.value {
                    let subject = self.nodes.get_expr_node(value_id);
                    self.process_expression(subject);
                }

                // Process branches
                let branch_length = self.nodes.array.size(when.branches);
                for i in 0..branch_length {
                    if let Some(branch_id) = self.nodes.array.get_node_id_at(when.branches, i) {
                        let branch = self.nodes.get_when_branch_node(branch_id);
                        self.process_when_branch(branch);
                    }
                }

                // Process else branch if present
                if let Some(else_id) = when.else_branch {
                    let else_expr = self.nodes.get_expr_node(else_id);
                    self.process_expression(else_expr);
                }
            }
        }
    }

    fn process_object_entry(&mut self, entry: TypedNodeRef<ObjectEntryNode>) {
        // Property key
        let key_node = self.nodes.get_identifier_node(entry.node.key);
        self.add_token(
            key_node.node.span.start,
            key_node.node.span.end - key_node.node.span.start,
            SemanticTokenType::PROPERTY,
        );

        // Property value
        let value = self.nodes.get_expr_node(entry.node.value);
        self.process_expression(value);
    }

    fn process_when_branch(&mut self, branch: TypedNodeRef<WhenBranchNode>) {
        // Process condition
        let cond = self.nodes.get_expr_node(branch.node.condition);
        self.process_expression(cond);

        // Process body (it's an expression, not a statement!)
        let body = self.nodes.get_expr_node(branch.node.body);
        self.process_expression(body);
    }

    fn process_lambda_body(&mut self, body_id: qanglang_core::NodeId) {
        // Lambda bodies can be either BlockStmt or Expr
        let node = self.nodes.get_node(body_id);

        // Check if it's a BlockStmt
        if matches!(node, AstNode::BlockStmt(_)) {
            let block = self.nodes.get_block_stmt_node(body_id);
            self.process_block(block);
        } else {
            // It's an expression
            let expr = self.nodes.get_expr_node(body_id);
            self.process_expression(expr);
        }
    }

    fn process_for_initializer(&mut self, init_id: qanglang_core::NodeId) {
        // For loop initializers can be either VariableDeclNode or ExprNode
        let node = self.nodes.get_node(init_id);

        // Check if it's a VariableDecl
        if matches!(node, AstNode::VariableDecl(_)) {
            let var_decl = self.nodes.get_var_decl_node(init_id);
            self.process_variable(var_decl);
        } else {
            // It's an expression
            let expr = self.nodes.get_expr_node(init_id);
            self.process_expression(expr);
        }
    }

    fn add_token(&mut self, start: usize, length: usize, token_type: SemanticTokenType) {
        let token_type_idx = crate::server::token_type_index(token_type);

        self.tokens.push(RawToken {
            start,
            length,
            token_type: token_type_idx,
            modifiers: 0,
        });
    }
}

/// Convert absolute-positioned tokens to LSP delta-encoded format
fn delta_encode_tokens(tokens: &[RawToken], source_map: &SourceMap) -> Vec<SemanticToken> {
    let mut result = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for token in tokens {
        // Convert to 0-based line and column (SourceMap returns 1-based)
        let line = (source_map.get_line_number(token.start) - 1) as u32;
        let start = (source_map.get_column_number(token.start) - 1) as u32;

        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 {
            start - prev_start
        } else {
            start
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length: token.length as u32,
            token_type: token.token_type,
            token_modifiers_bitset: token.modifiers,
        });

        prev_line = line;
        prev_start = start;
    }

    result
}
