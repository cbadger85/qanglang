use std::collections::HashMap;

use crate::nodes::*;
use crate::{AstNodeArena, NodeId, StringInterner, TypedNodeRef};

/// Information about a symbol's declaration
#[derive(Debug, Clone)]
pub struct SymbolInfo {
    /// The NodeId of the declaration
    pub decl_node_id: NodeId,
    /// The kind of declaration
    pub kind: SymbolKind,
    /// The scope depth where this symbol was declared
    pub scope_depth: usize,
}

/// The kind of symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Parameter,
    Function,
    Class,
    Field,
}

/// Maps identifier references to their declarations
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// Map from identifier NodeId (usage site) to declaration info
    resolutions: HashMap<NodeId, SymbolInfo>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            resolutions: HashMap::new(),
        }
    }

    /// Record that an identifier reference resolves to a declaration
    pub fn record_resolution(&mut self, identifier_id: NodeId, info: SymbolInfo) {
        self.resolutions.insert(identifier_id, info);
    }

    /// Look up what an identifier reference resolves to
    pub fn resolve(&self, identifier_id: NodeId) -> Option<&SymbolInfo> {
        self.resolutions.get(&identifier_id)
    }

    /// Get all resolutions (for debugging)
    pub fn all_resolutions(&self) -> &HashMap<NodeId, SymbolInfo> {
        &self.resolutions
    }
}

/// Resolves all identifier references in an AST to their declarations
pub struct SymbolResolver<'a> {
    nodes: &'a AstNodeArena,
    #[allow(dead_code)] // Keep for future use (e.g., debugging)
    strings: &'a StringInterner,
    symbol_table: SymbolTable,
    /// Stack of scopes, each scope maps name → symbol info
    scopes: Vec<HashMap<u32, SymbolInfo>>, // u32 is the string handle
    /// Current scope depth
    scope_depth: usize,
}

impl<'a> SymbolResolver<'a> {
    pub fn new(nodes: &'a AstNodeArena, strings: &'a StringInterner) -> Self {
        Self {
            nodes,
            strings,
            symbol_table: SymbolTable::new(),
            scopes: vec![HashMap::new()], // Start with global scope
            scope_depth: 0,
        }
    }

    /// Resolve all symbols in a module and return the symbol table
    pub fn resolve_module(mut self, module: TypedNodeRef<Module>) -> SymbolTable {
        // First pass: Declare all hoisted symbols (classes, functions, lambdas)
        self.declare_hoisted_symbols(module);

        // Second pass: Resolve identifier references and process bodies
        self.visit_module(module);

        self.symbol_table
    }

    /// First pass: Declare all hoisted symbols at module scope
    fn declare_hoisted_symbols(&mut self, module: TypedNodeRef<Module>) {
        let length = self.nodes.array.size(module.node.decls);

        for i in 0..length {
            if let Some(node_id) = self.nodes.array.get_node_id_at(module.node.decls, i) {
                let decl = self.nodes.get_decl_node(node_id);

                match decl.node {
                    DeclNode::Class(class) => {
                        let name_node = self.nodes.get_identifier_node(class.name);
                        self.declare_symbol(
                            name_node.node.name,
                            SymbolInfo {
                                decl_node_id: decl.id,
                                kind: SymbolKind::Class,
                                scope_depth: self.scope_depth,
                            },
                        );
                    }
                    DeclNode::Function(func) => {
                        let func_expr = self.nodes.get_func_expr_node(func.function);
                        let name_node = self.nodes.get_identifier_node(func_expr.node.name);
                        self.declare_symbol(
                            name_node.node.name,
                            SymbolInfo {
                                decl_node_id: decl.id,
                                kind: SymbolKind::Function,
                                scope_depth: self.scope_depth,
                            },
                        );
                    }
                    _ => {
                        // Variables, lambdas, and statements are not hoisted
                        // Lambdas are variable declarations (lambda name = ...)
                    }
                }
            }
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
        self.scope_depth -= 1;
    }

    /// Declare a symbol in the current scope
    fn declare_symbol(&mut self, name: u32, info: SymbolInfo) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, info);
        }
    }

    /// Resolve an identifier to its declaration (searching scopes from innermost to outermost)
    fn resolve_identifier(&mut self, identifier: TypedNodeRef<IdentifierNode>) {
        let name = identifier.node.name;

        // Search from innermost scope to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(&name) {
                // Found the declaration - record the resolution
                self.symbol_table.record_resolution(identifier.id, info.clone());
                return;
            }
        }

        // Not found - identifier is unresolved (could be a builtin or error)
        // Don't record anything for unresolved identifiers
    }

    fn visit_module(&mut self, module: TypedNodeRef<Module>) {
        let length = self.nodes.array.size(module.node.decls);

        for i in 0..length {
            if let Some(node_id) = self.nodes.array.get_node_id_at(module.node.decls, i) {
                let decl = self.nodes.get_decl_node(node_id);
                self.visit_decl(decl);
            }
        }
    }

    fn visit_decl(&mut self, decl: TypedNodeRef<DeclNode>) {
        match decl.node {
            DeclNode::Class(class) => self.visit_class(TypedNodeRef::new(decl.id, class)),
            DeclNode::Function(func) => self.visit_function(TypedNodeRef::new(decl.id, func)),
            DeclNode::Variable(var) => self.visit_variable(TypedNodeRef::new(decl.id, var)),
            DeclNode::Lambda(lambda) => self.visit_lambda_decl(TypedNodeRef::new(decl.id, lambda)),
            DeclNode::Stmt(stmt) => self.visit_statement(TypedNodeRef::new(decl.id, stmt)),
            DeclNode::Module(_) => {
                // Import declarations - nothing to resolve yet
            }
        }
    }

    fn visit_class(&mut self, class: TypedNodeRef<ClassDeclNode>) {
        // Note: Class name already declared in hoisting pass, don't redeclare

        // Resolve superclass reference if present
        if let Some(superclass_id) = class.node.superclass {
            let superclass_node = self.nodes.get_identifier_node(superclass_id);
            self.resolve_identifier(superclass_node);
        }

        // Visit class members
        let length = self.nodes.array.size(class.node.members);
        for i in 0..length {
            if let Some(member_id) = self.nodes.array.get_node_id_at(class.node.members, i) {
                let member = self.nodes.get_class_member_node(member_id);
                self.visit_class_member(member);
            }
        }
    }

    fn visit_class_member(&mut self, member: TypedNodeRef<ClassMemberNode>) {
        match member.node {
            ClassMemberNode::Method(method) => {
                self.visit_method(TypedNodeRef::new(member.id, method))
            }
            ClassMemberNode::Field(field) => self.visit_field(TypedNodeRef::new(member.id, field)),
        }
    }

    fn visit_method(&mut self, method: TypedNodeRef<FunctionExprNode>) {
        // Methods don't declare their name in the outer scope (they're accessed via object)
        // But we need to visit their parameters and body

        // Begin new scope for the method
        self.begin_scope();

        // Declare parameters
        let params_length = self.nodes.array.size(method.node.parameters);
        for i in 0..params_length {
            if let Some(param_id) = self.nodes.array.get_node_id_at(method.node.parameters, i) {
                let param = self.nodes.get_identifier_node(param_id);
                self.declare_symbol(
                    param.node.name,
                    SymbolInfo {
                        decl_node_id: param.id,
                        kind: SymbolKind::Parameter,
                        scope_depth: self.scope_depth,
                    },
                );
            }
        }

        // Visit method body
        let body = self.nodes.get_block_stmt_node(method.node.body);
        self.visit_block(body);

        self.end_scope();
    }

    fn visit_field(&mut self, field: TypedNodeRef<FieldDeclNode>) {
        // Fields don't get declared in the scope (they're accessed via object)
        // But we need to resolve any identifiers in their initializers

        if let Some(init_id) = field.node.initializer {
            let init_expr = self.nodes.get_expr_node(init_id);
            self.visit_expression(init_expr);
        }
    }

    fn visit_function(&mut self, func: TypedNodeRef<FunctionDeclNode>) {
        let func_expr = self.nodes.get_func_expr_node(func.node.function);

        // Note: Function name already declared in hoisting pass, don't redeclare

        // Begin new scope for function
        self.begin_scope();

        // Declare parameters
        let params_length = self.nodes.array.size(func_expr.node.parameters);
        for i in 0..params_length {
            if let Some(param_id) = self.nodes.array.get_node_id_at(func_expr.node.parameters, i) {
                let param = self.nodes.get_identifier_node(param_id);
                self.declare_symbol(
                    param.node.name,
                    SymbolInfo {
                        decl_node_id: param.id,
                        kind: SymbolKind::Parameter,
                        scope_depth: self.scope_depth,
                    },
                );
            }
        }

        // Visit function body
        let body = self.nodes.get_block_stmt_node(func_expr.node.body);
        self.visit_block(body);

        self.end_scope();
    }

    fn visit_lambda_decl(&mut self, lambda: TypedNodeRef<LambdaDeclNode>) {
        let name_node = self.nodes.get_identifier_node(lambda.node.name);

        // Lambdas are NOT hoisted - they're variable declarations
        // Declare lambda name in current scope
        self.declare_symbol(
            name_node.node.name,
            SymbolInfo {
                decl_node_id: lambda.id,
                kind: SymbolKind::Function,
                scope_depth: self.scope_depth,
            },
        );

        // Visit the lambda expression
        let lambda_expr = self.nodes.get_lambda_expr_node(lambda.node.lambda);
        self.visit_lambda_expr(lambda_expr);
    }

    fn visit_lambda_expr(&mut self, lambda: TypedNodeRef<LambdaExprNode>) {
        // Begin new scope for lambda
        self.begin_scope();

        // Declare parameters
        let params_length = self.nodes.array.size(lambda.node.parameters);
        for i in 0..params_length {
            if let Some(param_id) = self.nodes.array.get_node_id_at(lambda.node.parameters, i) {
                let param = self.nodes.get_identifier_node(param_id);
                self.declare_symbol(
                    param.node.name,
                    SymbolInfo {
                        decl_node_id: param.id,
                        kind: SymbolKind::Parameter,
                        scope_depth: self.scope_depth,
                    },
                );
            }
        }

        // Visit lambda body (can be expression or block)
        let node = self.nodes.get_node(lambda.node.body);
        if matches!(node, AstNode::BlockStmt(_)) {
            let block = self.nodes.get_block_stmt_node(lambda.node.body);
            self.visit_block(block);
        } else {
            let expr = self.nodes.get_expr_node(lambda.node.body);
            self.visit_expression(expr);
        }

        self.end_scope();
    }

    fn visit_variable(&mut self, var: TypedNodeRef<VariableDeclNode>) {
        let name_node = self.nodes.get_identifier_node(var.node.target);

        // Visit initializer first (before declaring the variable)
        // This prevents the variable from referencing itself in its own initializer
        if let Some(init_id) = var.node.initializer {
            let init_expr = self.nodes.get_expr_node(init_id);
            self.visit_expression(init_expr);
        }

        // Now declare the variable
        self.declare_symbol(
            name_node.node.name,
            SymbolInfo {
                decl_node_id: var.id,
                kind: SymbolKind::Variable,
                scope_depth: self.scope_depth,
            },
        );
    }

    fn visit_block(&mut self, block: TypedNodeRef<BlockStmtNode>) {
        self.begin_scope();

        // First pass: hoist function and class declarations in this block
        let length = self.nodes.array.size(block.node.decls);
        for i in 0..length {
            if let Some(decl_id) = self.nodes.array.get_node_id_at(block.node.decls, i) {
                let decl = self.nodes.get_decl_node(decl_id);

                match decl.node {
                    DeclNode::Class(class) => {
                        let name_node = self.nodes.get_identifier_node(class.name);
                        self.declare_symbol(
                            name_node.node.name,
                            SymbolInfo {
                                decl_node_id: decl.id,
                                kind: SymbolKind::Class,
                                scope_depth: self.scope_depth,
                            },
                        );
                    }
                    DeclNode::Function(func) => {
                        let func_expr = self.nodes.get_func_expr_node(func.function);
                        let name_node = self.nodes.get_identifier_node(func_expr.node.name);
                        self.declare_symbol(
                            name_node.node.name,
                            SymbolInfo {
                                decl_node_id: decl.id,
                                kind: SymbolKind::Function,
                                scope_depth: self.scope_depth,
                            },
                        );
                    }
                    _ => {
                        // Variables, lambdas, and statements are not hoisted
                    }
                }
            }
        }

        // Second pass: visit all declarations to resolve their contents
        for i in 0..length {
            if let Some(decl_id) = self.nodes.array.get_node_id_at(block.node.decls, i) {
                let decl = self.nodes.get_decl_node(decl_id);
                self.visit_decl(decl);
            }
        }

        self.end_scope();
    }

    fn visit_statement(&mut self, stmt: TypedNodeRef<StmtNode>) {
        match stmt.node {
            StmtNode::Expr(expr_stmt) => {
                let expr = self.nodes.get_expr_node(expr_stmt.expr);
                self.visit_expression(expr);
            }
            StmtNode::Block(block) => {
                self.visit_block(TypedNodeRef::new(stmt.id, block));
            }
            StmtNode::If(if_stmt) => {
                let cond = self.nodes.get_expr_node(if_stmt.condition);
                self.visit_expression(cond);

                let then_branch = self.nodes.get_stmt_node(if_stmt.then_branch);
                self.visit_statement(then_branch);

                if let Some(else_id) = if_stmt.else_branch {
                    let else_branch = self.nodes.get_stmt_node(else_id);
                    self.visit_statement(else_branch);
                }
            }
            StmtNode::While(while_stmt) => {
                let cond = self.nodes.get_expr_node(while_stmt.condition);
                self.visit_expression(cond);

                let body = self.nodes.get_stmt_node(while_stmt.body);
                self.visit_statement(body);
            }
            StmtNode::For(for_stmt) => {
                // For loop creates its own scope
                self.begin_scope();

                if let Some(init_id) = for_stmt.initializer {
                    self.visit_for_initializer(init_id);
                }

                if let Some(cond_id) = for_stmt.condition {
                    let cond = self.nodes.get_expr_node(cond_id);
                    self.visit_expression(cond);
                }

                if let Some(after_id) = for_stmt.afterthought {
                    let after = self.nodes.get_expr_node(after_id);
                    self.visit_expression(after);
                }

                let body = self.nodes.get_stmt_node(for_stmt.body);
                self.visit_statement(body);

                self.end_scope();
            }
            StmtNode::Return(ret_stmt) => {
                if let Some(value_id) = ret_stmt.value {
                    let value = self.nodes.get_expr_node(value_id);
                    self.visit_expression(value);
                }
            }
            StmtNode::Break(_) | StmtNode::Continue(_) => {
                // Nothing to resolve
            }
        }
    }

    fn visit_for_initializer(&mut self, init_id: NodeId) {
        let node = self.nodes.get_node(init_id);

        if matches!(node, AstNode::VariableDecl(_)) {
            let var_decl = self.nodes.get_var_decl_node(init_id);
            self.visit_variable(var_decl);
        } else {
            let expr = self.nodes.get_expr_node(init_id);
            self.visit_expression(expr);
        }
    }

    fn visit_expression(&mut self, expr: TypedNodeRef<ExprNode>) {
        match expr.node {
            ExprNode::Primary(primary) => {
                self.visit_primary(TypedNodeRef::new(expr.id, primary));
            }
            ExprNode::Call(call) => {
                let callee = self.nodes.get_expr_node(call.callee);
                self.visit_expression(callee);

                let op = self.nodes.get_call_operation_node(call.operation);
                self.visit_call_operation(op);
            }
            ExprNode::Assignment(assign) => {
                let target = self.nodes.get_assignment_target_node(assign.target);
                self.visit_assignment_target(target);

                let value = self.nodes.get_expr_node(assign.value);
                self.visit_expression(value);
            }
            ExprNode::Unary(unary) => {
                let operand = self.nodes.get_expr_node(unary.operand);
                self.visit_expression(operand);
            }
            ExprNode::Equality(equality) => {
                let left = self.nodes.get_expr_node(equality.left);
                self.visit_expression(left);

                let right = self.nodes.get_expr_node(equality.right);
                self.visit_expression(right);
            }
            ExprNode::Comparison(comparison) => {
                let left = self.nodes.get_expr_node(comparison.left);
                self.visit_expression(left);

                let right = self.nodes.get_expr_node(comparison.right);
                self.visit_expression(right);
            }
            ExprNode::Term(term) => {
                let left = self.nodes.get_expr_node(term.left);
                self.visit_expression(left);

                let right = self.nodes.get_expr_node(term.right);
                self.visit_expression(right);
            }
            ExprNode::Factor(factor) => {
                let left = self.nodes.get_expr_node(factor.left);
                self.visit_expression(left);

                let right = self.nodes.get_expr_node(factor.right);
                self.visit_expression(right);
            }
            ExprNode::Pipe(pipe) => {
                let left = self.nodes.get_expr_node(pipe.left);
                self.visit_expression(left);

                let right = self.nodes.get_expr_node(pipe.right);
                self.visit_expression(right);
            }
            ExprNode::LogicalOr(logical) => {
                let left = self.nodes.get_expr_node(logical.left);
                self.visit_expression(left);

                let right = self.nodes.get_expr_node(logical.right);
                self.visit_expression(right);
            }
            ExprNode::LogicalAnd(logical) => {
                let left = self.nodes.get_expr_node(logical.left);
                self.visit_expression(left);

                let right = self.nodes.get_expr_node(logical.right);
                self.visit_expression(right);
            }
            ExprNode::Ternary(ternary) => {
                let condition = self.nodes.get_expr_node(ternary.condition);
                self.visit_expression(condition);

                let then_expr = self.nodes.get_expr_node(ternary.then_expr);
                self.visit_expression(then_expr);

                let else_expr = self.nodes.get_expr_node(ternary.else_expr);
                self.visit_expression(else_expr);
            }
        }
    }

    fn visit_call_operation(&mut self, op: TypedNodeRef<CallOperationNode>) {
        match op.node {
            CallOperationNode::Call(call_node) => {
                let length = self.nodes.array.size(call_node.args);
                for i in 0..length {
                    if let Some(arg_id) = self.nodes.array.get_node_id_at(call_node.args, i) {
                        let arg = self.nodes.get_expr_node(arg_id);
                        self.visit_expression(arg);
                    }
                }
            }
            CallOperationNode::Property(_prop) => {
                // Property access - the identifier is a property name, not a variable reference
                // Don't resolve it
            }
            CallOperationNode::Index(index) => {
                let index_expr = self.nodes.get_expr_node(index.index);
                self.visit_expression(index_expr);
            }
            CallOperationNode::OptionalProperty(_) | CallOperationNode::Map(_) | CallOperationNode::OptionalMap(_) => {
                // Optional chaining and map operations - nothing extra to resolve
            }
        }
    }

    fn visit_assignment_target(&mut self, target: TypedNodeRef<AssignmentTargetNode>) {
        match target.node {
            AssignmentTargetNode::Identifier(ident) => {
                // This is a variable being assigned to - resolve it
                self.resolve_identifier(TypedNodeRef::new(target.id, ident));
            }
            AssignmentTargetNode::Property(prop) => {
                // Resolve the object part
                let object = self.nodes.get_expr_node(prop.object);
                self.visit_expression(object);
                // The property name is not a variable reference
            }
            AssignmentTargetNode::Index(index) => {
                // Resolve the object and index expressions
                let object = self.nodes.get_expr_node(index.object);
                self.visit_expression(object);

                let index_expr = self.nodes.get_expr_node(index.index);
                self.visit_expression(index_expr);
            }
        }
    }

    fn visit_primary(&mut self, primary: TypedNodeRef<PrimaryNode>) {
        match primary.node {
            PrimaryNode::Identifier(ident) => {
                // This is an identifier reference - resolve it!
                self.resolve_identifier(TypedNodeRef::new(primary.id, ident));
            }
            PrimaryNode::Array(arr) => {
                let length = self.nodes.array.size(arr.elements);
                for i in 0..length {
                    if let Some(elem_id) = self.nodes.array.get_node_id_at(arr.elements, i) {
                        let elem = self.nodes.get_expr_node(elem_id);
                        self.visit_expression(elem);
                    }
                }
            }
            PrimaryNode::Object(obj) => {
                let length = self.nodes.array.size(obj.entries);
                for i in 0..length {
                    if let Some(entry_id) = self.nodes.array.get_node_id_at(obj.entries, i) {
                        let entry = self.nodes.get_obj_entry_node(entry_id);
                        // Object keys are property names, not variable references
                        // But values need to be resolved
                        let value = self.nodes.get_expr_node(entry.node.value);
                        self.visit_expression(value);
                    }
                }
            }
            PrimaryNode::Grouping(group) => {
                let inner = self.nodes.get_expr_node(group.expr);
                self.visit_expression(inner);
            }
            PrimaryNode::Lambda(lambda) => {
                self.visit_lambda_expr(TypedNodeRef::new(primary.id, lambda));
            }
            PrimaryNode::When(when) => {
                if let Some(value_id) = when.value {
                    let subject = self.nodes.get_expr_node(value_id);
                    self.visit_expression(subject);
                }

                let branch_length = self.nodes.array.size(when.branches);
                for i in 0..branch_length {
                    if let Some(branch_id) = self.nodes.array.get_node_id_at(when.branches, i) {
                        let branch = self.nodes.get_when_branch_node(branch_id);
                        self.visit_when_branch(branch);
                    }
                }

                if let Some(else_id) = when.else_branch {
                    let else_expr = self.nodes.get_expr_node(else_id);
                    self.visit_expression(else_expr);
                }
            }
            // Literals don't contain identifiers to resolve
            PrimaryNode::Number(_)
            | PrimaryNode::String(_)
            | PrimaryNode::Boolean(_)
            | PrimaryNode::Nil(_)
            | PrimaryNode::This(_)
            | PrimaryNode::Super(_) => {}
        }
    }

    fn visit_when_branch(&mut self, branch: TypedNodeRef<WhenBranchNode>) {
        let cond = self.nodes.get_expr_node(branch.node.condition);
        self.visit_expression(cond);

        let body = self.nodes.get_expr_node(branch.node.body);
        self.visit_expression(body);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{AnalysisPipeline, Parser, ParserConfig, SourceMap};
    use std::sync::Arc;

    #[test]
    fn test_basic_symbol_resolution() {
        let source = r#"
var x = 10;
print(x);
        "#;

        let source_map = Arc::new(SourceMap::from_source(source.to_string()));
        let mut strings = StringInterner::new();
        let mut nodes = AstNodeArena::new();
        let mut parser = Parser::new(source_map.clone(), &mut nodes, &mut strings)
            .with_config(ParserConfig { skip_modules: true });
        let modules = parser.parse();

        let mut errors = parser.into_errors();
        AnalysisPipeline::new(&mut strings)
            .analyze(&modules, &mut nodes, &mut errors)
            .unwrap();

        let root_module_id = modules.get_main().unwrap().node;
        let module_node = nodes.get_program_node(root_module_id);

        let resolver = SymbolResolver::new(&nodes, &strings);
        let symbol_table = resolver.resolve_module(module_node);

        // Check that we have at least one resolution (the `x` in `print(x)`)
        let resolutions = symbol_table.all_resolutions();
        println!("Number of resolutions: {}", resolutions.len());

        for (node_id, symbol_info) in resolutions {
            println!("NodeId {:?} resolves to {:?}", node_id, symbol_info);
        }

        assert!(
            resolutions.len() > 0,
            "Symbol table should have at least one resolution for the identifier 'x' in print(x)"
        );
    }

    // TODO: Add hoisting test once parser syntax issues are resolved
}
