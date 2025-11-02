use log::debug;
use qanglang_core::nodes::*;
use qanglang_core::symbol_resolver::SymbolKind;
use qanglang_core::{NodeId, TypedNodeRef};
use std::collections::HashSet;
use tower_lsp::lsp_types::*;

use crate::analyzer::AnalysisResult;
use crate::builtins;
use crate::stdlib_analyzer;

/// Keywords in the QangLang language
const KEYWORDS: &[&str] = &[
    "fn", "var", "class", "if", "else", "for", "while", "return", "import", "mod", "init", "this",
    "super", "break", "continue", "true", "false", "nil", "when", "is",
];

#[derive(Debug, Clone, PartialEq)]
enum CompletionContext {
    /// Completing after a dot (member access): `obj.`
    MemberAccess,
    /// Completing an identifier or keyword
    Identifier,
}

/// Information about the scope at a cursor position
#[derive(Debug, Clone)]
struct ScopeContext {
    /// The depth of the current scope (0 = module level, 1 = first nested level, etc.)
    scope_depth: usize,
    /// Stack of containing block node IDs (from outermost to innermost)
    containing_blocks: Vec<NodeId>,
    /// The cursor offset in the source file
    cursor_offset: usize,
}

/// Provides completion suggestions based on the analysis result and cursor position
pub fn provide_completions(
    analysis: &AnalysisResult,
    position: Position,
) -> Option<Vec<CompletionItem>> {
    let offset = analysis
        .source_map
        .position_to_offset(position.line, position.character)?;

    debug!(
        "Providing completions at line={}, char={}, offset={}",
        position.line, position.character, offset
    );

    // Find the scope context at the cursor position
    let scope_context = find_scope_at_offset(analysis, offset);
    debug!(
        "Scope context: depth={}, blocks={:?}",
        scope_context.scope_depth, scope_context.containing_blocks
    );

    // Determine the completion context
    let context = find_completion_context(analysis, offset);
    debug!("Completion context returned: {:?}", context);

    let context = context?;

    let completions = match context {
        CompletionContext::MemberAccess => {
            // Without type inference, we cannot reliably determine which properties/methods
            // are available on the object before the dot. Showing all possible members
            // creates too much noise, so we disable completions for member access.
            // TODO: Implement type inference to enable accurate property completions
            debug!("Member access completions disabled (no type inference available)");
            return None;
        }
        CompletionContext::Identifier => {
            // Combine visible symbols and keywords with scope-aware filtering
            let mut completions = collect_visible_symbols(analysis, &scope_context);

            // Also collect nested declarations from containing scopes
            let mut seen_names = HashSet::new();
            for item in &completions {
                seen_names.insert(item.label.clone());
            }
            collect_nested_declarations(
                analysis,
                &scope_context,
                &mut completions,
                &mut seen_names,
                offset,
            );

            // Add native functions
            add_native_function_completions(&mut completions, &mut seen_names);

            // Add stdlib classes and functions
            add_stdlib_completions(&mut completions, &mut seen_names);

            completions.extend(get_keyword_completions());
            completions
        }
    };

    debug!("Returning {} completion items", completions.len());
    Some(completions)
}

/// Determines what kind of completion context we're in based on the cursor position
/// Returns None if we're in a context where completion should not be provided (e.g., inside strings)
fn find_completion_context(analysis: &AnalysisResult, offset: usize) -> Option<CompletionContext> {
    // Look backwards from the cursor to find if we're after a dot
    let source = analysis.source_map.get_source();

    // First check if we're inside a string literal by scanning back to find an unmatched quote
    if is_inside_string(source, offset) {
        debug!("Cursor is inside a string literal, suppressing all completions");
        return None;
    }

    // Get the character just before the cursor
    if offset > 0 {
        let mut check_offset = offset - 1;

        // Skip whitespace backwards, but stop at newlines
        // (member access should only be detected on the same line)
        while check_offset > 0 && check_offset < source.len() {
            let ch = source[check_offset];
            if ch == '\n' || ch == '\r' {
                // Newline - stop here, dot is not on same line
                break;
            }
            if !ch.is_ascii_whitespace() {
                // Found non-whitespace character
                break;
            }
            check_offset -= 1;
        }

        // Check if we're right after a dot (on the same line)
        if check_offset < source.len() && source[check_offset] == '.' {
            debug!("Found dot at offset {}", check_offset);
            return Some(CompletionContext::MemberAccess);
        }
    }

    Some(CompletionContext::Identifier)
}

/// Checks if the given offset is inside a string literal
/// This is a simplified check that scans backwards to find if we're between quotes
fn is_inside_string(source: &[char], offset: usize) -> bool {
    if offset == 0 {
        return false;
    }

    // Scan backwards from offset to the start of the line (or file)
    // Count unescaped quotes to determine if we're inside a string
    let mut scan_pos = offset.saturating_sub(1);
    let mut in_string = false;
    let mut quote_char = None;

    // Find the start of the current line
    while scan_pos > 0 {
        let ch = source[scan_pos];
        if ch == '\n' || ch == '\r' {
            scan_pos += 1; // Move back to first char of current line
            break;
        }
        scan_pos = scan_pos.saturating_sub(1);
    }

    // Now scan forward from the line start to the cursor position
    while scan_pos < offset && scan_pos < source.len() {
        let ch = source[scan_pos];

        if !in_string {
            // Not currently in a string, check if we're starting one
            if ch == '"' || ch == '\'' {
                in_string = true;
                quote_char = Some(ch);
            }
        } else {
            // Currently in a string, check for closing quote
            // Handle escaped quotes by checking if the previous char is a backslash
            let is_escaped = scan_pos > 0 && source[scan_pos - 1] == '\\';

            if !is_escaped && Some(ch) == quote_char {
                in_string = false;
                quote_char = None;
            }
        }

        scan_pos += 1;
    }

    debug!(
        "is_inside_string(offset={}) = {} (checked from line start to offset)",
        offset, in_string
    );
    in_string
}

/// Finds the scope context at the given offset by traversing the AST
fn find_scope_at_offset(analysis: &AnalysisResult, offset: usize) -> ScopeContext {
    let module = analysis.nodes.get_program_node(analysis.root_module_id);

    let mut finder = ScopeFinder {
        offset,
        scope_depth: 0,
        containing_blocks: Vec::new(),
        nodes: &analysis.nodes,
    };

    finder.find_in_module(module);

    ScopeContext {
        scope_depth: finder.scope_depth,
        containing_blocks: finder.containing_blocks,
        cursor_offset: offset,
    }
}

/// Helper struct for finding the scope at a specific offset
struct ScopeFinder<'a> {
    offset: usize,
    scope_depth: usize,
    containing_blocks: Vec<NodeId>,
    nodes: &'a qanglang_core::AstNodeArena,
}

impl<'a> ScopeFinder<'a> {
    fn find_in_module(&mut self, module: TypedNodeRef<Module>) {
        let length = self.nodes.array.size(module.node.decls);

        for i in 0..length {
            if let Some(node_id) = self.nodes.array.get_node_id_at(module.node.decls, i) {
                let decl = self.nodes.get_decl_node(node_id);
                if self.check_decl(decl) {
                    // Found the containing declaration, no need to check others
                    return;
                }
            }
        }
    }

    fn check_decl(&mut self, decl: TypedNodeRef<DeclNode>) -> bool {
        match decl.node {
            DeclNode::Function(func) => {
                let func_expr = self.nodes.get_func_expr_node(func.function);
                // Check if offset is inside the function body (not the entire function expression)
                let body_node = self.nodes.get_node(func_expr.node.body);
                if let AstNode::BlockStmt(block) = body_node {
                    if self.span_contains(block.span) {
                        self.check_function_expr(func_expr);
                        return true; // Found the containing declaration
                    }
                }
            }
            DeclNode::Class(class) => {
                // Check if offset is inside this class
                if self.span_contains(class.span) {
                    self.scope_depth += 1;

                    // Check class members
                    let members_length = self.nodes.array.size(class.members);
                    for i in 0..members_length {
                        if let Some(member_id) = self.nodes.array.get_node_id_at(class.members, i) {
                            let member_node = self.nodes.get_node(member_id);

                            if let AstNode::FunctionExpr(method) = member_node {
                                if self.span_contains(method.span) {
                                    let method_ref = TypedNodeRef::new(member_id, *method);
                                    self.check_function_expr(method_ref);
                                }
                            }
                        }
                    }

                    return true; // Found the containing declaration
                }
            }
            DeclNode::Lambda(lambda) => {
                let lambda_expr = self.nodes.get_lambda_expr_node(lambda.lambda);
                if self.span_contains(lambda_expr.node.span) {
                    self.check_lambda_expr(lambda_expr);
                    return true; // Found the containing declaration
                }
            }
            DeclNode::Stmt(stmt) => {
                return self.check_statement(TypedNodeRef::new(decl.id, stmt));
            }
            _ => {}
        }
        false
    }

    fn check_function_expr(&mut self, func: TypedNodeRef<FunctionExprNode>) {
        // Check function body (the block itself will handle scope increment)
        let body_node = self.nodes.get_node(func.node.body);
        if let AstNode::BlockStmt(block) = body_node {
            let block_ref = TypedNodeRef::new(func.node.body, *block);
            self.check_block(block_ref);
        }
    }

    fn check_lambda_expr(&mut self, lambda: TypedNodeRef<LambdaExprNode>) {
        // Check lambda body
        let body_node = self.nodes.get_node(lambda.node.body);
        match body_node {
            AstNode::BlockStmt(block) => {
                // Block will handle scope increment
                let block_ref = TypedNodeRef::new(lambda.node.body, *block);
                self.check_block(block_ref);
            }
            _ => {
                // Lambda body is an expression, not a block
                // Expression lambdas still create a scope, but we don't track it for completions
                // since there are no nested declarations possible
            }
        }
    }

    fn check_block(&mut self, block: TypedNodeRef<BlockStmtNode>) -> bool {
        // Blocks create new scopes
        if self.span_contains(block.node.span) {
            // This block contains the cursor, so it's part of our scope chain
            self.scope_depth += 1;
            self.containing_blocks.push(block.id);

            // Process declarations in this block to find even deeper nesting
            let decls_length = self.nodes.array.size(block.node.decls);
            for i in 0..decls_length {
                if let Some(decl_id) = self.nodes.array.get_node_id_at(block.node.decls, i) {
                    let decl = self.nodes.get_decl_node(decl_id);
                    if self.check_decl(decl) {
                        // Found a nested declaration containing the cursor
                        return true;
                    }
                }
            }

            // The cursor is in this block (but not in any nested declaration)
            return true;
        }
        false
    }

    fn check_statement(&mut self, stmt: TypedNodeRef<StmtNode>) -> bool {
        match stmt.node {
            StmtNode::Block(block) => {
                return self.check_block(TypedNodeRef::new(stmt.id, block));
            }
            StmtNode::If(if_stmt) => {
                // Check then branch
                let then_node = self.nodes.get_node(if_stmt.then_branch);
                if let AstNode::BlockStmt(block) = then_node {
                    let block_ref = TypedNodeRef::new(if_stmt.then_branch, *block);
                    if self.check_block(block_ref) {
                        return true;
                    }
                }

                // Check else branch if it exists
                if let Some(else_branch) = if_stmt.else_branch {
                    let else_node = self.nodes.get_node(else_branch);
                    if let AstNode::BlockStmt(block) = else_node {
                        let block_ref = TypedNodeRef::new(else_branch, *block);
                        if self.check_block(block_ref) {
                            return true;
                        }
                    } else if let AstNode::IfStmt(nested_if) = else_node {
                        if self.check_statement(TypedNodeRef::new(
                            else_branch,
                            StmtNode::If(*nested_if),
                        )) {
                            return true;
                        }
                    }
                }
            }
            StmtNode::While(while_stmt) => {
                let body_node = self.nodes.get_node(while_stmt.body);
                if let AstNode::BlockStmt(block) = body_node {
                    let block_ref = TypedNodeRef::new(while_stmt.body, *block);
                    if self.check_block(block_ref) {
                        return true;
                    }
                }
            }
            StmtNode::For(for_stmt) => {
                let body_node = self.nodes.get_node(for_stmt.body);
                if let AstNode::BlockStmt(block) = body_node {
                    let block_ref = TypedNodeRef::new(for_stmt.body, *block);
                    if self.check_block(block_ref) {
                        return true;
                    }
                }
            }
            _ => {}
        }
        false
    }

    fn span_contains(&self, span: SourceSpan) -> bool {
        // Use < for end check to exclude the closing brace/bracket
        // This helps avoid incorrectly matching when the cursor is just after a block
        self.offset >= span.start && self.offset < span.end
    }
}

/// Collects all symbols visible at the current position with scope filtering
fn collect_visible_symbols(
    analysis: &AnalysisResult,
    scope_context: &ScopeContext,
) -> Vec<CompletionItem> {
    let mut completions = Vec::new();
    let mut seen_names = HashSet::new();

    // First, collect top-level declarations using two-pass approach:
    // Pass 1: hoisted declarations (functions, classes)
    // Pass 2: non-hoisted declarations (modules, variables, lambdas) declared before cursor
    collect_top_level_declarations(
        analysis,
        &mut completions,
        &mut seen_names,
        scope_context.cursor_offset,
    );

    // Then, iterate through symbol table to get additional symbols (parameters, local vars, etc.)
    // Only include symbols that are visible from the current scope
    for (_ref_id, symbol_info) in analysis.symbol_table.all_resolutions() {
        // Filter by scope: only show symbols from current or outer scopes
        // A symbol at depth N is visible at depths >= N
        if symbol_info.scope_depth > scope_context.scope_depth {
            continue; // Skip symbols from deeper (inner) scopes
        }
        let decl_node_id = symbol_info.decl_node_id;
        let node = analysis.nodes.get_node(decl_node_id);

        // Get the name and create completion based on symbol kind
        match node {
            AstNode::Function(func_decl) => {
                let func_expr = analysis.nodes.get_func_expr_node(func_decl.function);
                let name_node = analysis.nodes.get_identifier_node(func_expr.node.name);
                let name = analysis.strings.get(name_node.node.name);

                if seen_names.insert(name.to_string()) {
                    completions.push(CompletionItem {
                        label: name.to_string(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some("function".to_string()),
                        ..Default::default()
                    });
                }
            }
            AstNode::Class(class_decl) => {
                let name_node = analysis.nodes.get_identifier_node(class_decl.name);
                let name = analysis.strings.get(name_node.node.name);

                if seen_names.insert(name.to_string()) {
                    completions.push(CompletionItem {
                        label: name.to_string(),
                        kind: Some(CompletionItemKind::CLASS),
                        detail: Some("class".to_string()),
                        ..Default::default()
                    });
                }
            }
            AstNode::VariableDecl(var_decl) => {
                let name_node = analysis.nodes.get_identifier_node(var_decl.target);
                let name = analysis.strings.get(name_node.node.name);

                if seen_names.insert(name.to_string()) {
                    completions.push(CompletionItem {
                        label: name.to_string(),
                        kind: Some(CompletionItemKind::VARIABLE),
                        detail: Some("variable".to_string()),
                        ..Default::default()
                    });
                }
            }
            AstNode::LambdaDecl(lambda_decl) => {
                let name_node = analysis.nodes.get_identifier_node(lambda_decl.name);
                let name = analysis.strings.get(name_node.node.name);

                if seen_names.insert(name.to_string()) {
                    completions.push(CompletionItem {
                        label: name.to_string(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some("lambda".to_string()),
                        ..Default::default()
                    });
                }
            }
            AstNode::ImportModuleDecl(module_decl) => {
                let name_node = analysis.nodes.get_identifier_node(module_decl.name);
                let name = analysis.strings.get(name_node.node.name);

                if seen_names.insert(name.to_string()) {
                    completions.push(CompletionItem {
                        label: name.to_string(),
                        kind: Some(CompletionItemKind::MODULE),
                        detail: Some("module".to_string()),
                        ..Default::default()
                    });
                }
            }
            AstNode::Identifier(ident_node) => {
                // This handles parameters and other identifiers
                let name = analysis.strings.get(ident_node.name);

                if seen_names.insert(name.to_string()) {
                    let kind = match symbol_info.kind {
                        SymbolKind::Parameter => CompletionItemKind::VARIABLE,
                        SymbolKind::Field => CompletionItemKind::FIELD,
                        _ => CompletionItemKind::VARIABLE,
                    };

                    completions.push(CompletionItem {
                        label: name.to_string(),
                        kind: Some(kind),
                        detail: Some(format!("{:?}", symbol_info.kind).to_lowercase()),
                        ..Default::default()
                    });
                }
            }
            _ => {}
        }
    }

    completions
}

/// Collects top-level declarations from the AST using a two-pass approach
/// Pass 1: Collect hoisted declarations (functions and classes) - available throughout global scope
/// Pass 2: Collect non-hoisted declarations (modules, variables, lambdas) - only available after declaration
/// This ensures we include all declarations even if they haven't been referenced
fn collect_top_level_declarations(
    analysis: &AnalysisResult,
    completions: &mut Vec<CompletionItem>,
    seen_names: &mut HashSet<String>,
    cursor_offset: usize,
) {
    let module = analysis.nodes.get_program_node(analysis.root_module_id);
    let length = analysis.nodes.array.size(module.node.decls);

    // Pass 1: Collect hoisted declarations (functions and classes)
    // These are available throughout the entire global scope
    for i in 0..length {
        if let Some(node_id) = analysis.nodes.array.get_node_id_at(module.node.decls, i) {
            let decl = analysis.nodes.get_decl_node(node_id);

            match decl.node {
                DeclNode::Function(func_decl) => {
                    let func_expr = analysis.nodes.get_func_expr_node(func_decl.function);
                    let name_node = analysis.nodes.get_identifier_node(func_expr.node.name);
                    let name = analysis.strings.get(name_node.node.name);

                    if seen_names.insert(name.to_string()) {
                        completions.push(CompletionItem {
                            label: name.to_string(),
                            kind: Some(CompletionItemKind::FUNCTION),
                            detail: Some("function".to_string()),
                            ..Default::default()
                        });
                    }
                }
                DeclNode::Class(class_decl) => {
                    let name_node = analysis.nodes.get_identifier_node(class_decl.name);
                    let name = analysis.strings.get(name_node.node.name);

                    if seen_names.insert(name.to_string()) {
                        completions.push(CompletionItem {
                            label: name.to_string(),
                            kind: Some(CompletionItemKind::CLASS),
                            detail: Some("function".to_string()),
                            ..Default::default()
                        });
                    }
                }
                _ => {} // Skip non-hoisted declarations in first pass
            }
        }
    }

    // Pass 2: Collect non-hoisted declarations (modules, variables, lambdas)
    // These are only available after their declaration point
    for i in 0..length {
        if let Some(node_id) = analysis.nodes.array.get_node_id_at(module.node.decls, i) {
            let decl = analysis.nodes.get_decl_node(node_id);

            // Only include non-hoisted declarations if they're declared before the cursor
            match decl.node {
                DeclNode::Module(module_decl) => {
                    if module_decl.span.start < cursor_offset {
                        let name_node = analysis.nodes.get_identifier_node(module_decl.name);
                        let name = analysis.strings.get(name_node.node.name);

                        if seen_names.insert(name.to_string()) {
                            completions.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::MODULE),
                                detail: Some("module".to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
                DeclNode::Lambda(lambda_decl) => {
                    if lambda_decl.span.start < cursor_offset {
                        let name_node = analysis.nodes.get_identifier_node(lambda_decl.name);
                        let name = analysis.strings.get(name_node.node.name);

                        if seen_names.insert(name.to_string()) {
                            completions.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::FUNCTION),
                                detail: Some("lambda".to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
                DeclNode::Variable(var_decl) => {
                    if var_decl.span.start < cursor_offset {
                        let name_node = analysis.nodes.get_identifier_node(var_decl.target);
                        let name = analysis.strings.get(name_node.node.name);

                        if seen_names.insert(name.to_string()) {
                            completions.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::VARIABLE),
                                detail: Some("variable".to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
                _ => {} // Functions and classes already handled in pass 1
            }
        }
    }
}

/// Collects nested declarations from the current scope and all containing scopes
/// In nested scopes, declarations are only available after their textual declaration point
fn collect_nested_declarations(
    analysis: &AnalysisResult,
    scope_context: &ScopeContext,
    completions: &mut Vec<CompletionItem>,
    seen_names: &mut HashSet<String>,
    cursor_offset: usize,
) {
    // Collect from each containing block (innermost to outermost)
    for block_id in scope_context.containing_blocks.iter().rev() {
        collect_declarations_in_block(analysis, *block_id, completions, seen_names, cursor_offset);
    }
}

/// Collects declarations from a specific block that are declared before the cursor position
/// In nested scopes, NOTHING is hoisted - all declarations are only available after their declaration
fn collect_declarations_in_block(
    analysis: &AnalysisResult,
    block_id: NodeId,
    completions: &mut Vec<CompletionItem>,
    seen_names: &mut HashSet<String>,
    cursor_offset: usize,
) {
    let block = analysis.nodes.get_block_stmt_node(block_id);
    let length = analysis.nodes.array.size(block.node.decls);

    for i in 0..length {
        if let Some(decl_id) = analysis.nodes.array.get_node_id_at(block.node.decls, i) {
            let decl = analysis.nodes.get_decl_node(decl_id);

            // In nested scopes, nothing is hoisted - all declarations are only available
            // after their textual declaration point
            match decl.node {
                DeclNode::Function(func_decl) => {
                    // Functions in nested scopes are NOT hoisted
                    if func_decl.span.start < cursor_offset {
                        let func_expr = analysis.nodes.get_func_expr_node(func_decl.function);
                        let name_node = analysis.nodes.get_identifier_node(func_expr.node.name);
                        let name = analysis.strings.get(name_node.node.name);

                        if seen_names.insert(name.to_string()) {
                            completions.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::FUNCTION),
                                detail: Some("function".to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
                DeclNode::Class(class_decl) => {
                    // Classes in nested scopes are NOT hoisted
                    if class_decl.span.start < cursor_offset {
                        let name_node = analysis.nodes.get_identifier_node(class_decl.name);
                        let name = analysis.strings.get(name_node.node.name);

                        if seen_names.insert(name.to_string()) {
                            completions.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::CLASS),
                                detail: Some("class".to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
                DeclNode::Module(module_decl) => {
                    // Modules in nested scopes are NOT hoisted
                    if module_decl.span.start < cursor_offset {
                        let name_node = analysis.nodes.get_identifier_node(module_decl.name);
                        let name = analysis.strings.get(name_node.node.name);

                        if seen_names.insert(name.to_string()) {
                            completions.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::MODULE),
                                detail: Some("module".to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
                DeclNode::Variable(var_decl) => {
                    // Variables are NOT hoisted
                    if var_decl.span.start < cursor_offset {
                        let name_node = analysis.nodes.get_identifier_node(var_decl.target);
                        let name = analysis.strings.get(name_node.node.name);

                        if seen_names.insert(name.to_string()) {
                            completions.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::VARIABLE),
                                detail: Some("variable".to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
                DeclNode::Lambda(lambda_decl) => {
                    // Lambdas are NOT hoisted
                    if lambda_decl.span.start < cursor_offset {
                        let name_node = analysis.nodes.get_identifier_node(lambda_decl.name);
                        let name = analysis.strings.get(name_node.node.name);

                        if seen_names.insert(name.to_string()) {
                            completions.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::FUNCTION),
                                detail: Some("lambda".to_string()),
                                ..Default::default()
                            });
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

/// Gets keyword completions
fn get_keyword_completions() -> Vec<CompletionItem> {
    KEYWORDS
        .iter()
        .map(|keyword| CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("keyword".to_string()),
            ..Default::default()
        })
        .collect()
}

/// Adds native function completions to the completion list
fn add_native_function_completions(
    completions: &mut Vec<CompletionItem>,
    seen_names: &mut HashSet<String>,
) {
    for func_info in builtins::get_all_native_functions() {
        if seen_names.insert(func_info.name.to_string()) {
            completions.push(CompletionItem {
                label: func_info.name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("function".to_string()),
                ..Default::default()
            });
        }
    }
}

/// Adds stdlib class and function completions
fn add_stdlib_completions(completions: &mut Vec<CompletionItem>, seen_names: &mut HashSet<String>) {
    let stdlib = stdlib_analyzer::get_stdlib_cache();

    // Add stdlib classes
    for class_name in stdlib.get_classes() {
        if seen_names.insert(class_name.clone()) {
            completions.push(CompletionItem {
                label: class_name.clone(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some("class".to_string()),
                ..Default::default()
            });
        }
    }

    // Add stdlib functions
    for func_name in stdlib.get_functions() {
        if seen_names.insert(func_name.clone()) {
            completions.push(CompletionItem {
                label: func_name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("function".to_string()),
                ..Default::default()
            });
        }
    }
}
