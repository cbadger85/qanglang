use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    ErrorReporter, QangCompilerError, SourceMap,
    frontend::{
        node_array_arena::NodeArrayId,
        nodes::*,
        source::ModuleMap,
        tokenizer::{Token, TokenType, Tokenizer},
        typed_node_arena::{ExprNode, NodeId, PrimaryNode, TypedNodeArena},
    },
    memory::StringInterner,
};

type ParseResult<T> = Result<T, QangCompilerError>;

pub struct Parser<'a> {
    source_map: Arc<SourceMap>,
    tokens: Tokenizer,
    previous_token: Option<Token>,
    current_token: Option<Token>,
    errors: ErrorReporter,
    strings: &'a mut StringInterner,
    nodes: &'a mut TypedNodeArena,
    module_queue: VecDeque<PathBuf>,
}

impl<'a> Parser<'a> {
    pub fn new(
        source_map: Arc<SourceMap>,
        nodes: &'a mut TypedNodeArena,
        strings: &'a mut StringInterner,
    ) -> Self {
        let errors = ErrorReporter::new();
        let mut parser = Self {
            tokens: Tokenizer::new(source_map.clone()),
            source_map,
            previous_token: None,
            current_token: None,
            errors,
            nodes,
            strings,
            module_queue: VecDeque::new(),
        };

        parser.advance();
        parser
    }

    pub fn into_errors(self) -> ErrorReporter {
        self.errors
    }

    fn into_parts(self) -> (ErrorReporter, VecDeque<PathBuf>) {
        (self.errors, self.module_queue)
    }

    fn advance(&mut self) {
        self.previous_token = self.current_token.take();

        while let Some(token) = self.tokens.next() {
            if token.token_type == TokenType::Error {
                self.handle_tokenizer_error(&token);
                continue;
            }

            self.current_token = Some(token);
            break;
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> ParseResult<()> {
        if let Some(current_token_type) = self.current_token.as_ref().map(|t| &t.token_type)
            && token_type == *current_token_type
        {
            self.advance();
            return Ok(());
        }

        let span = self
            .current_token
            .as_ref()
            .map(SourceSpan::from_token)
            .unwrap_or_default();

        if !self.is_at_end() {
            self.advance();
        }

        Err(QangCompilerError::new_syntax_error(
            message.to_string(),
            span,
        ))
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            return false;
        }

        self.advance();

        true
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.current_token
            .as_ref()
            .map(|t| t.token_type == token_type)
            .unwrap_or(false)
    }

    fn handle_tokenizer_error(&mut self, token: &Token) {
        let message = token
            .error_message
            .as_deref()
            .unwrap_or("Tokenization error")
            .to_string();

        self.errors
            .report_error(QangCompilerError::new_syntax_error(
                message,
                SourceSpan::from_token(token),
            ));
    }

    fn get_current_span(&self) -> SourceSpan {
        self.current_token
            .as_ref()
            .map(SourceSpan::from_token)
            .unwrap_or_default()
    }

    fn get_previous_span(&self) -> SourceSpan {
        self.previous_token
            .as_ref()
            .map(SourceSpan::from_token)
            .unwrap_or_default()
    }

    fn get_identifier(&mut self) -> ParseResult<NodeId> {
        let token = self.previous_token.as_ref();
        let span = token
            .map(SourceSpan::from_token)
            .unwrap_or(SourceSpan { start: 0, end: 0 });

        if let Some(token) = token {
            let name = token.lexeme(&self.source_map);
            let name = self.strings.intern(&name.iter().collect::<String>());
            let node_id = self
                .nodes
                .create_node(AstNode::Identifier(IdentifierNode { name, span }));
            Ok(node_id)
        } else {
            Err(QangCompilerError::new_syntax_error(
                "Expected identifier.".to_string(),
                span,
            ))
        }
    }

    fn synchronize(&mut self) {
        loop {
            let current_token_type = match self.current_token.as_ref() {
                Some(token) => &token.token_type,
                None => {
                    return;
                }
            };

            if current_token_type == &TokenType::Eof {
                break;
            }

            if let Some(prev_token) = &self.previous_token
                && prev_token.token_type == TokenType::Semicolon
            {
                return;
            }

            match current_token_type {
                TokenType::Class
                | TokenType::Fn
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Return
                | TokenType::Break
                | TokenType::Continue => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        match &self.current_token {
            Some(token) => token.token_type == TokenType::Eof,
            None => true,
        }
    }

    fn is_lambda_start_after_paren(&mut self) -> bool {
        self.check_lambda_pattern(true)
    }

    fn check_lambda_pattern(&mut self, after_paren: bool) -> bool {
        if after_paren {
            // After paren case - check current token and look ahead
            let check_token = if let Some(current) = &self.current_token {
                current.token_type
            } else {
                return false;
            };

            // Special case: if we see another '(' after the first '(', we need to check if it's a lambda
            // pattern like "(() -> nil)" where the inner () is the lambda parameter list
            if check_token == TokenType::LeftParen {
                // Look for () -> pattern starting from current position
                if let Some(next_token) = self.tokens.peek()
                    && next_token.token_type == TokenType::RightParen
                {
                    // Found "(()" pattern, check for -> after the )
                    return self
                        .tokens
                        .peek_ahead(1)
                        .map(|t| t.token_type == TokenType::Arrow)
                        .unwrap_or(false);
                }
                // For non-empty parameter lists like "(x) ->" or "(x, y) ->"
                // we need to find the matching ) and then check for ->
                let mut paren_depth = 1;
                let mut offset = 0;
                while let Some(token) = self.tokens.peek_ahead(offset) {
                    match token.token_type {
                        TokenType::LeftParen => paren_depth += 1,
                        TokenType::RightParen => {
                            paren_depth -= 1;
                            if paren_depth == 0 {
                                // Found matching ), check for -> after it
                                return self
                                    .tokens
                                    .peek_ahead(offset + 1)
                                    .map(|t| t.token_type == TokenType::Arrow)
                                    .unwrap_or(false);
                            }
                        }
                        _ => {}
                    }
                    offset += 1;
                }
                false
            } else if check_token == TokenType::RightParen {
                // Empty parameter list: () ->
                self.tokens
                    .peek()
                    .map(|t| t.token_type == TokenType::Arrow)
                    .unwrap_or(false)
            } else {
                // Non-empty parameter list: look for ) ->
                let mut offset = 0;
                while self
                    .tokens
                    .peek_ahead(offset)
                    .map(|t| t.token_type != TokenType::RightParen)
                    .unwrap_or(false)
                {
                    offset += 1;
                }
                self.tokens
                    .peek_ahead(offset + 1)
                    .map(|t| t.token_type == TokenType::Arrow)
                    .unwrap_or(false)
            }
        } else {
            // Before paren case - must start with '(' immediately
            if let Some(first_token) = self.tokens.peek() {
                if first_token.token_type != TokenType::LeftParen {
                    return false;
                }
                // Check if it's followed by ) -> or parameters ) ->
                if let Some(second_token) = self.tokens.peek_ahead(1) {
                    if second_token.token_type == TokenType::RightParen {
                        // Empty parameter list: () ->
                        self.tokens
                            .peek_ahead(2)
                            .map(|t| t.token_type == TokenType::Arrow)
                            .unwrap_or(false)
                    } else if second_token.token_type == TokenType::LeftParen {
                        // If we see "((", this is a grouped expression, not a direct lambda
                        false
                    } else {
                        // Non-empty parameter list: look for ) ->
                        let mut offset = 2;
                        while self
                            .tokens
                            .peek_ahead(offset)
                            .map(|t| t.token_type != TokenType::RightParen)
                            .unwrap_or(false)
                        {
                            offset += 1;
                        }
                        self.tokens
                            .peek_ahead(offset + 1)
                            .map(|t| t.token_type == TokenType::Arrow)
                            .unwrap_or(false)
                    }
                } else {
                    false
                }
            } else {
                false
            }
        }
    }

    pub fn parse(&mut self) -> ModuleMap {
        let main_id = self.parse_file();
        let mut modules = ModuleMap::new(main_id, self.source_map.clone());

        while let Some(module_path) = self.module_queue.pop_front() {
            if modules.has(&module_path) {
                continue;
            }

            let source_map = match SourceMap::from_path(&module_path) {
                Ok(source_map) => source_map,
                Err(err) => {
                    self.errors
                        .report_error(QangCompilerError::new_analysis_error(
                            format!("Error loading module '{}': {}", module_path.display(), err),
                            SourceSpan::default(),
                        ));
                    continue;
                }
            };

            let source_map = Arc::new(source_map);

            let mut module_parser = Parser::new(source_map.clone(), self.nodes, self.strings);

            let module_id = module_parser.parse_file();

            let (errors, queue) = module_parser.into_parts();
            self.errors.merge(errors);

            modules.insert(module_path.as_path(), module_id, source_map);

            self.module_queue.extend(queue);
        }

        modules
    }

    fn parse_file(&mut self) -> NodeId {
        let start_span = self.get_current_span();
        let decls = self.nodes.array.create();

        while !self.is_at_end() {
            if let Some(decl) = self.declaration() {
                self.nodes.array.push(decls, decl);
            }
        }

        let end_span = self.get_current_span();

        self.nodes.create_node(AstNode::Module(Module {
            decls,
            span: SourceSpan::combine(start_span, end_span),
        }))
    }

    fn declaration(&mut self) -> Option<NodeId> {
        let current_token_type = self.current_token.as_ref()?.token_type;

        let result = match current_token_type {
            TokenType::Var => {
                self.advance();
                self.variable_declaration()
            }
            TokenType::Fn => {
                self.advance();
                self.function_declaration()
            }
            TokenType::Class => {
                self.advance();
                self.class_declaration()
            }
            TokenType::Mod => {
                self.advance();
                self.import_module_declaration()
            }
            _ => self.declaration_statement(),
        };

        match result {
            Ok(decl) => Some(decl),
            Err(error) => {
                let formatted_error =
                    QangCompilerError::new_syntax_error(error.message, error.span);
                self.errors.report_error(formatted_error);
                self.synchronize();
                None
            }
        }
    }

    fn import_module_declaration(&mut self) -> ParseResult<NodeId> {
        let span_start = self.get_previous_span();
        self.consume(TokenType::Identifier, "Expected module identifier.")?;
        let name = self.get_identifier()?;

        self.consume(TokenType::From, "Expected import declaration.")?;
        self.consume(TokenType::String, "Expected import path as string.")?;

        let token = self.previous_token.as_ref().unwrap();

        let path_span = self.get_previous_span();

        let value = token.lexeme(&self.source_map);
        let path_as_string = value[1..value.len() - 1].iter().collect::<String>();

        let current_dir = self
            .source_map
            .get_path()
            .parent()
            .expect("Expected path to be file and to be within a dir");
        let combined_path = current_dir.join(Path::new(&path_as_string));

        match std::fs::exists(combined_path.as_path()) {
            Ok(exists) => {
                if !exists {
                    self.errors
                        .report_error(QangCompilerError::new_syntax_error(
                            format!(
                                "module at path '{}' does not exist.",
                                combined_path.as_path().display(),
                            ),
                            path_span,
                        ));
                }
            }
            Err(_) => {
                self.errors
                    .report_error(QangCompilerError::new_syntax_error(
                        format!(
                            "Unable to load module from path '{}'",
                            combined_path.as_path().display(),
                        ),
                        path_span,
                    ));
            }
        }

        self.module_queue.push_back(combined_path);

        let path = self.strings.intern(&path_as_string);
        self.consume(TokenType::Semicolon, "Expected semicolon.")?;
        let span_end = self.get_previous_span();

        Ok(self
            .nodes
            .create_node(AstNode::ImportModuleDecl(ImportModuleDeclNode {
                path,
                name,
                span: SourceSpan::combine(span_start, span_end),
            })))
    }

    fn variable_declaration(&mut self) -> ParseResult<NodeId> {
        let var_span = self.get_previous_span();
        self.consume(TokenType::Identifier, "Expect variable name.")?;

        let identifier = self.get_identifier()?;

        let initializer = if self.match_token(TokenType::Equals) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        let semicolon_span = self.get_previous_span();
        let span = SourceSpan::combine(var_span, semicolon_span);

        let node_id = self
            .nodes
            .create_node(AstNode::VariableDecl(VariableDeclNode {
                target: identifier,
                initializer,
                span,
            }));

        Ok(node_id)
    }

    fn class_declaration(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_previous_span();

        self.consume(TokenType::Identifier, "Expect class name.")?;
        let name = self.get_identifier()?;

        let superclass = if self.match_token(TokenType::Colon) {
            self.consume(TokenType::Identifier, "Expect superclass name.")?;
            Some(self.get_identifier()?)
        } else {
            None
        };

        self.consume(TokenType::LeftBrace, "Expect '{' before class body.")?;

        let members = self.nodes.array.create();
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            let class_member = self.class_member()?;
            self.nodes.array.push(members, class_member);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.")?;
        let end_span = self.get_previous_span();
        let span = SourceSpan::combine(start_span, end_span);

        let node_id = self.nodes.create_node(AstNode::Class(ClassDeclNode {
            name,
            superclass,
            members,
            span,
        }));

        Ok(node_id)
    }

    fn class_member(&mut self) -> ParseResult<NodeId> {
        if self.check(TokenType::Identifier) {
            if self
                .tokens
                .peek()
                .map(|t| t.token_type == TokenType::LeftParen)
                .unwrap_or(false)
            {
                Ok(self.function_expression()?)
            } else {
                Ok(self.field_declaration()?)
            }
        } else {
            Err(QangCompilerError::new_syntax_error(
                "Expected field or method declaration.".to_string(),
                self.get_current_span(),
            ))
        }
    }

    fn field_declaration(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_current_span();
        self.consume(TokenType::Identifier, "Expect field name.")?;
        let name = self.get_identifier()?;

        let initializer = if self.match_token(TokenType::Equals) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon, "Expect ';' after field declaration.")?;
        let end_span = self.get_previous_span();
        let span = SourceSpan::combine(start_span, end_span);

        let node_id = self.nodes.create_node(AstNode::FieldDecl(FieldDeclNode {
            name,
            initializer,
            span,
        }));

        Ok(node_id)
    }

    fn function_declaration(&mut self) -> ParseResult<NodeId> {
        let function_expr = self.function_expression()?;
        let function_expr_span = self.nodes.get_func_expr_node(function_expr).node.span;
        let span = SourceSpan::combine(self.get_previous_span(), function_expr_span);

        let node_id = self
            .nodes
            .create_node(AstNode::FunctionDecl(FunctionDeclNode {
                function: function_expr,
                span,
            }));

        Ok(node_id)
    }

    fn declaration_statement(&mut self) -> ParseResult<NodeId> {
        let current_token_type = self
            .current_token
            .as_ref()
            .ok_or(QangCompilerError::new_syntax_error(
                "Expected statement.".to_string(),
                self.previous_token
                    .as_ref()
                    .map(SourceSpan::from_token)
                    .unwrap_or_default(),
            ))?
            .token_type;

        match current_token_type {
            TokenType::While => Ok(self.while_statement()?),
            TokenType::If => Ok(self.if_statement()?),
            TokenType::LeftBrace => Ok(self.block_statement()?),
            TokenType::For => Ok(self.for_statement()?),
            TokenType::Break => Ok(self.break_statement()?),
            TokenType::Continue => Ok(self.continue_statement()?),
            TokenType::Return => Ok(self.return_statement()?),
            _ => Ok(self.statement()?),
        }
    }

    fn block_statement(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_current_span();
        self.advance();
        let decls = self.nodes.array.create();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            if let Some(decl) = self.declaration() {
                self.nodes.array.push(decls, decl);
            }
        }
        self.consume(TokenType::RightBrace, "Expected '}'.")?;
        let end_span = self.get_current_span();

        let node_id = self.nodes.create_node(AstNode::BlockStmt(BlockStmtNode {
            decls,
            span: SourceSpan::combine(start_span, end_span),
        }));

        Ok(node_id)
    }

    fn if_statement(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::LeftParen, "Expected '('.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'.")?;

        let then_branch = self.statement()?;

        let (else_branch, span) = if self.match_token(TokenType::Else) {
            let else_stmt = self.statement()?;
            let else_stmt_span = self.nodes.get_node(else_stmt).span();
            let span = SourceSpan::combine(start_span, else_stmt_span);
            (Some(else_stmt), span)
        } else {
            let then_branch_span = self.nodes.get_node(then_branch).span();
            (None, SourceSpan::combine(start_span, then_branch_span))
        };

        let node_id = self.nodes.create_node(AstNode::IfStmt(IfStmtNode {
            condition,
            then_branch,
            else_branch,
            span,
        }));

        Ok(node_id)
    }

    fn statement(&mut self) -> ParseResult<NodeId> {
        let current_token_type = self
            .current_token
            .as_ref()
            .ok_or(QangCompilerError::new_syntax_error(
                "Expected statement.".to_string(),
                self.previous_token
                    .as_ref()
                    .map(SourceSpan::from_token)
                    .unwrap_or_default(),
            ))?
            .token_type;

        match current_token_type {
            TokenType::While => Ok(self.while_statement()?),
            TokenType::If => Ok(self.if_statement()?),
            TokenType::LeftBrace => Ok(self.block_statement()?),
            TokenType::For => Ok(self.for_statement()?),
            TokenType::Break => Ok(self.break_statement()?),
            TokenType::Continue => Ok(self.continue_statement()?),
            TokenType::Return => Ok(self.return_statement()?),
            _ => Ok(self.expression_statement()?),
        }
    }

    fn while_statement(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::LeftParen, "Expected '('.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'.")?;

        let stmt = self.statement()?;
        let stmt_span = self.nodes.get_node(stmt).span();
        let span = SourceSpan::combine(start_span, stmt_span);

        let node_id = self.nodes.create_node(AstNode::WhileStmt(WhileStmtNode {
            condition,
            body: stmt,
            span,
        }));

        Ok(node_id)
    }

    fn for_statement(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_token(TokenType::Semicolon) {
            None
        } else if self.match_token(TokenType::Var) {
            let var_decl = self.variable_declaration()?;
            Some(var_decl)
        } else {
            let expr = self.expression()?;
            self.consume(
                TokenType::Semicolon,
                "Expect ';' after for loop initializer.",
            )?;
            Some(expr)
        };

        let condition = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::Semicolon, "Expect ';' after for loop condition.")?;

        let increment = if self.check(TokenType::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let body = self.statement()?;
        let body_span = self.nodes.get_node(body).span();
        let span = SourceSpan::combine(start_span, body_span);
        let node_id = self.nodes.create_node(AstNode::ForStmt(ForStmtNode {
            initializer,
            condition,
            increment,
            body,
            span,
        }));

        Ok(node_id)
    }

    fn break_statement(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::Semicolon, "Expected ';'.")?;
        let span = SourceSpan::combine(start_span, self.get_previous_span());

        let node_id = self
            .nodes
            .create_node(AstNode::BreakStmt(BreakStmtNode { span }));

        Ok(node_id)
    }

    fn continue_statement(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::Semicolon, "Expected ';'.")?;
        let span = SourceSpan::combine(start_span, self.get_previous_span());

        let node_id = self
            .nodes
            .create_node(AstNode::ContinueStmt(ContinueStmtNode { span }));

        Ok(node_id)
    }

    fn return_statement(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_current_span();
        self.advance();

        let value = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        let end_span = self.get_previous_span();
        let span = SourceSpan::combine(start_span, end_span);

        let node_id = self
            .nodes
            .create_node(AstNode::ReturnStmt(ReturnStmtNode { value, span }));

        Ok(node_id)
    }

    fn expression_statement(&mut self) -> ParseResult<NodeId> {
        let expr = self.expression()?;

        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?; // TODO move this error to the previous line.

        let semicolon_span = self.get_previous_span();

        let expr_span = self.nodes.get_node(expr).span();

        let node_id = self.nodes.create_node(AstNode::ExprStmt(ExprStmtNode {
            expr,
            span: SourceSpan::combine(expr_span, semicolon_span),
        }));

        Ok(node_id)
    }

    fn expression(&mut self) -> ParseResult<NodeId> {
        self.assignment()
    }

    fn function_expression(&mut self) -> ParseResult<NodeId> {
        let start_span = self.get_current_span();
        self.consume(TokenType::Identifier, "Expect function name.")?;
        let name = self.get_identifier()?;

        let parameters = self.argument_parameters()?;
        let body = self.block_statement()?;
        let body_span = self.nodes.get_node(body).span();
        let span = SourceSpan::combine(start_span, body_span);

        let node_id = self
            .nodes
            .create_node(AstNode::FunctionExpr(FunctionExprNode {
                name,
                parameters,
                body,
                span,
            }));

        Ok(node_id)
    }

    fn assignment(&mut self) -> ParseResult<NodeId> {
        let expr = expression_parser::parse(self, expression_parser::Precedence::Ternary)?;

        let assignment_operator = if self.match_token(TokenType::Equals) {
            Some(AssignmentOperator::Assign)
        } else if self.match_token(TokenType::PlusAssign) {
            Some(AssignmentOperator::AddAssign)
        } else if self.match_token(TokenType::MinusAssign) {
            Some(AssignmentOperator::SubtractAssign)
        } else if self.match_token(TokenType::StarAssign) {
            Some(AssignmentOperator::MultiplyAssign)
        } else if self.match_token(TokenType::SlashAssign) {
            Some(AssignmentOperator::DivideAssign)
        } else if self.match_token(TokenType::ModuloAssign) {
            Some(AssignmentOperator::ModuloAssign)
        } else {
            None
        };

        if let Some(operator) = assignment_operator {
            let value = self.expression()?;
            let span = self.nodes.get_node(value).span();

            let expr = self.nodes.get_expr_node(expr);
            let target = match expr.node {
                ExprNode::Primary(PrimaryNode::Identifier(_)) => expr.id,
                ExprNode::Call(call_expr) => {
                    let call_operation = *self.nodes.get_node(call_expr.operation);

                    match call_operation {
                        AstNode::PropertyAccess(property) => self.nodes.create_node(
                            AstNode::PropertyAssignment(PropertyAssignmentNode {
                                property: property.identifier,
                                object: call_expr.callee,
                                span,
                            }),
                        ),
                        AstNode::IndexAccess(index) => {
                            self.nodes
                                .create_node(AstNode::IndexAssignment(IndexAssignmentNode {
                                    object: call_expr.callee,
                                    index: index.index,
                                    span,
                                }))
                        }
                        _ => {
                            return Err(QangCompilerError::new_syntax_error(
                                "Invalid assignment target".to_string(),
                                span,
                            ));
                        }
                    }
                }
                _ => {
                    return Err(QangCompilerError::new_syntax_error(
                        "Invalid assignment target".to_string(),
                        span,
                    ));
                }
            };

            let node_id = self
                .nodes
                .create_node(AstNode::AssignmentExpr(AssignmentExprNode {
                    target,
                    operator,
                    value,
                    span,
                }));

            Ok(node_id)
        } else {
            Ok(expr)
        }
    }

    fn argument_parameters(&mut self) -> ParseResult<NodeArrayId> {
        self.consume(TokenType::LeftParen, "Expect '(' before parameters.")?;

        let parameters = self.nodes.array.create();

        if self.match_token(TokenType::RightParen) {
            return Ok(parameters);
        }

        self.advance();
        let parameter = self.get_identifier()?;
        self.nodes.array.push(parameters, parameter);

        while self.match_token(TokenType::Comma) {
            if self.check(TokenType::RightParen) {
                break;
            }
            self.advance();
            let parameter = self.get_identifier()?;
            self.nodes.array.push(parameters, parameter);
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;

        Ok(parameters)
    }
}

mod expression_parser {
    use super::*;
    use crate::{frontend::node_array_arena::NodeArrayId, frontend::tokenizer};

    #[derive(Debug, PartialEq, PartialOrd)]
    #[repr(u8)]
    pub enum Precedence {
        None = 0,
        Assignment, // =
        Ternary,    // ? :
        Pipe,       // |>
        Or,         // or
        And,        // and
        Equality,   // == !=
        Comparison, // > >= < <=
        Term,       // + -
        Factor,     // * / %
        Unary,      // ! -
        Call,       // . () []
    }

    impl From<u8> for Precedence {
        fn from(byte: u8) -> Self {
            match byte {
                0 => Precedence::None,
                1 => Precedence::Assignment,
                2 => Precedence::Pipe,
                3 => Precedence::Ternary,
                4 => Precedence::Or,
                5 => Precedence::And,
                6 => Precedence::Equality,
                7 => Precedence::Comparison,
                8 => Precedence::Term,
                9 => Precedence::Factor,
                10 => Precedence::Unary,
                11 => Precedence::Call,
                _ => panic!("Unknown precedence: {}", byte),
            }
        }
    }

    impl From<Precedence> for u8 {
        fn from(precedence: Precedence) -> Self {
            precedence as u8
        }
    }

    type PrefixParseFn = fn(&mut Parser) -> ParseResult<NodeId>;
    type InfixParseFn = fn(&mut Parser, NodeId) -> ParseResult<NodeId>;

    fn number(parser: &mut Parser) -> ParseResult<NodeId> {
        let token = get_previous_token(parser);

        let span = SourceSpan::from_token(token);

        let value = token
            .lexeme(&parser.source_map)
            .iter()
            .collect::<String>()
            .parse::<f64>()
            .map_err(|_| {
                crate::QangCompilerError::new_syntax_error("Expected number.".to_string(), span)
            })?;

        let node_id = parser
            .nodes
            .create_node(AstNode::NumberLiteral(NumberLiteralNode { value, span }));

        Ok(node_id)
    }

    fn grouping_or_lambda(parser: &mut Parser) -> ParseResult<NodeId> {
        // Check if this is a lambda by looking at the current parser state
        // At this point, '(' has been consumed, so we use is_lambda_start_after_paren
        let is_lambda = parser.is_lambda_start_after_paren();

        if is_lambda {
            // Check if we're parsing a grouped lambda (like "(() -> nil)") vs direct lambda
            let is_grouped_lambda = parser.check(TokenType::LeftParen);

            let lambda_expr = lambda(parser)?;

            // Only consume closing ')' if this is a grouped lambda
            if is_grouped_lambda {
                parser.consume(TokenType::RightParen, "Expect ')' after lambda expression.")?;
            }

            Ok(lambda_expr)
        } else {
            grouping(parser)
        }
    }

    fn grouping(parser: &mut Parser) -> ParseResult<NodeId> {
        let expr = parser.expression()?;
        parser.consume(
            tokenizer::TokenType::RightParen,
            "Expect ')' after expression.",
        )?;

        Ok(expr)
    }

    fn lambda(parser: &mut Parser) -> ParseResult<NodeId> {
        let start_span = parser.get_previous_span();

        // If we encounter a '(' at the start, consume it (start of parameter list)
        if parser.check(TokenType::LeftParen) {
            parser.advance();
        }

        let parameters = parser.nodes.array.create();

        if !parser.match_token(TokenType::RightParen) {
            parser.consume(TokenType::Identifier, "Expect parameter name.")?;
            let parameter = parser.get_identifier()?;
            parser.nodes.array.push(parameters, parameter);

            while parser.match_token(TokenType::Comma) {
                if parser.check(TokenType::RightParen) {
                    break;
                }
                parser.consume(TokenType::Identifier, "Expect parameter name.")?;
                let parameter = parser.get_identifier()?;
                parser.nodes.array.push(parameters, parameter);
            }

            parser.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        }

        parser.consume(TokenType::Arrow, "Expect '->' after lambda parameters.")?;

        let body = if parser.check(TokenType::LeftBrace) {
            parser.block_statement()?
        } else {
            parser.expression()?
        };
        let body_span = parser.nodes.get_node(body).span();
        let span = SourceSpan::combine(start_span, body_span);

        let node_id = parser
            .nodes
            .create_node(AstNode::LambdaExpr(LambdaExprNode {
                parameters,
                body,
                span,
            }));

        Ok(node_id)
    }

    fn unary(parser: &mut Parser) -> ParseResult<NodeId> {
        let token = get_previous_token(parser);
        let operator_type = token.token_type;
        let operator_span = SourceSpan::from_token(token);
        let operand = parse(parser, Precedence::Unary)?;
        let operand_span = parser.nodes.get_node(operand).span();

        let span = SourceSpan::combine(operator_span, operand_span);
        match operator_type {
            tokenizer::TokenType::Minus => {
                Ok(parser.nodes.create_node(AstNode::UnaryExpr(UnaryExprNode {
                    operator: UnaryOperator::Minus,
                    operand,
                    span,
                })))
            }
            tokenizer::TokenType::Bang => {
                Ok(parser.nodes.create_node(AstNode::UnaryExpr(UnaryExprNode {
                    operator: UnaryOperator::Not,
                    operand,
                    span,
                })))
            }
            _ => Err(crate::QangCompilerError::new_syntax_error(
                "Unknown operator.".to_string(),
                span,
            )),
        }
    }

    fn literal(parser: &mut Parser) -> ParseResult<NodeId> {
        let token = get_previous_token(parser);

        let span = SourceSpan::from_token(token);

        match token.token_type {
            tokenizer::TokenType::False => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::BooleanLiteral(BooleanLiteralNode {
                        value: false,
                        span,
                    })))
            }
            tokenizer::TokenType::True => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::BooleanLiteral(BooleanLiteralNode {
                        value: true,
                        span,
                    })))
            }
            tokenizer::TokenType::Nil => Ok(parser
                .nodes
                .create_node(AstNode::NilLiteral(NilLiteralNode { span }))),
            tokenizer::TokenType::This => Ok(parser
                .nodes
                .create_node(AstNode::ThisExpr(ThisExprNode { span }))),
            tokenizer::TokenType::Super => {
                if !parser.match_token(tokenizer::TokenType::Dot) {
                    return Err(crate::QangCompilerError::new_syntax_error(
                        "Expect '.' after 'super'.".to_string(),
                        span,
                    ));
                }

                parser.consume(
                    tokenizer::TokenType::Identifier,
                    "Expect method name after 'super.'.",
                )?;

                let method_name = parser.get_identifier()?;
                let method_name_span = parser.nodes.get_node(method_name).span();
                let method_span = SourceSpan::combine(span, method_name_span);
                let node_id = parser.nodes.create_node(AstNode::SuperExpr(SuperExprNode {
                    method: method_name,
                    span: method_span,
                }));
                Ok(node_id)
            }
            _ => Err(crate::QangCompilerError::new_syntax_error(
                "Unknown literal.".to_string(),
                span,
            )),
        }
    }

    fn process_escape_sequences(raw_string: &str) -> String {
        let mut result = String::new();
        let mut chars = raw_string.chars();

        while let Some(c) = chars.next() {
            if c == '\\' {
                if let Some(escaped) = chars.next() {
                    match escaped {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        'r' => result.push('\r'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        '\'' => result.push('\''),
                        '0' => result.push('\0'),
                        _ => {
                            // This should not happen as tokenizer validates escapes
                            result.push('\\');
                            result.push(escaped);
                        }
                    }
                } else {
                    result.push('\\');
                }
            } else {
                result.push(c);
            }
        }

        result
    }

    fn string(parser: &mut Parser) -> ParseResult<NodeId> {
        let token = get_previous_token(parser);

        let value = token.lexeme(&parser.source_map);

        let span = SourceSpan::from_token(token);

        let raw_content = value[1..value.len() - 1].iter().collect::<String>();

        let processed_content = process_escape_sequences(&raw_content);

        let string_handle = parser.strings.intern(&processed_content);
        let node_id = parser
            .nodes
            .create_node(AstNode::StringLiteral(StringLiteralNode {
                value: string_handle,
                span,
            }));

        Ok(node_id)
    }

    fn identifier(parser: &mut Parser) -> ParseResult<NodeId> {
        parser.get_identifier()
    }

    fn array(parser: &mut Parser) -> ParseResult<NodeId> {
        let start_span = parser.get_previous_span();
        let array_node_id = parser.nodes.array.create();

        // Handle empty array case
        if parser.check(tokenizer::TokenType::RightSquareBracket) {
            parser.advance();
            let end_span = parser.get_previous_span();
            let span = SourceSpan::combine(start_span, end_span);
            let node_id =
                parser
                    .nodes
                    .create_node(AstNode::ArrayLiteralExpr(ArrayLiteralExprNode {
                        elements: array_node_id,
                        span,
                    }));
            return Ok(node_id);
        }

        // Parse the first expression
        let first_expr = parser.expression()?;
        parser.nodes.array.push(array_node_id, first_expr);

        // Continue parsing comma-separated elements
        while parser.match_token(tokenizer::TokenType::Comma)
            && parser
                .current_token
                .as_ref()
                .map(|t| t.token_type != TokenType::RightSquareBracket)
                .unwrap_or(false)
        {
            let expr = parser.expression()?;
            parser.nodes.array.push(array_node_id, expr);
        }

        let _ = parser.match_token(TokenType::Comma);

        parser.consume(
            tokenizer::TokenType::RightSquareBracket,
            "Expect ']' after array elements.",
        )?;

        let end_span = parser.get_previous_span();
        let span = SourceSpan::combine(start_span, end_span);
        let node_id = parser
            .nodes
            .create_node(AstNode::ArrayLiteralExpr(ArrayLiteralExprNode {
                elements: array_node_id,
                span,
            }));

        Ok(node_id)
    }

    fn map_expression(parser: &mut Parser, left: NodeId) -> ParseResult<NodeId> {
        let start_span: SourceSpan = parser.nodes.get_node(left).span();

        let parameter = if parser.match_token(TokenType::Identifier) {
            parser.get_identifier()?
        } else {
            return Err(crate::QangCompilerError::new_syntax_error(
                "Expect parameter name before '->' in map expression.".to_string(),
                parser.get_current_span(),
            ));
        };

        parser.consume(TokenType::Arrow, "Expect '->' after parameters.")?;
        let body = expression_parser::parse(parser, Precedence::Ternary)?;
        parser.consume(TokenType::Bar, "Expect '|' after map body.")?;

        let span = SourceSpan::combine(start_span, parser.get_previous_span());

        let operation = parser.nodes.create_node(AstNode::MapExpr(MapExprNode {
            parameter,
            body,
            span,
        }));
        let node_id = parser.nodes.create_node(AstNode::CallExpr(CallExprNode {
            callee: left,
            operation,
            span,
        }));

        Ok(node_id)
    }

    fn optional_map_expression(parser: &mut Parser, left: NodeId) -> ParseResult<NodeId> {
        let start_span = parser.nodes.get_node(left).span();

        let parameter = if parser.check(TokenType::Identifier) {
            parser.advance();
            parser.get_identifier()?
        } else {
            return Err(crate::QangCompilerError::new_syntax_error(
                "Expect parameter name before '->' in optional map expression.".to_string(),
                parser.get_current_span(),
            ));
        };

        parser.consume(TokenType::Arrow, "Expect '->' after parameters.")?;

        let body = expression_parser::parse(parser, Precedence::Ternary)?;

        parser.consume(TokenType::Bar, "Expect '|' after map body.")?;

        let span = SourceSpan::combine(start_span, parser.get_previous_span());

        let operation = parser
            .nodes
            .create_node(AstNode::OptionalMapExpr(OptionalMapExprNode {
                parameter,
                body,
                span,
            }));
        let node_id = parser.nodes.create_node(AstNode::CallExpr(CallExprNode {
            callee: left,
            operation,
            span,
        }));

        Ok(node_id)
    }

    fn object(parser: &mut Parser) -> ParseResult<NodeId> {
        let start_span = parser.get_previous_span();
        let entries = parser.nodes.array.create();

        if parser.check(tokenizer::TokenType::DoubleRightBrace) {
            parser.advance();
            let end_span = parser.get_previous_span();
            let span = SourceSpan::combine(start_span, end_span);
            let node_id =
                parser
                    .nodes
                    .create_node(AstNode::ObjectLiteralExpr(ObjectLiteralExprNode {
                        entries,
                        span,
                    }));
            return Ok(node_id);
        }

        while !parser.check(tokenizer::TokenType::DoubleRightBrace) && !parser.is_at_end() {
            parser.advance();
            let key = parser.get_identifier()?;
            let key_span = parser.nodes.get_node(key).span();

            if parser.match_token(tokenizer::TokenType::Equals) {
                let value = parser.expression()?;
                let value_span = parser.nodes.get_node(value).span();
                let entry = parser
                    .nodes
                    .create_node(AstNode::ObjectEntry(ObjectEntryNode {
                        key,
                        value,
                        span: SourceSpan::combine(key_span, value_span),
                    }));
                parser.nodes.array.push(entries, entry);

                if !parser.match_token(tokenizer::TokenType::Comma) {
                    break;
                }
            } else if parser.match_token(tokenizer::TokenType::Comma)
                || parser.check(tokenizer::TokenType::DoubleRightBrace)
            {
                let entry = parser
                    .nodes
                    .create_node(AstNode::ObjectEntry(ObjectEntryNode {
                        key,
                        value: key,
                        span: key_span,
                    }));
                parser.nodes.array.push(entries, entry);
            } else {
                break;
            }
        }

        parser.consume(tokenizer::TokenType::DoubleRightBrace, "Expected '}}'.")?;
        let end_span = parser.get_previous_span();

        let node_id = parser
            .nodes
            .create_node(AstNode::ObjectLiteralExpr(ObjectLiteralExprNode {
                entries,
                span: SourceSpan::combine(start_span, end_span),
            }));
        Ok(node_id)
    }

    fn binary(parser: &mut Parser, left: NodeId) -> ParseResult<NodeId> {
        let token = get_previous_token(parser);
        let span_start = parser.nodes.get_node(left).span().start;
        let token_type = token.token_type;

        let rule = get_rule(token_type);

        if rule.is_empty() {
            return Err(QangCompilerError::new_syntax_error(
                "Unexpected token.".to_string(),
                SourceSpan::from_token(token),
            ));
        }

        let precedence: Precedence = (rule.precedence as u8 + 1).into();

        let right = parse(parser, precedence)?;
        let right_span = parser.nodes.get_node(right).span();
        let span = SourceSpan::new(span_start, right_span.end);

        match token_type {
            tokenizer::TokenType::Plus => {
                Ok(parser.nodes.create_node(AstNode::TermExpr(TermExprNode {
                    left,
                    operator: TermOperator::Add,
                    right,
                    span,
                })))
            }
            tokenizer::TokenType::Minus => {
                Ok(parser.nodes.create_node(AstNode::TermExpr(TermExprNode {
                    left,
                    operator: TermOperator::Subtract,
                    right,
                    span,
                })))
            }
            tokenizer::TokenType::Star => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::FactorExpr(FactorExprNode {
                        left,
                        operator: FactorOperator::Multiply,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::Slash => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::FactorExpr(FactorExprNode {
                        left,
                        operator: FactorOperator::Divide,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::Modulo => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::FactorExpr(FactorExprNode {
                        left,
                        operator: FactorOperator::Modulo,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::EqualsEquals => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::EqualityExpr(EqualityExprNode {
                        left,
                        operator: EqualityOperator::Equal,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::BangEquals => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::EqualityExpr(EqualityExprNode {
                        left,
                        operator: EqualityOperator::NotEqual,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::Is => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::EqualityExpr(EqualityExprNode {
                        left,
                        operator: EqualityOperator::Is,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::Less => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::ComparisonExpr(ComparisonExprNode {
                        left,
                        operator: ComparisonOperator::Less,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::LessEquals => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::ComparisonExpr(ComparisonExprNode {
                        left,
                        operator: ComparisonOperator::LessEqual,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::Greater => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::ComparisonExpr(ComparisonExprNode {
                        left,
                        operator: ComparisonOperator::Greater,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::GreaterEquals => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::ComparisonExpr(ComparisonExprNode {
                        left,
                        operator: ComparisonOperator::GreaterEqual,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::And => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::LogicalAndExpr(LogicalAndExprNode {
                        left,
                        right,
                        span,
                    })))
            }
            tokenizer::TokenType::Or => {
                Ok(parser
                    .nodes
                    .create_node(AstNode::LogicalOrExpr(LogicalOrExprNode {
                        left,
                        right,
                        span,
                    })))
            }
            _ => Err(crate::QangCompilerError::new_syntax_error(
                "Unknown operator.".to_string(),
                span,
            )),
        }
    }

    fn ternary(parser: &mut Parser, left: NodeId) -> ParseResult<NodeId> {
        let then_expr = parse(parser, Precedence::Ternary)?;

        parser.consume(
            tokenizer::TokenType::Colon,
            "Expect ':' after then expression in ternary.",
        )?;

        let else_expr = parse(parser, Precedence::Ternary)?;

        let left_span = parser.nodes.get_node(left).span();
        let else_expr_span = parser.nodes.get_node(else_expr).span();
        let span = SourceSpan::combine(left_span, else_expr_span);

        let node_id = parser
            .nodes
            .create_node(AstNode::TernaryExpr(TernaryExprNode {
                condition: left,
                then_expr,
                else_expr,
                span,
            }));

        Ok(node_id)
    }

    fn pipe(parser: &mut Parser, left: NodeId) -> ParseResult<NodeId> {
        let right = parse(parser, (Precedence::Pipe as u8 + 1).into())?;
        let left_span = parser.nodes.get_node(left).span();
        let right_span = parser.nodes.get_node(right).span();
        let span = SourceSpan::combine(left_span, right_span);

        let node_id =
            parser
                .nodes
                .create_node(AstNode::PipeExpr(PipeExprNode { left, right, span }));

        Ok(node_id)
    }

    fn call(parser: &mut Parser, left: NodeId) -> ParseResult<NodeId> {
        let operation = match parser.previous_token.as_ref().unwrap().token_type {
            tokenizer::TokenType::LeftParen => {
                let args = parse_arguments(parser)?;
                parser.consume(
                    tokenizer::TokenType::RightParen,
                    "Expect ')' after arguments.",
                )?;
                let start = parser.nodes.get_node(left).span().end;
                let end = get_previous_token(parser).end;
                parser.nodes.create_node(AstNode::CallOperation(CallNode {
                    args,
                    span: SourceSpan { start, end },
                }))
            }
            tokenizer::TokenType::Dot => {
                parser.consume(
                    tokenizer::TokenType::Identifier,
                    "Expect property name after '.'.",
                )?;
                let property_span = parser.get_previous_span();
                let property_name = parser
                    .previous_token
                    .as_ref()
                    .map(|t| t.lexeme(&parser.source_map))
                    .unwrap();
                let property_handle = parser
                    .strings
                    .intern(&property_name.iter().collect::<String>());
                let identifier = parser
                    .nodes
                    .create_node(AstNode::Identifier(IdentifierNode {
                        name: property_handle,
                        span: property_span,
                    }));
                parser
                    .nodes
                    .create_node(AstNode::PropertyAccess(PropertyNode {
                        identifier,
                        span: property_span,
                    }))
            }
            tokenizer::TokenType::OptionalDot => {
                parser.consume(
                    tokenizer::TokenType::Identifier,
                    "Expect property name after '?.'.",
                )?;
                let property_span = parser.get_previous_span();
                let property_name = parser
                    .previous_token
                    .as_ref()
                    .map(|t| t.lexeme(&parser.source_map))
                    .unwrap();
                let property_handle = parser
                    .strings
                    .intern(&property_name.iter().collect::<String>());
                let identifier = parser
                    .nodes
                    .create_node(AstNode::Identifier(IdentifierNode {
                        name: property_handle,
                        span: property_span,
                    }));
                parser
                    .nodes
                    .create_node(AstNode::OptionalPropertyAccess(OptionalPropertyNode {
                        identifier,
                        span: property_span,
                    }))
            }
            tokenizer::TokenType::LeftSquareBracket => {
                let index = parser.expression()?;
                parser.consume(
                    tokenizer::TokenType::RightSquareBracket,
                    "Expect ']' after array index.",
                )?;
                let index_span = parser.nodes.get_node(index).span();
                parser.nodes.create_node(AstNode::IndexAccess(IndexNode {
                    index,
                    span: index_span,
                }))
            }
            _ => return Ok(left), // This shouldn't happen
        };

        let left_span = parser.nodes.get_node(left).span();
        let end_span = parser.get_previous_span();
        let span = SourceSpan::combine(left_span, end_span);
        let node_id = parser.nodes.create_node(AstNode::CallExpr(CallExprNode {
            callee: left,
            operation,
            span,
        }));

        Ok(node_id)
    }

    fn parse_arguments(parser: &mut Parser) -> ParseResult<NodeArrayId> {
        let arguments = parser.nodes.array.create();

        if parser.check(tokenizer::TokenType::RightParen) {
            return Ok(arguments);
        }

        let arg = parser.expression()?;
        parser.nodes.array.push(arguments, arg);

        while parser.match_token(tokenizer::TokenType::Comma)
            && parser
                .current_token
                .as_ref()
                .map(|t| t.token_type != TokenType::RightParen)
                .unwrap_or(false)
        {
            let arg = parser.expression()?;
            parser.nodes.array.push(arguments, arg);
        }

        let _ = parser.match_token(TokenType::Comma);

        Ok(arguments)
    }

    fn get_previous_token<'a>(parser: &'a Parser) -> &'a Token {
        // This should never panic because the expression parser will always have a previous token available to it.
        parser
            .previous_token
            .as_ref()
            .expect("Expected token but found none.")
    }

    pub struct ParseRule {
        infix: Option<InfixParseFn>,
        prefix: Option<PrefixParseFn>,
        precedence: Precedence,
    }

    impl ParseRule {
        fn is_empty(&self) -> bool {
            self.infix.is_none() && self.prefix.is_none() && self.precedence == Precedence::None
        }

        fn is_not_empty(&self) -> bool {
            !self.is_empty()
        }
    }

    const fn get_rule(token_type: tokenizer::TokenType) -> ParseRule {
        match token_type {
            tokenizer::TokenType::Number => ParseRule {
                prefix: Some(number),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::LeftParen => ParseRule {
                prefix: Some(grouping_or_lambda),
                infix: Some(call),
                precedence: Precedence::Call,
            },
            tokenizer::TokenType::Minus => ParseRule {
                prefix: Some(unary),
                infix: Some(binary),
                precedence: Precedence::Term,
            },
            tokenizer::TokenType::Plus => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Term,
            },
            tokenizer::TokenType::Star => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Factor,
            },
            tokenizer::TokenType::Slash => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Factor,
            },
            tokenizer::TokenType::Modulo => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Factor,
            },
            tokenizer::TokenType::Greater => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Comparison,
            },
            tokenizer::TokenType::GreaterEquals => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Comparison,
            },
            tokenizer::TokenType::Less => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Comparison,
            },
            tokenizer::TokenType::LessEquals => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Comparison,
            },
            tokenizer::TokenType::EqualsEquals => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Equality,
            },
            tokenizer::TokenType::BangEquals => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Equality,
            },
            tokenizer::TokenType::Is => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Equality,
            },
            tokenizer::TokenType::And => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::And,
            },
            tokenizer::TokenType::Or => ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Or,
            },
            tokenizer::TokenType::False => ParseRule {
                prefix: Some(literal),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::True => ParseRule {
                prefix: Some(literal),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::Nil => ParseRule {
                prefix: Some(literal),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::Bang => ParseRule {
                prefix: Some(unary),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::String => ParseRule {
                prefix: Some(string),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::Identifier => ParseRule {
                prefix: Some(identifier),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::Question => ParseRule {
                prefix: None,
                infix: Some(ternary),
                precedence: Precedence::Ternary,
            },
            tokenizer::TokenType::Pipe => ParseRule {
                prefix: None,
                infix: Some(pipe),
                precedence: Precedence::Pipe,
            },
            tokenizer::TokenType::LeftSquareBracket => ParseRule {
                prefix: Some(array),
                infix: Some(call),
                precedence: Precedence::Call,
            },
            tokenizer::TokenType::Super => ParseRule {
                prefix: Some(literal),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::This => ParseRule {
                prefix: Some(literal),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::Dot => ParseRule {
                prefix: None,
                infix: Some(call),
                precedence: Precedence::Call,
            },
            tokenizer::TokenType::DoubleLeftBrace => ParseRule {
                prefix: Some(object),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::OptionalDot => ParseRule {
                prefix: None,
                infix: Some(call),
                precedence: Precedence::Call,
            },
            tokenizer::TokenType::DoubleBar => ParseRule {
                prefix: None,
                infix: Some(map_expression),
                precedence: Precedence::Call,
            },
            tokenizer::TokenType::OptionalBar => ParseRule {
                prefix: None,
                infix: Some(optional_map_expression),
                precedence: Precedence::Call,
            },
            _ => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    pub fn parse(parser: &mut Parser, precedence: Precedence) -> ParseResult<NodeId> {
        parser.advance();

        let prefix_rule = parser
            .previous_token
            .as_ref()
            .map(|t| get_rule(t.token_type))
            // .filter(ParseRule::is_not_empty)
            .and_then(|r| r.prefix);

        let mut expr = match prefix_rule {
            Some(rule) => rule(parser)?,
            None => {
                let span = parser
                    .previous_token
                    .as_ref()
                    .map(SourceSpan::from_token)
                    .unwrap_or_default();

                return Err(crate::QangCompilerError::new_syntax_error(
                    "Expected expression.".to_string(),
                    span,
                ));
            }
        };

        while let Some(current_token) = &parser.current_token {
            let rule = get_rule(current_token.token_type);

            if precedence <= rule.precedence && rule.is_not_empty() {
                parser.advance();

                if let Some(infix_rule) = rule.infix {
                    expr = infix_rule(parser, expr)?;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        // Check for potential malformed expressions
        // If we have an identifier or other expression-starting token immediately following
        // our parsed expression, it's likely a syntax error
        if let Some(current_token) = &parser.current_token {
            match current_token.token_type {
                TokenType::Identifier
                | TokenType::Number
                | TokenType::String
                | TokenType::True
                | TokenType::False
                | TokenType::Nil
                | TokenType::This
                | TokenType::Super
                | TokenType::LeftParen => {
                    // We have an expression-like token that couldn't be parsed as part of the current expression
                    // This suggests a syntax error like missing operator or semicolon.
                    let location = parser
                        .previous_token
                        .as_ref()
                        .map(|t| SourceSpan::from_token(t).end)
                        .unwrap_or(SourceSpan::from_token(current_token).start);

                    let span = SourceSpan::new(location, location); // TODO move the logic to get this span into a helper in the Parser.

                    return Err(crate::QangCompilerError::new_syntax_error(
                        "Unexpected oprand. Missing operator or ';'.".to_string(),
                        span,
                    ));
                }
                _ => {}
            }
        }

        Ok(expr)
    }
}
