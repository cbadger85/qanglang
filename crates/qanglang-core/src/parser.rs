use crate::{
    ErrorReporter, QangSyntaxError, SourceMap,
    ast::{self, SourceSpan},
    tokenizer::{Token, TokenType, Tokenizer},
};

type ParseResult<T> = Result<T, QangSyntaxError>;

pub struct Parser<'a> {
    source_map: &'a SourceMap,
    tokens: Tokenizer<'a>,
    previous_token: Option<Token>,
    current_token: Option<Token>,
    errors: ErrorReporter,
}

impl<'a> Parser<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        let tokens = Tokenizer::new(source_map);
        let errors = ErrorReporter::new();
        let mut parser = Self {
            source_map,
            tokens,
            previous_token: None,
            current_token: None,
            errors,
        };

        parser.advance();
        parser
    }

    pub fn into_reporter(self) -> ErrorReporter {
        self.errors
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
            .map(ast::SourceSpan::from_token)
            .unwrap_or_default();

        if !self.is_at_end() {
            self.advance();
        }

        Err(QangSyntaxError::new(message.to_string(), span))
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

        self.errors.report_error(QangSyntaxError::new(
            message,
            ast::SourceSpan::from_token(token),
        ));
    }

    fn get_current_span(&self) -> ast::SourceSpan {
        self.current_token
            .as_ref()
            .map(ast::SourceSpan::from_token)
            .unwrap_or_default()
    }

    fn get_previous_span(&self) -> ast::SourceSpan {
        self.previous_token
            .as_ref()
            .map(ast::SourceSpan::from_token)
            .unwrap_or_default()
    }

    fn get_identifier(&mut self) -> ParseResult<ast::Identifier> {
        let token = self.previous_token.as_ref();
        let span = token
            .map(ast::SourceSpan::from_token)
            .unwrap_or(ast::SourceSpan { start: 0, end: 0 });

        if let Some(token) = token {
            let name = token.lexeme(self.source_map);
            Ok(ast::Identifier::new(
                name.iter().collect::<String>().into_boxed_str(),
                span,
            ))
        } else {
            Err(QangSyntaxError::new(
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
                | TokenType::Try
                | TokenType::Catch
                | TokenType::Finally
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

    pub fn parse(&mut self) -> ast::Program {
        let start_span = self.get_current_span();
        let mut decls = Vec::new();

        while !self.is_at_end() {
            if let Some(decl) = self.declaration() {
                decls.push(decl);
            }
        }

        let end_span = self.get_current_span();

        ast::Program::new(decls, ast::SourceSpan::combine(start_span, end_span))
    }

    fn declaration(&mut self) -> Option<ast::Decl> {
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
            _ => self.declaration_statement(),
        };

        match result {
            Ok(decl) => Some(decl),
            Err(error) => {
                let formatted_error = QangSyntaxError::new(error.message, error.span);
                self.errors.report_error(formatted_error);
                self.synchronize();
                None
            }
        }
    }

    fn variable_declaration(&mut self) -> ParseResult<ast::Decl> {
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
        let span = ast::SourceSpan::combine(var_span, semicolon_span);

        Ok(ast::Decl::Variable(ast::VariableDecl {
            name: identifier,
            initializer,
            span,
        }))
    }

    fn class_declaration(&mut self) -> ParseResult<ast::Decl> {
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

        let mut members = Vec::new();
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            members.push(self.class_member()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.")?;
        let end_span = self.get_previous_span();
        let span = ast::SourceSpan::combine(start_span, end_span);

        Ok(ast::Decl::Class(ast::ClassDecl {
            name,
            superclass,
            members,
            span,
        }))
    }

    fn class_member(&mut self) -> ParseResult<ast::ClassMember> {
        if self.check(TokenType::Identifier) {
            if self
                .tokens
                .peek()
                .map(|t| t.token_type == TokenType::LeftParen)
                .unwrap_or(false)
            {
                Ok(ast::ClassMember::Method(self.function_expression()?))
            } else {
                Ok(ast::ClassMember::Field(self.field_declaration()?))
            }
        } else {
            Err(QangSyntaxError::new(
                "Expected field or method declaration.".to_string(),
                self.get_current_span(),
            ))
        }
    }

    fn field_declaration(&mut self) -> ParseResult<ast::FieldDecl> {
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
        let span = ast::SourceSpan::combine(start_span, end_span);

        Ok(ast::FieldDecl {
            name,
            initializer,
            span,
        })
    }

    fn function_declaration(&mut self) -> ParseResult<ast::Decl> {
        let function_expr = self.function_expression()?;
        let span = ast::SourceSpan::combine(self.get_previous_span(), function_expr.span);

        Ok(ast::Decl::Function(ast::FunctionDecl {
            function: function_expr,
            span,
        }))
    }

    fn declaration_statement(&mut self) -> ParseResult<ast::Decl> {
        let current_token_type = self
            .current_token
            .as_ref()
            .ok_or(QangSyntaxError::new(
                "Expected statement.".to_string(),
                self.previous_token
                    .as_ref()
                    .map(SourceSpan::from_token)
                    .unwrap_or_default(),
            ))?
            .token_type;

        match current_token_type {
            TokenType::While => Ok(ast::Decl::Stmt(ast::Stmt::While(self.while_statement()?))),
            TokenType::If => Ok(ast::Decl::Stmt(ast::Stmt::If(self.if_statement()?))),
            TokenType::LeftBrace => Ok(ast::Decl::Stmt(ast::Stmt::Block(self.block_statement()?))),
            TokenType::For => Ok(ast::Decl::Stmt(ast::Stmt::For(self.for_statement()?))),
            TokenType::Break => Ok(ast::Decl::Stmt(ast::Stmt::Break(self.break_statement()?))),
            TokenType::Continue => Ok(ast::Decl::Stmt(ast::Stmt::Continue(
                self.continue_statement()?,
            ))),
            TokenType::Return => Ok(ast::Decl::Stmt(ast::Stmt::Return(self.return_statement()?))),
            TokenType::Throw => Ok(ast::Decl::Stmt(ast::Stmt::Throw(self.throw_statement()?))),
            TokenType::Try => Ok(ast::Decl::Stmt(ast::Stmt::Try(self.try_statement()?))),
            _ => Ok(ast::Decl::Stmt(self.statement()?)),
        }
    }

    fn block_statement(&mut self) -> ParseResult<ast::BlockStmt> {
        let start_span = self.get_current_span();
        self.advance();
        let mut decls = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            if let Some(decl) = self.declaration() {
                decls.push(decl);
            }
        }
        self.consume(TokenType::RightBrace, "Expected '}'.")?;
        let end_span = self.get_current_span();

        Ok(ast::BlockStmt {
            decls,
            span: ast::SourceSpan::combine(start_span, end_span),
        })
    }

    fn if_statement(&mut self) -> ParseResult<ast::IfStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::LeftParen, "Expected '('.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'.")?;

        let then_branch = Box::new(self.statement()?);

        let (else_branch, span) = if self.match_token(TokenType::Else) {
            let else_stmt = self.statement()?;
            let span = ast::SourceSpan::combine(start_span, else_stmt.span());
            (Some(Box::new(else_stmt)), span)
        } else {
            (
                None,
                ast::SourceSpan::combine(start_span, then_branch.span()),
            )
        };

        Ok(ast::IfStmt {
            condition,
            then_branch,
            else_branch,
            span,
        })
    }

    fn statement(&mut self) -> ParseResult<ast::Stmt> {
        let current_token_type = self
            .current_token
            .as_ref()
            .ok_or(QangSyntaxError::new(
                "Expected statement.".to_string(),
                self.previous_token
                    .as_ref()
                    .map(SourceSpan::from_token)
                    .unwrap_or_default(),
            ))?
            .token_type;

        match current_token_type {
            TokenType::While => Ok(ast::Stmt::While(self.while_statement()?)),
            TokenType::If => Ok(ast::Stmt::If(self.if_statement()?)),
            TokenType::LeftBrace => Ok(ast::Stmt::Block(self.block_statement()?)),
            TokenType::For => Ok(ast::Stmt::For(self.for_statement()?)),
            TokenType::Break => Ok(ast::Stmt::Break(self.break_statement()?)),
            TokenType::Continue => Ok(ast::Stmt::Continue(self.continue_statement()?)),
            TokenType::Return => Ok(ast::Stmt::Return(self.return_statement()?)),
            TokenType::Throw => Ok(ast::Stmt::Throw(self.throw_statement()?)),
            TokenType::Try => Ok(ast::Stmt::Try(self.try_statement()?)),
            _ => Ok(ast::Stmt::Expr(self.expression_statement()?)),
        }
    }

    fn while_statement(&mut self) -> ParseResult<ast::WhileStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::LeftParen, "Expected '('.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'.")?;

        let block_stmt = self.block_statement()?;
        let span = ast::SourceSpan::combine(start_span, block_stmt.span);
        let body = Box::new(ast::Stmt::Block(block_stmt));

        Ok(ast::WhileStmt {
            condition,
            body,
            span,
        })
    }

    fn for_statement(&mut self) -> ParseResult<ast::ForStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_token(TokenType::Semicolon) {
            None
        } else if self.match_token(TokenType::Var) {
            let var_decl = self.variable_declaration()?;
            if let ast::Decl::Variable(var_decl) = var_decl {
                Some(ast::ForInitializer::Variable(var_decl))
            } else {
                return Err(QangSyntaxError::new(
                    "Expected variable declaration.".to_string(),
                    self.get_current_span(),
                ));
            }
        } else {
            let expr = self.expression()?;
            self.consume(
                TokenType::Semicolon,
                "Expect ';' after for loop initializer.",
            )?;
            Some(ast::ForInitializer::Expr(expr))
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

        let body = Box::new(ast::Stmt::Block(self.block_statement()?));
        let span = ast::SourceSpan::combine(start_span, body.span());

        Ok(ast::ForStmt {
            initializer,
            condition,
            increment,
            body,
            span,
        })
    }

    fn break_statement(&mut self) -> ParseResult<ast::BreakStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::Semicolon, "Expected ';'.")?;
        let span = ast::SourceSpan::combine(start_span, self.get_previous_span());

        Ok(ast::BreakStmt { span })
    }

    fn continue_statement(&mut self) -> ParseResult<ast::ContinueStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::Semicolon, "Expected ';'.")?;
        let span = ast::SourceSpan::combine(start_span, self.get_previous_span());

        Ok(ast::ContinueStmt { span })
    }

    fn return_statement(&mut self) -> ParseResult<ast::ReturnStmt> {
        let start_span = self.get_current_span();
        self.advance();

        let value = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        let end_span = self.get_previous_span();
        let span = ast::SourceSpan::combine(start_span, end_span);

        Ok(ast::ReturnStmt { value, span })
    }

    fn throw_statement(&mut self) -> ParseResult<ast::ThrowStmt> {
        let start_span = self.get_current_span();
        self.advance();

        let value = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenType::Semicolon, "Expect ';' after throw value.")?;
        let end_span = self.get_previous_span();
        let span = ast::SourceSpan::combine(start_span, end_span);

        Ok(ast::ThrowStmt { value, span })
    }

    fn try_statement(&mut self) -> ParseResult<ast::TryStmt> {
        let start_span = self.get_current_span();
        self.advance();

        let try_block = self.block_statement()?;

        let catch_clause = if self.match_token(TokenType::Catch) {
            let catch_start = self.get_previous_span();

            let parameter = if self.match_token(TokenType::LeftParen) {
                self.consume(TokenType::Identifier, "Expect parameter name.")?;
                let param = Some(self.get_identifier()?);
                self.consume(TokenType::RightParen, "Expect ')' after catch parameter.")?;
                param
            } else {
                None
            };

            let body = self.block_statement()?;
            let catch_span = ast::SourceSpan::combine(catch_start, body.span);

            Some(ast::CatchClause {
                parameter,
                body,
                span: catch_span,
            })
        } else {
            None
        };

        let finally_block = if self.match_token(TokenType::Finally) {
            Some(self.block_statement()?)
        } else {
            None
        };

        if catch_clause.is_none() && finally_block.is_none() {
            return Err(QangSyntaxError::new(
                "Expected 'catch' or 'finally' after try block.".to_string(),
                self.get_current_span(),
            ));
        }

        let end_span = finally_block
            .as_ref()
            .map(|b| b.span)
            .or_else(|| catch_clause.as_ref().map(|c| c.span))
            .unwrap_or(try_block.span);

        let span = ast::SourceSpan::combine(start_span, end_span);

        Ok(ast::TryStmt {
            try_block,
            catch_clause,
            finally_block,
            span,
        })
    }

    fn expression_statement(&mut self) -> ParseResult<ast::ExprStmt> {
        let expr = self.expression()?;

        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?; // TODO move this error to the previous line.

        let semicolon_span = self.get_previous_span();

        let expr_span = expr.span();

        Ok(ast::ExprStmt {
            expr,
            span: ast::SourceSpan::combine(expr_span, semicolon_span),
        })
    }

    fn expression(&mut self) -> ParseResult<ast::Expr> {
        self.assignment()
    }

    fn function_expression(&mut self) -> ParseResult<ast::FunctionExpr> {
        let start_span = self.get_current_span();
        self.consume(TokenType::Identifier, "Expect function name.")?;
        let name = self.get_identifier()?;

        let parameters = self.argument_parameters()?;
        let body = self.block_statement()?;
        let span = ast::SourceSpan::combine(start_span, body.span);

        Ok(ast::FunctionExpr {
            name,
            parameters,
            body,
            span,
        })
    }

    fn assignment(&mut self) -> ParseResult<ast::Expr> {
        let expr = expression_parser::parse(self, expression_parser::Precedence::Ternary)?;

        if self.match_token(TokenType::Equals) {
            let value = Box::new(self.expression()?);
            let span = ast::SourceSpan::combine(expr.span(), value.span());

            match expr {
                ast::Expr::Primary(ast::PrimaryExpr::Identifier(id)) => {
                    Ok(ast::Expr::Assignment(ast::AssignmentExpr {
                        target: ast::AssignmentTarget::Identifier(id),
                        value,
                        span,
                    }))
                }
                ast::Expr::Call(call_expr) => {
                    if let ast::CallOperation::Property(property) = call_expr.operation.as_ref() {
                        let property_access = ast::PropertyAccess {
                            object: call_expr.callee,
                            property: property.clone(),
                            span: call_expr.span,
                        };
                        Ok(ast::Expr::Assignment(ast::AssignmentExpr {
                            target: ast::AssignmentTarget::Property(property_access),
                            value,
                            span,
                        }))
                    } else {
                        Err(QangSyntaxError::new(
                            "Invalid assignment target".to_string(),
                            span,
                        ))
                    }
                }
                _ => Err(QangSyntaxError::new(
                    "Invalid assignment target".to_string(),
                    span,
                )),
            }
        } else {
            Ok(expr)
        }
    }

    fn argument_parameters(&mut self) -> ParseResult<Vec<ast::Identifier>> {
        self.consume(TokenType::LeftParen, "Expect '(' before parameters.")?;

        let mut parameters = Vec::new();

        if self.match_token(TokenType::RightParen) {
            return Ok(parameters);
        }

        self.advance();
        parameters.push(self.get_identifier()?);

        while self.match_token(TokenType::Comma) {
            if self.check(TokenType::RightParen) {
                break;
            }
            self.advance();
            parameters.push(self.get_identifier()?);
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;

        Ok(parameters)
    }
}

mod expression_parser {
    use super::*;
    use crate::tokenizer;

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

    type PrefixParseFn = fn(&mut Parser) -> ParseResult<ast::Expr>;
    type InfixParseFn = fn(&mut Parser, ast::Expr) -> ParseResult<ast::Expr>;

    fn number(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let token = get_previous_token(parser);

        let span = ast::SourceSpan::from_token(token);

        let value = token
            .lexeme(parser.source_map)
            .iter()
            .collect::<String>()
            .parse::<f64>()
            .map_err(|_| crate::QangSyntaxError::new("Expected number.".to_string(), span))?;

        Ok(ast::Expr::Primary(ast::PrimaryExpr::Number(
            ast::NumberLiteral { value, span },
        )))
    }

    fn grouping_or_lambda(parser: &mut Parser) -> ParseResult<ast::Expr> {
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

    fn grouping(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let expr = parser.expression()?;
        parser.consume(
            tokenizer::TokenType::RightParen,
            "Expect ')' after expression.",
        )?;

        Ok(expr)
    }

    fn lambda(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let start_span = parser.get_previous_span();

        // If we encounter a '(' at the start, consume it (start of parameter list)
        if parser.check(TokenType::LeftParen) {
            parser.advance();
        }

        let mut parameters = Vec::new();

        if !parser.match_token(TokenType::RightParen) {
            parser.consume(TokenType::Identifier, "Expect parameter name.")?;
            parameters.push(parser.get_identifier()?);

            while parser.match_token(TokenType::Comma) {
                if parser.check(TokenType::RightParen) {
                    break;
                }
                parser.consume(TokenType::Identifier, "Expect parameter name.")?;
                parameters.push(parser.get_identifier()?);
            }

            parser.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        }

        parser.consume(TokenType::Arrow, "Expect '->' after lambda parameters.")?;

        let body = if parser.check(TokenType::LeftBrace) {
            Box::new(ast::LambdaBody::Block(parser.block_statement()?))
        } else {
            let expr = parser.expression()?;
            Box::new(ast::LambdaBody::Expr(Box::new(expr)))
        };

        let span = ast::SourceSpan::combine(start_span, body.span());

        Ok(ast::Expr::Primary(ast::PrimaryExpr::Lambda(Box::new(
            ast::LambdaExpr {
                parameters,
                body,
                span,
            },
        ))))
    }

    fn unary(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let token = get_previous_token(parser);
        let operator_type = token.token_type;
        let operator_span = ast::SourceSpan::from_token(token);
        let operand = Box::new(parse(parser, Precedence::Unary)?);

        let span = ast::SourceSpan::combine(operator_span, operand.span());
        match operator_type {
            tokenizer::TokenType::Minus => Ok(ast::Expr::Unary(ast::UnaryExpr {
                operator: ast::UnaryOperator::Minus,
                operand,
                span,
            })),
            tokenizer::TokenType::Bang => Ok(ast::Expr::Unary(ast::UnaryExpr {
                operator: ast::UnaryOperator::Not,
                operand,
                span,
            })),
            _ => Err(crate::QangSyntaxError::new(
                "Unknown operator.".to_string(),
                span,
            )),
        }
    }

    fn literal(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let token = get_previous_token(parser);

        let span = ast::SourceSpan::from_token(token);

        match token.token_type {
            tokenizer::TokenType::False => Ok(ast::Expr::Primary(ast::PrimaryExpr::Boolean(
                ast::BooleanLiteral { value: false, span },
            ))),
            tokenizer::TokenType::True => Ok(ast::Expr::Primary(ast::PrimaryExpr::Boolean(
                ast::BooleanLiteral { value: true, span },
            ))),
            tokenizer::TokenType::Nil => {
                Ok(ast::Expr::Primary(ast::PrimaryExpr::Nil(ast::NilLiteral {
                    span,
                })))
            }
            tokenizer::TokenType::This => {
                Ok(ast::Expr::Primary(ast::PrimaryExpr::This(ast::ThisExpr {
                    span,
                })))
            }
            tokenizer::TokenType::Super => {
                if !parser.match_token(tokenizer::TokenType::Dot) {
                    return Err(crate::QangSyntaxError::new(
                        "Expect '.' after 'super'.".to_string(),
                        span,
                    ));
                }

                parser.consume(
                    tokenizer::TokenType::Identifier,
                    "Expect method name after 'super.'.",
                )?;

                let method_name = parser.get_identifier()?;
                let method_span = ast::SourceSpan::combine(span, method_name.span);

                Ok(ast::Expr::Primary(ast::PrimaryExpr::Super(
                    ast::SuperExpr::Method(ast::SuperMethod {
                        method: method_name,
                        span: method_span,
                    }),
                )))
            }
            _ => Err(crate::QangSyntaxError::new(
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

    fn string(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let token = get_previous_token(parser);

        let value = token.lexeme(parser.source_map);

        let span = ast::SourceSpan::from_token(token);

        let raw_content = value[1..value.len() - 1].iter().collect::<String>();

        let processed_content = process_escape_sequences(&raw_content);

        Ok(ast::Expr::Primary(ast::PrimaryExpr::String(
            ast::StringLiteral {
                value: processed_content.into_boxed_str(),
                span,
            },
        )))
    }

    fn identifier(parser: &mut Parser) -> ParseResult<ast::Expr> {
        Ok(ast::Expr::Primary(ast::PrimaryExpr::Identifier(
            parser.get_identifier()?,
        )))
    }

    fn array(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let start_span = parser.get_previous_span();

        // Handle empty array case
        if parser.check(tokenizer::TokenType::RightSquareBracket) {
            parser.advance();
            let end_span = parser.get_previous_span();
            let span = ast::SourceSpan::combine(start_span, end_span);
            return Ok(ast::Expr::Primary(ast::PrimaryExpr::Array(
                ast::ArrayLiteral {
                    elements: Vec::new(),
                    span,
                },
            )));
        }

        // Parse the first expression
        let first_expr = parser.expression()?;

        // Check if this is array-of-length syntax [length; initializer?]
        if parser.match_token(tokenizer::TokenType::Semicolon) {
            // This is array-of-length syntax
            let length = Box::new(first_expr);
            let initializer = if parser.check(tokenizer::TokenType::RightSquareBracket) {
                // No initializer provided, will default to nil
                None
            } else {
                // Parse the initializer expression
                Some(Box::new(parser.expression()?))
            };

            parser.consume(
                tokenizer::TokenType::RightSquareBracket,
                "Expect ']' after array-of-length syntax.",
            )?;

            let end_span = parser.get_previous_span();
            let span = ast::SourceSpan::combine(start_span, end_span);

            return Ok(ast::Expr::Primary(ast::PrimaryExpr::ArrayOfLength(
                ast::ArrayOfLength {
                    length,
                    initializer,
                    span,
                },
            )));
        }

        // This is regular array literal syntax [elem1, elem2, ...]
        let mut elements = vec![first_expr];

        // Continue parsing comma-separated elements
        while parser.match_token(tokenizer::TokenType::Comma)
            && parser
                .current_token
                .as_ref()
                .map(|t| t.token_type != TokenType::RightSquareBracket)
                .unwrap_or(false)
        {
            elements.push(parser.expression()?);
        }

        let _ = parser.match_token(TokenType::Comma);

        parser.consume(
            tokenizer::TokenType::RightSquareBracket,
            "Expect ']' after array elements.",
        )?;

        let end_span = parser.get_previous_span();
        let span = ast::SourceSpan::combine(start_span, end_span);

        Ok(ast::Expr::Primary(ast::PrimaryExpr::Array(
            ast::ArrayLiteral { elements, span },
        )))
    }

    fn object(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let start_span = parser.get_previous_span();
        let mut entries: Vec<ast::ObjectEntry> = Vec::new();

        // Handle empty array case
        if parser.check(tokenizer::TokenType::RightBrace) {
            parser.advance();
            let end_span = parser.get_previous_span();
            let span = ast::SourceSpan::combine(start_span, end_span);
            return Ok(ast::Expr::Primary(ast::PrimaryExpr::ObjectLiteral(
                ast::ObjectLiteral { entries, span },
            )));
        }

        while !parser.check(tokenizer::TokenType::RightBrace) && !parser.is_at_end() {
            parser.advance();
            let key = parser.get_identifier()?;
            let key_span = key.span;

            if parser.match_token(tokenizer::TokenType::Equals) {
                let value = parser.expression()?;
                let value_span = value.span();
                entries.push(ast::ObjectEntry {
                    key,
                    value: Box::new(value),
                    span: SourceSpan::combine(key_span, value_span),
                });

                if !parser.match_token(tokenizer::TokenType::Comma) {
                    break;
                }
            } else if parser.match_token(tokenizer::TokenType::Comma)
                || parser.check(tokenizer::TokenType::RightBrace)
            {
                let value = ast::Expr::Primary(ast::PrimaryExpr::Identifier(key.clone()));
                entries.push(ast::ObjectEntry {
                    key,
                    value: Box::new(value),
                    span: key_span,
                });
            } else {
                break;
            }
        }

        parser.consume(tokenizer::TokenType::RightBrace, "Expected '}'.")?;
        let end_span = parser.get_previous_span();

        Ok(ast::Expr::Primary(ast::PrimaryExpr::ObjectLiteral(
            ast::ObjectLiteral {
                entries,
                span: SourceSpan::combine(start_span, end_span),
            },
        )))
    }

    fn binary(parser: &mut Parser, left: ast::Expr) -> ParseResult<ast::Expr> {
        let token = get_previous_token(parser);
        let span_start = left.span().start;
        let token_type = token.token_type;

        let rule = get_rule(token_type);

        if rule.is_empty() {
            return Err(QangSyntaxError::new(
                "Unexpected token.".to_string(),
                ast::SourceSpan::from_token(token),
            ));
        }

        let precedence: Precedence = (rule.precedence as u8 + 1).into();

        let right = parse(parser, precedence)?;
        let span = ast::SourceSpan::new(span_start, right.span().end);

        match token_type {
            tokenizer::TokenType::Plus => Ok(ast::Expr::Term(ast::TermExpr {
                left: Box::new(left),
                operator: ast::TermOperator::Add,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Minus => Ok(ast::Expr::Term(ast::TermExpr {
                left: Box::new(left),
                operator: ast::TermOperator::Subtract,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Star => Ok(ast::Expr::Factor(ast::FactorExpr {
                left: Box::new(left),
                operator: ast::FactorOperator::Multiply,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Slash => Ok(ast::Expr::Factor(ast::FactorExpr {
                left: Box::new(left),
                operator: ast::FactorOperator::Divide,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Modulo => Ok(ast::Expr::Factor(ast::FactorExpr {
                left: Box::new(left),
                operator: ast::FactorOperator::Modulo,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::EqualsEquals => Ok(ast::Expr::Equality(ast::EqualityExpr {
                left: Box::new(left),
                operator: ast::EqualityOperator::Equal,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::BangEquals => Ok(ast::Expr::Equality(ast::EqualityExpr {
                left: Box::new(left),
                operator: ast::EqualityOperator::NotEqual,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Less => Ok(ast::Expr::Comparison(ast::ComparisonExpr {
                left: Box::new(left),
                operator: ast::ComparisonOperator::Less,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::LessEquals => Ok(ast::Expr::Comparison(ast::ComparisonExpr {
                left: Box::new(left),
                operator: ast::ComparisonOperator::LessEqual,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Greater => Ok(ast::Expr::Comparison(ast::ComparisonExpr {
                left: Box::new(left),
                operator: ast::ComparisonOperator::Greater,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::GreaterEquals => Ok(ast::Expr::Comparison(ast::ComparisonExpr {
                left: Box::new(left),
                operator: ast::ComparisonOperator::GreaterEqual,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::And => Ok(ast::Expr::LogicalAnd(ast::LogicalAndExpr {
                left: Box::new(left),
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Or => Ok(ast::Expr::LogicalOr(ast::LogicalOrExpr {
                left: Box::new(left),
                right: Box::new(right),
                span,
            })),
            _ => Err(crate::QangSyntaxError::new(
                "Unknown operator.".to_string(),
                span,
            )),
        }
    }

    fn ternary(parser: &mut Parser, left: ast::Expr) -> ParseResult<ast::Expr> {
        let then_expr = Box::new(parse(parser, Precedence::Ternary)?);

        parser.consume(
            tokenizer::TokenType::Colon,
            "Expect ':' after then expression in ternary.",
        )?;

        // Parse the "else" expression with ternary precedence (right associative)
        let else_expr = Box::new(parse(parser, Precedence::Ternary)?);

        let span = ast::SourceSpan::combine(left.span(), else_expr.span());

        Ok(ast::Expr::Ternary(ast::TernaryExpr {
            condition: Box::new(left),
            then_expr: Some(then_expr),
            else_expr: Some(else_expr),
            span,
        }))
    }

    fn pipe(parser: &mut Parser, left: ast::Expr) -> ParseResult<ast::Expr> {
        let right = Box::new(parse(parser, (Precedence::Pipe as u8 + 1).into())?);
        let span = ast::SourceSpan::combine(left.span(), right.span());

        Ok(ast::Expr::Pipe(ast::PipeExpr {
            left: Box::new(left),
            right: Some(right),
            span,
        }))
    }

    fn call(parser: &mut Parser, left: ast::Expr) -> ParseResult<ast::Expr> {
        let operation = match parser.previous_token.as_ref().unwrap().token_type {
            tokenizer::TokenType::LeftParen => {
                let arguments = parse_arguments(parser)?;
                parser.consume(
                    tokenizer::TokenType::RightParen,
                    "Expect ')' after arguments.",
                )?;
                ast::CallOperation::Call(arguments)
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
                    .map(|t| t.lexeme(parser.source_map))
                    .unwrap();
                let property = ast::Identifier::new(
                    property_name.iter().collect::<String>().into_boxed_str(),
                    property_span,
                );
                ast::CallOperation::Property(property)
            }
            tokenizer::TokenType::LeftSquareBracket => {
                let index = parser.expression()?;
                parser.consume(
                    tokenizer::TokenType::RightSquareBracket,
                    "Expect ']' after array index.",
                )?;
                ast::CallOperation::Index(index)
            }
            _ => return Ok(left), // This shouldn't happen
        };

        let end_span = parser.get_previous_span();
        let span = ast::SourceSpan::combine(left.span(), end_span);

        Ok(ast::Expr::Call(ast::CallExpr {
            callee: Box::new(left),
            operation: Box::new(operation),
            span,
        }))
    }

    fn parse_arguments(parser: &mut Parser) -> ParseResult<Vec<ast::Expr>> {
        let mut arguments = Vec::new();

        if parser.check(tokenizer::TokenType::RightParen) {
            return Ok(arguments);
        }

        arguments.push(parser.expression()?);

        while parser.match_token(tokenizer::TokenType::Comma)
            && parser
                .current_token
                .as_ref()
                .map(|t| t.token_type != TokenType::RightParen)
                .unwrap_or(false)
        {
            arguments.push(parser.expression()?);
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
                infix: Some(ternary),
                prefix: None,
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
            tokenizer::TokenType::ColonBrace => ParseRule {
                prefix: Some(object),
                infix: None,
                precedence: Precedence::None,
            },
            _ => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    pub fn parse(parser: &mut Parser, precedence: Precedence) -> ParseResult<ast::Expr> {
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
                    .map(ast::SourceSpan::from_token)
                    .unwrap_or_default();

                return Err(crate::QangSyntaxError::new(
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

                    return Err(crate::QangSyntaxError::new(
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
