use crate::{
    ErrorReporter, QangError, SourceMap,
    ast::{self, SourceSpan},
    tokenizer::{Token, TokenType, Tokenizer},
};

type ParseResult<T> = Result<T, QangError>;

pub struct Parser<'a> {
    source_map: &'a SourceMap,
    tokens: Tokenizer<'a>,
    previous_token: Option<Token>,
    current_token: Option<Token>,
    errors: ErrorReporter<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        let mut parser = Self {
            source_map,
            tokens: Tokenizer::new(source_map),
            previous_token: None,
            current_token: None,
            errors: ErrorReporter::new(source_map),
        };

        parser.advance();
        parser
    }

    pub fn into_reporter(self) -> ErrorReporter<'a> {
        self.errors
    }

    fn advance(&mut self) {
        self.previous_token = self.current_token.take();

        while let Some(token) = self.tokens.next() {
            if token.token_type == TokenType::Error {
                println!("Is token error.");
                self.handle_tokenizer_error(&token);
                continue;
            }

            if token.token_type == TokenType::Comment {
                continue;
            }

            self.current_token = Some(token);
            break;
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> ParseResult<()> {
        if let Some(current_token_type) = self.current_token.as_ref().map(|t| &t.token_type) {
            if &token_type == current_token_type {
                self.advance();
                return Ok(());
            }
        }

        let span = self
            .current_token
            .as_ref()
            .map(ast::SourceSpan::from_token)
            .unwrap_or_default();

        if !self.is_at_end() {
            self.advance();
        }

        Err(QangError::parse_error(message, span))
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
            .unwrap_or("Tokenization error");

        self.errors
            .report_parse_error(message, ast::SourceSpan::from_token(token));
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
            Ok(ast::Identifier::new(name, span))
        } else {
            Err(QangError::parse_error("Expected identifier.", span))
        }
    }

    fn synchronize(&mut self) {
        loop {
            // If current_token is None, we've reached the end of input
            let current_token_type = match self.current_token.as_ref() {
                Some(token) => &token.token_type,
                None => {
                    // No more tokens available - treat as EOF and exit
                    return;
                }
            };

            // Check if we've reached EOF
            if current_token_type == &TokenType::Eof {
                break;
            }

            // Check if previous token was a semicolon (good synchronization point)
            if let Some(prev_token) = &self.previous_token {
                if prev_token.token_type == TokenType::Semicolon {
                    return;
                }
            }

            // Check if current token is a statement keyword (good synchronization point)
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

    fn is_lambda_start(&mut self) -> bool {
        // Case 1: () ->
        if let Some(token) = self.tokens.peek() {
            if token.token_type == TokenType::RightParen {
                return self
                    .tokens
                    .peek_ahead(1)
                    .map(|t| t.token_type == TokenType::Arrow)
                    .unwrap_or(false);
            }
        }

        // Case 2: (id) -> or (id, id, ...) ->
        let mut offset = 0;
        let mut expecting_identifier = true;

        loop {
            match self.tokens.peek_ahead(offset) {
                Some(token) => match token.token_type {
                    TokenType::Identifier if expecting_identifier => {
                        expecting_identifier = false;
                        offset += 1;
                    }
                    TokenType::Comma if !expecting_identifier => {
                        expecting_identifier = true;
                        offset += 1;
                    }
                    TokenType::RightParen if !expecting_identifier => {
                        // Found valid param list, check for arrow
                        return self
                            .tokens
                            .peek_ahead(offset + 1)
                            .map(|t| t.token_type == TokenType::Arrow)
                            .unwrap_or(false);
                    }
                    _ => return false, // Invalid pattern
                },
                None => return false, // Hit EOF
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
            _ => self.statement(),
        };

        match result {
            Ok(decl) => Some(decl),
            Err(error) => {
                self.errors.report_error(error);
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
            if self.is_lambda_start() {
                let lambda_expr = self.lambda_expression()?;
                Some(ast::Expr::Primary(ast::PrimaryExpr::Lambda(Box::new(
                    lambda_expr,
                ))))
            } else {
                Some(self.expression()?)
            }
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
            Err(QangError::parse_error(
                "Expected field or method declaration.",
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

    fn statement(&mut self) -> ParseResult<ast::Decl> {
        let current_token_type = self
            .current_token
            .as_ref()
            .ok_or(QangError::parse_error(
                "Expected statement.",
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
            _ => Ok(ast::Decl::Stmt(ast::Stmt::Expr(
                self.expression_statement()?,
            ))),
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

        let then_branch = Box::new(ast::Stmt::Block(self.block_statement()?));

        println!("current token: {:?}", self.current_token);

        let (else_branch, span) = if self.match_token(TokenType::Else) {
            let block_stmt = ast::Stmt::Block(self.block_statement()?);
            let span = ast::SourceSpan::combine(start_span, block_stmt.span());
            (Some(Box::new(block_stmt)), span)
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

        // Parse initializer
        let initializer = if self.match_token(TokenType::Semicolon) {
            None
        } else if self.match_token(TokenType::Var) {
            let var_decl = self.variable_declaration()?;
            if let ast::Decl::Variable(var_decl) = var_decl {
                Some(ast::ForInitializer::Variable(var_decl))
            } else {
                return Err(QangError::parse_error(
                    "Expected variable declaration.",
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

        // Parse condition
        let condition = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::Semicolon, "Expect ';' after for loop condition.")?;

        // Parse increment
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

        // Validate that we have at least catch or finally
        if catch_clause.is_none() && finally_block.is_none() {
            return Err(QangError::parse_error(
                "Expected 'catch' or 'finally' after try block.",
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

        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;

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

    fn lambda_expression(&mut self) -> ParseResult<ast::LambdaExpr> {
        let start_span = self.get_current_span();

        // We should be at '('
        self.consume(TokenType::LeftParen, "Expect '(' at start of lambda.")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                self.consume(TokenType::Identifier, "Expect parameter name.")?;
                parameters.push(self.get_identifier()?);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(TokenType::Arrow, "Expect '->' after lambda parameters.")?;

        let body = if self.check(TokenType::LeftBrace) {
            Box::new(ast::LambdaBody::Block(self.block_statement()?))
        } else {
            let expr = self.expression()?;
            Box::new(ast::LambdaBody::Expr(Box::new(expr)))
        };

        let span = ast::SourceSpan::combine(start_span, body.span());

        Ok(ast::LambdaExpr {
            parameters,
            body,
            span,
        })
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
                        Err(QangError::parse_error("Invalid assignment target", span))
                    }
                }
                _ => Err(QangError::parse_error("Invalid assignment target", span)),
            }
        } else {
            Ok(expr)
        }
    }

    fn argument_parameters(&mut self) -> ParseResult<Vec<ast::Identifier>> {
        self.consume(TokenType::LeftParen, "Expect '(' before parameters.")?;

        let mut parameters = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                self.consume(TokenType::Identifier, "Expect parameter name.")?;
                parameters.push(self.get_identifier()?);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;

        Ok(parameters)
    }
}

mod expression_parser {
    use super::*;
    use crate::{ast, tokenizer};

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
            .parse::<f64>()
            .map_err(|_| crate::QangError::parse_error("Expected number.", span))?;

        Ok(ast::Expr::Primary(ast::PrimaryExpr::Number(
            ast::NumberLiteral { value, span },
        )))
    }

    fn grouping_or_lambda(parser: &mut Parser) -> ParseResult<ast::Expr> {
        if parser.is_lambda_start() {
            Ok(ast::Expr::Primary(ast::PrimaryExpr::Lambda(Box::new(
                parser.lambda_expression()?,
            ))))
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
            _ => Err(crate::QangError::parse_error("Unknown operator.", span)),
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
            tokenizer::TokenType::Super => Ok(ast::Expr::Primary(ast::PrimaryExpr::Super(
                ast::SuperExpr { span },
            ))),
            _ => Err(crate::QangError::parse_error("Unknown literal.", span)),
        }
    }

    fn string(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let token = get_previous_token(parser);

        let value = token.lexeme(parser.source_map);

        let span = ast::SourceSpan::from_token(token);

        Ok(ast::Expr::Primary(ast::PrimaryExpr::String(
            ast::StringLiteral { value, span },
        )))
    }

    fn identifier(parser: &mut Parser) -> ParseResult<ast::Expr> {
        Ok(ast::Expr::Primary(ast::PrimaryExpr::Identifier(
            parser.get_identifier()?,
        )))
    }

    fn array(parser: &mut Parser) -> ParseResult<ast::Expr> {
        let start_span = parser.get_previous_span();
        let mut elements = Vec::new();

        // Handle empty array case
        if !parser.check(tokenizer::TokenType::RightSquareBracket) {
            loop {
                elements.push(parser.expression()?);

                if !parser.match_token(tokenizer::TokenType::Comma) {
                    break;
                }
            }
        }

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

    fn binary(parser: &mut Parser, left: ast::Expr) -> ParseResult<ast::Expr> {
        let token = get_previous_token(parser);
        let span_start = left.span().start;
        let token_type = token.token_type;

        let rule = get_rule(token_type);

        if rule.is_empty() {
            return Err(QangError::parse_error(
                "Unexpected token.",
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
            _ => Err(crate::QangError::parse_error("Unknown operator.", span)),
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
                let property = ast::Identifier::new(property_name, property_span);
                ast::CallOperation::Property(property)
            }
            tokenizer::TokenType::LeftSquareBracket => {
                let index = parse(parser, Precedence::None)?;
                parser.consume(
                    tokenizer::TokenType::RightSquareBracket,
                    "Expect ']' after array index.",
                )?;
                ast::CallOperation::Index(index)
            }
            tokenizer::TokenType::OptionalChaining => {
                parser.consume(
                    tokenizer::TokenType::Identifier,
                    "Expect property name after '.?'.",
                )?;

                let property_span = parser.get_previous_span();
                let property_name = parser
                    .previous_token
                    .as_ref()
                    .map(|t| t.lexeme(parser.source_map))
                    .unwrap();
                let property = ast::Identifier::new(property_name, property_span);

                ast::CallOperation::OptionalProperty(property)
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

        while parser.match_token(tokenizer::TokenType::Comma) {
            arguments.push(parser.expression()?);
        }

        Ok(arguments)
    }

    fn get_previous_token<'a>(parser: &'a Parser) -> &'a Token {
        // This should never panic because the expression parser will always have a previous token available to it.
        let token: &&Token = &parser
            .previous_token
            .as_ref()
            .expect("Expected token but found none.");

        token
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
            tokenizer::TokenType::OptionalChaining => ParseRule {
                prefix: None,
                infix: Some(call),
                precedence: Precedence::Call,
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

                return Err(crate::QangError::parse_error("Expected expression.", span));
            }
        };

        while let Some(current_token) = &parser.current_token {
            let rule = get_rule(current_token.token_type);
            println!(
                "Current token: {:?}, precedence: {:?}, rule precedence: {:?}",
                current_token.token_type, precedence, rule.precedence
            );
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
                    // This suggests a syntax error like missing operator
                    let span = ast::SourceSpan::from_token(current_token);
                    return Err(crate::QangError::parse_error(
                        "Unexpected token in expression. Missing operator?",
                        span,
                    ));
                }
                _ => {}
            }
        }

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SourceMap, ast::*};

    fn parse_source(source_map: &SourceMap) -> (Program, ErrorReporter) {
        let mut parser = Parser::new(&source_map);
        let program = parser.parse();
        let errors = parser.into_reporter();
        (program, errors)
    }

    fn assert_no_parse_errors(errors: &ErrorReporter) {
        if errors.has_errors() {
            panic!("Unexpected parse errors:\n{}", errors.format_errors());
        }
    }

    fn assert_parse_error(errors: &ErrorReporter, expected_message: &str) {
        assert!(
            errors.has_errors(),
            "Expected parse error but none occurred"
        );
        let error_text = errors.format_errors();
        assert!(
            error_text.contains(expected_message),
            "Expected error message '{}' but got: {}",
            expected_message,
            error_text
        );
    }

    #[test]
    fn test_empty_program() {
        let source_code = r#""#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 0);
    }

    #[test]
    fn test_simple_variable_declaration() {
        let source_code = r#"var x = 42;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 1);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert_eq!(var_decl.name.name.as_ref(), "x");
            assert!(var_decl.initializer.is_some());

            if let Some(Expr::Primary(PrimaryExpr::Number(num))) = &var_decl.initializer {
                assert_eq!(num.value, 42.0);
            } else {
                panic!("Expected number literal");
            }
        } else {
            panic!("Expected variable declaration");
        }
    }

    #[test]
    fn test_variable_declaration_without_initializer() {
        let source_code = r#"var x;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 1);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert_eq!(var_decl.name.name.as_ref(), "x");
            assert!(var_decl.initializer.is_none());
        } else {
            panic!("Expected variable declaration");
        }
    }

    #[test]
    fn test_function_declaration() {
        let source_code = r#"
            fn add(a, b) {
                return a + b;
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 1);

        if let Decl::Function(func_decl) = &program.decls[0] {
            assert_eq!(func_decl.function.name.name.as_ref(), "add");
            assert_eq!(func_decl.function.parameters.len(), 2);
            assert_eq!(func_decl.function.parameters[0].name.as_ref(), "a");
            assert_eq!(func_decl.function.parameters[1].name.as_ref(), "b");
            assert_eq!(func_decl.function.body.decls.len(), 1);

            // Verify the return statement in the function body
            if let Decl::Stmt(Stmt::Return(return_stmt)) = &func_decl.function.body.decls[0] {
                assert!(return_stmt.value.is_some());

                // Verify the return expression: a + b
                if let Some(Expr::Term(term_expr)) = &return_stmt.value {
                    // Left side should be identifier 'a'
                    if let Expr::Primary(PrimaryExpr::Identifier(left_id)) = &*term_expr.left {
                        assert_eq!(left_id.name.as_ref(), "a");
                    } else {
                        panic!("Expected identifier 'a' on left side of addition");
                    }

                    // Operator should be Add
                    assert_eq!(term_expr.operator, TermOperator::Add);

                    // Right side should be identifier 'b'
                    if let Expr::Primary(PrimaryExpr::Identifier(right_id)) = &*term_expr.right {
                        assert_eq!(right_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b' on right side of addition");
                    }
                } else {
                    panic!("Expected term expression (a + b) in return statement");
                }
            } else {
                panic!("Expected return statement in function body");
            }
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_function_without_parameters() {
        let source_code = r#"
            fn main() {
                var x = 5;
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Function(func_decl) = &program.decls[0] {
            assert_eq!(func_decl.function.name.name.as_ref(), "main");
            assert_eq!(func_decl.function.parameters.len(), 0);
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_class_declaration() {
        let source_code = r#"
            class Person {
                name;
                age = 0;
                
                get_name() {
                    return this.name;
                }
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 1);

        if let Decl::Class(class_decl) = &program.decls[0] {
            assert_eq!(class_decl.name.name.as_ref(), "Person");
            assert!(class_decl.superclass.is_none());
            assert_eq!(class_decl.members.len(), 3);

            // Check field without initializer
            if let ClassMember::Field(field) = &class_decl.members[0] {
                assert_eq!(field.name.name.as_ref(), "name");
                assert!(field.initializer.is_none());
            } else {
                panic!("Expected field declaration");
            }

            // Check field with initializer
            if let ClassMember::Field(field) = &class_decl.members[1] {
                assert_eq!(field.name.name.as_ref(), "age");
                assert!(field.initializer.is_some());
            } else {
                panic!("Expected field declaration");
            }

            // Check method
            if let ClassMember::Method(method) = &class_decl.members[2] {
                assert_eq!(method.name.name.as_ref(), "get_name");
                assert_eq!(method.parameters.len(), 0);

                // Verify method body contains return statement
                assert_eq!(method.body.decls.len(), 1);
                if let Decl::Stmt(Stmt::Return(return_stmt)) = &method.body.decls[0] {
                    assert!(return_stmt.value.is_some());

                    // Verify the return expression: this.name
                    if let Some(Expr::Call(call_expr)) = &return_stmt.value {
                        // Verify the callee is 'this'
                        if let Expr::Primary(PrimaryExpr::This(_)) = &*call_expr.callee {
                            // Verify the operation is property access to 'name'
                            if let CallOperation::Property(property_id) =
                                call_expr.operation.as_ref()
                            {
                                assert_eq!(property_id.name.as_ref(), "name");
                            } else {
                                panic!("Expected property access to 'name'");
                            }
                        } else {
                            panic!("Expected 'this' as callee");
                        }
                    } else {
                        panic!("Expected call expression (this.name) in return statement");
                    }
                } else {
                    panic!("Expected return statement in method body");
                }
            } else {
                panic!("Expected method declaration");
            }
        } else {
            panic!("Expected class declaration");
        }
    }

    #[test]
    fn test_class_with_inheritance() {
        let source_code = r#"
            class Student : Person {
                grade = "A";
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Class(class_decl) = &program.decls[0] {
            assert_eq!(class_decl.name.name.as_ref(), "Student");
            assert!(class_decl.superclass.is_some());
            assert_eq!(
                class_decl.superclass.as_ref().unwrap().name.as_ref(),
                "Person"
            );
        } else {
            panic!("Expected class declaration");
        }
    }

    #[test]
    fn test_lambda_declaration() {
        let source_code = r#"var add = (a, b) -> a + b;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 1);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert_eq!(var_decl.name.name.as_ref(), "add");

            if let Some(Expr::Primary(PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
                assert_eq!(lambda.parameters.len(), 2);
                assert_eq!(lambda.parameters[0].name.as_ref(), "a");
                assert_eq!(lambda.parameters[1].name.as_ref(), "b");

                if let LambdaBody::Expr(_) = lambda.body.as_ref() {
                    // TODO write tests for expected expression body
                } else {
                    panic!("Expected expression body in lambda");
                }
            } else {
                panic!("Expected lambda expression");
            }
        } else {
            panic!("Expected variable declaration");
        }
    }

    #[test]
    fn test_lambda_with_block_body() {
        let source_code = r#"var calc = (x) -> { return x * 2; };"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            if let Some(Expr::Primary(PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
                if let LambdaBody::Block(_) = lambda.body.as_ref() {
                    // TODO write tests for expected expression body
                } else {
                    panic!("Expected block body in lambda");
                }
            } else {
                panic!("Expected lambda expression");
            }
        }
    }

    #[test]
    fn test_if_statement() {
        let source_code = r#"
            if (x > 0) {
                return true;
            } else {
                return false;
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 1);

        if let Decl::Stmt(Stmt::If(if_stmt)) = &program.decls[0] {
            // TODO write test for statements in if and else branches
            assert!(if_stmt.else_branch.is_some());
        } else {
            panic!("Expected if statement");
        }
    }

    #[test]
    fn test_if_statement_without_else() {
        let source_code = r#"
            if (condition) {
                doSomething();
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::If(if_stmt)) = &program.decls[0] {
            // TODO write test for statements in if branch
            assert!(if_stmt.else_branch.is_none());
        } else {
            panic!("Expected if statement");
        }
    }

    #[test]
    fn test_while_statement() {
        let source_code = r#"
            while (i < 10) {
                i = i + 1;
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 1);

        if let Decl::Stmt(Stmt::While(_)) = &program.decls[0] {
            // TODO write tests for expected statements.
        } else {
            panic!("Expected while statement");
        }
    }

    #[test]
    fn test_for_statement_with_all_clauses() {
        let source_code = r#"
            for (var i = 0; i < 10; i = i + 1) {
                print(i);
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::For(for_stmt)) = &program.decls[0] {
            // TODO Write tests that has expected values for all nodes.
            assert!(for_stmt.initializer.is_some());
            assert!(for_stmt.condition.is_some());
            assert!(for_stmt.increment.is_some());

            if let Some(ForInitializer::Variable(var_decl)) = &for_stmt.initializer {
                assert_eq!(var_decl.name.name.as_ref(), "i");
            } else {
                panic!("Expected variable initializer");
            }
        } else {
            panic!("Expected for statement");
        }
    }

    #[test]
    fn test_for_statement_with_expression_initializer() {
        let source_code = r#"
            for (i = 0; i < 10; i = i + 1) {
                print(i);
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::For(for_stmt)) = &program.decls[0] {
            if let Some(ForInitializer::Expr(_)) = &for_stmt.initializer {
                // TODO write tests for expected expression initializer
            } else {
                panic!("Expected expression initializer");
            }
        } else {
            panic!("Expected for statement");
        }
    }

    #[test]
    fn test_for_statement_minimal() {
        let source_code = r#"
            for (;;) {
                break;
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::For(for_stmt)) = &program.decls[0] {
            assert!(for_stmt.initializer.is_none());
            assert!(for_stmt.condition.is_none());
            assert!(for_stmt.increment.is_none());
        } else {
            panic!("Expected for statement");
        }
    }

    #[test]
    fn test_break_and_continue_statements() {
        let source_code = r#"
            while (true) {
                if (condition1) {
                    break;
                }
                if (condition2) {
                    continue;
                }
                doWork();
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        // TODO write tests for the rest of the expected nodes.
    }

    #[test]
    fn test_return_statement() {
        let source_code = r#"
            fn test() {
                return 42;
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Function(func_decl) = &program.decls[0] {
            if let Decl::Stmt(Stmt::Return(ret_stmt)) = &func_decl.function.body.decls[0] {
                // TODO write more specific test for return statement that checks all the AST nodes.
                assert!(ret_stmt.value.is_some());
            } else {
                panic!("Expected return statement");
            }
        }
    }

    #[test]
    fn test_return_statement_without_value() {
        let source_code = r#"
            fn test() {
                return;
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Function(func_decl) = &program.decls[0] {
            if let Decl::Stmt(Stmt::Return(ret_stmt)) = &func_decl.function.body.decls[0] {
                assert!(ret_stmt.value.is_none());
            } else {
                panic!("Expected return statement");
            }
        }
    }

    #[test]
    fn test_try_catch_finally() {
        let source_code = r#"
            try {
                riskyOperation();
            } catch (error) {
                handleError(error);
            } finally {
                cleanup();
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::Try(try_stmt)) = &program.decls[0] {
            // TODO write tests that assert all nodes in the AST
            assert!(try_stmt.catch_clause.is_some());
            assert!(try_stmt.finally_block.is_some());

            let catch_clause = try_stmt.catch_clause.as_ref().unwrap();
            assert!(catch_clause.parameter.is_some());
            assert_eq!(
                catch_clause.parameter.as_ref().unwrap().name.as_ref(),
                "error"
            );
        } else {
            panic!("Expected try statement");
        }
    }

    #[test]
    fn test_try_catch_without_parameter() {
        let source_code = r#"
            try {
                riskyOperation();
            } catch {
                handleError();
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::Try(try_stmt)) = &program.decls[0] {
            // TODO write tests for all expected nodes.
            let catch_clause = try_stmt.catch_clause.as_ref().unwrap();
            assert!(catch_clause.parameter.is_none());
        }
    }

    #[test]
    fn test_try_finally_without_catch() {
        let source_code = r#"
            try {
                riskyOperation();
            } finally {
                cleanup();
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::Try(try_stmt)) = &program.decls[0] {
            assert!(try_stmt.catch_clause.is_none());
            // TODO write tests for all statements in try and finally blocks.
            assert!(try_stmt.finally_block.is_some());
        }
    }

    #[test]
    fn test_throw_statement() {
        let source_code = r#"
            throw Error("Something went wrong");
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::Throw(throw_stmt)) = &program.decls[0] {
            // TODO test the nodes for the thrown expression.
            assert!(throw_stmt.value.is_some());
        } else {
            panic!("Expected throw statement");
        }
    }

    #[test]
    fn test_throw_statement_without_value() {
        let source_code = r#"throw;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::Throw(throw_stmt)) = &program.decls[0] {
            assert!(throw_stmt.value.is_none());
        }
    }

    #[test]
    fn test_arithmetic_expressions() {
        let source_code = r#"var result = a + b * c - d / e % f;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert!(var_decl.initializer.is_some());
            // TODO write tests to assert all nodes in the AST are assembled correctly.
        }
    }

    #[test]
    fn test_comparison_expressions() {
        let source_code = r#"var check = x > y and a <= b or c != d and e == f;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            // TODO write tests to assert all nodes in the AST are assembled correctly.
            assert!(var_decl.initializer.is_some());
        }
    }

    #[test]
    fn test_unary_expressions() {
        let source_code = r#"var result = !condition and -number;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            // TODO write tests to assert all nodes in the AST are assembled correctly.
            assert!(var_decl.initializer.is_some());
        }
    }

    #[test]
    fn test_assignment_expressions() {
        let source_code = r#"
            x = 5;
            obj.property = "value";
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 2);

        // Both should be expression statements containing assignments
        if let Decl::Stmt(Stmt::Expr(expr_stmt)) = &program.decls[0] {
            if let Expr::Assignment(_) = expr_stmt.expr {
                // TODO tests for expected assignment
            } else {
                panic!("Expected assignment expression");
            }
        }

        if let Decl::Stmt(Stmt::Expr(expr_stmt)) = &program.decls[1] {
            if let Expr::Assignment(_) = expr_stmt.expr {
                // TODO test for expected assignment
            } else {
                panic!("Expected assignment expression");
            }
        }
    }

    #[test]
    fn test_ternary_expressions() {
        let source_code = r#"var result = condition ? trueValue : falseValue;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            if let Some(Expr::Ternary(ternary)) = &var_decl.initializer {
                // TODO test that the nodes in the AST are correct.
                assert!(ternary.then_expr.is_some());
                assert!(ternary.else_expr.is_some());
            } else {
                panic!("Expected ternary expression");
            }
        }
    }

    #[test]
    fn test_pipe_expressions() {
        let source_code = r#"var result = value |> transform |> finalize;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            if let Some(Expr::Pipe(_)) = &var_decl.initializer {
                // TODO test for expected pipe expression
            } else {
                panic!("Expected pipe expression");
            }
        }
    }

    #[test]
    fn test_function_calls() {
        let source_code = r#"
            result = func();
            result2 = func(a, b, c);
            result3 = obj.method(arg);
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 3);

        // TODO write tests to ensure all declaration nodes are assembled correctly.
    }

    #[test]
    fn test_property_access() {
        let source_code = r#"
            value = obj.property;
            value2 = obj.nested.deep.property;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 2);
        // TODO write tests to ensure all declaration nodes are assembled correctly.
    }

    #[test]
    fn test_optional_chaining() {
        let source_code = r#"
            value = obj.?property;
            value2 = obj.?method().?result;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 2);
        // TODO write tests to ensure all declaration nodes are assembled correctly.
    }

    #[test]
    fn test_array_literals() {
        let source_code = r#"
            empty = [];
            numbers = [1, 2, 3, 4];
            mixed = [1, "string", true, nil];
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 3);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            if let Some(Expr::Primary(PrimaryExpr::Array(array))) = &var_decl.initializer {
                assert_eq!(array.elements.len(), 0);
            } else {
                panic!("Expected array literal");
            }
        }

        if let Decl::Variable(var_decl) = &program.decls[1] {
            if let Some(Expr::Primary(PrimaryExpr::Array(array))) = &var_decl.initializer {
                // TODO write tests to ensure all items in the array are in the AST correctly.
                assert_eq!(array.elements.len(), 4);
            } else {
                panic!("Expected array literal");
            }
        }
    }

    #[test]
    fn test_array_access() {
        let source_code = r#"
            value = array[0];
            value2 = matrix[row][col];
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 2);
        // TODO write tests to ensure all declaration nodes are assembled correctly.
    }

    #[test]
    fn test_grouping_expressions() {
        let source_code = r#"var result = (a + b) * (c - d);"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert!(var_decl.initializer.is_some());
            // TODO write tests to ensure all declaration nodes are assembled correctly.
        }
    }

    #[test]
    fn test_literals() {
        let source_code = r#"
            var num = 42.5;
            var str = "hello world";
            var bool1 = true;
            var bool2 = false;
            var nothing = nil;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 5);

        // Check number literal
        if let Decl::Variable(var_decl) = &program.decls[0] {
            if let Some(Expr::Primary(PrimaryExpr::Number(num))) = &var_decl.initializer {
                assert_eq!(num.value, 42.5);
            } else {
                panic!("Expected number literal");
            }
        }

        // Check string literal
        if let Decl::Variable(var_decl) = &program.decls[1] {
            if let Some(Expr::Primary(PrimaryExpr::String(str))) = &var_decl.initializer {
                assert_eq!(str.value.as_ref(), "\"hello world\"");
            } else {
                panic!("Expected string literal");
            }
        }

        // Check boolean literals
        if let Decl::Variable(var_decl) = &program.decls[2] {
            if let Some(Expr::Primary(PrimaryExpr::Boolean(bool))) = &var_decl.initializer {
                assert_eq!(bool.value, true);
            } else {
                panic!("Expected boolean literal");
            }
        }

        if let Decl::Variable(var_decl) = &program.decls[3] {
            if let Some(Expr::Primary(PrimaryExpr::Boolean(bool))) = &var_decl.initializer {
                assert_eq!(bool.value, false);
            } else {
                panic!("Expected boolean literal");
            }
        }

        // Check nil literal
        if let Decl::Variable(var_decl) = &program.decls[4] {
            if let Some(Expr::Primary(PrimaryExpr::Nil(_))) = &var_decl.initializer {
                // Expected nil
            } else {
                panic!("Expected nil literal");
            }
        }
    }

    #[test]
    fn test_this_and_super() {
        let source_code = r#"
            class Child : Parent {
                method() {
                    this.value = super.getValue();
                }
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Class(class_decl) = &program.decls[0] {
            if let ClassMember::Method(method) = &class_decl.members[0] {
                // TODO write tests to ensure all nodes are assembled correctly.
                assert_eq!(method.body.decls.len(), 1);
            }
        }
    }

    #[test]
    fn test_complex_nested_expression() {
        let source_code = r#"
            var result = obj.method(a + b * c).property[index].?optional |> transform;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 1);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert!(var_decl.initializer.is_some());
            // TODO write tests to ensure all nodes are assembled correctly.
        }
    }

    #[test]
    fn test_nested_function_calls() {
        let source_code = r#"
            var result = outer(inner(deep(value)));
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert!(var_decl.initializer.is_some());
            // TODO write tests to ensure all nodes are assembled correctly.
        }
    }

    #[test]
    fn test_operator_precedence() {
        let source_code = r#"
            var result = a + b * c == d - e / f;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            // Should be parsed as: (a + (b * c)) == (d - (e / f))
            if let Some(Expr::Equality(_)) = &var_decl.initializer {
                // TODO write tests to ensure all nodes are assembled correctly.
            } else {
                panic!("Expected equality expression at top level");
            }
        }
    }

    #[test]
    fn test_right_associative_ternary() {
        let source_code = r#"
            var result = a ? b ? c : d : e;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            // Should be parsed as: a ? (b ? c : d) : e
            if let Some(Expr::Ternary(_)) = &var_decl.initializer {
                // TODO write tests to ensure all nodes are assembled correctly.
            } else {
                panic!("Expected ternary expression");
            }
        }
    }

    #[test]
    fn test_block_statements() {
        let source_code = r#"
            {
                var x = 1;
                var y = 2;
                x + y;
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Stmt(Stmt::Block(block)) = &program.decls[0] {
            assert_eq!(block.decls.len(), 3);
            // TODO write tests to ensure all nodes are assembled correctly.
        } else {
            panic!("Expected block statement");
        }
    }

    #[test]
    fn test_expression_statements() {
        let source_code = r#"
            someFunction();
            obj.method();
            x + y;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 3);

        for decl in &program.decls {
            if let Decl::Stmt(Stmt::Expr(_)) = decl {
                // TODO write tests for expected expression statement
            } else {
                panic!("Expected expression statement");
            }
        }
    }

    // Error handling tests
    #[test]
    fn test_missing_semicolon_error() {
        let source_code = r#"var x = 5"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Expect ';'");
    }

    #[test]
    fn test_unterminated_string_error() {
        let source_code = r#"var msg = "unterminated string"#;
        let source_map = SourceMap::new(source_code.to_string());
        println!(
            "tokens: {:?}",
            Tokenizer::new(&source_map).collect::<Vec<_>>()
        );
        let (_program, errors) = parse_source(&source_map);

        assert!(errors.has_errors());
    }

    #[test]
    fn test_missing_closing_brace_error() {
        let source_code = r#"
            fn test() {
                var x = 5;
                // Missing closing brace
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Expected '}'");
    }

    #[test]
    fn test_invalid_assignment_target_error() {
        let source_code = r#"5 = x;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Invalid assignment target");
    }

    #[test]
    fn test_missing_function_name_error() {
        let source_code = r#"fn () { return 42; }"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Expect function name");
    }

    #[test]
    fn test_missing_variable_name_error() {
        let source_code = r#"var = 5;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Expect variable name");
    }

    #[test]
    fn test_missing_class_name_error() {
        let source_code = r#"class { }"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Expect class name");
    }

    #[test]
    fn test_invalid_try_without_catch_or_finally() {
        let source_code = r#"
            try {
                riskyOperation();
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Expected 'catch' or 'finally'");
    }

    #[test]
    fn test_missing_parentheses_in_if() {
        let source_code = r#"
            if condition {
                doSomething();
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Expected '('");
    }

    #[test]
    fn test_missing_parentheses_in_while() {
        let source_code = r#"
            while condition {
                doSomething();
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Expected '('");
    }

    #[test]
    fn test_missing_parentheses_in_for() {
        let source_code = r#"
            for var i = 0; i < 10; i = i + 1 {
                print(i);
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Expect '(' after 'for'");
    }

    #[test]
    fn test_unexpected_token_in_expression() {
        let source_code = r#"var x = 5 @ 3;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert!(errors.has_errors());
    }

    #[test]
    fn test_missing_arrow_in_lambda() {
        let source_code = r#"var func = (x) x + 1;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (_program, errors) = parse_source(&source_map);

        assert_parse_error(&errors, "Unexpected token in expression. Missing operator?");
    }

    #[test]
    fn test_empty_lambda_parameters() {
        let source_code = r#"var func = () -> 42;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            if let Some(Expr::Primary(PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
                assert_eq!(lambda.parameters.len(), 0);
                // TODO write tests to ensure all nodes are assembled correctly.
            } else {
                panic!("Expected lambda expression");
            }
        }
    }

    #[test]
    fn test_lambda_with_single_parameter() {
        let source_code = r#"var func = (x) -> x * 2;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            if let Some(Expr::Primary(PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
                assert_eq!(lambda.parameters.len(), 1);
                assert_eq!(lambda.parameters[0].name.as_ref(), "x");
                // TODO write tests to ensure all nodes are assembled correctly.
            } else {
                panic!("Expected lambda expression");
            }
        }
    }

    #[test]
    fn test_lambda_with_multiple_parameters() {
        let source_code = r#"var func = (a, b, c) -> a + b + c;"#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            if let Some(Expr::Primary(PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
                assert_eq!(lambda.parameters.len(), 3);
                assert_eq!(lambda.parameters[0].name.as_ref(), "a");
                assert_eq!(lambda.parameters[1].name.as_ref(), "b");
                assert_eq!(lambda.parameters[2].name.as_ref(), "c");
                // TODO write tests to ensure all nodes are assembled correctly.
            } else {
                panic!("Expected lambda expression");
            }
        }
    }

    #[test]
    fn test_chained_method_calls() {
        let source_code = r#"
            var result = obj.method1().method2().method3();
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            // TODO write tests to ensure all nodes are assembled correctly.
            assert!(var_decl.initializer.is_some());
        }
    }

    #[test]
    fn test_mixed_property_and_method_access() {
        let source_code = r#"
            var result = obj.property.method().field[index];
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert!(var_decl.initializer.is_some());
            // TODO write tests to ensure all nodes are assembled correctly.
        }
    }

    #[test]
    fn test_complex_array_access() {
        let source_code = r#"
            var result = matrix[row + 1][col - 1];
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert!(var_decl.initializer.is_some());
            // TODO write tests to ensure all nodes are assembled correctly.
        }
    }

    #[test]
    fn test_assignment_chaining() {
        let source_code = r#"
            a = b = c = 5;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 1);

        if let Decl::Stmt(Stmt::Expr(expr_stmt)) = &program.decls[0] {
            if let Expr::Assignment(_) = expr_stmt.expr {
                // TODO test for expected assignment expression
            } else {
                panic!("Expected assignment expression");
            }
        }
    }

    #[test]
    fn test_nested_ternary_expressions() {
        let source_code = r#"
            var result = a ? b : c ? d : e ? f : g;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            if let Some(Expr::Ternary(_)) = &var_decl.initializer {
                // TODO write tests to ensure all nodes are assembled correctly.
            } else {
                panic!("Expected ternary expression");
            }
        }
    }

    #[test]
    fn test_complex_boolean_logic() {
        let source_code = r#"
            var condition = !a and (b or c) and !(d or e);
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert!(var_decl.initializer.is_some());
            // TODO write tests to ensure all nodes are assembled correctly.
        }
    }

    #[test]
    fn test_pipe_operator_precedence() {
        let source_code = r#"
            var result = value + 1 |> transform |> process - 2;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            // TODO write tests to ensure all nodes are assembled correctly.
            assert!(var_decl.initializer.is_some());
        }
    }

    #[test]
    fn test_deeply_nested_expressions() {
        let source_code = r#"
            var result = ((((a + b) * c) - d) / e) % f;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);

        if let Decl::Variable(var_decl) = &program.decls[0] {
            assert!(var_decl.initializer.is_some());
        }
    }

    #[test]
    fn test_error_recovery() {
        let source_code = r#"
            var x = 5;
            var y = ; // Error here
            var z = 10; // This should still parse
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert!(errors.has_errors());
        assert!(program.decls.len() >= 2);

        // TODO write tests to ensure that we have at least the first and third declarations
    }

    #[test]
    fn test_comments_ignored() {
        let source_code = r#"
            // This is a comment
            var x = 5; // Another comment
            /* Multi-line
               comment */
            var y = 10;
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 2);
        // TODO assert the two declarations are correct.
    }

    #[test]
    fn test_whitespace_handling() {
        let source_code = r#"
            

            var    x    =    5    ;


            var y=10;
            
            
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert_eq!(program.decls.len(), 2);
    }

    #[test]
    fn test_complete_program_example() {
        let source_code = r#"
            class Calculator {
                add(a, b) {
                    return a + b;
                }
                
                multiply(a, b) {
                    return a * b;
                }
            }
            
            var calc = Calculator();
            var result = calc.add(5, 3);
            
            if (result > 0) {
                print("Positive result: " + result);
            } else {
                print("Non-positive result");
            }
            
            for (var i = 0; i < 5; i = i + 1) {
                var doubled = calc.multiply(i, 2);
                print("Double of " + i + " is " + doubled);
            }
            
            try {
                var risky = someRiskyOperation();
                print("Success: " + risky);
            } catch (error) {
                print("Error occurred: " + error);
            } finally {
                print("Cleanup completed");
            }
        "#;
        let source_map = SourceMap::new(source_code.to_string());

        let (program, errors) = parse_source(&source_map);

        assert_no_parse_errors(&errors);
        assert!(program.decls.len() > 0);

        // TODO replace the below with test that actually verify the AST was created correctly.

        // Verify we have the expected top-level declarations
        let mut class_count = 0;
        let mut var_count = 0;
        let mut stmt_count = 0;

        for decl in &program.decls {
            match decl {
                Decl::Class(_) => class_count += 1,
                Decl::Variable(_) => var_count += 1,
                Decl::Stmt(_) => stmt_count += 1,
                _ => {}
            }
        }

        assert!(class_count > 0, "Should have at least one class");
        assert!(var_count > 0, "Should have at least one variable");
        assert!(stmt_count > 0, "Should have at least one statement");
    }
}
