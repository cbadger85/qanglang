use crate::{
    AssignmentExpr, AssignmentTarget, BlockStmt, BreakStmt, CallOperation, CatchClause, ClassDecl,
    ClassMember, ContinueStmt, Decl, Expr, ExprStmt, FieldDecl, ForInitializer, ForStmt,
    FunctionDecl, FunctionExpr, Identifier, IfStmt, LambdaBody, LambdaExpr, PrimaryExpr, Program,
    PropertyAccess, QangError, QangResult, ReturnStmt, SourceMap, SourceSpan, Stmt, ThrowStmt,
    TryStmt, VariableDecl, WhileStmt,
    error::ErrorReporter,
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
        if let Some(current_token_type) = self.current_token.as_ref().map(|t| &t.token_type) {
            if &token_type == current_token_type {
                self.advance();
                return Ok(());
            }
        }

        let span = self
            .current_token
            .as_ref()
            .map(SourceSpan::from_token)
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
            .report_parse_error(message, SourceSpan::from_token(token));
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

    fn get_identifier(&mut self) -> ParseResult<Identifier> {
        let token = self.previous_token.as_ref();
        let span = token
            .map(SourceSpan::from_token)
            .unwrap_or(SourceSpan { start: 0, end: 0 });

        if let Some(token) = token {
            let name = token.lexeme(self.source_map);
            Ok(Identifier::new(name, span))
        } else {
            Err(QangError::parse_error("Expected identifier.", span))
        }
    }

    fn synchronize(&mut self) {
        loop {
            if self
                .current_token
                .as_ref()
                .map(|t| &t.token_type)
                .map(|t| t == &TokenType::Eof)
                .unwrap_or(false)
            {
                break;
            }

            if (self
                .previous_token
                .as_ref()
                .map(|t| t.token_type == TokenType::Semicolon))
            .unwrap_or(false)
            {
                return;
            }

            if let Some(
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
                | TokenType::Continue,
            ) = self.current_token.as_ref().map(|t| &t.token_type)
            {
                return;
            }

            self.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        self.check(TokenType::Eof)
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

    pub fn parse(&mut self) -> QangResult<Program> {
        let start_span = self.get_current_span();
        let mut decls = Vec::new();

        while !self.is_at_end() {
            if let Some(decl) = self.declaration() {
                decls.push(decl);
            }
        }

        let end_span = self.get_current_span();

        let program = Program::new(decls, SourceSpan::combine(start_span, end_span));

        if self.errors.has_errors() {
            let errors = self.errors.take_errors();
            Err(errors)
        } else {
            Ok(program)
        }
    }

    fn declaration(&mut self) -> Option<Decl> {
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

    fn variable_declaration(&mut self) -> ParseResult<Decl> {
        let var_span = self.get_previous_span();
        self.consume(TokenType::Identifier, "Expect variable name.")?;

        let identifier = self.get_identifier()?;

        let initializer = if self.match_token(TokenType::Equals) {
            if self.is_lambda_start() {
                let lambda_expr = self.lambda_expression()?;
                Some(Expr::Primary(PrimaryExpr::Lambda(Box::new(lambda_expr))))
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
        let span = SourceSpan::combine(var_span, semicolon_span);

        Ok(Decl::Variable(VariableDecl {
            name: identifier,
            initializer,
            span,
        }))
    }

    fn class_declaration(&mut self) -> ParseResult<Decl> {
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
            if let Some(member) = self.class_member()? {
                members.push(member);
            }
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.")?;
        let end_span = self.get_previous_span();
        let span = SourceSpan::combine(start_span, end_span);

        Ok(Decl::Class(ClassDecl {
            name,
            superclass,
            members,
            span,
        }))
    }

    fn class_member(&mut self) -> ParseResult<Option<ClassMember>> {
        if self.check(TokenType::Identifier) {
            let checkpoint = self.current_token.clone();

            // Try to parse as method (identifier followed by '(')
            self.advance(); // consume identifier
            if self.check(TokenType::LeftParen) {
                // Reset to checkpoint and parse as method
                self.current_token = checkpoint;
                let function_expr = self.function_expression()?;
                return Ok(Some(ClassMember::Method(function_expr)));
            } else {
                // Reset to checkpoint and parse as field
                self.current_token = checkpoint;
                let field = self.field_declaration()?;
                return Ok(Some(ClassMember::Field(field)));
            }
        }

        Err(QangError::parse_error(
            "Expected field or method declaration.",
            self.get_current_span(),
        ))
    }

    fn field_declaration(&mut self) -> ParseResult<FieldDecl> {
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

        Ok(FieldDecl {
            name,
            initializer,
            span,
        })
    }

    fn function_declaration(&mut self) -> ParseResult<Decl> {
        let function_expr = self.function_expression()?;
        let span = SourceSpan::combine(self.get_previous_span(), function_expr.span);

        Ok(Decl::Function(FunctionDecl {
            function: function_expr,
            span,
        }))
    }

    fn statement(&mut self) -> ParseResult<Decl> {
        let current_token_type = self
            .current_token
            .as_ref()
            .expect("Expected statement.")
            .token_type;

        match current_token_type {
            TokenType::While => Ok(Decl::Stmt(Stmt::While(self.while_statement()?))),
            TokenType::If => Ok(Decl::Stmt(Stmt::If(self.if_statement()?))),
            TokenType::LeftBrace => Ok(Decl::Stmt(Stmt::Block(self.block_statement()?))),
            TokenType::For => Ok(Decl::Stmt(Stmt::For(self.for_statement()?))),
            TokenType::Break => Ok(Decl::Stmt(Stmt::Break(self.break_statement()?))),
            TokenType::Continue => Ok(Decl::Stmt(Stmt::Continue(self.continue_statement()?))),
            TokenType::Return => Ok(Decl::Stmt(Stmt::Return(self.return_statement()?))),
            TokenType::Throw => Ok(Decl::Stmt(Stmt::Throw(self.throw_statement()?))),
            TokenType::Try => Ok(Decl::Stmt(Stmt::Try(self.try_statement()?))),
            _ => Ok(Decl::Stmt(Stmt::Expr(self.expression_statement()?))),
        }
    }

    fn block_statement(&mut self) -> ParseResult<BlockStmt> {
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

        Ok(BlockStmt {
            decls,
            span: SourceSpan::combine(start_span, end_span),
        })
    }

    fn if_statement(&mut self) -> ParseResult<IfStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::LeftParen, "Expected '('.")?;
        let condition = self.expression()?;

        let then_branch = Box::new(Stmt::Block(self.block_statement()?));

        let (else_branch, span) = if self.match_token(TokenType::Else) {
            let block_stmt = Stmt::Block(self.block_statement()?);
            let span = SourceSpan::combine(start_span, block_stmt.span());
            (Some(Box::new(block_stmt)), span)
        } else {
            (None, SourceSpan::combine(start_span, then_branch.span()))
        };

        Ok(IfStmt {
            condition,
            then_branch,
            else_branch,
            span,
        })
    }

    fn while_statement(&mut self) -> ParseResult<WhileStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::LeftParen, "Expected '('.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')'.")?;

        let block_stmt = self.block_statement()?;
        let span = SourceSpan::combine(start_span, block_stmt.span);
        let body = Box::new(Stmt::Block(block_stmt));

        Ok(WhileStmt {
            condition,
            body,
            span,
        })
    }

    fn for_statement(&mut self) -> ParseResult<ForStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        // Parse initializer
        let initializer = if self.match_token(TokenType::Semicolon) {
            None
        } else if self.match_token(TokenType::Var) {
            let var_decl = self.variable_declaration()?;
            if let Decl::Variable(var_decl) = var_decl {
                Some(ForInitializer::Variable(var_decl))
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
            Some(ForInitializer::Expr(expr))
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

        let body = Box::new(Stmt::Block(self.block_statement()?));
        let span = SourceSpan::combine(start_span, body.span());

        Ok(ForStmt {
            initializer,
            condition,
            increment,
            body,
            span,
        })
    }

    fn break_statement(&mut self) -> ParseResult<BreakStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::Semicolon, "Expected ';'.")?;
        let span = SourceSpan::combine(start_span, self.get_previous_span());

        Ok(BreakStmt { span })
    }

    fn continue_statement(&mut self) -> ParseResult<ContinueStmt> {
        let start_span = self.get_current_span();
        self.advance();
        self.consume(TokenType::Semicolon, "Expected ';'.")?;
        let span = SourceSpan::combine(start_span, self.get_previous_span());

        Ok(ContinueStmt { span })
    }

    fn return_statement(&mut self) -> ParseResult<ReturnStmt> {
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

        Ok(ReturnStmt { value, span })
    }

    fn throw_statement(&mut self) -> ParseResult<ThrowStmt> {
        let start_span = self.get_current_span();
        self.advance();

        let value = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenType::Semicolon, "Expect ';' after throw value.")?;
        let end_span = self.get_previous_span();
        let span = SourceSpan::combine(start_span, end_span);

        Ok(ThrowStmt { value, span })
    }

    fn try_statement(&mut self) -> ParseResult<TryStmt> {
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
            let catch_span = SourceSpan::combine(catch_start, body.span);

            Some(CatchClause {
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

        let span = SourceSpan::combine(start_span, end_span);

        Ok(TryStmt {
            try_block,
            catch_clause,
            finally_block,
            span,
        })
    }

    fn expression_statement(&mut self) -> ParseResult<ExprStmt> {
        let expr = self.expression()?;

        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;

        let semicolon_span = self.get_previous_span();

        let expr_span = expr.span();

        Ok(ExprStmt {
            expr,
            span: SourceSpan::combine(expr_span, semicolon_span),
        })
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    fn lambda_expression(&mut self) -> ParseResult<LambdaExpr> {
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
            Box::new(LambdaBody::Block(self.block_statement()?))
        } else {
            let expr = self.expression()?;
            Box::new(LambdaBody::Expr(Box::new(expr)))
        };

        let span = SourceSpan::combine(start_span, body.span());

        Ok(LambdaExpr {
            parameters,
            body,
            span,
        })
    }

    fn function_expression(&mut self) -> ParseResult<FunctionExpr> {
        let start_span = self.get_current_span();
        self.consume(TokenType::Identifier, "Expect function name.")?;
        let name = self.get_identifier()?;

        let parameters = self.argument_parameters()?;
        let body = self.block_statement()?;
        let span = SourceSpan::combine(start_span, body.span);

        Ok(FunctionExpr {
            name,
            parameters,
            body,
            span,
        })
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        let expr = expression_parser::parse(self, expression_parser::Precedence::Ternary)?;

        if self.match_token(TokenType::Equals) {
            let value = Box::new(self.expression()?);
            let span = SourceSpan::combine(expr.span(), value.span());

            match expr {
                Expr::Primary(PrimaryExpr::Identifier(id)) => {
                    Ok(Expr::Assignment(AssignmentExpr {
                        target: AssignmentTarget::Identifier(id),
                        value,
                        span,
                    }))
                }
                Expr::Call(call_expr) => {
                    if let CallOperation::Property(property) = call_expr.operation.as_ref() {
                        let property_access = PropertyAccess {
                            object: call_expr.callee,
                            property: property.clone(),
                            span: call_expr.span,
                        };
                        Ok(Expr::Assignment(AssignmentExpr {
                            target: AssignmentTarget::Property(property_access),
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

    fn argument_parameters(&mut self) -> ParseResult<Vec<Identifier>> {
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
    use crate::tokenizer;

    #[derive(Debug, PartialEq, PartialOrd)]
    #[repr(u8)]
    pub enum Precedence {
        None = 0,
        Assignment, // =
        Pipe,       // |>
        Ternary,    // ? :
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

    type PrefixParseFn = fn(&mut Parser) -> ParseResult<crate::Expr>;
    type InfixParseFn = fn(&mut Parser, crate::Expr) -> ParseResult<crate::Expr>;

    fn number(parser: &mut Parser) -> ParseResult<crate::Expr> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let span = crate::SourceSpan::from_token(token);

        let value = token
            .lexeme(parser.source_map)
            .parse::<f64>()
            .map_err(|_| crate::QangError::parse_error("Expected number.", span))?;

        Ok(crate::Expr::Primary(crate::PrimaryExpr::Number(
            crate::NumberLiteral { value, span },
        )))
    }

    fn grouping_or_lambda(parser: &mut Parser) -> ParseResult<crate::Expr> {
        if parser.is_lambda_start() {
            Ok(Expr::Primary(crate::PrimaryExpr::Lambda(Box::new(
                parser.lambda_expression()?,
            ))))
        } else {
            grouping(parser)
        }
    }

    fn grouping(parser: &mut Parser) -> ParseResult<crate::Expr> {
        let expr = parser.expression()?;
        parser.consume(
            tokenizer::TokenType::RightParen,
            "Expect ')' after expression.",
        )?;

        Ok(expr)
    }

    fn unary(parser: &mut Parser) -> ParseResult<crate::Expr> {
        let operand = Box::new(parser.expression()?);
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let operator_type = token.token_type;
        let operator_span = crate::SourceSpan::from_token(token);

        let span = crate::SourceSpan::combine(operator_span, operand.span());
        match operator_type {
            tokenizer::TokenType::Minus => Ok(crate::Expr::Unary(crate::UnaryExpr {
                operator: crate::UnaryOperator::Minus,
                operand,
                span,
            })),
            tokenizer::TokenType::Bang => Ok(crate::Expr::Unary(crate::UnaryExpr {
                operator: crate::UnaryOperator::Not,
                operand,
                span,
            })),
            _ => Err(crate::QangError::parse_error("Unknown operator.", span)),
        }
    }

    fn literal(parser: &mut Parser) -> ParseResult<crate::Expr> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let span = crate::SourceSpan::from_token(token);

        match token.token_type {
            tokenizer::TokenType::False => Ok(crate::Expr::Primary(crate::PrimaryExpr::Boolean(
                crate::BooleanLiteral { value: false, span },
            ))),
            tokenizer::TokenType::True => Ok(crate::Expr::Primary(crate::PrimaryExpr::Boolean(
                crate::BooleanLiteral { value: true, span },
            ))),
            tokenizer::TokenType::Nil => Ok(crate::Expr::Primary(crate::PrimaryExpr::Nil(
                crate::NilLiteral { span },
            ))),
            tokenizer::TokenType::This => Ok(crate::Expr::Primary(crate::PrimaryExpr::This(
                crate::ThisExpr { span },
            ))),
            tokenizer::TokenType::Super => Ok(crate::Expr::Primary(crate::PrimaryExpr::Super(
                crate::SuperExpr { span },
            ))),
            _ => Err(crate::QangError::parse_error("Unknown literal.", span)),
        }
    }

    fn string(parser: &mut Parser) -> ParseResult<crate::Expr> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let value = token.lexeme(parser.source_map);

        let span = crate::SourceSpan::from_token(token);

        Ok(crate::Expr::Primary(crate::PrimaryExpr::String(
            crate::StringLiteral { value, span },
        )))
    }

    fn identifier(parser: &mut Parser) -> ParseResult<crate::Expr> {
        Ok(crate::Expr::Primary(crate::PrimaryExpr::Identifier(
            parser.get_identifier()?,
        )))
    }

    fn array(parser: &mut Parser) -> ParseResult<crate::Expr> {
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
        let span = crate::SourceSpan::combine(start_span, end_span);

        Ok(crate::Expr::Primary(crate::PrimaryExpr::Array(
            crate::ArrayLiteral { elements, span },
        )))
    }

    fn binary(parser: &mut Parser, left: crate::Expr) -> ParseResult<crate::Expr> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Expected previous token.");
        let span_start = left.span().start;
        let token_type = token.token_type;

        let rule = get_rule(token_type);

        let precedence: Precedence = (rule.precedence as u8 + 1).into();

        let right = parse(parser, precedence)?;
        let span = crate::SourceSpan::new(span_start, right.span().end);

        match token_type {
            tokenizer::TokenType::Plus => Ok(crate::Expr::Term(crate::TermExpr {
                left: Box::new(left),
                operator: crate::TermOperator::Add,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Minus => Ok(crate::Expr::Term(crate::TermExpr {
                left: Box::new(left),
                operator: crate::TermOperator::Subtract,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Star => Ok(crate::Expr::Factor(crate::FactorExpr {
                left: Box::new(left),
                operator: crate::FactorOperator::Multiply,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Slash => Ok(crate::Expr::Factor(crate::FactorExpr {
                left: Box::new(left),
                operator: crate::FactorOperator::Divide,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Modulo => Ok(crate::Expr::Factor(crate::FactorExpr {
                left: Box::new(left),
                operator: crate::FactorOperator::Modulo,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::EqualsEquals => Ok(crate::Expr::Equality(crate::EqualityExpr {
                left: Box::new(left),
                operator: crate::EqualityOperator::Equal,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::BangEquals => Ok(crate::Expr::Equality(crate::EqualityExpr {
                left: Box::new(left),
                operator: crate::EqualityOperator::NotEqual,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Less => Ok(crate::Expr::Comparison(crate::ComparisonExpr {
                left: Box::new(left),
                operator: crate::ComparisonOperator::Less,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::LessEquals => {
                Ok(crate::Expr::Comparison(crate::ComparisonExpr {
                    left: Box::new(left),
                    operator: crate::ComparisonOperator::LessEqual,
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::Greater => Ok(crate::Expr::Comparison(crate::ComparisonExpr {
                left: Box::new(left),
                operator: crate::ComparisonOperator::Greater,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::GreaterEquals => {
                Ok(crate::Expr::Comparison(crate::ComparisonExpr {
                    left: Box::new(left),
                    operator: crate::ComparisonOperator::GreaterEqual,
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::And => Ok(crate::Expr::LogicalAnd(crate::LogicalAndExpr {
                left: Box::new(left),
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Or => Ok(crate::Expr::LogicalOr(crate::LogicalOrExpr {
                left: Box::new(left),
                right: Box::new(right),
                span,
            })),
            _ => Err(crate::QangError::parse_error("Unknown operator.", span)),
        }
    }

    fn ternary(parser: &mut Parser, left: crate::Expr) -> ParseResult<crate::Expr> {
        // Parse the "then" expression with ternary precedence
        let then_expr = Box::new(parse(parser, Precedence::Ternary)?);

        parser.consume(
            tokenizer::TokenType::Colon,
            "Expect ':' after then expression in ternary.",
        )?;

        // Parse the "else" expression with ternary precedence (right associative)
        let else_expr = Box::new(parse(parser, Precedence::Ternary)?);

        let span = crate::SourceSpan::combine(left.span(), else_expr.span());

        Ok(crate::Expr::Ternary(crate::TernaryExpr {
            condition: Box::new(left),
            then_expr: Some(then_expr),
            else_expr: Some(else_expr),
            span,
        }))
    }

    fn pipe(parser: &mut Parser, left: crate::Expr) -> ParseResult<crate::Expr> {
        // Parse the right side with pipe precedence + 1 for left associativity
        let right = Box::new(parse(parser, Precedence::Pipe)?);
        let span = crate::SourceSpan::combine(left.span(), right.span());

        Ok(crate::Expr::Pipe(crate::PipeExpr {
            left: Box::new(left),
            right: Some(right),
            span,
        }))
    }

    fn call(parser: &mut Parser, left: crate::Expr) -> ParseResult<crate::Expr> {
        let mut expr = left;

        // Keep applying call operations as long as we find them
        loop {
            let current_token = match &parser.current_token {
                Some(token) => token,
                None => break, // No more tokens, we're done
            };

            // Determine what kind of call operation this is
            let operation = match current_token.token_type {
                // Function call: expr(args...)
                tokenizer::TokenType::LeftParen => {
                    parser.advance(); // consume the '('
                    let arguments = parse_arguments(parser)?;
                    parser.consume(
                        tokenizer::TokenType::RightParen,
                        "Expect ')' after arguments.",
                    )?;
                    crate::CallOperation::Call(arguments)
                }

                // Property access: expr.property
                tokenizer::TokenType::Dot => {
                    parser.advance(); // consume the '.'
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
                    let property = crate::Identifier::new(property_name, property_span);

                    crate::CallOperation::Property(property)
                }

                // Optional property access: expr.?property
                tokenizer::TokenType::OptionalChaining => {
                    parser.advance(); // consume the '.?'
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
                    let property = crate::Identifier::new(property_name, property_span);

                    crate::CallOperation::OptionalProperty(property)
                }

                // Array/index access: expr[index]
                tokenizer::TokenType::LeftSquareBracket => {
                    parser.advance(); // consume the '['
                    let index = parser.expression()?;
                    parser.consume(
                        tokenizer::TokenType::RightSquareBracket,
                        "Expect ']' after array index.",
                    )?;
                    crate::CallOperation::Index(index)
                }

                // Not a call operation, we're done with this chain
                _ => break,
            };

            // Create a new CallExpr with the operation applied
            let end_span = parser.get_previous_span();
            let span = crate::SourceSpan::combine(expr.span(), end_span);

            expr = crate::Expr::Call(crate::CallExpr {
                callee: Box::new(expr),
                operation: Box::new(operation),
                span,
            });
        }

        Ok(expr)
    }

    fn parse_arguments(parser: &mut Parser) -> ParseResult<Vec<crate::Expr>> {
        let mut arguments = Vec::new();

        // Handle empty argument list: func()
        if parser.check(tokenizer::TokenType::RightParen) {
            return Ok(arguments);
        }

        // Parse first argument
        arguments.push(parser.expression()?);

        // Parse remaining arguments separated by commas
        while parser.match_token(tokenizer::TokenType::Comma) {
            arguments.push(parser.expression()?);
        }

        Ok(arguments)
    }

    pub struct ParseRule {
        infix: Option<InfixParseFn>,
        prefix: Option<PrefixParseFn>,
        precedence: Precedence,
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
            _ => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    pub fn parse(parser: &mut Parser, precedence: Precedence) -> ParseResult<crate::Expr> {
        parser.advance();

        let prefix_rule = parser
            .previous_token
            .as_ref()
            .map(|t| get_rule(t.token_type))
            .and_then(|r| r.prefix);

        let mut expr = match prefix_rule {
            Some(rule) => rule(parser)?,
            None => {
                let span = parser
                    .previous_token
                    .as_ref()
                    .map(crate::SourceSpan::from_token)
                    .unwrap_or_default();

                return Err(crate::QangError::parse_error("Expected expression.", span));
            }
        };

        while let Some(current_token) = &parser.current_token {
            let rule = get_rule(current_token.token_type);
            if precedence <= rule.precedence {
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

        Ok(expr)
    }
}
