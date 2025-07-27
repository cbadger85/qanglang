use crate::{
    AssignmentExpr, BlockStmt, Decl, Expr, ExprStmt, Identifier, Program, QangError, QangResult,
    SourceMap, SourceSpan, VariableDecl,
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
        let result = if self.match_token(TokenType::Var) {
            self.variable_declaration()
        } else {
            self.statement()
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

        let identifier_span = self.get_previous_span();
        let name = self
            .previous_token
            .as_ref()
            .map(|t| t.lexeme(self.source_map))
            .unwrap();
        let identifier = Identifier::new(name, identifier_span);

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

        Ok(Decl::Variable(VariableDecl {
            name: identifier,
            initializer,
            span,
        }))
    }

    fn statement(&mut self) -> ParseResult<Decl> {
        let expr_stmt = self.expression_statement()?;
        Ok(Decl::Stmt(crate::Stmt::Expr(expr_stmt)))
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

    fn assignment(&mut self) -> ParseResult<crate::Expr> {
        let expr = expression_parser::parse(self, expression_parser::Precedence::Ternary)?;

        if self.match_token(TokenType::Equals) {
            let value = Box::new(self.expression()?);
            let span = SourceSpan::combine(expr.span(), value.span());

            match expr {
                crate::Expr::Primary(crate::PrimaryExpr::Identifier(id)) => {
                    Ok(crate::Expr::Assignment(AssignmentExpr {
                        target: crate::AssignmentTarget::Identifier(id),
                        value,
                        span,
                    }))
                }
                // TODO: Handle property assignments
                _ => Err(crate::QangError::parse_error(
                    "Invalid assignemnt target",
                    span,
                )),
            }
        } else {
            Ok(expr)
        }
    }

    fn argument_parameters(&mut self) -> ParseResult<Vec<Identifier>> {
        todo!()
    }

    fn block(&mut self) -> ParseResult<BlockStmt> {
        todo!()
    }
}

pub mod expression_parser {
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
        // Look ahead to see if this is a lambda
        if parser.is_lambda_start() {
            todo!()
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
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");
        let span = crate::SourceSpan::from_token(token);
        let name = token.lexeme(parser.source_map);
        let identifier = crate::Identifier::new(name, span);

        Ok(crate::Expr::Primary(crate::PrimaryExpr::Identifier(
            identifier,
        )))
    }

    fn array(parser: &mut Parser) -> ParseResult<crate::Expr> {
        todo!()
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
        todo!("use a combination of logical ands and ors to create a ternary expression.")
    }

    fn pipe(parser: &mut Parser, left: crate::Expr) -> ParseResult<crate::Expr> {
        todo!()
    }

    fn call(parser: &mut Parser, left: crate::Expr) -> ParseResult<crate::Expr> {
        todo!()
    }

    pub struct ParseRule {
        infix: Option<InfixParseFn>,
        prefix: Option<PrefixParseFn>,
        precedence: Precedence,
    }

    pub const fn get_rule(token_type: tokenizer::TokenType) -> ParseRule {
        match token_type {
            tokenizer::TokenType::Number => ParseRule {
                prefix: Some(number),
                infix: None,
                precedence: Precedence::None,
            },
            tokenizer::TokenType::LeftParen => ParseRule {
                prefix: Some(grouping_or_lambda),
                infix: Some(call), // TODO does this go here?
                precedence: Precedence::None,
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
                infix: None,
                precedence: Precedence::Pipe,
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
