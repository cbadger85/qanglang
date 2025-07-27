use crate::{
    AssignmentExpression, Declaration, Expression, ExpressionStatement, Identifier, Program,
    QangError, QangResult, SourceMap, SourceSpan, VariableDeclaration,
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
        self.current_token = self.tokens.next();
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

        return Err(QangError::parse_error(message, span));
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

            if let Some(current_token_type) = self.current_token.as_ref().map(|t| &t.token_type) {
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
                    _ => (),
                }
            }

            self.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        self.check(TokenType::Eof)
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
            let errors = self.errors.into_errors();
            Err(errors)
        } else {
            Ok(program)
        }
    }

    fn declaration(&mut self) -> Option<Declaration> {
        let result = if self.match_token(TokenType::Var) {
            self.var_declaration()
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

    fn var_declaration(&mut self) -> ParseResult<Declaration> {
        let var_span = self.get_previous_span();
        self.consume(TokenType::Identifier, "Expect variable name.")?;

        let identifier_span = self.get_previous_span();
        let name = self
            .previous_token
            .as_ref()
            .map(|t| t.lexeme(&self.source_map))
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

        Ok(Declaration::Variable(VariableDeclaration {
            name: identifier,
            initializer,
            span,
        }))
    }

    fn statement(&mut self) -> ParseResult<Declaration> {
        let expression_statement = self.expression_statement()?;
        Ok(Declaration::Statement(crate::Statement::Expression(
            expression_statement,
        )))
    }

    fn expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let expr = self.expression()?;

        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;

        let semicolon_span = self.get_previous_span();

        let expression_span = expr.span();

        Ok(ExpressionStatement {
            expr,
            span: SourceSpan::combine(expression_span, semicolon_span),
        })
    }

    fn expression(&mut self) -> ParseResult<Expression> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<crate::Expression> {
        let expr = expression_parser::parse(self, expression_parser::Precedence::Ternary)?;

        if self.match_token(TokenType::Equals) {
            let value = Box::new(self.expression()?);
            let span = SourceSpan::combine(expr.span(), value.span());

            match expr {
                crate::Expression::Primary(crate::PrimaryExpression::Identifier(id)) => {
                    Ok(crate::Expression::Assignment(AssignmentExpression {
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

    type PrefixParseFn = fn(&mut Parser) -> ParseResult<crate::Expression>;
    type InfixParseFn = fn(&mut Parser, crate::Expression) -> ParseResult<crate::Expression>;

    fn number(parser: &mut Parser) -> ParseResult<crate::Expression> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let span = crate::SourceSpan::from_token(token);

        let value = token
            .lexeme(&parser.source_map)
            .parse::<f64>()
            .map_err(|_| crate::QangError::parse_error("Expected number.", span))?;

        Ok(crate::Expression::Primary(
            crate::PrimaryExpression::Number(crate::NumberLiteral { value, span }),
        ))
    }

    fn grouping(parser: &mut Parser) -> ParseResult<crate::Expression> {
        let expr = parser.expression()?;
        parser.consume(
            tokenizer::TokenType::RightParen,
            "Expect ')' after expression.",
        )?;

        Ok(expr)
    }

    fn unary(parser: &mut Parser) -> ParseResult<crate::Expression> {
        let operand = Box::new(parser.expression()?);
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let operator_type = &token.token_type.clone();
        let operator_span = crate::SourceSpan::from_token(token);

        let span = crate::SourceSpan::combine(operator_span, operand.span());
        match operator_type {
            tokenizer::TokenType::Minus => Ok(crate::Expression::Unary(crate::UnaryExpression {
                operator: crate::UnaryOperator::Minus,
                operand,
                span,
            })),
            tokenizer::TokenType::Bang => Ok(crate::Expression::Unary(crate::UnaryExpression {
                operator: crate::UnaryOperator::Not,
                operand,
                span,
            })),
            _ => Err(crate::QangError::parse_error("Unknown operator.", span)),
        }
    }

    fn literal(parser: &mut Parser) -> ParseResult<crate::Expression> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let span = crate::SourceSpan::from_token(token);

        match token.token_type {
            tokenizer::TokenType::False => Ok(crate::Expression::Primary(
                crate::PrimaryExpression::Boolean(crate::BooleanLiteral { value: false, span }),
            )),
            tokenizer::TokenType::True => Ok(crate::Expression::Primary(
                crate::PrimaryExpression::Boolean(crate::BooleanLiteral { value: true, span }),
            )),
            tokenizer::TokenType::Nil => Ok(crate::Expression::Primary(
                crate::PrimaryExpression::Nil(crate::NilLiteral { span }),
            )),
            _ => Err(crate::QangError::parse_error("Unknown literal.", span)),
        }
    }

    fn string(parser: &mut Parser) -> ParseResult<crate::Expression> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let value = token.lexeme(&parser.source_map);

        let span = crate::SourceSpan::from_token(token);

        Ok(crate::Expression::Primary(
            crate::PrimaryExpression::String(crate::StringLiteral { value, span }),
        ))
    }

    fn binary(parser: &mut Parser, left: crate::Expression) -> ParseResult<crate::Expression> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Expected previous token.");
        let span_start = left.span().start;
        let token_type = token.token_type.clone();

        let rule = get_rule(&token_type);

        let precedence: Precedence = (rule.precedence as u8 + 1).into();

        let right = parse(parser, precedence)?;
        let span = crate::SourceSpan::new(span_start, right.span().end);

        match token_type {
            tokenizer::TokenType::Plus => Ok(crate::Expression::Term(crate::TermExpression {
                left: Box::new(left),
                operator: crate::TermOperator::Add,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Minus => Ok(crate::Expression::Term(crate::TermExpression {
                left: Box::new(left),
                operator: crate::TermOperator::Subtract,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Star => Ok(crate::Expression::Factor(crate::FactorExpression {
                left: Box::new(left),
                operator: crate::FactorOperator::Multiply,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Slash => Ok(crate::Expression::Factor(crate::FactorExpression {
                left: Box::new(left),
                operator: crate::FactorOperator::Divide,
                right: Box::new(right),
                span,
            })),
            tokenizer::TokenType::Modulo => {
                Ok(crate::Expression::Factor(crate::FactorExpression {
                    left: Box::new(left),
                    operator: crate::FactorOperator::Modulo,
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::EqualsEquals => {
                Ok(crate::Expression::Equality(crate::EqualityExpression {
                    left: Box::new(left),
                    operator: crate::EqualityOperator::Equal,
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::BangEquals => {
                Ok(crate::Expression::Equality(crate::EqualityExpression {
                    left: Box::new(left),
                    operator: crate::EqualityOperator::NotEqual,
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::Less => {
                Ok(crate::Expression::Comparison(crate::ComparisonExpression {
                    left: Box::new(left),
                    operator: crate::ComparisonOperator::Less,
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::LessEquals => {
                Ok(crate::Expression::Comparison(crate::ComparisonExpression {
                    left: Box::new(left),
                    operator: crate::ComparisonOperator::LessEqual,
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::Greater => {
                Ok(crate::Expression::Comparison(crate::ComparisonExpression {
                    left: Box::new(left),
                    operator: crate::ComparisonOperator::Greater,
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::GreaterEquals => {
                Ok(crate::Expression::Comparison(crate::ComparisonExpression {
                    left: Box::new(left),
                    operator: crate::ComparisonOperator::GreaterEqual,
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::And => {
                Ok(crate::Expression::LogicalAnd(crate::LogicalAndExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    span,
                }))
            }
            tokenizer::TokenType::Or => {
                Ok(crate::Expression::LogicalOr(crate::LogicalOrExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    span,
                }))
            }
            _ => Err(crate::QangError::parse_error("Unknown operator.", span)),
        }
    }

    fn identifier(parser: &mut Parser) -> ParseResult<crate::Expression> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");
        let span = crate::SourceSpan::from_token(token);
        let name = token.lexeme(&parser.source_map);
        let identifier = crate::Identifier::new(name, span);

        Ok(crate::Expression::Primary(
            crate::PrimaryExpression::Identifier(identifier),
        ))
    }

    struct ParseRule {
        infix: Option<InfixParseFn>,
        prefix: Option<PrefixParseFn>,
        precedence: Precedence,
    }

    impl Default for ParseRule {
        fn default() -> Self {
            Self {
                infix: None,
                prefix: None,
                precedence: Precedence::None,
            }
        }
    }

    const fn get_rule(token_type: &tokenizer::TokenType) -> ParseRule {
        match token_type {
            tokenizer::TokenType::Number => ParseRule {
                infix: None,
                prefix: Some(number),
                precedence: Precedence::None,
            },
            tokenizer::TokenType::LeftParen => ParseRule {
                prefix: Some(grouping),
                infix: None,
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
            _ => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    pub fn parse(parser: &mut Parser, precedence: Precedence) -> ParseResult<crate::Expression> {
        parser.advance();

        let prefix_rule = parser
            .previous_token
            .as_ref()
            .map(|t| get_rule(&t.token_type))
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
            let rule = get_rule(&current_token.token_type);
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
