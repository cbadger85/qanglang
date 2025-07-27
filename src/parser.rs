use std::iter::Peekable;

use crate::{
    Declaration, Expression, ExpressionStatement, Identifier, Program, QangError, QangResult,
    SourceSpan, VariableDeclaration,
    error::ErrorReporter,
    tokenizer::{SourceMap, Token, TokenType, Tokenizer},
};

type ParseResult<T> = Result<T, QangError>;

pub struct Parser<'a> {
    source_map: &'a SourceMap,
    tokens: Peekable<Tokenizer<'a>>,
    previous_token: Option<Token>,
    current_token: Option<Token>,
    errors: ErrorReporter<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            tokens: Tokenizer::new(source_map).peekable(),
            previous_token: None,
            current_token: None,
            errors: ErrorReporter::new(source_map),
        }
    }

    pub fn parse(&mut self) -> QangResult<Program> {
        let mut program = Program::new(
            Vec::new(),
            SourceSpan::new(0, self.source_map.get_source().len() - 1),
        );

        if self.errors.has_errors() {
            let errors = self.errors.into_errors();
            Err(errors)
        } else {
            Ok(program)
        }
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

        return Err(QangError::parse_error(message, span));
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            return false;
        }

        self.advance();

        true
    }

    fn check(&mut self, token_type: TokenType) -> bool {
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

    fn expression(&mut self) -> ParseResult<Expression> {
        expression_parser::parse(self, expression_parser::Precedence::Assignment)
    }

    fn expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let expression = self.expression()?;

        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;

        let semicolon_span = self.get_previous_span();

        let expression_span = expression.span();

        Ok(ExpressionStatement {
            expression,
            span: SourceSpan::combine(expression_span, semicolon_span),
        })
    }

    fn declaration(&mut self) -> Option<Declaration> {
        let result = if self.match_token(TokenType::Var) {
            self.var_declaration()
        } else {
            self.statement()
        };

        match result {
            Ok(declaration) => Some(declaration),
            Err(_) => {
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
        let identifer = Identifier::new(name, identifier_span);

        let var_declaration = if self.match_token(TokenType::Equals) {
            let equals_span = self.get_previous_span();
            let expression = self.expression()?;

            let span = SourceSpan::combine(
                var_span,
                SourceSpan::combine(
                    SourceSpan::combine(identifier_span, equals_span),
                    expression.span(),
                ),
            );

            Ok(Declaration::Variable(VariableDeclaration {
                name: identifer,
                initializer: Some(expression),
                span,
            }))
        } else {
            Ok(Declaration::Variable(VariableDeclaration {
                name: identifer,
                initializer: None,
                span: identifier_span,
            }))
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        var_declaration
    }

    fn statement(&mut self) -> ParseResult<Declaration> {
        let expression_statement = self.expression_statement()?;
        Ok(Declaration::Statement(crate::Statement::Expression(
            expression_statement,
        )))
    }
}

mod expression_parser {
    use crate::{
        AssignmentExpression, Expression, Identifier, QangError, SourceSpan, StringLiteral,
        parser::{self, ParseResult},
        tokenizer,
    };

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

    type PrefixParseFn = fn(&mut parser::Parser) -> parser::ParseResult<crate::Expression>;
    type InfixParseFn =
        fn(&mut parser::Parser, crate::Expression) -> parser::ParseResult<crate::Expression>;

    fn number(parser: &mut parser::Parser) -> parser::ParseResult<crate::Expression> {
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

    fn grouping(parser: &mut parser::Parser) -> parser::ParseResult<crate::Expression> {
        let expression = parser.expression()?;
        parser.consume(
            tokenizer::TokenType::RightParen,
            "Expect ')' after expression.",
        )?;

        Ok(expression)
    }

    fn unary(parser: &mut parser::Parser) -> parser::ParseResult<crate::Expression> {
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

    fn literal(parser: &mut parser::Parser) -> parser::ParseResult<crate::Expression> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let span = SourceSpan::from_token(token);

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
            _ => Err(QangError::parse_error("Unknown literal.", span)),
        }
    }

    fn string(parser: &mut parser::Parser) -> parser::ParseResult<crate::Expression> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");

        let value = token.lexeme(&parser.source_map);

        let span = SourceSpan::from_token(token);

        Ok(crate::Expression::Primary(
            crate::PrimaryExpression::String(StringLiteral { value, span }),
        ))
    }

    fn binary(
        parser: &mut parser::Parser,
        left: crate::Expression,
    ) -> parser::ParseResult<crate::Expression> {
        let token = parser
            .previous_token
            .as_ref()
            .expect("Expected previous token.");
        let span_start = left.span().start;
        let token_type = token.token_type.clone();

        let rule = get_rule(&token_type).expect("Expected rule for token type.");

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

    fn variable(parser: &mut parser::Parser) -> ParseResult<crate::Expression> {
        // TODO validate the assignment target is valid.
        let token = parser
            .previous_token
            .as_ref()
            .expect("Missing previous token.");
        let identifier_span = SourceSpan::from_token(token);
        let precedence = get_rule(&token.token_type)
            .map(|t| t.precedence)
            .unwrap_or(Precedence::None);
        let is_assignable = precedence <= Precedence::Assignment;

        let name = token.lexeme(&parser.source_map);
        let identifier = Identifier::new(name, identifier_span);

        if parser.match_token(tokenizer::TokenType::Equals) && is_assignable {
            let value = parser.expression()?;
            let equals_span = parser.get_previous_span();

            let span = SourceSpan::combine(
                SourceSpan::combine(identifier_span, equals_span),
                value.span(),
            );

            Ok(crate::Expression::Assignment(AssignmentExpression {
                value: Box::new(value),
                target: crate::AssignmentTarget::Identifier(identifier),
                span,
            }))
        } else {
            Ok(crate::Expression::Primary(
                crate::PrimaryExpression::Identifier(identifier),
            ))
        }
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

    const fn get_rule(token_type: &tokenizer::TokenType) -> Option<ParseRule> {
        match token_type {
            tokenizer::TokenType::Number => Some(ParseRule {
                infix: None,
                prefix: Some(number),
                precedence: Precedence::None,
            }),
            tokenizer::TokenType::LeftParen => Some(ParseRule {
                prefix: Some(grouping),
                infix: None,
                precedence: Precedence::None,
            }),
            tokenizer::TokenType::Minus => Some(ParseRule {
                prefix: Some(unary),
                infix: Some(binary),
                precedence: Precedence::Term,
            }),
            tokenizer::TokenType::Plus => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Term,
            }),
            tokenizer::TokenType::Star => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Factor,
            }),
            tokenizer::TokenType::Slash => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Factor,
            }),
            tokenizer::TokenType::Modulo => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Factor,
            }),
            tokenizer::TokenType::Greater => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Comparison,
            }),
            tokenizer::TokenType::GreaterEquals => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Comparison,
            }),
            tokenizer::TokenType::Less => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Comparison,
            }),
            tokenizer::TokenType::LessEquals => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Comparison,
            }),
            tokenizer::TokenType::EqualsEquals => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Equality,
            }),
            tokenizer::TokenType::BangEquals => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Equality,
            }),
            tokenizer::TokenType::And => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::And,
            }),
            tokenizer::TokenType::Or => Some(ParseRule {
                prefix: None,
                infix: Some(binary),
                precedence: Precedence::Or,
            }),
            tokenizer::TokenType::False => Some(ParseRule {
                prefix: Some(literal),
                infix: None,
                precedence: Precedence::None,
            }),
            tokenizer::TokenType::True => Some(ParseRule {
                prefix: Some(literal),
                infix: None,
                precedence: Precedence::None,
            }),
            tokenizer::TokenType::Nil => Some(ParseRule {
                prefix: Some(literal),
                infix: None,
                precedence: Precedence::None,
            }),
            tokenizer::TokenType::Bang => Some(ParseRule {
                prefix: Some(unary),
                infix: None,
                precedence: Precedence::None,
            }),
            tokenizer::TokenType::String => Some(ParseRule {
                prefix: Some(string),
                infix: None,
                precedence: Precedence::None,
            }),
            tokenizer::TokenType::Identifier => Some(ParseRule {
                prefix: Some(variable),
                infix: None,
                precedence: Precedence::None,
            }),
            _ => None,
        }
    }

    pub fn parse(
        parser: &mut parser::Parser,
        precedence: Precedence,
    ) -> parser::ParseResult<crate::Expression> {
        parser.advance();

        let prefix_rule = parser
            .previous_token
            .as_ref()
            .and_then(|t| get_rule(&t.token_type))
            .and_then(|r| r.prefix);

        let mut expression = match prefix_rule {
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
            if let Some(rule) = get_rule(&current_token.token_type) {
                if precedence <= rule.precedence {
                    parser.advance();

                    if let Some(infix_rule) = rule.infix {
                        expression = infix_rule(parser, expression)?;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(expression)
    }
}
