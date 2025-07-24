use std::iter::Peekable;

use crate::{
    Program, SourceSpan,
    error::{ErrorKind, ErrorReporter, QangResult},
    tokenizer::{SourceMap, Token, Tokenizer},
};

pub struct Parser<'a> {
    source_map: &'a SourceMap,
    tokens: Peekable<Tokenizer<'a>>,
    previous_token: Option<Token>,
    current_token: Option<Token>,
    has_error: bool,
    errors: ErrorReporter<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            tokens: Tokenizer::new(source_map).peekable(),
            previous_token: None,
            current_token: None,
            has_error: false,
            errors: ErrorReporter::new(source_map),
        }
    }

    pub fn parse(&mut self) -> QangResult<Program> {
        let mut program = Program::new(
            Vec::new(),
            SourceSpan::new(0, self.source_map.get_source().len() - 1),
        );

        if self.has_error {
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

    fn error_at(&mut self, token: Option<&Token>, message: &str) {
        self.has_error = true;
        self.errors
            .report_at_token(ErrorKind::Parse, message.to_string(), token);
    }

    fn synchronize(&mut self) {
        todo!("synchronize the parser past the current statement.");
        self.has_error = false;
    }
}

enum Precedence {
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
