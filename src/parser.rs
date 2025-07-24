use std::iter::Peekable;

use crate::{
    Program, SourceSpan,
    tokenizer::{SourceMap, Tokenizer},
};

pub struct Parser<'a> {
    source_map: &'a SourceMap,
    tokens: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            tokens: Tokenizer::new(source_map).peekable(),
        }
    }

    pub fn parse(&self) -> Program {
        let mut program = Program::new(
            Vec::new(),
            SourceSpan::new(0, self.source_map.get_source().len() - 1),
        );

        program
    }
}
