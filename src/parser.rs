use std::iter::Peekable;

use crate::{SourceMap, Tokenizer};

pub struct Parser<'a> {
    source_map: &'a SourceMap,
    tokenizer: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            tokenizer: Tokenizer::new(source_map).peekable(),
        }
    }
}
