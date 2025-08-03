use std::{collections::VecDeque, rc::Rc};

use phf::phf_map;

use crate::SourceMap;

#[derive(PartialEq, Clone, Debug, Copy)]
#[repr(u8)]
pub enum TokenType {
    EqualsEquals,       // ==
    Equals,             // =
    Bang,               // !
    BangEquals,         // !=
    Less,               // <
    LessEquals,         // <=
    GreaterEquals,      // >=
    Comma,              // ,
    Dot,                // .
    Arrow,              // ->
    Colon,              // :
    Question,           // ?
    Plus,               // +
    Minus,              // -
    Star,               // *
    Slash,              // /
    Modulo,             // %
    Semicolon,          // ;
    Greater,            // >
    LeftParen,          // ()
    RightParen,         // )
    LeftBrace,          // {
    RightBrace,         // }
    LeftSquareBracket,  // [
    RightSquareBracket, // [
    Identifier,         // any identifier that is not covered by the other types
    String,             // any string literal
    Number,             // any number literal
    And,                // and
    Class,              // class
    Else,               // else
    False,              // false
    For,                // for
    Fn,                 // fn
    If,                 // if
    Nil,                // nil
    Or,                 // or
    Return,             // return
    Super,              // super
    This,               // this
    True,               // true
    Var,                // var
    While,              // while
    Break,              // break
    Continue,           // continue
    OptionalChaining,   // .?
    Pipe,               // |>
    Throw,              // throw
    Try,                // try
    Catch,              // catch
    Finally,            // finally
    ColonBrace,         // :{
    Error,              // use when an error occurs during tokenization
    Eof,                // EoF
}

impl TokenType {
    pub fn from_keyword(keyword: &str) -> TokenType {
        *KEYWORDS.get(keyword).unwrap_or(&TokenType::Identifier)
    }
}

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "and" => TokenType::And,
    "class" => TokenType::Class,
    "else" => TokenType::Else,
    "false" => TokenType::False,
    "for" => TokenType::For,
    "continue" => TokenType::Continue,
    "true" => TokenType::True,
    "var" => TokenType::Var,
    "while" => TokenType::While,
    "fn" => TokenType::Fn,
    "if" => TokenType::If,
    "nil" => TokenType::Nil,
    "or" => TokenType::Or,
    "break" => TokenType::Break,
    "super" => TokenType::Super,
    "this" => TokenType::This,
    "return" => TokenType::Return,
    "throw" => TokenType::Throw,
    "try" => TokenType::Try,
    "catch" => TokenType::Catch,
    "finally" => TokenType::Finally,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub start: usize,
    pub end: usize,
    pub error_message: Option<String>,
}

impl Token {
    pub fn lexeme<'a>(&self, source_map: &'a SourceMap) -> &'a [char] {
        &source_map.get_source()[self.start..self.end]
    }
    pub fn line(&self, source_map: &SourceMap) -> u32 {
        source_map.get_line_number(self.start)
    }

    pub fn col(&self, source_map: &SourceMap) -> u32 {
        source_map.get_column_number(self.start)
    }
}

#[derive(Debug, Clone)]
pub struct Tokenizer {
    source_map: Rc<SourceMap>,
    line: u32,
    col: u32,
    location: usize,
    is_eof: bool,
    lookahead_buffer: VecDeque<Token>,
}

impl Tokenizer {
    /// Creates a new tokenizer for the source map.
    pub fn new(source_map: Rc<SourceMap>) -> Self {
        Self {
            source_map,
            line: 1,
            col: 1,
            location: 0,
            is_eof: false,
            lookahead_buffer: VecDeque::new(),
        }
    }

    /// Peek at the nth token ahead (0 = next token, 1 = token after that, etc.)
    pub fn peek_ahead(&mut self, n: usize) -> Option<&Token> {
        while self.lookahead_buffer.len() <= n {
            if let Some(token) = self.next_token() {
                self.lookahead_buffer.push_back(token);
            } else {
                break;
            }
        }
        self.lookahead_buffer.get(n)
    }

    /// Convenience method for peek (peek_ahead(0))
    pub fn peek(&mut self) -> Option<&Token> {
        self.peek_ahead(0)
    }

    /// Convenience method for peek_next (peek_ahead(1))
    #[allow(dead_code)]
    pub fn peek_next(&mut self) -> Option<&Token> {
        self.peek_ahead(1)
    }

    fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        if self.is_at_end() {
            if self.is_eof {
                return None;
            } else {
                self.is_eof = true;
                return self.make_token(TokenType::Eof, self.location);
            }
        }

        let start = self.location;
        let c = self.advance();

        match c {
            '(' => self.make_token(TokenType::LeftParen, start),
            ')' => self.make_token(TokenType::RightParen, start),
            '!' if self.match_char('=') => self.make_token(TokenType::BangEquals, start),
            '!' => self.make_token(TokenType::Bang, start),
            '<' if self.match_char('=') => self.make_token(TokenType::LessEquals, start),
            '<' => self.make_token(TokenType::Less, start),
            '>' if self.match_char('=') => self.make_token(TokenType::GreaterEquals, start),
            '>' => self.make_token(TokenType::Greater, start),
            '=' if self.match_char('=') => self.make_token(TokenType::EqualsEquals, start),
            '=' => self.make_token(TokenType::Equals, start),
            '-' if self.match_char('>') => self.make_token(TokenType::Arrow, start),
            '-' => self.make_token(TokenType::Minus, start),
            '*' => self.make_token(TokenType::Star, start),
            '+' => self.make_token(TokenType::Plus, start),
            '/' if self.match_char('/') => self.single_line_comment(),
            '/' if self.match_char('*') => self.multi_line_comment(),
            '/' => self.make_token(TokenType::Slash, start),
            '%' => self.make_token(TokenType::Modulo, start),
            '.' if self.match_char('?') => self.make_token(TokenType::OptionalChaining, start),
            '.' if self.peek_char().is_ascii_digit() => self.number(),
            '.' => self.make_token(TokenType::Dot, start),
            ',' => self.make_token(TokenType::Comma, start),
            ':' if self.match_char('{') => self.make_token(TokenType::ColonBrace, start),
            ':' => self.make_token(TokenType::Colon, start),
            '?' => self.make_token(TokenType::Question, start),
            ';' => self.make_token(TokenType::Semicolon, start),
            '{' => self.make_token(TokenType::LeftBrace, start),
            '}' => self.make_token(TokenType::RightBrace, start),
            '[' => self.make_token(TokenType::LeftSquareBracket, start),
            ']' => self.make_token(TokenType::RightSquareBracket, start),
            '"' => self.string(),
            '|' if self.match_char('>') => self.make_token(TokenType::Pipe, start),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            _ => self.error_token(format!("Unexpected character: '{}'.", c).as_str()),
        }
    }

    /// Advances the cursor.
    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        let c = self.source_map.get_source()[self.location];
        self.location += 1;

        if c == '\n' {
            self.line += 1;
            self.col = 1;
        }

        c
    }

    // Peeks at the next character.
    fn peek_char(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source_map.get_source()[self.location]
        }
    }

    /// Peeks at the character after the next.
    fn peek_next_char(&self) -> char {
        if self.location + 1 >= self.source_map.get_source().len() {
            '\0'
        } else {
            self.source_map.get_source()[self.location + 1]
        }
    }

    /// Moves the cursor through whitespace.
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek_char() {
                ' ' | '\r' | '\t' => {
                    self.col += 1;
                    self.advance();
                }
                '\n' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    /// returns true if the cursor is at the end of the source code.
    fn is_at_end(&self) -> bool {
        self.is_eof || self.location >= self.source_map.get_source().len()
    }

    /// Peeks at the next character. If it matches, advances the cursor.
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek_char() != expected {
            false
        } else {
            self.location += expected.len_utf8();
            // self.advance();
            true
        }
    }

    /// Creates a Token of a given type and adds its lexeme.
    fn make_token(&mut self, token_type: TokenType, start: usize) -> Option<Token> {
        let end = self.location;

        Some(Token {
            token_type,
            start,
            end,
            error_message: None,
        })
    }

    /// Adds an error token for tokenization errors.
    fn error_token(&self, message: &str) -> Option<Token> {
        Some(Token {
            token_type: TokenType::Error,
            start: self.location,
            end: self.location,
            error_message: Some(message.to_string()),
        })
    }

    /// Creates a string token.
    fn string(&mut self) -> Option<Token> {
        let start = self.location - 1;

        while self.peek_char() != '"' && !self.is_at_end() {
            if self.peek_char() == '\n' {
                // Don't consume the newline - leave it for the next token
                return Some(Token {
                    token_type: TokenType::Error,
                    start,
                    end: self.location,
                    error_message: Some("Unterminated string.".to_string()),
                });
            }

            if self.peek_char() == '\\' {
                self.advance_in_string(); // consume backslash
                if !self.is_at_end() {
                    let escaped = self.peek_char();
                    match escaped {
                        'n' | 't' | 'r' | '\\' | '"' | '\'' | '0' => {
                            self.advance_in_string(); // valid escape
                        }
                        _ => {
                            return self
                                .error_token(&format!("Invalid escape sequence: \\{}", escaped));
                        }
                    }
                }
            } else {
                self.advance_in_string();
            }
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        self.advance(); // Consume closing quote 
        self.make_token(TokenType::String, start)
    }

    /// Advances cursor without treating characters as line breaks (for use in strings)
    fn advance_in_string(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        let c = self.source_map.get_source()[self.location];
        self.location += 1;

        // Don't increment line/col for characters inside strings
        // The column tracking will be handled in make_token

        c
    }

    /// Creates a number token.
    fn number(&mut self) -> Option<Token> {
        let start = self.location - 1; // Account for first digit or dot

        let started_with_dot = self.source_map.get_source()[start] == '.';

        while self.peek_char().is_ascii_digit() {
            self.advance();
        }

        if !started_with_dot && self.peek_char() == '.' && self.peek_next_char().is_ascii_digit() {
            self.advance(); // Consume the '.'

            while self.peek_char().is_ascii_digit() {
                self.advance();
            }
        }

        self.make_token(TokenType::Number, start)
    }

    /// Creates an identifier token.
    fn identifier(&mut self) -> Option<Token> {
        let start = self.location - 1;

        while self.peek_char().is_ascii_alphanumeric() || self.peek_char() == '_' {
            self.advance();
        }

        let keyword: String = self.source_map.get_source()[start..self.location]
            .iter()
            .collect();
        let token_type = TokenType::from_keyword(&keyword);

        self.make_token(token_type, start)
    }

    /// Creates a single line comment token.
    fn single_line_comment(&mut self) -> Option<Token> {
        while self.peek_char() != '\n' && !self.is_at_end() {
            self.advance();
        }

        // Continue to next token after consuming comment
        self.next_token()
    }

    /// Creates a multi-line comment token.
    fn multi_line_comment(&mut self) -> Option<Token> {
        while !self.is_at_end() {
            if self.peek_char() == '*' && self.peek_next_char() == '/' {
                self.advance(); // consume *
                self.advance(); // consume /
                break;
            }
            self.advance();
        }

        if self.is_at_end()
            && (self.source_map.get_source().get(self.location - 2) != Some(&'*')
                || self.source_map.get_source().get(self.location - 1) != Some(&'/'))
        {
            return self.error_token("Unterminated comment.");
        }

        // Continue to next token after consuming comment
        self.next_token()
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.lookahead_buffer.pop_front() {
            Some(token)
        } else {
            self.next_token()
        }
    }
}
