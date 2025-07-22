use phf::phf_map;

#[derive(PartialEq, Clone)]
pub enum TokenType {
    Equals,             // =
    Bang,               // !
    BangEquals,         // !=
    Less,               // <
    LessEquals,         // <=
    GreaterEquals,      // >=
    Comma,              // ,
    Dot,                // .
    Arrow,              // =>
    Colon,              // :
    Question,           // ?
    Plus,               // +
    Minus,              // -
    Star,               // *
    Slash,              // /
    Modulo,             // %
    Semicolon,          // ;
    Greater,            // >
    LeftParen,          // )
    RightParen,         // (
    LeftBrace,          // {
    RightBrace,         // }
    LeftSquareBracket,  // [
    RightSquareBracket, // [
    Identifier,         // any identifier that is not covered by the other types
    String,             // any string literal
    Number,             // any number literal
    Comment,            // // or /*/
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
    Error(String),      // use when an error occurs during tokenization
    Eof,                // EoF
}

impl TokenType {
    pub fn from_keyword(keyword: &str) -> TokenType {
        IDENTIFIERS
            .get(keyword)
            .unwrap_or(&TokenType::Identifier)
            .clone()
    }
}

static IDENTIFIERS: phf::Map<&'static str, TokenType> = phf_map! {
    "continue" => TokenType::Continue,
    "and" => TokenType::And,
    "class" => TokenType::Class,
    "else" => TokenType::Else,
    "false" => TokenType::False,
    "true" => TokenType::True,
    "var" => TokenType::Var,
    "while" => TokenType::While,
    "fn" => TokenType::Fn,
    "if" => TokenType::False,
    "nil" => TokenType::Nil,
    "or" => TokenType::False,
    "break" => TokenType::Break,
    "super" => TokenType::Super,
    "this" => TokenType::This,
};

pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
    pub col: u32,
    location: usize,
    length: usize,
}

impl Token {
    pub fn lexeme<'a>(&self, source: &'a str) -> &'a str {
        &source[self.location..self.location + self.length]
    }
}

pub struct Tokenizer {
    source: String,
    line: u32,
    col: u32,
    location: usize,
}

impl Tokenizer {
    pub fn new(source: String) -> Self {
        Self {
            source,
            line: 1,
            col: 1,
            location: 0,
        }
    }

    /// Advances the cursor.
    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        let c = self.source.chars().nth(self.location).unwrap_or('\0');
        self.location += c.len_utf8(); // Handle UTF-8 properly
        self.col += 1;

        c
    }

    // Peeks at the next character.
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.location).unwrap_or('\0')
        }
    }

    /// Peeks at the character after the next.
    fn peek_next(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.location + 1).unwrap_or('\0')
        }
    }

    /// Moves the cursor through whitespace.
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.col += 1;
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.location >= self.source.len()
    }

    /// Peeks at the next character. If it matches, advances the cursor.
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.location += expected.len_utf8();
            self.col += 1;
            true
        }
    }

    fn make_token(&self, token_type: TokenType, length: usize) -> Option<Token> {
        Some(Token {
            token_type,
            line: self.line,
            col: self.col,
            length,
            location: self.location,
        })
    }

    fn error_token(&self, message: &str) -> Option<Token> {
        Some(Token {
            token_type: TokenType::Error(message.to_string()),
            line: self.line,
            col: self.col,
            length: 1,
            location: self.location,
        })
    }

    fn string(&mut self) -> Option<Token> {
        let mut length = 0;

        while !self.match_char('"') {
            length += 1;
        }

        self.make_token(TokenType::String, length)
    }

    fn number(&mut self) -> Option<Token> {
        let mut length = 0;
        while self.peek().is_ascii_digit() {
            length += 1;
            self.advance();
        }

        // Look for fractional part
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            // Consume the '.'
            self.advance();
            length += 1;

            while self.peek().is_ascii_digit() {
                self.advance();
                length += 1;
            }
        }

        self.make_token(TokenType::Number, length)
    }

    fn identifier(&mut self) -> Option<Token> {
        let mut length = 0;
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            length += 1;
            self.advance();
        }

        let keyword = &self.source[self.location..(self.location + length)];
        let token_type = TokenType::from_keyword(keyword);
        return self.make_token(token_type, length);
    }

    fn single_line_comment(&mut self) -> Option<Token> {
        let mut length = 0;
        while !self.match_char('\n') && !self.is_at_end() {
            length += 1;
            self.advance();
        }

        self.make_token(TokenType::Comment, length)
    }

    fn multi_line_commment(&mut self) -> Option<Token> {
        let mut length = 0;
        while self.peek() != '*' && self.peek_next() != '/' && !self.is_at_end() {
            length += 1;
            self.advance();
        }

        self.advance();
        self.advance();

        self.make_token(TokenType::Comment, length)
    }

    pub fn source(&self) -> &str {
        &self.source
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        if self.is_at_end() {
            return self.make_token(TokenType::Eof, 0);
        }

        let c = self.advance();

        match c {
            '(' => self.make_token(TokenType::LeftParen, 1),
            ')' => self.make_token(TokenType::RightParen, 1),
            '!' => {
                if self.match_char('=') {
                    self.advance();
                    self.make_token(TokenType::BangEquals, 2)
                } else {
                    self.make_token(TokenType::Bang, 1)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.advance();
                    self.make_token(TokenType::LessEquals, 2)
                } else {
                    self.make_token(TokenType::Less, 1)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.advance();
                    self.make_token(TokenType::GreaterEquals, 2)
                } else {
                    self.make_token(TokenType::Greater, 1)
                }
            }
            '=' => self.make_token(TokenType::Equals, 1),
            '-' => {
                if self.match_char('>') {
                    self.advance();
                    self.make_token(TokenType::Arrow, 2)
                } else {
                    self.make_token(TokenType::Minus, 1)
                }
            }
            '*' => self.make_token(TokenType::Star, 1),
            '+' => self.make_token(TokenType::Plus, 1),
            '/' => {
                if self.match_char('/') {
                    self.single_line_comment()
                } else if self.match_char('*') {
                    self.multi_line_commment()
                } else {
                    self.make_token(TokenType::Slash, 1)
                }
            }
            '%' => self.make_token(TokenType::Modulo, 1),
            '.' => self.make_token(TokenType::Dot, 1),
            ',' => self.make_token(TokenType::Comma, 1),
            ':' => self.make_token(TokenType::Colon, 1),
            '?' => self.make_token(TokenType::Question, 1),
            ';' => self.make_token(TokenType::Semicolon, 1),
            '{' => self.make_token(TokenType::LeftBrace, 1),
            '}' => self.make_token(TokenType::RightBrace, 1),
            '[' => self.make_token(TokenType::RightSquareBracket, 1),
            ']' => self.make_token(TokenType::LeftSquareBracket, 1),
            '"' => self.string(),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            _ => self.error_token("Unexpected character."),
        }
    }
}
