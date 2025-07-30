use std::collections::VecDeque;

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
    OptionalChaining,   // .?
    Pipe,               // |>
    Throw,              // throw
    Try,                // try
    Catch,              // catch
    Finally,            // finalyy
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
    pub fn lexeme(&self, source_map: &SourceMap) -> Box<str> {
        source_map.get_source()[self.start..self.end]
            .iter()
            .collect()
    }
    pub fn line(&self, source_map: &SourceMap) -> u32 {
        source_map.get_line_number(self.start)
    }

    pub fn col(&self, source_map: &SourceMap) -> u32 {
        source_map.get_column_number(self.start)
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Tokenizer<'a> {
    source_map: &'a SourceMap,
    line: u32,
    col: u32,
    location: usize,
    is_eof: bool,
    lookahead_buffer: VecDeque<Token>,
}

impl<'a> Tokenizer<'a> {
    /// Creates a new tokenizer for the source map.
    pub fn new(source_map: &'a SourceMap) -> Self {
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
        // Fill buffer until we have enough tokens
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
                return self.make_token(TokenType::Eof, 0);
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
        let start = self.location - 1; // Account for first digit

        while self.peek_char().is_ascii_digit() {
            self.advance();
        }

        // Look for fractional part
        if self.peek_char() == '.' && self.peek_next_char().is_ascii_digit() {
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
        let start = self.location - 2;

        while self.peek_char() != '\n' && !self.is_at_end() {
            self.advance();
        }

        self.make_token(TokenType::Comment, start)
    }

    /// Creates a multi-line comment token.
    fn multi_line_comment(&mut self) -> Option<Token> {
        let start = self.location - 2;

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

        self.make_token(TokenType::Comment, start)
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.lookahead_buffer.pop_front() {
            Some(token)
        } else {
            self.next_token()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tokenizes all source code in the given SourceMap and returns a vector of Tokens.
    fn tokenize_all(source_map: &SourceMap) -> Vec<Token> {
        let tokenizer = Tokenizer::new(source_map);
        tokenizer.collect()
    }

    /// Asserts that the token types produced from tokenizing the source string match the expected types.
    fn assert_token_types(source: &str, expected: &[TokenType]) {
        let source_map = &SourceMap::new(source.to_string());
        let tokens = tokenize_all(source_map);
        let actual: Vec<TokenType> = tokens.into_iter().map(|t| t.token_type).collect();
        assert_eq!(actual, expected);
    }

    /// Asserts that a single token of the expected type is produced from tokenizing the source string,
    /// followed by an EOF token.
    fn assert_single_token(source: &str, expected: TokenType) {
        assert_token_types(source, &[expected, TokenType::Eof]);
    }

    #[test]
    fn test_empty_input() {
        assert_token_types("", &[TokenType::Eof]);
    }

    #[test]
    fn test_whitespace_only() {
        assert_token_types("   \n  \t\r  ", &[TokenType::Eof]);
    }

    #[test]
    fn test_single_character_tokens() {
        assert_single_token("(", TokenType::LeftParen);
        assert_single_token(")", TokenType::RightParen);
        assert_single_token("{", TokenType::LeftBrace);
        assert_single_token("}", TokenType::RightBrace);
        assert_single_token("[", TokenType::LeftSquareBracket);
        assert_single_token("]", TokenType::RightSquareBracket);
        assert_single_token(",", TokenType::Comma);
        assert_single_token(".", TokenType::Dot);
        assert_single_token(":", TokenType::Colon);
        assert_single_token("?", TokenType::Question);
        assert_single_token("+", TokenType::Plus);
        assert_single_token("-", TokenType::Minus);
        assert_single_token("*", TokenType::Star);
        assert_single_token("/", TokenType::Slash);
        assert_single_token("%", TokenType::Modulo);
        assert_single_token(";", TokenType::Semicolon);
        assert_single_token("=", TokenType::Equals);
        assert_single_token("!", TokenType::Bang);
        assert_single_token("<", TokenType::Less);
        assert_single_token(">", TokenType::Greater);
    }

    #[test]
    fn test_two_character_tokens() {
        assert_single_token("!=", TokenType::BangEquals);
        assert_single_token("<=", TokenType::LessEquals);
        assert_single_token(">=", TokenType::GreaterEquals);
        assert_single_token("->", TokenType::Arrow);
    }

    #[test]
    fn test_keywords() {
        assert_single_token("and", TokenType::And);
        assert_single_token("class", TokenType::Class);
        assert_single_token("else", TokenType::Else);
        assert_single_token("false", TokenType::False);
        assert_single_token("for", TokenType::For);
        assert_single_token("fn", TokenType::Fn);
        assert_single_token("if", TokenType::If);
        assert_single_token("nil", TokenType::Nil);
        assert_single_token("or", TokenType::Or);
        assert_single_token("return", TokenType::Return);
        assert_single_token("super", TokenType::Super);
        assert_single_token("this", TokenType::This);
        assert_single_token("true", TokenType::True);
        assert_single_token("var", TokenType::Var);
        assert_single_token("while", TokenType::While);
        assert_single_token("break", TokenType::Break);
        assert_single_token("continue", TokenType::Continue);
    }

    #[test]
    fn test_identifiers() {
        assert_single_token("hello", TokenType::Identifier);
        assert_single_token("_underscore", TokenType::Identifier);
        assert_single_token("with123numbers", TokenType::Identifier);
        assert_single_token("_", TokenType::Identifier);
        assert_single_token("CamelCase", TokenType::Identifier);
    }

    #[test]
    fn test_numbers() {
        assert_single_token("123", TokenType::Number);
        assert_single_token("0", TokenType::Number);
        assert_single_token("123.456", TokenType::Number);
        assert_single_token("0.5", TokenType::Number);
        assert_single_token("999.0", TokenType::Number);
    }

    #[test]
    fn test_strings() {
        assert_single_token("\"hello\"", TokenType::String);
        assert_single_token("\"\"", TokenType::String);
        assert_single_token("\"hello world\"", TokenType::String);
        assert_single_token("\"with spaces and 123 numbers\"", TokenType::String);
    }

    #[test]
    fn test_unterminated_string() {
        let source_map = &SourceMap::new("\"unterminated".to_string());
        let tokens = tokenize_all(source_map);
        assert_eq!(tokens.len(), 2);
        match &tokens[0].token_type {
            TokenType::Error => {
                assert_eq!(
                    tokens[0].error_message.as_ref().unwrap(),
                    "Unterminated string."
                )
            }
            _ => panic!("Expected error token"),
        }
    }

    #[test]
    fn test_single_line_comments() {
        assert_single_token("// this is a comment", TokenType::Comment);
        assert_single_token("//no space", TokenType::Comment);

        let source_map = &SourceMap::new("// comment\n".to_string());
        let tokens = tokenize_all(source_map);
        // Comment followed by newline should only tokenize the comment
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token_type, TokenType::Comment);
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_multi_line_comments() {
        assert_single_token("/* this is a comment */", TokenType::Comment);
        assert_single_token("/*no spaces*/", TokenType::Comment);
        assert_single_token("/* multi\nline\ncomment */", TokenType::Comment);
    }

    #[test]
    fn test_complex_expression() {
        let source = "var x = 123 + 456.789;";
        let expected = vec![
            TokenType::Var,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Number,
            TokenType::Plus,
            TokenType::Number,
            TokenType::Semicolon,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_function_definition() {
        let source = "fn add(a, b) { return a + b }";
        let expected = vec![
            TokenType::Fn,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::Identifier,
            TokenType::Comma,
            TokenType::Identifier,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Return,
            TokenType::Identifier,
            TokenType::Plus,
            TokenType::Identifier,
            TokenType::RightBrace,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_lambda_definition() {
        let source = "var add = (a, b) -> { return a + b }";
        let expected = vec![
            TokenType::Var,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::LeftParen,
            TokenType::Identifier,
            TokenType::Comma,
            TokenType::Identifier,
            TokenType::RightParen,
            TokenType::Arrow,
            TokenType::LeftBrace,
            TokenType::Return,
            TokenType::Identifier,
            TokenType::Plus,
            TokenType::Identifier,
            TokenType::RightBrace,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_lambda_with_implicit_return_definition() {
        let source = "var add = (a, b) -> a + b";
        let expected = vec![
            TokenType::Var,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::LeftParen,
            TokenType::Identifier,
            TokenType::Comma,
            TokenType::Identifier,
            TokenType::RightParen,
            TokenType::Arrow,
            TokenType::Identifier,
            TokenType::Plus,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_conditional_with_comparison() {
        let source = "if x >= 10 and x <= 100";
        let expected = vec![
            TokenType::If,
            TokenType::Identifier,
            TokenType::GreaterEquals,
            TokenType::Number,
            TokenType::And,
            TokenType::Identifier,
            TokenType::LessEquals,
            TokenType::Number,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_array_access() {
        let source = "array[index]";
        let expected = vec![
            TokenType::Identifier,
            TokenType::LeftSquareBracket,
            TokenType::Identifier,
            TokenType::RightSquareBracket,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_object_access() {
        let source = "object.property";
        let expected = vec![
            TokenType::Identifier,
            TokenType::Dot,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_ternary_operator() {
        let source = "condition ? true : false";
        let expected = vec![
            TokenType::Identifier,
            TokenType::Question,
            TokenType::True,
            TokenType::Colon,
            TokenType::False,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_lexeme_extraction() {
        let source_map = &SourceMap::new("hello world 123".to_string());
        let tokens = tokenize_all(source_map);

        assert_eq!(tokens[0].lexeme(source_map), Box::<str>::from("hello"));
        assert_eq!(tokens[1].lexeme(source_map), Box::<str>::from("world"));
        assert_eq!(tokens[2].lexeme(source_map), Box::<str>::from("123"));
    }

    #[test]
    fn test_line_and_column_tracking() {
        let source_map = &SourceMap::new("first\nsecond line".to_string());
        let tokens = tokenize_all(source_map);

        assert_eq!(tokens[0].line(source_map), 1);
        assert_eq!(tokens[0].col(source_map), 1);
        assert_eq!(tokens[1].line(source_map), 2);
        assert_eq!(tokens[1].col(source_map), 1);
        assert_eq!(tokens[2].line(source_map), 2);
        assert_eq!(tokens[2].col(source_map), 8);
    }

    #[test]
    fn test_mixed_content_with_comments() {
        let source = "var x = 5; // variable declaration\n/* block comment */ y = 10;";
        let expected = vec![
            TokenType::Var,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Number,
            TokenType::Semicolon,
            TokenType::Comment,
            TokenType::Comment,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Number,
            TokenType::Semicolon,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_unexpected_character() {
        let source_map = &SourceMap::new("@".to_string());
        let tokens = tokenize_all(source_map);
        assert_eq!(tokens.len(), 2);
        match &tokens[0].token_type {
            TokenType::Error => assert_eq!(
                tokens[0].error_message.as_ref().unwrap(),
                "Unexpected character: '@'."
            ),
            _ => panic!("Expected error token"),
        }
    }

    #[test]
    fn test_class_definition() {
        let source = "class MyClass { fn method() { return this.value; } }";
        let expected = vec![
            TokenType::Class,
            TokenType::Identifier,
            TokenType::LeftBrace,
            TokenType::Fn,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Return,
            TokenType::This,
            TokenType::Dot,
            TokenType::Identifier,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::RightBrace,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_loop_constructs() {
        let source = "for i in array { if condition { break; } else { continue; } }";
        let expected = vec![
            TokenType::For,
            TokenType::Identifier,
            TokenType::Identifier, // "in" is not a keyword in your tokenizer
            TokenType::Identifier,
            TokenType::LeftBrace,
            TokenType::If,
            TokenType::Identifier,
            TokenType::LeftBrace,
            TokenType::Break,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::Else,
            TokenType::LeftBrace,
            TokenType::Continue,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::RightBrace,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_arithmetic_expressions() {
        let source = "result = a + b - c * d / e % f";
        let expected = vec![
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Identifier,
            TokenType::Plus,
            TokenType::Identifier,
            TokenType::Minus,
            TokenType::Identifier,
            TokenType::Star,
            TokenType::Identifier,
            TokenType::Slash,
            TokenType::Identifier,
            TokenType::Modulo,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_pipe_operator() {
        let source = "foo() |> bar";
        let expected = vec![
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::Pipe,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_boolean_expressions() {
        let source = "result = !condition and (x != y or z == w)";
        let expected = vec![
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Bang,
            TokenType::Identifier,
            TokenType::And,
            TokenType::LeftParen,
            TokenType::Identifier,
            TokenType::BangEquals,
            TokenType::Identifier,
            TokenType::Or,
            TokenType::Identifier,
            TokenType::EqualsEquals,
            TokenType::Identifier,
            TokenType::RightParen,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_get_line_single_line() {
        let source_map = &SourceMap::new("hello world".to_string());
        let tokenizer = Tokenizer::new(source_map);
        tokenizer.for_each(drop);

        let line1: String = source_map.get_line(1).iter().collect();
        assert_eq!(line1, "hello world");

        assert_eq!(source_map.get_line(2).len(), 0);
        assert_eq!(source_map.get_line(0).len(), 0);
    }

    #[test]
    fn test_get_line_multiple_lines() {
        let source_map = &SourceMap::new("first line\nsecond line\nthird line".to_string());
        let tokenizer = Tokenizer::new(source_map);
        tokenizer.for_each(drop);
        let line1: String = source_map.get_line(1).iter().collect();
        let line2: String = source_map.get_line(2).iter().collect();
        let line3: String = source_map.get_line(3).iter().collect();

        assert_eq!(line1, "first line");
        assert_eq!(line2, "second line");
        assert_eq!(line3, "third line");
    }

    #[test]
    fn test_get_line_with_trailing_newline() {
        let source_map = &SourceMap::new("line one\nline two\n".to_string());
        let tokenizer = Tokenizer::new(source_map);
        tokenizer.for_each(drop);

        let line1: String = source_map.get_line(1).iter().collect();
        let line2: String = source_map.get_line(2).iter().collect();

        assert_eq!(line1, "line one");
        assert_eq!(line2, "line two");
        assert_eq!(source_map.get_line(3).len(), 0);
    }

    #[test]
    fn test_get_line_empty_lines() {
        let source_map = &SourceMap::new("first\n\nthird\n\n".to_string());
        let tokenizer = Tokenizer::new(source_map);
        tokenizer.for_each(drop);

        let line1: String = source_map.get_line(1).iter().collect();
        let line2: String = source_map.get_line(2).iter().collect();
        let line3: String = source_map.get_line(3).iter().collect();
        let line4: String = source_map.get_line(4).iter().collect();

        assert_eq!(line1, "first");
        assert_eq!(line2, "");
        assert_eq!(line3, "third");
        assert_eq!(line4, "");
    }

    #[test]
    fn test_get_line_single_newline() {
        let source_map = &SourceMap::new("\n".to_string());
        let tokenizer = Tokenizer::new(source_map);
        tokenizer.for_each(drop);

        let line1: String = source_map.get_line(1).iter().collect();
        assert_eq!(line1, "");
    }

    #[test]
    fn test_get_line_bounds_checking() {
        let source_map = &SourceMap::new("one\ntwo\nthree".to_string());
        let tokenizer = Tokenizer::new(source_map);
        tokenizer.for_each(drop);

        // Valid lines
        assert!(!source_map.get_line(1).is_empty());
        assert!(!source_map.get_line(2).is_empty());
        assert!(!source_map.get_line(3).is_empty());

        // Invalid lines
        assert!(source_map.get_line(0).is_empty());
        assert!(source_map.get_line(4).is_empty());
        assert!(source_map.get_line(100).is_empty());
    }

    #[test]
    fn test_get_line_with_complex_content() {
        let source_map = &SourceMap::new(
            "fn main() {\n    var x = \"hello world\";\n    // comment\n}".to_string(),
        );
        let tokenizer = Tokenizer::new(source_map);
        tokenizer.for_each(drop);

        let line1: String = source_map.get_line(1).iter().collect();
        let line2: String = source_map.get_line(2).iter().collect();
        let line3: String = source_map.get_line(3).iter().collect();
        let line4: String = source_map.get_line(4).iter().collect();

        assert_eq!(line1, "fn main() {");
        assert_eq!(line2, "    var x = \"hello world\";");
        assert_eq!(line3, "    // comment");
        assert_eq!(line4, "}");
    }

    #[test]
    fn test_get_line_error_reporting_use_case() {
        let source_map = &SourceMap::new("var x = 5;\nvar y = ;\nvar z = 10;".to_string());
        let tokenizer = Tokenizer::new(source_map);
        let tokens = tokenizer.collect::<Vec<Token>>();

        let error_token = tokens
            .iter()
            .find(|t| matches!(t.token_type, TokenType::Error));

        if let Some(token) = error_token {
            let error_line: String = source_map.get_line(token.line(source_map)).iter().collect();
            assert_eq!(error_line, "var y = ;");
            assert_eq!(token.line(source_map), 2);
        }
    }

    #[test]
    fn test_string_with_escaped_newline() {
        assert_single_token("\"hello\\nworld\"", TokenType::String);

        let source_map: &SourceMap = &SourceMap::new("\"hello\\nworld\"".to_string());
        let tokens = tokenize_all(source_map);
        assert_eq!((tokens[0].line(source_map)), 1);
    }

    #[test]
    fn test_string_with_various_escapes() {
        assert_single_token("\"hello\\tworld\\\"test\\\\path\"", TokenType::String);
    }

    #[test]
    fn test_multiline_code_with_string_escapes() {
        let source_map = &SourceMap::new("var msg = \"line 1\\nline 2\";\nvar x = 5;".to_string());
        let tokens = tokenize_all(source_map);

        assert_eq!(tokens[3].line(source_map), 1);
        assert_eq!(tokens[3].token_type, TokenType::String);
        assert_eq!(tokens[5].line(source_map), 2);
    }

    #[test]
    fn test_get_line_with_string_escapes() {
        let source_map = SourceMap::new("var msg = \"hello\\nworld\";\nvar x = 5;".to_string());

        let line1: String = source_map.get_line(1).iter().collect();
        let line2: String = source_map.get_line(2).iter().collect();

        assert_eq!(line1, "var msg = \"hello\\nworld\";");
        assert_eq!(line2, "var x = 5;");
    }

    #[test]
    fn test_invalid_escape_sequences() {
        let source_map = &SourceMap::new("\"hello\\qworld\"".to_string());
        let tokens = tokenize_all(source_map);

        println!("{:?}", tokens);
        assert!(matches!(tokens[0].token_type, TokenType::Error));
    }

    #[test]
    fn test_unterminated_multiline_comment() {
        let source_map = &SourceMap::new("/* unterminated comment".to_string());
        let tokens = tokenize_all(source_map);
        assert!(matches!(tokens[0].token_type, TokenType::Error));
    }

    #[test]
    fn test_nested_quotes_in_strings() {
        assert_single_token("\"He said \\\"hello\\\"\"", TokenType::String);
    }

    #[test]
    fn test_empty_string() {
        assert_single_token("\"\"", TokenType::String);
    }

    #[test]
    fn test_unicode_in_strings() {
        assert_single_token("\"café 🦀\"", TokenType::String);
    }

    #[test]
    fn test_decimal_point_numbers() {
        assert_single_token(".5", TokenType::Number);
        assert_single_token(".123", TokenType::Number);
    }

    #[test]
    fn test_optional_chaining_operator() {
        // First, you'll need to add this to your TokenType enum:
        // OptionalChaining,    // .?

        assert_single_token(".?", TokenType::OptionalChaining);
    }

    #[test]
    fn test_optional_chaining_in_expressions() {
        let source = "user.?name";
        let expected = vec![
            TokenType::Identifier,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_chained_optional_access() {
        let source = "user.?profile.?avatar.?url";
        let expected = vec![
            TokenType::Identifier,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_throw_try_catch_finally() {
        let source = "try { \n throw Error(); \n } catch (err) { \n println(err); \n } finally { \n println(\"moving along\"); }";
        let expected = vec![
            TokenType::Try,
            TokenType::LeftBrace,
            TokenType::Throw,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::Catch,
            TokenType::LeftParen,
            TokenType::Identifier,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::Identifier,
            TokenType::RightParen,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::Finally,
            TokenType::LeftBrace,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::String,
            TokenType::RightParen,
            TokenType::Semicolon,
            TokenType::RightBrace,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_mixed_optional_and_regular_chaining() {
        let source = "user.name.?nickname";
        let expected = vec![
            TokenType::Identifier,
            TokenType::Dot,
            TokenType::Identifier,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_optional_chaining_with_method_calls() {
        let source = "user.?getName()";
        let expected = vec![
            TokenType::Identifier,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_optional_chaining_with_array_access() {
        let source = "users.?[0].?name";
        let expected = vec![
            TokenType::Identifier,
            TokenType::OptionalChaining,
            TokenType::LeftSquareBracket,
            TokenType::Number,
            TokenType::RightSquareBracket,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_distinguish_dot_question_from_optional_chaining() {
        // Make sure ". ?" (with space) is different from ".?"
        let source = "condition ? user.name : nil";
        let expected = vec![
            TokenType::Identifier,
            TokenType::Question,
            TokenType::Identifier,
            TokenType::Dot,
            TokenType::Identifier,
            TokenType::Colon,
            TokenType::Nil,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_optional_chaining_in_complex_expression() {
        let source = "var result = api.?getData().?result.?value or \"default\";";
        let expected = vec![
            TokenType::Var,
            TokenType::Identifier,
            TokenType::Equals,
            TokenType::Identifier,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::OptionalChaining,
            TokenType::Identifier,
            TokenType::Or,
            TokenType::String,
            TokenType::Semicolon,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_peek_ahead_basic() {
        let source_map = SourceMap::new("hello world 123".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        // Test peek_ahead without consuming
        assert_eq!(
            tokenizer.peek_ahead(0).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer.peek_ahead(1).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer.peek_ahead(2).unwrap().token_type,
            TokenType::Number
        );
        assert_eq!(tokenizer.peek_ahead(3).unwrap().token_type, TokenType::Eof);

        // Verify we haven't consumed anything
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Identifier);
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Identifier);
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Number);
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Eof);
    }

    #[test]
    fn test_peek_ahead_out_of_bounds() {
        let source_map = SourceMap::new("x".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        // Test peeking beyond available tokens
        assert_eq!(
            tokenizer.peek_ahead(0).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(tokenizer.peek_ahead(1).unwrap().token_type, TokenType::Eof);
        assert!(tokenizer.peek_ahead(2).is_none());
        assert!(tokenizer.peek_ahead(10).is_none());
    }

    #[test]
    fn test_peek_convenience_methods() {
        let source_map = SourceMap::new("a + b".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        // Test peek() and peek_next() convenience methods
        assert_eq!(tokenizer.peek().unwrap().token_type, TokenType::Identifier);
        assert_eq!(tokenizer.peek_next().unwrap().token_type, TokenType::Plus);

        // Verify they're equivalent to peek_ahead
        assert_eq!(
            tokenizer.peek().unwrap().token_type.clone(),
            tokenizer.peek_ahead(0).unwrap().token_type
        );
        assert_eq!(
            tokenizer.peek_next().unwrap().token_type.clone(),
            tokenizer.peek_ahead(1).unwrap().token_type
        );
    }

    #[test]
    fn test_mixed_peek_and_consume() {
        let source_map = SourceMap::new("var x = 5;".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        // Peek ahead
        assert_eq!(tokenizer.peek_ahead(0).unwrap().token_type, TokenType::Var);
        assert_eq!(
            tokenizer.peek_ahead(2).unwrap().token_type,
            TokenType::Equals
        );

        // Consume one token
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Var);

        // Peek should now show next tokens
        assert_eq!(tokenizer.peek().unwrap().token_type, TokenType::Identifier);
        assert_eq!(
            tokenizer.peek_ahead(1).unwrap().token_type,
            TokenType::Equals
        );

        // Consume the rest
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Identifier);
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Equals);
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Number);
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Semicolon);
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Eof);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn test_iterator_consistency_with_buffer() {
        let source_map = SourceMap::new("fn test() { return 42; }".to_string());
        let tokenizer = Tokenizer::new(&source_map);

        // Collect tokens using iterator
        let tokens_via_iterator: Vec<Token> = tokenizer.collect();

        // Reset and collect using peek_ahead
        let mut tokenizer2 = Tokenizer::new(&source_map);
        let mut tokens_via_peek = Vec::new();
        let mut i = 0;
        while let Some(token) = tokenizer2.peek_ahead(i) {
            tokens_via_peek.push(token.clone());
            i += 1;
        }

        // Should be identical
        assert_eq!(tokens_via_iterator.len(), tokens_via_peek.len());
        for (iter_token, peek_token) in tokens_via_iterator.iter().zip(tokens_via_peek.iter()) {
            assert_eq!(iter_token.token_type, peek_token.token_type);
            assert_eq!(iter_token.start, peek_token.start);
            assert_eq!(iter_token.end, peek_token.end);
        }
    }

    #[test]
    fn test_lambda_detection_patterns() {
        // Test empty lambda: () ->
        let source_map = SourceMap::new("() -> 42".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        assert_eq!(
            tokenizer.peek_ahead(0).unwrap().token_type,
            TokenType::LeftParen
        );
        assert_eq!(
            tokenizer.peek_ahead(1).unwrap().token_type,
            TokenType::RightParen
        );
        assert_eq!(
            tokenizer.peek_ahead(2).unwrap().token_type,
            TokenType::Arrow
        );
        assert_eq!(
            tokenizer.peek_ahead(3).unwrap().token_type,
            TokenType::Number
        );

        // Test single param lambda: (x) ->
        let source_map2 = SourceMap::new("(x) -> x + 1".to_string());
        let mut tokenizer2 = Tokenizer::new(&source_map2);

        assert_eq!(
            tokenizer2.peek_ahead(0).unwrap().token_type,
            TokenType::LeftParen
        );
        assert_eq!(
            tokenizer2.peek_ahead(1).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer2.peek_ahead(2).unwrap().token_type,
            TokenType::RightParen
        );
        assert_eq!(
            tokenizer2.peek_ahead(3).unwrap().token_type,
            TokenType::Arrow
        );

        // Test multi param lambda: (a, b) ->
        let source_map3 = SourceMap::new("(a, b) -> a + b".to_string());
        let mut tokenizer3 = Tokenizer::new(&source_map3);

        assert_eq!(
            tokenizer3.peek_ahead(0).unwrap().token_type,
            TokenType::LeftParen
        );
        assert_eq!(
            tokenizer3.peek_ahead(1).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer3.peek_ahead(2).unwrap().token_type,
            TokenType::Comma
        );
        assert_eq!(
            tokenizer3.peek_ahead(3).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer3.peek_ahead(4).unwrap().token_type,
            TokenType::RightParen
        );
        assert_eq!(
            tokenizer3.peek_ahead(5).unwrap().token_type,
            TokenType::Arrow
        );
    }

    #[test]
    fn test_distinguish_lambda_from_grouping() {
        // Test grouping: (x + y)
        let source_map = SourceMap::new("(x + y)".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        assert_eq!(
            tokenizer.peek_ahead(0).unwrap().token_type,
            TokenType::LeftParen
        );
        assert_eq!(
            tokenizer.peek_ahead(1).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(tokenizer.peek_ahead(2).unwrap().token_type, TokenType::Plus);
        assert_eq!(
            tokenizer.peek_ahead(3).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer.peek_ahead(4).unwrap().token_type,
            TokenType::RightParen
        );
        // No arrow after closing paren
        assert_eq!(tokenizer.peek_ahead(5).unwrap().token_type, TokenType::Eof);

        // Test method call: func(x, y)
        let source_map2 = SourceMap::new("func(x, y)".to_string());
        let mut tokenizer2 = Tokenizer::new(&source_map2);

        assert_eq!(
            tokenizer2.peek_ahead(0).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer2.peek_ahead(1).unwrap().token_type,
            TokenType::LeftParen
        );
        assert_eq!(
            tokenizer2.peek_ahead(2).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer2.peek_ahead(3).unwrap().token_type,
            TokenType::Comma
        );
        assert_eq!(
            tokenizer2.peek_ahead(4).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer2.peek_ahead(5).unwrap().token_type,
            TokenType::RightParen
        );
        // No arrow after closing paren
        assert_eq!(tokenizer2.peek_ahead(6).unwrap().token_type, TokenType::Eof);
    }

    #[test]
    fn test_peek_ahead_with_complex_tokens() {
        let source_map = SourceMap::new("\"hello world\" >= 123.45 != true".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        assert_eq!(
            tokenizer.peek_ahead(0).unwrap().token_type,
            TokenType::String
        );
        assert_eq!(
            tokenizer.peek_ahead(1).unwrap().token_type,
            TokenType::GreaterEquals
        );
        assert_eq!(
            tokenizer.peek_ahead(2).unwrap().token_type,
            TokenType::Number
        );
        assert_eq!(
            tokenizer.peek_ahead(3).unwrap().token_type,
            TokenType::BangEquals
        );
        assert_eq!(tokenizer.peek_ahead(4).unwrap().token_type, TokenType::True);
        assert_eq!(tokenizer.peek_ahead(5).unwrap().token_type, TokenType::Eof);

        // Verify lexemes are correct
        assert_eq!(
            tokenizer.peek_ahead(0).unwrap().lexeme(&source_map),
            Box::<str>::from("\"hello world\"")
        );
        assert_eq!(
            tokenizer.peek_ahead(2).unwrap().lexeme(&source_map),
            Box::<str>::from("123.45")
        );
    }

    #[test]
    fn test_peek_ahead_with_comments() {
        let source_map = SourceMap::new("var x // comment\n= 5;".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        assert_eq!(tokenizer.peek_ahead(0).unwrap().token_type, TokenType::Var);
        assert_eq!(
            tokenizer.peek_ahead(1).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer.peek_ahead(2).unwrap().token_type,
            TokenType::Comment
        );
        assert_eq!(
            tokenizer.peek_ahead(3).unwrap().token_type,
            TokenType::Equals
        );
        assert_eq!(
            tokenizer.peek_ahead(4).unwrap().token_type,
            TokenType::Number
        );
        assert_eq!(
            tokenizer.peek_ahead(5).unwrap().token_type,
            TokenType::Semicolon
        );
    }

    #[test]
    fn test_empty_source_peek_ahead() {
        let source_map = SourceMap::new("".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        assert_eq!(tokenizer.peek_ahead(0).unwrap().token_type, TokenType::Eof);
        assert!(tokenizer.peek_ahead(1).is_none());

        // Iterator should also work correctly
        assert_eq!(tokenizer.next().unwrap().token_type, TokenType::Eof);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn test_peek_ahead_buffer_reuse() {
        let source_map = SourceMap::new("a b c d e".to_string());
        let mut tokenizer = Tokenizer::new(&source_map);

        // Peek far ahead multiple times
        assert_eq!(
            tokenizer.peek_ahead(4).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer.peek_ahead(2).unwrap().token_type,
            TokenType::Identifier
        );
        assert_eq!(
            tokenizer.peek_ahead(4).unwrap().token_type,
            TokenType::Identifier
        );

        // Consume some tokens
        tokenizer.next(); // consume 'a'
        tokenizer.next(); // consume 'b'

        // Peek should still work correctly
        assert_eq!(
            tokenizer.peek_ahead(0).unwrap().token_type,
            TokenType::Identifier
        ); // 'c'
        assert_eq!(
            tokenizer.peek_ahead(1).unwrap().token_type,
            TokenType::Identifier
        ); // 'd'
        assert_eq!(
            tokenizer.peek_ahead(2).unwrap().token_type,
            TokenType::Identifier
        ); // 'e'
    }
}
