use phf::phf_map;

#[derive(PartialEq, Clone, Debug)]
pub enum TokenType {
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
};

#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
    pub col: u32,
    pub lexeme: String,
}

pub struct SourceMap {
    source: Vec<char>,
    line_indices: Vec<usize>,
}

impl SourceMap {
    /// Creates a new source map from source code.
    pub fn new(source: String) -> Self {
        let chars: Vec<char> = source.chars().collect();
        let mut line_indices = Vec::new();
        let mut in_string = false;
        let mut i = 0;

        while i < chars.len() {
            match chars[i] {
                '"' if !in_string => {
                    in_string = true;
                }
                '"' if in_string => {
                    in_string = false;
                }
                '\\' if in_string => {
                    // Skip escape sequence
                    i += 1; // Skip the backslash
                    if i < chars.len() {
                        i += 1; // Skip the escaped character
                        continue;
                    }
                }
                '\n' if !in_string => {
                    line_indices.push(i);
                }
                _ => {}
            }
            i += 1;
        }

        Self {
            source: chars,
            line_indices,
        }
    }

    /// Returns the source code as a vector of characters.
    pub fn get_source(&self) -> &Vec<char> {
        &self.source
    }

    /// Returns a specified line of source code as a vector of characters.
    pub fn get_line(&self, line_number: u32) -> &[char] {
        if line_number == 0 {
            return &[];
        }

        let line_index = (line_number - 1) as usize;

        let start = if line_number == 1 {
            0
        } else {
            // Start after the previous newline
            if let Some(&prev_newline_pos) = self.line_indices.get(line_index - 1) {
                prev_newline_pos + 1
            } else {
                return &[];
            }
        };

        let end = if let Some(&newline_pos) = self.line_indices.get(line_index) {
            newline_pos // Stop at the newline (don't include it)
        } else if line_index == self.line_indices.len() {
            // Last line (no trailing newline)
            self.source.len()
        } else {
            // Line doesn't exist
            return &[];
        };

        if start <= end && end <= self.source.len() {
            &self.source[start..end]
        } else {
            &[]
        }
    }
}

pub struct Tokenizer<'a> {
    source_map: &'a SourceMap,
    line: u32,
    col: u32,
    location: usize,
    is_eof: bool,
}

impl<'a> Tokenizer<'a> {
    // Creates a new tokenizer for the source map.
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            line: 1,
            col: 1,
            location: 0,
            is_eof: false,
        }
    }

    /// Advances the cursor.
    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        let c = self.source_map.source[self.location];
        self.location += 1;

        if c == '\n' {
            self.line += 1;
            self.col = 1;
        }

        c
    }

    // Peeks at the next character.
    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source_map.source[self.location]
        }
    }

    /// Peeks at the character after the next.
    fn peek_next(&self) -> char {
        if self.location + 1 >= self.source_map.source.len() {
            '\0'
        } else {
            self.source_map.source[self.location + 1]
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
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.is_eof || self.location >= self.source_map.source.len()
    }

    /// Peeks at the next character. If it matches, advances the cursor.
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.location += expected.len_utf8();
            // self.advance();
            true
        }
    }

    fn make_token(&mut self, token_type: TokenType, start: usize) -> Option<Token> {
        let lexeme: String = self.source_map.source[start..self.location]
            .iter()
            .collect();
        let col = self.col;
        self.col += lexeme.len() as u32;

        Some(Token {
            token_type,
            line: self.line,
            col,
            lexeme,
        })
    }

    fn error_token(&self, message: &str) -> Option<Token> {
        Some(Token {
            token_type: TokenType::Error(message.to_string()),
            line: self.line,
            col: self.col,
            lexeme: "".to_string(),
        })
    }

    fn string(&mut self) -> Option<Token> {
        let start = self.location - 1;

        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\\' {
                // Handle escape sequence
                self.advance_in_string(); // consume backslash
                if !self.is_at_end() {
                    self.advance_in_string(); // consume escaped character
                }
            } else {
                self.advance_in_string();
            }
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        self.advance(); // Consume closing quote (this one can affect line tracking)
        self.make_token(TokenType::String, start)
    }

    /// Advances cursor without treating characters as line breaks (for use in strings)
    fn advance_in_string(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        let c = self.source_map.source[self.location];
        self.location += 1;

        // Don't increment line/col for characters inside strings
        // The column tracking will be handled in make_token

        c
    }

    fn number(&mut self) -> Option<Token> {
        let start = self.location - 1; // Account for first digit

        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Look for fractional part
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance(); // Consume the '.'

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.make_token(TokenType::Number, start)
    }

    fn identifier(&mut self) -> Option<Token> {
        let start = self.location - 1;

        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let keyword: String = self.source_map.source[start..self.location]
            .iter()
            .collect();
        let token_type = TokenType::from_keyword(&keyword);

        self.make_token(token_type, start)
    }

    fn single_line_comment(&mut self) -> Option<Token> {
        let start = self.location - 2;

        while self.peek() != '\n' && !self.is_at_end() {
            self.advance();
        }

        self.make_token(TokenType::Comment, start)
    }

    fn multi_line_commment(&mut self) -> Option<Token> {
        let start = self.location - 2;

        while !self.is_at_end() {
            if self.peek() == '*' && self.peek_next() == '/' {
                self.advance(); // consume *
                self.advance(); // consume /
                break;
            }
            self.advance();
        }

        self.make_token(TokenType::Comment, start)
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
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
            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenType::BangEquals, start)
                } else {
                    self.make_token(TokenType::Bang, start)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.make_token(TokenType::LessEquals, start)
                } else {
                    self.make_token(TokenType::Less, start)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.make_token(TokenType::GreaterEquals, start)
                } else {
                    self.make_token(TokenType::Greater, start)
                }
            }
            '=' => self.make_token(TokenType::Equals, start),
            '-' => {
                if self.match_char('>') {
                    self.make_token(TokenType::Arrow, start)
                } else {
                    self.make_token(TokenType::Minus, start)
                }
            }
            '*' => self.make_token(TokenType::Star, start),
            '+' => self.make_token(TokenType::Plus, start),
            '/' => {
                if self.match_char('/') {
                    self.single_line_comment()
                } else if self.match_char('*') {
                    self.multi_line_commment()
                } else {
                    self.make_token(TokenType::Slash, start)
                }
            }
            '%' => self.make_token(TokenType::Modulo, start),
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
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier(),
            _ => self.error_token("Unexpected character."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize_all(source: &str) -> Vec<Token> {
        let mut source_map = SourceMap::new(source.to_string());
        let tokenizer = Tokenizer::new(&mut source_map);
        tokenizer.collect()
    }

    fn assert_token_types(source: &str, expected: &[TokenType]) {
        let tokens = tokenize_all(source);
        let actual: Vec<TokenType> = tokens.into_iter().map(|t| t.token_type).collect();
        assert_eq!(actual, expected);
    }

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
        let tokens = tokenize_all("\"unterminated");
        assert_eq!(tokens.len(), 2);
        match &tokens[0].token_type {
            TokenType::Error(msg) => assert_eq!(msg, "Unterminated string."),
            _ => panic!("Expected error token"),
        }
    }

    #[test]
    fn test_single_line_comments() {
        assert_single_token("// this is a comment", TokenType::Comment);
        assert_single_token("//no space", TokenType::Comment);

        // Comment followed by newline should only tokenize the comment
        let tokens = tokenize_all("// comment\n");
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
        let source = "hello world 123";
        let tokens = tokenize_all(source);

        assert_eq!(tokens[0].lexeme, "hello");
        assert_eq!(tokens[1].lexeme, "world");
        assert_eq!(tokens[2].lexeme, "123");
    }

    #[test]
    fn test_line_and_column_tracking() {
        let source = "first\nsecond line";
        let tokens = tokenize_all(source);

        assert_eq!(tokens[0].line, 1);
        assert_eq!(tokens[0].col, 1);
        assert_eq!(tokens[1].line, 2);
        assert_eq!(tokens[1].col, 1);
        assert_eq!(tokens[2].line, 2);
        assert_eq!(tokens[2].col, 8);
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
        let tokens = tokenize_all("@");
        assert_eq!(tokens.len(), 2);
        match &tokens[0].token_type {
            TokenType::Error(msg) => assert_eq!(msg, "Unexpected character."),
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
            TokenType::Equals, // Note: you don't have == as a separate token
            TokenType::Equals,
            TokenType::Identifier,
            TokenType::RightParen,
            TokenType::Eof,
        ];
        assert_token_types(source, &expected);
    }

    #[test]
    fn test_get_line_single_line() {
        let mut source_map = SourceMap::new("hello world".to_string());
        let tokenizer = Tokenizer::new(&mut source_map);
        tokenizer.for_each(drop);

        let line1: String = source_map.get_line(1).iter().collect();
        assert_eq!(line1, "hello world");

        // Non-existent lines should return empty
        assert_eq!(source_map.get_line(2).len(), 0);
        assert_eq!(source_map.get_line(0).len(), 0);
    }

    #[test]
    fn test_get_line_multiple_lines() {
        let mut source_map = SourceMap::new("first line\nsecond line\nthird line".to_string());
        let tokenizer = Tokenizer::new(&mut source_map);
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
        let mut source_map = SourceMap::new("line one\nline two\n".to_string());
        let tokenizer = Tokenizer::new(&mut source_map);
        tokenizer.for_each(drop);

        let line1: String = source_map.get_line(1).iter().collect();
        let line2: String = source_map.get_line(2).iter().collect();

        assert_eq!(line1, "line one");
        assert_eq!(line2, "line two");

        // Should not have a third line
        assert_eq!(source_map.get_line(3).len(), 0);
    }

    #[test]
    fn test_get_line_empty_lines() {
        let mut source_map = SourceMap::new("first\n\nthird\n\n".to_string());
        let tokenizer = Tokenizer::new(&mut source_map);
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
        let mut source_map = SourceMap::new("\n".to_string());
        let tokenizer = Tokenizer::new(&mut source_map);
        tokenizer.for_each(drop);

        let line1: String = source_map.get_line(1).iter().collect();
        assert_eq!(line1, "");
    }

    #[test]
    fn test_get_line_bounds_checking() {
        let mut source_map = SourceMap::new("one\ntwo\nthree".to_string());
        let tokenizer = Tokenizer::new(&mut source_map);
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
        let mut source_map = SourceMap::new(
            "fn main() {\n    var x = \"hello world\";\n    // comment\n}".to_string(),
        );
        let tokenizer = Tokenizer::new(&mut source_map);
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
        // This test demonstrates how get_line would be used for error reporting
        let mut source_map = SourceMap::new("var x = 5;\nvar y = ;\nvar z = 10;".to_string());
        let tokenizer = Tokenizer::new(&mut source_map);
        let tokens = tokenizer.collect::<Vec<Token>>();

        // Find an error token (there should be one on line 2)
        let error_token = tokens
            .iter()
            .find(|t| matches!(t.token_type, TokenType::Error(_)));

        if let Some(token) = error_token {
            let error_line: String = source_map.get_line(token.line).iter().collect();
            assert_eq!(error_line, "var y = ;");
            assert_eq!(token.line, 2);
        }
    }

    #[test]
    fn test_string_with_escaped_newline() {
        assert_single_token("\"hello\\nworld\"", TokenType::String);

        // The token should be on line 1, not line 2
        let tokens = tokenize_all("\"hello\\nworld\"");
        assert_eq!(tokens[0].line, 1);
    }

    #[test]
    fn test_string_with_various_escapes() {
        assert_single_token("\"hello\\tworld\\\"test\\\\path\"", TokenType::String);
    }

    #[test]
    fn test_multiline_code_with_string_escapes() {
        let source = "var msg = \"line 1\\nline 2\";\nvar x = 5;";
        let tokens = tokenize_all(source);

        // The string should be on line 1
        assert_eq!(tokens[3].line, 1); // The string token
        assert_eq!(tokens[3].token_type, TokenType::String);

        // The var x should be on line 2
        assert_eq!(tokens[5].line, 2); // The second 'var' token
    }

    #[test]
    fn test_get_line_with_string_escapes() {
        let source_map = SourceMap::new("var msg = \"hello\\nworld\";\nvar x = 5;".to_string());

        let line1: String = source_map.get_line(1).iter().collect();
        let line2: String = source_map.get_line(2).iter().collect();

        assert_eq!(line1, "var msg = \"hello\\nworld\";");
        assert_eq!(line2, "var x = 5;");
    }
}
