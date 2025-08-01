use qanglang::{SourceMap, Token, TokenType, Tokenizer};

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

/// Asserts that the token produced from tokenizing the source string match the expected types.
fn assert_single_token(source: &str, expected: Token) {
    assert_tokens(
        source,
        &[
            expected,
            Token {
                token_type: TokenType::Eof,
                start: source.len(),
                end: source.len(),
                error_message: None,
            },
        ],
    );
}

/// Asserts that the tokens produced from tokenizing the source string match the expected types.
fn assert_tokens(source: &str, expected: &[Token]) {
    let source_map = &SourceMap::new(source.to_string());
    let tokens = tokenize_all(source_map);
    assert_eq!(&tokens, expected);
}

/// Asserts that a single token of the expected type is produced from tokenizing the source string,
/// followed by an EOF token.
fn assert_single_token_type(source: &str, expected: TokenType) {
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
    assert_single_token_type("(", TokenType::LeftParen);
    assert_single_token_type(")", TokenType::RightParen);
    assert_single_token_type("{", TokenType::LeftBrace);
    assert_single_token_type("}", TokenType::RightBrace);
    assert_single_token_type("[", TokenType::LeftSquareBracket);
    assert_single_token_type("]", TokenType::RightSquareBracket);
    assert_single_token_type(",", TokenType::Comma);
    assert_single_token_type(".", TokenType::Dot);
    assert_single_token_type(":", TokenType::Colon);
    assert_single_token_type("?", TokenType::Question);
    assert_single_token_type("+", TokenType::Plus);
    assert_single_token_type("-", TokenType::Minus);
    assert_single_token_type("*", TokenType::Star);
    assert_single_token_type("/", TokenType::Slash);
    assert_single_token_type("%", TokenType::Modulo);
    assert_single_token_type(";", TokenType::Semicolon);
    assert_single_token_type("=", TokenType::Equals);
    assert_single_token_type("!", TokenType::Bang);
    assert_single_token_type("<", TokenType::Less);
    assert_single_token_type(">", TokenType::Greater);
    assert_single_token_type(".?", TokenType::OptionalChaining);
}

#[test]
fn test_two_character_tokens() {
    assert_single_token_type("!=", TokenType::BangEquals);
    assert_single_token_type("<=", TokenType::LessEquals);
    assert_single_token_type(">=", TokenType::GreaterEquals);
    assert_single_token_type("->", TokenType::Arrow);
}

#[test]
fn test_keywords() {
    assert_single_token_type("and", TokenType::And);
    assert_single_token_type("class", TokenType::Class);
    assert_single_token_type("else", TokenType::Else);
    assert_single_token_type("false", TokenType::False);
    assert_single_token_type("for", TokenType::For);
    assert_single_token_type("fn", TokenType::Fn);
    assert_single_token_type("if", TokenType::If);
    assert_single_token_type("nil", TokenType::Nil);
    assert_single_token_type("or", TokenType::Or);
    assert_single_token_type("return", TokenType::Return);
    assert_single_token_type("super", TokenType::Super);
    assert_single_token_type("this", TokenType::This);
    assert_single_token_type("true", TokenType::True);
    assert_single_token_type("var", TokenType::Var);
    assert_single_token_type("while", TokenType::While);
    assert_single_token_type("break", TokenType::Break);
    assert_single_token_type("continue", TokenType::Continue);
}

#[test]
fn test_identifiers() {
    assert_single_token_type("hello", TokenType::Identifier);
    assert_single_token_type("_underscore", TokenType::Identifier);
    assert_single_token_type("with123numbers", TokenType::Identifier);
    assert_single_token_type("_", TokenType::Identifier);
    assert_single_token_type("CamelCase", TokenType::Identifier);
}

#[test]
fn test_numbers() {
    assert_single_token_type("123", TokenType::Number);
    assert_single_token_type("0", TokenType::Number);
    assert_single_token_type("123.456", TokenType::Number);
    assert_single_token_type("0.5", TokenType::Number);
    assert_single_token_type("999.0", TokenType::Number);
}

#[test]
fn test_strings() {
    assert_single_token(
        "\"hello\"",
        Token {
            start: 0,
            end: 7,
            error_message: None,
            token_type: TokenType::String,
        },
    );
    assert_single_token(
        "\"\"",
        Token {
            start: 0,
            end: 2,
            error_message: None,
            token_type: TokenType::String,
        },
    );
    assert_single_token(
        "\"hello world\"",
        Token {
            start: 0,
            end: 13,
            error_message: None,
            token_type: TokenType::String,
        },
    );
    assert_single_token(
        "\"with spaces and 123 numbers\"",
        Token {
            start: 0,
            end: 29,
            error_message: None,
            token_type: TokenType::String,
        },
    );
}

#[test]
fn test_string_with_various_escapes() {
    assert_single_token_type("\"hello\\tworld\\\"test\\\\path\"", TokenType::String);
}

#[test]
fn test_comments() {
    let source1 = "var x = 5; // variable declaration\n/* block comment */ y = 10;";
    let expected1 = vec![
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
    assert_token_types(source1, &expected1);

    let source2 = "var x = /* block comment */ 5;";
    let expected2 = vec![
        TokenType::Var,
        TokenType::Identifier,
        TokenType::Equals,
        TokenType::Comment,
        TokenType::Number,
        TokenType::Semicolon,
        TokenType::Eof,
    ];
    assert_token_types(source2, &expected2);
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
fn test_decimal_point_numbers() {
    assert_single_token_type(".5", TokenType::Number);
    assert_single_token_type(".123", TokenType::Number);
}

#[test]
fn test_optional_chaining() {
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
fn test_unterminated_string_with_following_code() {
    let source_map = &SourceMap::new("\"unterminated\nvar x = 5;".to_string());
    let mut tokenizer = Tokenizer::new(source_map);

    // First token should be error for unterminated string
    let first_token = tokenizer.next().unwrap();
    assert_eq!(first_token.token_type, TokenType::Error);

    // Should continue parsing after the error
    let tokens: Vec<Token> = tokenizer.collect();
    println!("tokens.len(): {:?}", tokens.len());
    // Should have: var, x, =, 5, ;, EOF
    assert!(tokens.len() >= 5);
    assert_eq!(tokens[0].token_type, TokenType::Var);
    assert_eq!(tokens[1].token_type, TokenType::Identifier);
    assert_eq!(tokens[2].token_type, TokenType::Equals);
    assert_eq!(tokens[3].token_type, TokenType::Number);
    assert_eq!(tokens[4].token_type, TokenType::Semicolon);
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
fn test_malformed_numbers() {
    // verify tokens have an error message
    // TODO: Test malformed numbers like 123.456.789, 123., .123.456
}

#[test]
fn test_invalid_characters_in_different_contexts() {
    // verify tokens have an error message
    // TODO: Test @ # $ % in various positions
}

#[test]
fn test_nested_unterminated_constructs() {
    // verify tokens have an error message
    // TODO: Test nested unterminated strings, comments, etc.
}

#[test]
fn test_invalid_escape_sequences() {
    let source_map = &SourceMap::new("\"hello\\qworld\"".to_string());
    let tokens = tokenize_all(source_map);

    println!("{:?}", tokens);
    // TODO verify error message is correct
    assert!(matches!(tokens[0].token_type, TokenType::Error));
}

#[test]
fn test_unterminated_multiline_comment() {
    let source_map = &SourceMap::new("/* unterminated comment".to_string());
    let tokens = tokenize_all(source_map);
    assert!(matches!(tokens[0].token_type, TokenType::Error));
}

#[test]
fn test_number_lexeme_content() {
    // TODO: Verify lexeme content for integers, floats, decimals,
}

#[test]
fn test_string_lexeme_content() {
    // TODO: Verify lexeme content for strings with escapes, unicode, etc, verifying the lexeme is correctly extracted from the source.
    assert_single_token_type("\"café 🦀\"", TokenType::String);
}

#[test]
fn test_identifier_lexem_contentt() {
    // TODO: Verify lexeme content for various identifier patterns
}

#[test]
fn test_operator_lexemw_content() {
    // TODO: Verify lexeme content for single and multi-character operators
}

#[test]
fn test_keyword_lexeme_token_contentt() {
    // TODO: Verify lexeme content for all keywords
}

#[test]
fn test_comment_token_content() {
    // TODO: Verify lexeme content for single-line and multi-line comments
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
