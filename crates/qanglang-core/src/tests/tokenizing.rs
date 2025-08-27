use crate::{SourceMap, Token, TokenType, Tokenizer};

/// Tokenizes all source code in the given SourceMap and returns a vector of Tokens.
fn tokenize_all(source_map: &SourceMap) -> Vec<Token> {
    let tokenizer = Tokenizer::new(source_map);
    tokenizer.collect()
}

/// Asserts that the token types produced from tokenizing the source string match the expected types.
fn assert_token_types(source: &str, expected: &[TokenType]) {
    let source_map = SourceMap::new(source.to_string());
    let tokens = tokenize_all(&source_map);
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
    let source_map = SourceMap::new(source.to_string());
    let tokens = tokenize_all(&source_map);
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
    let source = "for (var = i; i < arr_len(array); i = i + 1) { if condition { break; } else { continue; } }";
    let expected = vec![
        TokenType::For,
        TokenType::LeftParen,
        TokenType::Var,
        TokenType::Equals,
        TokenType::Identifier,
        TokenType::Semicolon,
        TokenType::Identifier,
        TokenType::Less,
        TokenType::Identifier,
        TokenType::LeftParen,
        TokenType::Identifier,
        TokenType::RightParen,
        TokenType::Semicolon,
        TokenType::Identifier,
        TokenType::Equals,
        TokenType::Identifier,
        TokenType::Plus,
        TokenType::Number,
        TokenType::RightParen,
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
    let source_map = SourceMap::new("\"unterminated".to_string());
    let tokens = tokenize_all(&source_map);
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
    let source_map = SourceMap::new("\"unterminated\nvar x = 5;".to_string());
    let mut tokenizer = Tokenizer::new(&source_map);

    let first_token = tokenizer.next().unwrap();
    assert_eq!(first_token.token_type, TokenType::Error);

    let tokens: Vec<Token> = tokenizer.collect();
    println!("tokens.len(): {:?}", tokens.len());
    assert!(tokens.len() >= 5);
    assert_eq!(tokens[0].token_type, TokenType::Var);
    assert_eq!(tokens[1].token_type, TokenType::Identifier);
    assert_eq!(tokens[2].token_type, TokenType::Equals);
    assert_eq!(tokens[3].token_type, TokenType::Number);
    assert_eq!(tokens[4].token_type, TokenType::Semicolon);
}

#[test]
fn test_unexpected_character() {
    let source_map = SourceMap::new("@".to_string());
    let tokens = tokenize_all(&source_map);
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
    let source1 = "123.456.789";
    let expected1 = vec![TokenType::Number, TokenType::Number, TokenType::Eof];
    assert_token_types(source1, &expected1);

    let source2 = "123.";
    let expected2 = vec![
        TokenType::Number, // 123
        TokenType::Dot,    // .
        TokenType::Eof,
    ];
    assert_token_types(source2, &expected2);

    let source3 = "123..";
    let expected3 = vec![
        TokenType::Number, // 123
        TokenType::Dot,    // .
        TokenType::Dot,    // .
        TokenType::Eof,
    ];
    assert_token_types(source3, &expected3);

    let source4 = ".123.456";
    let expected4 = vec![
        TokenType::Number, // .123
        TokenType::Number, // .456
        TokenType::Eof,
    ];
    println!("{:?}", tokenize_all(&SourceMap::new(source4.to_owned())));
    assert_token_types(source4, &expected4); // <- This assertion fails.

    let source5 = "..123";
    let expected5 = vec![
        TokenType::Dot,    // .
        TokenType::Number, // .123
        TokenType::Eof,
    ];
    assert_token_types(source5, &expected5);

    let source6 = "var x = 123.45.67;";
    let expected6 = vec![
        TokenType::Var,
        TokenType::Identifier, // x
        TokenType::Equals,
        TokenType::Number, // 123.45
        TokenType::Number, // .67
        TokenType::Semicolon,
        TokenType::Eof,
    ];
    assert_token_types(source6, &expected6);
}

#[test]
fn test_invalid_characters_in_different_contexts() {
    let source_map1 = SourceMap::new("@variable".to_string());
    let tokens1 = tokenize_all(&source_map1);
    assert_eq!(tokens1[0].token_type, TokenType::Error);
    assert_eq!(
        tokens1[0].error_message.as_ref().unwrap(),
        "Unexpected character: '@'."
    );

    let source_map2 = SourceMap::new("var x = #123".to_string());
    let tokens2 = tokenize_all(&source_map2);
    assert_eq!(tokens2[3].token_type, TokenType::Error);
    assert_eq!(
        tokens2[3].error_message.as_ref().unwrap(),
        "Unexpected character: '#'."
    );

    let source_map3 = SourceMap::new("func($arg)".to_string());
    let tokens3 = tokenize_all(&source_map3);
    assert_eq!(tokens3[2].token_type, TokenType::Error);
    assert_eq!(
        tokens3[2].error_message.as_ref().unwrap(),
        "Unexpected character: '$'."
    );

    let source_map4 = SourceMap::new("var result = 10 & 5;".to_string());
    let tokens4 = tokenize_all(&source_map4);
    assert_eq!(tokens4[4].token_type, TokenType::Error);
    assert_eq!(
        tokens4[4].error_message.as_ref().unwrap(),
        "Unexpected character: '&'."
    );

    let source_map5 = SourceMap::new("@#$".to_string());
    let tokens5 = tokenize_all(&source_map5);
    assert_eq!(tokens5[0].token_type, TokenType::Error);
    assert_eq!(
        tokens5[0].error_message.as_ref().unwrap(),
        "Unexpected character: '@'."
    );
    assert_eq!(tokens5[1].token_type, TokenType::Error);
    assert_eq!(
        tokens5[1].error_message.as_ref().unwrap(),
        "Unexpected character: '#'."
    );
    assert_eq!(tokens5[2].token_type, TokenType::Error);
    assert_eq!(
        tokens5[2].error_message.as_ref().unwrap(),
        "Unexpected character: '$'."
    );
}

#[test]
fn test_nested_unterminated_constructs() {
    let source_map1 = SourceMap::new("\"unterminated string with 'single quotes'".to_string());
    let tokens1 = tokenize_all(&source_map1);
    assert_eq!(tokens1[0].token_type, TokenType::Error);
    assert_eq!(
        tokens1[0].error_message.as_ref().unwrap(),
        "Unterminated string."
    );

    let source_map2 = SourceMap::new("\"unterminated\n// comment after".to_string());
    let tokens2 = tokenize_all(&source_map2);
    assert_eq!(tokens2[0].token_type, TokenType::Error);
    assert_eq!(
        tokens2[0].error_message.as_ref().unwrap(),
        "Unterminated string."
    );

    let source_map3 = SourceMap::new("/* unterminated with // inside".to_string());
    let tokens3 = tokenize_all(&source_map3);
    assert_eq!(tokens3[0].token_type, TokenType::Error);
    assert_eq!(
        tokens3[0].error_message.as_ref().unwrap(),
        "Unterminated comment."
    );

    let source_map4 = SourceMap::new("/* outer comment /* nested start but no end".to_string());
    let tokens4 = tokenize_all(&source_map4);
    assert_eq!(tokens4[0].token_type, TokenType::Error);
    assert_eq!(
        tokens4[0].error_message.as_ref().unwrap(),
        "Unterminated comment."
    );

    let source_map5 = SourceMap::new("\"escaped \\\" quote but still unterminated".to_string());
    let tokens5 = tokenize_all(&source_map5);
    assert_eq!(tokens5[0].token_type, TokenType::Error);
    assert_eq!(
        tokens5[0].error_message.as_ref().unwrap(),
        "Unterminated string."
    );
}

#[test]
fn test_invalid_escape_sequences() {
    let source_map1 = SourceMap::new("\"hello\\qworld\"".to_string());
    let tokens1 = tokenize_all(&source_map1);
    assert_eq!(tokens1[0].token_type, TokenType::Error);
    assert_eq!(
        tokens1[0].error_message.as_ref().unwrap(),
        "Invalid escape sequence: \\q"
    );

    let source_map2 = SourceMap::new("\"\\x invalid escape\"".to_string());
    let tokens2 = tokenize_all(&source_map2);
    assert_eq!(tokens2[0].token_type, TokenType::Error);
    assert_eq!(
        tokens2[0].error_message.as_ref().unwrap(),
        "Invalid escape sequence: \\x"
    );

    let source_map3 = SourceMap::new("\"\\z another invalid\"".to_string());
    let tokens3 = tokenize_all(&source_map3);
    assert_eq!(tokens3[0].token_type, TokenType::Error);
    assert_eq!(
        tokens3[0].error_message.as_ref().unwrap(),
        "Invalid escape sequence: \\z"
    );

    let source_map4 = SourceMap::new("\"hello\\nworld\\t\\\"test\\\\\"".to_string());
    let tokens4 = tokenize_all(&source_map4);
    assert_eq!(tokens4[0].token_type, TokenType::String);
    assert!(tokens4[0].error_message.is_none());
}

#[test]
fn test_unterminated_multiline_comment() {
    let source_map = SourceMap::new("/* unterminated comment".to_string());
    let tokens = tokenize_all(&source_map);
    assert!(matches!(tokens[0].token_type, TokenType::Error));
}

#[test]
fn test_number_lexeme_content() {
    let source_map1 = SourceMap::new("123".to_string());
    let tokens1 = tokenize_all(&source_map1);
    assert_eq!(tokens1[0].token_type, TokenType::Number);
    let lexeme1: String = tokens1[0].lexeme(&source_map1).iter().collect();
    assert_eq!(lexeme1, "123");

    let source_map2 = SourceMap::new("123.456".to_string());
    let tokens2 = tokenize_all(&source_map2);
    assert_eq!(tokens2[0].token_type, TokenType::Number);
    let lexeme2: String = tokens2[0].lexeme(&source_map2).iter().collect();
    assert_eq!(lexeme2, "123.456");

    let source_map3 = SourceMap::new(".789".to_string());
    let tokens3 = tokenize_all(&source_map3);
    assert_eq!(tokens3[0].token_type, TokenType::Number);
    let lexeme3: String = tokens3[0].lexeme(&source_map3).iter().collect();
    assert_eq!(lexeme3, ".789");

    let source_map4 = SourceMap::new("0".to_string());
    let tokens4 = tokenize_all(&source_map4);
    assert_eq!(tokens4[0].token_type, TokenType::Number);
    let lexeme4: String = tokens4[0].lexeme(&source_map4).iter().collect();
    assert_eq!(lexeme4, "0");

    let source_map5 = SourceMap::new("999999999".to_string());
    let tokens5 = tokenize_all(&source_map5);
    assert_eq!(tokens5[0].token_type, TokenType::Number);
    let lexeme5: String = tokens5[0].lexeme(&source_map5).iter().collect();
    assert_eq!(lexeme5, "999999999");

    let source_map6 = SourceMap::new("0.0".to_string());
    let tokens6 = tokenize_all(&source_map6);
    assert_eq!(tokens6[0].token_type, TokenType::Number);
    let lexeme6: String = tokens6[0].lexeme(&source_map6).iter().collect();
    assert_eq!(lexeme6, "0.0");
}

#[test]
fn test_string_lexeme_content() {
    let source_map1 = SourceMap::new("\"hello\"".to_string());
    let tokens1 = tokenize_all(&source_map1);
    assert_eq!(tokens1[0].token_type, TokenType::String);
    let lexeme1: String = tokens1[0].lexeme(&source_map1).iter().collect();
    assert_eq!(lexeme1, "\"hello\"");

    let source_map2 = SourceMap::new("\"\"".to_string());
    let tokens2 = tokenize_all(&source_map2);
    assert_eq!(tokens2[0].token_type, TokenType::String);
    let lexeme2: String = tokens2[0].lexeme(&source_map2).iter().collect();
    assert_eq!(lexeme2, "\"\"");

    let source_map3 = SourceMap::new("\"hello\\nworld\\t\"".to_string());
    let tokens3 = tokenize_all(&source_map3);
    assert_eq!(tokens3[0].token_type, TokenType::String);
    let lexeme3: String = tokens3[0].lexeme(&source_map3).iter().collect();
    assert_eq!(lexeme3, "\"hello\\nworld\\t\"");

    let source_map4 = SourceMap::new("\"say \\\"hello\\\"\"".to_string());
    let tokens4 = tokenize_all(&source_map4);
    assert_eq!(tokens4[0].token_type, TokenType::String);
    let lexeme4: String = tokens4[0].lexeme(&source_map4).iter().collect();
    assert_eq!(lexeme4, "\"say \\\"hello\\\"\"");

    let source_map5 = SourceMap::new("\"café 🦀\"".to_string());
    let tokens5 = tokenize_all(&source_map5);
    assert_eq!(tokens5[0].token_type, TokenType::String);
    let lexeme5: String = tokens5[0].lexeme(&source_map5).iter().collect();
    assert_eq!(lexeme5, "\"café 🦀\"");

    let source_map6 = SourceMap::new("\"hello world! @#$%^&*()\"".to_string());
    let tokens6 = tokenize_all(&source_map6);
    assert_eq!(tokens6[0].token_type, TokenType::String);
    let lexeme6: String = tokens6[0].lexeme(&source_map6).iter().collect();
    assert_eq!(lexeme6, "\"hello world! @#$%^&*()\"");

    let source_map7 = SourceMap::new("\"path\\\\to\\\\file\"".to_string());
    let tokens7 = tokenize_all(&source_map7);
    assert_eq!(tokens7[0].token_type, TokenType::String);
    let lexeme7: String = tokens7[0].lexeme(&source_map7).iter().collect();
    assert_eq!(lexeme7, "\"path\\\\to\\\\file\"");
}

#[test]
fn test_identifier_lexem_content() {
    let source_map1 = SourceMap::new("hello".to_string());
    let tokens1 = tokenize_all(&source_map1);
    assert_eq!(tokens1[0].token_type, TokenType::Identifier);
    let lexeme1: String = tokens1[0].lexeme(&source_map1).iter().collect();
    assert_eq!(lexeme1, "hello");

    let source_map2 = SourceMap::new("_underscore".to_string());
    let tokens2 = tokenize_all(&source_map2);
    assert_eq!(tokens2[0].token_type, TokenType::Identifier);
    let lexeme2: String = tokens2[0].lexeme(&source_map2).iter().collect();
    assert_eq!(lexeme2, "_underscore");

    let source_map3 = SourceMap::new("var123".to_string());
    let tokens3 = tokenize_all(&source_map3);
    assert_eq!(tokens3[0].token_type, TokenType::Identifier);
    let lexeme3: String = tokens3[0].lexeme(&source_map3).iter().collect();
    assert_eq!(lexeme3, "var123");

    let source_map4 = SourceMap::new("_".to_string());
    let tokens4 = tokenize_all(&source_map4);
    assert_eq!(tokens4[0].token_type, TokenType::Identifier);
    let lexeme4: String = tokens4[0].lexeme(&source_map4).iter().collect();
    assert_eq!(lexeme4, "_");

    let source_map5 = SourceMap::new("CamelCaseVar".to_string());
    let tokens5 = tokenize_all(&source_map5);
    assert_eq!(tokens5[0].token_type, TokenType::Identifier);
    let lexeme5: String = tokens5[0].lexeme(&source_map5).iter().collect();
    assert_eq!(lexeme5, "CamelCaseVar");

    let source_map6 = SourceMap::new("myVar123_test".to_string());
    let tokens6 = tokenize_all(&source_map6);
    assert_eq!(tokens6[0].token_type, TokenType::Identifier);
    let lexeme6: String = tokens6[0].lexeme(&source_map6).iter().collect();
    assert_eq!(lexeme6, "myVar123_test");

    let source_map7 = SourceMap::new("x".to_string());
    let tokens7 = tokenize_all(&source_map7);
    assert_eq!(tokens7[0].token_type, TokenType::Identifier);
    let lexeme7: String = tokens7[0].lexeme(&source_map7).iter().collect();
    assert_eq!(lexeme7, "x");
}

#[test]
fn test_operator_lexeme_content() {
    let operators = vec![
        ("(", TokenType::LeftParen),
        (")", TokenType::RightParen),
        ("{", TokenType::LeftBrace),
        ("}", TokenType::RightBrace),
        ("[", TokenType::LeftSquareBracket),
        ("]", TokenType::RightSquareBracket),
        (",", TokenType::Comma),
        (".", TokenType::Dot),
        (":", TokenType::Colon),
        ("?", TokenType::Question),
        ("+", TokenType::Plus),
        ("-", TokenType::Minus),
        ("*", TokenType::Star),
        ("/", TokenType::Slash),
        ("%", TokenType::Modulo),
        (";", TokenType::Semicolon),
        ("=", TokenType::Equals),
        ("!", TokenType::Bang),
        ("<", TokenType::Less),
        (">", TokenType::Greater),
    ];

    for (op_str, expected_type) in operators {
        let source_map = SourceMap::new(op_str.to_string());
        let tokens = tokenize_all(&source_map);
        assert_eq!(tokens[0].token_type, expected_type);
        let lexeme: String = tokens[0].lexeme(&source_map).iter().collect();
        assert_eq!(lexeme, op_str);
    }

    let multi_operators = vec![
        ("!=", TokenType::BangEquals),
        ("<=", TokenType::LessEquals),
        (">=", TokenType::GreaterEquals),
        ("->", TokenType::Arrow),
        ("==", TokenType::EqualsEquals),
        ("|>", TokenType::Pipe),
    ];

    for (op_str, expected_type) in multi_operators {
        let source_map = SourceMap::new(op_str.to_string());
        let tokens = tokenize_all(&source_map);
        assert_eq!(tokens[0].token_type, expected_type);
        let lexeme: String = tokens[0].lexeme(&source_map).iter().collect();
        assert_eq!(lexeme, op_str);
    }
}

#[test]
fn test_keyword_lexeme_token_contentt() {
    let keywords = vec![
        ("and", TokenType::And),
        ("class", TokenType::Class),
        ("else", TokenType::Else),
        ("false", TokenType::False),
        ("for", TokenType::For),
        ("continue", TokenType::Continue),
        ("true", TokenType::True),
        ("var", TokenType::Var),
        ("while", TokenType::While),
        ("fn", TokenType::Fn),
        ("if", TokenType::If),
        ("nil", TokenType::Nil),
        ("or", TokenType::Or),
        ("break", TokenType::Break),
        ("super", TokenType::Super),
        ("this", TokenType::This),
        ("return", TokenType::Return),
        ("throw", TokenType::Throw),
        ("try", TokenType::Try),
        ("catch", TokenType::Catch),
        ("finally", TokenType::Finally),
        ("is", TokenType::Is),
    ];

    for (keyword_str, expected_type) in keywords {
        let source_map = SourceMap::new(keyword_str.to_string());
        let tokens = tokenize_all(&source_map);
        assert_eq!(tokens[0].token_type, expected_type);
        let lexeme: String = tokens[0].lexeme(&source_map).iter().collect();
        assert_eq!(lexeme, keyword_str);
    }
}

#[test]
fn test_line_and_column_tracking() {
    let source_map = SourceMap::new("first\nsecond line".to_string());
    let tokens = tokenize_all(&source_map);

    assert_eq!(tokens[0].line(&source_map), 1);
    assert_eq!(tokens[0].col(&source_map), 1);
    assert_eq!(tokens[1].line(&source_map), 2);
    assert_eq!(tokens[1].col(&source_map), 1);
    assert_eq!(tokens[2].line(&source_map), 2);
    assert_eq!(tokens[2].col(&source_map), 8);
}

#[test]
fn test_get_line_multiple_lines() {
    let source_map = SourceMap::new("first line\nsecond line\nthird line".to_string());
    let tokenizer = Tokenizer::new(&source_map);
    tokenizer.for_each(drop);
    let line1: String = source_map.get_line(1).iter().collect();
    let line2: String = source_map.get_line(2).iter().collect();
    let line3: String = source_map.get_line(3).iter().collect();

    assert_eq!(line1, "first line");
    assert_eq!(line2, "second line");
    assert_eq!(line3, "third line");
}

#[test]
fn test_is_operator() {
    let input = "x is y";
    let expected = vec![
        TokenType::Identifier,
        TokenType::Is,
        TokenType::Identifier,
        TokenType::Eof,
    ];
    assert_token_types(&input, &expected);
}

#[test]
fn test_assignment_operators() {
    let input = "x += y -= z *= w /= v %= u";
    let expected = vec![
        TokenType::Identifier,
        TokenType::PlusAssign,
        TokenType::Identifier,
        TokenType::MinusAssign,
        TokenType::Identifier,
        TokenType::StarAssign,
        TokenType::Identifier,
        TokenType::SlashAssign,
        TokenType::Identifier,
        TokenType::ModuloAssign,
        TokenType::Identifier,
        TokenType::Eof,
    ];
    assert_token_types(&input, &expected);
}
