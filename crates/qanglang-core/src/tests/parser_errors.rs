use super::{assert_parse_error, parse_source};
use crate::SourceMap;

#[test]
fn test_error_recovery() {
    let source_code = r#"
            var x = 5;
            var y = ; // Error here
            var z = 10; // This should still parse
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert!(errors.has_errors());
    assert!(program.decls.len() >= 2);

    // TODO write tests to ensure that we have at least the first and third declarations
}

#[test]
fn test_missing_arrow_in_lambda() {
    let source_code = r#"var func = (x) x + 1;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Unexpected oprand. Missing operator or ';'.");
}

#[test]
fn test_unexpected_token_in_expression() {
    let source_code = r#"var x = 5 @ 3;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert!(errors.has_errors());

    // TODO test that the specfied error is in the error reporter
}

#[test]
fn test_missing_semicolon_error() {
    let source_code = r#"var x = 5"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expect ';'");
}

#[test]
fn test_unterminated_string_error() {
    let source_code = r#"var msg = "unterminated string"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert!(errors.has_errors());
}

#[test]
fn test_missing_closing_brace_error() {
    let source_code = r#"
            fn test() {
                var x = 5;
                // Missing closing brace
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expected '}'");
}

#[test]
fn test_invalid_assignment_target_error() {
    let source_code = r#"5 = x;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Invalid assignment target");
}

#[test]
fn test_missing_function_name_error() {
    let source_code = r#"fn () { return 42; }"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expect function name");
}

#[test]
fn test_missing_variable_name_error() {
    let source_code = r#"var = 5;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expect variable name");
}

#[test]
fn test_missing_class_name_error() {
    let source_code = r#"class { }"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expect class name");
}

#[test]
fn test_missing_parentheses_in_if() {
    let source_code = r#"
            if condition {
                doSomething();
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expected '('");
}

#[test]
fn test_missing_parentheses_in_while() {
    let source_code = r#"
            while condition {
                doSomething();
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expected '('");
}

#[test]
fn test_missing_parentheses_in_for() {
    let source_code = r#"
            for var i = 0; i < 10; i = i + 1 {
                print(i);
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expect '(' after 'for'");
}

#[test]
fn test_unterminated_array() {
    let source = r#"var array = [1, 2, 3"#;
    let source_map = SourceMap::new(source.to_string());
    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expect ']' after array elements.");
}

#[test]
fn test_unterminated_with_trailing_comma() {
    let source = r#"var array = [1, 2, 3,"#;
    let source_map = SourceMap::new(source.to_string());
    let (_program, errors) = parse_source(&source_map);

    assert_parse_error(&errors, "Expected expression.");
}
