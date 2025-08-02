pub mod compiler_tests;
pub mod parse_declarations;
pub mod parse_expressions;
pub mod parse_statements;
pub mod parser_errors;

pub fn parse_source(source_map: &crate::SourceMap) -> (crate::ast::Program, crate::ErrorReporter) {
    let mut parser = crate::Parser::new(&source_map);
    let program = parser.parse();
    let errors = parser.into_reporter();
    (program, errors)
}

pub fn assert_no_parse_errors(errors: &crate::ErrorReporter) {
    if errors.has_errors() {
        panic!("Unexpected parse errors:\n{}", errors.format_errors());
    }
}

pub fn assert_parse_error(errors: &crate::ErrorReporter, expected_message: &str) {
    assert!(
        errors.has_errors(),
        "Expected parse error but none occurred"
    );
    let error_text = errors.format_errors();
    assert!(
        error_text.contains(expected_message),
        "Expected error message '{}' but got: {}",
        expected_message,
        error_text
    );
}
