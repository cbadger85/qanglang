pub mod alloc_tests;
pub mod compound_assignment_tests;
pub mod parse_declarations;
pub mod parse_expressions;
pub mod parse_statements;
pub mod parser_errors;
pub mod simple_return_test;
pub mod source_analysis_test;
pub mod tail_call_debug;
pub mod tail_call_comparison;
pub mod tail_call_simple;
pub mod tail_call_tests;
pub mod tokenizing;
pub mod vm_closure_tests;
pub mod vm_tests;

pub fn parse_source(source_map: &crate::SourceMap) -> (crate::ast::Program, crate::ErrorReporter) {
    let mut parser = crate::Parser::new(source_map);
    let program = parser.parse();
    let errors = parser.into_reporter();
    (program, errors)
}

pub fn assert_no_parse_errors(errors: &crate::ErrorReporter) {
    if errors.has_errors() {
        for error in errors.errors() {
            eprintln!("{}", error.message);
        }
        panic!("Unexpected parse errors.");
    }
}

pub fn assert_parse_error(errors: &crate::ErrorReporter, expected_message: &str) {
    assert!(
        errors.has_errors(),
        "Expected parse error but none occurred"
    );
    let error_text = errors
        .errors()
        .iter()
        .map(|e| e.message.clone())
        .collect::<Vec<String>>()
        .join("\n");
    assert!(
        error_text.contains(expected_message),
        "Expected error message '{}' but got: {}",
        expected_message,
        error_text
    );
}
