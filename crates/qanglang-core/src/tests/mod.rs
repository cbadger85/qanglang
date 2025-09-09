use std::{path::PathBuf, sync::Arc};

pub mod alloc_tests;
pub mod compound_assignment_tests;
pub mod parse_declarations;
pub mod parse_expressions;
pub mod parse_statements;
pub mod parser_errors;
pub mod regression_tests;
pub mod source_analysis_test;
// pub mod tail_call_tests;
pub mod tokenizing;
pub mod vm_closure_tests;
pub mod vm_tests;

pub fn parse_source(
    source_map: Arc<crate::SourceMap>,
    mut nodes: crate::TypedNodeArena,
    mut strings: crate::memory::StringInterner,
) -> (crate::NodeId, crate::ErrorReporter) {
    // TODO maybe fix this so we don't need to clone it
    let mut parser = crate::Parser::new(
        source_map,
        PathBuf::new().as_path(),
        &mut nodes,
        &mut strings,
    );
    let modules = parser.parse();
    let errors = parser.into_errors();
    (modules.get_main().module_id, errors)
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
