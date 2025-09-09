use std::sync::Arc;

use super::{assert_no_parse_errors, parse_source};
use crate::{SourceMap, TypedNodeArena, memory::StringInterner};

#[test]
fn test_simple_variable_declaration() {
    let source_code = r#"var x = 42;"#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_variable_declaration_without_initializer() {
    let source_code = r#"var x;"#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_function_declaration() {
    let source_code = r#"
            fn add(a, b) {
                return a + b;
            }
        "#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_function_declaration_with_trailing_comma() {
    let source_code = r#"
            fn add(a, b,) {
                return a + b;
            }
        "#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_function_without_parameters() {
    let source_code = r#"
            fn main() {
                var x = 5;
            }
        "#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_class_declaration() {
    let source_code = r#"
            class Person {
                name;
                age = 0;
                
                get_name() {
                    return this.name;
                }
            }
        "#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_class_declaration_with_method_containing_trailing_comma_in_parameters() {
    let source_code = r#"
            class Class {
                method(arg1, arg2,) {
                    return;
                }
            }
        "#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_class_with_inheritance() {
    let source_code = r#"
            class Student : Person {
                grade = "A";
            }
        "#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_declaration() {
    let source_code = r#"var add = (a, b) -> a + b;"#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_declaration_with_trailing_comma_in_parameters() {
    let source_code = r#"var add = (a, b,) -> a + b;"#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_with_block_body() {
    let source_code = r#"var calc = (x) -> { return x * 2; };"#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_empty_lambda_parameters() {
    let source_code = r#"var func = () -> 42;"#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_variable_declaration_with_call_and_lambda() {
    let source_code = r#"var y = identity(() -> "hello world");"#;
    let source_map = Arc::new(SourceMap::new(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_, errors) = parse_source(source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}
