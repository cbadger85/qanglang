use super::{assert_no_parse_errors, parse_source};
use crate::{SourceMap, TypedNodeArena, memory::StringInterner};

#[test]
fn test_if_statement() {
    let source_code = r#"
            if (x > 0) {
                return true;
            } else {
                return false;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_if_statement_without_else() {
    let source_code = r#"
            if (condition) {
                doSomething();
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_if_else_statement() {
    let source_code = r#"
            if (condition) {
                do_something();
            } else if (other_condition) {
                do_something_else(); 
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (__program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_if_expression_statement() {
    let source_code = r#"
            if (condition) do_something();
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (__program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_else_expression_statement() {
    let source_code = r#"
            if (condition) do_something(); else do_something_else();
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (__program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_while_statement() {
    let source_code = r#"
            while (i < 10) {
                i = i + 1;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_for_statement_with_all_clauses() {
    let source_code = r#"
            for (var i = 0; i < 10; i = i + 1) {
                print(i);
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_for_statement_with_expression_initializer() {
    let source_code = r#"
            for (i = 0; i < 10; i = i + 1) {
                print(i);
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_for_statement_minimal() {
    let source_code = r#"
            for (;;) {
                break;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_break_and_continue_statements() {
    let source_code = r#"
            while (true) {
                if (condition1) {
                    break;
                }
                if (condition2) {
                    continue;
                }
                doWork();
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_return_statement() {
    let source_code = r#"
            fn test() {
                return 42;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_return_statement_without_value() {
    let source_code = r#"
            fn test() {
                return;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_block_statements() {
    let source_code = r#"
            {
                var x = 1;
                var y = 2;
                x + y;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_expression_statements() {
    let source_code = r#"
            someFunction();
            obj.method();
            x + y;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_comments_ignored() {
    let source_code = r#"
            // This is a comment
            var x = 5; // Another comment
            /* Multi-line
               comment */
            var y = 10;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_block_scope_variables() {
    let source_code = r#"
        var two = "two";
        print(two);
        two = 2;
        {
            var two = "2";
            print(two);
            print("TEST");
        }
        print(two);
  "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}
