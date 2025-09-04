use super::{assert_no_parse_errors, parse_source};
use crate::{SourceMap, TypedNodeArena, memory::StringInterner};

#[test]
fn test_object_literals() {
    let source_code = r#"
        var empty_obj = {{}};
        var basic_obj = {{ field_1 = 1, field_2 = 2 }};
        var other_field = "other value";
        var obj = {{
            field = "value",
            other_field
        }};
    "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_object_literals_with_trailing_comma() {
    let source_code = r#"
        var basic_obj = {{ field_1 = 1, field_2 = 2, }};
        var other_field = "other value";
        var shorthand_obj = {{
            field = "value",
            other_field,
        }};
    "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_arithmetic_expressions() {
    let source_code = r#"var result = a + b * c - d / e % f;"#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_comparison_expressions() {
    let source_code = r#"var check = x > y and a <= b or c != d and e == f;"#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_unary_expressions() {
    let source_code = r#"var result = !condition and -number;"#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_assignment_expressions() {
    let source_code = r#"
            x = 5;
            obj.property = "value";
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_ternary_expressions() {
    let source_code = r#"var result = condition ? trueValue : falseValue;"#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_pipe_expressions() {
    let source_code = r#"var result = value |> transform |> finalize;"#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_function_calls() {
    let source_code = r#"
            result = func();
            result2 = func(a, b, c);
            result3 = obj.method(arg);
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_function_calls_with_trailing_comma() {
    let source_code = r#"
            result2 = func(a, b, c,);
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_property_access() {
    let source_code = r#"
            value = obj.property;
            value2 = obj.nested.deep.property;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_array_access() {
    let source_code = r#"
            value = array[0];
            value2 = matrix[row][col];
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_array_literals() {
    let source_code = r#"
            empty = [];
            numbers = [1, 2, 3, 4];
            mixed = [1, "string", true, nil];
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_array_literals_with_trailing_commas() {
    let source_code = r#"numbers = [1, 2, 3, 4,];"#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_grouping_expressions() {
    let source_code = r#"var result = (a + b) * (c - d);"#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_literals() {
    let source_code = r#"
            var num = 42.5;
            var str = "hello world";
            var bool1 = true;
            var bool2 = false;
            var nothing = nil;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_this_and_super() {
    let source_code = r#"
            class Child : Parent {
                method() {
                    this.value = super.getValue();
                }
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_nested_function_calls() {
    let source_code = r#"
            var result = outer(inner(deep(value)));
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_operator_precedence() {
    let source_code = r#"
            var result = a + b * c == d - e / f;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_right_associative_ternary() {
    let source_code = r#"
            var result = a ? b ? c : d : e;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_chained_method_calls() {
    let source_code = r#"
            var result = obj.method1().method2().method3();
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_mixed_property_and_method_access() {
    let source_code = r#"
            var result = obj.property.method().field[index];
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_complex_array_access() {
    let source_code = r#"
            var result = matrix[row + 1][col - 1];
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_assignment_chaining() {
    let source_code = r#"
            a = b = c = 5;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_nested_ternary_expressions() {
    let source_code = r#"
            var result = a ? b : c ? d : e ? f : g;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_complex_boolean_logic() {
    let source_code = r#"
            var condition = !a and (b or c) and !(d or e);
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_pipe_operator_precedence() {
    let source_code = r#"
            var result = value + 1 |> transform |> process - 2;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_deeply_nested_expressions() {
    let source_code = r#"
            var result = ((((a + b) * c) - d) / e) % f;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_as_expression() {
    let source_code = r#"
            assert_throws(() -> 1 / 0);
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (__program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_as_immediately_invoked_expression() {
    let source_code = r#"
            var test = (() -> nil)();
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_as_immediately_invoked_expression_with_args() {
    let source_code = r#"
            var result = ((x) -> x * 2)(5);
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_index_assignment() {
    let source_code = r#"
        foo[1] = true;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (__program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_map_expressions() {
    let source_code = r#"
            var simple_map = arr||item -> item * 2|;
            var param_map = obj||x -> x + x|.floor();
            var single_param_map = value||v -> 42|;
            var precedence_test = nums||n -> n + 1 * 2|;
            var comparison_test = items||i -> i > 5|;
            var logical_test = flags||f -> f and true|;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_map_expression_chaining() {
    let source_code = r#"
        obj||o -> o.inner|.value; 
        println(obj||o -> inner|.value);
        var foo = obj||o -> () -> o|;
        println(foo());
    "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_optional_map_expressions() {
    let source_code = r#"
            var optional_simple = arr?|item -> item * 3|;
            var optional_precedence = vals?|v -> v + 2 * 4|;
            var optional_logical = flags?|f -> f or false|;
        "#;
    let source_map = SourceMap::new(source_code.to_string());
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(&source_map, nodes, strings);

    assert_no_parse_errors(&errors);
}
