use super::{assert_no_parse_errors, parse_source};
use crate::{SourceMap, TypedNodeArena, memory::StringInterner};
use std::sync::Arc;

#[test]
fn test_variable_type_annotations() {
    let source_code = r#"
        var name: String = "hello";
        var age: Number = 42;
        var isActive: Boolean = true;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_optional_type_annotations() {
    let source_code = r#"
        var optional: String? = nil;
        var maybeNumber: Number? = 42;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_function_parameter_type_annotations() {
    let source_code = r#"
        fn greet(name: String, age: Number) {
            return name;
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_function_return_type_annotations() {
    let source_code = r#"
        fn getName() -> String {
            return "test";
        }

        fn calculate(x: Number, y: Number) -> Number {
            return x + y;
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_primitive_types() {
    let source_code = r#"
        var str: String = "hello";
        var num: Number = 42;
        var bool: Boolean = true;
        var dynamic: dyn = "anything";
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_array_types() {
    let source_code = r#"
        var strings: [String] = ["hello", "world"];
        var numbers: [Number] = [1, 2, 3];
        var nested: [[String]] = [["a"], ["b"]];
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_union_types() {
    let source_code = r#"
        var value: String | Number = "hello";
        var multiType: String | Number | Boolean = true;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_optional_types() {
    let source_code = r#"
        var optional: String? = nil;
        var arrayOptional: [Number]? = nil;
        var unionOptional: String | Number = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_function_types() {
    let source_code = r#"
        var callback: (String, Number) -> Boolean = nil;
        var simpleFunc: () -> String = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_object_types() {
    let source_code = r#"
        var user: { name: String, age: Number } = nil;
        var empty: {} = nil;
        var trailing: { name: String, } = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_nested_object_types() {
    let source_code = r#"
        var complex: { user: { name: String, details: { age: Number } } } = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_complex_nested_types() {
    let source_code = r#"
        var complex: [(String | Number)?] = [nil, "hello", 42];
        var callback: String = "fallback";
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_parameter_types() {
    let source_code = r#"
        var lambda = (param: String, other: Number) -> param;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_casting() {
    let source_code = r#"
        var value = (someExpr as String) * 4;
        var number = (42 as Number);
        var complex = (expr as [String | Number]);
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_chained_type_casting() {
    let source_code = r#"
        var result = ((expr as String) as dyn);
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_object_union_types() {
    let source_code = r#"
        var value: String | { name: String } = "hello";
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_optional_object_types() {
    let source_code = r#"
        var maybeUser: { name: String }? = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_mixed_function_and_lambda_types() {
    let source_code = r#"
        fn process(callback: (String) -> Number, data: String) -> Number {
            return callback(data);
        }

        var lambda: (String) -> Number = (s: String) -> 42;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

// Error case tests
#[test]
fn test_invalid_type_syntax_errors() {
    let source_code = r#"
        var invalid: [ = "hello";
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert!(
        errors.has_errors(),
        "Expected parse errors for invalid type syntax"
    );
}

#[test]
fn test_incomplete_union_type_errors() {
    let source_code = r#"
        var invalid: String | = "hello";
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert!(
        errors.has_errors(),
        "Expected parse errors for incomplete union type"
    );
}

#[test]
fn test_malformed_object_type_errors() {
    let source_code = r#"
        var invalid: { name } = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert!(
        errors.has_errors(),
        "Expected parse errors for malformed object type"
    );
}

// ============================================================================
// Generic Type Tests
// ============================================================================

#[test]
fn test_simple_generic_types() {
    let source_code = r#"
        var container: Container<String> = nil;
        var map: Map<String, Number> = nil;
        var optional: Option<Boolean> = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_nested_generic_types() {
    let source_code = r#"
        var nested: Container<[String]> = nil;
        var complex: Map<String, [Number]> = nil;
        var deep: Container<Map<String, Number>> = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_generic_with_union_types() {
    let source_code = r#"
        var unionGeneric: Container<String | Number> = nil;
        var genericUnion: Container<String> | Map<String, Number> = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_generic_with_optional_types() {
    let source_code = r#"
        var optionalGeneric: Container<String?> = nil;
        var genericOptional: Container<String>? = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_generic_function_types() {
    let source_code = r#"
        var funcGeneric: Container<(String) -> Number> = nil;
        var genericFunc: ((Container<String>) -> Map<String, Number>) = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_single_type_parameter_generic() {
    let source_code = r#"
        var single: List<T> = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_multiple_type_parameters() {
    let source_code = r#"
        var triple: Tuple<String, Number, Boolean> = nil;
        var quad: Map<String, Number, Boolean, T> = nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

// ============================================================================
// Function/Class Generic Tests
// ============================================================================

#[test]
fn test_function_generic_parameters() {
    let source_code = r#"
        fn process<T>(item: T) -> T {
            return item;
        }

        fn map<T, U>(items: [T], mapper: (T) -> U) -> [U] {
            return [];
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_class_generic_parameters() {
    let source_code = r#"
        class Container<T> {
        }

        class Map<K, V> {
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_class_generic_with_methods() {
    let source_code = r#"
        class Container<T> {
            getValue() {
                return nil;
            }

            setValue(value) {
                return;
            }
        }

        class Map<K, V> {
            get(key) {
                return nil;
            }

            put(key, value) {
                return;
            }
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_class_generic_with_fields() {
    let source_code = r#"
        class Container<T> {
            value;

            init(val) {
                this.value = val;
            }

            getValue() {
                return this.value;
            }
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_mixed_generics_and_inheritance() {
    let source_code = r#"
        class Base<T> {
        }

        class Derived<T, U> : Base {
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_class_field_type_annotations() {
    let source_code = r#"
        class TestClass {
            name: String;
            age: Number = 25;
            isActive: Boolean?;
            tags: [String];
            data: { key: String, value: Number };
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_class_method_type_annotations() {
    let source_code = r#"
        class TestClass {
            getName() -> String {
                return "test";
            }

            getAge() -> Number {
                return 25;
            }

            setName(newName: String) -> String {
                return newName;
            }

            process(input: [String]) -> [String] {
                return input;
            }

            complexMethod(a: String | Number, b: Boolean?) -> { result: String } {
                return {{ result = "done" }};
            }
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_class_generic_with_typed_fields_and_methods() {
    let source_code = r#"
        class Container<T> {
            value: T;
            count: Number = 0;

            getValue() -> T {
                return this.value;
            }

            setValue(newValue: T) -> T {
                this.value = newValue;
                return this.value;
            }

            getCount() -> Number {
                return this.count;
            }

            process<U>(input: U) -> [T | U] {
                return [this.value, input];
            }
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_mixed_class_members_with_types() {
    let source_code = r#"
        class MixedClass<T, U> {
            // Fields with and without types
            untyped_field;
            typed_field: String;
            generic_field: T;
            optional_field: U? = nil;

            // Methods with and without return types
            simple_method() {
                return "simple";
            }

            typed_method() -> String {
                return this.typed_field;
            }

            generic_method(param: T) -> U {
                return this.optional_field;
            }

            complex_method<V>(a: T, b: U, c: V) -> [T | U | V] {
                return [a, b, c];
            }
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_simple_type_declarations() {
    let source_code = r#"
        type SimpleString = String;
        type SimpleNumber = Number;
        type SimpleBool = Boolean;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_complex_type_declarations() {
    let source_code = r#"
        type OptionalNumber = Number?;
        type StringArray = [String];
        type UserObject = { name: String, age: Number };
        type UnionType = String | Number | Boolean;
        type FunctionType = (String, Number) -> Boolean;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_generic_type_declarations() {
    let source_code = r#"
        type Container<T> = [T];
        type Pair<T, U> = { first: T, second: U };
        type Triple<A, B, C> = { a: A, b: B, c: C };
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_complex_generic_type_declarations() {
    let source_code = r#"
        type ComplexGeneric<T> = Container<T | String>;
        type NestedGeneric<T, U> = Pair<[T], { value: U }>;
        type FunctionGeneric<T, U, R> = (T, U) -> R;
        type OptionalGeneric<T> = T?;
        type UnionGeneric<T, U> = T | U | Number;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}
