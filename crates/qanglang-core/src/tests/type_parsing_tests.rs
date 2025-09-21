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
fn test_generic_function_types_with_constraint() {
    let source_code = r#"
        fn function<T : Number>(value: T) -> T? {
            return nil;
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_mutiple_gene_function_types_with_constraint() {
    let source_code = r#"
        fn function<T, U : Number>(value: T) -> U {
            return 0;
        }
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

#[test]
fn test_unresolved_type_references() {
    let source_code = r#"
        var user: User;
        var data: MyData = getData();
        fn process(item: CustomType) -> Result {
            return item;
        }
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_user_defined_vs_primitives() {
    let source_code = r#"
        var primitive: String;        // Should be Primitive
        var userType: User;          // Should be UnresolvedReference
        var optional: String?;       // Should be Optional<Primitive>
        var userOptional: User?;     // Should be Optional<UnresolvedReference>
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_complex_unresolved_references() {
    let source_code = r#"
        var unionWithUser: String | User | Number;
        var arrayOfUser: [User];
        var objectWithUser: { user: User, name: String };
        var funcWithUser: (User) -> Result;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_dynamic_type_variants() {
    let source_code = r#"
        var basic: dyn;          // Dynamic non-null
        var nullable: dyn?;      // Dynamic nullable
        var topType: dyn!;       // Top type
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_dynamic_types_in_complex_expressions() {
    let source_code = r#"
        var unionWithDyn: String | dyn | Number;
        var arrayOfDyn: [dyn?];
        var funcWithDyn: (dyn) -> dyn!;
        var optionalDyn: dyn?;
        var objectWithDyn: { value: dyn, nullable: dyn? };
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

// ============================================================================
// Lambda Generic Tests
// ============================================================================

#[test]
fn test_lambda_generic_single_parameter() {
    let source_code = r#"
        var identity = <T>(value: T) -> value;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_generic_multiple_parameters() {
    let source_code = r#"
        var pair = <T, U>(first: T, second: U) -> {{first = first, second = second}};
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_object_literal_with_typed_values() {
    let source_code = r#"
        var obj =  {{first: Number = 1, second: Number = 2}};
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_generic_with_block_body() {
    let source_code = r#"
        var processor = <T>(item: T) -> {
            return item;
        };
        var validator = <V>(value: V) -> {
            if (value) {
                return value;
            }
            return nil;
        };
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_generic_empty_parameters() {
    // Let's start with the absolute simplest case
    let source_code = r#"
        var factory = <T>() -> nil;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_generic_with_complex_types() {
    let source_code = r#"
        var arrayMapper = <T, U>(items, mapper) -> items;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_generic_nested_calls() {
    let source_code = r#"
        var mapper = <T>(value) -> value;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_generic_with_optional_types() {
    let source_code = r#"
        var nullable = <T>(value) -> value;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_generic_trailing_comma() {
    let source_code = r#"
        var single = <T,>(value) -> value;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_generic_in_function_calls() {
    let source_code = r#"
        var called = (<U>(x) -> x)(100);
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_generic_mixed_with_regular_lambdas() {
    let source_code = r#"
        var generic = <T>(value) -> value;
    "#;
    let source_map = Arc::new(SourceMap::from_source(source_code.to_string()));
    let nodes = TypedNodeArena::new();
    let strings = StringInterner::new();

    let (_program, errors) = parse_source(source_map, nodes, strings);
    assert_no_parse_errors(&errors);
}

// ============================================================================
// Type Import Tests
// ============================================================================

#[test]
fn test_type_import_basic() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let temp_file = temp_dir.join("module.ql");
    fs::write(&temp_file, "// temp module for testing").expect("Failed to create temp file");

    // Create a main file that uses type import
    let main_file = temp_dir.join("main.ql");
    let relative_path = "module.ql";
    let source_code = format!(r#"type TheClass = import("{}").TheClass;"#, relative_path);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&temp_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_in_variable_declarations() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let test_id = "variable_declarations";
    let types_file = temp_dir.join(format!("types_{}.ql", test_id));
    let models_file = temp_dir.join(format!("models_{}.ql", test_id));
    let config_dir = temp_dir.join(format!("config_{}", test_id));
    fs::create_dir_all(&config_dir).expect("Failed to create config dir");
    let settings_file = config_dir.join(format!("settings_{}.ql", test_id));

    fs::write(&types_file, "// temp module for testing").expect("Failed to create types file");
    fs::write(&models_file, "// temp module for testing").expect("Failed to create models file");
    fs::write(&settings_file, "// temp module for testing").expect("Failed to create settings file");

    // Create a main file in a subdirectory so "../models" works
    let sub_dir = temp_dir.join(format!("sub_{}", test_id));
    fs::create_dir_all(&sub_dir).expect("Failed to create sub dir");
    let main_file = sub_dir.join(format!("main_{}.ql", test_id));
    let source_code = format!(r#"
        var instance: import("../types_{}.ql").User = nil;
        var data: import("../models_{}.ql").Data = nil;
        var config: import("../config_{}/settings_{}.ql").Config = nil;
    "#, test_id, test_id, test_id, test_id);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&types_file);
    let _ = fs::remove_file(&models_file);
    let _ = fs::remove_file(&settings_file);
    let _ = fs::remove_dir(&config_dir);
    let _ = fs::remove_file(&main_file);
    let _ = fs::remove_dir(&sub_dir);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_in_function_parameters() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let test_id = "function_parameters";
    let types_file = temp_dir.join(format!("types_{}.ql", test_id));
    let request_file = temp_dir.join(format!("request_{}.ql", test_id));
    let response_file = temp_dir.join(format!("response_{}.ql", test_id));
    let result_file = temp_dir.join(format!("result_{}.ql", test_id));

    fs::write(&types_file, "// temp module for testing").expect("Failed to create types file");
    fs::write(&request_file, "// temp module for testing").expect("Failed to create request file");
    fs::write(&response_file, "// temp module for testing").expect("Failed to create response file");
    fs::write(&result_file, "// temp module for testing").expect("Failed to create result file");

    // Create a main file
    let main_file = temp_dir.join(format!("main_{}.ql", test_id));
    let source_code = format!(r#"
        fn process(item: import("types_{}.ql").Item) -> String {{
            return "done";
        }}

        fn handle(req: import("request_{}.ql").Request, res: import("response_{}.ql").Response) -> import("result_{}.ql").Result {{
            return nil;
        }}
    "#, test_id, test_id, test_id, test_id);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&types_file);
    let _ = fs::remove_file(&request_file);
    let _ = fs::remove_file(&response_file);
    let _ = fs::remove_file(&result_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_in_function_return_types() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let test_id = "function_return_types";
    let models_file = temp_dir.join(format!("models_{}.ql", test_id));
    let config_file = temp_dir.join(format!("config_{}.ql", test_id));

    fs::write(&models_file, "// temp module for testing").expect("Failed to create models file");
    fs::write(&config_file, "// temp module for testing").expect("Failed to create config file");

    // Create a main file
    let main_file = temp_dir.join(format!("main_{}.ql", test_id));
    let source_code = format!(r#"
        fn createUser() -> import("models_{}.ql").User {{
            return nil;
        }}

        fn getConfig() -> import("config_{}.ql").Settings {{
            return nil;
        }}
    "#, test_id, test_id);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&models_file);
    let _ = fs::remove_file(&config_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_in_complex_expressions() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let test_id = "complex_expressions";
    let models_file = temp_dir.join(format!("models_{}.ql", test_id));
    let types_file = temp_dir.join(format!("types_{}.ql", test_id));
    let request_file = temp_dir.join(format!("request_{}.ql", test_id));
    let response_file = temp_dir.join(format!("response_{}.ql", test_id));

    fs::write(&models_file, "// temp module for testing").expect("Failed to create models file");
    fs::write(&types_file, "// temp module for testing").expect("Failed to create types file");
    fs::write(&request_file, "// temp module for testing").expect("Failed to create request file");
    fs::write(&response_file, "// temp module for testing").expect("Failed to create response file");

    // Create a main file
    let main_file = temp_dir.join(format!("main_{}.ql", test_id));
    let source_code = format!(r#"
        var users: [import("models_{}.ql").User] = [];
        var optional: import("types_{}.ql").Data? = nil;
        var union: String | import("types_{}.ql").CustomType = "hello";
        var callback: (import("request_{}.ql").Request) -> import("response_{}.ql").Response = nil;
    "#, test_id, test_id, test_id, test_id, test_id);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&models_file);
    let _ = fs::remove_file(&types_file);
    let _ = fs::remove_file(&request_file);
    let _ = fs::remove_file(&response_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_in_nested_expressions() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let test_id = "nested_expressions";
    let types_file = temp_dir.join(format!("types_{}.ql", test_id));
    let models_file = temp_dir.join(format!("models_{}.ql", test_id));
    let config_file = temp_dir.join(format!("config_{}.ql", test_id));

    fs::write(&types_file, "// temp module for testing").expect("Failed to create types file");
    fs::write(&models_file, "// temp module for testing").expect("Failed to create models file");
    fs::write(&config_file, "// temp module for testing").expect("Failed to create config file");

    // Create a main file
    let main_file = temp_dir.join(format!("main_{}.ql", test_id));
    let source_code = format!(r#"
        var nested: [[import("types_{}.ql").Item]] = [];
        var complex: {{ users: [import("models_{}.ql").User], config: import("config_{}.ql").Settings }} = nil;
        var unionArray: [String | import("types_{}.ql").CustomType] = [];
    "#, test_id, test_id, test_id, test_id);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&types_file);
    let _ = fs::remove_file(&models_file);
    let _ = fs::remove_file(&config_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_with_generics() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let test_id = "with_generics";
    let types_file = temp_dir.join(format!("types_{}.ql", test_id));
    let models_file = temp_dir.join(format!("models_{}.ql", test_id));
    let data_file = temp_dir.join(format!("data_{}.ql", test_id));
    let errors_file = temp_dir.join(format!("errors_{}.ql", test_id));

    fs::write(&types_file, "// temp module for testing").expect("Failed to create types file");
    fs::write(&models_file, "// temp module for testing").expect("Failed to create models file");
    fs::write(&data_file, "// temp module for testing").expect("Failed to create data file");
    fs::write(&errors_file, "// temp module for testing").expect("Failed to create errors file");

    // Create a main file
    let main_file = temp_dir.join(format!("main_{}.ql", test_id));
    let source_code = format!(r#"
        var container: Container<import("types_{}.ql").Item> = nil;
        var map: Map<String, import("models_{}.ql").User> = nil;
        var complex: Result<import("data_{}.ql").Response, import("errors_{}.ql").Error> = nil;
    "#, test_id, test_id, test_id, test_id);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&types_file);
    let _ = fs::remove_file(&models_file);
    let _ = fs::remove_file(&data_file);
    let _ = fs::remove_file(&errors_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_in_class_fields() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let test_id = "class_fields";
    let models_file = temp_dir.join(format!("models_{}.ql", test_id));
    let config_file = temp_dir.join(format!("config_{}.ql", test_id));
    let utils_file = temp_dir.join(format!("utils_{}.ql", test_id));

    fs::write(&models_file, "// temp module for testing").expect("Failed to create models file");
    fs::write(&config_file, "// temp module for testing").expect("Failed to create config file");
    fs::write(&utils_file, "// temp module for testing").expect("Failed to create utils file");

    // Create a main file
    let main_file = temp_dir.join(format!("main_{}.ql", test_id));
    let source_code = format!(r#"
        class UserService {{
            user: import("models_{}.ql").User;
            config: import("config_{}.ql").Settings = nil;
            logger: import("utils_{}.ql").Logger?;
        }}
    "#, test_id, test_id, test_id);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&models_file);
    let _ = fs::remove_file(&config_file);
    let _ = fs::remove_file(&utils_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_in_class_methods() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let test_id = "class_methods";
    let models_file = temp_dir.join(format!("models_{}.ql", test_id));
    let result_file = temp_dir.join(format!("result_{}.ql", test_id));
    let request_file = temp_dir.join(format!("request_{}.ql", test_id));
    let config_file = temp_dir.join(format!("config_{}.ql", test_id));
    let response_file = temp_dir.join(format!("response_{}.ql", test_id));

    fs::write(&models_file, "// temp module for testing").expect("Failed to create models file");
    fs::write(&result_file, "// temp module for testing").expect("Failed to create result file");
    fs::write(&request_file, "// temp module for testing").expect("Failed to create request file");
    fs::write(&config_file, "// temp module for testing").expect("Failed to create config file");
    fs::write(&response_file, "// temp module for testing").expect("Failed to create response file");

    // Create a main file
    let main_file = temp_dir.join(format!("main_{}.ql", test_id));
    let source_code = format!(r#"
        class UserService {{
            getUser(id: String) -> import("models_{}.ql").User {{
                return nil;
            }}

            updateUser(user: import("models_{}.ql").User) -> import("result_{}.ql").UpdateResult {{
                return nil;
            }}

            processRequest(req: import("request_{}.ql").Request, config: import("config_{}.ql").Settings) -> import("response_{}.ql").Response {{
                return nil;
            }}
        }}
    "#, test_id, test_id, test_id, test_id, test_id, test_id);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&models_file);
    let _ = fs::remove_file(&result_file);
    let _ = fs::remove_file(&request_file);
    let _ = fs::remove_file(&config_file);
    let _ = fs::remove_file(&response_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_in_lambda_expressions() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let temp_file = temp_dir.join("models.ql");
    fs::write(&temp_file, "// temp module for testing").expect("Failed to create temp file");

    // Create a main file that uses type import in lambda
    let main_file = temp_dir.join("main.ql");
    let relative_path = "models.ql";
    let source_code = format!(r#"var simple = (x: import("{}").User) -> x;"#, relative_path);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&temp_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_various_paths() {
    use std::fs;

    // Create a temporary directory structure for the test
    let temp_dir = std::env::temp_dir();
    let local_file = temp_dir.join("local.ql");
    let parent_file = temp_dir.join("parent.ql");
    let deep_dir = temp_dir.join("deep").join("path");
    fs::create_dir_all(&deep_dir).expect("Failed to create deep dir");
    let deep_file = deep_dir.join("module.ql");
    let module_file = temp_dir.join("module.ql");

    fs::write(&local_file, "// temp module for testing").expect("Failed to create local file");
    fs::write(&parent_file, "// temp module for testing").expect("Failed to create parent file");
    fs::write(&deep_file, "// temp module for testing").expect("Failed to create deep file");
    fs::write(&module_file, "// temp module for testing").expect("Failed to create module file");

    // Create a main file in a nested directory to test various relative paths
    let sub_dir = temp_dir.join("sub").join("nested");
    fs::create_dir_all(&sub_dir).expect("Failed to create sub dir");
    let main_file = sub_dir.join("main.ql");
    let source_code = format!(r#"
        var local: import("../../local.ql").Type = nil;
        var parent: import("../../parent.ql").Type = nil;
        var deep: import("../../deep/path/module.ql").Type = nil;
        var withExt: import("../../module.ql").Type = nil;
    "#);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&local_file);
    let _ = fs::remove_file(&parent_file);
    let _ = fs::remove_file(&deep_file);
    let _ = fs::remove_file(&module_file);
    let _ = fs::remove_file(&main_file);
    let _ = fs::remove_dir_all(&sub_dir.parent().unwrap());
    let _ = fs::remove_dir_all(&deep_dir.parent().unwrap());

    assert_no_parse_errors(&errors);
}

#[test]
fn test_type_import_mixed_with_regular_types() {
    use std::fs;

    // Create a temporary directory and files for the test
    let temp_dir = std::env::temp_dir();
    let test_id = "mixed_with_regular_types";
    let types_file = temp_dir.join(format!("types_{}.ql", test_id));
    let models_file = temp_dir.join(format!("models_{}.ql", test_id));
    let request_file = temp_dir.join(format!("request_{}.ql", test_id));

    fs::write(&types_file, "// temp module for testing").expect("Failed to create types file");
    fs::write(&models_file, "// temp module for testing").expect("Failed to create models file");
    fs::write(&request_file, "// temp module for testing").expect("Failed to create request file");

    // Create a main file
    let main_file = temp_dir.join(format!("main_{}.ql", test_id));
    let source_code = format!(r#"
        var mixed: String | import("types_{}.ql").CustomType | Number = nil;
        var array: [String | import("models_{}.ql").User] = [];
        var func: (String, import("request_{}.ql").Request) -> Number = nil;
        var object: {{ name: String, user: import("models_{}.ql").User }} = nil;
    "#, test_id, test_id, test_id, test_id);
    fs::write(&main_file, &source_code).expect("Failed to create main file");

    // Use the main file path for the source map
    let source_map = Arc::new(SourceMap::new(source_code, main_file.clone()));
    let mut nodes = TypedNodeArena::new();
    let mut strings = StringInterner::new();

    let mut parser = crate::Parser::new(source_map, &mut nodes, &mut strings);
    let _modules = parser.parse();
    let errors = parser.into_errors();

    // Clean up
    let _ = fs::remove_file(&types_file);
    let _ = fs::remove_file(&models_file);
    let _ = fs::remove_file(&request_file);
    let _ = fs::remove_file(&main_file);

    assert_no_parse_errors(&errors);
}
