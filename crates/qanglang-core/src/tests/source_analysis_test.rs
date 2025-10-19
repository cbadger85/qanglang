use crate::{GlobalCompilerPipeline, HeapAllocator, error::QangErrorKind};

#[test]
fn test_class_field_with_valid_constant_initializers() {
    let source = r#"
        class Foo {
            x = 42;
            y = "hello";
            z = true;
            w = false;
            v = nil;
        }
  "#;
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => {
            // Success expected - all field initializers are constants
        }
        Err(errors) => panic!(
            "Expected success but got errors: {:?}",
            errors.all().iter().map(|e| &e.message).collect::<Vec<_>>()
        ),
    }
}

#[test]
fn test_class_field_without_initializer() {
    let source = r#"
        class Foo {
            x;
            y;
        }
  "#;
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => {
            // Success expected - fields without initializers are allowed
        }
        Err(errors) => panic!(
            "Expected success but got errors: {:?}",
            errors.all().iter().map(|e| &e.message).collect::<Vec<_>>()
        ),
    }
}

#[test]
fn test_class_field_with_identifier_initializer() {
    let source = r#"
        class Foo {
            x = someVar;
        }
  "#;
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = &errors.all()[0].message;
            assert!(
                error_message.contains("Class field initializers can only contain constant values")
            );
        }
    }
}

#[test]
fn test_class_field_with_function_call_initializer() {
    let source = r#"
        class Foo {
            x = getValue();
        }
  "#;
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = &errors.all()[0].message;
            assert!(
                error_message.contains("Class field initializers can only contain constant values")
            );
        }
    }
}

#[test]
fn test_class_field_with_arithmetic_expression_initializer() {
    let source = r#"
        class Foo {
            x = 1 + 2;
        }
  "#;
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = &errors.all()[0].message;
            assert!(
                error_message.contains("Class field initializers can only contain constant values")
            );
        }
    }
}

#[test]
fn test_class_field_with_unary_expression_initializer() {
    let source = r#"
        class Foo {
            x = -5;
        }
  "#;
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = &errors.all()[0].message;
            assert!(
                error_message.contains("Class field initializers can only contain constant values")
            );
        }
    }
}

#[test]
fn test_initializing_local_variable_with_same_name() {
    let source = r#"
        {
            var a = "a";
            var a = "b";
        }
  "#;
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = &errors.all()[0].message;
            assert!(error_message.contains("Already a variable with this name in this scope."));
        }
    }
}

#[test]
fn test_initializing_local_variable_with_itself() {
    let source = r#"
        {
            var a = "outer";
            {
                var a = a;
            }
        }
  "#;
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = &errors.all()[0].message;
            assert!(
                error_message.contains("Cannot read local variable during its initialization.")
            );
        }
    }
}

#[test]
fn test_local_recursive_function_declaration() {
    let source = r#"
        fn main() {
            var count = 0;

            fn inner() {
                if (count > 20) {
                return;
                }

                inner();
            }
        }

        main();
    "#;

    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error = &errors.all()[0];
            let error_message = &error.message;
            assert!(
                error_message.contains("Cannot read local variable during its initialization.")
            );
            let error_kind = error.kind;
            assert!(matches!(error_kind, QangErrorKind::Analysis));
        }
    }
}

#[test]
fn test_global_recursive_function_declaration() {
    let source = r#"
        var count = 0;

        fn main() {
            if (count > 20) {
            return;
            }

            main();
        }
  "#;
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match GlobalCompilerPipeline::compile_source(source.to_string(), &mut allocator) {
        Ok(_) => {}
        Err(errors) => panic!(
            "Expected success but got errors: {:?}",
            errors.all().iter().map(|e| &e.message).collect::<Vec<_>>()
        ),
    }
}
