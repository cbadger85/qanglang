use crate::{CompilerPipeline, HeapAllocator, SourceMap};

#[test]
fn test_initializing_local_variable_with_same_name() {
    let source = r#"
        {
            var a = "a";
            var a = "b";
        }
  "#;
    let source_map = SourceMap::from_source(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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
    let source_map = SourceMap::from_source(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
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
fn test_missing_super_init_call() {
    let source = r#"
        class Parent {
            init() {
                println("Parent init");
            }
        }

        class Child : Parent {
            init() {
                println("Child init - missing super.init()");
            }
        }
    "#;
    let source_map = SourceMap::from_source(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
        Ok(_) => panic!("Expected failure but found none."),
        Err(errors) => {
            assert_eq!(errors.all().len(), 1);
            let error_message = &errors.all()[0].message;
            assert!(error_message.contains("Constructor must call 'super.init()' when parent class has an init method"));
        }
    }
}

#[test]
fn test_valid_super_init_call() {
    let source = r#"
        class Parent {
            init() {
                println("Parent init");
            }
        }

        class Child : Parent {
            init() {
                super.init();
                println("Child init");
            }
        }
    "#;
    let source_map = SourceMap::from_source(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
        Ok(_) => {
            // Should succeed without errors
        }
        Err(errors) => {
            panic!("Expected success but got errors: {:?}", errors.all());
        }
    }
}

#[test]
fn test_no_parent_init_no_error() {
    let source = r#"
        class Parent {
            some_method() {
                println("Parent method");
            }
        }

        class Child : Parent {
            init() {
                println("Child init - no parent init, so no super.init() required");
            }
        }
    "#;
    let source_map = SourceMap::from_source(source.to_string());
    let mut allocator: HeapAllocator = HeapAllocator::new();

    match CompilerPipeline::new().compile(source_map, &mut allocator) {
        Ok(_) => {
            // Should succeed without errors
        }
        Err(errors) => {
            panic!("Expected success but got errors: {:?}", errors.all());
        }
    }
}
