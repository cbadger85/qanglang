use crate::{GlobalCompilerPipeline, HeapAllocator};

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
