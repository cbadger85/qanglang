#[cfg(test)]
mod tests {
    use crate::analyzer::analyze;
    use crate::completion::provide_completions;
    use qanglang_core::SourceMap;
    use std::path::PathBuf;
    use std::sync::Arc;
    use tower_lsp::lsp_types::Position;

    fn get_completion_labels(source: &str, line: u32, character: u32) -> Vec<String> {
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));

        let analysis_result = analyze(source_map.clone());
        let analysis = analysis_result.expect("Analysis should succeed");

        let position = Position { line, character };
        let offset_result = source_map.position_to_offset(line, character);

        if offset_result.is_none() {
            return Vec::new();
        }

        let completions = provide_completions(&analysis, position);

        if let Some(items) = completions {
            items.into_iter().map(|item| item.label).collect()
        } else {
            Vec::new()
        }
    }

    #[test]
    fn test_global_variable_completion() {
        let source = r#"var outer_variable;

class OuterClass { }

// Requesting completion here
"#;
        // Position at line 4, column 0 (after all declarations)
        let labels = get_completion_labels(source, 4, 0);

        assert!(
            labels.contains(&"outer_variable".to_string()),
            "Variable 'outer_variable' should appear in completions. Found: {:?}",
            labels
        );
        assert!(
            labels.contains(&"OuterClass".to_string()),
            "Class 'OuterClass' should appear in completions. Found: {:?}",
            labels
        );
    }

    #[test]
    fn test_inner_variable_completion() {
        let source = r#"fn function() {
    var inner_variable;

    // Requesting completion here
}
"#;
        // Position at line 3, column 4 (inside function, after variable declaration)
        let labels = get_completion_labels(source, 3, 4);

        assert!(
            labels.contains(&"inner_variable".to_string()),
            "Variable 'inner_variable' should appear in completions. Found: {:?}",
            labels
        );
    }

    #[test]
    fn test_inner_class_completion() {
        let source = r#"fn function() {
    class InnerClass { }

    // Requesting completion here
}
"#;
        // Position at line 3, column 4 (inside function, after class declaration)
        let labels = get_completion_labels(source, 3, 4);

        assert!(
            labels.contains(&"InnerClass".to_string()),
            "Class 'InnerClass' should appear in completions. Found: {:?}",
            labels
        );
    }

    #[test]
    fn test_inner_function_completion() {
        let source = r#"fn function() {
    fn inner_function() {}

    // Requesting completion here
}
"#;
        // Position at line 3, column 4 (inside function, after inner function declaration)
        let labels = get_completion_labels(source, 3, 4);

        assert!(
            labels.contains(&"inner_function".to_string()),
            "Function 'inner_function' should appear in completions. Found: {:?}",
            labels
        );
    }

    #[test]
    fn test_inner_lambda_completion() {
        let source = r#"fn function() {
    var myLambda;

    // Requesting completion here
}
"#;
        // Position at line 3, column 4 (inside function, after lambda declaration)
        let labels = get_completion_labels(source, 3, 4);

        assert!(
            labels.contains(&"myLambda".to_string()),
            "Variable 'myLambda' should appear in completions. Found: {:?}",
            labels
        );
    }

    #[test]
    fn test_all_inner_declarations() {
        let source = r#"fn function() {
    var inner_variable;
    class InnerClass { }
    fn inner_function() {}
    var another_variable;

    // Requesting completion here
}
"#;
        // Position at line 6, column 4 (inside function, after all declarations)
        let labels = get_completion_labels(source, 6, 4);

        assert!(
            labels.contains(&"inner_variable".to_string()),
            "Variable 'inner_variable' should appear. Found: {:?}",
            labels
        );
        assert!(
            labels.contains(&"InnerClass".to_string()),
            "Class 'InnerClass' should appear. Found: {:?}",
            labels
        );
        assert!(
            labels.contains(&"inner_function".to_string()),
            "Function 'inner_function' should appear. Found: {:?}",
            labels
        );
        assert!(
            labels.contains(&"another_variable".to_string()),
            "Variable 'another_variable' should appear. Found: {:?}",
            labels
        );
    }

    #[test]
    fn test_no_hoisting_in_nested_scope() {
        let source = r#"fn function() {
    // Requesting completion here (before declarations)

    var inner_variable;
    fn inner_function() {}
}
"#;
        // Position at line 1, column 4 (inside function, BEFORE declarations)
        let labels = get_completion_labels(source, 1, 4);

        assert!(
            !labels.contains(&"inner_variable".to_string()),
            "Variable 'inner_variable' should NOT appear before declaration. Found: {:?}",
            labels
        );
        assert!(
            !labels.contains(&"inner_function".to_string()),
            "Function 'inner_function' should NOT appear before declaration (no hoisting in nested scope). Found: {:?}",
            labels
        );
    }

    #[test]
    fn test_global_hoisting_functions_and_classes() {
        let source = r#"// Requesting completion here (before declarations)

fn myFunction() {}
class MyClass {}
var myVariable;
"#;
        // Position at line 0, column 0 (BEFORE declarations)
        let labels = get_completion_labels(source, 0, 0);

        assert!(
            labels.contains(&"myFunction".to_string()),
            "Function 'myFunction' should appear (hoisted at global scope). Found: {:?}",
            labels
        );
        assert!(
            labels.contains(&"MyClass".to_string()),
            "Class 'MyClass' should appear (hoisted at global scope). Found: {:?}",
            labels
        );
        assert!(
            !labels.contains(&"myVariable".to_string()),
            "Variable 'myVariable' should NOT appear before declaration (not hoisted). Found: {:?}",
            labels
        );
    }

    #[test]
    fn test_complete_example() {
        let source = r#"var outer_variable;

class OuterClass { }

fn function() {
    var inner_variable;

    class InnerClass { }

    fn inner_function() {}

    // Requesting completion here (inside function)
}

// Requesting completion here (global scope)
"#;
        // Test inside function at line 11
        let inner_labels = get_completion_labels(source, 11, 4);

        assert!(
            inner_labels.contains(&"inner_variable".to_string()),
            "inner_variable should appear inside function"
        );
        assert!(
            inner_labels.contains(&"InnerClass".to_string()),
            "InnerClass should appear inside function"
        );
        assert!(
            inner_labels.contains(&"inner_function".to_string()),
            "inner_function should appear inside function"
        );

        // Also outer scope should be visible
        assert!(
            inner_labels.contains(&"outer_variable".to_string()),
            "outer_variable should be visible from inner scope"
        );
        assert!(
            inner_labels.contains(&"OuterClass".to_string()),
            "OuterClass should be visible from inner scope"
        );
        assert!(
            inner_labels.contains(&"function".to_string()),
            "function should be visible from inner scope"
        );

        // Test at global scope at line 14
        let global_labels = get_completion_labels(source, 14, 0);

        assert!(
            global_labels.contains(&"outer_variable".to_string()),
            "outer_variable should appear at global scope"
        );
        assert!(
            global_labels.contains(&"OuterClass".to_string()),
            "OuterClass should appear at global scope"
        );
        assert!(
            global_labels.contains(&"function".to_string()),
            "function should appear at global scope"
        );

        // Inner scope items should NOT appear at global scope
        assert!(
            !global_labels.contains(&"inner_variable".to_string()),
            "inner_variable should NOT appear at global scope"
        );
        assert!(
            !global_labels.contains(&"InnerClass".to_string()),
            "InnerClass should NOT appear at global scope"
        );
    }

    #[test]
    fn test_completion_before_and_after_comment() {
        let source = r#"var outer_variable;

class OuterClass { }

fn function() {
    var inner_variable;
}




// everything here autocompletes correctly except module.

// no autocomplete here anymore.

"#;
        // Test before the comment (line 10)
        let labels_before = get_completion_labels(source, 10, 0);
        assert!(
            labels_before.contains(&"outer_variable".to_string()),
            "outer_variable should appear before comment"
        );

        // Test after first comment (line 12)
        let labels_after_first = get_completion_labels(source, 12, 0);
        assert!(
            labels_after_first.contains(&"outer_variable".to_string()),
            "outer_variable should appear after first comment. Found: {:?}",
            labels_after_first
        );

        // Test after second comment (line 14)
        let labels_after_second = get_completion_labels(source, 14, 0);
        assert!(
            labels_after_second.contains(&"outer_variable".to_string()),
            "outer_variable should appear after second comment. Found: {:?}",
            labels_after_second
        );
    }
}
