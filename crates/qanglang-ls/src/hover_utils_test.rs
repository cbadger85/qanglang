#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use std::path::PathBuf;
    use qanglang_core::SourceMap;
    use crate::analyzer::analyze;
    use crate::hover_utils::{find_node_at_offset, NodeKind};

    #[test]
    fn test_hover_on_class_name() {
        let source = r#"
class Person {
    name;
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));

        let analysis = analyze(source_map.clone()).expect("Analysis should succeed");

        // Position of "Person" (line 1, column 6) - LSP uses 0-based indexing
        // Line 0 is empty, line 1 has "class Person {"
        let offset = source_map.position_to_offset(1, 6).expect("Valid position");

        let node_info = find_node_at_offset(&analysis, offset);

        assert!(node_info.is_some(), "Should find a node at class name position");
        let info = node_info.unwrap();

        match info.kind {
            NodeKind::Class(name) => {
                assert_eq!(name, "Person", "Should find class named Person");
            }
            _ => panic!("Expected Class node, got {:?}", info.kind),
        }
    }

    #[test]
    fn test_hover_on_function_name() {
        let source = r#"
fn calculateSum(a, b) {
    return a + b;
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));

        let analysis = analyze(source_map.clone()).expect("Analysis should succeed");

        // Position of "calculateSum" (line 1, column 3)
        let offset = source_map.position_to_offset(1, 3).expect("Valid position");

        let node_info = find_node_at_offset(&analysis, offset);

        assert!(node_info.is_some(), "Should find a node at function name position");
        let info = node_info.unwrap();

        match info.kind {
            NodeKind::Function(name) => {
                assert_eq!(name, "calculateSum", "Should find function named calculateSum");
            }
            _ => panic!("Expected Function node, got {:?}", info.kind),
        }
    }

    #[test]
    fn test_hover_on_parameter() {
        let source = r#"
fn calculateSum(a, b) {
    return a + b;
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));

        let analysis = analyze(source_map.clone()).expect("Analysis should succeed");

        // Position of parameter "a" (line 1, column 16)
        let offset = source_map.position_to_offset(1, 16).expect("Valid position");

        let node_info = find_node_at_offset(&analysis, offset);

        assert!(node_info.is_some(), "Should find a node at parameter position");
        let info = node_info.unwrap();

        match info.kind {
            NodeKind::Parameter(name) => {
                assert_eq!(name, "a", "Should find parameter named a");
            }
            _ => panic!("Expected Parameter node, got {:?}", info.kind),
        }
    }

    #[test]
    fn test_hover_on_variable() {
        let source = r#"
fn test() {
    var result = 42;
    return result;
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));

        let analysis = analyze(source_map.clone()).expect("Analysis should succeed");

        // Position of "result" (line 2, column 8)
        let offset = source_map.position_to_offset(2, 8).expect("Valid position");

        let node_info = find_node_at_offset(&analysis, offset);

        assert!(node_info.is_some(), "Should find a node at variable position");
        let info = node_info.unwrap();

        match info.kind {
            NodeKind::Variable(name) => {
                assert_eq!(name, "result", "Should find variable named result");
            }
            _ => panic!("Expected Variable node, got {:?}", info.kind),
        }
    }

    #[test]
    fn test_position_to_offset_basic() {
        let source = "line1\nline2\nline3";
        let source_map = SourceMap::new(source.to_string(), PathBuf::from("test.ql"));

        // Line 0, column 0 should be offset 0 ('l' in "line1")
        assert_eq!(source_map.position_to_offset(0, 0), Some(0));

        // Line 0, column 4 should be offset 4 ('1' in "line1")
        assert_eq!(source_map.position_to_offset(0, 4), Some(4));

        // Line 1, column 0 should be offset 6 ('l' in "line2")
        assert_eq!(source_map.position_to_offset(1, 0), Some(6));

        // Line 2, column 0 should be offset 12 ('l' in "line3")
        assert_eq!(source_map.position_to_offset(2, 0), Some(12));
    }
}
