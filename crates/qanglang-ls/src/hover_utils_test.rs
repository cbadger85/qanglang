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

    #[test]
    fn test_hover_on_regular_field_in_assignment() {
        // First, let's test that regular field access in assignments works
        let source = r#"
class MyClass {
    test_field;

    init() {
        var x = this.test_field;
    }
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));
        let analysis = analyze(source_map.clone()).expect("Analysis should succeed");

        let lines: Vec<&str> = source.lines().collect();
        let line_5 = lines[5];
        eprintln!("Line 5: '{}'", line_5);
        let this_pos = line_5.find("this.test_field").expect("Should find this.test_field");
        let test_field_pos = this_pos + "this.".len();
        eprintln!("test_field starts at column {}", test_field_pos);

        let offset = source_map.position_to_offset(5, test_field_pos as u32).expect("Valid position");
        eprintln!("Offset for test_field in this.test_field: {}", offset);

        let node_info = find_node_at_offset(&analysis, offset);
        assert!(node_info.is_some(), "Should find node for this.test_field");
        eprintln!("Found: {:?}", node_info);
    }

    #[test]
    fn test_hover_on_field_in_assignment_statement() {
        // Test field access on the RIGHT side of an assignment STATEMENT (not variable declaration)
        let source = r#"
class MyClass {
    field1;
    field2;

    init() {
        this.field1 = this.field2;
    }
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));
        let analysis = analyze(source_map.clone()).expect("Analysis should succeed");

        let lines: Vec<&str> = source.lines().collect();
        let line_6 = lines[6];
        eprintln!("Line 6: '{}'", line_6);

        // Find field2 (on the right side)
        let field2_match = "this.field2";
        let field2_start = line_6.find(field2_match).expect("Should find this.field2");
        let field2_pos = field2_start + "this.".len();
        eprintln!("field2 starts at column {}", field2_pos);

        let offset = source_map.position_to_offset(6, field2_pos as u32).expect("Valid position");
        eprintln!("Offset for field2: {}", offset);

        // Try a few offsets around it
        for test_offset in (offset.saturating_sub(5))..=(offset+15) {
            let node = find_node_at_offset(&analysis, test_offset);
            if node.is_some() {
                eprintln!("  Offset {}: {:?}", test_offset, node);
            }
        }

        let node_info = find_node_at_offset(&analysis, offset);
        assert!(node_info.is_some(), "Should find node for field2 on right side of assignment statement");
        eprintln!("Found: {:?}", node_info);
    }

    #[test]
    fn test_super_ast_structure() {
        // Check what AST is actually generated for super.field
        let source = r#"
class Base {
    field;
}

class Child : Base {
    init() {
        var x = super.field;
    }
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));
        let analysis = analyze(source_map.clone()).expect("Should parse");

        use qanglang_core::nodes::{DeclNode, ClassMemberNode, ExprNode};

        // Navigate to the var declaration
        let module = analysis.nodes.get_program_node(analysis.root_module_id);
        let child_class_id = analysis.nodes.array.get_node_id_at(module.node.decls, 1).expect("Child class");
        let decl = analysis.nodes.get_decl_node(child_class_id);

        if let DeclNode::Class(class) = decl.node {
            let method_id = analysis.nodes.array.get_node_id_at(class.members, 0).expect("init method");
            let member = analysis.nodes.get_class_member_node(method_id);

            if let ClassMemberNode::Method(method) = member.node {
                let body = analysis.nodes.get_block_stmt_node(method.body);
                let var_decl_id = analysis.nodes.array.get_node_id_at(body.node.decls, 0).expect("var declaration");
                let var_decl_node = analysis.nodes.get_decl_node(var_decl_id);

                if let DeclNode::Variable(var_decl) = var_decl_node.node {
                    let init_id = var_decl.initializer.expect("Should have initializer");
                    let init_expr = analysis.nodes.get_expr_node(init_id);

                    eprintln!("=== AST Structure for 'super.field' ===");
                    eprintln!("Initializer expression type: {:?}", init_expr.node);
                    eprintln!("Discriminant: {:?}", std::mem::discriminant(&init_expr.node));

                    match init_expr.node {
                        ExprNode::Call(call) => {
                            eprintln!("✓ It's a Call expression");
                            eprintln!("  Call span: {:?}", call.span);

                            let callee = analysis.nodes.get_expr_node(call.callee);
                            eprintln!("  Callee type: {:?}", callee.node);

                            let op = analysis.nodes.get_call_operation_node(call.operation);
                            eprintln!("  Operation: {:?}", op.node);
                        }
                        ExprNode::Primary(primary) => {
                            eprintln!("✗ It's a Primary expression");
                            eprintln!("  Primary type: {:?}", primary);

                            use qanglang_core::nodes::PrimaryNode;
                            if let PrimaryNode::Super(super_expr) = primary {
                                eprintln!("  Super expression method NodeId: {:?}", super_expr.method);
                                eprintln!("  Super expression span: {:?}", super_expr.span);

                                // Try to get it as an identifier
                                let method_ident = analysis.nodes.get_identifier_node(super_expr.method);
                                let method_name = analysis.strings.get_string(method_ident.node.name);
                                eprintln!("  Method/field name: {}", method_name);
                                eprintln!("  Method/field identifier span: {:?}", method_ident.node.span);
                            }
                        }
                        other => {
                            eprintln!("✗ It's some other expression type: {:?}", other);
                        }
                    }
                } else {
                    panic!("Expected Variable declaration");
                }
            } else {
                panic!("Expected Method member");
            }
        } else {
            panic!("Expected Class declaration");
        }
    }

    #[test]
    fn test_simple_super_property() {
        // Simplest possible super property access
        let source = r#"
class Base {
    field;
}

class Child : Base {
    init() {
        var x = super.field;
    }
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));
        let analysis = analyze(source_map.clone());

        if analysis.is_err() {
            eprintln!("PARSE ERROR: {:?}", analysis.err());
            panic!("Failed to parse source with super.field");
        }

        let analysis = analysis.unwrap();

        let lines: Vec<&str> = source.lines().collect();
        let line_7 = lines[7];
        eprintln!("Line 7: '{}'", line_7);

        // Find "field" in "super.field"
        let super_field = line_7.find("super.field").expect("Should find super.field");
        let field_pos = super_field + "super.".len();
        eprintln!("field starts at column {}", field_pos);

        let offset = source_map.position_to_offset(7, field_pos as u32).expect("Valid position");
        eprintln!("Offset for field: {}", offset);

        // Check nodes around this offset
        eprintln!("Checking offsets:");
        for test_offset in (offset.saturating_sub(20))..=(offset+20) {
            let node = find_node_at_offset(&analysis, test_offset);
            if node.is_some() {
                eprintln!("  Offset {}: {:?}", test_offset, node);
            }
        }

        let node_info = find_node_at_offset(&analysis, offset);
        assert!(node_info.is_some(), "Should find node for super.field");
    }

    #[test]
    fn test_hover_on_super_field() {
        let source = r#"
class BaseClass {
    test_field;
}

class ChildClass : BaseClass {
    init() {
        this.value = super.test_field;
    }
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));

        let analysis = analyze(source_map.clone()).expect("Analysis should succeed");

        // Position of "test_field" in "super.test_field" (line 7, after "super.")
        // Let's find the exact position
        let lines: Vec<&str> = source.lines().collect();
        eprintln!("Source lines:");
        for (i, line) in lines.iter().enumerate() {
            eprintln!("  Line {}: '{}'", i, line);
        }

        // Line 7 (0-indexed) should be "        this.value = super.test_field;"
        let line_7 = lines[7];
        eprintln!("Line 7: '{}'", line_7);
        let super_pos = line_7.find("super.test_field").expect("Should find super.test_field");
        let test_field_pos = super_pos + "super.".len();
        eprintln!("super starts at column {}, test_field starts at column {}", super_pos, test_field_pos);

        let offset = source_map.position_to_offset(7, test_field_pos as u32).expect("Valid position");
        eprintln!("Offset for test_field in super.test_field: {}", offset);

        // Debug: Let's try finding nodes at different offsets to understand the span coverage
        for test_offset in (offset-10)..=(offset+10) {
            let node = find_node_at_offset(&analysis, test_offset);
            if node.is_some() {
                eprintln!("  Offset {}: {:?}", test_offset, node);
            }
        }

        let node_info = find_node_at_offset(&analysis, offset);

        assert!(node_info.is_some(), "Should find a node at super.test_field position");
        let info = node_info.unwrap();

        eprintln!("Found node: {:?}", info.kind);
        match info.kind {
            NodeKind::Field(name) => {
                assert_eq!(name, "test_field", "Should find field named test_field");
            }
            NodeKind::Function(name) => {
                assert_eq!(name, "test_field", "Should find method named test_field");
            }
            _ => panic!("Expected Field or Function node for super property, got {:?}", info.kind),
        }
    }

    #[test]
    fn test_hover_on_super_method() {
        let source = r#"
class BaseClass {
    baseMethod() {
        return 42;
    }
}

class ChildClass : BaseClass {
    init() {
        super.baseMethod();
    }
}
"#;
        let source_map = Arc::new(SourceMap::new(source.to_string(), PathBuf::from("test.ql")));

        let analysis = analyze(source_map.clone()).expect("Analysis should succeed");

        // Position of "baseMethod" in "super.baseMethod()" (line 9)
        let lines: Vec<&str> = source.lines().collect();
        let line_9 = lines[9];
        let super_pos = line_9.find("super.baseMethod").expect("Should find super.baseMethod");
        let base_method_pos = super_pos + "super.".len();

        let offset = source_map.position_to_offset(9, base_method_pos as u32).expect("Valid position");

        let node_info = find_node_at_offset(&analysis, offset);

        assert!(node_info.is_some(), "Should find a node at super.baseMethod position");
        let info = node_info.unwrap();

        match info.kind {
            NodeKind::Function(name) => {
                assert_eq!(name, "baseMethod", "Should find method named baseMethod");
            }
            _ => panic!("Expected Function node for super method, got {:?}", info.kind),
        }
    }
}
