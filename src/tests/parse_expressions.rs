use super::{assert_no_parse_errors, parse_source};
use crate::{SourceMap, ast};

#[test]
fn test_arithmetic_expressions() {
    let source_code = r#"var result = a + b * c - d / e % f;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert!(var_decl.initializer.is_some());
        // TODO write tests to assert all nodes in the AST are assembled correctly.
    }
}

#[test]
fn test_comparison_expressions() {
    let source_code = r#"var check = x > y and a <= b or c != d and e == f;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        // TODO write tests to assert all nodes in the AST are assembled correctly.
        assert!(var_decl.initializer.is_some());
    }
}

#[test]
fn test_unary_expressions() {
    let source_code = r#"var result = !condition and -number;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        // TODO write tests to assert all nodes in the AST are assembled correctly.
        assert!(var_decl.initializer.is_some());
    }
}

#[test]
fn test_assignment_expressions() {
    let source_code = r#"
            x = 5;
            obj.property = "value";
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 2);

    // Both should be expression statements containing assignments
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[0] {
        if let ast::Expr::Assignment(_) = expr_stmt.expr {
            // TODO tests for expected assignment
        } else {
            panic!("Expected assignment expression");
        }
    }

    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[1] {
        if let ast::Expr::Assignment(_) = expr_stmt.expr {
            // TODO test for expected assignment
        } else {
            panic!("Expected assignment expression");
        }
    }
}

#[test]
fn test_ternary_expressions() {
    let source_code = r#"var result = condition ? trueValue : falseValue;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        if let Some(ast::Expr::Ternary(ternary)) = &var_decl.initializer {
            // TODO test that the nodes in the AST are correct.
            assert!(ternary.then_expr.is_some());
            assert!(ternary.else_expr.is_some());
        } else {
            panic!("Expected ternary expression");
        }
    }
}

#[test]
fn test_pipe_expressions() {
    let source_code = r#"var result = value |> transform |> finalize;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        if let Some(ast::Expr::Pipe(_)) = &var_decl.initializer {
            // TODO test for expected pipe expression
        } else {
            panic!("Expected pipe expression");
        }
    }
}

#[test]
fn test_function_calls() {
    let source_code = r#"
            result = func();
            result2 = func(a, b, c);
            result3 = obj.method(arg);
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 3);

    // TODO write tests to ensure all declaration nodes are assembled correctly.
}

#[test]
fn test_property_access() {
    let source_code = r#"
            value = obj.property;
            value2 = obj.nested.deep.property;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 2);
    // TODO write tests to ensure all declaration nodes are assembled correctly.
}

#[test]
fn test_optional_chaining() {
    let source_code = r#"
            value = obj.?property;
            value2 = obj.?method().?result;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 2);
    // TODO write tests to ensure all declaration nodes are assembled correctly.
}

#[test]
fn test_array_literals() {
    let source_code = r#"
            empty = [];
            numbers = [1, 2, 3, 4];
            mixed = [1, "string", true, nil];
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 3);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Array(array))) = &var_decl.initializer {
            assert_eq!(array.elements.len(), 0);
        } else {
            panic!("Expected array literal");
        }
    }

    if let ast::Decl::Variable(var_decl) = &program.decls[1] {
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Array(array))) = &var_decl.initializer {
            // TODO write tests to ensure all items in the array are in the AST correctly.
            assert_eq!(array.elements.len(), 4);
        } else {
            panic!("Expected array literal");
        }
    }
}

#[test]
fn test_array_access() {
    let source_code = r#"
            value = array[0];
            value2 = matrix[row][col];
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 2);
    // TODO write tests to ensure all declaration nodes are assembled correctly.
}

#[test]
fn test_grouping_expressions() {
    let source_code = r#"var result = (a + b) * (c - d);"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert!(var_decl.initializer.is_some());
        // TODO write tests to ensure all declaration nodes are assembled correctly.
    }
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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 5);

    // Check number literal
    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Number(num))) = &var_decl.initializer {
            assert_eq!(num.value, 42.5);
        } else {
            panic!("Expected number literal");
        }
    }

    // Check string literal
    if let ast::Decl::Variable(var_decl) = &program.decls[1] {
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::String(str))) = &var_decl.initializer {
            assert_eq!(str.value.as_ref(), "hello world");
        } else {
            panic!("Expected string literal");
        }
    }

    // Check boolean literals
    if let ast::Decl::Variable(var_decl) = &program.decls[2] {
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Boolean(bool))) = &var_decl.initializer {
            assert_eq!(bool.value, true);
        } else {
            panic!("Expected boolean literal");
        }
    }

    if let ast::Decl::Variable(var_decl) = &program.decls[3] {
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Boolean(bool))) = &var_decl.initializer {
            assert_eq!(bool.value, false);
        } else {
            panic!("Expected boolean literal");
        }
    }

    // Check nil literal
    if let ast::Decl::Variable(var_decl) = &program.decls[4] {
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Nil(_))) = &var_decl.initializer {
            // Expected nil
        } else {
            panic!("Expected nil literal");
        }
    }
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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Class(class_decl) = &program.decls[0] {
        if let ast::ClassMember::Method(method) = &class_decl.members[0] {
            // TODO write tests to ensure all nodes are assembled correctly.
            assert_eq!(method.body.decls.len(), 1);
        }
    }
}

#[test]
fn test_complex_nested_expression() {
    let source_code = r#"
            var result = obj.method(a + b * c).property[index].?optional |> transform;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert!(var_decl.initializer.is_some());
        // TODO write tests to ensure all nodes are assembled correctly.
    }
}

#[test]
fn test_nested_function_calls() {
    let source_code = r#"
            var result = outer(inner(deep(value)));
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert!(var_decl.initializer.is_some());
        // TODO write tests to ensure all nodes are assembled correctly.
    }
}

#[test]
fn test_operator_precedence() {
    let source_code = r#"
            var result = a + b * c == d - e / f;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        // Should be parsed as: (a + (b * c)) == (d - (e / f))
        if let Some(ast::Expr::Equality(_)) = &var_decl.initializer {
            // TODO write tests to ensure all nodes are assembled correctly.
        } else {
            panic!("Expected equality expression at top level");
        }
    }
}

#[test]
fn test_right_associative_ternary() {
    let source_code = r#"
            var result = a ? b ? c : d : e;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        // Should be parsed as: a ? (b ? c : d) : e
        if let Some(ast::Expr::Ternary(_)) = &var_decl.initializer {
            // TODO write tests to ensure all nodes are assembled correctly.
        } else {
            panic!("Expected ternary expression");
        }
    }
}

#[test]
fn test_chained_method_calls() {
    let source_code = r#"
            var result = obj.method1().method2().method3();
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        // TODO write tests to ensure all nodes are assembled correctly.
        assert!(var_decl.initializer.is_some());
    }
}

#[test]
fn test_mixed_property_and_method_access() {
    let source_code = r#"
            var result = obj.property.method().field[index];
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert!(var_decl.initializer.is_some());
        // TODO write tests to ensure all nodes are assembled correctly.
    }
}

#[test]
fn test_complex_array_access() {
    let source_code = r#"
            var result = matrix[row + 1][col - 1];
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert!(var_decl.initializer.is_some());
        // TODO write tests to ensure all nodes are assembled correctly.
    }
}

#[test]
fn test_assignment_chaining() {
    let source_code = r#"
            a = b = c = 5;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[0] {
        if let ast::Expr::Assignment(_) = expr_stmt.expr {
            // TODO test for expected assignment expression
        } else {
            panic!("Expected assignment expression");
        }
    }
}

#[test]
fn test_nested_ternary_expressions() {
    let source_code = r#"
            var result = a ? b : c ? d : e ? f : g;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        if let Some(ast::Expr::Ternary(_)) = &var_decl.initializer {
            // TODO write tests to ensure all nodes are assembled correctly.
        } else {
            panic!("Expected ternary expression");
        }
    }
}

#[test]
fn test_complex_boolean_logic() {
    let source_code = r#"
            var condition = !a and (b or c) and !(d or e);
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert!(var_decl.initializer.is_some());
        // TODO write tests to ensure all nodes are assembled correctly.
    }
}

#[test]
fn test_pipe_operator_precedence() {
    let source_code = r#"
            var result = value + 1 |> transform |> process - 2;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        // TODO write tests to ensure all nodes are assembled correctly.
        assert!(var_decl.initializer.is_some());
    }
}

#[test]
fn test_deeply_nested_expressions() {
    let source_code = r#"
            var result = ((((a + b) * c) - d) / e) % f;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert!(var_decl.initializer.is_some());
    }
}
