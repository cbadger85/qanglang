use super::{assert_no_parse_errors, parse_source};
use crate::{SourceMap, ast};

// Helper function to get the name from a VariableDecl target
fn get_variable_name(var_decl: &ast::VariableDecl) -> &str {
    match &var_decl.target {
        ast::VariableTarget::Identifier(id) => &id.name,
        ast::VariableTarget::Destructure(_) => panic!("Destructuring not expected in these tests"),
    }
}

// Helper function to get the name from a Parameter
fn get_parameter_name(param: &ast::Parameter) -> &str {
    match param {
        ast::Parameter::Identifier(id) => &id.name,
        ast::Parameter::Destructure(_) => panic!("Destructuring not expected in these tests"),
    }
}

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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 4);

    // First object: empty_obj = :{}
    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "empty_obj");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::ObjectLiteral(object))) =
            &var_decl.initializer
        {
            assert_eq!(object.entries.len(), 0);
        } else {
            panic!("Expected empty object literal");
        }
    } else {
        panic!("Expected variable declaration for empty_obj");
    }

    // Second object: basic_obj = {{ field_1 = 1, field_2 = 2 }}
    if let ast::Decl::Variable(var_decl) = &program.decls[1] {
        assert_eq!(get_variable_name(var_decl), "basic_obj");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::ObjectLiteral(object))) =
            &var_decl.initializer
        {
            assert_eq!(object.entries.len(), 2);

            // First entry: field_1 = 1
            assert_eq!(object.entries[0].key.name.as_ref(), "field_1");
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num)) =
                object.entries[0].value.as_ref()
            {
                assert_eq!(num.value, 1.0);
            } else {
                panic!("Expected number literal '1' for field_1");
            }

            // Second entry: field_2 = 2
            assert_eq!(object.entries[1].key.name.as_ref(), "field_2");
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num)) =
                object.entries[1].value.as_ref()
            {
                assert_eq!(num.value, 2.0);
            } else {
                panic!("Expected number literal '2' for field_2");
            }
        } else {
            panic!("Expected object literal for basic_obj");
        }
    } else {
        panic!("Expected variable declaration for basic_obj");
    }

    // Third declaration: other_field = "other value"
    if let ast::Decl::Variable(var_decl) = &program.decls[2] {
        assert_eq!(get_variable_name(var_decl), "other_field");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::String(str_lit))) = &var_decl.initializer {
            assert_eq!(str_lit.value.as_ref(), "other value");
        } else {
            panic!("Expected string literal for other_field");
        }
    } else {
        panic!("Expected variable declaration for other_field");
    }

    // Fourth object: obj = {{ field = "value", other_field }}
    if let ast::Decl::Variable(var_decl) = &program.decls[3] {
        assert_eq!(get_variable_name(var_decl), "obj");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::ObjectLiteral(object))) =
            &var_decl.initializer
        {
            assert_eq!(object.entries.len(), 2);

            // First entry: field = "value"
            assert_eq!(object.entries[0].key.name.as_ref(), "field");
            if let ast::Expr::Primary(ast::PrimaryExpr::String(str_lit)) =
                object.entries[0].value.as_ref()
            {
                assert_eq!(str_lit.value.as_ref(), "value");
            } else {
                panic!("Expected string literal '\"value\"' for field");
            }

            // Second entry: other_field (shorthand)
            assert_eq!(object.entries[1].key.name.as_ref(), "other_field");
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(id)) =
                object.entries[1].value.as_ref()
            {
                assert_eq!(id.name.as_ref(), "other_field");
            } else {
                panic!("Expected identifier 'other_field' for shorthand property");
            }
        } else {
            panic!("Expected object literal for obj");
        }
    } else {
        panic!("Expected variable declaration for obj");
    }
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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 3);

    // First object: basic_obj = {{ field_1 = 1, field_2 = 2, }}
    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "basic_obj");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::ObjectLiteral(object))) =
            &var_decl.initializer
        {
            assert_eq!(object.entries.len(), 2);

            // First entry: field_1 = 1
            assert_eq!(object.entries[0].key.name.as_ref(), "field_1");
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num)) =
                object.entries[0].value.as_ref()
            {
                assert_eq!(num.value, 1.0);
            } else {
                panic!("Expected number literal '1' for field_1");
            }

            // Second entry: field_2 = 2
            assert_eq!(object.entries[1].key.name.as_ref(), "field_2");
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num)) =
                object.entries[1].value.as_ref()
            {
                assert_eq!(num.value, 2.0);
            } else {
                panic!("Expected number literal '2' for field_2");
            }
        } else {
            panic!("Expected object literal for basic_obj");
        }
    } else {
        panic!("Expected variable declaration for basic_obj");
    }

    // Second declaration: other_field = "other value"
    if let ast::Decl::Variable(var_decl) = &program.decls[1] {
        assert_eq!(get_variable_name(var_decl), "other_field");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::String(str_lit))) = &var_decl.initializer {
            assert_eq!(str_lit.value.as_ref(), "other value");
        } else {
            panic!("Expected string literal for other_field");
        }
    } else {
        panic!("Expected variable declaration for other_field");
    }

    // Third object: shorthand_obj = {{ field = "value", other_field, }}
    if let ast::Decl::Variable(var_decl) = &program.decls[2] {
        assert_eq!(get_variable_name(var_decl), "shorthand_obj");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::ObjectLiteral(object))) =
            &var_decl.initializer
        {
            assert_eq!(object.entries.len(), 2);

            // First entry: field = "value"
            assert_eq!(object.entries[0].key.name.as_ref(), "field");
            if let ast::Expr::Primary(ast::PrimaryExpr::String(str_lit)) =
                object.entries[0].value.as_ref()
            {
                assert_eq!(str_lit.value.as_ref(), "value");
            } else {
                panic!("Expected string literal '\"value\"' for field");
            }

            // Second entry: other_field (shorthand with trailing comma)
            assert_eq!(object.entries[1].key.name.as_ref(), "other_field");
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(id)) =
                object.entries[1].value.as_ref()
            {
                assert_eq!(id.name.as_ref(), "other_field");
            } else {
                panic!("Expected identifier 'other_field' for shorthand property");
            }
        } else {
            panic!("Expected object literal for shorthand_obj");
        }
    } else {
        panic!("Expected variable declaration for shorthand_obj");
    }
}

#[test]
fn test_arithmetic_expressions() {
    let source_code = r#"var result = a + b * c - d / e % f;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        // Expression should be: a + b * c - d / e % f
        // This should parse as: a + (b * c) - (d / e) % f
        // Which is: ((a + (b * c)) - ((d / e) % f))
        if let Some(ast::Expr::Term(term_expr)) = &var_decl.initializer {
            // Top level should be subtraction: (a + (b * c)) - ((d / e) % f)
            assert_eq!(term_expr.operator, ast::TermOperator::Subtract);

            // Left side: a + (b * c)
            if let ast::Expr::Term(left_term) = term_expr.left.as_ref() {
                assert_eq!(left_term.operator, ast::TermOperator::Add);

                // Left side of addition should be identifier 'a'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) =
                    left_term.left.as_ref()
                {
                    assert_eq!(a_id.name.as_ref(), "a");
                } else {
                    panic!("Expected identifier 'a'");
                }

                // Right side of addition should be b * c
                if let ast::Expr::Factor(factor_expr) = left_term.right.as_ref() {
                    assert_eq!(factor_expr.operator, ast::FactorOperator::Multiply);

                    // Left side should be 'b'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) =
                        factor_expr.left.as_ref()
                    {
                        assert_eq!(b_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b'");
                    }

                    // Right side should be 'c'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) =
                        factor_expr.right.as_ref()
                    {
                        assert_eq!(c_id.name.as_ref(), "c");
                    } else {
                        panic!("Expected identifier 'c'");
                    }
                } else {
                    panic!("Expected factor expression (b * c)");
                }
            } else {
                panic!("Expected term expression (a + (b * c))");
            }

            // Right side: (d / e) % f
            if let ast::Expr::Factor(right_factor) = term_expr.right.as_ref() {
                assert_eq!(right_factor.operator, ast::FactorOperator::Modulo);

                // Left side should be d / e
                if let ast::Expr::Factor(div_expr) = right_factor.left.as_ref() {
                    assert_eq!(div_expr.operator, ast::FactorOperator::Divide);

                    // Left side should be 'd'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(d_id)) =
                        div_expr.left.as_ref()
                    {
                        assert_eq!(d_id.name.as_ref(), "d");
                    } else {
                        panic!("Expected identifier 'd'");
                    }

                    // Right side should be 'e'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(e_id)) =
                        div_expr.right.as_ref()
                    {
                        assert_eq!(e_id.name.as_ref(), "e");
                    } else {
                        panic!("Expected identifier 'e'");
                    }
                } else {
                    panic!("Expected factor expression (d / e)");
                }

                // Right side should be 'f'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(f_id)) =
                    right_factor.right.as_ref()
                {
                    assert_eq!(f_id.name.as_ref(), "f");
                } else {
                    panic!("Expected identifier 'f'");
                }
            } else {
                panic!("Expected factor expression ((d / e) % f)");
            }
        } else {
            panic!("Expected term expression for arithmetic expression");
        }
    }
}

#[test]
fn test_comparison_expressions() {
    let source_code = r#"var check = x > y and a <= b or c != d and e == f;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "check");
        assert!(var_decl.initializer.is_some());

        // Expression should be: x > y and a <= b or c != d and e == f
        // This should parse as: ((x > y) and (a <= b)) or ((c != d) and (e == f))
        if let Some(ast::Expr::LogicalOr(or_expr)) = &var_decl.initializer {
            // Left side: (x > y) and (a <= b)
            if let ast::Expr::LogicalAnd(left_and) = or_expr.left.as_ref() {
                // Left side of first AND: x > y
                if let ast::Expr::Comparison(comp1) = left_and.left.as_ref() {
                    assert_eq!(comp1.operator, ast::ComparisonOperator::Greater);

                    // Left side should be 'x'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(x_id)) =
                        comp1.left.as_ref()
                    {
                        assert_eq!(x_id.name.as_ref(), "x");
                    } else {
                        panic!("Expected identifier 'x'");
                    }

                    // Right side should be 'y'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(y_id)) =
                        comp1.right.as_ref()
                    {
                        assert_eq!(y_id.name.as_ref(), "y");
                    } else {
                        panic!("Expected identifier 'y'");
                    }
                } else {
                    panic!("Expected comparison expression (x > y)");
                }

                // Right side of first AND: a <= b
                if let ast::Expr::Comparison(comp2) = left_and.right.as_ref() {
                    assert_eq!(comp2.operator, ast::ComparisonOperator::LessEqual);

                    // Left side should be 'a'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) =
                        comp2.left.as_ref()
                    {
                        assert_eq!(a_id.name.as_ref(), "a");
                    } else {
                        panic!("Expected identifier 'a'");
                    }

                    // Right side should be 'b'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) =
                        comp2.right.as_ref()
                    {
                        assert_eq!(b_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b'");
                    }
                } else {
                    panic!("Expected comparison expression (a <= b)");
                }
            } else {
                panic!("Expected logical AND expression on left side");
            }

            // Right side: (c != d) and (e == f)
            if let ast::Expr::LogicalAnd(right_and) = or_expr.right.as_ref() {
                // Left side of second AND: c != d
                if let ast::Expr::Equality(eq1) = right_and.left.as_ref() {
                    assert_eq!(eq1.operator, ast::EqualityOperator::NotEqual);

                    // Left side should be 'c'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) =
                        eq1.left.as_ref()
                    {
                        assert_eq!(c_id.name.as_ref(), "c");
                    } else {
                        panic!("Expected identifier 'c'");
                    }

                    // Right side should be 'd'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(d_id)) =
                        eq1.right.as_ref()
                    {
                        assert_eq!(d_id.name.as_ref(), "d");
                    } else {
                        panic!("Expected identifier 'd'");
                    }
                } else {
                    panic!("Expected equality expression (c != d)");
                }

                // Right side of second AND: e == f
                if let ast::Expr::Equality(eq2) = right_and.right.as_ref() {
                    assert_eq!(eq2.operator, ast::EqualityOperator::Equal);

                    // Left side should be 'e'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(e_id)) =
                        eq2.left.as_ref()
                    {
                        assert_eq!(e_id.name.as_ref(), "e");
                    } else {
                        panic!("Expected identifier 'e'");
                    }

                    // Right side should be 'f'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(f_id)) =
                        eq2.right.as_ref()
                    {
                        assert_eq!(f_id.name.as_ref(), "f");
                    } else {
                        panic!("Expected identifier 'f'");
                    }
                } else {
                    panic!("Expected equality expression (e == f)");
                }
            } else {
                panic!("Expected logical AND expression on right side");
            }
        } else {
            panic!("Expected logical OR expression for comparison expression");
        }
    }
}

#[test]
fn test_unary_expressions() {
    let source_code = r#"var result = !condition and -number;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        // Expression should be: !condition and -number
        // This should parse as: (!condition) and (-number)
        if let Some(ast::Expr::LogicalAnd(and_expr)) = &var_decl.initializer {
            // Left side: !condition
            if let ast::Expr::Unary(left_unary) = and_expr.left.as_ref() {
                assert_eq!(left_unary.operator, ast::UnaryOperator::Not);

                // Operand should be identifier 'condition'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(condition_id)) =
                    left_unary.operand.as_ref()
                {
                    assert_eq!(condition_id.name.as_ref(), "condition");
                } else {
                    panic!("Expected identifier 'condition'");
                }
            } else {
                panic!("Expected unary expression (!condition)");
            }

            // Right side: -number
            if let ast::Expr::Unary(right_unary) = and_expr.right.as_ref() {
                assert_eq!(right_unary.operator, ast::UnaryOperator::Minus);

                // Operand should be identifier 'number'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(number_id)) =
                    right_unary.operand.as_ref()
                {
                    assert_eq!(number_id.name.as_ref(), "number");
                } else {
                    panic!("Expected identifier 'number'");
                }
            } else {
                panic!("Expected unary expression (-number)");
            }
        } else {
            panic!("Expected logical AND expression for unary expression");
        }
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
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // First assignment: x = 5
            // Target should be identifier 'x'
            if let ast::AssignmentTarget::Identifier(target_id) = &assignment.target {
                assert_eq!(target_id.name.as_ref(), "x");
            } else {
                panic!("Expected identifier target 'x'");
            }

            // Value should be number literal 5
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) = assignment.value.as_ref()
            {
                assert_eq!(num_lit.value, 5.0);
            } else {
                panic!("Expected number literal '5'");
            }
        } else {
            panic!("Expected assignment expression");
        }
    }

    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[1] {
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // Second assignment: obj.property = "value"
            // Target should be property access obj.property
            if let ast::AssignmentTarget::Property(prop_access) = &assignment.target {
                // Object should be identifier 'obj'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(obj_id)) =
                    prop_access.object.as_ref()
                {
                    assert_eq!(obj_id.name.as_ref(), "obj");
                } else {
                    panic!("Expected identifier 'obj'");
                }

                // Property should be 'property'
                assert_eq!(prop_access.property.name.as_ref(), "property");
            } else {
                panic!("Expected property access target");
            }

            // Value should be string literal "value"
            if let ast::Expr::Primary(ast::PrimaryExpr::String(str_lit)) = assignment.value.as_ref()
            {
                assert_eq!(str_lit.value.as_ref(), "value");
            } else {
                panic!("Expected string literal 'value'");
            }
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
        assert_eq!(get_variable_name(var_decl), "result");

        if let Some(ast::Expr::Ternary(ternary)) = &var_decl.initializer {
            // Condition should be identifier 'condition'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(condition_id)) =
                ternary.condition.as_ref()
            {
                assert_eq!(condition_id.name.as_ref(), "condition");
            } else {
                panic!("Expected identifier 'condition'");
            }

            // Then expression should be identifier 'trueValue'
            assert!(ternary.then_expr.is_some());
            if let Some(then_expr) = &ternary.then_expr {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(true_id)) =
                    then_expr.as_ref()
                {
                    assert_eq!(true_id.name.as_ref(), "trueValue");
                } else {
                    panic!("Expected identifier 'trueValue' in then expression");
                }
            }

            // Else expression should be identifier 'falseValue'
            assert!(ternary.else_expr.is_some());
            if let Some(else_expr) = &ternary.else_expr {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(false_id)) =
                    else_expr.as_ref()
                {
                    assert_eq!(false_id.name.as_ref(), "falseValue");
                } else {
                    panic!("Expected identifier 'falseValue' in else expression");
                }
            }
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
        assert_eq!(get_variable_name(var_decl), "result");

        if let Some(ast::Expr::Pipe(pipe_expr)) = &var_decl.initializer {
            // Expression should be: value |> transform |> finalize
            // This should parse as: (value |> transform) |> finalize

            // Left side should be another pipe expression: value |> transform
            if let ast::Expr::Pipe(left_pipe) = pipe_expr.left.as_ref() {
                // Left side of first pipe should be identifier 'value'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(value_id)) =
                    left_pipe.left.as_ref()
                {
                    assert_eq!(value_id.name.as_ref(), "value");
                } else {
                    panic!("Expected identifier 'value'");
                }

                // Right side of first pipe should be identifier 'transform'
                assert!(left_pipe.right.is_some());
                if let Some(right_expr) = &left_pipe.right {
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(transform_id)) =
                        right_expr.as_ref()
                    {
                        assert_eq!(transform_id.name.as_ref(), "transform");
                    } else {
                        panic!("Expected identifier 'transform'");
                    }
                }
            } else {
                panic!("Expected pipe expression on left side");
            }

            // Right side should be identifier 'finalize'
            assert!(pipe_expr.right.is_some());
            if let Some(right_expr) = &pipe_expr.right {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(finalize_id)) =
                    right_expr.as_ref()
                {
                    assert_eq!(finalize_id.name.as_ref(), "finalize");
                } else {
                    panic!("Expected identifier 'finalize'");
                }
            }
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

    // First call: result = func();
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[0] {
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // Target should be identifier 'result'
            if let ast::AssignmentTarget::Identifier(target_id) = &assignment.target {
                assert_eq!(target_id.name.as_ref(), "result");
            } else {
                panic!("Expected identifier target 'result'");
            }

            // Value should be function call func()
            if let ast::Expr::Call(call_expr) = assignment.value.as_ref() {
                // Callee should be identifier 'func'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                    call_expr.callee.as_ref()
                {
                    assert_eq!(func_id.name.as_ref(), "func");
                } else {
                    panic!("Expected identifier 'func'");
                }

                // Operation should be call with no arguments
                if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                    assert_eq!(args.len(), 0);
                } else {
                    panic!("Expected call operation");
                }
            } else {
                panic!("Expected call expression");
            }
        } else {
            panic!("Expected assignment expression");
        }
    }

    // Second call: result2 = func(a, b, c);
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[1] {
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // Target should be identifier 'result2'
            if let ast::AssignmentTarget::Identifier(target_id) = &assignment.target {
                assert_eq!(target_id.name.as_ref(), "result2");
            } else {
                panic!("Expected identifier target 'result2'");
            }

            // Value should be function call func(a, b, c)
            if let ast::Expr::Call(call_expr) = assignment.value.as_ref() {
                // Callee should be identifier 'func'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                    call_expr.callee.as_ref()
                {
                    assert_eq!(func_id.name.as_ref(), "func");
                } else {
                    panic!("Expected identifier 'func'");
                }

                // Operation should be call with 3 arguments
                if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                    assert_eq!(args.len(), 3);

                    // First argument should be 'a'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) = &args[0] {
                        assert_eq!(a_id.name.as_ref(), "a");
                    } else {
                        panic!("Expected identifier 'a' as first argument");
                    }

                    // Second argument should be 'b'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) = &args[1] {
                        assert_eq!(b_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b' as second argument");
                    }

                    // Third argument should be 'c'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) = &args[2] {
                        assert_eq!(c_id.name.as_ref(), "c");
                    } else {
                        panic!("Expected identifier 'c' as third argument");
                    }
                } else {
                    panic!("Expected call operation");
                }
            } else {
                panic!("Expected call expression");
            }
        } else {
            panic!("Expected assignment expression");
        }
    }

    // Third call: result3 = obj.method(arg);
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[2] {
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // Target should be identifier 'result3'
            if let ast::AssignmentTarget::Identifier(target_id) = &assignment.target {
                assert_eq!(target_id.name.as_ref(), "result3");
            } else {
                panic!("Expected identifier target 'result3'");
            }

            // Value should be method call obj.method(arg)
            // This should be parsed as a chained call: (obj.method)(arg)
            if let ast::Expr::Call(call_expr) = assignment.value.as_ref() {
                // The callee should be another call expression for obj.method
                if let ast::Expr::Call(method_call) = call_expr.callee.as_ref() {
                    // The callee of method_call should be 'obj'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(obj_id)) =
                        method_call.callee.as_ref()
                    {
                        assert_eq!(obj_id.name.as_ref(), "obj");
                    } else {
                        panic!("Expected identifier 'obj'");
                    }

                    // The operation should be property access to 'method'
                    if let ast::CallOperation::Property(method_id) = method_call.operation.as_ref()
                    {
                        assert_eq!(method_id.name.as_ref(), "method");
                    } else {
                        panic!("Expected property access to 'method'");
                    }
                } else {
                    panic!("Expected call expression for method access");
                }

                // The operation should be call with 1 argument
                if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                    assert_eq!(args.len(), 1);

                    // Argument should be 'arg'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(arg_id)) = &args[0] {
                        assert_eq!(arg_id.name.as_ref(), "arg");
                    } else {
                        panic!("Expected identifier 'arg' as argument");
                    }
                } else {
                    panic!("Expected call operation");
                }
            } else {
                panic!("Expected call expression");
            }
        } else {
            panic!("Expected assignment expression");
        }
    }
}

#[test]
fn test_function_calls_with_trailing_comma() {
    let source_code = r#"
            result2 = func(a, b, c,);
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    // Second call: result2 = func(a, b, c,);
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[0] {
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // Target should be identifier 'result2'
            if let ast::AssignmentTarget::Identifier(target_id) = &assignment.target {
                assert_eq!(target_id.name.as_ref(), "result2");
            } else {
                panic!("Expected identifier target 'result2'");
            }

            // Value should be function call func(a, b, c,)
            if let ast::Expr::Call(call_expr) = assignment.value.as_ref() {
                // Callee should be identifier 'func'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                    call_expr.callee.as_ref()
                {
                    assert_eq!(func_id.name.as_ref(), "func");
                } else {
                    panic!("Expected identifier 'func'");
                }

                // Operation should be call with 3 arguments
                if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                    assert_eq!(args.len(), 3);

                    // First argument should be 'a'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) = &args[0] {
                        assert_eq!(a_id.name.as_ref(), "a");
                    } else {
                        panic!("Expected identifier 'a' as first argument");
                    }

                    // Second argument should be 'b'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) = &args[1] {
                        assert_eq!(b_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b' as second argument");
                    }

                    // Third argument should be 'c'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) = &args[2] {
                        assert_eq!(c_id.name.as_ref(), "c");
                    } else {
                        panic!("Expected identifier 'c' as third argument");
                    }
                } else {
                    panic!("Expected call operation");
                }
            } else {
                panic!("Expected call expression");
            }
        } else {
            panic!("Expected assignment expression");
        }
    }
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

    // First access: value = obj.property;
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[0] {
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // Target should be identifier 'value'
            if let ast::AssignmentTarget::Identifier(target_id) = &assignment.target {
                assert_eq!(target_id.name.as_ref(), "value");
            } else {
                panic!("Expected identifier target 'value'");
            }

            // Value should be property access obj.property
            if let ast::Expr::Call(call_expr) = assignment.value.as_ref() {
                // Callee should be identifier 'obj'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(obj_id)) =
                    call_expr.callee.as_ref()
                {
                    assert_eq!(obj_id.name.as_ref(), "obj");
                } else {
                    panic!("Expected identifier 'obj'");
                }

                // Operation should be property access to 'property'
                if let ast::CallOperation::Property(property_id) = call_expr.operation.as_ref() {
                    assert_eq!(property_id.name.as_ref(), "property");
                } else {
                    panic!("Expected property access to 'property'");
                }
            } else {
                panic!("Expected call expression for property access");
            }
        } else {
            panic!("Expected assignment expression");
        }
    }

    // Second access: value2 = obj.nested.deep.property;
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[1] {
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // Target should be identifier 'value2'
            if let ast::AssignmentTarget::Identifier(target_id) = &assignment.target {
                assert_eq!(target_id.name.as_ref(), "value2");
            } else {
                panic!("Expected identifier target 'value2'");
            }

            // Value should be chained property access obj.nested.deep.property
            // This should be parsed as: (((obj.nested).deep).property)
            if let ast::Expr::Call(call_expr3) = assignment.value.as_ref() {
                // The final property access to 'property'
                if let ast::CallOperation::Property(property_id) = call_expr3.operation.as_ref() {
                    assert_eq!(property_id.name.as_ref(), "property");
                } else {
                    panic!("Expected property access to 'property'");
                }

                // The callee should be obj.nested.deep
                if let ast::Expr::Call(call_expr2) = call_expr3.callee.as_ref() {
                    // Property access to 'deep'
                    if let ast::CallOperation::Property(deep_id) = call_expr2.operation.as_ref() {
                        assert_eq!(deep_id.name.as_ref(), "deep");
                    } else {
                        panic!("Expected property access to 'deep'");
                    }

                    // The callee should be obj.nested
                    if let ast::Expr::Call(call_expr1) = call_expr2.callee.as_ref() {
                        // Property access to 'nested'
                        if let ast::CallOperation::Property(nested_id) =
                            call_expr1.operation.as_ref()
                        {
                            assert_eq!(nested_id.name.as_ref(), "nested");
                        } else {
                            panic!("Expected property access to 'nested'");
                        }

                        // The callee should be 'obj'
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(obj_id)) =
                            call_expr1.callee.as_ref()
                        {
                            assert_eq!(obj_id.name.as_ref(), "obj");
                        } else {
                            panic!("Expected identifier 'obj'");
                        }
                    } else {
                        panic!("Expected call expression for obj.nested");
                    }
                } else {
                    panic!("Expected call expression for obj.nested.deep");
                }
            } else {
                panic!("Expected call expression for property access chain");
            }
        } else {
            panic!("Expected assignment expression");
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

    // First access: value = array[0];
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[0] {
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // Target should be identifier 'value'
            if let ast::AssignmentTarget::Identifier(target_id) = &assignment.target {
                assert_eq!(target_id.name.as_ref(), "value");
            } else {
                panic!("Expected identifier target 'value'");
            }

            // Value should be array access array[0]
            if let ast::Expr::Call(call_expr) = assignment.value.as_ref() {
                // Callee should be identifier 'array'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(array_id)) =
                    call_expr.callee.as_ref()
                {
                    assert_eq!(array_id.name.as_ref(), "array");
                } else {
                    panic!("Expected identifier 'array'");
                }

                // Operation should be index access with 0
                if let ast::CallOperation::Index(index_expr) = call_expr.operation.as_ref() {
                    if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) = index_expr {
                        assert_eq!(num_lit.value, 0.0);
                    } else {
                        panic!("Expected number literal '0'");
                    }
                } else {
                    panic!("Expected index operation");
                }
            } else {
                panic!("Expected call expression for array access");
            }
        } else {
            panic!("Expected assignment expression");
        }
    }

    // Second access: value2 = matrix[row][col];
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[1] {
        if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
            // Target should be identifier 'value2'
            if let ast::AssignmentTarget::Identifier(target_id) = &assignment.target {
                assert_eq!(target_id.name.as_ref(), "value2");
            } else {
                panic!("Expected identifier target 'value2'");
            }

            // Value should be chained array access matrix[row][col]
            // This should be parsed as: (matrix[row])[col]
            if let ast::Expr::Call(outer_call) = assignment.value.as_ref() {
                // Final operation should be index access with 'col'
                if let ast::CallOperation::Index(col_expr) = outer_call.operation.as_ref() {
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(col_id)) = col_expr {
                        assert_eq!(col_id.name.as_ref(), "col");
                    } else {
                        panic!("Expected identifier 'col'");
                    }
                } else {
                    panic!("Expected index operation for 'col'");
                }

                // The callee should be matrix[row]
                if let ast::Expr::Call(inner_call) = outer_call.callee.as_ref() {
                    // Callee should be identifier 'matrix'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(matrix_id)) =
                        inner_call.callee.as_ref()
                    {
                        assert_eq!(matrix_id.name.as_ref(), "matrix");
                    } else {
                        panic!("Expected identifier 'matrix'");
                    }

                    // Operation should be index access with 'row'
                    if let ast::CallOperation::Index(row_expr) = inner_call.operation.as_ref() {
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(row_id)) = row_expr {
                            assert_eq!(row_id.name.as_ref(), "row");
                        } else {
                            panic!("Expected identifier 'row'");
                        }
                    } else {
                        panic!("Expected index operation for 'row'");
                    }
                } else {
                    panic!("Expected call expression for matrix[row]");
                }
            } else {
                panic!("Expected call expression for chained array access");
            }
        } else {
            panic!("Expected assignment expression");
        }
    }
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

    // First array: empty = [];
    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "empty");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Array(array))) = &var_decl.initializer {
            assert_eq!(array.elements.len(), 0);
        } else {
            panic!("Expected array literal");
        }
    }

    // Second array: numbers = [1, 2, 3, 4];
    if let ast::Decl::Variable(var_decl) = &program.decls[1] {
        assert_eq!(get_variable_name(var_decl), "numbers");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Array(array))) = &var_decl.initializer {
            assert_eq!(array.elements.len(), 4);

            // First element should be number 1
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num1)) = &array.elements[0] {
                assert_eq!(num1.value, 1.0);
            } else {
                panic!("Expected number literal '1'");
            }

            // Second element should be number 2
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num2)) = &array.elements[1] {
                assert_eq!(num2.value, 2.0);
            } else {
                panic!("Expected number literal '2'");
            }

            // Third element should be number 3
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num3)) = &array.elements[2] {
                assert_eq!(num3.value, 3.0);
            } else {
                panic!("Expected number literal '3'");
            }

            // Fourth element should be number 4
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num4)) = &array.elements[3] {
                assert_eq!(num4.value, 4.0);
            } else {
                panic!("Expected number literal '4'");
            }
        } else {
            panic!("Expected array literal");
        }
    }

    // Third array: mixed = [1, "string", true, nil];
    if let ast::Decl::Variable(var_decl) = &program.decls[2] {
        assert_eq!(get_variable_name(var_decl), "mixed");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Array(array))) = &var_decl.initializer {
            assert_eq!(array.elements.len(), 4);

            // First element should be number 1
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num1)) = &array.elements[0] {
                assert_eq!(num1.value, 1.0);
            } else {
                panic!("Expected number literal '1'");
            }

            // Second element should be string "string"
            if let ast::Expr::Primary(ast::PrimaryExpr::String(str_lit)) = &array.elements[1] {
                assert_eq!(str_lit.value.as_ref(), "string");
            } else {
                panic!("Expected string literal 'string'");
            }

            // Third element should be boolean true
            if let ast::Expr::Primary(ast::PrimaryExpr::Boolean(bool_lit)) = &array.elements[2] {
                assert_eq!(bool_lit.value, true);
            } else {
                panic!("Expected boolean literal 'true'");
            }

            // Fourth element should be nil
            if let ast::Expr::Primary(ast::PrimaryExpr::Nil(_)) = &array.elements[3] {
                // Expected nil
            } else {
                panic!("Expected nil literal");
            }
        } else {
            panic!("Expected array literal");
        }
    }
}

#[test]
fn test_array_literals_with_trailing_commas() {
    let source_code = r#"numbers = [1, 2, 3, 4,];"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    // Second array: numbers = [1, 2, 3, 4,];
    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "numbers");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Array(array))) = &var_decl.initializer {
            assert_eq!(array.elements.len(), 4);

            // First element should be number 1
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num1)) = &array.elements[0] {
                assert_eq!(num1.value, 1.0);
            } else {
                panic!("Expected number literal '1'");
            }

            // Second element should be number 2
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num2)) = &array.elements[1] {
                assert_eq!(num2.value, 2.0);
            } else {
                panic!("Expected number literal '2'");
            }

            // Third element should be number 3
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num3)) = &array.elements[2] {
                assert_eq!(num3.value, 3.0);
            } else {
                panic!("Expected number literal '3'");
            }

            // Fourth element should be number 4
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num4)) = &array.elements[3] {
                assert_eq!(num4.value, 4.0);
            } else {
                panic!("Expected number literal '4'");
            }
        } else {
            panic!("Expected array literal");
        }
    }
}

#[test]
fn test_grouping_expressions() {
    let source_code = r#"var result = (a + b) * (c - d);"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        if let Some(ast::Expr::Factor(factor_expr)) = &var_decl.initializer {
            assert_eq!(factor_expr.operator, ast::FactorOperator::Multiply);

            // Left side should be term expression (a + b)
            if let ast::Expr::Term(left_term) = factor_expr.left.as_ref() {
                assert_eq!(left_term.operator, ast::TermOperator::Add);

                // Left side should be identifier 'a'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) =
                    left_term.left.as_ref()
                {
                    assert_eq!(a_id.name.as_ref(), "a");
                } else {
                    panic!("Expected identifier 'a'");
                }

                // Right side should be identifier 'b'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) =
                    left_term.right.as_ref()
                {
                    assert_eq!(b_id.name.as_ref(), "b");
                } else {
                    panic!("Expected identifier 'b'");
                }
            } else {
                panic!("Expected term expression (a + b) on left side");
            }

            // Right side should be term expression (c - d)
            if let ast::Expr::Term(right_term) = factor_expr.right.as_ref() {
                assert_eq!(right_term.operator, ast::TermOperator::Subtract);

                // Left side should be identifier 'c'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) =
                    right_term.left.as_ref()
                {
                    assert_eq!(c_id.name.as_ref(), "c");
                } else {
                    panic!("Expected identifier 'c'");
                }

                // Right side should be identifier 'd'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(d_id)) =
                    right_term.right.as_ref()
                {
                    assert_eq!(d_id.name.as_ref(), "d");
                } else {
                    panic!("Expected identifier 'd'");
                }
            } else {
                panic!("Expected term expression (c - d) on right side");
            }
        } else {
            panic!("Expected factor expression for grouping multiplication");
        }
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

    // Check number literal: var num = 42.5;
    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "num");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Number(num))) = &var_decl.initializer {
            assert_eq!(num.value, 42.5);
        } else {
            panic!("Expected number literal");
        }
    }

    // Check string literal: var str = "hello world";
    if let ast::Decl::Variable(var_decl) = &program.decls[1] {
        assert_eq!(get_variable_name(var_decl), "str");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::String(str))) = &var_decl.initializer {
            assert_eq!(str.value.as_ref(), "hello world");
        } else {
            panic!("Expected string literal");
        }
    }

    // Check boolean literal true: var bool1 = true;
    if let ast::Decl::Variable(var_decl) = &program.decls[2] {
        assert_eq!(get_variable_name(var_decl), "bool1");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Boolean(bool))) = &var_decl.initializer {
            assert_eq!(bool.value, true);
        } else {
            panic!("Expected boolean literal");
        }
    }

    // Check boolean literal false: var bool2 = false;
    if let ast::Decl::Variable(var_decl) = &program.decls[3] {
        assert_eq!(get_variable_name(var_decl), "bool2");
        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Boolean(bool))) = &var_decl.initializer {
            assert_eq!(bool.value, false);
        } else {
            panic!("Expected boolean literal");
        }
    }

    // Check nil literal: var nothing = nil;
    if let ast::Decl::Variable(var_decl) = &program.decls[4] {
        assert_eq!(get_variable_name(var_decl), "nothing");
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
        // Verify class structure
        assert_eq!(class_decl.name.name.as_ref(), "Child");
        assert!(class_decl.superclass.is_some());
        assert_eq!(
            class_decl.superclass.as_ref().unwrap().name.as_ref(),
            "Parent"
        );
        assert_eq!(class_decl.members.len(), 1);

        if let ast::ClassMember::Method(method) = &class_decl.members[0] {
            // Verify method structure
            assert_eq!(method.name.name.as_ref(), "method");
            assert_eq!(method.parameters.len(), 0);
            assert_eq!(method.body.decls.len(), 1);

            // Verify method body contains: this.value = super.getValue();
            if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &method.body.decls[0] {
                if let ast::Expr::Assignment(assignment) = &expr_stmt.expr {
                    // Left side: this.value (property assignment target)
                    if let ast::AssignmentTarget::Property(prop_access) = &assignment.target {
                        // Object should be 'this'
                        if let ast::Expr::Primary(ast::PrimaryExpr::This(_)) =
                            prop_access.object.as_ref()
                        {
                            // Expected this
                        } else {
                            panic!("Expected 'this' expression");
                        }

                        // Property should be 'value'
                        assert_eq!(prop_access.property.name.as_ref(), "value");
                    } else {
                        panic!("Expected property access target");
                    }

                    // Right side: super.getValue() (method call)
                    if let ast::Expr::Call(call_expr) = assignment.value.as_ref() {
                        // The callee should be a super method access
                        if let ast::Expr::Primary(ast::PrimaryExpr::Super(super_expr)) =
                            call_expr.callee.as_ref()
                        {
                            if let ast::SuperExpr::Method(super_method) = super_expr {
                                assert_eq!(super_method.method.name.as_ref(), "getValue");
                            } else {
                                panic!("Expected super method access");
                            }
                        } else {
                            panic!("Expected super expression");
                        }

                        // The operation should be function call with no arguments
                        if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                            assert_eq!(args.len(), 0);
                        } else {
                            panic!("Expected call operation");
                        }
                    } else {
                        panic!("Expected call expression for super.getValue()");
                    }
                } else {
                    panic!("Expected assignment expression");
                }
            } else {
                panic!("Expected expression statement");
            }
        } else {
            panic!("Expected method declaration");
        }
    } else {
        panic!("Expected class declaration");
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
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        // Expression: outer(inner(deep(value)))
        // This should parse as: outer(inner(deep(value)))
        if let Some(ast::Expr::Call(outer_call)) = &var_decl.initializer {
            // Outermost call: outer(...)
            // Callee should be identifier 'outer'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(outer_id)) =
                outer_call.callee.as_ref()
            {
                assert_eq!(outer_id.name.as_ref(), "outer");
            } else {
                panic!("Expected identifier 'outer'");
            }

            // Operation should be call with 1 argument: inner(deep(value))
            if let ast::CallOperation::Call(outer_args) = outer_call.operation.as_ref() {
                assert_eq!(outer_args.len(), 1);

                // First argument: inner(deep(value))
                if let ast::Expr::Call(inner_call) = &outer_args[0] {
                    // Middle call: inner(...)
                    // Callee should be identifier 'inner'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(inner_id)) =
                        inner_call.callee.as_ref()
                    {
                        assert_eq!(inner_id.name.as_ref(), "inner");
                    } else {
                        panic!("Expected identifier 'inner'");
                    }

                    // Operation should be call with 1 argument: deep(value)
                    if let ast::CallOperation::Call(inner_args) = inner_call.operation.as_ref() {
                        assert_eq!(inner_args.len(), 1);

                        // Second argument: deep(value)
                        if let ast::Expr::Call(deep_call) = &inner_args[0] {
                            // Innermost call: deep(...)
                            // Callee should be identifier 'deep'
                            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(deep_id)) =
                                deep_call.callee.as_ref()
                            {
                                assert_eq!(deep_id.name.as_ref(), "deep");
                            } else {
                                panic!("Expected identifier 'deep'");
                            }

                            // Operation should be call with 1 argument: value
                            if let ast::CallOperation::Call(deep_args) =
                                deep_call.operation.as_ref()
                            {
                                assert_eq!(deep_args.len(), 1);

                                // Third argument: value
                                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(value_id)) =
                                    &deep_args[0]
                                {
                                    assert_eq!(value_id.name.as_ref(), "value");
                                } else {
                                    panic!("Expected identifier 'value'");
                                }
                            } else {
                                panic!("Expected call operation for 'deep'");
                            }
                        } else {
                            panic!("Expected call expression for deep(value)");
                        }
                    } else {
                        panic!("Expected call operation for 'inner'");
                    }
                } else {
                    panic!("Expected call expression for inner(deep(value))");
                }
            } else {
                panic!("Expected call operation for 'outer'");
            }
        } else {
            panic!("Expected call expression for nested function calls");
        }
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
        assert_eq!(get_variable_name(var_decl), "result");

        // Expression: a + b * c == d - e / f
        // Should be parsed as: (a + (b * c)) == (d - (e / f))
        if let Some(ast::Expr::Equality(equality_expr)) = &var_decl.initializer {
            assert_eq!(equality_expr.operator, ast::EqualityOperator::Equal);

            // Left side: a + (b * c)
            if let ast::Expr::Term(left_term) = equality_expr.left.as_ref() {
                assert_eq!(left_term.operator, ast::TermOperator::Add);

                // Left side of addition: identifier 'a'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) =
                    left_term.left.as_ref()
                {
                    assert_eq!(a_id.name.as_ref(), "a");
                } else {
                    panic!("Expected identifier 'a'");
                }

                // Right side of addition: b * c
                if let ast::Expr::Factor(factor_expr) = left_term.right.as_ref() {
                    assert_eq!(factor_expr.operator, ast::FactorOperator::Multiply);

                    // Left side of multiplication: identifier 'b'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) =
                        factor_expr.left.as_ref()
                    {
                        assert_eq!(b_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b'");
                    }

                    // Right side of multiplication: identifier 'c'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) =
                        factor_expr.right.as_ref()
                    {
                        assert_eq!(c_id.name.as_ref(), "c");
                    } else {
                        panic!("Expected identifier 'c'");
                    }
                } else {
                    panic!("Expected factor expression (b * c)");
                }
            } else {
                panic!("Expected term expression (a + (b * c))");
            }

            // Right side: d - (e / f)
            if let ast::Expr::Term(right_term) = equality_expr.right.as_ref() {
                assert_eq!(right_term.operator, ast::TermOperator::Subtract);

                // Left side of subtraction: identifier 'd'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(d_id)) =
                    right_term.left.as_ref()
                {
                    assert_eq!(d_id.name.as_ref(), "d");
                } else {
                    panic!("Expected identifier 'd'");
                }

                // Right side of subtraction: e / f
                if let ast::Expr::Factor(factor_expr) = right_term.right.as_ref() {
                    assert_eq!(factor_expr.operator, ast::FactorOperator::Divide);

                    // Left side of division: identifier 'e'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(e_id)) =
                        factor_expr.left.as_ref()
                    {
                        assert_eq!(e_id.name.as_ref(), "e");
                    } else {
                        panic!("Expected identifier 'e'");
                    }

                    // Right side of division: identifier 'f'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(f_id)) =
                        factor_expr.right.as_ref()
                    {
                        assert_eq!(f_id.name.as_ref(), "f");
                    } else {
                        panic!("Expected identifier 'f'");
                    }
                } else {
                    panic!("Expected factor expression (e / f)");
                }
            } else {
                panic!("Expected term expression (d - (e / f))");
            }
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
        assert_eq!(get_variable_name(var_decl), "result");

        // Expression: a ? b ? c : d : e
        // Should be parsed as: a ? (b ? c : d) : e (right associative)
        if let Some(ast::Expr::Ternary(outer_ternary)) = &var_decl.initializer {
            // Condition should be identifier 'a'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) =
                outer_ternary.condition.as_ref()
            {
                assert_eq!(a_id.name.as_ref(), "a");
            } else {
                panic!("Expected identifier 'a' as outer condition");
            }

            // Then expression should be nested ternary: b ? c : d
            assert!(outer_ternary.then_expr.is_some());
            if let Some(then_expr) = &outer_ternary.then_expr {
                if let ast::Expr::Ternary(inner_ternary) = then_expr.as_ref() {
                    // Inner condition should be identifier 'b'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) =
                        inner_ternary.condition.as_ref()
                    {
                        assert_eq!(b_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b' as inner condition");
                    }

                    // Inner then expression should be identifier 'c'
                    assert!(inner_ternary.then_expr.is_some());
                    if let Some(inner_then) = &inner_ternary.then_expr {
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) =
                            inner_then.as_ref()
                        {
                            assert_eq!(c_id.name.as_ref(), "c");
                        } else {
                            panic!("Expected identifier 'c' in inner then expression");
                        }
                    }

                    // Inner else expression should be identifier 'd'
                    assert!(inner_ternary.else_expr.is_some());
                    if let Some(inner_else) = &inner_ternary.else_expr {
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(d_id)) =
                            inner_else.as_ref()
                        {
                            assert_eq!(d_id.name.as_ref(), "d");
                        } else {
                            panic!("Expected identifier 'd' in inner else expression");
                        }
                    }
                } else {
                    panic!("Expected nested ternary expression (b ? c : d) in then clause");
                }
            }

            // Else expression should be identifier 'e'
            assert!(outer_ternary.else_expr.is_some());
            if let Some(else_expr) = &outer_ternary.else_expr {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(e_id)) = else_expr.as_ref() {
                    assert_eq!(e_id.name.as_ref(), "e");
                } else {
                    panic!("Expected identifier 'e' in outer else expression");
                }
            }
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
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        // Expression: obj.method1().method2().method3()
        // This should parse as: (((obj.method1()).method2()).method3())
        if let Some(ast::Expr::Call(final_call)) = &var_decl.initializer {
            // Final call: .method3()
            if let ast::CallOperation::Call(final_args) = final_call.operation.as_ref() {
                assert_eq!(final_args.len(), 0);
            } else {
                panic!("Expected call operation for method3()");
            }

            // Callee should be obj.method1().method2()
            if let ast::Expr::Call(method2_access) = final_call.callee.as_ref() {
                // Property access to method3
                if let ast::CallOperation::Property(method3_id) = method2_access.operation.as_ref()
                {
                    assert_eq!(method3_id.name.as_ref(), "method3");
                } else {
                    panic!("Expected property access to 'method3'");
                }

                // Callee should be obj.method1().method2()
                if let ast::Expr::Call(method2_call) = method2_access.callee.as_ref() {
                    // Call operation for method2()
                    if let ast::CallOperation::Call(method2_args) = method2_call.operation.as_ref()
                    {
                        assert_eq!(method2_args.len(), 0);
                    } else {
                        panic!("Expected call operation for method2()");
                    }

                    // Callee should be obj.method1().method2
                    if let ast::Expr::Call(method2_access_inner) = method2_call.callee.as_ref() {
                        // Property access to method2
                        if let ast::CallOperation::Property(method2_id) =
                            method2_access_inner.operation.as_ref()
                        {
                            assert_eq!(method2_id.name.as_ref(), "method2");
                        } else {
                            panic!("Expected property access to 'method2'");
                        }

                        // Callee should be obj.method1()
                        if let ast::Expr::Call(method1_call) = method2_access_inner.callee.as_ref()
                        {
                            // Call operation for method1()
                            if let ast::CallOperation::Call(method1_args) =
                                method1_call.operation.as_ref()
                            {
                                assert_eq!(method1_args.len(), 0);
                            } else {
                                panic!("Expected call operation for method1()");
                            }

                            // Callee should be obj.method1
                            if let ast::Expr::Call(method1_access) = method1_call.callee.as_ref() {
                                // Property access to method1
                                if let ast::CallOperation::Property(method1_id) =
                                    method1_access.operation.as_ref()
                                {
                                    assert_eq!(method1_id.name.as_ref(), "method1");
                                } else {
                                    panic!("Expected property access to 'method1'");
                                }

                                // Callee should be identifier 'obj'
                                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(obj_id)) =
                                    method1_access.callee.as_ref()
                                {
                                    assert_eq!(obj_id.name.as_ref(), "obj");
                                } else {
                                    panic!("Expected identifier 'obj'");
                                }
                            } else {
                                panic!("Expected call expression for obj.method1");
                            }
                        } else {
                            panic!("Expected call expression for method1()");
                        }
                    } else {
                        panic!("Expected call expression for obj.method1().method2");
                    }
                } else {
                    panic!("Expected call expression for method2()");
                }
            } else {
                panic!("Expected call expression for obj.method1().method2");
            }
        } else {
            panic!("Expected call expression for chained method calls");
        }
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
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        // Expression: obj.property.method().field[index]
        // This should parse as: ((((obj.property).method()).field)[index])
        if let Some(ast::Expr::Call(index_access)) = &var_decl.initializer {
            // Final operation: [index]
            if let ast::CallOperation::Index(index_expr) = index_access.operation.as_ref() {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(index_id)) = index_expr {
                    assert_eq!(index_id.name.as_ref(), "index");
                } else {
                    panic!("Expected identifier 'index'");
                }
            } else {
                panic!("Expected index operation");
            }

            // Callee should be obj.property.method().field
            if let ast::Expr::Call(field_access) = index_access.callee.as_ref() {
                // Property access to 'field'
                if let ast::CallOperation::Property(field_id) = field_access.operation.as_ref() {
                    assert_eq!(field_id.name.as_ref(), "field");
                } else {
                    panic!("Expected property access to 'field'");
                }

                // Callee should be obj.property.method()
                if let ast::Expr::Call(method_call) = field_access.callee.as_ref() {
                    // Call operation for method()
                    if let ast::CallOperation::Call(method_args) = method_call.operation.as_ref() {
                        assert_eq!(method_args.len(), 0);
                    } else {
                        panic!("Expected call operation for method()");
                    }

                    // Callee should be obj.property.method
                    if let ast::Expr::Call(method_access) = method_call.callee.as_ref() {
                        // Property access to 'method'
                        if let ast::CallOperation::Property(method_id) =
                            method_access.operation.as_ref()
                        {
                            assert_eq!(method_id.name.as_ref(), "method");
                        } else {
                            panic!("Expected property access to 'method'");
                        }

                        // Callee should be obj.property
                        if let ast::Expr::Call(property_access) = method_access.callee.as_ref() {
                            // Property access to 'property'
                            if let ast::CallOperation::Property(property_id) =
                                property_access.operation.as_ref()
                            {
                                assert_eq!(property_id.name.as_ref(), "property");
                            } else {
                                panic!("Expected property access to 'property'");
                            }

                            // Callee should be identifier 'obj'
                            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(obj_id)) =
                                property_access.callee.as_ref()
                            {
                                assert_eq!(obj_id.name.as_ref(), "obj");
                            } else {
                                panic!("Expected identifier 'obj'");
                            }
                        } else {
                            panic!("Expected call expression for obj.property");
                        }
                    } else {
                        panic!("Expected call expression for obj.property.method");
                    }
                } else {
                    panic!("Expected call expression for method()");
                }
            } else {
                panic!("Expected call expression for field access");
            }
        } else {
            panic!("Expected call expression for mixed property and method access");
        }
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
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        // Expression: matrix[row + 1][col - 1]
        // This should parse as: (matrix[row + 1])[col - 1]
        if let Some(ast::Expr::Call(outer_access)) = &var_decl.initializer {
            // Final operation: [col - 1]
            if let ast::CallOperation::Index(col_expr) = outer_access.operation.as_ref() {
                // Index expression should be: col - 1
                if let ast::Expr::Term(term_expr) = col_expr {
                    assert_eq!(term_expr.operator, ast::TermOperator::Subtract);

                    // Left side should be identifier 'col'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(col_id)) =
                        term_expr.left.as_ref()
                    {
                        assert_eq!(col_id.name.as_ref(), "col");
                    } else {
                        panic!("Expected identifier 'col'");
                    }

                    // Right side should be number literal 1
                    if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                        term_expr.right.as_ref()
                    {
                        assert_eq!(num_lit.value, 1.0);
                    } else {
                        panic!("Expected number literal '1'");
                    }
                } else {
                    panic!("Expected term expression (col - 1)");
                }
            } else {
                panic!("Expected index operation for [col - 1]");
            }

            // Callee should be matrix[row + 1]
            if let ast::Expr::Call(inner_access) = outer_access.callee.as_ref() {
                // Callee should be identifier 'matrix'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(matrix_id)) =
                    inner_access.callee.as_ref()
                {
                    assert_eq!(matrix_id.name.as_ref(), "matrix");
                } else {
                    panic!("Expected identifier 'matrix'");
                }

                // Operation should be index access with [row + 1]
                if let ast::CallOperation::Index(row_expr) = inner_access.operation.as_ref() {
                    // Index expression should be: row + 1
                    if let ast::Expr::Term(term_expr) = row_expr {
                        assert_eq!(term_expr.operator, ast::TermOperator::Add);

                        // Left side should be identifier 'row'
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(row_id)) =
                            term_expr.left.as_ref()
                        {
                            assert_eq!(row_id.name.as_ref(), "row");
                        } else {
                            panic!("Expected identifier 'row'");
                        }

                        // Right side should be number literal 1
                        if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                            term_expr.right.as_ref()
                        {
                            assert_eq!(num_lit.value, 1.0);
                        } else {
                            panic!("Expected number literal '1'");
                        }
                    } else {
                        panic!("Expected term expression (row + 1)");
                    }
                } else {
                    panic!("Expected index operation for [row + 1]");
                }
            } else {
                panic!("Expected call expression for matrix[row + 1]");
            }
        } else {
            panic!("Expected call expression for complex array access");
        }
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
        // Expression: a = b = c = 5
        // This should parse as: a = (b = (c = 5)) (right associative)
        if let ast::Expr::Assignment(outer_assignment) = &expr_stmt.expr {
            // Outermost assignment target should be identifier 'a'
            if let ast::AssignmentTarget::Identifier(a_id) = &outer_assignment.target {
                assert_eq!(a_id.name.as_ref(), "a");
            } else {
                panic!("Expected identifier target 'a'");
            }

            // Value should be another assignment: b = (c = 5)
            if let ast::Expr::Assignment(middle_assignment) = outer_assignment.value.as_ref() {
                // Middle assignment target should be identifier 'b'
                if let ast::AssignmentTarget::Identifier(b_id) = &middle_assignment.target {
                    assert_eq!(b_id.name.as_ref(), "b");
                } else {
                    panic!("Expected identifier target 'b'");
                }

                // Value should be another assignment: c = 5
                if let ast::Expr::Assignment(inner_assignment) = middle_assignment.value.as_ref() {
                    // Inner assignment target should be identifier 'c'
                    if let ast::AssignmentTarget::Identifier(c_id) = &inner_assignment.target {
                        assert_eq!(c_id.name.as_ref(), "c");
                    } else {
                        panic!("Expected identifier target 'c'");
                    }

                    // Value should be number literal 5
                    if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                        inner_assignment.value.as_ref()
                    {
                        assert_eq!(num_lit.value, 5.0);
                    } else {
                        panic!("Expected number literal '5'");
                    }
                } else {
                    panic!("Expected assignment expression (c = 5)");
                }
            } else {
                panic!("Expected assignment expression (b = (c = 5))");
            }
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
        assert_eq!(get_variable_name(var_decl), "result");

        // Expression: a ? b : c ? d : e ? f : g
        // Should be parsed as: a ? b : (c ? d : (e ? f : g)) (right associative)
        if let Some(ast::Expr::Ternary(outer_ternary)) = &var_decl.initializer {
            // Condition should be identifier 'a'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) =
                outer_ternary.condition.as_ref()
            {
                assert_eq!(a_id.name.as_ref(), "a");
            } else {
                panic!("Expected identifier 'a' as outer condition");
            }

            // Then expression should be identifier 'b'
            assert!(outer_ternary.then_expr.is_some());
            if let Some(then_expr) = &outer_ternary.then_expr {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) = then_expr.as_ref() {
                    assert_eq!(b_id.name.as_ref(), "b");
                } else {
                    panic!("Expected identifier 'b' in then expression");
                }
            }

            // Else expression should be nested ternary: c ? d : (e ? f : g)
            assert!(outer_ternary.else_expr.is_some());
            if let Some(else_expr) = &outer_ternary.else_expr {
                if let ast::Expr::Ternary(middle_ternary) = else_expr.as_ref() {
                    // Middle condition should be identifier 'c'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) =
                        middle_ternary.condition.as_ref()
                    {
                        assert_eq!(c_id.name.as_ref(), "c");
                    } else {
                        panic!("Expected identifier 'c' as middle condition");
                    }

                    // Middle then expression should be identifier 'd'
                    assert!(middle_ternary.then_expr.is_some());
                    if let Some(middle_then) = &middle_ternary.then_expr {
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(d_id)) =
                            middle_then.as_ref()
                        {
                            assert_eq!(d_id.name.as_ref(), "d");
                        } else {
                            panic!("Expected identifier 'd' in middle then expression");
                        }
                    }

                    // Middle else expression should be innermost ternary: e ? f : g
                    assert!(middle_ternary.else_expr.is_some());
                    if let Some(middle_else) = &middle_ternary.else_expr {
                        if let ast::Expr::Ternary(inner_ternary) = middle_else.as_ref() {
                            // Inner condition should be identifier 'e'
                            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(e_id)) =
                                inner_ternary.condition.as_ref()
                            {
                                assert_eq!(e_id.name.as_ref(), "e");
                            } else {
                                panic!("Expected identifier 'e' as inner condition");
                            }

                            // Inner then expression should be identifier 'f'
                            assert!(inner_ternary.then_expr.is_some());
                            if let Some(inner_then) = &inner_ternary.then_expr {
                                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(f_id)) =
                                    inner_then.as_ref()
                                {
                                    assert_eq!(f_id.name.as_ref(), "f");
                                } else {
                                    panic!("Expected identifier 'f' in inner then expression");
                                }
                            }

                            // Inner else expression should be identifier 'g'
                            assert!(inner_ternary.else_expr.is_some());
                            if let Some(inner_else) = &inner_ternary.else_expr {
                                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(g_id)) =
                                    inner_else.as_ref()
                                {
                                    assert_eq!(g_id.name.as_ref(), "g");
                                } else {
                                    panic!("Expected identifier 'g' in inner else expression");
                                }
                            }
                        } else {
                            panic!("Expected innermost ternary expression (e ? f : g)");
                        }
                    }
                } else {
                    panic!("Expected middle ternary expression (c ? d : (e ? f : g))");
                }
            }
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
        assert_eq!(get_variable_name(var_decl), "condition");
        assert!(var_decl.initializer.is_some());

        // Expression: !a and (b or c) and !(d or e)
        // Should be parsed as: ((!a) and (b or c)) and (!(d or e))
        if let Some(ast::Expr::LogicalAnd(outer_and)) = &var_decl.initializer {
            // Left side: (!a) and (b or c)
            if let ast::Expr::LogicalAnd(inner_and) = outer_and.left.as_ref() {
                // Left side of inner AND: !a
                if let ast::Expr::Unary(left_unary) = inner_and.left.as_ref() {
                    assert_eq!(left_unary.operator, ast::UnaryOperator::Not);

                    // Operand should be identifier 'a'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) =
                        left_unary.operand.as_ref()
                    {
                        assert_eq!(a_id.name.as_ref(), "a");
                    } else {
                        panic!("Expected identifier 'a'");
                    }
                } else {
                    panic!("Expected unary expression (!a)");
                }

                // Right side of inner AND: (b or c)
                if let ast::Expr::LogicalOr(or_expr) = inner_and.right.as_ref() {
                    // Left side of OR: identifier 'b'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) =
                        or_expr.left.as_ref()
                    {
                        assert_eq!(b_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b'");
                    }

                    // Right side of OR: identifier 'c'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) =
                        or_expr.right.as_ref()
                    {
                        assert_eq!(c_id.name.as_ref(), "c");
                    } else {
                        panic!("Expected identifier 'c'");
                    }
                } else {
                    panic!("Expected logical OR expression (b or c)");
                }
            } else {
                panic!("Expected logical AND expression ((!a) and (b or c))");
            }

            // Right side: !(d or e)
            if let ast::Expr::Unary(right_unary) = outer_and.right.as_ref() {
                assert_eq!(right_unary.operator, ast::UnaryOperator::Not);

                // Operand should be (d or e)
                if let ast::Expr::LogicalOr(inner_or) = right_unary.operand.as_ref() {
                    // Left side of inner OR: identifier 'd'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(d_id)) =
                        inner_or.left.as_ref()
                    {
                        assert_eq!(d_id.name.as_ref(), "d");
                    } else {
                        panic!("Expected identifier 'd'");
                    }

                    // Right side of inner OR: identifier 'e'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(e_id)) =
                        inner_or.right.as_ref()
                    {
                        assert_eq!(e_id.name.as_ref(), "e");
                    } else {
                        panic!("Expected identifier 'e'");
                    }
                } else {
                    panic!("Expected logical OR expression (d or e)");
                }
            } else {
                panic!("Expected unary expression (!(d or e))");
            }
        } else {
            panic!("Expected logical AND expression");
        }
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
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        // Expression: value + 1 |> transform |> process - 2
        // Should be parsed as: (value + 1) |> transform |> (process - 2)
        // Which becomes: ((value + 1) |> transform) |> (process - 2)
        if let Some(ast::Expr::Pipe(outer_pipe)) = &var_decl.initializer {
            // Right side should be: process - 2
            assert!(outer_pipe.right.is_some());
            if let Some(right_expr) = &outer_pipe.right {
                if let ast::Expr::Term(term_expr) = right_expr.as_ref() {
                    assert_eq!(term_expr.operator, ast::TermOperator::Subtract);

                    // Left side should be identifier 'process'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(process_id)) =
                        term_expr.left.as_ref()
                    {
                        assert_eq!(process_id.name.as_ref(), "process");
                    } else {
                        panic!("Expected identifier 'process'");
                    }

                    // Right side should be number literal 2
                    if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                        term_expr.right.as_ref()
                    {
                        assert_eq!(num_lit.value, 2.0);
                    } else {
                        panic!("Expected number literal '2'");
                    }
                } else {
                    panic!("Expected term expression (process - 2)");
                }
            }

            // Left side should be: (value + 1) |> transform
            if let ast::Expr::Pipe(inner_pipe) = outer_pipe.left.as_ref() {
                // Right side of inner pipe should be identifier 'transform'
                assert!(inner_pipe.right.is_some());
                if let Some(inner_right) = &inner_pipe.right {
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(transform_id)) =
                        inner_right.as_ref()
                    {
                        assert_eq!(transform_id.name.as_ref(), "transform");
                    } else {
                        panic!("Expected identifier 'transform'");
                    }
                }

                // Left side of inner pipe should be: value + 1
                if let ast::Expr::Term(term_expr) = inner_pipe.left.as_ref() {
                    assert_eq!(term_expr.operator, ast::TermOperator::Add);

                    // Left side should be identifier 'value'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(value_id)) =
                        term_expr.left.as_ref()
                    {
                        assert_eq!(value_id.name.as_ref(), "value");
                    } else {
                        panic!("Expected identifier 'value'");
                    }

                    // Right side should be number literal 1
                    if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                        term_expr.right.as_ref()
                    {
                        assert_eq!(num_lit.value, 1.0);
                    } else {
                        panic!("Expected number literal '1'");
                    }
                } else {
                    panic!("Expected term expression (value + 1)");
                }
            } else {
                panic!("Expected pipe expression ((value + 1) |> transform)");
            }
        } else {
            panic!("Expected pipe expression for operator precedence test");
        }
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
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        // Expression: ((((a + b) * c) - d) / e) % f
        // Should be parsed as: ((((a + b) * c) - d) / e) % f
        // Outermost operation: % (modulo)
        if let Some(ast::Expr::Factor(outermost_factor)) = &var_decl.initializer {
            assert_eq!(outermost_factor.operator, ast::FactorOperator::Modulo);

            // Right side should be identifier 'f'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(f_id)) =
                outermost_factor.right.as_ref()
            {
                assert_eq!(f_id.name.as_ref(), "f");
            } else {
                panic!("Expected identifier 'f'");
            }

            // Left side: (((a + b) * c) - d) / e
            // Second level operation: / (division)
            if let ast::Expr::Factor(second_factor) = outermost_factor.left.as_ref() {
                assert_eq!(second_factor.operator, ast::FactorOperator::Divide);

                // Right side should be identifier 'e'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(e_id)) =
                    second_factor.right.as_ref()
                {
                    assert_eq!(e_id.name.as_ref(), "e");
                } else {
                    panic!("Expected identifier 'e'");
                }

                // Left side: ((a + b) * c) - d
                // Third level operation: - (subtraction)
                if let ast::Expr::Term(term_expr) = second_factor.left.as_ref() {
                    assert_eq!(term_expr.operator, ast::TermOperator::Subtract);

                    // Right side should be identifier 'd'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(d_id)) =
                        term_expr.right.as_ref()
                    {
                        assert_eq!(d_id.name.as_ref(), "d");
                    } else {
                        panic!("Expected identifier 'd'");
                    }

                    // Left side: (a + b) * c
                    // Fourth level operation: * (multiplication)
                    if let ast::Expr::Factor(third_factor) = term_expr.left.as_ref() {
                        assert_eq!(third_factor.operator, ast::FactorOperator::Multiply);

                        // Right side should be identifier 'c'
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(c_id)) =
                            third_factor.right.as_ref()
                        {
                            assert_eq!(c_id.name.as_ref(), "c");
                        } else {
                            panic!("Expected identifier 'c'");
                        }

                        // Left side: a + b
                        // Innermost operation: + (addition)
                        if let ast::Expr::Term(innermost_term) = third_factor.left.as_ref() {
                            assert_eq!(innermost_term.operator, ast::TermOperator::Add);

                            // Left side should be identifier 'a'
                            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(a_id)) =
                                innermost_term.left.as_ref()
                            {
                                assert_eq!(a_id.name.as_ref(), "a");
                            } else {
                                panic!("Expected identifier 'a'");
                            }

                            // Right side should be identifier 'b'
                            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(b_id)) =
                                innermost_term.right.as_ref()
                            {
                                assert_eq!(b_id.name.as_ref(), "b");
                            } else {
                                panic!("Expected identifier 'b'");
                            }
                        } else {
                            panic!("Expected term expression (a + b)");
                        }
                    } else {
                        panic!("Expected factor expression ((a + b) * c)");
                    }
                } else {
                    panic!("Expected term expression (((a + b) * c) - d)");
                }
            } else {
                panic!("Expected factor expression ((((a + b) * c) - d) / e)");
            }
        } else {
            panic!("Expected factor expression for deeply nested expression");
        }
    }
}

#[test]
fn test_lambda_as_expression() {
    let source_code = r#"
            assert_throws(() -> 1 / 0);
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
}

#[test]
fn test_lambda_as_immediately_invoked_expression() {
    let source_code = r#"
            var test = (() -> nil)();
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    // Should be: var test = (() -> nil)();
    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "test");
        assert!(var_decl.initializer.is_some());

        // The initializer should be a call expression: (() -> nil)()
        if let Some(ast::Expr::Call(call_expr)) = &var_decl.initializer {
            // The callee should be a lambda expression: () -> nil
            if let ast::Expr::Primary(ast::PrimaryExpr::Lambda(lambda)) = call_expr.callee.as_ref()
            {
                // Lambda should have no parameters
                assert_eq!(lambda.parameters.len(), 0);

                // Lambda body should be an expression: nil
                if let ast::LambdaBody::Expr(body_expr) = lambda.body.as_ref() {
                    if let ast::Expr::Primary(ast::PrimaryExpr::Nil(_)) = body_expr.as_ref() {
                        // This is correct - lambda body is nil
                    } else {
                        panic!("Expected lambda body to be nil expression");
                    }
                } else {
                    panic!("Expected lambda body to be an expression, not a block");
                }
            } else {
                panic!("Expected lambda expression as callee");
            }

            // The call operation should be a function call with no arguments
            if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                assert_eq!(args.len(), 0);
            } else {
                panic!("Expected call operation with no arguments");
            }
        } else {
            panic!("Expected call expression for IIFE");
        }
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_lambda_as_immediately_invoked_expression_with_args() {
    let source_code = r#"
            var result = ((x) -> x * 2)(5);
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    // Should be: var result = ((x) -> x * 2)(5);
    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "result");
        assert!(var_decl.initializer.is_some());

        // The initializer should be a call expression: ((x) -> x * 2)(5)
        if let Some(ast::Expr::Call(call_expr)) = &var_decl.initializer {
            // The callee should be a lambda expression: (x) -> x * 2
            if let ast::Expr::Primary(ast::PrimaryExpr::Lambda(lambda)) = call_expr.callee.as_ref()
            {
                // Lambda should have one parameter 'x'
                assert_eq!(lambda.parameters.len(), 1);
                assert_eq!(get_parameter_name(&lambda.parameters[0]), "x");

                // Lambda body should be an expression: x * 2
                if let ast::LambdaBody::Expr(body_expr) = lambda.body.as_ref() {
                    if let ast::Expr::Factor(factor_expr) = body_expr.as_ref() {
                        assert_eq!(factor_expr.operator, ast::FactorOperator::Multiply);

                        // Left side should be identifier 'x'
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(x_id)) =
                            factor_expr.left.as_ref()
                        {
                            assert_eq!(x_id.name.as_ref(), "x");
                        } else {
                            panic!("Expected identifier 'x' in lambda body");
                        }

                        // Right side should be number literal 2
                        if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                            factor_expr.right.as_ref()
                        {
                            assert_eq!(num_lit.value, 2.0);
                        } else {
                            panic!("Expected number literal '2' in lambda body");
                        }
                    } else {
                        panic!("Expected factor expression 'x * 2' in lambda body");
                    }
                } else {
                    panic!("Expected lambda body to be an expression, not a block");
                }
            } else {
                panic!("Expected lambda expression as callee");
            }

            // The call operation should be a function call with one argument (5)
            if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                assert_eq!(args.len(), 1);

                if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) = &args[0] {
                    assert_eq!(num_lit.value, 5.0);
                } else {
                    panic!("Expected number literal '5' as argument");
                }
            } else {
                panic!("Expected call operation with one argument");
            }
        } else {
            panic!("Expected call expression for IIFE with argument");
        }
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_index_assignment() {
    let source_code = r#"
        foo[1] = true;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
}
