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
fn test_simple_variable_declaration() {
    let source_code = r#"var x = 42;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "x");
        assert!(var_decl.initializer.is_some());

        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Number(num))) = &var_decl.initializer {
            assert_eq!(num.value, 42.0);
        } else {
            panic!("Expected number literal");
        }
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_variable_declaration_without_initializer() {
    let source_code = r#"var x;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "x");
        assert!(var_decl.initializer.is_none());
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_function_declaration() {
    let source_code = r#"
            fn add(a, b) {
                return a + b;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Function(func_decl) = &program.decls[0] {
        assert_eq!(func_decl.function.name.name.as_ref(), "add");
        assert_eq!(func_decl.function.parameters.len(), 2);
        assert_eq!(get_parameter_name(&func_decl.function.parameters[0]), "a");
        assert_eq!(get_parameter_name(&func_decl.function.parameters[1]), "b");
        assert_eq!(func_decl.function.body.decls.len(), 1);

        // Verify the return statement in the function body
        if let ast::Decl::Stmt(ast::Stmt::Return(return_stmt)) = &func_decl.function.body.decls[0] {
            assert!(return_stmt.value.is_some());

            // Verify the return expression: a + b
            if let Some(ast::Expr::Term(term_expr)) = &return_stmt.value {
                // Left side should be identifier 'a'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                    term_expr.left.as_ref()
                {
                    assert_eq!(left_id.name.as_ref(), "a");
                } else {
                    panic!("Expected identifier 'a' on left side of addition");
                }

                // Operator should be Add
                assert_eq!(term_expr.operator, ast::TermOperator::Add);

                // Right side should be identifier 'b'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(right_id)) =
                    term_expr.right.as_ref()
                {
                    assert_eq!(right_id.name.as_ref(), "b");
                } else {
                    panic!("Expected identifier 'b' on right side of addition");
                }
            } else {
                panic!("Expected term expression (a + b) in return statement");
            }
        } else {
            panic!("Expected return statement in function body");
        }
    } else {
        panic!("Expected function declaration");
    }
}

#[test]
fn test_function_declaration_with_trailing_comma() {
    let source_code = r#"
            fn add(a, b,) {
                return a + b;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Function(func_decl) = &program.decls[0] {
        assert_eq!(func_decl.function.name.name.as_ref(), "add");
        assert_eq!(func_decl.function.parameters.len(), 2);

        assert_eq!(get_parameter_name(&func_decl.function.parameters[0]), "a");
        assert_eq!(get_parameter_name(&func_decl.function.parameters[1]), "b");
        assert_eq!(func_decl.function.body.decls.len(), 1);

        // Verify the return statement in the function body
        if let ast::Decl::Stmt(ast::Stmt::Return(return_stmt)) = &func_decl.function.body.decls[0] {
            assert!(return_stmt.value.is_some());

            // Verify the return expression: a + b
            if let Some(ast::Expr::Term(term_expr)) = &return_stmt.value {
                // Left side should be identifier 'a'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                    term_expr.left.as_ref()
                {
                    assert_eq!(left_id.name.as_ref(), "a");
                } else {
                    panic!("Expected identifier 'a' on left side of addition");
                }

                // Operator should be Add
                assert_eq!(term_expr.operator, ast::TermOperator::Add);

                // Right side should be identifier 'b'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(right_id)) =
                    term_expr.right.as_ref()
                {
                    assert_eq!(right_id.name.as_ref(), "b");
                } else {
                    panic!("Expected identifier 'b' on right side of addition");
                }
            } else {
                panic!("Expected term expression (a + b) in return statement");
            }
        } else {
            panic!("Expected return statement in function body");
        }
    } else {
        panic!("Expected function declaration");
    }
}

#[test]
fn test_function_without_parameters() {
    let source_code = r#"
            fn main() {
                var x = 5;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Function(func_decl) = &program.decls[0] {
        assert_eq!(func_decl.function.name.name.as_ref(), "main");
        assert_eq!(func_decl.function.parameters.len(), 0);
    } else {
        panic!("Expected function declaration");
    }
}

#[test]
fn test_class_declaration() {
    let source_code = r#"
            class Person {
                name;
                age = 0;
                
                get_name() {
                    return this.name;
                }
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Class(class_decl) = &program.decls[0] {
        assert_eq!(class_decl.name.name.as_ref(), "Person");
        assert!(class_decl.superclass.is_none());
        assert_eq!(class_decl.members.len(), 3);

        // Check field without initializer
        if let ast::ClassMember::Field(field) = &class_decl.members[0] {
            assert_eq!(field.name.name.as_ref(), "name");
            assert!(field.initializer.is_none());
        } else {
            panic!("Expected field declaration");
        }

        // Check field with initializer
        if let ast::ClassMember::Field(field) = &class_decl.members[1] {
            assert_eq!(field.name.name.as_ref(), "age");
            assert!(field.initializer.is_some());
        } else {
            panic!("Expected field declaration");
        }

        // Check method
        if let ast::ClassMember::Method(method) = &class_decl.members[2] {
            assert_eq!(method.name.name.as_ref(), "get_name");
            assert_eq!(method.parameters.len(), 0);

            // Verify method body contains return statement
            assert_eq!(method.body.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Return(return_stmt)) = &method.body.decls[0] {
                assert!(return_stmt.value.is_some());

                // Verify the return expression: this.name
                if let Some(ast::Expr::Call(call_expr)) = &return_stmt.value {
                    // Verify the callee is 'this'
                    if let ast::Expr::Primary(ast::PrimaryExpr::This(_)) = call_expr.callee.as_ref()
                    {
                        // Verify the operation is property access to 'name'
                        if let ast::CallOperation::Property(property_id) =
                            call_expr.operation.as_ref()
                        {
                            assert_eq!(property_id.name.as_ref(), "name");
                        } else {
                            panic!("Expected property access to 'name'");
                        }
                    } else {
                        panic!("Expected 'this' as callee");
                    }
                } else {
                    panic!("Expected call expression (this.name) in return statement");
                }
            } else {
                panic!("Expected return statement in method body");
            }
        } else {
            panic!("Expected method declaration");
        }
    } else {
        panic!("Expected class declaration");
    }
}

#[test]
fn test_class_declaration_with_method_containing_trailing_comma_in_parameters() {
    let source_code = r#"
            class Class {
                method(arg1, arg2,) {
                    return;
                }
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Class(class_decl) = &program.decls[0] {
        assert_eq!(class_decl.name.name.as_ref(), "Class");
        assert!(class_decl.superclass.is_none());
        assert_eq!(class_decl.members.len(), 1);

        // Check method
        if let ast::ClassMember::Method(method) = &class_decl.members[0] {
            assert_eq!(method.name.name.as_ref(), "method");
            assert_eq!(method.parameters.len(), 2);
            assert_eq!(get_parameter_name(&method.parameters[0]), "arg1");
            assert_eq!(get_parameter_name(&method.parameters[1]), "arg2");

            // Verify method body contains return statement
            assert_eq!(method.body.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Return(return_stmt)) = &method.body.decls[0] {
                assert!(return_stmt.value.is_none());
            } else {
                panic!("Expected return statement in method body");
            }
        } else {
            panic!("Expected method declaration");
        }
    } else {
        panic!("Expected class declaration");
    }
}

#[test]
fn test_class_with_inheritance() {
    let source_code = r#"
            class Student : Person {
                grade = "A";
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Class(class_decl) = &program.decls[0] {
        assert_eq!(class_decl.name.name.as_ref(), "Student");
        assert!(class_decl.superclass.is_some());
        assert_eq!(
            class_decl.superclass.as_ref().unwrap().name.as_ref(),
            "Person"
        );
    } else {
        panic!("Expected class declaration");
    }
}

#[test]
fn test_lambda_declaration() {
    let source_code = r#"var add = (a, b) -> a + b;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "add");

        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
            assert_eq!(lambda.parameters.len(), 2);
            assert_eq!(get_parameter_name(&lambda.parameters[0]), "a");
            assert_eq!(get_parameter_name(&lambda.parameters[1]), "b");

            if let ast::LambdaBody::Expr(expr) = lambda.body.as_ref() {
                // Verify the expression body: a + b
                if let ast::Expr::Term(term_expr) = expr.as_ref() {
                    // Left side should be identifier 'a'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                        term_expr.left.as_ref()
                    {
                        assert_eq!(left_id.name.as_ref(), "a");
                    } else {
                        panic!("Expected identifier 'a' on left side of addition");
                    }

                    // Operator should be Add
                    assert_eq!(term_expr.operator, ast::TermOperator::Add);

                    // Right side should be identifier 'b'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(right_id)) =
                        term_expr.right.as_ref()
                    {
                        assert_eq!(right_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b' on right side of addition");
                    }
                } else {
                    panic!("Expected term expression (a + b) in lambda body");
                }
            } else {
                panic!("Expected expression body in lambda");
            }
        } else {
            panic!("Expected lambda expression");
        }
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_lambda_declaration_with_trailing_comma_in_parameters() {
    let source_code = r#"var add = (a, b,) -> a + b;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "add");

        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
            assert_eq!(lambda.parameters.len(), 2);
            assert_eq!(get_parameter_name(&lambda.parameters[0]), "a");
            assert_eq!(get_parameter_name(&lambda.parameters[1]), "b");

            if let ast::LambdaBody::Expr(expr) = lambda.body.as_ref() {
                // Verify the expression body: a + b
                if let ast::Expr::Term(term_expr) = expr.as_ref() {
                    // Left side should be identifier 'a'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                        term_expr.left.as_ref()
                    {
                        assert_eq!(left_id.name.as_ref(), "a");
                    } else {
                        panic!("Expected identifier 'a' on left side of addition");
                    }

                    // Operator should be Add
                    assert_eq!(term_expr.operator, ast::TermOperator::Add);

                    // Right side should be identifier 'b'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(right_id)) =
                        term_expr.right.as_ref()
                    {
                        assert_eq!(right_id.name.as_ref(), "b");
                    } else {
                        panic!("Expected identifier 'b' on right side of addition");
                    }
                } else {
                    panic!("Expected term expression (a + b) in lambda body");
                }
            } else {
                panic!("Expected expression body in lambda");
            }
        } else {
            panic!("Expected lambda expression");
        }
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_lambda_with_block_body() {
    let source_code = r#"var calc = (x) -> { return x * 2; };"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(get_variable_name(var_decl), "calc");

        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
            assert_eq!(lambda.parameters.len(), 1);
            assert_eq!(get_parameter_name(&lambda.parameters[0]), "x");

            if let ast::LambdaBody::Block(block) = lambda.body.as_ref() {
                // Verify block contains one return statement
                assert_eq!(block.decls.len(), 1);

                if let ast::Decl::Stmt(ast::Stmt::Return(return_stmt)) = &block.decls[0] {
                    assert!(return_stmt.value.is_some());

                    // Verify the return expression: x * 2
                    if let Some(ast::Expr::Factor(factor_expr)) = &return_stmt.value {
                        // Left side should be identifier 'x'
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                            factor_expr.left.as_ref()
                        {
                            assert_eq!(left_id.name.as_ref(), "x");
                        } else {
                            panic!("Expected identifier 'x' on left side of multiplication");
                        }

                        // Operator should be Multiply
                        assert_eq!(factor_expr.operator, ast::FactorOperator::Multiply);

                        // Right side should be number literal 2
                        if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                            factor_expr.right.as_ref()
                        {
                            assert_eq!(num_lit.value, 2.0);
                        } else {
                            panic!("Expected number literal '2' on right side of multiplication");
                        }
                    } else {
                        panic!("Expected factor expression (x * 2) in return statement");
                    }
                } else {
                    panic!("Expected return statement in lambda block");
                }
            } else {
                panic!("Expected block body in lambda");
            }
        } else {
            panic!("Expected lambda expression");
        }
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_empty_lambda_parameters() {
    let source_code = r#"var func = () -> 42;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        // Verify variable name
        assert_eq!(get_variable_name(var_decl), "func");
        assert!(var_decl.initializer.is_some());

        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
            // Verify empty parameters
            assert_eq!(lambda.parameters.len(), 0);

            // Verify lambda body is an expression: 42
            if let ast::LambdaBody::Expr(expr) = lambda.body.as_ref() {
                // Verify the expression body: 42
                if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) = expr.as_ref() {
                    assert_eq!(num_lit.value, 42.0);
                } else {
                    panic!("Expected number literal '42' in lambda body");
                }
            } else {
                panic!("Expected expression body in lambda");
            }
        } else {
            panic!("Expected lambda expression");
        }
    } else {
        panic!("Expected variable declaration");
    }
}

#[test]
fn test_variable_declaration_with_call_and_lambda() {
    let source_code = r#"var y = identity(() -> "hello world");"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (_, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
}
