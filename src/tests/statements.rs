use crate::{SourceMap, ast, parser};

#[test]
fn test_simple_variable_declaration() {
    let source_code = r#"var x = 42;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(var_decl.name.name.as_ref(), "x");
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

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(var_decl.name.name.as_ref(), "x");
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

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Function(func_decl) = &program.decls[0] {
        assert_eq!(func_decl.function.name.name.as_ref(), "add");
        assert_eq!(func_decl.function.parameters.len(), 2);
        assert_eq!(func_decl.function.parameters[0].name.as_ref(), "a");
        assert_eq!(func_decl.function.parameters[1].name.as_ref(), "b");
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

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);

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

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);
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
fn test_class_with_inheritance() {
    let source_code = r#"
            class Student : Person {
                grade = "A";
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);

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

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(var_decl.name.name.as_ref(), "add");

        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
            assert_eq!(lambda.parameters.len(), 2);
            assert_eq!(lambda.parameters[0].name.as_ref(), "a");
            assert_eq!(lambda.parameters[1].name.as_ref(), "b");

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

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);

    if let ast::Decl::Variable(var_decl) = &program.decls[0] {
        assert_eq!(var_decl.name.name.as_ref(), "calc");

        if let Some(ast::Expr::Primary(ast::PrimaryExpr::Lambda(lambda))) = &var_decl.initializer {
            assert_eq!(lambda.parameters.len(), 1);
            assert_eq!(lambda.parameters[0].name.as_ref(), "x");

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
fn test_if_statement() {
    let source_code = r#"
            if (x > 0) {
                return true;
            } else {
                return false;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Stmt(ast::Stmt::If(if_stmt)) = &program.decls[0] {
        // Verify the condition: x > 0
        if let ast::Expr::Comparison(comp_expr) = &if_stmt.condition {
            // Left side should be identifier 'x'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                comp_expr.left.as_ref()
            {
                assert_eq!(left_id.name.as_ref(), "x");
            } else {
                panic!("Expected identifier 'x' on left side of comparison");
            }

            // Operator should be Greater
            assert_eq!(comp_expr.operator, ast::ComparisonOperator::Greater);

            // Right side should be number literal 0
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) = comp_expr.right.as_ref()
            {
                assert_eq!(num_lit.value, 0.0);
            } else {
                panic!("Expected number literal '0' on right side of comparison");
            }
        } else {
            panic!("Expected comparison expression (x > 0) in if condition");
        }

        // Verify the then branch: { return true; }
        if let ast::Stmt::Block(then_block) = if_stmt.then_branch.as_ref() {
            assert_eq!(then_block.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Return(return_stmt)) = &then_block.decls[0] {
                assert!(return_stmt.value.is_some());
                if let Some(ast::Expr::Primary(ast::PrimaryExpr::Boolean(bool_lit))) =
                    &return_stmt.value
                {
                    assert_eq!(bool_lit.value, true);
                } else {
                    panic!("Expected boolean literal 'true' in then branch return");
                }
            } else {
                panic!("Expected return statement in then branch");
            }
        } else {
            panic!("Expected block statement in then branch");
        }

        // Verify the else branch: { return false; }
        assert!(if_stmt.else_branch.is_some());
        if let Some(else_branch) = &if_stmt.else_branch {
            if let ast::Stmt::Block(else_block) = else_branch.as_ref() {
                assert_eq!(else_block.decls.len(), 1);
                if let ast::Decl::Stmt(ast::Stmt::Return(return_stmt)) = &else_block.decls[0] {
                    assert!(return_stmt.value.is_some());
                    if let Some(ast::Expr::Primary(ast::PrimaryExpr::Boolean(bool_lit))) =
                        &return_stmt.value
                    {
                        assert_eq!(bool_lit.value, false);
                    } else {
                        panic!("Expected boolean literal 'false' in else branch return");
                    }
                } else {
                    panic!("Expected return statement in else branch");
                }
            } else {
                panic!("Expected block statement in else branch");
            }
        }
    } else {
        panic!("Expected if statement");
    }
}

#[test]
fn test_if_statement_without_else() {
    let source_code = r#"
            if (condition) {
                doSomething();
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::If(if_stmt)) = &program.decls[0] {
        // Verify the condition: condition (identifier)
        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(cond_id)) = &if_stmt.condition {
            assert_eq!(cond_id.name.as_ref(), "condition");
        } else {
            panic!("Expected identifier 'condition' in if condition");
        }

        // Verify the then branch: { doSomething(); }
        if let ast::Stmt::Block(then_block) = if_stmt.then_branch.as_ref() {
            assert_eq!(then_block.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &then_block.decls[0] {
                // Verify the function call: doSomething()
                if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                    // Verify the callee is 'doSomething'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                        call_expr.callee.as_ref()
                    {
                        assert_eq!(func_id.name.as_ref(), "doSomething");
                    } else {
                        panic!("Expected identifier 'doSomething' as callee");
                    }

                    // Verify it's a function call with no arguments
                    if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                        assert_eq!(args.len(), 0);
                    } else {
                        panic!("Expected function call operation");
                    }
                } else {
                    panic!("Expected call expression in if branch");
                }
            } else {
                panic!("Expected expression statement in if branch");
            }
        } else {
            panic!("Expected block statement in then branch");
        }

        // Verify no else branch
        assert!(if_stmt.else_branch.is_none());
    } else {
        panic!("Expected if statement");
    }
}

#[test]
fn test_while_statement() {
    let source_code = r#"
            while (i < 10) {
                i = i + 1;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Stmt(ast::Stmt::While(while_stmt)) = &program.decls[0] {
        // Verify the condition: i < 10
        if let ast::Expr::Comparison(comp_expr) = &while_stmt.condition {
            // Left side should be identifier 'i'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                comp_expr.left.as_ref()
            {
                assert_eq!(left_id.name.as_ref(), "i");
            } else {
                panic!("Expected identifier 'i' on left side of comparison");
            }

            // Operator should be Less
            assert_eq!(comp_expr.operator, ast::ComparisonOperator::Less);

            // Right side should be number literal 10
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) = comp_expr.right.as_ref()
            {
                assert_eq!(num_lit.value, 10.0);
            } else {
                panic!("Expected number literal '10' on right side of comparison");
            }
        } else {
            panic!("Expected comparison expression (i < 10) in while condition");
        }

        // Verify the body: { i = i + 1; }
        if let ast::Stmt::Block(body_block) = while_stmt.body.as_ref() {
            assert_eq!(body_block.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &body_block.decls[0] {
                // Verify the assignment: i = i + 1
                if let ast::Expr::Assignment(assign_expr) = &expr_stmt.expr {
                    // Verify the target is identifier 'i'
                    if let ast::AssignmentTarget::Identifier(target_id) = &assign_expr.target {
                        assert_eq!(target_id.name.as_ref(), "i");
                    } else {
                        panic!("Expected identifier 'i' as assignment target");
                    }

                    // Verify the value: i + 1
                    if let ast::Expr::Term(term_expr) = assign_expr.value.as_ref() {
                        // Left side should be identifier 'i'
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                            term_expr.left.as_ref()
                        {
                            assert_eq!(left_id.name.as_ref(), "i");
                        } else {
                            panic!("Expected identifier 'i' on left side of addition");
                        }

                        // Operator should be Add
                        assert_eq!(term_expr.operator, ast::TermOperator::Add);

                        // Right side should be number literal 1
                        if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                            term_expr.right.as_ref()
                        {
                            assert_eq!(num_lit.value, 1.0);
                        } else {
                            panic!("Expected number literal '1' on right side of addition");
                        }
                    } else {
                        panic!("Expected term expression (i + 1) as assignment value");
                    }
                } else {
                    panic!("Expected assignment expression in while body");
                }
            } else {
                panic!("Expected expression statement in while body");
            }
        } else {
            panic!("Expected block statement as while body");
        }
    } else {
        panic!("Expected while statement");
    }
}

#[test]
fn test_for_statement_with_all_clauses() {
    let source_code = r#"
            for (var i = 0; i < 10; i = i + 1) {
                print(i);
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::For(for_stmt)) = &program.decls[0] {
        assert!(for_stmt.initializer.is_some());
        assert!(for_stmt.condition.is_some());
        assert!(for_stmt.increment.is_some());

        // Verify the initializer: var i = 0
        if let Some(ast::ForInitializer::Variable(var_decl)) = &for_stmt.initializer {
            assert_eq!(var_decl.name.name.as_ref(), "i");
            assert!(var_decl.initializer.is_some());
            if let Some(ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit))) =
                &var_decl.initializer
            {
                assert_eq!(num_lit.value, 0.0);
            } else {
                panic!("Expected number literal '0' in variable initializer");
            }
        } else {
            panic!("Expected variable initializer");
        }

        // Verify the condition: i < 10
        if let Some(ast::Expr::Comparison(comp_expr)) = &for_stmt.condition {
            // Left side should be identifier 'i'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                comp_expr.left.as_ref()
            {
                assert_eq!(left_id.name.as_ref(), "i");
            } else {
                panic!("Expected identifier 'i' on left side of comparison");
            }

            // Operator should be Less
            assert_eq!(comp_expr.operator, ast::ComparisonOperator::Less);

            // Right side should be number literal 10
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) = comp_expr.right.as_ref()
            {
                assert_eq!(num_lit.value, 10.0);
            } else {
                panic!("Expected number literal '10' on right side of comparison");
            }
        } else {
            panic!("Expected comparison expression in for condition");
        }

        // Verify the increment: i = i + 1
        if let Some(ast::Expr::Assignment(assign_expr)) = &for_stmt.increment {
            // Verify the target is identifier 'i'
            if let ast::AssignmentTarget::Identifier(target_id) = &assign_expr.target {
                assert_eq!(target_id.name.as_ref(), "i");
            } else {
                panic!("Expected identifier 'i' as assignment target");
            }

            // Verify the value: i + 1
            if let ast::Expr::Term(term_expr) = assign_expr.value.as_ref() {
                // Left side should be identifier 'i'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                    term_expr.left.as_ref()
                {
                    assert_eq!(left_id.name.as_ref(), "i");
                } else {
                    panic!("Expected identifier 'i' on left side of addition");
                }

                // Operator should be Add
                assert_eq!(term_expr.operator, ast::TermOperator::Add);

                // Right side should be number literal 1
                if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                    term_expr.right.as_ref()
                {
                    assert_eq!(num_lit.value, 1.0);
                } else {
                    panic!("Expected number literal '1' on right side of addition");
                }
            } else {
                panic!("Expected term expression (i + 1) as assignment value");
            }
        } else {
            panic!("Expected assignment expression in for increment");
        }

        // Verify the body: { print(i); }
        if let ast::Stmt::Block(body_block) = for_stmt.body.as_ref() {
            assert_eq!(body_block.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &body_block.decls[0] {
                // Verify the function call: print(i)
                if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                    // Verify the callee is 'print'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                        call_expr.callee.as_ref()
                    {
                        assert_eq!(func_id.name.as_ref(), "print");
                    } else {
                        panic!("Expected identifier 'print' as callee");
                    }

                    // Verify it's a function call with one argument 'i'
                    if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                        assert_eq!(args.len(), 1);
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(arg_id)) = &args[0] {
                            assert_eq!(arg_id.name.as_ref(), "i");
                        } else {
                            panic!("Expected identifier 'i' as function argument");
                        }
                    } else {
                        panic!("Expected function call operation");
                    }
                } else {
                    panic!("Expected call expression in for body");
                }
            } else {
                panic!("Expected expression statement in for body");
            }
        } else {
            panic!("Expected block statement as for body");
        }
    } else {
        panic!("Expected for statement");
    }
}

#[test]
fn test_for_statement_with_expression_initializer() {
    let source_code = r#"
            for (i = 0; i < 10; i = i + 1) {
                print(i);
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::For(for_stmt)) = &program.decls[0] {
        // Verify the expression initializer: i = 0
        if let Some(ast::ForInitializer::Expr(init_expr)) = &for_stmt.initializer {
            if let ast::Expr::Assignment(assign_expr) = init_expr {
                // Verify the target is identifier 'i'
                if let ast::AssignmentTarget::Identifier(target_id) = &assign_expr.target {
                    assert_eq!(target_id.name.as_ref(), "i");
                } else {
                    panic!("Expected identifier 'i' as assignment target");
                }

                // Verify the value is number literal 0
                if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                    assign_expr.value.as_ref()
                {
                    assert_eq!(num_lit.value, 0.0);
                } else {
                    panic!("Expected number literal '0' as assignment value");
                }
            } else {
                panic!("Expected assignment expression in initializer");
            }
        } else {
            panic!("Expected expression initializer");
        }

        // Verify the condition: i < 10
        assert!(for_stmt.condition.is_some());
        if let Some(ast::Expr::Comparison(comp_expr)) = &for_stmt.condition {
            // Left side should be identifier 'i'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                comp_expr.left.as_ref()
            {
                assert_eq!(left_id.name.as_ref(), "i");
            } else {
                panic!("Expected identifier 'i' on left side of comparison");
            }

            // Operator should be Less
            assert_eq!(comp_expr.operator, ast::ComparisonOperator::Less);

            // Right side should be number literal 10
            if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) = comp_expr.right.as_ref()
            {
                assert_eq!(num_lit.value, 10.0);
            } else {
                panic!("Expected number literal '10' on right side of comparison");
            }
        } else {
            panic!("Expected comparison expression in for condition");
        }

        // Verify the increment: i = i + 1
        assert!(for_stmt.increment.is_some());
        if let Some(ast::Expr::Assignment(assign_expr)) = &for_stmt.increment {
            // Verify the target is identifier 'i'
            if let ast::AssignmentTarget::Identifier(target_id) = &assign_expr.target {
                assert_eq!(target_id.name.as_ref(), "i");
            } else {
                panic!("Expected identifier 'i' as assignment target");
            }

            // Verify the value: i + 1
            if let ast::Expr::Term(term_expr) = assign_expr.value.as_ref() {
                // Left side should be identifier 'i'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                    term_expr.left.as_ref()
                {
                    assert_eq!(left_id.name.as_ref(), "i");
                } else {
                    panic!("Expected identifier 'i' on left side of addition");
                }

                // Operator should be Add
                assert_eq!(term_expr.operator, ast::TermOperator::Add);

                // Right side should be number literal 1
                if let ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit)) =
                    term_expr.right.as_ref()
                {
                    assert_eq!(num_lit.value, 1.0);
                } else {
                    panic!("Expected number literal '1' on right side of addition");
                }
            } else {
                panic!("Expected term expression (i + 1) as assignment value");
            }
        } else {
            panic!("Expected assignment expression in for increment");
        }
    } else {
        panic!("Expected for statement");
    }
}

#[test]
fn test_for_statement_minimal() {
    let source_code = r#"
            for (;;) {
                break;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::For(for_stmt)) = &program.decls[0] {
        assert!(for_stmt.initializer.is_none());
        assert!(for_stmt.condition.is_none());
        assert!(for_stmt.increment.is_none());

        // Verify the body: { break; }
        if let ast::Stmt::Block(body_block) = for_stmt.body.as_ref() {
            assert_eq!(body_block.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Break(_)) = &body_block.decls[0] {
                // Break statement verified
            } else {
                panic!("Expected break statement in for body");
            }
        } else {
            panic!("Expected block statement as for body");
        }
    } else {
        panic!("Expected for statement");
    }
}

#[test]
fn test_break_and_continue_statements() {
    let source_code = r#"
            while (true) {
                if (condition1) {
                    break;
                }
                if (condition2) {
                    continue;
                }
                doWork();
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 1);

    if let ast::Decl::Stmt(ast::Stmt::While(while_stmt)) = &program.decls[0] {
        // Verify the condition: true
        if let ast::Expr::Primary(ast::PrimaryExpr::Boolean(bool_lit)) = &while_stmt.condition {
            assert_eq!(bool_lit.value, true);
        } else {
            panic!("Expected boolean literal 'true' in while condition");
        }

        // Verify the body contains 3 statements
        if let ast::Stmt::Block(body_block) = while_stmt.body.as_ref() {
            assert_eq!(body_block.decls.len(), 3);

            // First statement: if (condition1) { break; }
            if let ast::Decl::Stmt(ast::Stmt::If(if_stmt1)) = &body_block.decls[0] {
                // Verify condition1
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(cond_id)) =
                    &if_stmt1.condition
                {
                    assert_eq!(cond_id.name.as_ref(), "condition1");
                } else {
                    panic!("Expected identifier 'condition1' in first if condition");
                }

                // Verify then branch contains break
                if let ast::Stmt::Block(then_block) = if_stmt1.then_branch.as_ref() {
                    assert_eq!(then_block.decls.len(), 1);
                    if let ast::Decl::Stmt(ast::Stmt::Break(_)) = &then_block.decls[0] {
                        // Break statement verified
                    } else {
                        panic!("Expected break statement in first if then branch");
                    }
                } else {
                    panic!("Expected block statement in first if then branch");
                }

                // Verify no else branch
                assert!(if_stmt1.else_branch.is_none());
            } else {
                panic!("Expected first if statement");
            }

            // Second statement: if (condition2) { continue; }
            if let ast::Decl::Stmt(ast::Stmt::If(if_stmt2)) = &body_block.decls[1] {
                // Verify condition2
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(cond_id)) =
                    &if_stmt2.condition
                {
                    assert_eq!(cond_id.name.as_ref(), "condition2");
                } else {
                    panic!("Expected identifier 'condition2' in second if condition");
                }

                // Verify then branch contains continue
                if let ast::Stmt::Block(then_block) = if_stmt2.then_branch.as_ref() {
                    assert_eq!(then_block.decls.len(), 1);
                    if let ast::Decl::Stmt(ast::Stmt::Continue(_)) = &then_block.decls[0] {
                        // Continue statement verified
                    } else {
                        panic!("Expected continue statement in second if then branch");
                    }
                } else {
                    panic!("Expected block statement in second if then branch");
                }

                // Verify no else branch
                assert!(if_stmt2.else_branch.is_none());
            } else {
                panic!("Expected second if statement");
            }

            // Third statement: doWork();
            if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &body_block.decls[2] {
                // Verify the function call: doWork()
                if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                    // Verify the callee is 'doWork'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                        call_expr.callee.as_ref()
                    {
                        assert_eq!(func_id.name.as_ref(), "doWork");
                    } else {
                        panic!("Expected identifier 'doWork' as callee");
                    }

                    // Verify it's a function call with no arguments
                    if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                        assert_eq!(args.len(), 0);
                    } else {
                        panic!("Expected function call operation");
                    }
                } else {
                    panic!("Expected call expression for doWork");
                }
            } else {
                panic!("Expected expression statement for doWork call");
            }
        } else {
            panic!("Expected block statement as while body");
        }
    } else {
        panic!("Expected while statement");
    }
}

#[test]
fn test_return_statement_without_value() {
    let source_code = r#"
            fn test() {
                return;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parser::tests::parse_source(&source_map);

    parser::tests::assert_no_parse_errors(&errors);

    if let ast::Decl::Function(func_decl) = &program.decls[0] {
        if let ast::Decl::Stmt(ast::Stmt::Return(ret_stmt)) = &func_decl.function.body.decls[0] {
            assert!(ret_stmt.value.is_none());
        } else {
            panic!("Expected return statement");
        }
    }
}
