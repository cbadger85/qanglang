use super::{assert_no_parse_errors, parse_source};
use crate::{SourceMap, ast};

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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
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
fn test_return_statement() {
    let source_code = r#"
            fn test() {
                return 42;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Function(func_decl) = &program.decls[0] {
        // Verify function name
        assert_eq!(func_decl.function.name.name.as_ref(), "test");
        assert_eq!(func_decl.function.parameters.len(), 0);
        assert_eq!(func_decl.function.body.decls.len(), 1);

        if let ast::Decl::Stmt(ast::Stmt::Return(ret_stmt)) = &func_decl.function.body.decls[0] {
            assert!(ret_stmt.value.is_some());

            // Verify the return value: 42
            if let Some(ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit))) = &ret_stmt.value {
                assert_eq!(num_lit.value, 42.0);
            } else {
                panic!("Expected number literal '42' in return statement");
            }
        } else {
            panic!("Expected return statement");
        }
    } else {
        panic!("Expected function declaration");
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

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Function(func_decl) = &program.decls[0] {
        if let ast::Decl::Stmt(ast::Stmt::Return(ret_stmt)) = &func_decl.function.body.decls[0] {
            assert!(ret_stmt.value.is_none());
        } else {
            panic!("Expected return statement");
        }
    }
}

#[test]
fn test_try_catch_finally() {
    let source_code = r#"
            try {
                riskyOperation();
            } catch (error) {
                handleError(error);
            } finally {
                cleanup();
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::Try(try_stmt)) = &program.decls[0] {
        // Verify try block: { riskyOperation(); }
        assert_eq!(try_stmt.try_block.decls.len(), 1);
        if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &try_stmt.try_block.decls[0] {
            if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                    &*call_expr.callee
                {
                    assert_eq!(func_id.name.as_ref(), "riskyOperation");
                } else {
                    panic!("Expected identifier 'riskyOperation' as callee");
                }
                if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                    assert_eq!(args.len(), 0);
                } else {
                    panic!("Expected function call operation");
                }
            } else {
                panic!("Expected call expression in try block");
            }
        } else {
            panic!("Expected expression statement in try block");
        }

        // Verify catch clause: catch (error) { handleError(error); }
        assert!(try_stmt.catch_clause.is_some());
        if let Some(catch_clause) = &try_stmt.catch_clause {
            // Verify catch parameter
            assert!(catch_clause.parameter.is_some());
            if let Some(param) = &catch_clause.parameter {
                assert_eq!(param.name.as_ref(), "error");
            } else {
                panic!("Expected catch parameter 'error'");
            }

            // Verify catch body: { handleError(error); }
            assert_eq!(catch_clause.body.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &catch_clause.body.decls[0] {
                if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                    // Verify callee is 'handleError'
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                        &*call_expr.callee
                    {
                        assert_eq!(func_id.name.as_ref(), "handleError");
                    } else {
                        panic!("Expected identifier 'handleError' as callee");
                    }

                    // Verify argument is 'error'
                    if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                        assert_eq!(args.len(), 1);
                        if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(arg_id)) = &args[0] {
                            assert_eq!(arg_id.name.as_ref(), "error");
                        } else {
                            panic!("Expected identifier 'error' as function argument");
                        }
                    } else {
                        panic!("Expected function call operation");
                    }
                } else {
                    panic!("Expected call expression in catch block");
                }
            } else {
                panic!("Expected expression statement in catch block");
            }
        }

        // Verify finally block: { cleanup(); }
        assert!(try_stmt.finally_block.is_some());
        if let Some(finally_block) = &try_stmt.finally_block {
            assert_eq!(finally_block.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &finally_block.decls[0] {
                if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                        &*call_expr.callee
                    {
                        assert_eq!(func_id.name.as_ref(), "cleanup");
                    } else {
                        panic!("Expected identifier 'cleanup' as callee");
                    }
                    if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                        assert_eq!(args.len(), 0);
                    } else {
                        panic!("Expected function call operation");
                    }
                } else {
                    panic!("Expected call expression in finally block");
                }
            } else {
                panic!("Expected expression statement in finally block");
            }
        }
    } else {
        panic!("Expected try statement");
    }
}

#[test]
fn test_try_catch_without_parameter() {
    let source_code = r#"
            try {
                riskyOperation();
            } catch {
                handleError();
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::Try(try_stmt)) = &program.decls[0] {
        // Verify try block: { riskyOperation(); }
        assert_eq!(try_stmt.try_block.decls.len(), 1);
        if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &try_stmt.try_block.decls[0] {
            if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                    &*call_expr.callee
                {
                    assert_eq!(func_id.name.as_ref(), "riskyOperation");
                } else {
                    panic!("Expected identifier 'riskyOperation' as callee");
                }
                if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                    assert_eq!(args.len(), 0);
                } else {
                    panic!("Expected function call operation");
                }
            } else {
                panic!("Expected call expression in try block");
            }
        } else {
            panic!("Expected expression statement in try block");
        }

        // Verify catch clause without parameter: catch { handleError(); }
        let catch_clause = try_stmt.catch_clause.as_ref().unwrap();
        assert!(catch_clause.parameter.is_none());

        // Verify catch body: { handleError(); }
        assert_eq!(catch_clause.body.decls.len(), 1);
        if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &catch_clause.body.decls[0] {
            if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                    &*call_expr.callee
                {
                    assert_eq!(func_id.name.as_ref(), "handleError");
                } else {
                    panic!("Expected identifier 'handleError' as callee");
                }
                if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                    assert_eq!(args.len(), 0);
                } else {
                    panic!("Expected function call operation");
                }
            } else {
                panic!("Expected call expression in catch block");
            }
        } else {
            panic!("Expected expression statement in catch block");
        }

        // Verify no finally block
        assert!(try_stmt.finally_block.is_none());
    } else {
        panic!("Expected try statement");
    }
}

#[test]
fn test_try_finally_without_catch() {
    let source_code = r#"
            try {
                riskyOperation();
            } finally {
                cleanup();
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::Try(try_stmt)) = &program.decls[0] {
        // Verify try block: { riskyOperation(); }
        assert_eq!(try_stmt.try_block.decls.len(), 1);
        if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &try_stmt.try_block.decls[0] {
            if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                    &*call_expr.callee
                {
                    assert_eq!(func_id.name.as_ref(), "riskyOperation");
                } else {
                    panic!("Expected identifier 'riskyOperation' as callee");
                }
                if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                    assert_eq!(args.len(), 0);
                } else {
                    panic!("Expected function call operation");
                }
            } else {
                panic!("Expected call expression in try block");
            }
        } else {
            panic!("Expected expression statement in try block");
        }

        // Verify no catch clause
        assert!(try_stmt.catch_clause.is_none());

        // Verify finally block: { cleanup(); }
        assert!(try_stmt.finally_block.is_some());
        if let Some(finally_block) = &try_stmt.finally_block {
            assert_eq!(finally_block.decls.len(), 1);
            if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &finally_block.decls[0] {
                if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
                    if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                        &*call_expr.callee
                    {
                        assert_eq!(func_id.name.as_ref(), "cleanup");
                    } else {
                        panic!("Expected identifier 'cleanup' as callee");
                    }
                    if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                        assert_eq!(args.len(), 0);
                    } else {
                        panic!("Expected function call operation");
                    }
                } else {
                    panic!("Expected call expression in finally block");
                }
            } else {
                panic!("Expected expression statement in finally block");
            }
        }
    } else {
        panic!("Expected try statement");
    }
}

#[test]
fn test_throw_statement() {
    let source_code = r#"
            throw Error("Something went wrong");
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::Throw(throw_stmt)) = &program.decls[0] {
        assert!(throw_stmt.value.is_some());

        // Verify the thrown expression: new Error("Something went wrong")
        if let Some(ast::Expr::Call(call_expr)) = &throw_stmt.value {
            // Verify the callee is 'Error'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) = &*call_expr.callee {
                assert_eq!(func_id.name.as_ref(), "Error");
            } else {
                panic!("Expected identifier 'Error' as callee");
            }

            // Verify it's a function call with one string argument
            if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                assert_eq!(args.len(), 1);
                if let ast::Expr::Primary(ast::PrimaryExpr::String(str_lit)) = &args[0] {
                    assert_eq!(str_lit.value.as_ref(), "Something went wrong");
                } else {
                    panic!("Expected string literal 'Something went wrong' as function argument");
                }
            } else {
                panic!("Expected function call operation");
            }
        } else {
            panic!("Expected call expression in throw statement");
        }
    } else {
        panic!("Expected throw statement");
    }
}

#[test]
fn test_throw_statement_without_value() {
    let source_code = r#"throw;"#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::Throw(throw_stmt)) = &program.decls[0] {
        assert!(throw_stmt.value.is_none());
    } else {
        panic!("Expected throw statement");
    }
}

#[test]
fn test_block_statements() {
    let source_code = r#"
            {
                var x = 1;
                var y = 2;
                x + y;
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);

    if let ast::Decl::Stmt(ast::Stmt::Block(block)) = &program.decls[0] {
        assert_eq!(block.decls.len(), 3);

        // First declaration: var x = 1;
        if let ast::Decl::Variable(var_decl) = &block.decls[0] {
            assert_eq!(var_decl.name.name.as_ref(), "x");
            assert!(var_decl.initializer.is_some());
            if let Some(ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit))) =
                &var_decl.initializer
            {
                assert_eq!(num_lit.value, 1.0);
            } else {
                panic!("Expected number literal '1' in first variable declaration");
            }
        } else {
            panic!("Expected first variable declaration");
        }

        // Second declaration: var y = 2;
        if let ast::Decl::Variable(var_decl) = &block.decls[1] {
            assert_eq!(var_decl.name.name.as_ref(), "y");
            assert!(var_decl.initializer.is_some());
            if let Some(ast::Expr::Primary(ast::PrimaryExpr::Number(num_lit))) =
                &var_decl.initializer
            {
                assert_eq!(num_lit.value, 2.0);
            } else {
                panic!("Expected number literal '2' in second variable declaration");
            }
        } else {
            panic!("Expected second variable declaration");
        }

        // Third declaration: x + y; (expression statement)
        if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &block.decls[2] {
            // Verify the expression: x + y
            if let ast::Expr::Term(term_expr) = &expr_stmt.expr {
                // Left side should be identifier 'x'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                    term_expr.left.as_ref()
                {
                    assert_eq!(left_id.name.as_ref(), "x");
                } else {
                    panic!("Expected identifier 'x' on left side of addition");
                }

                // Operator should be Add
                assert_eq!(term_expr.operator, ast::TermOperator::Add);

                // Right side should be identifier 'y'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(right_id)) =
                    term_expr.right.as_ref()
                {
                    assert_eq!(right_id.name.as_ref(), "y");
                } else {
                    panic!("Expected identifier 'y' on right side of addition");
                }
            } else {
                panic!("Expected term expression (x + y) in expression statement");
            }
        } else {
            panic!("Expected expression statement for x + y");
        }
    } else {
        panic!("Expected block statement");
    }
}

#[test]
fn test_expression_statements() {
    let source_code = r#"
            someFunction();
            obj.method();
            x + y;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 3);

    // First expression statement: someFunction();
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[0] {
        if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
            // Verify the callee is 'someFunction'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(func_id)) =
                call_expr.callee.as_ref()
            {
                assert_eq!(func_id.name.as_ref(), "someFunction");
            } else {
                panic!("Expected identifier 'someFunction' as callee");
            }

            // Verify it's a function call with no arguments
            if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                assert_eq!(args.len(), 0);
            } else {
                panic!("Expected function call operation");
            }
        } else {
            panic!("Expected call expression in first statement");
        }
    } else {
        panic!("Expected first expression statement");
    }

    // Second expression statement: obj.method();
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[1] {
        if let ast::Expr::Call(call_expr) = &expr_stmt.expr {
            // Verify it's a chained call: obj.method()
            if let ast::Expr::Call(inner_call) = call_expr.callee.as_ref() {
                // Verify the base object is 'obj'
                if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(obj_id)) =
                    inner_call.callee.as_ref()
                {
                    assert_eq!(obj_id.name.as_ref(), "obj");
                } else {
                    panic!("Expected identifier 'obj' as base object");
                }

                // Verify property access to 'method'
                if let ast::CallOperation::Property(method_id) = inner_call.operation.as_ref() {
                    assert_eq!(method_id.name.as_ref(), "method");
                } else {
                    panic!("Expected property access to 'method'");
                }

                // Verify the final call has no arguments
                if let ast::CallOperation::Call(args) = call_expr.operation.as_ref() {
                    assert_eq!(args.len(), 0);
                } else {
                    panic!("Expected function call operation");
                }
            } else {
                panic!("Expected chained call expression for obj.method()");
            }
        } else {
            panic!("Expected call expression in second statement");
        }
    } else {
        panic!("Expected second expression statement");
    }

    // Third expression statement: x + y;
    if let ast::Decl::Stmt(ast::Stmt::Expr(expr_stmt)) = &program.decls[2] {
        if let ast::Expr::Term(term_expr) = &expr_stmt.expr {
            // Left side should be identifier 'x'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(left_id)) =
                term_expr.left.as_ref()
            {
                assert_eq!(left_id.name.as_ref(), "x");
            } else {
                panic!("Expected identifier 'x' on left side of addition");
            }

            // Operator should be Add
            assert_eq!(term_expr.operator, ast::TermOperator::Add);

            // Right side should be identifier 'y'
            if let ast::Expr::Primary(ast::PrimaryExpr::Identifier(right_id)) =
                term_expr.right.as_ref()
            {
                assert_eq!(right_id.name.as_ref(), "y");
            } else {
                panic!("Expected identifier 'y' on right side of addition");
            }
        } else {
            panic!("Expected term expression (x + y) in third statement");
        }
    } else {
        panic!("Expected third expression statement");
    }
}

#[test]
fn test_comments_ignored() {
    let source_code = r#"
            // This is a comment
            var x = 5; // Another comment
            /* Multi-line
               comment */
            var y = 10;
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 2);

    for decl in program.decls {
        match decl {
            ast::Decl::Variable(_) => (),
            _ => panic!("Expected variable statement."),
        }
    }
}
