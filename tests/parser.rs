use qanglang::{ErrorReporter, Parser, SourceMap, ast};

pub fn parse_source(source_map: &SourceMap) -> (ast::Program, ErrorReporter) {
    let mut parser = Parser::new(&source_map);
    let program = parser.parse();
    let errors = parser.into_reporter();
    (program, errors)
}

pub fn assert_no_parse_errors(errors: &ErrorReporter) {
    if errors.has_errors() {
        panic!("Unexpected parse errors:\n{}", errors.format_errors());
    }
}

#[test]
fn test_empty_program() {
    let source_code = r#""#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert_eq!(program.decls.len(), 0);
}

#[test]
fn test_complete_program_example() {
    let source_code = r#"
            class Calculator {
                add(a, b) {
                    return a + b;
                }
                
                multiply(a, b) {
                    return a * b;
                }
            }
            
            var calc = Calculator();
            var result = calc.add(5, 3);
            
            if (result > 0) {
                print("Positive result: " + result);
            } else {
                print("Non-positive result");
            }
            
            for (var i = 0; i < 5; i = i + 1) {
                var doubled = calc.multiply(i, 2);
                print("Double of " + i + " is " + doubled);
            }
            
            try {
                var risky = someRiskyOperation();
                print("Success: " + risky);
            } catch (error) {
                print("Error occurred: " + error);
            } finally {
                print("Cleanup completed");
            }
        "#;
    let source_map = SourceMap::new(source_code.to_string());

    let (program, errors) = parse_source(&source_map);

    assert_no_parse_errors(&errors);
    assert!(program.decls.len() > 0);

    // TODO replace the below with test that actually verify the AST was created correctly.

    // Verify we have the expected top-level declarations
    let mut class_count = 0;
    let mut var_count = 0;
    let mut stmt_count = 0;

    for decl in &program.decls {
        match decl {
            ast::Decl::Class(_) => class_count += 1,
            ast::Decl::Variable(_) => var_count += 1,
            ast::Decl::Stmt(_) => stmt_count += 1,
            _ => {}
        }
    }

    assert!(class_count > 0, "Should have at least one class");
    assert!(var_count > 0, "Should have at least one variable");
    assert!(stmt_count > 0, "Should have at least one statement");
}
