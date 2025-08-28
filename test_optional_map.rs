use qanglang_core::{SourceMap, tests::parse_source, tests::assert_no_parse_errors, ast};

fn main() {
    let source_code = r#"
        var maybe = nil;
        var result = maybe?|x -> x * 2|;
    "#;
    let source_map = SourceMap::new(source_code.to_string());
    
    let (program, errors) = parse_source(&source_map);
    
    if errors.has_errors() {
        for error in errors.errors() {
            println!("Parse error: {}", error.message);
        }
        println!("Test failed: parse errors occurred");
        return;
    }
    
    println!("Program parsed successfully!");
    println!("Number of declarations: {}", program.decls.len());
    
    // Check the optional map expression structure
    if let Some(ast::Decl::Variable(var_decl)) = program.decls.get(1) {
        if let Some(ast::Expr::OptionalMap(_map_expr)) = &var_decl.initializer {
            println!("✓ Optional map expression found and parsed correctly");
        } else {
            println!("✗ Expected optional map expression but found different expression type");
        }
    } else {
        println!("✗ Expected variable declaration at index 1");
    }
}