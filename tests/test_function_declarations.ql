var test_description = "Testing function declarations and definitions.";

fn test_function_declaration_without_parameters() {
  fn simple_function() {
    return "hello";
  }
  
  var result = simple_function();
  assert_eq(result, "hello", "Expected function to return 'hello'.");
}

fn test_function_declaration_with_single_parameter() {
  fn add_one(x) {
    return x + 1;
  }
  
  var result = add_one(5);
  assert_eq(result, 6, "Expected add_one(5) to return 6.");
}

fn test_function_declaration_with_multiple_parameters() {
  fn add_three(x, y, z) {
    return x + y + z;
  }
  
  var result = add_three(1, 2, 3);
  assert_eq(result, 6, "Expected add_three(1, 2, 3) to return 6.");
}

fn test_function_declaration_with_trailing_comma() {
  fn add_two(x, y,) {
    return x + y;
  }
  
  var result = add_two(3, 4);
  assert_eq(result, 7, "Expected add_two(3, 4) to return 7.");
}

fn test_function_with_empty_body() {
  fn empty_function() {}
  
  var result = empty_function();
  assert_eq(result, nil, "Expected empty function to return nil.");
}

fn test_function_with_return_statement() {
  fn early_return() {
    return;
    var unreachable = true;
  }
  
  var result = early_return();
  assert_eq(result, nil, "Expected early return to return nil.");
}

fn test_function_with_return_value() {
  fn return_value() {
    return 42;
  }
  
  var result = return_value();
  assert_eq(result, 42, "Expected function to return 42.");
}

fn test_function_with_return_expression() {
  fn calculate(x) {
    return x * 2 + 1;
  }
  
  var result = calculate(5);
  assert_eq(result, 11, "Expected calculate(5) to return 11.");
}

fn test_function_recursion() {
  var factorial;
  factorial = (n) -> {
    if (n <= 1) {
      return 1;
    }
    return n * factorial(n - 1);
  };
  
  var result = factorial(5);
  assert_eq(result, 120, "Expected factorial(5) to return 120.");
}

fn test_nested_function_declarations() {
  fn outer_function() {
    fn inner_function() {
      return "inner";
    }
    return inner_function();
  }
  
  var result = outer_function();
  assert_eq(result, "inner", "Expected nested function to work correctly.");
}

fn test_function_parameter_shadowing() {
  var x = 10;
  
  fn shadow_test(x) {
    return x + 1;
  }
  
  var result = shadow_test(5);
  assert_eq(result, 6, "Expected parameter to shadow outer variable.");
  assert_eq(x, 10, "Expected outer variable to remain unchanged.");
}

fn test_function_local_variables() {
  fn local_vars_test() {
    var local_var = "local";
    var another_local = "42";
    return local_var + " " + another_local;
  }
  
  var result = local_vars_test();
  assert_eq(result, "local 42", "Expected function to handle local variables correctly.");
}