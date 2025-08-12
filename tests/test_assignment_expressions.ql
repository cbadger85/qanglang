var test_description = "Testing assignment expressions and operators.";

fn test_simple_variable_assignment() {
  var test_var;
  
  test_var = "test";

  assert_eq(test_var, "test", "Expected test_var to be 'test'.");
}

fn test_assignment_with_expression() {
  var test_var;
  
  test_var = 2 + 2 + 2;

  assert_eq(test_var, 6, "Expected test_var to be 6.");
}

// fn test_assignment_to_object_field() {}

// fn test_assignment_to_array_index() {}

fn test_chained_assignments() {
  var test_var_1;
  var test_var_2 = test_var_1 = "test";
  
  assert_eq(test_var_1, "test", "Expected test_var_1 to be 'test'.");
  assert_eq(test_var_2, "test", "Expected test_var_2 to be 'test'.");
}

// fn test_assignment_return_value() {}

// fn test_assignment_in_expression_context() {}

// fn test_assignment_precedence() {}

// fn test_assignment_to_function_call_result() {}

// fn test_assignment_error_cases() {}
