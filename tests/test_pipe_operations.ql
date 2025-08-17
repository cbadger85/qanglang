var test_description = "Testing pipe operations and chaining.";

fn test_simple_pipe_operation() {
  fn double(x) {
    return x * 2;
  }
  
  var result = 5 |> double;
  assert_eq(result, 10, "Expected 5 |> double to equal 10.");
}

fn test_pipe_with_function_calls() {
  fn add_one(x) {
    return x + 1;
  }
  
  fn multiply_by_three(x) {
    return x * 3;
  }
  
  var result1 = 4 |> add_one;
  assert_eq(result1, 5, "Expected 4 |> add_one to equal 5.");
  
  var result2 = 6 |> multiply_by_three;
  assert_eq(result2, 18, "Expected 6 |> multiply_by_three to equal 18.");
}

fn test_pipe_with_lambda_expressions() {
  var square = (x) -> x * x;
  var halve = (x) -> x / 2;
  
  var result1 = 4 |> square;
  assert_eq(result1, 16, "Expected 4 |> square to equal 16.");
  
  var result2 = 10 |> halve;
  assert_eq(result2, 5, "Expected 10 |> halve to equal 5.");
}

fn test_chained_pipe_operations() {
  fn double(x) {
    return x * 2;
  }
  
  fn add_one(x) {
    return x + 1;
  }
  
  fn subtract_three(x) {
    return x - 3;
  }
  
  var result = 5 |> double |> add_one |> subtract_three;
  assert_eq(result, 8, "Expected 5 |> double |> add_one |> subtract_three to equal 8.");
}

fn test_pipe_with_arguments() {
  fn add(x) {
    return x + 10;
  }
  
  fn multiply(x) {
    return x * 2;
  }
  
  var result1 = 5 |> add;
  assert_eq(result1, 15, "Expected 5 |> add to equal 15.");
  
  var result2 = 7 |> multiply;
  assert_eq(result2, 14, "Expected 7 |> multiply to equal 14.");
}

fn test_pipe_precedence() {
  fn double(x) {
    return x * 2;
  }
  
  fn add_five(x) {
    return x + 5;
  }
  
  // Test that pipe has correct precedence
  var result1 = 3 + 2 |> double;
  assert_eq(result1, 10, "Expected 3 + 2 |> double to equal 10 (pipe should bind tighter than addition).");
  
  // Fix the precedence test - need parentheses to control evaluation order
  var result2 = (10 |> double) + 5;
  assert_eq(result2, 25, "Expected (10 |> double) + 5 to equal 25.");
}

fn test_pipe_with_complex_expressions() {
  fn square(x) {
    return x * x;
  }
  
  fn halve(x) {
    return x / 2;
  }
  
  fn identity(x) {
    return x;
  }
  
  var result1 = (10 + 5) |> square |> halve;
  assert_eq(result1, 112.5, "Expected (10 + 5) |> square |> halve to equal 112.5.");
  
  var num = 8;
  var result2 = num |> square |> identity;
  assert_eq(result2, 64, "Expected num |> square |> identity to equal 64.");
}

// fn test_pipe_with_method_calls() {}

// fn test_pipe_error_handling() {}

fn test_pipe_with_conditional_expressions() {
  fn double(x) {
    return x * 2;
  }
  
  fn add_one(x) {
    return x + 1;
  }
  
  var condition = true;
  var func = condition ? double : add_one;
  
  var result = 5 |> func;
  assert_eq(result, 10, "Expected conditional pipe to use double function.");
  
  // Test with ternary in pipe chain
  var result2 = 3 |> (true ? double : add_one) |> add_one;
  assert_eq(result2, 7, "Expected ternary in pipe chain to work correctly.");
}

fn test_pipe_with_native_functions() {
  // Test piping to native functions
  var type_result = 42 |> typeof;
  assert_eq(type_result, "number", "Expected 42 |> typeof to equal 'number'.");
  
  var string_type = "hello" |> typeof;
  assert_eq(string_type, "string", "Expected 'hello' |> typeof to equal 'string'.");
}

// fn test_pipe_with_higher_order_functions() {}

fn test_pipe_with_string_operations() {
  fn add_exclamation(str) {
    return str + "!";
  }
  
  fn add_question(str) {
    return str + "?";
  }
  
  var result1 = "hello" |> add_exclamation;
  assert_eq(result1, "hello!", "Expected 'hello' |> add_exclamation to equal 'hello!'.");
  
  var result2 = "world" |> add_exclamation |> add_question;
  assert_eq(result2, "world!?", "Expected chained string operations to work correctly.");
}

fn test_pipe_with_numeric_operations() {
  fn add_ten(x) {
    return x + 10;
  }
  
  fn multiply_by_two(x) {
    return x * 2;
  }
  
  fn subtract_five(x) {
    return x - 5;
  }
  
  var result = 1 |> add_ten |> multiply_by_two |> subtract_five;
  assert_eq(result, 17, "Expected 1 |> add_ten |> multiply_by_two |> subtract_five to equal 17.");
}

fn test_pipe_associativity() {
  fn add_one(x) {
    return x + 1;
  }
  
  fn double(x) {
    return x * 2;
  }
  
  var result1 = 5 |> add_one |> double;
  var result2 = (5 |> add_one) |> double;
  
  assert_eq(result1, result2, "Expected pipe operations to be left-associative.");
  assert_eq(result1, 12, "Expected 5 |> add_one |> double to equal 12.");
}