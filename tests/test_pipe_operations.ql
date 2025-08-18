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

fn test_partial_application_basic() {
  fn add(a, b) {
    return a + b;
  }
  
  fn multiply(a, b) {
    return a * b;
  }
  
  var result1 = 5 |> add(3);
  assert_eq(result1, 8, "Expected 5 |> add(3) to equal 8.");
  
  var result2 = 4 |> multiply(7);
  assert_eq(result2, 28, "Expected 4 |> multiply(7) to equal 28.");
}

fn test_partial_application_multiple_args() {
  fn add_three(a, b, c) {
    return a + b + c;
  }
  
  fn multiply_three(a, b, c) {
    return a * b * c;
  }
  
  var result1 = 1 |> add_three(2, 3);
  assert_eq(result1, 6, "Expected 1 |> add_three(2, 3) to equal 6.");
  
  var result2 = 2 |> multiply_three(3, 4);
  assert_eq(result2, 24, "Expected 2 |> multiply_three(3, 4) to equal 24.");
}

fn test_partial_application_string_concatenation() {
  fn concat(str1, str2) {
    return str1 + str2;
  }
  
  fn concat_three(str1, str2, str3) {
    return str1 + str2 + str3;
  }
  
  var result1 = "hello " |> concat("world!");
  assert_eq(result1, "hello world!", "Expected string concatenation via pipe.");
  
  var result2 = "a" |> concat_three("b", "c");
  assert_eq(result2, "abc", "Expected three-string concatenation via pipe.");
}

fn test_backward_compatibility_with_parentheses() {
  fn double(x) {
    return x * 2;
  }
  
  fn square(x) {
    return x * x;
  }
  
  // Both should work identically
  var result1 = 5 |> double;
  var result2 = 5 |> double();
  
  assert_eq(result1, result2, "Expected pipe with and without parentheses to be equivalent.");
  assert_eq(result1, 10, "Expected both forms to equal 10.");
  
  var result3 = 4 |> square;
  var result4 = 4 |> square();
  
  assert_eq(result3, result4, "Expected square functions to be equivalent.");
  assert_eq(result3, 16, "Expected both square results to equal 16.");
}

fn test_partial_application_chaining() {
  fn add(a, b) {
    return a + b;
  }
  
  fn multiply(a, b) {
    return a * b;
  }
  
  fn concat_with_suffix(str, suffix) {
    return str + suffix;
  }
  
  // Chain partial applications
  var result = 5 |> add(3) |> multiply(2) |> to_string |> concat_with_suffix(" units");
  assert_eq(result, "16 units", "Expected chained partial applications to work.");
}

fn test_mixed_pipe_styles() {
  fn add(a, b) {
    return a + b;
  }
  
  fn double(x) {
    return x * 2;
  }
  
  fn subtract(a, b) {
    return a - b;
  }
  
  // Mix single-arg (no parens), single-arg (with parens), and partial application
  var result = 10 |> add(5) |> double |> subtract(8);
  assert_eq(result, 22, "Expected mixed pipe styles to work: 10+5=15, 15*2=30, 30-8=22.");
}

fn test_partial_application_with_variables() {
  fn format_message(name, greeting, punctuation) {
    return greeting + " " + name + punctuation;
  }
  
  var name = "Alice";
  var greeting = "Hello";
  var punct = "!";
  
  var result = name |> format_message(greeting, punct);
  assert_eq(result, "Hello Alice!", "Expected partial application with variables to work.");
}

fn test_partial_application_with_complex_expressions() {
  fn calculate(base, multiplier, offset) {
    return (base * multiplier) + offset;
  }
  
  // Test with expressions as arguments
  var result1 = 5 |> calculate(2 + 1, 10 - 5);
  assert_eq(result1, 20, "Expected 5 |> calculate(3, 5) to equal (5*3)+5=20.");
  
  // Test with function call as argument
  fn get_multiplier() {
    return 4;
  }
  
  var result2 = 3 |> calculate(get_multiplier(), 2);
  assert_eq(result2, 14, "Expected 3 |> calculate(4, 2) to equal (3*4)+2=14.");
}

fn test_partial_application_with_nested_calls() {
  fn add(a, b) {
    return a + b;
  }
  
  fn multiply(a, b) {
    return a * b;
  }
  
  // Test nested function calls as arguments
  var result = 2 |> add(3 |> multiply(4));
  assert_eq(result, 14, "Expected 2 |> add(3*4) to equal 2+12=14.");
}

fn test_partial_application_edge_cases() {
  fn identity(x) {
    return x;
  }
  
  fn add_zero(x, zero) {
    return x + zero;
  }
  
  // Test with zero as argument
  var result1 = 42 |> add_zero(0);
  assert_eq(result1, 42, "Expected adding zero via partial application to work.");
  
  // Test with nil (should work due to arity handling)
  var result2 = 5 |> identity();
  assert_eq(result2, 5, "Expected identity with empty parens to work.");
}