var test_description = "Testing lambda expressions and declarations.";

fn test_lambda_declaration_without_parameters() {
  var test_lambda = () -> 1;

  assert_eq(test_lambda(), 1);
}

fn test_lambda_declaration_with_single_parameter() {
  var identity = (x) -> x;

  assert_eq(identity(1), 1);
}

fn test_lambda_declaration_with_multiple_parameters() {
  var sum = (x, y) -> x + y;

  assert_eq(sum(1, 2), 3);
}

fn test_lambda_with_expression_body() {
  var one = () -> 1 or 2;
  
  assert_eq(one(), 1);
}

fn test_lambda_with_block_body() {
  var run_count = 0;
  var test_lambda = () -> {
    assert(true);
    run_count = run_count + 1;
  };

  test_lambda();
  assert_eq(run_count, 1);
}

fn test_lambda_as_expression() {
  var test_value = true ? () -> 1 : () -> 2;

  assert_eq(test_value(), 1);
}

fn test_lambda_as_function_argument() {
    fn identity(x) {
      return x;
  }

  var y = identity(() -> "hello world");
  assert_eq(y(), "hello world");
}

fn test_lambda_as_return_value() {
  fn high_order() {
    return () -> "inner";
  }

  assert_eq(high_order()(), "inner");
}

fn test_lambda_closure_capture() {
    fn high_order() {
      var foo = "inner";
    return () -> foo;
  }

  assert_eq(high_order()(), "inner");
}

// fn test_lambda_nested_scopes() {}

// fn test_lambda_parameter_shadowing() {}

fn test_immediately_invoked_lambda() {
  assert_eq((() -> nil)(), nil);
}