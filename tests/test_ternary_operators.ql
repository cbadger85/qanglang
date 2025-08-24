var test_description = "Testing ternary conditional operators.";

fn test_simple_ternary_operator() {
  var passed = true ? true : false;

  assert(passed, "Expected ternery to return true.");
}

fn test_ternary_with_boolean_conditions() {
  var passed = true and true ? true : false;

  assert(passed, "Expected ternery to return true.");
}

fn test_ternary_with_truthy_conditions() {
  var passed = "pass" ? true : false;

  assert(passed, "Expected ternery to return true.");
}

fn test_nested_ternary_operators() {
  var passed_1 = false 
                  ? 1
                  : false
                  ? 2
                  : 3;

  assert_eq(passed_1, 3, "Expected passed_1 to be 3.");

  var passed_2 = true 
                  ? false ? 1 : 2
                  : true ? 3 : 4;

  assert_eq(passed_2, 2, "Expected passed_2 to be 2.");
}

fn test_ternary_precedence() {
  var passed = 1 * 2 + 3 == 5 ? "test" + " " + "passed" + "." : false;
  assert_eq(passed, "test passed.", "Expected passed to be 'test passed'.");
}

fn test_ternary_with_function_calls() {
  fn true_fn() {
    return true;
  }

  fn result_1() {
    return true;
  }

  fn result_2() {
    return false;
  }

  var passed = true_fn() ? result_1() : result_2();

  assert(passed, "Expected value to be true.");
}