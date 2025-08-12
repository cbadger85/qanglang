var test_description = "Testing conditional statement and expressions.";

fn test_simple_if_statement() {
  var passed = false;
  if (true) {
    passed = true;
  }
  assert(passed, "Expected true to be true.");
}

fn test_if_statement_with_true_condition() {
  var passed = false;
  if (1 == 1) {
    passed = true;
  }
  assert(passed, "Expected 1 to equal 1 to be true.");
}

fn test_if_statement_with_false_condition() {
  var passed = true;
  if (1 != 1) {
    passed = false;
  }
  assert(passed, "Expected 1 does not equal one not to be true.");
}

fn test_if_else_statement() {
  var passed;
  if (true) {
    passed = true;
  } else {
    passed = false;
  }
  assert_eq(passed, !nil, "Expected true to be true.");
  assert(passed, "Expected true to be true.");
}

fn test_if_else_with_true_condition() {
  var passed;
  if (1 == 1) {
    passed = true;
  } else {
    passed = false;
  }
  assert_eq(passed, !nil, "Expected 1 equals 1 to be true.");
  assert(passed, "Expected 1 equals 1 to be true.");
}

fn test_if_else_with_false_condition() {
  var passed;
  if (1 != 1) {
    passed = false;
  } else {
    passed = true;
  }
  assert_eq(passed, !nil, "Expected 1 not equals 1 to be false.");
  assert(passed, "Expected 1 not equals 1 to be false.");
}

fn test_nested_if_statements() {
  var passed = false;
  if (true) {
    if (true) passed = true;
  }
  assert(passed, "Expected nested if statement to be reached.");
}

fn test_if_else_if_chain() {
  var value = 0;

  if (false) {
    value = value + 1;
  } else if (false) {
    value = value + 10;
  } else if (false) value = value + 100; else if (true) value = value + 1000;

  assert_eq(value, 1000, "Expected only the last else if to be called.");
}

fn test_if_with_expression_bodies() {
  if (true) assert(true, "Expected true to be true.");
}
