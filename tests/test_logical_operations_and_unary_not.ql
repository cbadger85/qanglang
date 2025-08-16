var test_description = "Testing the logical and unary not operators";

fn test_logical_and_with_true_operands() {
  assert_eq(true and true, true, "Expected true and true to be true.");
}

fn test_logical_and_with_false_operands() {
  assert_eq(false and false, false, "Expected false and false to be false.");
}

fn test_logical_or_with_true_operands() {
  assert(true or true, "Expected true or true to be true.");
}

fn test_logical_or_with_false_operands() {
  assert_eq(false or false, false, "Expected false or false to be false.");
}

fn test_logical_and_short_circuit_evaluation() {
  var passed = true;
  
  fn failure() {
    passed = false;
  }

  assert(!(false and failure()));
  assert(passed, "Expected failure not to be called.");
}

fn test_logical_or_short_circuit_evaluation() {
  var passed = true;
  
  fn failure() {
    passed = false;
  }

  assert(true or failure());
  assert(passed, "Expected failure not to be called.");
}


fn test_unary_not_with_boolean_values() {
  assert_eq(!false, true, "Expected !false to be true.");
  assert_eq(!true, false, "Expected !true to be false.");
}

fn test_unary_not_with_truthy_values() {
  assert_eq(!"true", false, "Expected !'true' to be false.");
  assert_eq(!0, false, "Expected !0 to be false.");
}

fn test_unary_not_with_falsy_values() {
  assert(!false, "Expected !false to be true.");
}

fn test_nested_logical_operations() {
  assert(true or false and true, "Expected true or false and true to be true.");
}
