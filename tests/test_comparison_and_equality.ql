var test_description = "Testing the comparison and equality operators and their precedence.";

fn test_equality_with_numbers() {
  assert(0 == 0, "Expected 0 to equal 0.");
  assert(0 != 1, "Expected 0 to not to equal 1.");
}

fn test_equality_with_strings() {
  assert("test" == "test", "Expected 'test' to equal 'test'.");
  assert("tes" + "t" == "test", "Expected 'test' to equal 'test'.");
}

fn test_equality_with_booleans() {
  assert(true == true, "Expected true to equal true.");
  assert(false == false, "Expected false to equal false.");
  assert(true != false, "Expected true not to equal false.");
}

fn test_equality_with_nil() {
  assert(nil == nil, "Expected nil to equal nil.");
}


fn test_inequality_operator() {
  assert(true != false, "Expected true not to equal false.");
}

fn test_equality_with_mixed_types() {
  assert(true != "true", "Expected true not to equal 'true'.");
  assert(nil != 0, "Expected nil not to equal 0.");
}

fn test_chained_equality() {
  assert(1 == 1 == true, "Expected 1 == 1 == true to be true.");
}

fn test_greater_than_comparison() {
  assert(1 > 0, "Expected 1 to be greater than 0.");
}

fn test_greater_than_or_equal_comparison() {
  assert(1 >= 0, "Expected 1 to be greater than or equal to 0.");
  assert(1 >= 1, "Expected 1 to be greater than or equal to 1");
}

fn test_less_than_comparison() {
  assert(0 < 1, "Expected 0 to be less than 1.");
}

fn test_less_than_or_equal_comparison() {
  assert(0 <= 1, "Expected 0 to be less than or equal to 1.");
  assert(1 <= 1, "Expected 1 to be less than or equal to 1.");
}

fn test_comparison_operator_precedence() {
  assert(1 + 0 > 0 * 1, "Expected 1 + 0 to be greater than 0 * 1.");
  assert(1 - 1 >= 0 / 1, "Expected 1 - 1 to be greater than or equal to 0 / 1.");
  assert(2 * -1 < 12 * 0.9, "Expected 2 * -1 to be less than 12 * 0.9.");
  assert(20 + -20 <= 0 / 1, "Expected 20 - 20 to be less than or equal to 0 / 1.");
}

fn test_chained_comparison() {
  assert(1 < 2 == true, "Expected 1 < 2 == true to be true.");
}
