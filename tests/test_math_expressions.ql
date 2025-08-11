var test_description = "Testing math expressions and their precedence.";

fn test_addition() {
  assert_eq(1 + 1, 2, "Expected 1 + 1 to equal 2.");
}

fn test_addition_of_negative() {
  assert_eq(1 + -1, 0, "Expected 1 + -1 to equal 0.");
}

fn test_subtraction() {
  assert_eq(1 - 1, 0, "Expected 1 - 1 to equal 0.");
}

fn test_subtraction_of_negative() {
  assert_eq(1 - -1, 2, "Expected 1 - -1 to equal 2.");
}



