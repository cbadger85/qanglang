var test_description = "Testing math expressions and their precedence.";

fn test_addition() {
  assert_eq(1 + 1, 2, "Expected 1 + 1 to equal 2.");
  assert_eq(1 + -1, 0, "Expected 1 + -1 to equal 0.");
  assert_eq(-1 + 1, 0, "Expected -1 + 1 to equal 0.");
}

fn test_subtraction() {
  assert_eq(1 - 1, 0, "Expected 1 - 1 to equal 0.");
  assert_eq(1 - -1, 2, "Expected 1 - -1 to equal 2.");
  assert_eq(-1 - 1, -2, "Expected -1 - 1 to equal -2.");
}

fn test_multiplication() {
  assert_eq(2 * 3, 6, "Expect 2 * 3 to equal 6.");
  assert_eq(2 * -3, -6, "Expect 2 * -3 to equal -6.");
  assert_eq(-2 * 3, -6, "Expect -2 * 3 to equal -6.");
  assert_eq(-2 * -3, 6, "Expect -2 * -3 to equal 6.");
}

fn test_division() {
  assert_eq(2 / 4, 0.5, "Expect 2 / 4 to equal 0.5.");
  assert_eq(2 / -4, -0.5, "Expect 2 / -4 to equal 0.5.");
  assert_eq(-4 / 2, -0.5, "Expect -2 / 4 to equal 0.5");
  assert_eq(-4 / -2, 0.5, "Expect -2 / -4 to equal 0.5.");
}

