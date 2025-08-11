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
  assert_eq(-2 / 4, -0.5, "Expect -2 / 4 to equal 0.5");
  assert_eq(-2 / -4, 0.5, "Expect -2 / -4 to equal 0.5.");
}

fn test_modulo() {
  assert_eq(5 % 2, 1, "Expect 5 % 2 to equal 1.");
  assert_eq(5 % -2, 1, "Expect 5 % -2 to equal 1.");
  assert_eq(-5 % 2, -1, "Expect -5 % 2 to equal -1");
  assert_eq(-5 % -2, -1, "Expect -5 % -2 to equal -1.");
}

fn test_operator_precedence() {
  // Multiplication before addition
  assert_eq(2 + 3 * 4, 14, "Expected 2 + 3 * 4 to equal 14 (not 20).");
  assert_eq(3 * 4 + 2, 14, "Expected 3 * 4 + 2 to equal 14.");
  
  // Division before subtraction
  assert_eq(10 - 8 / 2, 6, "Expected 10 - 8 / 2 to equal 6 (not 1).");
  assert_eq(8 / 2 - 1, 3, "Expected 8 / 2 - 1 to equal 3.");
  
  // Modulo before addition
  assert_eq(1 + 5 % 3, 3, "Expected 1 + 5 % 3 to equal 3 (not 0).");
  
  // Mixed multiplication and division (left-to-right)
  assert_eq(12 / 3 * 2, 8, "Expected 12 / 3 * 2 to equal 8.");
  assert_eq(12 * 2 / 3, 8, "Expected 12 * 2 / 3 to equal 8.");
  
  // Mixed addition and subtraction (left-to-right)
  assert_eq(10 - 3 + 2, 9, "Expected 10 - 3 + 2 to equal 9.");
  assert_eq(10 + 2 - 3, 9, "Expected 10 + 2 - 3 to equal 9.");
}

fn test_parentheses_grouping() {
  // Parentheses override precedence
  assert_eq((2 + 3) * 4, 20, "Expected (2 + 3) * 4 to equal 20.");
  assert_eq(2 * (3 + 4), 14, "Expected 2 * (3 + 4) to equal 14.");
  assert_eq((10 - 6) / 2, 2, "Expected (10 - 6) / 2 to equal 2.");
  assert_eq(10 / (5 - 3), 5, "Expected 10 / (5 - 3) to equal 5.");
  
  // Nested parentheses
  assert_eq(((2 + 3) * 4) - 1, 19, "Expected ((2 + 3) * 4) - 1 to equal 19.");
  assert_eq(2 + (3 * (4 - 1)), 11, "Expected 2 + (3 * (4 - 1)) to equal 11.");
  assert_eq((2 + 3) * (4 - 1), 15, "Expected (2 + 3) * (4 - 1) to equal 15.");
  
  // Parentheses with negative numbers
  assert_eq((-2 + 3) * 4, 4, "Expected (-2 + 3) * 4 to equal 4.");
  assert_eq(2 * (-3 + 5), 4, "Expected 2 * (-3 + 5) to equal 4.");
}

fn test_complex_nested_expressions() {
  // Complex expressions mixing all operators
  assert_eq(2 + 3 * 4 - 5 / 2, 11.5, "Expected 2 + 3 * 4 - 5 / 2 to equal 11.5.");
  assert_eq((2 + 3) * (4 - 5) / 2, -2.5, "Expected (2 + 3) * (4 - 5) / 2 to equal -2.5.");
  assert_eq(10 % 3 + 2 * 4 - 1, 8, "Expected 10 % 3 + 2 * 4 - 1 to equal 8.");
  
  // Long chains with mixed operations
  assert_eq(1 + 2 * 3 - 4 / 2 + 5 % 3, 7, "Expected 1 + 2 * 3 - 4 / 2 + 5 % 3 to equal 7.");
  assert_eq((1 + 2) * (3 - 4) / (2 + 5) % 3, -0.42857142857142855, "Expected complex expression result.");
  
  // Deeply nested parentheses
  assert_eq(((1 + 2) * (3 + 4)) / ((5 - 2) + 1), 5.25, "Expected ((1 + 2) * (3 + 4)) / ((5 - 2) + 1) to equal 5.25.");
  assert_eq(2 * (3 + (4 * (5 - 2))), 30, "Expected 2 * (3 + (4 * (5 - 2))) to equal 30.");
}

fn test_associativity() {
  // Left-to-right associativity for same precedence operators
  
  // Addition and subtraction (left-to-right)
  assert_eq(10 - 5 + 2, 7, "Expected 10 - 5 + 2 to equal 7 (not 3).");
  assert_eq(1 + 2 - 3 + 4, 4, "Expected 1 + 2 - 3 + 4 to equal 4.");
  assert_eq(20 - 5 - 3, 12, "Expected 20 - 5 - 3 to equal 12 (not 18).");
  
  // Multiplication and division (left-to-right)
  assert_eq(24 / 4 * 2, 12, "Expected 24 / 4 * 2 to equal 12 (not 3).");
  assert_eq(2 * 3 / 6 * 4, 4, "Expected 2 * 3 / 6 * 4 to equal 4.");
  assert_eq(100 / 5 / 2, 10, "Expected 100 / 5 / 2 to equal 10 (not 40).");
  
  // Modulo with multiplication/division
  assert_eq(15 % 7 * 2, 2, "Expected 15 % 7 * 2 to equal 2.");
  assert_eq(20 / 3 % 5, 1.666666666666667, "Expected 20 / 3 % 5 to equal 1.666666666666667.");
}

fn test_mixed_positive_negative_expressions() {
  // Complex expressions with mixed positive/negative numbers
  assert_eq(-2 + 3 * -4 - 5, -19, "Expected -2 + 3 * -4 - 5 to equal -19.");
  assert_eq((-2 + 3) * (-4 - 5), -9, "Expected (-2 + 3) * (-4 - 5) to equal -9.");
  assert_eq(-10 / -2 + -3 * 2, -1, "Expected -10 / -2 + -3 * 2 to equal -1.");
  
  // Alternating signs
  assert_eq(-1 + -2 - -3 * -4, -15, "Expected -1 + -2 - -3 * -4 to equal -15.");
  assert_eq(5 + -3 * 2 / -1, 11, "Expected 5 + -3 * 2 / -1 to equal 11.");
  
  // Negative results with positive operations
  assert_eq(3 - 5 * 2, -7, "Expected 3 - 5 * 2 to equal -7.");
  assert_eq(2 * 3 - 10, -4, "Expected 2 * 3 - 10 to equal -4.");
  
  // Double negatives
  assert_eq(-(-5), 5, "Expected -(-5) to equal 5.");
  assert_eq(-(-2 * -3), -6, "Expected -(-2 * -3) to equal -6.");
}

fn test_floating_point_precision() {
  // Basic decimal operations
  assert_eq(0.1 + 0.2, 0.30000000000000004, "Expected 0.1 + 0.2 to equal 0.30000000000000004.");
  assert_eq(1.5 * 2.5, 3.75, "Expected 1.5 * 2.5 to equal 3.75.");
  assert_eq(7.5 / 2.5, 3, "Expected 7.5 / 2.5 to equal 3.");
  
  // Mixed integer and decimal
  assert_eq(5 + 0.5, 5.5, "Expected 5 + 0.5 to equal 5.5.");
  assert_eq(10 / 4.0, 2.5, "Expected 10 / 4.0 to equal 2.5.");
  assert_eq(3.14 * 2, 6.28, "Expected 3.14 * 2 to equal 6.28.");
  
  // Very small numbers
  assert_eq(0.001 + 0.002, 0.003, "Expected 0.001 + 0.002 to equal 0.003.");
  assert_eq(0.1 * 0.1, 0.010000000000000002, "Expected 0.1 * 0.1 to equal 0.010000000000000002.");
  
  // Decimal modulo
  assert_eq(5.5 % 2.0, 1.5, "Expected 5.5 % 2.0 to equal 1.5.");
  assert_eq(7.25 % 2.5, 2.25, "Expected 7.25 % 2.5 to equal 2.25.");
}

fn test_unary_operators() {
  // Unary minus with expressions
  assert_eq(-(3 + 2), -5, "Expected -(3 + 2) to equal -5.");
  assert_eq(-(3 * 2), -6, "Expected -(3 * 2) to equal -6.");
  assert_eq(-(10 / 2), -5, "Expected -(10 / 2) to equal -5.");
  
  // Multiple unary operators
  assert_eq(--5, 5, "Expected --5 to equal 5.");
  assert_eq(---5, -5, "Expected ---5 to equal -5.");
  assert_eq(----5, 5, "Expected ----5 to equal 5.");
  
  // Unary with complex expressions
  assert_eq(-(2 + 3 * 4), -14, "Expected -(2 + 3 * 4) to equal -14.");
  assert_eq(-((2 + 3) * 4), -20, "Expected -((2 + 3) * 4) to equal -20.");
  assert_eq(-(5 - 3 * 2), 1, "Expected -(5 - 3 * 2) to equal 1.");
  
  // Unary in larger expressions
  assert_eq(5 + -(3 * 2), -1, "Expected 5 + -(3 * 2) to equal -1.");
  assert_eq(-(2 * 3) + 10, 4, "Expected -(2 * 3) + 10 to equal 4.");
  assert_eq(2 * -(5 - 3), -4, "Expected 2 * -(5 - 3) to equal -4.");
}

fn test_edge_cases() {
  // Large numbers
  assert_eq(1000000 + 2000000, 3000000, "Expected 1000000 + 2000000 to equal 3000000.");
  assert_eq(9999999 * 2, 19999998, "Expected 9999999 * 2 to equal 19999998.");
  assert_eq(1000000 / 1000, 1000, "Expected 1000000 / 1000 to equal 1000.");
  
  // Very small decimal numbers
  assert_eq(0.0001 + 0.0002, 0.00030000000000000003, "Expected 0.0001 + 0.0002 to equal 0.00030000000000000003.");
  assert_eq(0.001 * 0.001, 0.000001, "Expected 0.001 * 0.001 to equal 0.000001.");
  
  // Zero operations
  assert_eq(0 + 5, 5, "Expected 0 + 5 to equal 5.");
  assert_eq(5 + 0, 5, "Expected 5 + 0 to equal 5.");
  assert_eq(0 * 1000, 0, "Expected 0 * 1000 to equal 0.");
  assert_eq(1000 * 0, 0, "Expected 1000 * 0 to equal 0.");
  assert_eq(0 - 5, -5, "Expected 0 - 5 to equal -5.");
  assert_eq(5 - 0, 5, "Expected 5 - 0 to equal 5.");
  // assert_throws(() -> nil, "Unexpected division by zero."); // TODO test division by zero.
   
  // Operations that result in zero
  assert_eq(5 - 5, 0, "Expected 5 - 5 to equal 0.");
  assert_eq(7 * 8 - 56, 0, "Expected 7 * 8 - 56 to equal 0.");
  
  // Identity operations  
  assert_eq(42 * 1, 42, "Expected 42 * 1 to equal 42.");
  assert_eq(1 * 42, 42, "Expected 1 * 42 to equal 42.");
  assert_eq(42 / 1, 42, "Expected 42 / 1 to equal 42.");
}
