var test_description = "Testing variable declarations and initialization.";

fn test_variable_declaration_without_initialization() {
  var value;
  assert_eq(value, nil, "Expected value to be nil.");
}

fn test_variable_declaration_with_initialization() {
  var passed = true;
  assert(passed, "Expected value to be true.");
}

fn test_variable_declaration_with_number() {
  var passed = 1;
  assert_eq(passed, 1, "Expected value to be 1.");
}

fn test_variable_declaration_with_string() {
  var passed = "yes";
  assert_eq(passed, "yes", "Expected value to be 'yet'.");
}

fn test_variable_declaration_with_boolean() {
  var passed = true;
  assert_eq(passed, true, "Expected value to be true.");
}

fn test_variable_declaration_with_nil() {
  var passed = nil;
  assert_eq(passed, nil, "Expected value to be nil.");
}