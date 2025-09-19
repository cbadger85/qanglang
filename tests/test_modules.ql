var test_description = "Testing module import and export functionality.";

fn test_simple_module_functionality() {
  mod simple = import("./module_helper_simple.ql");
  // Test basic import and function calls
  var result = simple.sum(5, 3);
  assert_eq(result, 8, "Expected simple.sum(5, 3) to return 8.");
  // Test exported variables and functions
  assert_eq(simple.exported_value, 42, "Expected exported_value to be 42.");
  assert_eq(simple.get_value(), 42, "Expected get_value() to return 42.");
  assert_eq(simple.double_value(5), 10, "Expected double_value(5) to return 10.");
}

fn test_complex_module_state_and_functions() {
  mod complex = import("./module_helper_complex.ql");
  // Test module state and increment functionality
  var current_value = complex.value;
  var incremented = complex.increment();
  assert_eq(incremented, current_value + 1, "Expected increment to return current + 1.");
  assert_eq(complex.value, current_value + 1, "Expected value to be updated.");
  // Test create_array functionality
  var arr = complex.create_array(3, (x) -> x * 2);
  assert_eq(arr.length(), 3, "Expected array length to be 3.");
  assert_eq(arr[0], 0, "Expected arr[0] to be 0.");
  assert_eq(arr[1], 2, "Expected arr[1] to be 2.");
  assert_eq(arr[2], 4, "Expected arr[2] to be 4.");
  // Test counter functionality
  assert_eq(complex.counter, 0, "Expected initial counter to be 0.");
  var first_increment = complex.increment_counter();
  assert_eq(first_increment, 1, "Expected first increment to return 1.");
  assert_eq(complex.counter, 1, "Expected counter to be updated to 1.");
  complex.increment_counter();
  complex.increment_counter();
  assert_eq(complex.counter, 3, "Expected counter to be 3 after three increments.");
  var reset_result = complex.reset_counter();
  assert_eq(reset_result, 0, "Expected reset to return 0.");
  assert_eq(complex.counter, 0, "Expected counter to be reset to 0.");
}

fn test_module_state_persistence() {
  mod complex1 = import("./module_helper_complex.ql");
  mod complex2 = import("./module_helper_complex.ql");
  var initial_value = complex1.value;
  complex1.increment();
  assert_eq(complex2.value, initial_value + 1, "Expected both module references to share state.");
  complex2.increment();
  assert_eq(complex1.value, initial_value + 2, "Expected state changes to be visible across references.");
}

fn test_multiple_module_imports() {
  mod simple = import("./module_helper_simple.ql");
  mod complex = import("./module_helper_complex.ql");
  var simple_result = simple.sum(10, 20);
  var complex_result = complex.increment();
  assert_eq(simple_result, 30, "Expected simple module to work correctly.");
  assert(simple_result != complex_result, "Expected different modules to be independent.");
}

fn test_module_function_usage() {
  mod simple = import("./module_helper_simple.ql");
  // Test functions as first-class values
  var sum_fn = simple.sum;
  var result = sum_fn(7, 8);
  assert_eq(result, 15, "Expected stored module function to work correctly.");
  // Test functions in expressions
  var expr_result = simple.sum(1, 2) + simple.sum(3, 4);
  assert_eq(expr_result, 10, "Expected module functions to work in expressions: (1+2) + (3+4) = 10.");
  // Test nested function calls
  fn calculate_total(a, b, c, d) {
    return simple.sum(simple.sum(a, b), simple.sum(c, d));
  }
  var nested_result = calculate_total(1, 2, 3, 4);
  assert_eq(nested_result, 10, "Expected nested module calls to work: sum(sum(1,2), sum(3,4)) = 10.");
}

fn test_module_with_closures() {
  mod complex = import("./module_helper_complex.ql");
  var multiplier = 3;
  var arr = complex.create_array(4, (x) -> x * multiplier);
  assert_eq(arr[0], 0, "Expected arr[0] to be 0.");
  assert_eq(arr[1], 3, "Expected arr[1] to be 3.");
  assert_eq(arr[2], 6, "Expected arr[2] to be 6.");
  assert_eq(arr[3], 9, "Expected arr[3] to be 9.");
}

fn test_nested_modules() {
  mod simple = import("./module_helper_simple.ql");

  assert_eq(simple.hello_world.hello(), "Hello world!");
}