var test_description = "Testing module import and export functionality.";

fn test_basic_module_import() {
  mod math = import("../examples/math.ql");
  var result = math.sum(5, 3);
  assert_eq(result, 8, "Expected math.sum(5, 3) to return 8.");
}

fn test_module_with_variables_and_functions() {
  mod other = import("../examples/other_module.ql");
  // Note: Module state persists, so we check current value, not initial
  var current_value = other.value;
  var incremented = other.increment();
  assert_eq(incremented, current_value + 1, "Expected increment to return current + 1.");
  assert_eq(other.value, current_value + 1, "Expected value to be updated.");
  var arr = other.create_array(3, (x) -> x * 2);
  assert_eq(arr.length(), 3, "Expected array length to be 3.");
  assert_eq(arr[0], 0, "Expected arr[0] to be 0.");
  assert_eq(arr[1], 2, "Expected arr[1] to be 2.");
  assert_eq(arr[2], 4, "Expected arr[2] to be 4.");
}

fn test_simple_helper_module() {
  mod simple = import("./module_helper_simple.ql");
  assert_eq(simple.exported_value, 42, "Expected exported_value to be 42.");
  assert_eq(simple.get_value(), 42, "Expected get_value() to return 42.");
  assert_eq(simple.double_value(5), 10, "Expected double_value(5) to return 10.");
}

fn test_module_state_persistence() {
  mod other1 = import("../examples/other_module.ql");
  mod other2 = import("../examples/other_module.ql");
  var initial_value = other1.value;
  other1.increment();
  assert_eq(other2.value, initial_value + 1, "Expected both module references to share state.");
  other2.increment();
  assert_eq(other1.value, initial_value + 2, "Expected state changes to be visible across references.");
}

fn test_multiple_module_imports() {
  mod math = import("../examples/math.ql");
  mod other = import("../examples/other_module.ql");
  var math_result = math.sum(10, 20);
  var other_result = other.increment();
  assert_eq(math_result, 30, "Expected math module to work correctly.");
  assert(math_result != other_result, "Expected different modules to be independent.");
}

fn test_module_function_as_first_class_value() {
  mod math = import("../examples/math.ql");
  var sum_fn = math.sum;
  var result = sum_fn(7, 8);
  assert_eq(result, 15, "Expected stored module function to work correctly.");
}

fn test_module_in_expressions() {
  mod math = import("../examples/math.ql");
  var result = math.sum(1, 2) + math.sum(3, 4);
  assert_eq(result, 10, "Expected module functions to work in expressions: (1+2) + (3+4) = 10.");
}

fn test_nested_module_calls() {
  mod math = import("../examples/math.ql");
  fn calculate_total(a, b, c, d) {
    return math.sum(math.sum(a, b), math.sum(c, d));
  }
  var result = calculate_total(1, 2, 3, 4);
  assert_eq(result, 10, "Expected nested module calls to work: sum(sum(1,2), sum(3,4)) = 10.");
}

fn test_module_with_closures() {
  mod other = import("../examples/other_module.ql");
  var multiplier = 3;
  var arr = other.create_array(4, (x) -> x * multiplier);
  assert_eq(arr[0], 0, "Expected arr[0] to be 0.");
  assert_eq(arr[1], 3, "Expected arr[1] to be 3.");
  assert_eq(arr[2], 6, "Expected arr[2] to be 6.");
  assert_eq(arr[3], 9, "Expected arr[3] to be 9.");
}

fn test_complex_helper_module_state() {
  mod complex = import("./module_helper_complex.ql");
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