var test_description = "Testing function and lambda calls and their precedence.";

fn test_function_call_without_arguments() {
  fn simple_function() {
    return "called";
  }
  
  var result = simple_function();
  assert_eq(result, "called", "Expected function call without arguments to work.");
}

fn test_function_call_with_single_argument() {
  fn double(x) {
    return x * 2;
  }
  
  var result = double(5);
  assert_eq(result, 10, "Expected function call with single argument to work.");
}

fn test_function_call_with_multiple_arguments() {
  fn add_three(a, b, c) {
    return a + b + c;
  }
  
  var result = add_three(1, 2, 3);
  assert_eq(result, 6, "Expected function call with multiple arguments to work.");
}

fn test_function_call_with_trailing_comma() {
  fn add(x, y) {
    return x + y;
  }
  
  var result = add(3, 4,);
  assert_eq(result, 7, "Expected function call with trailing comma to work.");
}

fn test_function_call_with_expressions_as_arguments() {
  fn multiply(x, y) {
    return x * y;
  }
  
  var result = multiply(2 + 3, 4 - 1);
  assert_eq(result, 15, "Expected function call with expressions as arguments to work.");
}

fn test_nested_function_calls() {
  fn add_one(x) {
    return x + 1;
  }
  
  fn double(x) {
    return x * 2;
  }
  
  var result = double(add_one(5));
  assert_eq(result, 12, "Expected nested function calls to work correctly.");
}

fn test_function_call_precedence() {
  fn get_value() {
    return 10;
  }
  
  var result = get_value() + 5 * 2;
  assert_eq(result, 20, "Expected function call to have higher precedence than arithmetic.");
}

fn test_lambda_function_calls() {
  var square = (x) -> x * x;
  
  var result = square(4);
  assert_eq(result, 16, "Expected lambda function calls to work correctly.");
}

// fn test_method_calls_on_objects() {}

// fn test_chained_method_calls() {}

fn test_function_call_with_return_value() {
  fn get_message() {
    return "hello world";
  }
  
  var message = get_message();
  assert_eq(message, "hello world", "Expected function call to return correct value.");
}

fn test_function_call_side_effects() {
  var counter = 0;
  
  fn increment() {
    counter = counter + 1;
    return counter;
  }
  
  increment();
  increment();
  var result = increment();
  
  assert_eq(result, 3, "Expected function calls to have side effects.");
  assert_eq(counter, 3, "Expected counter to be modified by function calls.");
}

fn test_recursive_function_calls() {
  var countdown;
  countdown = (n) -> {
    if (n <= 0) {
      return 0;
    }
    return n + countdown(n - 1);
  };
  
  var result = countdown(3);
  assert_eq(result, 6, "Expected recursive function calls to work correctly.");
}

fn test_function_call_argument_evaluation_order() {
  var order = "";
  
  fn record(name) {
    order = order + name;
    return name;
  }
  
  fn combine(a, b, c) {
    return a + b + c;
  }
  
  var result = combine(record("A"), record("B"), record("C"));
  assert_eq(order, "ABC", "Expected arguments to be evaluated left to right.");
  assert_eq(result, "ABC", "Expected function to receive evaluated arguments.");
}

fn test_higher_order_function_calls() {
  fn apply_twice(func, value) {
    return func(func(value));
  }
  
  var add_one = (x) -> x + 1;
  
  var result = apply_twice(add_one, 5);
  assert_eq(result, 7, "Expected higher order function calls to work correctly.");
}

fn test_calling_function_with_apply_intrinsic() {
  fn sum(a, b) {
    return a + b;
  }
  
  assert_eq(sum.apply([1, 2]), 3);
}

fn test_calling_function_with_call_intrinsic() {
  fn sum(a, b) {
    return a + b;
  }
  
  assert_eq(sum.call(1, 2), 3);
}