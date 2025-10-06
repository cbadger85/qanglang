var test_description = "Testing number intrinsic methods.";

fn test_number_ceil() {
  var num1 = 0.1;
  var num2 = -1.1;

  assert_eq(num1.ceil(), 1);
  assert_eq(num2.ceil(), -1);
}

fn test_number_floor() {
  var num1 = 1.1;
  var num2 = -1.1;

  assert_eq(num1.floor(), 1);
  assert_eq(num2.floor(), -2);
}

fn test_number_trunc() {
  var num1 = 1.5;
  var num2 = -1.5;

  assert_eq(num1.trunc(), 1);
  assert_eq(num2.trunc(), -1);
}

fn test_number_min() {
  var num1 = 1;
  var num2 = 2;

  assert_eq(num1.min(num2), 1);
}

fn test_number_max() {
  var num1 = 1;
  var num2 = 2;

  assert_eq(num1.max(num2), 2);
}
