var test_description = "Testing the standard library included with QangLang.";

fn test_to_string() {
  var foo = 12 |> to_string;
  assert_eq(typeof(foo), "string");
  assert_eq((12 |> to_string) + " is a number", "12 is a number");
}

fn test_to_uppercase() {
  var foo = "not so loud.".to_uppercase();
  assert_eq(foo, "NOT SO LOUD.", "Expected foo to be 'NOT SO LOUD.' not " + foo |> to_string);
}

fn test_to_lowercase() {
  var foo = "SHHHH.".to_lowercase();
  assert_eq(foo, "shhhh.", "Expected foo to be 'shhhh.' not " + foo |> to_string);
}

fn test_to_number_ok() {
  var num_as_string = "1";

  var num_result = num_as_string.to_number();
  assert(num_result is Result);
  assert(num_result.is_ok());
  assert_eq(num_result.unwrap(), 1);
}

fn test_to_number_err() {
  var num_as_string = "@";

  var num_result = num_as_string.to_number();
  assert(num_result is Result);
  assert(num_result.is_err());
  assert_throws(() -> num_result.unwrap());
}