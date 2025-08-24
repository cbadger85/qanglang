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