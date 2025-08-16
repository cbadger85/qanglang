var test_description = "Testing the standard library included with QangLang.";

fn test_to_string() {
  var foo = 12 |> to_string;
  assert_eq(typeof(foo), "string");
  assert_eq((12 |> to_string) + " is a number", "12 is a number");
}