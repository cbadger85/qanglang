var test_description = "Testing string functions and intrinisic methods";

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

fn test_array_from_split_string() {
  var string = "123";

  var arr = string.split();

  assert(arr is ARRAY);
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], "1");
  assert_eq(arr[1], "2");
  assert_eq(arr[2], "3");
}

fn test_array_from_split_string_with_delimeter() {
  var string = "1,2,3";

  var arr = string.split(",");

  assert(arr is ARRAY);
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], "1");
  assert_eq(arr[1], "2");
  assert_eq(arr[2], "3");
}

fn test_array_from_split_string_with_empty_string_delimeter() {
  var string = "123";

  var arr = string.split("");

  assert(arr is ARRAY);
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], "1");
  assert_eq(arr[1], "2");
  assert_eq(arr[2], "3");
}

fn test_concat_strings() {
  var str1 = "foo";
  var str2 = "bar";

  assert_eq(str1 + str2, "foobar");
  assert_eq(str1.concat(str2), "foobar");
}

fn test_string_contants() {
  var phrase = "the quick brown fox jumped over the lazy hound.";

  assert(phrase.contains("the"));
  assert(phrase.contains("quick brown"));
  assert(phrase.contains("lazy hound."));
  assert(phrase.contains("the quick brown fox jumped over the lazy hound."));
}

fn test_string_starts_with() { 
  var phrase = "the quick brown fox jumped over the lazy hound.";

  assert(phrase.starts_with("the"));
  assert(phrase.starts_with("the quick brown fox jumped over the lazy hound."));
}

fn test_string_ends_with() { 
  var phrase = "the quick brown fox jumped over the lazy hound.";

  assert(phrase.ends_with("."));
  assert(phrase.ends_with("the quick brown fox jumped over the lazy hound.")); 
}

fn test_array_length() { 
  var string = "this string is 28 chars long";

  assert_eq(string.length(), 28);
}

fn test_char_at() {
  var string = "the string";

  assert_eq(string.char_at(2), "e");
}

fn test_char_at_negative_index() {
  var string = "the string";

  assert_eq(string.char_at(-1), nil);
}

fn test_char_at_out_of_bounds_index() {
  var string = "the string";

  assert_eq(string.char_at(100), nil);
}

fn test_char_at_irrational_index() {
  var string = "the string";

  assert_eq(string.char_at(2.5), "e");
}

fn test_empty_string_builder() {
  var string = StringBuilder().to_string();
  assert_eq(string, "");
}

fn test_string_builder_with_initial_value() {
  assert_eq(StringBuilder("abc").to_string(), "abc");
}

fn test_string_builder_append() {
  assert_eq(StringBuilder("abc").append("def").to_string(), "abcdef");
}

fn test_string_builder_prepend() {
  assert_eq(StringBuilder("def").prepend("abc").to_string(), "abcdef");
}

fn test_string_builder_combine() {
  assert_eq(StringBuilder("abc").combine(StringBuilder("def")).to_string(), "abcdef");
}

fn test_pad_start() {
  assert_eq("foo".pad_start(5, " "), "  foo");
}

fn test_pad_end() {
  assert_eq("foo".pad_end(5, " "), "foo  ");
}

fn test_trim_start() {
  assert_eq("  foo".trim_start(), "foo");
}

fn test_trim_end() {
  assert_eq("foo  ".trim_end(), "foo");
}

fn test_trim() {
  assert_eq("  foo  ".trim(), "foo");
}