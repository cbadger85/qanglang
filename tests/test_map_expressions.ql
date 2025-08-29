var test_description = "Testing map expressions";

fn test_map_expression_with_value() {
  var value = false;

  assert(value||v -> !v|);

  var number = 0;

  var number_plus_one = number||n -> n + 1|;

  assert_eq(number_plus_one, 1);
}