var test_description = "Testing when expressions.";

fn test_when_expression_conditional_form_match() {
  var x = 3;
  var result = when {
      x == 1 => "one",
      x == 2 => "two",
      x == 3 => "three",
      else => "other",
  };
  assert_eq(result, "three");
}

fn test_when_expression_conditional_form_no_match() {
  var x = 4;
  var result = when {
      x == 1 => "one",
      x == 2 => "two",
      x == 3 => "three",
      else => "other",
  };
  assert_eq(result, "other");
}

fn test_when_expression_conditional_form_no_else_match() {
  var x = 3;
  var result = when {
      x == 1 => "one",
      x == 2 => "two",
      x == 3 => "three",
  };
  assert_eq(result, "three");
}

fn test_when_expression_conditional_form_no_else_no_match() {
  var x = 4;
  var result = when {
      x == 1 => "one",
      x == 2 => "two",
      x == 3 => "three",
  };
  assert_eq(result, nil);
}

fn test_when_expression_match_form_match() {
    var x = 2;
    var result = when (x) {
        1 => "one",
        2 => "two",
        3 => "three",
        else => "other",
    };
    assert_eq(result, "two");
}

fn test_when_expression_match_form_no_match() {
    var x = 4;
    var result = when (x) {
        1 => "one",
        2 => "two",
        3 => "three",
        else => "other",
    };
    assert_eq(result, "other");
}

fn test_when_expression_match_form_no_else_match() {
  var x = 3;
  var result = when (x) {
      1 => "one",
      2 => "two",
      3 => "three",
  };
  assert_eq(result, "three");
}

fn test_when_expression_match_form_no_else_no_match() {
  var x = 5;
  var result = when (x) {
      1 => "one",
      2 => "two",
      3 => "three",
  };
  assert_eq(result, nil);
}

fn test_when_expression_match_form_match_type() {
  var x = 2;
  var result = when (x) {
      is NUMBER => true,
      else => false,
  };
  assert_eq(typeof(result), BOOLEAN);
  assert(result);
}

fn test_when_expression_match_form_no_match_type() {
  var x = "2";
  var result = when (x) {
      is NUMBER => true,
      else => false,
  };
  assert_eq(typeof(result), BOOLEAN);
  assert(!result);
}