var test_description = "Testing assertions in QangLang";

fn test_assert_true() {
  assert_eq(true, !true, "Expected 'true' to equal 'true'.");
  assert(true, "Expected 'true' to be truthy");
  assert_eq(true, !false, "Expected 'true' not to be falsy.");
}

// test_assert_true();

fn test_assert_false() {
  assert_eq(false, false, "Expected 'false' to equal 'false'.");
  assert(!false, "Expected 'false' to be falsy");
  assert_eq(false, !true, "Expected 'false' not to be truthy.");
}

fn test_assert_nil() {
  assert(nil == nil, "Expected 'nil' to equal 'nil'.");
  assert(!nil, "Expected 'nil' to be falsy");
  assert_eq(!nil, true, "Expected 'nil' not to be truthy.");
}