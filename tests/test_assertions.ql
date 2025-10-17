var test_description = "Testing the native assertion functions.";

fn test_assert_true() {
  assert_eq(true, true, "Expected 'true' to equal 'true'.");
  assert(true, "Expected 'true' to be truthy");
  assert_eq(true, !false, "Expected 'true' not to be falsy.");
}

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

fn test_not_nil() {
  var value1 = 0;
  var value2 = true;
  var value3 = false;
  var value4 = "value";
  var value5 = [];
  var value6 = {{}};
  class MyClass { }
  var value7 = MyClass();

  assert_not_nil(value1);
  assert_not_nil(value2);
  assert_not_nil(value3);
  assert_not_nil(value4);
  assert_not_nil(value5);
  assert_not_nil(value6);
  assert_not_nil(value7);
  assert_not_nil(MyClass);

  assert_throws(() -> assert_not_nil(nil));
}