var test_description = "Testing block statements and scope.";

fn test_nested_blocks() {
  var test_value = "test";
  {
    {
      {
        assert_eq(test_value, "test", "Expected test_value to be 'test'.");
      }
    }
  }
}

fn test_block_scope_variables() {
  var test_value = "test";
  assert_eq(test_value, "test", "Expected test_value to be 'test'.");
  
  {
    assert_eq(test_value, "test", "Expected test_value to still be 'test'.");
  }
}

fn test_block_scope_shadowing() {
  var test_value = nil;
  assert_eq(test_value, nil, "Expected test_value to be nil.");
  test_value = 2;
  assert_eq(test_value, 2, "Expected test_value to be 2.");
  
  {
    var test_value = "2";
    assert_eq(test_value, "2", "Expected test_value to be '2'.");
  }

  assert_eq(test_value, 2, "Expected test_value to still be 2.");
}
