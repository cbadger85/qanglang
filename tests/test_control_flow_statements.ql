var test_description = "Testing break, continue, and return statements.";

// fn test_break_statement_in_while_loop() {}

// fn test_break_statement_in_for_loop() {}

// fn test_continue_statement_in_while_loop() {}

// fn test_continue_statement_in_for_loop() {}

// fn test_nested_loops_with_break() {}

// fn test_nested_loops_with_continue() {}

fn test_return_statement_without_value() {
  fn test_function() {
    return;
  }

  assert_eq(test_function(), nil);
}

fn test_return_statement_with_value() {
  fn test_function() {
    return 42;
  }

  assert_eq(test_function(), 42);
}

fn test_return_statement_with_expression() {
  fn test_function() {
    return 40 + 2;
  }

  assert_eq(test_function(), 42);
}

fn test_early_return_from_function() {
  fn test_function(should_return) {
    if (should_return) {
      return true;
    }

    return nil;
  }

  assert(test_function(true));
}

fn test_return_in_nested_blocks() {
  fn test_function(should_return) {
    {
      {
        {
          {
            return true;
          }
        }
      }
    }
  }

   assert(test_function(true));
}
