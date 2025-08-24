var test_description = "Testing break, continue, and return statements.";

fn test_break_statement_in_while_loop() {
  var count = 0;
  var iterations = 0;
  while (true) {
    iterations = iterations + 1;
    if (iterations >= 5) {
      break;
    }
    count = count + 1;
  }
  assert_eq(count, 4, "Expected count to be 4 before break.");
  assert_eq(iterations, 5, "Expected 5 iterations before break.");
}

fn test_break_statement_in_for_loop() {
  var sum = 0;
  for (var i = 0; i < 100; i = i + 1) {
    if (i == 10) {
      break;
    }
    sum = sum + i;
  }
  assert_eq(sum, 45, "Expected sum of 0-9 which is 45.");
}

fn test_continue_statement_in_while_loop() {
  var count = 0;
  var sum = 0;
  while (count < 10) {
    count = count + 1;
    if (count % 2 == 0) {
      continue;
    }
    sum = sum + count;
  }
  assert_eq(sum, 25, "Expected sum of odd numbers 1+3+5+7+9 = 25.");
}

fn test_continue_statement_in_for_loop() {
  var sum = 0;
  for (var i = 0; i < 10; i = i + 1) {
    if (i % 2 == 0) {
      continue;
    }
    sum = sum + i;
  }
  assert_eq(sum, 25, "Expected sum of odd numbers 1+3+5+7+9 = 25.");
}

fn test_nested_loops_with_break() {
  var outer_count = 0;
  var inner_count = 0;
  for (var i = 0; i < 5; i = i + 1) {
    outer_count = outer_count + 1;
    for (var j = 0; j < 5; j = j + 1) {
      inner_count = inner_count + 1;
      if (j == 2) {
        break;
      }
    }
  }
  assert_eq(outer_count, 5, "Expected outer loop to run 5 times.");
  assert_eq(inner_count, 15, "Expected inner loop to run 3 times per outer (5*3=15).");
}

fn test_nested_loops_with_continue() {
  var total = 0;
  for (var i = 0; i < 3; i = i + 1) {
    for (var j = 0; j < 5; j = j + 1) {
      if (j == 2) {
        continue;
      }
      total = total + 1;
    }
  }
  assert_eq(total, 12, "Expected 4 iterations per outer loop * 3 = 12.");
}

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
