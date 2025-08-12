var test_description = "Testing for and while loops.";

fn test_simple_while_loop() {
  var run = true;
  var count = 0;

  while (run) {
    run = false;
    count = count + 1;
  }

  assert_eq(count, 1, "Expected while loop to run once.");
}

fn test_while_loop_with_counter() {
  var count = 0;

  while (count < 10) {
    count = count + 1;
  }

  assert_eq(count, 10, "Expected while loop to run 10 times.");
}

fn test_nested_while_loops() {
  var run = true;
  var count = 0;
  
  while (run) {
    while (count < 10) {
      count = count + 1;
    }
    run = false;
  }

   assert_eq(count, 10, "Expected inner while loop to run 10 times.");
  }
  
// TODO remove todo when break is implemented.
// fn test_simple_for_loop() {
//   var count = 0;
//   for (;;) {
//     count = count + 1;
//     break;
//   }
//   assert_eq(count, 1, "Expected loop to run once.");
// }

// TODO remove todo when break is implemented.
// fn test_for_loop_with_initialization() {
//   var count = 0;
//   for (var i = 0;;) {
//     i = i + 1;
//     count = count + 1;
//     break;
//   }
//   assert_eq(count, 1, "Expected loop to run once.");
// }

fn test_for_loop_with_condition() {
  var count = 0;
  var keep_going = true;
  for (;keep_going;) {
    count = count + 1;
    keep_going = false;
  }
  assert_eq(count, 1, "Expected loop to run once.");
}

fn test_for_loop_with_all_parts() {
  var count = 0;
  for (var i = 0; i < 10; i = i + 1) {
    assert_eq(i, count, "Expected loop variable to be incremented.");
    count = count + 1;
  }
}

// TODO remove todo when break is implemented.
// fn test_for_loop_with_missing_parts() {
//   var count = 0;
//   for (var i = 0; ; i = i + 1) {
//     if (i == 10) break;
//   }
//   assert_eq(count, 10, "Expected inner while loop to run 10 times.");
// }

fn test_nested_for_loops() {
  var count = 0;
  for (var i = 0; i < 10; i = i + 1) {
    for (var j = 0; j < 10; j = j + 1) {
      count = count + 1;
    }
  }

  assert_eq(count, 100, "Expected count to equal 100 ");
}

// fn test_while_loop_break_statement() {}

// fn test_for_loop_break_statement() {}

// fn test_while_loop_continue_statement() {}

// fn test_for_loop_continue_statement() {}
