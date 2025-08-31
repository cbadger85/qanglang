var test_description = "Testing function declarations and definitions.";

fn test_function_declaration_without_parameters() {
  fn simple_function() {
    return "hello";
  }
  
  var result = simple_function();
  assert_eq(result, "hello", "Expected function to return 'hello'.");
}

fn test_function_declaration_with_single_parameter() {
  fn add_one(x) {
    return x + 1;
  }
  
  var result = add_one(5);
  assert_eq(result, 6, "Expected add_one(5) to return 6.");
}

fn test_function_declaration_with_multiple_parameters() {
  fn add_three(x, y, z) {
    return x + y + z;
  }
  
  var result = add_three(1, 2, 3);
  assert_eq(result, 6, "Expected add_three(1, 2, 3) to return 6.");
}

fn test_function_declaration_with_trailing_comma() {
  fn add_two(x, y,) {
    return x + y;
  }
  
  var result = add_two(3, 4);
  assert_eq(result, 7, "Expected add_two(3, 4) to return 7.");
}

fn test_function_with_empty_body() {
  fn empty_function() {}
  
  var result = empty_function();
  assert_eq(result, nil, "Expected empty function to return nil.");
}

fn test_function_with_return_statement() {
  fn early_return() {
    return;
    var unreachable = true;
  }
  
  var result = early_return();
  assert_eq(result, nil, "Expected early return to return nil.");
}

fn test_function_with_return_value() {
  fn return_value() {
    return 42;
  }
  
  var result = return_value();
  assert_eq(result, 42, "Expected function to return 42.");
}

fn test_function_with_return_expression() {
  fn calculate(x) {
    return x * 2 + 1;
  }
  
  var result = calculate(5);
  assert_eq(result, 11, "Expected calculate(5) to return 11.");
}

fn test_function_recursion() {
  var factorial;
  factorial = (n) -> {
    if (n <= 1) {
      return 1;
    }
    return n * factorial(n - 1);
  };
  
  var result = factorial(5);
  assert_eq(result, 120, "Expected factorial(5) to return 120.");
}

fn test_nested_function_declarations() {
  fn outer_function() {
    fn inner_function() {
      return "inner";
    }
    return inner_function();
  }
  
  var result = outer_function();
  assert_eq(result, "inner", "Expected nested function to work correctly.");
}

fn test_function_parameter_shadowing() {
  var x = 10;
  
  fn shadow_test(x) {
    return x + 1;
  }
  
  var result = shadow_test(5);
  assert_eq(result, 6, "Expected parameter to shadow outer variable.");
  assert_eq(x, 10, "Expected outer variable to remain unchanged.");
}

fn test_function_local_variables() {
  fn local_vars_test() {
    var local_var = "local";
    var another_local = "42";
    return local_var + " " + another_local;
  }
  
  var result = local_vars_test();
  assert_eq(result, "local 42", "Expected function to handle local variables correctly.");
}

fn test_three_level_nested_closures() {
  // Test for deeply nested closures with proper upvalue resolution
  // This ensures variables are correctly captured through multiple closure levels
  var result = ((a) -> {
    return ((b) -> {
      return ((c) -> {
        return a + b + c;
      })(4);
    })(3);
  })(2);
  
  assert_eq(result, 9, "Expected three-level closure to correctly capture variables: 2 + 3 + 4 = 9.");
}

fn test_map_expressions_with_nested_closures() {
  // Test that map expressions (which compile to IIFEs) also work with nested closures
  var result = 2||a -> (3||b -> (4||c -> a + b + c|)|)|;
  
  assert_eq(result, 9, "Expected nested map expressions to work like nested closures: 2 + 3 + 4 = 9.");
}


fn test_multiple_closures_capture_same_variable() {
  var shared_var = 42;
  
  var closure1 = () -> {
      return shared_var + 1;
  };
  
  var closure2 = () -> {
      return shared_var + 2;
  };
  
  var closure3 = () -> {
      return shared_var + 3;
  };
  
  var closure4 = () -> {
      return shared_var + 4;
  };
  
  var closure5 = () -> {
      return shared_var + 5;
  };
  
  var closure6 = () -> {
      return shared_var + 6;
  };
  
  var result1 = closure1();
  var result2 = closure2();
  var result3 = closure3();
  var result4 = closure4();
  var result5 = closure5();
  var result6 = closure6();
  
  assert_eq(result1, 43); // 42 + 1
  assert_eq(result2, 44); // 42 + 2
  assert_eq(result3, 45); // 42 + 3
  assert_eq(result4, 46); // 42 + 4
  assert_eq(result5, 47); // 42 + 5
  assert_eq(result6, 48); // 42 + 6
}

fn test_stress_many_closures_same_variable() {
  var shared = 99;
  
  // Create 10 closures that all capture 'shared' - definitely exceeds inline capacity
  var closures = [
      () -> shared * 1,
      () -> shared * 2, 
      () -> shared * 3,
      () -> shared * 4,
      () -> shared * 5,
      () -> shared * 6,
      () -> shared * 7,
      () -> shared * 8,
      () -> shared * 9,
      () -> shared * 10
  ];
  
  // Call each closure and verify correct results
  for (var i = 0; i < closures.length(); i += 1) {
      var expected = 99 * (i + 1);
      var actual = closures[i]();
      assert_eq(actual, expected);
  }
}

fn test_upvalue_overflow() {
  // Create a function that captures many upvalues (more than 4 inline capacity)
  var v1 = 1; var v2 = 2; var v3 = 3; var v4 = 4; var v5 = 5;
  var v6 = 6; var v7 = 7; var v8 = 8; var v9 = 9; var v10 = 10;
  var v11 = 11; var v12 = 12; var v13 = 13; var v14 = 14; var v15 = 15;
  
  // This closure captures 15 upvalues - should trigger overflow chunks
  var closure = () -> {
    return v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11 + v12 + v13 + v14 + v15;
  };
  
  assert_eq(closure(), 120, "Expected closure with 15 upvalues to return sum of 1-15 = 120.");
}

fn test_multiple_overflow_chunks() {
  // Test creating multiple closures that each need overflow chunks
  var shared1 = 100; var shared2 = 200; var shared3 = 300;
  var a1 = 1; var a2 = 2; var a3 = 3; var a4 = 4; var a5 = 5;
  var b1 = 10; var b2 = 20; var b3 = 30; var b4 = 40; var b5 = 50;
  
  var closure1 = () -> shared1 + a1 + a2 + a3 + a4 + a5;
  var closure2 = () -> shared2 + b1 + b2 + b3 + b4 + b5;
  var closure3 = () -> shared3 + shared1 + shared2;
  
  assert_eq(closure1(), 115, "Expected closure1 to return 100 + 15 = 115.");
  assert_eq(closure2(), 350, "Expected closure2 to return 200 + 150 = 350.");
  assert_eq(closure3(), 600, "Expected closure3 to return 300 + 100 + 200 = 600.");
}

fn test_upvalue_chain_linking() {
  // Test that overflow chunks properly link together
  var v1 = 1; var v2 = 2; var v3 = 3; var v4 = 4; var v5 = 5;
  var v6 = 6; var v7 = 7; var v8 = 8; var v9 = 9; var v10 = 10;
  var v11 = 11; var v12 = 12; var v13 = 13; var v14 = 14; var v15 = 15;
  var v16 = 16; var v17 = 17; var v18 = 18; var v19 = 19; var v20 = 20;
  
  // This closure captures 20 upvalues - needs multiple overflow chunks
  var closure = () -> {
    return v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 +
           v11 + v12 + v13 + v14 + v15 + v16 + v17 + v18 + v19 + v20;
  };
  
  assert_eq(closure(), 210, "Expected closure with 20 upvalues to return sum of 1-20 = 210.");
}