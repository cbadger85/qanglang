var test_description = "Testing optional chaining";

fn test_optional_property_access() {
  var maybe_obj1 = {{}};
  assert_eq(maybe_obj1?.foo, nil);
  var maybe_obj2 = {{
    foo = nil,
  }};
  assert_eq(maybe_obj2?.foo, nil);
  var maybe_obj3 = {{
    foo = true,
  }};
  assert(maybe_obj3?.foo);
}

fn test_optional_chaining_inner_access() {
  var maybe_obj1 = {{
    inner = {{
      value = true,
    }},
  }};
  assert(maybe_obj1.inner?.value);
  maybe_obj1.inner = nil;
  assert_eq(maybe_obj1.inner?.value, nil);
}

fn test_optional_calling_functions() {
  fn test_function(x) {
    return x;
  }

  assert(test_function?.call(true));
  var maybe_test_function;
  assert_eq(maybe_test_function?.call(true), nil);
}

fn test_optional_calling_methods() {
  class TestClass {}

  assert_eq(TestClass().maybe_method?.call(), nil);
}

fn test_optional_calling_on_intrinsics() {
  var maybe_array = nil;

  assert_eq(maybe_array?.length?.call(), nil);
}

fn test_optional_applying_args_to_functions() {
  fn test_function(x) {
    return x;
  }

  assert(test_function?.apply([true]));
  var maybe_test_function;
  assert_eq(maybe_test_function?.apply([true]), nil);
}

fn test_applying_args_to_methods() {
  class TestClass {}

  assert_eq(TestClass().maybe_method?.apply([]), nil);
}

fn test_applying_args_to_intrinsics() {
  var maybe_array = nil;

  assert_eq(maybe_array?.length?.apply([]), nil);
}

