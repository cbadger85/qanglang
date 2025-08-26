var test_description = "Testing object literals and field access.";

fn test_empty_object_literal() {
  var obj = {{}};

  assert(obj != nil);
}

fn test_object_literal_with_single_field() {
  var obj = {{
    field = true,
  }};

  assert(obj.field);
}

fn test_object_literal_with_multiple_fields() {
  var obj = {{
    field1 = 1,
    field2 = 2,
  }};

  assert_eq(obj.field1, 1);
  assert_eq(obj.field2, 2);
}

fn test_object_property_value_shorthand() {
  var field = true;
  var obj = {{ field }};

  assert(obj.field);
}

fn test_object_field_assignment() {
  var obj1 = {{}};
  obj1.field = true;
  assert(obj1.field);

  var obj2 = {{ field = obj1.field }};
  assert(obj2.field);
}

fn test_nested_object_literals() {
  var obj = {{
    field_obj = {{
      field = true,
    }},
  }};

  assert(obj.field_obj.field);
}

fn test_object_field_function_values() {
  var obj = {{
    fun = () -> true,
  }};

  var fun = obj.fun;
  assert(fun());
  assert(obj.fun());
}
