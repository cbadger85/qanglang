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

fn test_object_to_entries_with_object_literal() {
  var obj = {{
    a = 0,
    b = 1,
  }};

  var entries = object_to_entries(obj);
  assert(entries is ARRAY);
  assert_eq(entries.length(), 2);
  assert(entries[0] is Entry);
  assert_eq(entries[0].key, "a");
  assert_eq(entries[0].value, 0);
  assert(entries[1] is Entry);
  assert_eq(entries[1].key, "b");
  assert_eq(entries[1].value, 1);
}

fn test_object_to_entries_with_instance() {
  class MyClass {
    init() {
      this.a = 0;
      this.b = 1;
    }
  }

  var entries = object_to_entries(MyClass());
  assert(entries is ARRAY);
  assert_eq(entries.length(), 2);
  assert(entries[0] is Entry);
  assert_eq(entries[0].key, "a");
  assert_eq(entries[0].value, 0);
  assert(entries[1] is Entry);
  assert_eq(entries[1].key, "b");
  assert_eq(entries[1].value, 1);
}

fn test_object_get_with_object_literal() {
  var obj = {{ key = "value" }};
  assert_eq(object_get(obj, "key"), "value");
}

fn test_object_get_with_class_instance() {
  class MyClass { key = "value"; }
  var instance = MyClass();
  assert_eq(object_get(instance, "key"), "value");
}

fn test_object_set_with_object_literal() {
  var obj = {{ }};
  object_set(obj, "key", "value");

  assert_eq(obj.key, "value");
}

fn test_object_set_with_class_instance() {
  class MyClass { }
  var instance = MyClass();
  object_set(instance, "key", "value");

  assert_eq(instance.key, "value");
}

fn test_object_set_with_class_instance_in_init() {
  class MyClass { 
    init() {
      object_set(this, "key", "value");
    }
  }
  var instance = MyClass();

  assert_eq(instance.key, "value");
}

fn test_object_get_with_class_instance_and_lambda_field() {
  class MyParent {
    parent_key = "VALUE";
  }

  class MyClass : MyParent { 
    key = "value"; 

    init() {
      this.get_key = () -> {
        return this.key;
      };

      this.get_other_key = () -> super.parent_key;
    }
  }

  var instance = MyClass();
  assert_eq(instance.get_key(), "value");
  var get_key = object_get(instance, "get_key");
  assert(get_key is FUNCTION);
  assert_eq(get_key(), "value");
  var get_other_key = object_get(instance, "get_other_key");
  assert(get_other_key is FUNCTION);
  assert_eq(get_other_key(), "VALUE");
}

fn test_object_assign_with_object_literals() {
  var obj1 = {{
    a = 0,
  }};
  var obj2 = {{
    b = 1,
  }};

  var result = object_assign(obj1, obj2);
  assert_eq(obj1.a, 0);
  assert_eq(obj1.b, 1);
  assert(result is OBJECT);
  assert_eq(result.a, 0);
  assert_eq(result.b, 1);
}

fn test_object_assign_with_class_instances() {
  class MyClass1 { a = 0; }
  class MyClass2 { b = 1; }
  var my_class1 = MyClass1();
  var my_class2 = MyClass2();

  var result = object_assign(my_class1, my_class2);
  assert_eq(my_class1.a, 0);
  assert_eq(my_class1.b, 1);
  assert(result is MyClass1);
  assert_eq(result.a, 0);
  assert_eq(result.b, 1);
}

fn test_object_assign_with_object_literal_and_instance() {
  var obj = {{
    a = 0,
  }};
  class MyClass { b = 1; }
  var my_class = MyClass();
  
  var result = object_assign(obj, my_class);
  assert_eq(obj.a, 0);
  assert_eq(obj.b, 1);
  assert(result is OBJECT);
  assert_eq(result.a, 0);
  assert_eq(result.b, 1);
}

fn test_object_assign_with_instance_and_object_literal() {
  var obj = {{
    a = 0,
  }};
  class MyClass { b = 1; }
  var my_class = MyClass();
  
  var result = object_assign(my_class, obj);
  assert_eq(my_class.a, 0);
  assert_eq(my_class.b, 1);
  assert(result is MyClass);
  assert_eq(result.a, 0);
  assert_eq(result.b, 1);
}
