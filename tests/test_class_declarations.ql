var test_description = "Testing class declarations and members.";

fn test_empty_class_declaration() {
  class TestClass {}

  assert_eq(
    TestClass |> to_string, 
    "TestClass<class>", 
    "Expected TestClass<class> but received " + (TestClass |> to_string)
  );
  assert_eq(
    TestClass() |> to_string, 
    "instanceof TestClass", 
    "Expected instanceof TestClass but received " + (TestClass() |> to_string)
  );
}

fn test_class_with_field_declarations() {
    class TestClass {
      test_field;
    }

  assert_eq(
    TestClass().test_field, 
    nil, 
    "Expected nil but received " + (TestClass().test_field |> to_string)
  );
}

fn test_class_with_field_initialization() {
  class TestClass {
    test_field = 42;
  }

  assert_eq(
    TestClass().test_field, 
    42, 
    "Expected 42 but received " + (TestClass().test_field |> to_string)
  );
}

fn test_class_with_method_declarations() {
    class TestClass {
      test_method() {
        return 42;
      }
  }

  assert_eq(
    TestClass().test_method(), 
    42, 
    "Expected 42 but received " + (TestClass().test_method() |> to_string)
  );
  
  var test_method = TestClass().test_method;
  assert_eq(
    test_method(), 
    42, 
    "Expected 42 but received " + (test_method() |> to_string)
  );
}

fn test_class_with_mixed_members() {
  class TestClass {
    test_field = 42;

    test_method() {
      return 42;
    }
  }

  assert_eq(
    TestClass().test_field, 
    42, 
    "Expected 42 but received " + (TestClass().test_field |> to_string)
  );

  assert_eq(
    TestClass().test_method(), 
    42, 
    "Expected 42 but received " + (TestClass().test_method() |> to_string)
  );
  
  var test_method = TestClass().test_method;
  assert_eq(
    test_method(), 
    42, 
    "Expected 42 but received " + (test_method() |> to_string)
  );
}

fn test_class_inheritance() {
  class TestClass {
    test_field = 42;

    test_method() {
      return 42;
    }
  }

  class OtherClass : TestClass {}
  assert_eq(
    OtherClass().test_field, 
    42, 
    "Expected 42 but received " + (OtherClass().test_field |> to_string)
  );

  assert_eq(
    OtherClass().test_method(), 
    42, 
    "Expected 42 but received " + (OtherClass().test_method() |> to_string)
  );
  
  var test_method = OtherClass().test_method;
  assert_eq(
    test_method(), 
    42, 
    "Expected 42 but received " + (test_method() |> to_string)
  );
}

fn test_class_initialization() {
  class TestClass {
    test_field = 42;
    other_field;

    init() {
      this.other_field = this.test_field;
    }
  }

  var test_instance = TestClass();
  assert_eq(test_instance.test_field, test_instance.other_field, "Expected test_field and other_field to be equal.");
}

fn test_class_this_keyword() {
  class TestClass {
    test_field = 42;

    test_method() {
      return this.test_field;
    }
  }

  assert_eq(
    TestClass().test_method(), 
    42, 
    "Expected 42 but received " + (TestClass().test_method() |> to_string)
  );
}

fn test_class_super_keyword() {
  class TestClass {
    test_field = 42;
  }

  class OtherClass : TestClass {
    other_field;

    init() {
      super.init();
      this.other_field = super.other_field;
    }
  }  

  assert_eq(TestClass().test_field, OtherClass().other_field, "Expected fields to be equal.");
}

fn test_class_super_method_calls() {}

fn test_nested_class_declarations() {}