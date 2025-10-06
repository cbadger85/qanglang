var test_description = "Testing arrays and their intrinsic methods.";

fn test_empty_array_literal() {
  var arr = [];

  assert_eq(arr.length(), 0);
}

fn test_array_literal_with_single_element() {
  var arr = [1,];

  assert_eq(arr.length(), 1);
  assert_eq(arr[0], 1);
}

fn test_array_literal_with_multiple_elements() {
  var arr = [1, 2, 3,];
  
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], 1);
  assert_eq(arr[1], 2);
  assert_eq(arr[2], 3);
}

fn test_array_literal_with_mixed_types() {
  var arr = [true, 0, nil,];
  
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], true);
  assert_eq(arr[1], 0);
  assert_eq(arr[2], nil);
}

fn test_array_index_assignment() {
  var arr = [1,];

  assert_eq(arr[0], 1);
  arr[0] = 2;
  assert_eq(arr[0], 2);
}

fn test_negative_index() {
  var arr = [true, 0, nil,];
  
  assert_eq(arr[-1], nil);
  assert_eq(arr[-2], 0);
  assert_eq(arr[-3], true);
}

fn test_array_pop() {
    var arr = [1,];
    assert_eq(arr.length(), 1);
    assert_eq(arr[0], 1);
    assert_eq(arr.pop(), 1);
    assert_eq(arr.length(), 0);
}

fn test_pop_throws_error_for_out_of_bounds() {
  var arr = [];

  assert_throws(() -> arr.pop());
}

fn test_array_push() {
    var arr = [];
    assert_eq(arr.length(), 0);
    arr.push(1);
    assert_eq(arr.length(), 1);
    assert_eq(arr[0], 1);
}

fn test_array_reverse() {
  var arr = [1, 2, 3,];
  
  assert_eq(arr[0], 1);
  assert_eq(arr[1], 2);
  assert_eq(arr[2], 3);
  arr.reverse();
  assert_eq(arr[0], 3);
  assert_eq(arr[1], 2);
  assert_eq(arr[2], 1);
}

fn test_array_slice() {
  var arr = [1, 2, 3,];
  var arr_slice = arr.slice(1);
  assert_eq(arr_slice.length(), 2);
  assert_eq(arr_slice[0], 2);
  assert_eq(arr_slice[1], 3);

  var arr_slice2 = arr.slice(-1);
  assert_eq(arr_slice2.length(), 1);
  assert_eq(arr_slice2[0], 3);

  var arr_slice3 = arr.slice(1, 3);
  assert_eq(arr_slice3.length(), 2);
  assert_eq(arr_slice3[0], 2);
  assert_eq(arr_slice3[1], 3);

  var arr_slice4 = arr.slice(1, -1);
  assert_eq(arr_slice4.length(), 1);
  assert_eq(arr_slice4[0], 2);
}

fn test_array_slice_as_shallow_copy() {
  var inner = [3,];
  var arr = [1, 2, inner,];
  var arr_slice = arr.slice();
  assert(arr != arr_slice);
  assert_eq(arr_slice[0], arr[0]);
  assert_eq(arr_slice[1], arr[1]);
  assert_eq(arr_slice[2], arr[2]);
}

fn test_array_get() {
  var arr = [1, 2, 3];
  
  assert_eq(arr.get(0), 1);
  assert_eq(arr.get(-1), 3);
}

fn test_array_concat() {
  var arr1 = [1, 2,];
  var arr2 = [3, 4,];
  var arr3 = arr1.concat(arr2);
  
   assert_eq(arr3[0], 1);
   assert_eq(arr3[1], 2);
   assert_eq(arr3[2], 3);
   assert_eq(arr3[3], 4);
}

fn test_array_concat_with_plus() {
  var arr1 = [1, 2,];
  var arr2 = [3, 4,];
  var arr3 = arr1 + arr2;
  
   assert_eq(arr3[0], 1);
   assert_eq(arr3[1], 2);
   assert_eq(arr3[2], 3);
   assert_eq(arr3[3], 4);
}

fn test_array_from_native_method() {
  var arr1 = array_of_length(1);
  assert_eq(arr1.length(), 1);
  assert_eq(arr1[0], nil);
}

fn test_array_of() {
  var arr1 = array_of(3);
  assert_eq(arr1.length(), 3);
  assert_eq(arr1[0], 0);
  assert_eq(arr1[1], 1);
  assert_eq(arr1[2], 2);
}

fn test_array_of_with_init() {
  var arr1 = array_of(3, (i) -> i + 1);
  assert_eq(arr1.length(), 3);
  assert_eq(arr1[0], 1);
  assert_eq(arr1[1], 2);
  assert_eq(arr1[2], 3);
}

fn test_array_remove_at() {
  var arr = [1, 2, 3,];
  arr.remove_at(1);

  assert_eq(arr.length(), 2);
  assert_eq(arr[0], 1);
  assert_eq(arr[1], 3);
}

fn test_array_remove_at_negative_index() {
  var arr = [1, 2, 3,];
  arr.remove_at(-1);

  assert_eq(arr.length(), 2);
  assert_eq(arr[0], 1);
  assert_eq(arr[1], 2);
}

fn test_array_join() {
  var arr = [1, 2, 3,];

  var string = arr.join();

  assert_eq(string, "123");
}

fn test_array_join_with_delimeter() {
  var arr = [1, 2, 3,];

  var string = arr.join(",");

  assert_eq(string, "1,2,3");
}

fn test_array_from_split_string() {
  var string = "123";

  var arr = string.split();

  assert(arr is ARRAY);
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], "1");
  assert_eq(arr[1], "2");
  assert_eq(arr[2], "3");
}


fn test_array_from_split_string_with_delimeter() {
  var string = "1,2,3";

  var arr = string.split(",");

  assert(arr is ARRAY);
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], "1");
  assert_eq(arr[1], "2");
  assert_eq(arr[2], "3");
}

fn test_array_from_split_string_with_empty_string_delimeter() {
  var string = "123";

  var arr = string.split("");

  assert(arr is ARRAY);
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], "1");
  assert_eq(arr[1], "2");
  assert_eq(arr[2], "3");
}