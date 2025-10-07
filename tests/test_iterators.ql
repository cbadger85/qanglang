var test_description = "Testing Iterators.";

// fn test_array_iterator() {
//   var arr = [1, 2, 3,];

//   var iter = ArrayIterator(arr);

//   var collected_arr = iter.collect();
//   assert_eq(collected_arr[0], 1);
//   assert_eq(collected_arr[1], 2);
//   assert_eq(collected_arr[2], 3);
// }

// fn test_range_iterator() {
//   var range = Range(0, 10);

//   var range_arr = range.collect();

//   assert_eq(range_arr[0], 0);
//   assert_eq(range_arr[-1], 9);
// }

// fn test_map_iterator() {
//   var range = Range(0, 3);

//   var doubled_range = range.map((value) -> value * 2).collect();

//   assert_eq(doubled_range[0], 0);
//   assert_eq(doubled_range[1], 2);
//   assert_eq(doubled_range[2], 4);
// }

// fn test_map_indexed_iterator() {
//   var range = Range(0, 3);
//   var modified_range = range.map_indexed((i, value) -> i + value).collect();

//   assert_eq(modified_range[0], 0);
//   assert_eq(modified_range[1], 2);
//   assert_eq(modified_range[2], 4);
// }

// fn is_even(value) {
//   return value % 2 == 0;
// }

// fn test_filter_iterator() {
//   var range = Range(1, 5);
//   var filtered_range = range.filter(is_even).collect();

//   assert_eq(filtered_range[0], 2);
//   assert_eq(filtered_range[1], 4);
// }

// fn test_filter_indexed_iterator() {
//   var range = Range(1, 5);
//   var filtered_range = range.filter_indexed((i, value) -> is_even(i)).collect();

//   assert_eq(filtered_range[0], 1);
//   assert_eq(filtered_range[1], 3);
// }

// fn test_flat_map_iterator() {
//   var arrs = [[1, 2,], [3, 4, 5], [6, 7, 8]];

//   var flattened_arr = ArrayIterator(arrs)
//     .flat_map((arr) -> ArrayIterator(arr))
//     .collect();
  
//   assert_eq(flattened_arr[0], 1);
//   assert_eq(flattened_arr[1], 2);
//   assert_eq(flattened_arr[2], 3);
//   assert_eq(flattened_arr[3], 4);
//   assert_eq(flattened_arr[4], 5);
//   assert_eq(flattened_arr[5], 6);
//   assert_eq(flattened_arr[6], 7);
//   assert_eq(flattened_arr[7], 8);
// }

// fn test_for_each() {
//   var num = 1;

//   Range(1, 5).for_each((value) -> {
//     assert_eq(num, value);
//     num += 1;
//   });
// }

// fn test_for_each_indexed() {
//   Range(1, 5)
//     .for_each_indexed((i, value) -> assert_eq(i + 1, value));
// }

// fn test_fold() {
//   var sum = Range(1, 5).fold(0, (acc, value) -> acc + value);
//   assert_eq(sum, 10);
// }

// fn test_reduce() {
//   var sum = Range(1, 5).reduce((acc, value) -> acc + value);
//   assert_eq(sum, 10);
// }

// fn test_take() {
//   var arr = Range(1, 5).take(2).collect();

//   assert_eq(arr.length(), 2);
//   assert_eq(arr[0], 1);
//   assert_eq(arr[1], 2);
// }

// fn test_skip() {
//   var arr = Range(1, 5).skip(2).collect();

//   assert_eq(arr.length(), 2);
//   assert_eq(arr[0], 3);
//   assert_eq(arr[1], 4);
// }

// fn test_find() {
//   var animals = ["sheep", "goat", "cow", "chicken", "pig"];

//   var goat = ArrayIterator(animals).find((animal) -> animal == "goat");

//   assert_eq(goat, "goat");
// }


// fn test_find_not_found() {
//   var animals = ["sheep", "goat", "cow", "chicken", "pig"];

//   var horse = ArrayIterator(animals).find((animal) -> animal == "horse");

//   assert_eq(horse, nil);
// }

// fn test_any() {
//   var arr = [1, 2, 3, 4, -5];

//   var contains_negative = ArrayIterator(arr).any((value) -> value < 0);

//   assert(contains_negative);
// }

// fn test_all() {
//   var arr = [1, 2, 3, 4, 5];

//   var contains_only_positive = ArrayIterator(arr).all((value) -> value >= 0);

//    assert(contains_only_positive);
// }

// fn test_count() {
//   var count = Range(0, 10);

//   assert_eq(count.count(), 10);
// }

// fn test_chain() {
//   var range1 = Range(0, 5);
//   var range2 = Range(5, 10);

//   var range = range1.chain(range2).collect();
//   assert_eq(range.length(), 10);
//   assert_eq(range[0], 0);
//   assert_eq(range[1], 1);
//   assert_eq(range[2], 2);
//   assert_eq(range[3], 3);
//   assert_eq(range[4], 4);
//   assert_eq(range[5], 5);
//   assert_eq(range[6], 6);
//   assert_eq(range[7], 7);
//   assert_eq(range[8], 8);
//   assert_eq(range[9], 9);
// }

// fn test_chain() {
//   var range1 = Range(0, 5);
//   var range2 = Range(5, 10);

//   var range = range1.zip(range2).collect();
//   assert_eq(range[0].left, 0);
//   assert_eq(range[0].right, 5);
//   assert_eq(range[1].left, 1);
//   assert_eq(range[1].right, 6);
//   assert_eq(range[2].left, 2);
//   assert_eq(range[2].right, 7);
//   assert_eq(range[3].left, 3);
//   assert_eq(range[3].right, 8);
//   assert_eq(range[4].left, 4);
//   assert_eq(range[4].right, 9);
// }

// fn test_enumerate() {
//   var animals = ["sheep", "goat", "cow", "chicken", "pig"];

//   var enumerated_animals = ArrayIterator(animals).enumerate().collect();

//   assert_eq(enumerated_animals[0].left, 0);
//   assert_eq(enumerated_animals[0].right, "sheep");
//   assert_eq(enumerated_animals[1].left, 1);
//   assert_eq(enumerated_animals[1].right, "goat");
//   assert_eq(enumerated_animals[2].left, 2);
//   assert_eq(enumerated_animals[2].right, "cow");
//   assert_eq(enumerated_animals[3].left, 3);
//   assert_eq(enumerated_animals[3].right, "chicken");
//   assert_eq(enumerated_animals[4].left, 4);
//   assert_eq(enumerated_animals[4].right, "pig");
// }

fn test_array_iter_method() {
  var arr = [1, 2, 3];

  assert(arr.iter() is ArrayIterator);
}