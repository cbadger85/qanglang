var test_description = "Testing Iterators.";

fn test_array_iterator() {
  var collected_arr = [1, 2, 3,].iter().to_array();

  assert_eq(collected_arr[0], 1);
  assert_eq(collected_arr[1], 2);
  assert_eq(collected_arr[2], 3);
}

fn test_range_iterator() {
  var range_arr = Range(0, 10).to_array();

  assert_eq(range_arr[0], 0);
  assert_eq(range_arr[-1], 9);
}

fn test_range_iterator_inclusive() {
  var range_arr = RangeInclusive(0, 10).to_array();

  assert_eq(range_arr[0], 0);
  assert_eq(range_arr[-1], 10);
}

fn test_map_iterator() {
  var range = Range(0, 3);

  var doubled_range = range.map((value) -> value * 2).to_array();

  assert_eq(doubled_range[0], 0);
  assert_eq(doubled_range[1], 2);
  assert_eq(doubled_range[2], 4);
}

fn test_map_indexed_iterator() {
  var range = Range(0, 3);
  var modified_range = range.map_indexed((i, value) -> i + value).to_array();

  assert_eq(modified_range[0], 0);
  assert_eq(modified_range[1], 2);
  assert_eq(modified_range[2], 4);
}

fn is_even(value) {
  return value % 2 == 0;
}

fn test_filter_iterator() {
  var range = Range(1, 5);
  var filtered_range = range.filter(is_even).to_array();

  assert_eq(filtered_range[0], 2);
  assert_eq(filtered_range[1], 4);
}

fn test_filter_indexed_iterator() {
  var range = Range(1, 5);
  var filtered_range = range
    .filter_indexed((i, value) -> is_even(i))
    .to_array();

  assert_eq(filtered_range[0], 1);
  assert_eq(filtered_range[1], 3);
}

fn test_flat_map_iterator() {
  var arrs = [[1, 2,], [3, 4, 5], [6, 7, 8]];

  var flattened_arr = arrs.iter()
    .flat_map((arr) -> arr.iter())
    .to_array();
  
  assert_eq(flattened_arr[0], 1);
  assert_eq(flattened_arr[1], 2);
  assert_eq(flattened_arr[2], 3);
  assert_eq(flattened_arr[3], 4);
  assert_eq(flattened_arr[4], 5);
  assert_eq(flattened_arr[5], 6);
  assert_eq(flattened_arr[6], 7);
  assert_eq(flattened_arr[7], 8);
}

fn test_for_each() {
  var num = 1;

  Range(1, 5).for_each((value) -> {
    assert_eq(num, value);
    num += 1;
  });
}

fn test_for_each_indexed() {
  Range(1, 5)
    .for_each_indexed((i, value) -> assert_eq(i + 1, value));
}

fn test_fold() {
  var sum = Range(1, 5).fold(0, (acc, value) -> acc + value);
  assert_eq(sum, 10);
}

fn test_reduce() {
  var sum = Range(1, 5).reduce((acc, value) -> acc + value);
  assert_eq(sum, 10);
}

fn test_take() {
  var arr = Range(1, 5).take(2).to_array();

  assert_eq(arr.length(), 2);
  assert_eq(arr[0], 1);
  assert_eq(arr[1], 2);
}

fn test_skip() {
  var arr = Range(1, 5).skip(2).to_array();

  assert_eq(arr.length(), 2);
  assert_eq(arr[0], 3);
  assert_eq(arr[1], 4);
}

fn test_find() {
  var animals = ["sheep", "goat", "cow", "chicken", "pig"];

  var goat = animals.iter().find((animal) -> animal == "goat");

  assert_eq(goat, "goat");
}


fn test_find_not_found() {
  var animals = ["sheep", "goat", "cow", "chicken", "pig"];

  var horse = animals.iter().find((animal) -> animal == "horse");

  assert_eq(horse, nil);
}

fn test_any() {
  var arr = [1, 2, 3, 4, -5];

  var contains_negative = arr.iter().any((value) -> value < 0);

  assert(contains_negative);
}

fn test_all() {
  var arr = [1, 2, 3, 4, 5];

  var contains_only_positive =  arr.iter().all((value) -> value >= 0);

   assert(contains_only_positive);
}

fn test_count() {
  var count = Range(0, 10);

  assert_eq(count.count(), 10);
}

fn test_chain() {
  var range1 = Range(0, 5);
  var range2 = Range(5, 10);

  var range = range1.chain(range2).to_array();
  assert_eq(range.length(), 10);
  assert_eq(range[0], 0);
  assert_eq(range[1], 1);
  assert_eq(range[2], 2);
  assert_eq(range[3], 3);
  assert_eq(range[4], 4);
  assert_eq(range[5], 5);
  assert_eq(range[6], 6);
  assert_eq(range[7], 7);
  assert_eq(range[8], 8);
  assert_eq(range[9], 9);
}

fn test_chain() {
  var range1 = Range(0, 5);
  var range2 = Range(5, 10);

  var range = range1.zip(range2).to_array();
  assert_eq(range[0].left, 0);
  assert_eq(range[0].right, 5);
  assert_eq(range[1].left, 1);
  assert_eq(range[1].right, 6);
  assert_eq(range[2].left, 2);
  assert_eq(range[2].right, 7);
  assert_eq(range[3].left, 3);
  assert_eq(range[3].right, 8);
  assert_eq(range[4].left, 4);
  assert_eq(range[4].right, 9);
}

fn test_enumerate() {
  var animals = ["sheep", "goat", "cow", "chicken", "pig"];

  var enumerated_animals = animals.iter().enumerate().to_array();

  assert_eq(enumerated_animals[0].left, 0);
  assert_eq(enumerated_animals[0].right, "sheep");
  assert_eq(enumerated_animals[1].left, 1);
  assert_eq(enumerated_animals[1].right, "goat");
  assert_eq(enumerated_animals[2].left, 2);
  assert_eq(enumerated_animals[2].right, "cow");
  assert_eq(enumerated_animals[3].left, 3);
  assert_eq(enumerated_animals[3].right, "chicken");
  assert_eq(enumerated_animals[4].left, 4);
  assert_eq(enumerated_animals[4].right, "pig");
}

fn test_array_iter_method() {
  var arr = [1, 2, 3];

  assert(arr.iter() is ArrayIterator);
}

fn test_reverse_iterator() {
  var arr = [1, 2, 3];

  var reversed_arr = arr.iter().reverse().to_array();

  assert_eq(reversed_arr.length(), 3);
  assert_eq(reversed_arr[0], 3);
  assert_eq(reversed_arr[1], 2);
  assert_eq(reversed_arr[2], 1);
}

fn test_to_set() {
  var arr = [1, 1, 2, 2, 3];

  var set = arr.iter().to_set();
  assert(set is HashSet);
  assert_eq(set.size(), 3);
  assert_eq([1, 2, 3].iter().to_set().difference(set).size(), 0);
  assert(set.has(1));
  assert(set.has(2));
  assert(set.has(3));
}

fn test_to_map() {
  var arr = [Entry("a", 0), Entry("b", 1)];

  var map = arr.iter().to_map();
  assert(map is HashMap);
  assert_eq(map.size(), 2);
  assert_eq(map.get("a"), 0);
  assert_eq(map.get("b"), 1);
}

fn test_take_until() {
  var arr = [0, 1, 2, 3, 4, 5]
    .iter()
    .take_until((n) -> n <= 2)
    .to_array();
    
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], 0);
  assert_eq(arr[1], 1);
  assert_eq(arr[2], 2);
}

fn test_skip_until() {
  var arr = [0, 1, 2, 3, 4, 5]
    .iter()
    .skip_until((n) -> n > 2)
    .to_array();

  assert_eq(arr.length(), 3);
  assert_eq(arr[0], 3);
  assert_eq(arr[1], 4);
  assert_eq(arr[2], 5);
}

fn test_string_iterator() {
  var string = "abc";
  var arr = StringIterator(string).to_array();
  
  assert_eq(arr.length(), 3);
  assert_eq(arr[0], "a");
  assert_eq(arr[1], "b");
  assert_eq(arr[2], "c");
}