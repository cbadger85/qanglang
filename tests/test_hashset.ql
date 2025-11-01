var test_description = "Testing HashSet.";

fn test_hashset_with_inital_values() {
  var set = HashSet(["value"]);
  assert_eq(set.size(), 1);
  assert(set.has("value"));
}

fn test_hashset_panics_when_inital_value_not_array() {
  assert_throws(() -> HashSet(false));
}

fn test_hashset_add_and_has() {
  var set = HashSet();

  assert(set.add("value"));
  assert_eq(set.size(), 1);
  assert(set.has("value"));
}

fn test_hashset_add_existing_value() {
  var set = HashSet(["value"]);

  assert(!set.add("value"));
  assert_eq(set.size(), 1);
  assert(set.has("value"));
}

fn test_hashset_delete() {
  var set = HashSet(["value"]);

  set.delete("value");
  assert_eq(set.size(), 0);
}

fn test_hashset_clear() {
  var set = HashSet(["value"]);
  set.clear();
  assert_eq(set.size(), 0);
}

fn test_hashset_clear() {
  var values = HashSet(["value"]).values().to_array();
  assert_eq(values.length(), 1);
  assert_eq(values[0], "value");
}

fn test_hashset_union() {
  var set = HashSet(["value"]);
  var other_set = HashSet(["value"]);

  var union = set.union(other_set);

  assert_eq(union.size(), 1);
  assert(union.has("value"));
}

fn test_hashset_intersection() {
  var set = HashSet(["value"]);
  var other_set = HashSet(["value", "other_value"]);

  var intersection = set.intersection(other_set);

  assert_eq(intersection.size(), 1);
  assert(intersection.has("value"));
}

fn test_hashset_difference() {
  var set = HashSet(["value", "other_value"]);
  var other_set = HashSet(["value"]);

  var difference = set.difference(other_set);

  assert_eq(difference.size(), 1);
  assert(difference.has("other_value"));
}