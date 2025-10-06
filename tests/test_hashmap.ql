var test_description = "Testing Hashmaps.";

fn test_hashmap_get_and_set() {
  var map = HashMap();

  map.set("key", "value");

  var value = map.get("key");

  assert_eq(value, "value");
}

fn test_hashmap_has() {
  var map = HashMap();
  map.set("key", "value");

  assert(map.has("key"));
}

fn test_hashmap_size() {
  var map = HashMap();

  assert_eq(map.size(), 0);
  map.set("key", "value");
  assert_eq(map.size(), 1);
}

fn test_hashmap_delete() {
  var map = HashMap();

  map.set("key", "value");
  assert_eq(map.size(), 1);
  assert(map.delete("key"));
  assert_eq(map.size(), 0);
}

fn test_hashmap_delete_no_value_found() {
  var map = HashMap();

  assert(!map.delete("key"));
  assert_eq(map.size(), 0);
}

fn test_hashmap_clear() {
  var map = HashMap();

  map.set("key", "value");
  assert_eq(map.size(), 1);
  map.clear();
  assert_eq(map.size(), 0);
}

fn test_hashmap_entries() {
  var map = HashMap();
  map.set(1, "value");
  var entries = map.entries().collect();
  assert_eq(entries.length(), 1);
  assert_eq(entries[0].key, 1);
  assert_eq(entries[0].value, "value");
}

fn test_hashmap_keys() {
  var map = HashMap();
  map.set(1, "value");
  var keys = map.keys().collect();
  assert_eq(keys.length(), 1);
  assert_eq(keys[0], 1);
}

fn test_hashmap_values() {
  var map = HashMap();
  map.set(1, "value");
  var values = map.values().collect();
  assert_eq(values.length(), 1);
  assert_eq(values[0], "value");
}

fn test_hashmap_resize() {
  var map = HashMap();

  // Initial capacity is 64, resize threshold is 0.75
  // So it should resize after 48 entries
  for (var i = 0; i < 50; i += 1) {
    map.set(i, i * 2);
  }

  // Verify all entries are still accessible after resize
  assert_eq(map.size(), 50);
  for (var i = 0; i < 50; i += 1) {
    assert(map.has(i));
    assert_eq(map.get(i), i * 2);
  }
}