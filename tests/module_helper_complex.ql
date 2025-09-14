// Complex helper module for testing advanced module features
var counter = 0;
var config = {{
  name = "test_module",
  version = "1.0.0"
}};

fn increment_counter() {
  counter += 1;
  return counter;
}

fn reset_counter() {
  counter = 0;
  return counter;
}

fn get_config() {
  return config;
}

fn create_multiplier(factor) {
  return (x) -> x * factor;
}

var default_multiplier = create_multiplier(10);

// Array of functions
var operations = [
  (x) -> x + 1,
  (x) -> x * 2,
  (x) -> x - 1
];

fn apply_operations(value) {
  var result = value;
  for (var i = 0; i < operations.length(); i += 1) {
    result = operations[i](result);
  }
  return result;
}