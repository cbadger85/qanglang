mod hello_world = import("./module_other.ql");

// Simple helper module for testing basic exports
var exported_value = 42;

fn get_value() {
  return exported_value;
}

fn double_value(x) {
  return x * 2;
}

fn sum(a, b) {
  return a + b;
}