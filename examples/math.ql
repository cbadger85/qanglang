fn sum(a, b) {
  return a + b;
}

fn create_array(length, init) {
  return Range(0, length) |> iter_map(init) |> iter_collect();
}