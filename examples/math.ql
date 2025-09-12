fn sum(a, b) {
  return a + b;
}

var create_array = (length, init) -> Range(0, length) 
  |> iter_map(init) 
  |> iter_collect();