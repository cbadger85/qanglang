var create_array = (length, init) -> Range(0, length) 
  |> iter_map(init) 
  |> iter_collect();

var value = 0;

fn increment() {
  value += 1;
  return value;
}