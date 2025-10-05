var arr = [1, 2, 3, 4];

var new_arr = ArrayIterator(arr)
  .map((item) -> item * 3)
  .filter((item) -> item % 2 == 0)
  .collect();
println(new_arr);