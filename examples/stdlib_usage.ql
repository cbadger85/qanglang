var arr = [1, 2, 3, 4];

var new_arr = iter_array(arr) 
  |> MapIterator((item) -> item * 3) 
  |> FilterIterator((item) -> item % 2 == 0) 
  |> iter_collect();
println(new_arr);