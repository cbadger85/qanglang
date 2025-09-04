var arr = [1, 2, 3, 4];

var new_arr = iter_array(arr) 
  |> iter_map((item) -> item * 3) 
  |> iter_filter((item) -> item % 2 == 0) 
  |> iter_collect();
println(new_arr);