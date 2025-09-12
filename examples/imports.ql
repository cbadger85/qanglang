mod math from "./math.ql";

println(math.sum(1, 2));

var arr = math.create_array(10, (x) -> x);
println(arr.length());
