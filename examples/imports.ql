mod math = import("./math.ql");
mod other = import("./other_module.ql");

println(math.sum(1, 2));

var arr = other.create_array(10, (x) -> x);
println(arr.length());

println(other.value);
other.increment();
println(other.value);