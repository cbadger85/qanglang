// Test both approaches now work!

// Traditional curried approach  
fn concat_curried(str2) {
  return (str1) -> str1 + str2;
}
println("hello " |> concat_curried("world!"));

// New partial application approach
fn concat_partial(str1, str2) {
  return str1 + str2;
}
println("hello " |> concat_partial("world!"));

// Single argument - parentheses optional
fn add_one(value) {
  return value + 1;
}

println(10 |> add_one);      // Works
println(10 |> add_one());    // Also works

// Multiple argument partial application
fn add_three_numbers(a, b, c) {
  return a + b + c;
}

println(1 |> add_three_numbers(2, 3)); // Should print 6

// Chaining example
fn double(x) { return x * 2; }
fn add_ten(x) { return x + 10; }

println(5 |> double |> add_ten()); // Should print 20