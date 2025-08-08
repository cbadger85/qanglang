fn fib(n) {
  if (n < 2) {
    return n;
  }
  return fib(n - 2) + fib(n - 1);
}

var start = system_time();
println(fib(32));
print("Completed in ");
print(system_time() - start);
print("ms");
println("");
