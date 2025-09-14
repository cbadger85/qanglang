fn fib(n) {
  if (n < 2) return n;
  return fib(n - 2) + fib(n - 1);
}

var start = system_time();
println(fib(35) == 9227465);
println(system_time() - start);
