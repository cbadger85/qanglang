// examples/closure_bench.ql - Heavy closure usage benchmark

fn make_counter() {
  var count = 0;
  return () -> {
    count = count + 1;
    return count;
  };
}

fn make_accumulator(initial) {
  var sum = initial;
  return (value) -> {
    sum = sum + value;
    return sum;
  };
}

fn make_factorial_closure() {
  return (n) -> {
    if (n <= 1) return 1;
    return n * make_factorial_closure()(n - 1);
  };
}

// Simpler closure-heavy fibonacci that captures variables
fn make_fib_closure() {
  var cache_a = 0;
  var cache_b = 1;
  
  return (n) -> {
    if (n < 2) return n;
    
    var fib_inner = make_fib_closure();
    return fib_inner(n - 1) + fib_inner(n - 2);
  };
}

// More realistic closure benchmark - counters with captured state
fn counter_benchmark() {
  var counters = [];
  var i = 0;
  
  // Create 10 counter closures, each capturing different state
  while (i < 10) {
    var counter = make_counter();
    
    // Call each counter multiple times
    var j = 0;
    while (j < 100) {
      counter();
      j = j + 1;
    }
    i = i + 1;
  }
}

// Accumulator benchmark - closures that modify captured variables
fn accumulator_benchmark() {
  var acc1 = make_accumulator(0);
  var acc2 = make_accumulator(100);
  var acc3 = make_accumulator(1000);
  
  var i = 0;
  while (i < 1000) {
    acc1(i);
    acc2(i * 2);
    acc3(i * 3);
    i = i + 1;
  }
  
  return acc1(0) + acc2(0) + acc3(0);
}

// Main benchmark
var start = system_time();

// Run multiple closure-heavy operations
counter_benchmark();
var result = accumulator_benchmark();

// Simple closure fib for comparison with regular fib
var fib_closure = make_fib_closure();
fib_closure(20);  // Smaller number since closures are more expensive

print("Closure benchmark completed in ");
print(system_time() - start);
print(" ms, result: ");
print(result);
println("");