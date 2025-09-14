// Module operations performance benchmark
// Tests module loading, function calls, and state management

// Helper module for benchmarking (inline definition)
var module_cache = {{}};

fn create_counter_module(initial) {
    if (module_cache.counter) {
        return module_cache.counter;
    }

    var counter = initial;
    var ops = {{}};

    ops.increment = () -> {
        counter += 1;
        return counter;
    };

    ops.decrement = () -> {
        counter -= 1;
        return counter;
    };

    ops.get_value = () -> counter;

    ops.reset = () -> {
        counter = initial;
        return counter;
    };

    ops.multiply = (factor) -> {
        counter *= factor;
        return counter;
    };

    module_cache.counter = ops;
    return ops;
}

fn create_math_module() {
    if (module_cache.math) {
        return module_cache.math;
    }

    var ops = {{}};

    ops.factorial = (n) -> {
        if (n <= 1) return 1;
        var result = 1;
        for (var i = 2; i <= n; i += 1) {
            result *= i;
        }
        return result;
    };

    ops.fibonacci = (n) -> {
        if (n < 2) return n;
        var a = 0;
        var b = 1;
        for (var i = 2; i <= n; i += 1) {
            var temp = a + b;
            a = b;
            b = temp;
        }
        return b;
    };

    ops.sum_array = (arr) -> {
        var total = 0;
        for (var i = 0; i < arr.length(); i += 1) {
            total += arr[i];
        }
        return total;
    };

    ops.create_range = (start, end) -> {
        var result = [];
        for (var i = start; i < end; i += 1) {
            result.push(i);
        }
        return result;
    };

    module_cache.math = ops;
    return ops;
}

var start = system_time();

// Benchmark 1: Module function calls
var counter_mod = create_counter_module(0);
var math_mod = create_math_module();

var iterations = 10000;
println("=== Module Function Call Benchmark ===");
println("Iterations:");
println(iterations);

var call_start = system_time();
for (var i = 0; i < iterations; i += 1) {
    counter_mod.increment();
    counter_mod.get_value();
    if (i % 1000 == 0) {
        counter_mod.reset();
    }
}
var call_time = system_time() - call_start;

println("Function calls completed. Final counter value:");
println(counter_mod.get_value());
println("Time for function calls (ms):");
println(call_time);

// Benchmark 2: Mathematical operations through modules
println("\n=== Module Mathematical Operations ===");
var math_start = system_time();
var factorial_sum = 0;
var fib_sum = 0;

for (var i = 1; i <= 100; i += 1) {
    if (i <= 12) {  // Factorial gets large quickly
        factorial_sum += math_mod.factorial(i);
    }
    if (i <= 30) {  // Fibonacci for reasonable values
        fib_sum += math_mod.fibonacci(i);
    }
}

var math_time = system_time() - math_start;
println("Factorial sum (1-12):");
println(factorial_sum);
println("Fibonacci sum (1-30):");
println(fib_sum);
println("Time for math operations (ms):");
println(math_time);

// Benchmark 3: Module with array operations
println("\n=== Module Array Operations ===");
var array_start = system_time();
var total_sum = 0;

for (var i = 0; i < 1000; i += 1) {
    var arr = math_mod.create_range(0, 100);
    var sum = math_mod.sum_array(arr);
    total_sum += sum;
}

var array_time = system_time() - array_start;
println("Total sum of all array operations:");
println(total_sum);
println("Time for array operations (ms):");
println(array_time);

// Benchmark 4: Module state persistence
println("\n=== Module State Persistence ===");
var state_start = system_time();
var mod1 = create_counter_module(0);
var mod2 = create_counter_module(0); // Should return same instance

for (var i = 0; i < 5000; i += 1) {
    mod1.increment();
    mod2.multiply(2);
    if (mod1.get_value() > 1000000) {
        mod1.reset();
    }
}

var state_time = system_time() - state_start;
println("Final module state:");
println(mod1.get_value());
println("State persistence check (should be same):");
println(mod2.get_value());
println("Time for state operations (ms):");
println(state_time);

var total_time = system_time() - start;
println("\n=== Total Module Benchmark Time ===");
println("Total elapsed (ms):");
println(total_time);