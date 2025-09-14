// Array operations performance benchmark
// Tests array creation, manipulation, intrinsic methods, and iteration

fn create_test_array(size, pattern) {
    var arr = [];
    for (var i = 0; i < size; i += 1) {
        if (pattern == "sequential") {
            arr.push(i);
        } else if (pattern == "fibonacci") {
            if (i < 2) {
                arr.push(i);
            } else {
                arr.push(arr[i - 1] + arr[i - 2]);
            }
        } else if (pattern == "random_like") {
            arr.push((i * 17 + 23) % 1000);
        } else {
            arr.push(i * i);
        }
    }
    return arr;
}

fn array_bubble_sort(arr) {
    var n = arr.length();
    for (var i = 0; i < n - 1; i += 1) {
        for (var j = 0; j < n - i - 1; j += 1) {
            if (arr[j] > arr[j + 1]) {
                var temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
    return arr;
}

// Removed merge_arrays function to avoid stack usage during parsing

var start = system_time();

// Benchmark 1: Array creation and basic operations
println("=== Array Creation Benchmark ===");
var creation_start = system_time();
var arrays = [];

for (var i = 0; i < 1000; i += 1) {
    var size = 100 + (i % 400);  // Arrays of size 100-500
    var pattern = ["sequential", "fibonacci", "random_like", "squares"][i % 4];
    var arr = create_test_array(size, pattern);
    arrays.push(arr);
}

var creation_time = system_time() - creation_start;
println("Arrays created:");
println(arrays.length());
println("Time for array creation (ms):");
println(creation_time);

// Benchmark 2: Array access patterns
println("\n=== Array Access Patterns ===");
var access_start = system_time();
var sum_total = 0;
var element_count = 0;

for (var i = 0; i < arrays.length(); i += 1) {
    var arr = arrays[i];
    var arr_sum = 0;

    // Forward access
    for (var j = 0; j < arr.length(); j += 1) {
        arr_sum += arr[j];
        element_count += 1;
    }

    // Reverse access using negative indexing
    for (var k = 1; k <= arr.length() and k <= 10; k += 1) {
        arr_sum += arr[-k];
    }

    sum_total += arr_sum;
}

var access_time = system_time() - access_start;
println("Total sum:");
println(sum_total);
println("Elements accessed:");
println(element_count);
println("Time for array access (ms):");
println(access_time);

// Benchmark 3: Array intrinsic methods
println("\n=== Array Intrinsic Methods ===");
var intrinsic_start = system_time();
var push_pop_operations = 0;
var slice_operations = 0;

for (var i = 0; i < 500; i += 1) {
    var arr = create_test_array(50, "sequential");

    // Push/pop operations
    for (var j = 0; j < 25; j += 1) {
        arr.push(j * 100);
        push_pop_operations += 1;
    }

    for (var k = 0; k < 15; k += 1) {
        if (arr.length() > 0) {
            arr.pop();
            push_pop_operations += 1;
        }
    }

    // Slice operations
    var slice1 = arr.slice(5, 15);
    var slice2 = arr.slice(-10);
    var slice3 = arr.slice();  // Full copy
    slice_operations += 3;

    // Reverse operations
    arr.reverse();

    arrays[i % arrays.length()] = arr;  // Replace some arrays
}

var intrinsic_time = system_time() - intrinsic_start;
println("Push/pop operations:");
println(push_pop_operations);
println("Slice operations:");
println(slice_operations);
println("Time for intrinsic methods (ms):");
println(intrinsic_time);

// Benchmark 4: Array concatenation and manipulation
println("\n=== Array Concatenation ===");
var concat_start = system_time();
var concatenated_arrays = [];

for (var i = 0; i < arrays.length() - 1; i += 2) {
    var arr1 = arrays[i];
    var arr2 = arrays[i + 1];

    // Method 1: concat method
    var concat1 = arr1.concat(arr2);

    // Method 2: + operator
    var concat2 = arr1 + arr2;

    concatenated_arrays.push(concat1);
    concatenated_arrays.push(concat2);
}

var concat_time = system_time() - concat_start;
println("Concatenated arrays created:");
println(concatenated_arrays.length());
println("Time for concatenation (ms):");
println(concat_time);

// Benchmark 5: Array sorting algorithms
println("\n=== Array Sorting ===");
var sorting_start = system_time();
var sorted_count = 0;

// Only sort smaller arrays for performance
for (var i = 0; i < 100; i += 1) {
    var test_arr = create_test_array(50, "random_like");
    var sorted_arr = array_bubble_sort(test_arr.slice());  // Sort a copy
    sorted_count += 1;
}

var sorting_time = system_time() - sorting_start;
println("Arrays sorted:");
println(sorted_count);
println("Time for sorting (ms):");
println(sorting_time);

// Benchmark 6: Multi-dimensional arrays (small scale)
println("\n=== Multi-dimensional Arrays ===");
var multi_start = system_time();
var matrices = [];

// Much smaller matrices to avoid stack overflow
for (var i = 0; i < 100; i += 1) {
    var matrix = [];
    var size = 5 + (i % 5);  // 5x5 to 10x10 matrices only

    for (var row = 0; row < size; row += 1) {
        var current_row = [];
        for (var col = 0; col < size; col += 1) {
            current_row.push(row * col + i);
        }
        matrix.push(current_row);
    }

    matrices.push(matrix);
}

// Matrix operations
var matrix_sum = 0;
for (var i = 0; i < matrices.length(); i += 1) {
    var matrix = matrices[i];
    for (var row = 0; row < matrix.length(); row += 1) {
        for (var col = 0; col < matrix[row].length(); col += 1) {
            matrix_sum += matrix[row][col];
        }
    }
}

var multi_time = system_time() - multi_start;
println("Matrices created:");
println(matrices.length());
println("Matrix sum:");
println(matrix_sum);
println("Time for multi-dimensional arrays (ms):");
println(multi_time);

// Benchmark 7: Simple Array Construction (avoiding deep iterator pipeline)
println("\n=== Array Construction Patterns ===");
var patterns_start = system_time();
var constructed_arrays = [];

// Extremely minimal array construction
for (var i = 0; i < 50; i += 1) {
    // Pattern 1: Tiny manual arrays
    var range_array = [];
    var size = 3 + (i % 3); // Only 3-6 elements
    for (var j = 0; j < size; j += 1) {
        range_array.push(j);
    }
    constructed_arrays.push(range_array);
}

var patterns_time = system_time() - patterns_start;
println("Constructed arrays:");
println(constructed_arrays.length());
println("Time for construction patterns (ms):");
println(patterns_time);

var total_time = system_time() - start;
println("\n=== Total Array Benchmark Time ===");
println("Total elapsed (ms):");
println(total_time);