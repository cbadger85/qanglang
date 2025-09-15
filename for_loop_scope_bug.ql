// Reproduce the exact original bug from benchmarks/arrays.ql
// where arr becomes a function instead of an array

fn create_test_array(size, pattern) {
    var arr = [];
    for (var i = 0; i < size; i += 1) {
        if (pattern == "sequential") {
            arr.push(i);
        } else {
            arr.push(i * i);
        }
    }
    return arr;
}

var arrays = [];

// Create multiple arrays like the original benchmark
for (var i = 0; i < 3; i += 1) {
    var size = 10 + (i % 5);
    var pattern = ["sequential", "squares"][i % 2];
    var arr = create_test_array(size, pattern);
    arrays.push(arr);
}

println("Created " + to_string(arrays.length()) + " arrays");

// Reproduce the exact pattern from lines 92-103 of the original
for (var i = 0; i < arrays.length(); i += 1) {
    var arr = arrays[i];
    println("Line 93 equivalent: arr is " + typeof(arr));

    // Forward access - this is where the original bug manifested
    for (var j = 0; j < arr.length(); j += 1) {
        println("Line 100 equivalent: arr is " + typeof(arr));
        // This is line 101 equivalent where arr should be array but becomes function
        if (typeof(arr) != "array") {
            println("ORIGINAL BUG REPRODUCED: arr became " + typeof(arr) + " instead of array");
        }
    }
}