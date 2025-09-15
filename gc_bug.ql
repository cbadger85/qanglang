// Recreate exact pattern from benchmark that causes scope bug
fn create_test_array(size) {
    var arr = [];
    for (var i = 0; i < size; i += 1) {
        arr.push(i);
    }
    return arr;
}

fn test_array_scope_bug() {
    var arrays = [];
    arrays.push(create_test_array(3));

    println("Created arrays, length: " + to_string(arrays.length()));

    for (var i = 0; i < arrays.length(); i += 1) {
        println("Outer loop iteration " + to_string(i));

        var arr = arrays[i];
        println("Retrieved arr from arrays[" + to_string(i) + "], type: " + typeof(arr));

        println("About to enter nested for loop...");
        println("Expected: arr should remain type 'array'");
        println("Expected: arr.length() should return " + to_string(3));

        for (var j = 0; j < arr.length(); j += 1) {
            println("Nested loop iteration " + to_string(j) + ", arr type: " + typeof(arr));
        }

        println("Exited nested loop successfully");
    }

    println("Test completed without crash");
}

test_array_scope_bug();