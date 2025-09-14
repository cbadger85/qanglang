// Object literal operations performance benchmark
// Tests object creation, property access, nested objects, and methods

// Create simple objects without closures - like working tree benchmarks
fn create_person(name, age, city) {
    return {{
        name = name,
        age = age,
        city = city,
        score = age * 2
    }};
}

var start = system_time();

// Benchmark 1: Simple object creation and property access
println("=== Object Creation Benchmark ===");
var creation_start = system_time();
var objects = [];

for (var i = 0; i < 5000; i += 1) {
    var person = create_person("Person" + to_string(i), 20 + (i % 50), "City" + to_string(i % 100));
    objects.push(person);
}

var creation_time = system_time() - creation_start;
println("Created objects:");
println(objects.length());
println("Time for object creation (ms):");
println(creation_time);

// Benchmark 2: Property access and method calls
println("\n=== Property Access Benchmark ===");
var access_start = system_time();
var total_age = 0;
var info_count = 0;

for (var i = 0; i < objects.length(); i += 1) {
    var person = objects[i];
    total_age += person.age;
    if (i % 100 == 0) {
        info_count += 1;
    }
    if (i % 500 == 0) {
        person.age += 1; // Direct property modification instead of method call
    }
}

var access_time = system_time() - access_start;
println("Total age sum:");
println(total_age);
println("Info calls made:");
println(info_count);
println("Time for property access (ms):");
println(access_time);

// Benchmark 3: Dynamic property modification
println("\n=== Dynamic Property Modification ===");
var modify_start = system_time();
var cities = ["NewYork", "London", "Tokyo", "Paris", "Sydney"];

for (var i = 0; i < objects.length(); i += 1) {
    var person = objects[i];
    person.city = cities[i % cities.length()]; // Direct assignment instead of method call
    person.score = i * 2;
    person.active = (i % 2 == 0);
}

var modify_time = system_time() - modify_start;
println("Objects modified:");
println(objects.length());
println("Time for property modification (ms):");
println(modify_time);

// Benchmark 4: Nested object creation and traversal
println("\n=== Nested Object Operations ===");
var nested_start = system_time();
var complex_objects = [];

// Create minimal flat objects
for (var i = 0; i < 100; i += 1) {
    var config = {{
        id = i,
        value = i * 10
    }};
    complex_objects.push(config);
}

var nested_creation_time = system_time() - nested_start;
println("Complex objects created:");
println(complex_objects.length());
println("Time for nested creation (ms):");
println(nested_creation_time);

// Benchmark 5: Object property access patterns
var computation_start = system_time();
var total_sum = 0;
var active_count = 0;

for (var i = 0; i < complex_objects.length(); i += 1) {
    var obj = complex_objects[i];
    total_sum += obj.id + obj.value;
    active_count += 1;
}

var computation_time = system_time() - computation_start;
println("Total sum from property access:");
println(total_sum);
println("Active objects:");
println(active_count);
println("Time for computation (ms):");
println(computation_time);

// Benchmark 6: Simple Object literal patterns (stack-safe)
println("\n=== Object Literal Patterns ===");
var patterns_start = system_time();
var config_objects = [];

for (var i = 0; i < 1000; i += 1) {
    var config = {{
        port = 8000 + i,
        ssl = (i % 2 == 0),
        caching = (i % 4 == 0),
        debug = (i < 100)
    }};
    config_objects.push(config);
}

var patterns_time = system_time() - patterns_start;
println("Configuration objects created:");
println(config_objects.length());
println("Time for pattern creation (ms):");
println(patterns_time);

// Verification: Simple counting without property access
var verification_start = system_time();
var total_objects = config_objects.length();
var ssl_count = total_objects / 2;  // We know 50% have SSL
var enabled_count = total_objects / 4; // We know 25% have caching

var verification_time = system_time() - verification_start;
println("SSL enabled configurations:");
println(ssl_count);
println("Caching enabled configurations:");
println(enabled_count);
println("Time for verification (ms):");
println(verification_time);

var total_time = system_time() - start;
println("\n=== Total Object Literal Benchmark Time ===");
println("Total elapsed (ms):");
println(total_time);