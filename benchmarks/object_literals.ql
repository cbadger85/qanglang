// Object literal operations performance benchmark
// Tests object creation, property access, nested objects, and methods

fn create_person(name, age, city) {
    return {{
        name = name,
        age = age,
        city = city,
        get_info = () -> name + " is " + to_string(age) + " years old and lives in " + city,
        celebrate_birthday = () -> {
            age += 1;
            return age;
        },
        move_to = (new_city) -> {
            city = new_city;
        }
    }};
}

fn create_complex_object(depth, width) {
    if (depth <= 0) {
        return {{ value = depth * width }};
    }

    var obj = {{
        depth = depth,
        width = width,
        children = []
    }};

    for (var i = 0; i < width; i += 1) {
        var child = create_complex_object(depth - 1, width);
        obj.children.push(child);
    }

    obj.sum_children = () -> {
        var total = 0;
        for (var i = 0; i < obj.children.length(); i += 1) {
            if (obj.children[i].value != nil) {
                total += obj.children[i].value;
            } else {
                total += obj.children[i].sum_children();
            }
        }
        return total;
    };

    return obj;
}

var start = system_time();

// Benchmark 1: Simple object creation and property access
println("=== Object Creation Benchmark ===");
var creation_start = system_time();
var objects = [];

for (var i = 0; i < 10000; i += 1) {
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
        var info = person.get_info();
        info_count += 1;
    }
    if (i % 500 == 0) {
        person.celebrate_birthday();
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
    person.move_to(cities[i % cities.length()]);
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

// Create smaller nested structures for performance
for (var i = 0; i < 50; i += 1) {
    var obj = create_complex_object(3, 3); // 3 levels deep, 3 children each
    complex_objects.push(obj);
}

var nested_creation_time = system_time() - nested_start;
println("Complex objects created:");
println(complex_objects.length());
println("Time for nested creation (ms):");
println(nested_creation_time);

// Benchmark 5: Nested object computation
var computation_start = system_time();
var total_sum = 0;

for (var i = 0; i < complex_objects.length(); i += 1) {
    total_sum += complex_objects[i].sum_children();
}

var computation_time = system_time() - computation_start;
println("Total sum from nested computation:");
println(total_sum);
println("Time for nested computation (ms):");
println(computation_time);

// Benchmark 6: Object literal patterns
println("\n=== Object Literal Patterns ===");
var patterns_start = system_time();
var config_objects = [];

for (var i = 0; i < 5000; i += 1) {
    var config = {{
        server = {{
            host = "localhost",
            port = 8000 + i,
            ssl = (i % 2 == 0),
            middlewares = [
                {{ name = "auth", enabled = true }},
                {{ name = "cors", enabled = (i % 3 == 0) }},
                {{ name = "logging", enabled = true }}
            ]
        }},
        database = {{
            type = "postgresql",
            host = "db.example.com",
            port = 5432,
            credentials = {{
                username = "user" + to_string(i),
                password_hash = "hash" + to_string(i * 17)
            }}
        }},
        features = {{
            caching = (i % 4 == 0),
            analytics = true,
            debug = (i < 100)
        }}
    }};

    config_objects.push(config);
}

var patterns_time = system_time() - patterns_start;
println("Configuration objects created:");
println(config_objects.length());
println("Time for pattern creation (ms):");
println(patterns_time);

// Verification: Access nested properties
var verification_start = system_time();
var enabled_count = 0;
var ssl_count = 0;

for (var i = 0; i < config_objects.length(); i += 1) {
    var config = config_objects[i];
    if (config.server.ssl) {
        ssl_count += 1;
    }
    if (config.features.caching) {
        enabled_count += 1;
    }
}

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