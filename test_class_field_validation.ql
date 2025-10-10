// Valid class with constant field initializers
class Person {
    name = "Unknown";
    age = 0;
    isActive = true;
    notes = nil;
}

var p = Person();
print(p.name);  // Should print "Unknown"
print(p.age);   // Should print 0

// Invalid - this will cause a compilation error:
// class Invalid {
//     x = 1 + 2;  // Error: not a constant
// }

// Invalid - this will cause a compilation error:
// class Invalid2 {
//     y = someVariable;  // Error: not a constant
// }

// Invalid - this will cause a compilation error:
// class Invalid3 {
//     z = getValue();  // Error: not a constant
// }
