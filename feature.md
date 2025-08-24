# Class Field Declarations Implementation

## Overview
This document outlines the implementation plan for class field declarations in QangLang. Field declarations allow defining instance variables directly in the class body with optional initializers.

## Syntax
```javascript
class Person {
    name = "Unknown";     // field with initializer
    age;                  // field without initializer (defaults to nil)
    
    init(name, age) {
        this.name = name; // Constructor can override field values
        this.age = age;
    }
}
```

## Semantics

### Field Initialization Timing
- Fields are initialized at **class declaration time** (evaluated once per class, like methods)
- Field initialization happens **before** the constructor (`init()` method) runs
- Constructors can override field values

### Field Access
- Fields are accessible via `instance.fieldName` syntax (same as constructor-set fields)
- No access control modifiers - all fields are public
- Fields and constructor-set properties share the same namespace

### Field Inheritance
- Fields are inherited **only when `super.init()` is called**
- After calling `super.init()`, superclass fields become available on the subclass instance
- Subclass fields are initialized before `super.init()` is called

### Default Values
- Fields without initializers default to `nil`
- Field initializers are evaluated once per class (not per instance)
- Field values are copied to each instance during instantiation

## Implementation Plan

### 1. AST Changes (✅ Already Complete)
The AST already supports field declarations:
- `FieldDecl` struct exists with `name`, `initializer`, and `span`
- `ClassMember::Field(FieldDecl)` variant is defined
- Parser already handles field declarations in `field_declaration()` method

### 2. Compiler Changes

#### 2.1 New OpCode
Add a new bytecode instruction to handle field initialization:
- `OpCode::InitField` - Initializes a field on a class with a given value

#### 2.2 Class Compilation Enhancement
Modify `visit_class_declaration` in `compiler.rs`:
- During class compilation, collect all field declarations
- For each field with an initializer:
  - Compile the initializer expression
  - Emit `OpCode::InitField` with field name
- For fields without initializers:
  - Emit `OpCode::InitField` with `nil` value

#### 2.3 Field Storage in Classes
Fields will be stored in the class's method table alongside methods, but with a special prefix or marker to distinguish them from methods.

### 3. VM Changes

#### 3.1 OpCode Implementation
Add `OpCode::InitField` handling in the VM's run loop:
- Pop the field value from the stack
- Read the field name constant
- Store the field in the class's table (similar to how methods are stored)

#### 3.2 Instance Creation Enhancement
Modify class instantiation (`Value::Class` handling in `call_value`):
- When creating an instance, copy all fields from the class to the instance
- This happens before the constructor is called

#### 3.3 Super Constructor Enhancement
Modify `super.init()` handling (`OpCode::SuperInvoke` for `init`):
- After calling super constructor, copy superclass fields to the current instance
- Ensure subclass fields don't override already-copied superclass fields

### 4. Memory Management
- Fields are stored as key-value pairs in the class's table
- Field values are copied to instance tables during instantiation
- No special memory management needed - existing GC handles field values

### 5. Compilation Order
1. **Class declaration starts**
2. **Field declarations processed** (initializers compiled and stored)
3. **Method declarations processed**
4. **Class finalized**

During instantiation:
1. **Instance created**
2. **Class fields copied to instance**
3. **Constructor called** (if exists)

During inheritance with `super.init()`:
1. **Subclass instance created**
2. **Subclass fields copied to instance**
3. **`super.init()` called**
4. **Superclass fields copied to instance** (after super constructor runs)

### 6. Testing Strategy

#### 6.1 Basic Field Declaration Tests
```javascript
// Test 1: Field with initializer
class A {
    x = 42;
}
assert_eq(A().x, 42);

// Test 2: Field without initializer
class B {
    y;
}
assert_eq(B().y, nil);

// Test 3: Multiple fields
class C {
    a = "hello";
    b = 123;
    c;
}
var c = C();
assert_eq(c.a, "hello");
assert_eq(c.b, 123);
assert_eq(c.c, nil);
```

#### 6.2 Constructor Interaction Tests
```javascript
// Test 4: Constructor overrides field
class D {
    name = "default";
    
    init(name) {
        this.name = name;
    }
}
assert_eq(D("custom").name, "custom");

// Test 5: Constructor accesses field
class E {
    greeting = "Hello";
    
    init(name) {
        this.message = this.greeting + " " + name;
    }
}
assert_eq(E("World").message, "Hello World");
```

#### 6.3 Inheritance Tests
```javascript
// Test 6: Simple inheritance
class Parent {
    x = 10;
}
class Child : Parent {
    y = 20;
    
    init() {
        super.init();
    }
}
var child = Child();
assert_eq(child.x, 10); // inherited field
assert_eq(child.y, 20); // own field

// Test 7: No super.init() call - no inherited fields
class Parent2 {
    x = 10;
}
class Child2 : Parent2 {
    y = 20;
}
var child2 = Child2();
assert_eq(child2.x, nil); // not inherited
assert_eq(child2.y, 20);  // own field

// Test 8: Field shadowing
class Parent3 {
    x = "parent";
}
class Child3 : Parent3 {
    x = "child";
    
    init() {
        super.init();
    }
}
var child3 = Child3();
assert_eq(child3.x, "child"); // child field wins
```

#### 6.4 Complex Expression Tests
```javascript
// Test 9: Field with complex initializer
class F {
    computed = 2 + 3 * 4;
    str = "hello " + "world";
}
var f = F();
assert_eq(f.computed, 14);
assert_eq(f.str, "hello world");

// Test 10: Field initializer using function calls
class G {
    time = system_time();
    type_name = typeof(42);
}
var g = G();
assert_eq(typeof(g.time), "number");
assert_eq(g.type_name, "number");
```

#### 6.5 Error Cases Tests
```javascript
// Test 11: Field name collision with methods (should work)
class H {
    getValue = "field_value";
    
    getValue() {
        return "method_value";
    }
}
// Method should override field in property access
assert_eq(H().getValue(), "method_value");
```

## Implementation Files to Modify

### Core Files
1. **`crates/qanglang-core/src/chunk.rs`** - Add `OpCode::InitField`
2. **`crates/qanglang-core/src/compiler.rs`** - Add field compilation logic
3. **`crates/qanglang-core/src/vm.rs`** - Add field initialization and instance copying logic

### Test Files
4. **`crates/qanglang-core/src/tests/vm_tests.rs`** - Add comprehensive field declaration tests

## Potential Challenges

1. **Field vs Method Resolution**: Ensure methods override fields when both have the same name
2. **Inheritance Timing**: Carefully manage when superclass fields are copied during `super.init()`
3. **Memory Efficiency**: Consider memory usage of copying all fields to every instance
4. **Initialization Order**: Ensure fields are initialized in declaration order

## Future Considerations

1. **Static Fields**: Could extend to support class-level static fields
2. **Access Modifiers**: Could add private/protected field support
3. **Field Decorators**: Could add metadata or validation decorators
4. **Computed Properties**: Could support getter/setter syntax

This implementation maintains backward compatibility while adding the requested field declaration functionality to QangLang classes.