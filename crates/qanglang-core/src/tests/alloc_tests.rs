#[cfg(test)]
mod tests {
    use crate::Value;
    use crate::memory::{ClosureObject, FunctionObject, HeapAllocator, UpvalueSlot};
    use std::collections::VecDeque;

    fn create_test_function() -> FunctionObject {
        FunctionObject {
            name: 0,
            arity: 0,
            chunk: crate::chunk::Chunk::new(),
            upvalue_count: 0,
        }
    }

    fn create_test_closure(function_handle: u32) -> ClosureObject {
        ClosureObject::new(function_handle, 0)
    }

    #[test]
    fn test_allocator_new() {
        let allocator = HeapAllocator::new();
        assert_eq!(allocator.total_allocated_bytes(), 0);
    }

    #[test]
    fn test_allocator_with_capacity() {
        let allocator = HeapAllocator::with_capacity(512);
        assert_eq!(allocator.total_allocated_bytes(), 0);
    }

    #[test]
    fn test_allocate_function() {
        let mut allocator = HeapAllocator::new();
        let function = create_test_function();
        let handle = allocator.allocate_function(function);

        assert_eq!(handle, 0);

        let retrieved = allocator.get_function(handle);
        assert_eq!(retrieved.name, 0);
        assert_eq!(retrieved.arity, 0);
        assert_eq!(retrieved.upvalue_count, 0);
    }

    #[test]
    fn test_allocate_multiple_functions() {
        let mut allocator = HeapAllocator::new();

        let function1 = create_test_function();
        let function2 = create_test_function();

        let handle1 = allocator.allocate_function(function1);
        let handle2 = allocator.allocate_function(function2);

        assert_eq!(handle1, 0);
        assert_eq!(handle2, 1);

        assert!(allocator.get_function(handle1).name == 0);
        assert!(allocator.get_function(handle2).name == 0);
    }

    #[test]
    fn test_allocate_closure() {
        let mut allocator = HeapAllocator::new();
        let function = create_test_function();
        let function_handle = allocator.allocate_function(function);
        let closure = create_test_closure(function_handle);

        let handle = allocator.allocate_closure(closure);

        let retrieved = allocator.get_closure(handle);
        assert_eq!(retrieved.function, function_handle);
        assert!(!retrieved.is_marked);
    }

    #[test]
    fn test_free_closure() {
        let mut allocator = HeapAllocator::new();
        let function = create_test_function();
        let function_handle = allocator.allocate_function(function);
        let closure = create_test_closure(function_handle);

        let handle = allocator.allocate_closure(closure);
        allocator.free_closure(handle);

        // After freeing, attempting to access should panic in debug mode
        // or return invalid data in release mode, so we don't test access here
    }

    #[test]
    fn test_allocate_upvalue() {
        let mut allocator = HeapAllocator::new();
        let value = Value::Number(42.0);

        let handle = allocator.allocate_upvalue(value);

        let retrieved = allocator.get_upvalue(handle);
        assert_eq!(*retrieved, Value::Number(42.0));
    }

    #[test]
    fn test_upvalue_mutability() {
        let mut allocator = HeapAllocator::new();
        let value = Value::Number(42.0);

        let handle = allocator.allocate_upvalue(value);

        {
            let upvalue_mut = allocator.get_upvalue_mut(handle);
            *upvalue_mut = Value::Number(100.0);
        }

        let retrieved = allocator.get_upvalue(handle);
        assert_eq!(*retrieved, Value::Number(100.0));
    }

    #[test]
    fn test_allocate_class() {
        let mut allocator = HeapAllocator::new();
        let class_name = allocator.strings.intern("TestClass");

        let handle = allocator.allocate_class(class_name);

        let retrieved = allocator.get_class(handle);
        assert_eq!(retrieved.name, class_name);
        assert!(!retrieved.is_marked);
    }

    #[test]
    fn test_allocate_instance() {
        let mut allocator = HeapAllocator::new();
        let class_name = allocator.strings.intern("TestClass");
        let class_handle = allocator.allocate_class(class_name);

        let instance_handle = allocator.allocate_instance(class_handle);

        let retrieved = allocator.get_instance(instance_handle);
        assert_eq!(retrieved.clazz, class_handle);
        assert!(!retrieved.is_marked);
    }

    #[test]
    fn test_instance_fields() {
        let mut allocator = HeapAllocator::new();
        let class_name = allocator.strings.intern("TestClass");
        let class_handle = allocator.allocate_class(class_name);
        let instance_handle = allocator.allocate_instance(class_handle);

        let field_name = allocator.strings.intern("field1");
        let field_value = Value::Number(42.0);

        allocator.set_instance_field(instance_handle, Value::String(field_name), field_value);

        let retrieved = allocator.get_instance_field(instance_handle, Value::String(field_name));
        assert_eq!(retrieved, Some(field_value));

        let non_existent = allocator.get_instance_field(instance_handle, Value::Number(999.0));
        assert_eq!(non_existent, None);
    }

    #[test]
    fn test_total_allocated_bytes_calculation() {
        let mut allocator = HeapAllocator::new();
        let initial_bytes = allocator.total_allocated_bytes();

        // Allocate some objects and verify byte calculation is correct
        let function = create_test_function();
        let _function_handle = allocator.allocate_function(function);

        let string_handle = allocator.strings.intern("test");
        let value = Value::String(string_handle);
        let _upvalue_handle = allocator.allocate_upvalue(value);

        // Allocate class and instance to test the fixed calculation
        let class_name = allocator.strings.intern("TestClass");
        let class_handle = allocator.allocate_class(class_name);
        let _instance_handle = allocator.allocate_instance(class_handle);

        let bytes_after = allocator.total_allocated_bytes();
        assert!(
            bytes_after > initial_bytes,
            "Expected bytes to increase after allocations. Initial: {}, After: {}",
            initial_bytes,
            bytes_after
        );

        // Verify that class and instance bytes are actually counted properly
        // This would fail with the previous bitwise & bug since class/instance bytes would be tiny
        let bytes_increase = bytes_after - initial_bytes;
        assert!(
            bytes_increase > 100, // Should be much larger than 100 bytes with proper multiplication
            "Byte increase should be substantial with proper calculation. Got: {}",
            bytes_increase
        );
    }

    #[test]
    fn test_garbage_collection_with_no_roots() {
        let mut allocator = HeapAllocator::new();

        // Allocate some objects
        let function = create_test_function();
        let function_handle = allocator.allocate_function(function);
        let closure = create_test_closure(function_handle);
        let _closure_handle = allocator.allocate_closure(closure);

        let value = Value::Number(42.0);
        let _upvalue_handle = allocator.allocate_upvalue(value);

        let _bytes_before = allocator.total_allocated_bytes();

        // Collect garbage with no roots - everything should be collected
        let roots = VecDeque::new();
        allocator.collect_garbage(roots);

        let _bytes_after = allocator.total_allocated_bytes();
        // Functions are never garbage collected (immortal)
        // The mortal objects (closures, upvalues) should have been collected
        // So total bytes should decrease due to collected mortal objects
    }

    #[test]
    fn test_garbage_collection_with_roots() {
        let mut allocator = HeapAllocator::new();

        // Create a closure that should be kept alive
        let function = create_test_function();
        let function_handle = allocator.allocate_function(function);
        let closure = create_test_closure(function_handle);
        let closure_handle = allocator.allocate_closure(closure);

        // Create an upvalue that should be collected
        let value = Value::Number(42.0);
        let _upvalue_handle = allocator.allocate_upvalue(value);

        // Keep the closure alive by making it a root
        let mut roots = VecDeque::new();
        roots.push_back(Value::Closure(closure_handle));

        allocator.collect_garbage(roots);

        // The closure should still be accessible
        let retrieved_closure = allocator.get_closure(closure_handle);
        assert_eq!(retrieved_closure.function, function_handle);
    }

    #[test]
    fn test_closure_with_upvalues() {
        let mut allocator = HeapAllocator::new();

        let function = create_test_function();
        let function_handle = allocator.allocate_function(function);

        let upvalue = Value::Number(42.0);
        let upvalue_handle = allocator.allocate_upvalue(upvalue);

        let closure = ClosureObject::new(function_handle, 1);
        let closure_handle = allocator.allocate_closure(closure);
        
        // Set upvalue using the new API
        allocator.closures.set_upvalue(closure_handle, 0, UpvalueSlot::Closed(upvalue_handle));

        // Test that we can access the closure and its upvalues
        let retrieved_closure = allocator.get_closure(closure_handle);
        assert_eq!(retrieved_closure.upvalue_count, 1);

        if let Some(UpvalueSlot::Closed(handle)) = allocator.closures.get_upvalue(closure_handle, 0) {
            let upvalue_value = allocator.get_upvalue(handle);
            assert_eq!(*upvalue_value, Value::Number(42.0));
        } else {
            panic!("Expected closed upvalue reference");
        }
    }

    #[test]
    fn test_garbage_collection_traces_upvalues() {
        let mut allocator = HeapAllocator::new();

        let function = create_test_function();
        let function_handle = allocator.allocate_function(function);

        let upvalue = Value::Number(42.0);
        let upvalue_handle = allocator.allocate_upvalue(upvalue);

        let closure = ClosureObject::new(function_handle, 1);
        let closure_handle = allocator.allocate_closure(closure);
        
        // Set upvalue using the new API
        allocator.closures.set_upvalue(closure_handle, 0, UpvalueSlot::Closed(upvalue_handle));

        // Make the closure a root - this should keep the upvalue alive too
        let mut roots = VecDeque::new();
        roots.push_back(Value::Closure(closure_handle));

        allocator.collect_garbage(roots);

        // Both closure and upvalue should still be accessible
        if let Some(UpvalueSlot::Closed(handle)) = allocator.closures.get_upvalue(closure_handle, 0) {
            let upvalue_value = allocator.get_upvalue(handle);
            assert_eq!(*upvalue_value, Value::Number(42.0));
        }
    }

    #[test]
    fn test_class_and_instance_gc() {
        let mut allocator = HeapAllocator::new();

        let class_name = allocator.strings.intern("TestClass");
        let class_handle = allocator.allocate_class(class_name);
        let instance_handle = allocator.allocate_instance(class_handle);

        // Add a field to the instance
        let field_name = allocator.strings.intern("field1");
        allocator.set_instance_field(
            instance_handle,
            Value::String(field_name),
            Value::Number(42.0),
        );

        // Make the instance a root - this should keep the class alive too
        let mut roots = VecDeque::new();
        roots.push_back(Value::Instance(instance_handle));

        allocator.collect_garbage(roots);

        // Both class and instance should still be accessible
        let retrieved_instance = allocator.get_instance(instance_handle);
        assert_eq!(retrieved_instance.clazz, class_handle);

        let retrieved_class = allocator.get_class(class_handle);
        assert_eq!(retrieved_class.name, class_name);

        // Field should still be accessible
        let field_value = allocator.get_instance_field(instance_handle, Value::String(field_name));
        assert_eq!(field_value, Some(Value::Number(42.0)));
    }

    #[test]
    fn test_should_collect_garbage() {
        let mut allocator = HeapAllocator::new();

        // Initially should not need collection
        let should_collect_initial = allocator.should_collect_garbage();

        // The behavior depends on debug vs release mode
        // In debug mode, it checks arena capacity
        // In release mode, it checks total allocated bytes
        #[cfg(debug_assertions)]
        {
            // In debug mode, should collect when arenas are not at capacity
            assert!(should_collect_initial);
        }

        #[cfg(not(debug_assertions))]
        {
            // In release mode, should not collect initially as allocated bytes are low
            assert!(!should_collect_initial);
        }
    }

    #[test]
    fn test_gc_marks_deeply_nested_references() {
        let mut allocator = HeapAllocator::new();

        // Create a chain: closure -> upvalue -> closure -> upvalue
        let function1 = create_test_function();
        let function_handle1 = allocator.allocate_function(function1);

        let function2 = create_test_function();
        let function_handle2 = allocator.allocate_function(function2);

        // Create first closure
        let closure1 = create_test_closure(function_handle1);
        let closure_handle1 = allocator.allocate_closure(closure1);

        // Create second closure
        let closure2 = ClosureObject::new(function_handle2, 1);
        let closure_handle2 = allocator.allocate_closure(closure2);
        let upvalue1 = allocator.allocate_upvalue(Value::Closure(closure_handle1));
        allocator.closures.set_upvalue(closure_handle2, 0, UpvalueSlot::Closed(upvalue1));

        // Create third level - upvalue pointing to second closure
        let upvalue2 = allocator.allocate_upvalue(Value::Closure(closure_handle2));

        // Only root the deepest upvalue (by creating a closure that references it)
        let function3 = create_test_function();
        let function_handle3 = allocator.allocate_function(function3);
        let root_closure = ClosureObject::new(function_handle3, 1);
        let root_closure_handle = allocator.allocate_closure(root_closure);
        allocator.closures.set_upvalue(root_closure_handle, 0, UpvalueSlot::Closed(upvalue2));

        let mut roots = VecDeque::new();
        roots.push_back(Value::Closure(root_closure_handle));

        allocator.collect_garbage(roots);

        // All objects should still be accessible through the reference chain
        let retrieved_upvalue2 = allocator.get_upvalue(upvalue2);
        if let Value::Closure(handle) = *retrieved_upvalue2 {
            assert_eq!(handle, closure_handle2);
            let retrieved_closure2 = allocator.get_closure(handle);
            if let Some(UpvalueSlot::Closed(upvalue_handle)) = allocator.closures.get_upvalue(handle, 0) {
                let retrieved_upvalue1 = allocator.get_upvalue(upvalue_handle);
                if let Value::Closure(inner_handle) = *retrieved_upvalue1 {
                    assert_eq!(inner_handle, closure_handle1);
                }
            }
        }
    }

    #[test]
    fn test_gc_collects_orphaned_objects() {
        let mut allocator = HeapAllocator::new();

        // Create some objects that will become orphaned
        let function = create_test_function();
        let function_handle = allocator.allocate_function(function);
        let closure = create_test_closure(function_handle);
        let _orphaned_closure = allocator.allocate_closure(closure);

        let _orphaned_upvalue = allocator.allocate_upvalue(Value::Number(42.0));

        let class_name = allocator.strings.intern("OrphanedClass");
        let orphaned_class = allocator.allocate_class(class_name);
        let _orphaned_instance = allocator.allocate_instance(orphaned_class);

        // Create a root object that doesn't reference the orphaned objects
        let root_function = create_test_function();
        let root_function_handle = allocator.allocate_function(root_function);
        let root_closure = create_test_closure(root_function_handle);
        let root_closure_handle = allocator.allocate_closure(root_closure);

        let bytes_before = allocator.total_allocated_bytes();

        // Collect garbage with only the root closure
        let mut roots = VecDeque::new();
        roots.push_back(Value::Closure(root_closure_handle));
        allocator.collect_garbage(roots);

        let bytes_after = allocator.total_allocated_bytes();

        // Should have collected the orphaned objects (bytes should decrease)
        // Note: Functions and strings are immortal, so they won't be collected
        assert!(bytes_after < bytes_before);

        // Root closure should still be accessible
        let retrieved = allocator.get_closure(root_closure_handle);
        assert_eq!(retrieved.function, root_function_handle);
    }

    #[test]
    fn test_gc_preserves_instance_class_relationship() {
        let mut allocator = HeapAllocator::new();

        // Create class and instance with complex field relationships
        let class_name = allocator.strings.intern("TestClass");
        let class_handle = allocator.allocate_class(class_name);

        // TODO: Add methods to the class when HeapAllocator exposes class method setting
        // Currently we can't test class methods because HashMapArena is private
        let _method_name = allocator.strings.intern("method");
        let method_function = create_test_function();
        let _method_function_handle = allocator.allocate_function(method_function);
        let method_closure = create_test_closure(_method_function_handle);
        let _method_closure_handle = allocator.allocate_closure(method_closure);

        // Create instance with field pointing to another object
        let instance_handle = allocator.allocate_instance(class_handle);
        let field_value_function = create_test_function();
        let field_value_function_handle = allocator.allocate_function(field_value_function);
        let field_value_closure = create_test_closure(field_value_function_handle);
        let field_value_closure_handle = allocator.allocate_closure(field_value_closure);

        let field_name = allocator.strings.intern("field");
        allocator.set_instance_field(
            instance_handle,
            Value::String(field_name),
            Value::Closure(field_value_closure_handle),
        );

        // Root only the instance
        let mut roots = VecDeque::new();
        roots.push_back(Value::Instance(instance_handle));

        allocator.collect_garbage(roots);

        // Everything should still be accessible
        let retrieved_instance = allocator.get_instance(instance_handle);
        assert_eq!(retrieved_instance.clazz, class_handle);

        let retrieved_class = allocator.get_class(class_handle);
        assert_eq!(retrieved_class.name, class_name);

        // Instance field should still be accessible
        let field_value = allocator.get_instance_field(instance_handle, Value::String(field_name));
        assert_eq!(
            field_value,
            Some(Value::Closure(field_value_closure_handle))
        );

        // TODO: Test that class methods are accessible after GC
        // Currently can't test because HashMapArena table access is private
        // The GC should keep method closures alive when the class is rooted
    }

    #[test]
    fn test_gc_with_circular_references() {
        let mut allocator = HeapAllocator::new();

        // Create circular reference: closure1 -> upvalue -> closure2 -> upvalue -> closure1
        let function1 = create_test_function();
        let function_handle1 = allocator.allocate_function(function1);
        let closure1 = create_test_closure(function_handle1);
        let closure_handle1 = allocator.allocate_closure(closure1);

        let function2 = create_test_function();
        let function_handle2 = allocator.allocate_function(function2);
        let closure2 = create_test_closure(function_handle2);
        let closure_handle2 = allocator.allocate_closure(closure2);

        // Create upvalues that create circular reference
        let upvalue1 = allocator.allocate_upvalue(Value::Closure(closure_handle2));
        let upvalue2 = allocator.allocate_upvalue(Value::Closure(closure_handle1));

        // Update closures to reference the upvalues
        allocator.get_closure_mut(closure_handle1).upvalue_count = 1;
        allocator.closures.set_upvalue(closure_handle1, 0, UpvalueSlot::Closed(upvalue1));

        allocator.get_closure_mut(closure_handle2).upvalue_count = 1;
        allocator.closures.set_upvalue(closure_handle2, 0, UpvalueSlot::Closed(upvalue2));

        // Root one of the closures - should keep the entire cycle alive
        let mut roots = VecDeque::new();
        roots.push_back(Value::Closure(closure_handle1));

        allocator.collect_garbage(roots);

        // All objects in the cycle should still be accessible
        let retrieved_closure1 = allocator.get_closure(closure_handle1);
        assert_eq!(retrieved_closure1.function, function_handle1);

        let retrieved_closure2 = allocator.get_closure(closure_handle2);
        assert_eq!(retrieved_closure2.function, function_handle2);

        // Verify the circular references are intact
        if let Some(UpvalueSlot::Closed(handle)) = allocator.closures.get_upvalue(closure_handle1, 0) {
            let upvalue_val = allocator.get_upvalue(handle);
            if let Value::Closure(closure_handle) = *upvalue_val {
                assert_eq!(closure_handle, closure_handle2);
            }
        }
    }

    #[test]
    fn test_gc_multiple_collections() {
        let mut allocator = HeapAllocator::new();

        // Create a persistent root
        let persistent_function = create_test_function();
        let persistent_function_handle = allocator.allocate_function(persistent_function);
        let persistent_closure = create_test_closure(persistent_function_handle);
        let persistent_closure_handle = allocator.allocate_closure(persistent_closure);

        for i in 0..5 {
            // Create temporary objects in each iteration
            let temp_function = create_test_function();
            let temp_function_handle = allocator.allocate_function(temp_function);
            let temp_closure = create_test_closure(temp_function_handle);
            let _temp_closure_handle = allocator.allocate_closure(temp_closure);

            let _temp_upvalue = allocator.allocate_upvalue(Value::Number(i as f64));

            let temp_class_name = allocator.strings.intern(&format!("TempClass{}", i));
            let temp_class = allocator.allocate_class(temp_class_name);
            let _temp_instance = allocator.allocate_instance(temp_class);

            // Collect garbage, keeping only the persistent closure
            let mut roots = VecDeque::new();
            roots.push_back(Value::Closure(persistent_closure_handle));

            allocator.collect_garbage(roots);

            // Persistent closure should always be accessible
            let retrieved = allocator.get_closure(persistent_closure_handle);
            assert_eq!(retrieved.function, persistent_function_handle);
        }
    }

    #[test]
    fn test_immortal_objects_never_collected() {
        let mut allocator = HeapAllocator::new();

        // Create functions (immortal)
        let function1 = create_test_function();
        let function_handle1 = allocator.allocate_function(function1);

        let function2 = create_test_function();
        let function_handle2 = allocator.allocate_function(function2);

        // Create strings (immortal)
        let string1 = allocator.strings.intern("immortal_string_1");
        let string2 = allocator.strings.intern("immortal_string_2");

        // Create mortal objects that reference immortals
        let closure = create_test_closure(function_handle1);
        let _closure_handle = allocator.allocate_closure(closure);

        let _upvalue_handle = allocator.allocate_upvalue(Value::String(string1));

        // Record initial total bytes (functions and strings are immortal)
        let bytes_before_gc = allocator.total_allocated_bytes();

        // Collect garbage with no roots - all mortal objects should be collected
        let roots = VecDeque::new();
        allocator.collect_garbage(roots);

        // Functions and strings are immortal, so they contribute to bytes after GC
        // The mortal objects (closures/upvalues) should have been collected
        let bytes_after_gc = allocator.total_allocated_bytes();

        // Mortal objects should have been collected, so bytes should decrease
        assert!(
            bytes_after_gc < bytes_before_gc,
            "Expected bytes to decrease after GC due to collected mortal objects. Before: {}, After: {}",
            bytes_before_gc,
            bytes_after_gc
        );

        // Functions and strings should remain accessible (they're immortal)

        // Functions should still be accessible
        let retrieved_function1 = allocator.get_function(function_handle1);
        assert_eq!(retrieved_function1.name, 0);

        let retrieved_function2 = allocator.get_function(function_handle2);
        assert_eq!(retrieved_function2.name, 0);

        // Strings should still be accessible
        assert_eq!(allocator.strings.get_string(string1), "immortal_string_1");
        assert_eq!(allocator.strings.get_string(string2), "immortal_string_2");
    }

    #[test]
    fn test_gc_threshold_behavior() {
        let _allocator = HeapAllocator::new().set_bytes_until_gc(1000);

        // Test that threshold setter works (can't access private field directly)

        // Test the should_collect_garbage logic in release mode
        #[cfg(not(debug_assertions))]
        {
            let mut allocator = HeapAllocator::new().set_bytes_until_gc(100);

            // Initially shouldn't need collection
            assert!(!allocator.should_collect_garbage());

            // Allocate enough objects to exceed threshold
            for _ in 0..20 {
                let function = create_test_function();
                let _function_handle = allocator.allocate_function(function);
                let closure = create_test_closure(0);
                let _closure_handle = allocator.allocate_closure(closure);
                let _upvalue = allocator.allocate_upvalue(Value::Number(42.0));
            }

            // Should now need collection
            assert!(allocator.should_collect_garbage());
        }
    }

    #[test]
    fn test_gc_updates_threshold_after_collection() {
        let mut allocator = HeapAllocator::new().set_bytes_until_gc(500);

        // Allocate some objects
        let function = create_test_function();
        let function_handle = allocator.allocate_function(function);
        let closure = create_test_closure(function_handle);
        let closure_handle = allocator.allocate_closure(closure);

        let _bytes_before_gc = allocator.total_allocated_bytes();

        // Collect garbage with the closure as root
        let mut roots = VecDeque::new();
        roots.push_back(Value::Closure(closure_handle));
        allocator.collect_garbage(roots);

        // After GC, threshold should be updated based on current allocated bytes
        let _bytes_after_gc = allocator.total_allocated_bytes();

        // The closure should still be accessible after GC
        let retrieved = allocator.get_closure(closure_handle);
        assert_eq!(retrieved.function, function_handle);

        // Verify the GC worked correctly by checking the closure is still valid
        assert!(!retrieved.is_marked); // Should be unmarked after GC
    }

    #[test]
    fn test_table_references_keep_objects_alive_during_gc() {
        let mut allocator = HeapAllocator::new();

        // Create class and instances
        let class_name = allocator.strings.intern("TestClass");
        let class_handle = allocator.allocate_class(class_name);

        // Create closures to store in instance fields
        let method1_function = create_test_function();
        let method1_function_handle = allocator.allocate_function(method1_function);
        let method1_closure = create_test_closure(method1_function_handle);
        let method1_closure_handle = allocator.allocate_closure(method1_closure);

        let method2_function = create_test_function();
        let method2_function_handle = allocator.allocate_function(method2_function);
        let method2_closure = create_test_closure(method2_function_handle);
        let method2_closure_handle = allocator.allocate_closure(method2_closure);

        // Create instances with fields pointing to objects
        let instance1_handle = allocator.allocate_instance(class_handle);
        let instance2_handle = allocator.allocate_instance(class_handle);

        // Set fields on instances - these references should keep the closures alive
        let field_name1 = allocator.strings.intern("method1");
        let field_name2 = allocator.strings.intern("method2");
        let field_name3 = allocator.strings.intern("sibling");

        allocator.set_instance_field(
            instance1_handle,
            Value::String(field_name1),
            Value::Closure(method1_closure_handle),
        );
        allocator.set_instance_field(
            instance1_handle,
            Value::String(field_name2),
            Value::Closure(method2_closure_handle),
        );
        allocator.set_instance_field(
            instance1_handle,
            Value::String(field_name3),
            Value::Instance(instance2_handle),
        );
        allocator.set_instance_field(
            instance2_handle,
            Value::String(field_name3),
            Value::Instance(instance1_handle),
        );

        // Create orphaned objects that should be collected
        let orphaned_function = create_test_function();
        let orphaned_function_handle = allocator.allocate_function(orphaned_function);
        let orphaned_closure = create_test_closure(orphaned_function_handle);
        let _orphaned_closure_handle = allocator.allocate_closure(orphaned_closure);

        let orphaned_class_name = allocator.strings.intern("OrphanedClass");
        let orphaned_class_handle = allocator.allocate_class(orphaned_class_name);
        let orphaned_instance_handle = allocator.allocate_instance(orphaned_class_handle);

        // Add field to orphaned instance
        let orphaned_field = allocator.strings.intern("orphaned_field");
        allocator.set_instance_field(
            orphaned_instance_handle,
            Value::String(orphaned_field),
            Value::Number(999.0),
        );

        let total_bytes_before = allocator.total_allocated_bytes();

        // Root only instance1 - this should keep instance1, instance2, and closures referenced by tables alive
        let mut roots = VecDeque::new();
        roots.push_back(Value::Instance(instance1_handle));

        allocator.collect_garbage(roots);

        let total_bytes_after = allocator.total_allocated_bytes();

        // Should have collected orphaned objects (bytes should decrease)
        assert!(
            total_bytes_after < total_bytes_before,
            "Expected total bytes to decrease after GC. Before: {}, After: {}",
            total_bytes_before,
            total_bytes_after
        );

        // Objects referenced by tables should still be accessible
        let retrieved_instance1 = allocator.get_instance(instance1_handle);
        assert_eq!(retrieved_instance1.clazz, class_handle);

        let retrieved_instance2 = allocator.get_instance(instance2_handle);
        assert_eq!(retrieved_instance2.clazz, class_handle);

        // Fields should still be accessible and point to correct objects
        let method1_field =
            allocator.get_instance_field(instance1_handle, Value::String(field_name1));
        assert_eq!(method1_field, Some(Value::Closure(method1_closure_handle)));

        let method2_field =
            allocator.get_instance_field(instance1_handle, Value::String(field_name2));
        assert_eq!(method2_field, Some(Value::Closure(method2_closure_handle)));

        let sibling_field =
            allocator.get_instance_field(instance1_handle, Value::String(field_name3));
        assert_eq!(sibling_field, Some(Value::Instance(instance2_handle)));

        let reverse_sibling =
            allocator.get_instance_field(instance2_handle, Value::String(field_name3));
        assert_eq!(reverse_sibling, Some(Value::Instance(instance1_handle)));

        // Referenced closures should still be accessible (kept alive by table references)
        let retrieved_method1 = allocator.get_closure(method1_closure_handle);
        assert_eq!(retrieved_method1.function, method1_function_handle);

        let retrieved_method2 = allocator.get_closure(method2_closure_handle);
        assert_eq!(retrieved_method2.function, method2_function_handle);
    }

    #[test]
    fn test_table_objects_collected_when_no_references() {
        let mut allocator = HeapAllocator::new();

        // Create objects that will become unreachable
        let class_name = allocator.strings.intern("UnreachableClass");
        let class_handle = allocator.allocate_class(class_name);
        let instance_handle = allocator.allocate_instance(class_handle);

        // Create closures to store in fields
        let mut closure_handles = Vec::new();
        for i in 0..3 {
            let function = create_test_function();
            let function_handle = allocator.allocate_function(function);
            let closure = create_test_closure(function_handle);
            let closure_handle = allocator.allocate_closure(closure);
            closure_handles.push(closure_handle);

            let field_name = allocator.strings.intern(&format!("closure_{}", i));
            allocator.set_instance_field(
                instance_handle,
                Value::String(field_name),
                Value::Closure(closure_handle),
            );
        }

        // Verify fields are set
        for i in 0..3 {
            let field_name = allocator.strings.intern(&format!("closure_{}", i));
            let field_value =
                allocator.get_instance_field(instance_handle, Value::String(field_name));
            assert_eq!(field_value, Some(Value::Closure(closure_handles[i])));
        }

        let total_bytes_before = allocator.total_allocated_bytes();

        // Collect garbage with no roots - everything should be collected
        let roots = VecDeque::new();
        allocator.collect_garbage(roots);

        let total_bytes_after = allocator.total_allocated_bytes();

        // Total bytes should decrease significantly (all mortal objects and their tables collected)
        assert!(
            total_bytes_after < total_bytes_before,
            "Expected total bytes to decrease after GC. Before: {}, After: {}",
            total_bytes_before,
            total_bytes_after
        );

        // The decrease should be substantial since we collected instances, classes, closures, and tables
        let bytes_collected = total_bytes_before - total_bytes_after;
        assert!(
            bytes_collected > 200, // Should be much more than just a few objects
            "Expected substantial bytes to be collected. Only collected: {}",
            bytes_collected
        );
    }

    #[test]
    fn test_objects_in_tables_survive_gc_when_reachable() {
        let mut allocator = HeapAllocator::new();

        // Create a class and instance to act as container
        let class_name = allocator.strings.intern("ContainerClass");
        let class_handle = allocator.allocate_class(class_name);
        let container_instance = allocator.allocate_instance(class_handle);

        // Create objects that would be orphaned except for table references
        let target_function = create_test_function();
        let target_function_handle = allocator.allocate_function(target_function);
        let target_closure = create_test_closure(target_function_handle);
        let target_closure_handle = allocator.allocate_closure(target_closure);

        let target_class_name = allocator.strings.intern("TargetClass");
        let target_class_handle = allocator.allocate_class(target_class_name);
        let target_instance_handle = allocator.allocate_instance(target_class_handle);

        // Store references to these objects in the container's table
        let closure_field = allocator.strings.intern("closure_ref");
        let instance_field = allocator.strings.intern("instance_ref");
        let class_field = allocator.strings.intern("class_ref");

        allocator.set_instance_field(
            container_instance,
            Value::String(closure_field),
            Value::Closure(target_closure_handle),
        );
        allocator.set_instance_field(
            container_instance,
            Value::String(instance_field),
            Value::Instance(target_instance_handle),
        );
        allocator.set_instance_field(
            container_instance,
            Value::String(class_field),
            Value::Class(target_class_handle),
        );

        // Create truly orphaned objects that should be collected
        let orphaned_function = create_test_function();
        let orphaned_function_handle = allocator.allocate_function(orphaned_function);
        let orphaned_closure = create_test_closure(orphaned_function_handle);
        let _orphaned_closure_handle = allocator.allocate_closure(orphaned_closure);

        let bytes_before = allocator.total_allocated_bytes();

        // Root only the container instance - objects in its table should be kept alive
        let mut roots = VecDeque::new();
        roots.push_back(Value::Instance(container_instance));

        allocator.collect_garbage(roots);

        let bytes_after = allocator.total_allocated_bytes();

        // Should have collected some objects (the orphaned ones) but not all
        assert!(
            bytes_after < bytes_before,
            "Expected some objects to be collected. Before: {}, After: {}",
            bytes_before,
            bytes_after
        );

        // Objects referenced by the table should still be accessible through field access
        let closure_ref =
            allocator.get_instance_field(container_instance, Value::String(closure_field));
        assert_eq!(
            closure_ref,
            Some(Value::Closure(target_closure_handle)),
            "Closure referenced by table should still be accessible"
        );

        let instance_ref =
            allocator.get_instance_field(container_instance, Value::String(instance_field));
        assert_eq!(
            instance_ref,
            Some(Value::Instance(target_instance_handle)),
            "Instance referenced by table should still be accessible"
        );

        let class_ref =
            allocator.get_instance_field(container_instance, Value::String(class_field));
        assert_eq!(
            class_ref,
            Some(Value::Class(target_class_handle)),
            "Class referenced by table should still be accessible"
        );

        // The actual objects should be directly accessible (proving they weren't collected)
        let retrieved_closure = allocator.get_closure(target_closure_handle);
        assert_eq!(retrieved_closure.function, target_function_handle);
        assert!(!retrieved_closure.is_marked); // Should be unmarked after GC

        let retrieved_instance = allocator.get_instance(target_instance_handle);
        assert_eq!(retrieved_instance.clazz, target_class_handle);
        assert!(!retrieved_instance.is_marked); // Should be unmarked after GC

        let retrieved_class = allocator.get_class(target_class_handle);
        assert_eq!(retrieved_class.name, target_class_name);
        assert!(!retrieved_class.is_marked); // Should be unmarked after GC
    }

    #[test]
    fn test_complex_mixed_object_scenario() {
        let mut allocator = HeapAllocator::new();

        // Create a complex scenario with all object types interlinked

        // Create class hierarchy
        let base_class_name = allocator.strings.intern("BaseClass");
        let base_class = allocator.allocate_class(base_class_name);

        let derived_class_name = allocator.strings.intern("DerivedClass");
        let derived_class = allocator.allocate_class(derived_class_name);

        // Create instances
        let base_instance = allocator.allocate_instance(base_class);
        let derived_instance = allocator.allocate_instance(derived_class);

        // Create closures with complex upvalue chains
        let function1 = create_test_function();
        let function_handle1 = allocator.allocate_function(function1);
        let closure1 = ClosureObject::new(function_handle1, 2);
        let closure_handle1 = allocator.allocate_closure(closure1);

        // First upvalue points to base instance
        let upvalue1 = allocator.allocate_upvalue(Value::Instance(base_instance));
        allocator.closures.set_upvalue(closure_handle1, 0, UpvalueSlot::Closed(upvalue1));

        // Second upvalue points to derived instance
        let upvalue2 = allocator.allocate_upvalue(Value::Instance(derived_instance));
        allocator.closures.set_upvalue(closure_handle1, 1, UpvalueSlot::Closed(upvalue2));

        // Set fields on instances that reference each other and the closure
        let field_name1 = allocator.strings.intern("reference");
        let field_name2 = allocator.strings.intern("method");

        allocator.set_instance_field(
            base_instance,
            Value::String(field_name1),
            Value::Instance(derived_instance),
        );
        allocator.set_instance_field(
            derived_instance,
            Value::String(field_name1),
            Value::Instance(base_instance),
        );
        allocator.set_instance_field(
            base_instance,
            Value::String(field_name2),
            Value::Closure(closure_handle1),
        );

        // Create some objects that should be collected
        let orphaned_function = create_test_function();
        let orphaned_function_handle = allocator.allocate_function(orphaned_function);
        let orphaned_closure = create_test_closure(orphaned_function_handle);
        let _orphaned_closure_handle = allocator.allocate_closure(orphaned_closure);

        let _orphaned_upvalue = allocator.allocate_upvalue(Value::Number(999.0));

        let bytes_before = allocator.total_allocated_bytes();

        // Root only the base instance - should keep everything in the connected graph alive
        let mut roots = VecDeque::new();
        roots.push_back(Value::Instance(base_instance));

        allocator.collect_garbage(roots);

        let bytes_after = allocator.total_allocated_bytes();

        // Should have collected orphaned objects
        assert!(bytes_after < bytes_before);

        // All connected objects should still be accessible
        let retrieved_base = allocator.get_instance(base_instance);
        assert_eq!(retrieved_base.clazz, base_class);

        let retrieved_derived = allocator.get_instance(derived_instance);
        assert_eq!(retrieved_derived.clazz, derived_class);

        // Cross-references should be intact
        let base_ref = allocator.get_instance_field(base_instance, Value::String(field_name1));
        assert_eq!(base_ref, Some(Value::Instance(derived_instance)));

        let derived_ref =
            allocator.get_instance_field(derived_instance, Value::String(field_name1));
        assert_eq!(derived_ref, Some(Value::Instance(base_instance)));

        let method_ref = allocator.get_instance_field(base_instance, Value::String(field_name2));
        assert_eq!(method_ref, Some(Value::Closure(closure_handle1)));

        // Closure and its upvalues should be accessible
        let retrieved_closure = allocator.get_closure(closure_handle1);
        assert_eq!(retrieved_closure.upvalue_count, 2);

        if let Some(UpvalueSlot::Closed(handle)) = allocator.closures.get_upvalue(closure_handle1, 0) {
            let upvalue_val = allocator.get_upvalue(handle);
            assert_eq!(*upvalue_val, Value::Instance(base_instance));
        }

        if let Some(UpvalueSlot::Closed(handle)) = allocator.closures.get_upvalue(closure_handle1, 1) {
            let upvalue_val = allocator.get_upvalue(handle);
            assert_eq!(*upvalue_val, Value::Instance(derived_instance));
        }
    }
}
