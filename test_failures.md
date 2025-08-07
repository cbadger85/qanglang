# Test Failures Analysis

## Failed Tests

### 1. `test_booleans` (src/tests/compiler_tests.rs:421)
**Status**: FAILED  
**Issue**: Panic with "No active call frame" at src/vm.rs:95  
**Probable Cause**: The `OpCode::Return` handler calls `self.pop()` after decrementing the frame count to 0, but `pop()` calls `get_current_loc()` which needs an active frame.

### 2. `math_operations_test` (src/tests/compiler_tests.rs:300)
**Status**: FAILED  
**Issue**: Panic with "No active call frame" at src/vm.rs:95  
**Probable Cause**: Same as `test_booleans` - the return instruction handling is broken.

### 3. `equality_operations_test` (src/tests/compiler_tests.rs:326)  
**Status**: FAILED  
**Issue**: Panic with "No active call frame" at src/vm.rs:95  
**Probable Cause**: Same as above - return instruction handling issue.

### 4. `test_run` (src/tests/compiler_tests.rs:24)
**Status**: FAILED  
**Issue**: Panic with "No active call frame" at src/vm.rs:95  
**Probable Cause**: Same as the other tests - the return instruction handling issue. The `assert_eq` function has been fixed.

## Root Cause Analysis

### Primary Issue: OpCode::Return Implementation (vm.rs:410-426)
The main problem is in the `OpCode::Return` match arm. When the frame count reaches 0, it calls `self.pop()` but the `pop()` method tries to get the current location via `get_current_loc()`, which requires an active call frame. This creates a panic because there are no frames left.

Current problematic code:
```rust
let result = self.pop()?;
self.frame_count -= 1;

if self.frame_count == 0 {
    self.pop()?;  // This line causes the panic
    return Ok(());
}
```

## Impact
- All simple expression tests fail (booleans, math, equality)
- Complex program execution fails due to return instruction handling
- Basic VM functionality is broken for any program that needs to return from the main function

## Suggested Fix Priority
1. **HIGH**: Fix the `OpCode::Return` implementation to avoid calling methods that require active frames after frame count reaches 0