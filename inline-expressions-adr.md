# Architecture Decision Record: Inline Execution for Map Expressions and Lambda Calls

## Status

Proposed

## Context

Currently, map expressions like `obj||x -> x.property|` are compiled as immediately-invoked function expressions (IIFEs), which creates significant overhead:

1. **Closure allocation** - Each map expression creates a closure object on the heap
2. **Call frame overhead** - Each execution pushes a new frame onto the call stack
3. **Upvalue management** - Parameters require upvalue capturing even for immediate use
4. **GC pressure** - Short-lived closures increase garbage collection frequency

Map expressions are conceptually inline operations that should execute in the current scope without the overhead of function calls. Additionally, we need to support closure capture within map expressions:

```qang
value||x -> (() -> x)|  // Returns a lambda that has closed over 'value'
```

## Decision

We will implement **inline execution** for map expressions and immediate lambda calls through two new bytecode opcodes: `CallInline` and `CallInlineWithCapture`.

## Design

### Core Opcodes

#### `CallInline`

For simple inline execution without closure capture:

```
OpCode::CallInline {
    param_count: u8,      // Number of parameters
    local_base: u8,       // Starting local slot for parameters
    body_length: u16,     // Length of inlined body bytecode
    // Followed by: body bytecode instructions
}
```

#### `CallInlineWithCapture`

For inline execution that may create closures:

```
OpCode::CallInlineWithCapture {
    param_count: u8,      // Number of parameters
    local_base: u8,       // Starting local slot for parameters
    capture_count: u8,    // Number of variables that may be captured
    body_length: u16,     // Length of inlined body bytecode
    // Followed by: body bytecode instructions
}
```

### Execution Model

#### Stack Management

1. **Parameter binding**: Map stack arguments to temporary local slots
2. **Scope isolation**: Use a distinct local slot range to avoid conflicts
3. **Result preservation**: Ensure expression result remains on stack top
4. **Cleanup**: Remove parameter locals after execution

#### Example Compilation

**Source**: `obj||x -> x.inner.value|`

**Bytecode**:

```
OP_GET_GLOBAL    0  'obj'           // [obj]
OP_CALL_INLINE   1  5  8            // param_count=1, local_base=5, body_length=8
  OP_GET_LOCAL   5                  // [obj, obj.inner.value] (x parameter)
  OP_GET_PROPERTY 1 'inner'
  OP_GET_PROPERTY 2 'value'
  OP_RETURN_INLINE                  // End inline body
OP_CONTINUE                         // [obj.inner.value]
```

### Closure Capture Support

For expressions that create closures within map bodies:

**Source**: `value||x -> (() -> x)|`

**Compilation Strategy**:

1. **Detect capture**: Analyze body for lambda expressions that reference parameters
2. **Promote parameters**: Convert captured parameters to proper local variables
3. **Use capture opcode**: Emit `CallInlineWithCapture` instead of `CallInline`
4. **Upvalue management**: Handle upvalue creation for nested lambdas

**Bytecode**:

```
OP_GET_GLOBAL          0  'value'    // [value]
OP_CALL_INLINE_CAPTURE 1  5  1  12   // param_count=1, local_base=5, capture_count=1, body_length=12
  OP_SET_LOCAL         5             // Store parameter as capturable local
  OP_CLOSURE           0             // Create lambda closure
    OP_GET_LOCAL       5             // Lambda body references x
    OP_RETURN
  OP_UPVALUE           5  1          // Mark local 5 as upvalue
  OP_RETURN_INLINE                   // End inline body
OP_CONTINUE                          // [lambda_closure]
```

### VM Implementation

#### CallInline Execution

```rust
OpCode::CallInline => {
    let param_count = self.state.read_byte() as usize;
    let local_base = self.state.read_byte() as usize;
    let body_length = self.state.read_short() as usize;

    // Save current execution context
    let saved_ip = self.state.frames[self.state.frame_count - 1].ip;
    let saved_locals_base = self.state.frames[self.state.frame_count - 1].value_slot;

    // Bind parameters to local slots
    for i in (0..param_count).rev() {
        let param_value = pop_value!(self);
        self.state.stack[local_base + i] = param_value;
    }

    // Execute body inline (continue with next instructions)
    // Body ends with OP_RETURN_INLINE which restores context
}

OpCode::ReturnInline => {
    // Result is already on stack top
    // Clean up parameter locals
    for i in 0..param_count {
        self.state.stack[local_base + i] = Value::Nil; // Clear
    }
    // Continue execution after inline body
}
```

#### Capture Handling

```rust
OpCode::CallInlineWithCapture => {
    // Similar to CallInline but:
    // 1. Mark specified locals as capturable
    // 2. Handle upvalue creation during body execution
    // 3. Manage upvalue closing at end of inline execution
}
```

### Compiler Changes

#### Map Expression Compilation

```rust
ast::CallOperation::Map(map_expr) => {
    // Visit callee to get value on stack
    self.visit_expression(call.callee.as_ref(), errors)?;

    // Analyze body for closure capture
    let captures_parameter = self.analyze_captures(&map_expr.body, &map_expr.parameter);

    if captures_parameter {
        self.compile_inline_with_capture(map_expr, errors)
    } else {
        self.compile_inline_simple(map_expr, errors)
    }
}

fn compile_inline_simple(&mut self, map_expr: &MapExpr, errors: &mut ErrorReporter) -> Result<(), QangSyntaxError> {
    let local_base = self.allocate_inline_locals(1)?;
    let body_start = self.current_chunk_mut().code.len();

    // Compile body with parameter mapped to local slot
    self.push_inline_context(&map_expr.parameter, local_base);
    self.visit_expression(&map_expr.body, errors)?;
    self.emit_opcode(OpCode::ReturnInline, map_expr.span);
    self.pop_inline_context();

    let body_length = self.current_chunk_mut().code.len() - body_start;

    // Insert CallInline at the beginning
    self.emit_opcode_with_params(OpCode::CallInline, &[1, local_base as u8], map_expr.span);
    self.emit_short(body_length as u16, map_expr.span);

    Ok(())
}
```

### Benefits

1. **Performance**: Eliminates closure allocation and call frame overhead
2. **Memory**: Reduces GC pressure from short-lived closures
3. **Simplicity**: No complex scope management issues
4. **Compatibility**: Maintains current semantics while optimizing execution
5. **Flexibility**: Supports both simple inline execution and closure capture

### Drawbacks

1. **Bytecode complexity**: New opcodes increase VM implementation complexity
2. **Debugging**: Inline execution may complicate stack traces and debugging
3. **Code size**: Inlined bodies may increase bytecode size vs. shared closures
4. **Implementation effort**: Significant compiler and VM changes required

### Alternatives Considered

1. **Stack-only approach**: Using stack manipulation without local variables
   - Rejected due to complexity with nested expressions and closure capture
2. **Specialized scope cleanup**: Fixing `end_scope_expression`
   - Rejected due to ongoing complexity with local variable management
3. **Compiler optimization**: Post-compilation IIFE elimination
   - Rejected due to difficulty detecting optimization opportunities

### Migration Path

1. **Phase 1**: Implement basic `CallInline` for simple map expressions
2. **Phase 2**: Add `CallInlineWithCapture` for closure support
3. **Phase 3**: Extend to other inline expression types (ternary, etc.)
4. **Phase 4**: Optimize bytecode generation and VM execution

## Consequences

### Positive

- Significant performance improvement for map expressions
- Eliminates current scope management bugs
- Maintains language semantics
- Enables future optimization opportunities

### Negative

- Increased VM and compiler complexity
- New opcodes require thorough testing
- Debugging experience may change
- Implementation requires careful attention to edge cases

### Neutral

- Bytecode format changes (version bump required)
- Additional documentation needed
- May require profiling to measure actual performance gains
