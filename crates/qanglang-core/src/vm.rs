use std::{collections::HashMap, ops::Range};

#[cfg(feature = "profiler")]
use coz;

use crate::{
    ObjectHeap, QangProgram, QangRuntimeError, Value,
    chunk::{OpCode, SourceLocation},
    compiler::{FRAME_MAX, STACK_MAX},
    debug::disassemble_instruction,
    error::{Trace, ValueConversionError},
    memory::{ClosureHandle, FunctionHandle, StringHandle},
    object::{ClosureObject, FunctionObject, UpvalueReference},
    qang_std::{
        qang_assert, qang_assert_eq, qang_assert_throws, qang_print, qang_println,
        qang_system_time, qang_to_lowercase, qang_to_string, qang_to_uppercase, qang_typeof,
    },
    value::{
        BOOLEAN_TYPE_STRING, FUNCTION_TYPE_STRING, FunctionValueKind, NIL_TYPE_STRING,
        NUMBER_TYPE_STRING, NativeFunction, STRING_TYPE_STRING,
    },
};

#[derive(Debug, Clone)]
pub struct NativeFunctionError(pub String);

impl NativeFunctionError {
    pub fn new(message: &str) -> Self {
        Self(message.to_string())
    }

    fn into_qang_error(self, loc: SourceLocation) -> QangRuntimeError {
        QangRuntimeError::new(self.0, loc)
    }
}

impl From<&'static str> for NativeFunctionError {
    fn from(value: &'static str) -> Self {
        NativeFunctionError::new(value)
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOperationError(pub String);

impl BinaryOperationError {
    pub fn new(message: &str) -> Self {
        Self(message.to_string())
    }

    fn into_qang_error(self, loc: SourceLocation) -> QangRuntimeError {
        QangRuntimeError::new(self.0, loc)
    }
}

impl From<&'_ str> for BinaryOperationError {
    fn from(value: &'_ str) -> Self {
        BinaryOperationError::new(value)
    }
}

impl From<ValueConversionError> for BinaryOperationError {
    fn from(value: ValueConversionError) -> Self {
        BinaryOperationError(value.into_message())
    }
}

pub type RuntimeResult<T> = Result<T, QangRuntimeError>;

pub type NativeFn = fn(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError>;

type StackSlot = usize;
type UpvalueIndex = usize;
type OpenUpvalueEntry = (StackSlot, Vec<(ClosureHandle, UpvalueIndex)>);

macro_rules! push_value {
    ($vm:expr, $value:expr) => {
        if $vm.state.stack_top >= STACK_MAX {
            Err(QangRuntimeError::new(
                format!(
                    "Stack overflow: maximum stack size of {} exceeded",
                    STACK_MAX
                ),
                $vm.state.get_current_loc(),
            ))
        } else {
            $vm.state.stack[$vm.state.stack_top] = $value;
            $vm.state.stack_top += 1;
            Ok(())
        }
    };
}

macro_rules! pop_value {
    ($vm:expr) => {{
        debug_assert!(
            $vm.state.stack_top > 0,
            "Stack underflow: unexpected empty stack."
        );
        $vm.state.stack_top -= 1;
        $vm.state.stack[$vm.state.stack_top]
    }};
}

macro_rules! peek {
    ($vm:expr, $distance:expr) => {
        if $vm.state.stack_top > $distance {
            $vm.state
                .stack
                .get($vm.state.stack_top - 1 - $distance)
                .copied()
                .unwrap_or(Value::Nil)
        } else {
            Value::Nil
        }
    };
}

macro_rules! gc_allocate {
    // Closure allocation
    ($vm:expr, closure: $value:expr) => {{
        if !$vm.heap.can_allocate_closure() {
            $vm.collect_garbage();
        }
        $vm.heap.force_allocate_closure($value)
    }};

    // Value allocation
    ($vm:expr, value: $value:expr) => {{
        if !$vm.heap.can_allocate_value() {
            $vm.collect_garbage();
        }
        $vm.heap.force_allocate_value($value)
    }};
}


#[derive(Debug, Clone)]
struct CallFrame {
    closure: ClosureHandle,
    ip: usize,
    value_slot: usize,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self {
            closure: ClosureHandle::default(),
            ip: 0,
            value_slot: 0,
        }
    }
}

#[derive(Clone)]
pub struct VmState {
    stack_top: usize,
    frame_count: usize,
    stack: Vec<Value>,
    frames: [CallFrame; FRAME_MAX],
    globals: HashMap<StringHandle, Value>,
    open_upvalues: Vec<OpenUpvalueEntry>,
    current_function_ptr: *const FunctionObject,
}

impl VmState {
    fn get_current_function(&self) -> &FunctionObject {
        debug_assert!(
            !self.current_function_ptr.is_null(),
            "Function pointer is null"
        );
        unsafe { &*self.current_function_ptr }
    }

    fn get_current_loc(&self) -> SourceLocation {
        self.get_loc_at(self.frames[self.frame_count - 1].ip)
    }

    fn get_previous_loc(&self) -> SourceLocation {
        if self.frames[self.frame_count - 1].ip > 0 {
            self.get_loc_at(self.frames[self.frame_count - 1].ip - 1)
        } else {
            SourceLocation::default()
        }
    }

    fn get_loc_at(&self, index: usize) -> SourceLocation {
        self.get_current_function()
            .chunk
            .locs()
            .get(index)
            .copied()
            .unwrap_or_default()
    }

    fn read_byte(&mut self) -> u8 {
        let frame = &mut self.frames[self.frame_count - 1];
        debug_assert!(
            !self.current_function_ptr.is_null(),
            "Function pointer is null"
        );
        let code = unsafe { (*self.current_function_ptr).chunk.code() };
        debug_assert!(frame.ip < code.len(), "IP out of bounds");
        let byte = code[frame.ip];
        frame.ip += 1;
        byte
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        debug_assert!(
            !self.current_function_ptr.is_null(),
            "Function pointer is null"
        );
        let constants = unsafe { (*self.current_function_ptr).chunk.constants() };
        debug_assert!(index < constants.len(), "Constant index out of bounds");
        constants[index]
    }

    fn read_short(&mut self) -> usize {
        let high_byte = self.read_byte() as usize;
        let low_byte = self.read_byte() as usize;
        (high_byte << 8) | low_byte
    }

    pub fn gather_roots(&self) -> Vec<Value> {
        let mut roots = Vec::new();

        // Stack roots
        roots.extend_from_slice(&self.stack[..self.stack_top]);

        // Global roots
        roots.extend(self.globals.values().copied());

        // Frame closure roots
        for frame in &self.frames[..self.frame_count] {
            roots.push(Value::Function(FunctionValueKind::Closure(frame.closure)));
        }

        // Upvalue roots
        for (stack_slot, _) in &self.open_upvalues {
            roots.push(self.stack[*stack_slot]);
        }

        roots
    }
}

#[derive(Clone)]
pub struct Vm {
    is_debug: bool,
    state: VmState,
    heap: ObjectHeap,
}

impl Vm {
    pub fn new(mut heap: ObjectHeap) -> Self {
        let mut globals = HashMap::new();

        let nil_type_handle = heap.intern_string_slice("NIL".into());
        let nil_type_value_handle = heap.intern_string_slice(NIL_TYPE_STRING.into());
        globals.insert(nil_type_handle, Value::String(nil_type_value_handle));

        let boolean_type_handle = heap.intern_string_slice("BOOLEAN".into());
        let boolean_type_value_handle = heap.intern_string_slice(BOOLEAN_TYPE_STRING.into());
        globals.insert(
            boolean_type_handle,
            Value::String(boolean_type_value_handle),
        );

        let number_type_handle = heap.intern_string_slice("NUMBER".into());
        let number_type_value_handle = heap.intern_string_slice(NUMBER_TYPE_STRING.into());
        globals.insert(number_type_handle, Value::String(number_type_value_handle));

        let string_type_handle = heap.intern_string_slice("STRING".into());
        let string_type_value_handle = heap.intern_string_slice(STRING_TYPE_STRING.into());
        globals.insert(string_type_handle, Value::String(string_type_value_handle));

        let function_type_handle = heap.intern_string_slice("FUNCTION".into());
        let function_type_value_handle = heap.intern_string_slice(FUNCTION_TYPE_STRING.into());
        globals.insert(
            function_type_handle,
            Value::String(function_type_value_handle),
        );

        let state = VmState {
            frame_count: 0,
            stack_top: 1,
            stack: vec![Value::Nil; STACK_MAX],
            frames: std::array::from_fn(|_| CallFrame::default()),
            globals,
            open_upvalues: Vec::with_capacity(8),
            current_function_ptr: std::ptr::null(),
        };

        let vm = Self {
            is_debug: false,
            state,
            heap,
        };

        vm.add_native_function("assert", 2, qang_assert)
            .add_native_function("assert_eq", 3, qang_assert_eq)
            .add_native_function("assert_throws", 2, qang_assert_throws)
            .add_native_function("print", 1, qang_print)
            .add_native_function("println", 1, qang_println)
            .add_native_function("system_time", 0, qang_system_time)
            .add_native_function("typeof", 1, qang_typeof)
            .add_native_function("to_string", 1, qang_to_string)
            .add_native_function("to_uppercase", 1, qang_to_uppercase)
            .add_native_function("to_lowercase", 1, qang_to_lowercase)
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    pub fn add_native_function(mut self, name: &str, arity: usize, function: NativeFn) -> Self {
        let identifier_handle = self.heap.intern_string_slice(name);
        let native_function = NativeFunction {
            name: identifier_handle,
            arity,
            function,
        };

        self.state.globals.insert(
            identifier_handle,
            Value::Function(FunctionValueKind::NativeFunction(native_function)),
        );

        self
    }

    pub fn interpret(&mut self, program: QangProgram) -> RuntimeResult<()> {
        let function_rc = program.into_function();
        let upvalue_count = function_rc.upvalue_count;
        let function_handle = self.heap.allocate_function((*function_rc).clone());

        let handle =
            gc_allocate!(self, closure: ClosureObject::new(function_handle, upvalue_count));
        self.call(handle, 0)?;

        #[cfg(feature = "profiler")]
        coz::scope!("vm_interpret");

        let _ = self
            .run()
            .map_err(|e| e.with_stack_trace(self.get_stack_trace()))?;

        #[cfg(feature = "profiler")]
        coz::progress!("execution_complete");

        Ok(())
    }

    fn run(&mut self) -> RuntimeResult<Value> {
        loop {
            #[cfg(feature = "profiler")]
            coz::progress!("vm_instructions");

            if self.is_debug {
                self.debug();
            }

            let opcode: OpCode = self.state.read_byte().into();

            match opcode {
                OpCode::Constant => {
                    let constant = self.state.read_constant();
                    push_value!(self, constant)?;
                }
                OpCode::Negate => {
                    if let Value::Number(number) = peek!(self, 0) {
                        if let Some(stack_value) =
                            self.state.stack.get_mut(self.state.stack_top - 1)
                        {
                            *stack_value = Value::Number(-number);
                        }
                    } else {
                        return Err(QangRuntimeError::new(
                            "Operand must be a number.".to_string(),
                            self.state.get_previous_loc(),
                        ));
                    }
                }
                OpCode::Not => {
                    let value = peek!(self, 0);
                    if let Some(stack_value) = self.state.stack.get_mut(self.state.stack_top - 1) {
                        *stack_value = Value::Boolean(!value.is_truthy());
                    }
                }
                OpCode::True => {
                    push_value!(self, Value::Boolean(true))?;
                }
                OpCode::False => {
                    push_value!(self, Value::Boolean(false))?;
                }
                OpCode::Nil => {
                    push_value!(self, Value::Nil)?;
                }
                OpCode::Add => {
                    self.binary_operation(|a, b, heap| match (&a, &b) {
                        (Value::Number(num1), Value::Number(num2)) => {
                            Ok(Value::Number(num1 + num2))
                        }
                        (Value::String(handle1), Value::String(handle2)) => {
                            #[cfg(feature = "profiler")]
                            coz::scope!("string_concatenation");

                            let str1 = heap.get_string(*handle1);

                            let str2 = heap.get_string(*handle2);

                            let mut str1_str2 = String::with_capacity(str1.len() + str2.len());
                            str1_str2.push_str(&str1);
                            str1_str2.push_str(&str2);

                            let result = heap.intern_string_slice(&str1_str2);
                            Ok(Value::String(result))
                        }
                        (Value::Number(_), _) => {
                            Err(format!("Cannot add number to {}.", b.to_type_string())
                                .as_str()
                                .into())
                        }
                        (Value::String(_), _) => {
                            Err(format!("Cannot add string to {}.", b.to_type_string())
                                .as_str()
                                .into())
                        }
                        (_, Value::Number(_)) => {
                            Err(format!("Cannot add {} to number.", a.to_type_string())
                                .as_str()
                                .into())
                        }
                        (_, Value::String(_)) => {
                            Err(format!("Cannot add {} to string.", a.to_type_string())
                                .as_str()
                                .into())
                        }
                        _ => Err("Both operands must be a numbers or strings.".into()),
                    })?;
                }
                OpCode::Subtract => self.binary_operation(|a, b, _heap| {
                    let a: f64 = a.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;

                    Ok((a - b).into())
                })?,
                OpCode::Multiply => self.binary_operation(|a, b, _heap| {
                    let a: f64 = a.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;

                    Ok((a * b).into())
                })?,
                OpCode::Divide => self.binary_operation(|a, b, _heap| {
                    let a: f64 = a.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    let b: f64 = b
                        .try_into()
                        .map_err(|_| BinaryOperationError::new("Both operands must be a number."))
                        .and_then(|num| {
                            if num == 0.0 {
                                Err("Cannot divide by zero.".into())
                            } else {
                                Ok(num)
                            }
                        })?;

                    Ok((a / b).into())
                })?,
                OpCode::Modulo => self.binary_operation(|a, b, _heap| {
                    let a: f64 = a.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;

                    Ok((a % b).into())
                })?,
                OpCode::Equal => self.binary_operation(|a, b, _heap| Ok(Value::Boolean(a == b)))?,
                OpCode::Greater => self.binary_operation(|a, b, _heap| {
                    let a: f64 = a.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    Ok(Value::Boolean(a > b))
                })?,
                OpCode::GreaterEqual => self.binary_operation(|a, b, _heap| {
                    let a: f64 = a.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    Ok(Value::Boolean(a >= b))
                })?,
                OpCode::Less => self.binary_operation(|a, b, _heap| {
                    let a: f64 = a.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    Ok(Value::Boolean(a < b))
                })?,
                OpCode::LessEqual => self.binary_operation(|a, b, _heap| {
                    let a: f64 = a.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        BinaryOperationError::new("Both operands must be a number.")
                    })?;
                    Ok(Value::Boolean(a <= b))
                })?,
                OpCode::Pop => {
                    pop_value!(self);
                }
                OpCode::DefineGlobal => {
                    let identifier_handle: StringHandle =
                        self.state.read_constant().try_into().map_err(
                            |e: ValueConversionError| {
                                e.into_qang_error(self.state.get_previous_loc())
                            },
                        )?;
                    let value = pop_value!(self);
                    self.state.globals.insert(identifier_handle, value);
                }
                OpCode::GetGlobal => {
                    let identifier_handle: StringHandle =
                        self.state.read_constant().try_into().map_err(
                            |e: ValueConversionError| {
                                let loc = self.state.get_previous_loc();
                                e.into_qang_error(loc)
                            },
                        )?;
                    let value = *self.state.globals.get(&identifier_handle).ok_or_else(|| {
                        let loc = self.state.get_previous_loc();
                        let identifier_name = self.heap.get_string(identifier_handle);
                        QangRuntimeError::new(
                            format!("Undefined variable: {}.", identifier_name),
                            loc,
                        )
                    })?;
                    push_value!(self, value)?;
                }
                OpCode::SetGlobal => {
                    let identifier_handle: StringHandle =
                        self.state.read_constant().try_into().map_err(
                            |e: ValueConversionError| {
                                e.into_qang_error(self.state.get_previous_loc())
                            },
                        )?;

                    if !self.state.globals.contains_key(&identifier_handle) {
                        let identifier_name = self.heap.get_string(identifier_handle);
                        let loc = self.state.get_previous_loc();
                        return Err(QangRuntimeError::new(
                            format!("Undefined variable: {}.", identifier_name).to_string(),
                            loc,
                        ));
                    }
                    let value = peek!(self, 0);
                    self.state.globals.insert(identifier_handle, value);
                }
                OpCode::GetLocal => {
                    let slot = self.state.read_byte();
                    let absolute_slot = self.state.frames[self.state.frame_count - 1].value_slot + 1 + slot as usize;
                    debug_assert!(
                        absolute_slot < STACK_MAX,
                        "Local slot {} out of bounds",
                        absolute_slot
                    );

                    let value = self.state.stack[absolute_slot];
                    push_value!(self, value)?;
                }
                OpCode::SetLocal => {
                    let slot = self.state.read_byte();
                    let value = peek!(self, 0);
                    let absolute_slot = self.state.frames[self.state.frame_count - 1].value_slot + 1 + slot as usize;

                    self.state.stack[absolute_slot] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.state.read_short();
                    if !peek!(self, 0).is_truthy() {
                        self.state.frames[self.state.frame_count - 1].ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = self.state.read_short();
                    self.state.frames[self.state.frame_count - 1].ip += offset;
                }
                OpCode::Loop => {
                    let offset = self.state.read_short();
                    self.state.frames[self.state.frame_count - 1].ip -= offset;
                }
                OpCode::Call => {
                    let arg_count = self.state.read_byte() as usize;
                    let function_value = peek!(self, arg_count);
                    self.call_value(function_value, arg_count)?;
                }
                OpCode::Closure => {
                    let handle: FunctionHandle = self.state.read_constant().try_into().map_err(
                        |e: ValueConversionError| {
                            QangRuntimeError::new(
                                e.message().to_string(),
                                self.state.get_previous_loc(),
                            )
                        },
                    )?;
                    let upvalue_count = self.heap.get_function(handle).upvalue_count;
                    let closure_handle =
                        gc_allocate!(self, closure: ClosureObject::new(handle, upvalue_count));

                    for i in 0..self.heap.get_closure(closure_handle).upvalue_count {
                        let is_local = self.state.read_byte() != 0;
                        let index = self.state.read_byte() as usize;

                        if is_local {
                            let stack_slot = self.state.frames[self.state.frame_count - 1].value_slot + 1 + index;
                            self.capture_upvalue(stack_slot, closure_handle, i);
                        } else {
                            let current_closure = self.heap.get_closure(self.state.frames[self.state.frame_count - 1].closure);
                            let current_upvalue = current_closure.upvalues[index];
                            self.heap.get_closure_mut(closure_handle).upvalues[i] = current_upvalue;
                        }
                    }
                    push_value!(
                        self,
                        Value::Function(FunctionValueKind::Closure(closure_handle))
                    )?;
                }
                OpCode::GetUpvalue => {
                    let slot = self.state.read_byte() as usize;
                    let current_closure = self.heap.get_closure(self.state.frames[self.state.frame_count - 1].closure);
                    let upvalue = current_closure.upvalues[slot];

                    match upvalue {
                        UpvalueReference::Open(stack_slot) => {
                            let value = self.state.stack[stack_slot];
                            push_value!(self, value)?;
                        }
                        UpvalueReference::Closed(value_handle) => {
                            let value = *self.heap.get_value(value_handle);
                            push_value!(self, value)?;
                        }
                    }
                }
                OpCode::SetUpvalue => {
                    let slot = self.state.read_byte() as usize;
                    let value = peek!(self, 0);
                    let current_closure_handle = self.state.frames[self.state.frame_count - 1].closure;

                    let upvalue = self.heap.get_closure(current_closure_handle).upvalues[slot];
                    match upvalue {
                        UpvalueReference::Open(stack_slot) => {
                            self.state.stack[stack_slot] = value;
                        }
                        UpvalueReference::Closed(value_handle) => {
                            *self.heap.get_value_mut(value_handle) = value;
                        }
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalue(self.state.stack_top - 1);
                    pop_value!(self);
                }
                OpCode::Return => {
                    let result = pop_value!(self);
                    let value_slot = self.state.frames[self.state.frame_count - 1].value_slot;

                    // Close upvalues for the current function's locals
                    self.close_upvalue(value_slot);

                    self.state.frame_count -= 1;

                    #[cfg(feature = "profiler")]
                    coz::progress!("function_returns");

                    if self.state.frame_count == 0 {
                        return Ok(result);
                    }

                    // Restore the previous function pointer
                    let previous_frame = &self.state.frames[self.state.frame_count - 1];
                    let previous_closure = self.heap.get_closure(previous_frame.closure);
                    let previous_function = self.heap.get_function(previous_closure.function);
                    self.state.current_function_ptr = previous_function as *const FunctionObject;

                    self.state.stack_top = value_slot + 1;
                    self.state.stack[value_slot] = result;
                }
            };
        }
    }

    fn close_upvalue(&mut self, last_slot: StackSlot) {
        let mut i = self.state.open_upvalues.len();
        while i > 0 {
            i -= 1;
            let stack_slot = self.state.open_upvalues[i].0;

            if stack_slot >= last_slot {
                let value = self.state.stack[stack_slot];

                let value_handle = gc_allocate!(self, value: value);

                // Extract the closures list by removing and then updating
                let (_, closures_to_update) = self.state.open_upvalues.remove(i);

                // Update all closures that reference this stack slot
                for (closure_handle, upvalue_index) in closures_to_update.iter() {
                    self.heap.get_closure_mut(*closure_handle).upvalues[*upvalue_index] =
                        UpvalueReference::Closed(value_handle);
                }
            }
        }
    }

    fn capture_upvalue(
        &mut self,
        stack_slot: StackSlot,
        closure_handle: ClosureHandle,
        upvalue_index: UpvalueIndex,
    ) {
        // Check if there's already an open upvalue for this stack slot
        for (open_slot, closures) in &mut self.state.open_upvalues {
            if *open_slot == stack_slot {
                // Add this closure to the list for this stack slot
                closures.push((closure_handle, upvalue_index));
                self.heap.get_closure_mut(closure_handle).upvalues[upvalue_index] =
                    UpvalueReference::Open(stack_slot);
                return;
            }
        }

        // Create a new open upvalue
        self.heap.get_closure_mut(closure_handle).upvalues[upvalue_index] =
            UpvalueReference::Open(stack_slot);
        self.state
            .open_upvalues
            .push((stack_slot, vec![(closure_handle, upvalue_index)]));
    }

    fn binary_operation<F>(&mut self, op: F) -> RuntimeResult<()>
    where
        F: FnOnce(Value, Value, &mut ObjectHeap) -> Result<Value, BinaryOperationError>,
    {
        #[cfg(feature = "profiler")]
        coz::scope!("binary_operation");

        let b = pop_value!(self);
        let a = pop_value!(self);

        let value = op(a, b, &mut self.heap)
            .map_err(|e: BinaryOperationError| e.into_qang_error(self.state.get_previous_loc()))?;

        push_value!(self, value)?;
        Ok(())
    }

    fn call_value(&mut self, value: Value, arg_count: usize) -> RuntimeResult<()> {
        match value {
            Value::Function(FunctionValueKind::Closure(handle)) => self.call(handle, arg_count),
            Value::Function(FunctionValueKind::NativeFunction(function)) => {
                self.call_native_function(function, arg_count)
            }
            _ => {
                let value_str = value.to_display_string(&self.heap);
                Err(QangRuntimeError::new(
                    format!("Identifier '{}' not callable.", value_str).to_string(),
                    self.state.get_previous_loc(),
                ))
            }
        }
    }

    fn call(&mut self, closure_handle: ClosureHandle, arg_count: usize) -> RuntimeResult<()> {
        #[cfg(feature = "profiler")]
        coz::scope!("call_function");

        #[cfg(feature = "profiler")]
        coz::progress!("before_call");

        let loc = if self.state.frame_count > 0 {
            self.state.get_previous_loc()
        } else {
            SourceLocation::default()
        };

        let final_arg_count = {
            let closure = self.heap.get_closure(closure_handle);
            let function = self.heap.get_function(closure.function);
            if arg_count < function.arity {
                let arity = function.arity;
                for _ in arg_count..arity {
                    push_value!(self, Value::Nil)?;
                }
                arity
            } else {
                arg_count
            }
        };

        if self.state.frame_count >= FRAME_MAX {
            return Err(QangRuntimeError::new("Stack overflow.".to_string(), loc));
        }

        self.state.frame_count += 1;

        let value_slot = self.state.stack_top - final_arg_count - 1; // This should overwrite the function identifier with its return value.
        let call_frame = &mut self.state.frames[self.state.frame_count - 1];

        // Get closure and function, cache function pointer for fast access during execution
        let closure = self.heap.get_closure(closure_handle);
        let function = self.heap.get_function(closure.function);

        call_frame.value_slot = value_slot;
        call_frame.closure = closure_handle;
        call_frame.ip = 0;
        self.state.current_function_ptr = function as *const FunctionObject;

        #[cfg(feature = "profiler")]
        coz::progress!("after_call");

        Ok(())
    }

    pub fn call_function(
        &mut self,
        handle: ClosureHandle,
        args: Vec<Value>,
    ) -> RuntimeResult<Value> {
        let saved_stack_top = self.state.stack_top;
        let saved_frame_count = self.state.frame_count;
        let saved_function_ptr = self.state.current_function_ptr;

        push_value!(self, Value::Function(FunctionValueKind::Closure(handle)))?;

        for value in &args {
            push_value!(self, *value)?;
        }

        self.call(handle, args.len())?;

        match self.run() {
            Ok(return_value) => {
                // Reset the VM state to before the function call
                self.state.stack_top = saved_stack_top;
                self.state.frame_count = saved_frame_count;
                self.state.current_function_ptr = saved_function_ptr;
                Ok(return_value)
            }
            Err(error) => {
                // Reset the VM state to before the function call
                self.state.stack_top = saved_stack_top;
                self.state.frame_count = saved_frame_count;
                self.state.current_function_ptr = saved_function_ptr;
                Err(error)
            }
        }
    }

    fn call_native_function(
        &mut self,
        function: NativeFunction,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        let loc = self.state.get_previous_loc();
        let mut args = vec![Value::Nil; function.arity];

        for i in (0..arg_count).rev() {
            if i < function.arity {
                args[i] = pop_value!(self);
            } else {
                pop_value!(self); // discard values that are passed in but not needed by the function.
            }
        }
        pop_value!(self); // pop function off the stack now that it has been called.

        let value = (function.function)(args.as_slice(), self)
            .map_err(|e: NativeFunctionError| e.into_qang_error(loc))?
            .unwrap_or_default();

        push_value!(self, value)?;

        Ok(())
    }

    fn collect_garbage(&mut self) {
        let roots = self.state.gather_roots();
        self.heap.garbage_collect(&roots);
    }

    pub fn heap(&self) -> &ObjectHeap {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut ObjectHeap {
        &mut self.heap
    }

    pub fn globals(&self) -> &HashMap<StringHandle, Value> {
        &self.state.globals
    }

    fn get_stack_trace(&self) -> Vec<Trace> {
        self.get_stack_trace_from_frames(0..self.state.frame_count)
    }

    fn get_stack_trace_from_frames(&self, frame_id_range: Range<usize>) -> Vec<Trace> {
        let mut traces = Vec::new();

        for frame_idx in frame_id_range {
            let frame = &self.state.frames[frame_idx];
            let closure = self.heap.get_closure(frame.closure);
            let function = self.heap.get_function(closure.function);

            let name = self.heap.get_string(function.name);

            let loc = if frame.ip > 0 {
                function
                    .chunk
                    .locs()
                    .get(frame.ip - 1)
                    .copied()
                    .unwrap_or_default()
            } else {
                SourceLocation::default()
            };

            traces.push(Trace::new(name, loc));
        }

        traces
    }

    fn debug(&self) {
        print!("          ");
        for i in 0..self.state.stack_top {
            if let Some(value) = self.state.stack.get(i) {
                value.print(&self.heap);
                print!(" ");
            }
        }
        println!();

        disassemble_instruction(
            &self.state.get_current_function().chunk,
            &self.heap,
            self.state.frames[self.state.frame_count - 1].ip,
        );
    }
}
