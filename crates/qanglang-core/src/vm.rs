use std::{collections::VecDeque, ops::Range};

#[cfg(feature = "profiler")]
use coz;
use rustc_hash::{FxBuildHasher, FxHashMap};

#[cfg(debug_assertions)]
use crate::debug::disassemble_instruction;
use crate::{
    BoundIntrinsicObject, BoundMethodObject, ClassHandle, HashMapHandle, HeapAllocator, NativeFn,
    NativeFunctionError, NativeFunctionHandle, NativeFunctionObject, QangProgram, QangRuntimeError,
    Value,
    chunk::{OpCode, SourceLocation},
    compiler::{FRAME_MAX, STACK_MAX},
    debug_log,
    error::Trace,
    memory::{
        ClosureHandle, ClosureObject, FunctionObject, IntrinsicKind, IntrinsicMethod, StringHandle,
        UpvalueReference,
    },
    qang_std::{
        qang_array_concat, qang_array_construct, qang_array_get, qang_array_length, qang_array_pop,
        qang_array_push, qang_array_reverse, qang_array_slice, qang_assert, qang_assert_eq,
        qang_assert_throws, qang_hash, qang_print, qang_println, qang_string_to_lowercase,
        qang_string_to_uppercase, qang_system_time, qang_to_string, qang_typeof,
    },
    value::{
        ARRAY_TYPE_STRING, BOOLEAN_TYPE_STRING, CALL_INTRINSIC_STRING, CLASS_INITIALIZER_STRING,
        CLASS_TYPE_STRING, FUNCTION_TYPE_STRING, NIL_TYPE_STRING, NUMBER_TYPE_STRING,
        OBJECT_TYPE_STRING, STRING_TYPE_STRING,
    },
};

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

pub type RuntimeResult<T> = Result<T, QangRuntimeError>;

type StackSlot = usize;
type UpvalueIndex = usize;
type OpenUpvalueEntry = (StackSlot, Vec<(ClosureHandle, UpvalueIndex)>);

macro_rules! push_value {
    ($vm:expr, $value:expr) => {{
        if $vm.state.stack_top >= STACK_MAX {
            return Err(QangRuntimeError::new(
                format!(
                    "Stack overflow: maximum stack size of {} exceeded",
                    STACK_MAX
                ),
                $vm.state.get_current_loc(),
            ));
        }
        $vm.state.stack[$vm.state.stack_top] = $value;
        $vm.state.stack_top += 1;
    }};
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

macro_rules! peek_value {
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

macro_rules! read_string {
    ($vm:expr) => {
        match $vm.state.read_constant() {
            Value::String(handle) => handle,
            _ => {
                return Err(QangRuntimeError::new(
                    "Expected identifier.".to_string(),
                    $vm.state.get_previous_loc(),
                ));
            }
        }
    };
}

#[derive(Debug, Clone, Default)]
struct CallFrame {
    closure: ClosureHandle,
    ip: usize,
    value_slot: usize,
}

#[derive(Clone)]
pub struct VmState {
    stack_top: usize,
    frame_count: usize,
    stack: Vec<Value>,
    frames: [CallFrame; FRAME_MAX],
    globals: FxHashMap<StringHandle, Value>,
    intrinsics: FxHashMap<IntrinsicKind, IntrinsicMethod>,
    open_upvalues: Vec<OpenUpvalueEntry>,
    current_function_ptr: *const FunctionObject,
}

impl VmState {
    fn new(
        globals: FxHashMap<StringHandle, Value>,
        intrinsics: FxHashMap<IntrinsicKind, IntrinsicMethod>,
    ) -> Self {
        Self {
            frame_count: 0,
            stack_top: 0,
            stack: vec![Value::Nil; STACK_MAX],
            frames: std::array::from_fn(|_| CallFrame::default()),
            globals,
            intrinsics,
            open_upvalues: Vec::with_capacity(8),
            current_function_ptr: std::ptr::null(),
        }
    }

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
            .locs
            .get(index)
            .copied()
            .unwrap_or_default()
    }

    fn read_byte(&mut self) -> u8 {
        let frame = unsafe { self.frames.get_unchecked_mut(self.frame_count - 1) };
        debug_assert!(
            !self.current_function_ptr.is_null(),
            "Function pointer is null"
        );

        let code = unsafe { &(*self.current_function_ptr).chunk.code };
        debug_assert!(frame.ip < code.len(), "IP out of bounds");
        let byte = unsafe { *code.get_unchecked(frame.ip) };
        frame.ip += 1;
        byte
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        debug_assert!(
            !self.current_function_ptr.is_null(),
            "Function pointer is null"
        );
        let constants = unsafe { &(*self.current_function_ptr).chunk.constants };
        debug_assert!(index < constants.len(), "Constant index out of bounds");
        constants[index]
    }

    fn read_short(&mut self) -> usize {
        let high_byte = self.read_byte() as usize;
        let low_byte = self.read_byte() as usize;
        (high_byte << 8) | low_byte
    }
}

#[derive(Clone)]
pub struct Vm {
    pub is_debug: bool,
    pub is_gc_enabled: bool,
    pub(crate) state: VmState,
    pub alloc: HeapAllocator,
}

impl Vm {
    pub fn with_gc_check<T>(&mut self, op: impl FnOnce(&mut HeapAllocator) -> T) -> T {
        if self.is_gc_enabled && self.alloc.should_collect_garbage() {
            self.collect_garbage();
        }
        op(&mut self.alloc)
    }

    pub fn new(mut alloc: HeapAllocator) -> Self {
        let mut globals = FxHashMap::with_capacity_and_hasher(64, FxBuildHasher);

        let nil_type_handle = alloc.strings.intern("NIL");
        let nil_type_value_handle = alloc.strings.intern(NIL_TYPE_STRING);
        globals.insert(nil_type_handle, Value::String(nil_type_value_handle));

        let boolean_type_handle = alloc.strings.intern("BOOLEAN");
        let boolean_type_value_handle = alloc.strings.intern(BOOLEAN_TYPE_STRING);
        globals.insert(
            boolean_type_handle,
            Value::String(boolean_type_value_handle),
        );

        let number_type_handle = alloc.strings.intern("NUMBER");
        let number_type_value_handle = alloc.strings.intern(NUMBER_TYPE_STRING);
        globals.insert(number_type_handle, Value::String(number_type_value_handle));

        let string_type_handle = alloc.strings.intern("STRING");
        let string_type_value_handle = alloc.strings.intern(STRING_TYPE_STRING);
        globals.insert(string_type_handle, Value::String(string_type_value_handle));

        let function_type_handle = alloc.strings.intern("FUNCTION");
        let function_type_value_handle = alloc.strings.intern(FUNCTION_TYPE_STRING);
        globals.insert(
            function_type_handle,
            Value::String(function_type_value_handle),
        );
        let class_type_handle = alloc.strings.intern("CLASS");
        let class_type_value_handle = alloc.strings.intern(CLASS_TYPE_STRING);
        globals.insert(class_type_handle, Value::String(class_type_value_handle));
        let object_type_handle = alloc.strings.intern("OBJECT");
        let object_type_value_handle = alloc.strings.intern(OBJECT_TYPE_STRING);
        globals.insert(object_type_handle, Value::String(object_type_value_handle));
        let array_type_handle = alloc.strings.intern("ARRAY");
        let array_type_value_handle = alloc.strings.intern(ARRAY_TYPE_STRING);
        globals.insert(array_type_handle, Value::String(array_type_value_handle));

        let mut intrinsics = FxHashMap::with_hasher(FxBuildHasher);
        let to_uppercase_handle = alloc.strings.intern("to_uppercase");
        intrinsics.insert(
            IntrinsicKind::String(to_uppercase_handle),
            IntrinsicMethod {
                function: qang_string_to_uppercase,
                arity: 0,
            },
        );
        let to_lowercase_handle = alloc.strings.intern("to_lowercase");
        intrinsics.insert(
            IntrinsicKind::String(to_lowercase_handle),
            IntrinsicMethod {
                function: qang_string_to_lowercase,
                arity: 0,
            },
        );
        let array_length_handle = alloc.strings.intern("length");
        intrinsics.insert(
            IntrinsicKind::Array(array_length_handle),
            IntrinsicMethod {
                function: qang_array_length,
                arity: 0,
            },
        );
        let array_push_handle = alloc.strings.intern("push");
        intrinsics.insert(
            IntrinsicKind::Array(array_push_handle),
            IntrinsicMethod {
                function: qang_array_push,
                arity: 1,
            },
        );
        let array_pop_handle = alloc.strings.intern("pop");
        intrinsics.insert(
            IntrinsicKind::Array(array_pop_handle),
            IntrinsicMethod {
                function: qang_array_pop,
                arity: 0,
            },
        );
        let array_reverse_handle = alloc.strings.intern("reverse");
        intrinsics.insert(
            IntrinsicKind::Array(array_reverse_handle),
            IntrinsicMethod {
                function: qang_array_reverse,
                arity: 0,
            },
        );
        let array_slice_handle = alloc.strings.intern("slice");
        intrinsics.insert(
            IntrinsicKind::Array(array_slice_handle),
            IntrinsicMethod {
                function: qang_array_slice,
                arity: 2,
            },
        );
        let array_get_handle = alloc.strings.intern("get");
        intrinsics.insert(
            IntrinsicKind::Array(array_get_handle),
            IntrinsicMethod {
                function: qang_array_get,
                arity: 1,
            },
        );
        let array_concat_handle = alloc.strings.intern("concat");
        intrinsics.insert(
            IntrinsicKind::Array(array_concat_handle),
            IntrinsicMethod {
                function: qang_array_concat,
                arity: 1,
            },
        );

        let vm = Self {
            is_debug: false,
            is_gc_enabled: true,
            state: VmState::new(globals, intrinsics),
            alloc,
        };

        vm.add_native_function("assert", 2, qang_assert)
            .add_native_function("assert_eq", 3, qang_assert_eq)
            .add_native_function("assert_throws", 2, qang_assert_throws)
            .add_native_function("print", 1, qang_print)
            .add_native_function("println", 1, qang_println)
            .add_native_function("system_time", 0, qang_system_time)
            .add_native_function("typeof", 1, qang_typeof)
            .add_native_function("to_string", 1, qang_to_string)
            .add_native_function("hash", 1, qang_hash)
            .add_native_function("Array", 2, qang_array_construct)
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    pub fn set_gc_status(mut self, is_enabled: bool) -> Self {
        self.is_gc_enabled = is_enabled;
        self
    }

    pub fn add_native_function(mut self, name: &str, arity: usize, function: NativeFn) -> Self {
        let identifier_handle = self.alloc.strings.intern(name);
        let native_function = NativeFunctionObject {
            name_handle: identifier_handle,
            arity,
            function,
        };
        let handle = self.alloc.allocate_native_function(native_function);

        self.state
            .globals
            .insert(identifier_handle, Value::NativeFunction(handle));

        self
    }

    pub fn interpret(&mut self, program: QangProgram) -> RuntimeResult<()> {
        let function_handle = program.into_handle();
        let upvalue_count = self.alloc.get_function(function_handle).upvalue_count;

        let handle = self
            .alloc
            .allocate_closure(ClosureObject::new(function_handle, upvalue_count));
        push_value!(self, Value::Closure(handle));
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

            #[cfg(debug_assertions)]
            {
                if self.is_debug {
                    self.debug_print();
                }
            }

            let opcode: OpCode = self.state.read_byte().into();

            match opcode {
                OpCode::Constant => {
                    let constant = self.state.read_constant();
                    push_value!(self, constant);
                }
                OpCode::Negate => {
                    if let Value::Number(number) = peek_value!(self, 0) {
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
                    let value = peek_value!(self, 0);
                    if let Some(stack_value) = self.state.stack.get_mut(self.state.stack_top - 1) {
                        *stack_value = (!value.is_truthy()).into();
                    }
                }
                OpCode::True => {
                    push_value!(self, Value::True);
                }
                OpCode::False => {
                    push_value!(self, Value::False);
                }
                OpCode::Nil => {
                    push_value!(self, Value::Nil);
                }
                OpCode::Add => {
                    self.binary_operation(|a, b, alloc| match (&a, &b) {
                        (Value::Number(num1), Value::Number(num2)) => {
                            Ok(Value::Number(num1 + num2))
                        }
                        (Value::String(handle1), Value::String(handle2)) => {
                            #[cfg(feature = "profiler")]
                            coz::scope!("string_concatenation");
                            Ok(Value::String(
                                alloc.strings.concat_strings(*handle1, *handle2),
                            ))
                        }
                        (Value::Array(handle1), Value::Array(handle2)) => {
                            Ok(Value::Array(alloc.arrays.concat(*handle1, *handle2)))
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
                        (Value::Array(_), _) => {
                            Err(format!("Cannot add an array to {}.", b.to_type_string())
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
                        (_, Value::Array(_)) => {
                            Err(format!("Cannot add {} to an array.", a.to_type_string())
                                .as_str()
                                .into())
                        }
                        _ => Err("Both operands must be a numbers, strings or arrays.".into()),
                    })?;
                }
                OpCode::Subtract => self.binary_operation(|a, b, _allocator| match (&a, &b) {
                    (Value::Number(num1), Value::Number(num2)) => Ok((num1 - num2).into()),
                    _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                })?,
                OpCode::Multiply => self.binary_operation(|a, b, _allocator| match (&a, &b) {
                    (Value::Number(num1), Value::Number(num2)) => Ok((num1 * num2).into()),
                    _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                })?,
                OpCode::Divide => self.binary_operation(|a, b, _allocator| match (&a, &b) {
                    (Value::Number(num1), Value::Number(num2)) => {
                        if num2 == &0.0 {
                            Err(BinaryOperationError::new("Cannot divide by zero."))
                        } else {
                            Ok((num1 / num2).into())
                        }
                    }
                    _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                })?,
                OpCode::Modulo => self.binary_operation(|a, b, _allocator| match (&a, &b) {
                    (Value::Number(num1), Value::Number(num2)) => Ok((num1 % num2).into()),
                    _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                })?,
                OpCode::Equal => self.binary_operation(|a, b, _allocator| Ok((a == b).into()))?,
                OpCode::Greater => self.binary_operation(|a, b, _allocator| match (&a, &b) {
                    (Value::Number(num1), Value::Number(num2)) => Ok((num1 > num2).into()),
                    _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                })?,
                OpCode::GreaterEqual => {
                    self.binary_operation(|a, b, _allocator| match (&a, &b) {
                        (Value::Number(num1), Value::Number(num2)) => Ok((num1 >= num2).into()),
                        _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                    })?
                }
                OpCode::Less => self.binary_operation(|a, b, _allocator| match (&a, &b) {
                    (Value::Number(num1), Value::Number(num2)) => Ok((num1 < num2).into()),
                    _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                })?,
                OpCode::LessEqual => self.binary_operation(|a, b, _allocator| match (&a, &b) {
                    (Value::Number(num1), Value::Number(num2)) => Ok((num1 <= num2).into()),
                    _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                })?,
                OpCode::Pop => {
                    pop_value!(self);
                }
                OpCode::DefineGlobal => {
                    let identifier_handle = read_string!(self);
                    let value = pop_value!(self);
                    self.state.globals.insert(identifier_handle, value);
                }
                OpCode::GetGlobal => {
                    let identifier_handle = read_string!(self);
                    let value = *self.state.globals.get(&identifier_handle).ok_or_else(|| {
                        let loc = self.state.get_previous_loc();
                        let identifier_name = self.alloc.strings.get_string(identifier_handle);
                        QangRuntimeError::new(
                            format!("Undefined variable: {}.", identifier_name),
                            loc,
                        )
                    })?;
                    push_value!(self, value);
                }
                OpCode::SetGlobal => {
                    let identifier_handle = read_string!(self);

                    if !self.state.globals.contains_key(&identifier_handle) {
                        let identifier_name = self.alloc.strings.get_string(identifier_handle);
                        let loc = self.state.get_previous_loc();
                        return Err(QangRuntimeError::new(
                            format!("Undefined variable: {}.", identifier_name).to_string(),
                            loc,
                        ));
                    }
                    let value = peek_value!(self, 0);
                    self.state.globals.insert(identifier_handle, value);
                }
                OpCode::GetLocal => {
                    let slot = self.state.read_byte();
                    let absolute_slot =
                        self.state.frames[self.state.frame_count - 1].value_slot + slot as usize;
                    debug_assert!(
                        absolute_slot < STACK_MAX,
                        "Local slot {} out of bounds",
                        absolute_slot
                    );

                    let value = self.state.stack[absolute_slot];
                    push_value!(self, value);
                }
                OpCode::SetLocal => {
                    let slot = self.state.read_byte();
                    let value = peek_value!(self, 0);
                    let absolute_slot =
                        self.state.frames[self.state.frame_count - 1].value_slot + slot as usize;

                    self.state.stack[absolute_slot] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.state.read_short();
                    if !peek_value!(self, 0).is_truthy() {
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
                    let function_value = peek_value!(self, arg_count);
                    self.call_value(function_value, arg_count)?;
                }
                OpCode::Closure => {
                    let constant = self.state.read_constant();
                    let handle = match constant {
                        Value::FunctionDecl(handle) => handle,
                        _ => {
                            return Err(QangRuntimeError::new(
                                "Expected function.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                    };
                    let upvalue_count = self.alloc.get_function(handle).upvalue_count;
                    let closure_handle = self.with_gc_check(|alloc| {
                        alloc.allocate_closure(ClosureObject::new(handle, upvalue_count))
                    });

                    for i in 0..self.alloc.get_closure(closure_handle).upvalue_count {
                        let is_local = self.state.read_byte() != 0;
                        let index = self.state.read_byte() as usize;

                        if is_local {
                            let stack_slot =
                                self.state.frames[self.state.frame_count - 1].value_slot + index;
                            self.capture_upvalue(stack_slot, closure_handle, i);
                        } else {
                            let current_closure = self
                                .alloc
                                .get_closure(self.state.frames[self.state.frame_count - 1].closure);
                            let current_upvalue = current_closure.upvalues[index];
                            self.alloc.get_closure_mut(closure_handle).upvalues[i] =
                                current_upvalue;
                        }
                    }
                    push_value!(self, Value::Closure(closure_handle));
                }
                OpCode::GetUpvalue => {
                    let slot = self.state.read_byte() as usize;
                    let current_closure = self
                        .alloc
                        .get_closure(self.state.frames[self.state.frame_count - 1].closure);
                    let upvalue = current_closure.upvalues[slot];

                    match upvalue {
                        UpvalueReference::Open(stack_slot) => {
                            let value = self.state.stack[stack_slot];
                            push_value!(self, value);
                        }
                        UpvalueReference::Closed(value_handle) => {
                            let value = *self.alloc.get_upvalue(value_handle);
                            push_value!(self, value);
                        }
                    }
                }
                OpCode::SetUpvalue => {
                    let slot = self.state.read_byte() as usize;
                    let value = peek_value!(self, 0);
                    let current_closure_handle =
                        self.state.frames[self.state.frame_count - 1].closure;

                    let upvalue = self.alloc.get_closure(current_closure_handle).upvalues[slot];
                    match upvalue {
                        UpvalueReference::Open(stack_slot) => {
                            self.state.stack[stack_slot] = value;
                        }
                        UpvalueReference::Closed(value_handle) => {
                            *self.alloc.get_upvalue_mut(value_handle) = value;
                        }
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalue(self.state.stack_top - 1);
                    pop_value!(self);
                }
                OpCode::Class => {
                    let identifier_handle = read_string!(self);
                    let class_handle =
                        self.with_gc_check(|alloc| alloc.allocate_class(identifier_handle));
                    push_value!(self, Value::Class(class_handle))
                }
                OpCode::SetProperty => {
                    let instance = peek_value!(self, 1);
                    if let Value::Instance(instance_handle) = instance {
                        let instance_table = self.alloc.get_instance(instance_handle).table;
                        let constant = self.state.read_constant();
                        if !matches!(constant, Value::String(_)) {
                            return Err(QangRuntimeError::new(
                                "Expected property name as string".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                        let value = peek_value!(self, 0);
                        self.with_gc_check(|alloc| {
                            alloc.set_instance_field(instance_table, constant, value)
                        });
                        let value = pop_value!(self); // value to assign
                        pop_value!(self); // instance
                        push_value!(self, value); // push assigned value to top of stack
                    } else {
                        return Err(QangRuntimeError::new(
                            format!("Cannot access properties on {}.", instance.to_type_string()),
                            self.state.get_previous_loc(),
                        ));
                    }
                }
                OpCode::GetProperty => {
                    let value = peek_value!(self, 0);

                    match value {
                        Value::Instance(instance_handle) => {
                            let instance = self.alloc.get_instance(instance_handle);
                            let constant = self.state.read_constant();
                            if !matches!(constant, Value::String(_)) {
                                return Err(QangRuntimeError::new(
                                    "Expected property name as string".to_string(),
                                    self.state.get_previous_loc(),
                                ));
                            }

                            if let Some(value) =
                                self.alloc.get_instance_field(instance.table, constant)
                            {
                                pop_value!(self);
                                push_value!(self, value);
                            } else {
                                self.bind_method(instance.clazz, constant)?;
                            }
                        }
                        Value::String(_) => {
                            let identifer = read_string!(self);
                            self.bind_intrinsic_method(
                                identifer,
                                IntrinsicKind::String(identifer),
                                value,
                            )?;
                        }
                        Value::Array(_) => {
                            let identifer = read_string!(self);
                            self.bind_intrinsic_method(
                                identifer,
                                IntrinsicKind::Array(identifer),
                                value,
                            )?;
                        }
                        Value::Closure(_) => {
                            let identifier = read_string!(self);
                            if identifier == self.alloc.strings.intern(CALL_INTRINSIC_STRING) {
                                // do nothing, because function.call == function
                            } else {
                                return Err(QangRuntimeError::new(
                                    format!(
                                        "Cannot access properties from {}.",
                                        value.to_type_string()
                                    ),
                                    self.state.get_previous_loc(),
                                ));
                            }
                        }
                        _ => {
                            return Err(QangRuntimeError::new(
                                format!(
                                    "Cannot access properties from {}.",
                                    value.to_type_string()
                                ),
                                self.state.get_previous_loc(),
                            ));
                        }
                    }
                }
                OpCode::Method => {
                    let identifier_handle = read_string!(self);
                    self.define_method(identifier_handle)?;
                }
                OpCode::InitField => {
                    let identifier_handle = read_string!(self);
                    let value = pop_value!(self);
                    self.init_field(identifier_handle, value)?;
                }
                OpCode::Invoke => {
                    let method_handle = read_string!(self);
                    let arg_count = self.state.read_byte();
                    self.invoke(method_handle, arg_count as usize)?;
                }
                OpCode::Inherit => {
                    let superclass = peek_value!(self, 0);
                    let subclass = peek_value!(self, 1);

                    match (superclass, subclass) {
                        (Value::Class(superclass), Value::Class(subclass)) => {
                            let superclass = self.alloc.get_class(superclass);
                            let subclass = self.alloc.get_class(subclass);
                            let subclass_value_table = subclass.value_table;
                            let superclass_value_table = superclass.value_table;
                            let superclass_method_table = superclass.method_table;
                            let subclass_method_table = subclass.method_table;
                            self.alloc
                                .tables
                                .copy_into(superclass_method_table, subclass_method_table);
                            self.alloc
                                .tables
                                .copy_into(superclass_value_table, subclass_value_table);
                            pop_value!(self);
                            Ok(())
                        }
                        (Value::Class(_), _) => Err(QangRuntimeError::new(
                            "Invalid subclass.".to_string(),
                            self.state.get_previous_loc(),
                        )),
                        (_, Value::Class(_)) => Err(QangRuntimeError::new(
                            "Super class must be a class.".to_string(),
                            self.state.get_previous_loc(),
                        )),
                        _ => Err(QangRuntimeError::new(
                            "Invalid class declaration.".to_string(),
                            self.state.get_previous_loc(),
                        )),
                    }?
                }
                OpCode::GetSuper => {
                    let property_handle = read_string!(self);
                    let superclass = pop_value!(self);

                    if let Value::Class(superclass_handle) = superclass {
                        let superclass_obj = self.alloc.get_class(superclass_handle);

                        if let Some(field_value) = self.alloc.get_class_method(
                            superclass_obj.value_table,
                            Value::String(property_handle),
                        ) {
                            self.state.stack[self.state.stack_top - 1] = field_value; // replace 'this' with the value of the field.
                        } else if !self
                            .bind_super_method(superclass_obj.method_table, property_handle)?
                        {
                            pop_value!(self); // Pop 'this'
                            push_value!(self, Value::Nil);
                        }
                    } else {
                        return Err(QangRuntimeError::new(
                            "Super class must be a class.".to_string(),
                            self.state.get_previous_loc(),
                        ));
                    }
                }
                OpCode::SuperInvoke => {
                    let method_handle = read_string!(self);
                    let arg_count = self.state.read_byte() as usize;
                    let superclass = pop_value!(self);

                    if let Value::Class(superclass_handle) = superclass {
                        self.invoke_from_class(superclass_handle, method_handle, arg_count)?;
                    } else {
                        return Err(QangRuntimeError::new(
                            "Super class must be a class.".to_string(),
                            self.state.get_previous_loc(),
                        ));
                    }
                }
                OpCode::ArrayLiteral => {
                    let length = self.state.read_byte() as usize;
                    let array = self.with_gc_check(|alloc| alloc.arrays.create_array(length));
                    for i in (0..length).rev() {
                        let value = pop_value!(self);
                        self.alloc.arrays.insert(array, i, value);
                    }
                    push_value!(self, Value::Array(array));
                }
                OpCode::GetArrayIndex => {
                    let index = pop_value!(self);
                    match (index, peek_value!(self, 0)) {
                        (Value::Number(index), Value::Array(handle)) => {
                            let value = self.alloc.arrays.get(handle, index.trunc() as isize); // TODO verify this is an int instead of coercing it.
                            pop_value!(self);
                            push_value!(self, value);
                        }
                        (_, Value::Array(_)) => {
                            return Err(QangRuntimeError::new(
                                "An array can only be indexed by a number.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                        (Value::Number(_), _) => {
                            return Err(QangRuntimeError::new(
                                "Only arrays can be indexed.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                        _ => {
                            return Err(QangRuntimeError::new(
                                "Invalid operation.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                    }
                }
                OpCode::SetArrayIndex => {
                    let value = pop_value!(self);
                    let index = pop_value!(self);
                    match (index, peek_value!(self, 0)) {
                        (Value::Number(index), Value::Array(handle)) => {
                            let is_within_bounds =
                                self.alloc
                                    .arrays
                                    .insert(handle, index.trunc() as usize, value);

                            if !is_within_bounds {
                                return Err(QangRuntimeError::new(
                                    "Index out of bounds.".to_string(),
                                    self.state.get_previous_loc(),
                                ));
                            }
                            pop_value!(self);
                            push_value!(self, value);
                        }
                        (_, Value::Array(_)) => {
                            return Err(QangRuntimeError::new(
                                "An array can only be indexed by a number.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                        (Value::Number(_), _) => {
                            return Err(QangRuntimeError::new(
                                "Only arrays can be indexed.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                        _ => {
                            return Err(QangRuntimeError::new(
                                "Invalid operation.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                    }
                }
                OpCode::ObjectLiteral => {
                    let handle = self.alloc.tables.new_hashmap();
                    push_value!(self, Value::ObjectLiteral(handle));
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
                    let previous_closure = self.alloc.get_closure(previous_frame.closure);
                    let previous_function = self.alloc.get_function(previous_closure.function);
                    self.state.current_function_ptr = previous_function as *const FunctionObject;

                    self.state.stack_top = value_slot + 1;
                    self.state.stack[value_slot] = result;
                }
            };
        }
    }

    fn define_method(&mut self, name: StringHandle) -> RuntimeResult<()> {
        if let Value::Closure(method) = peek_value!(self, 0)
            && let Value::Class(clazz_handle) = peek_value!(self, 1)
        {
            let clazz = self.alloc.get_class(clazz_handle);
            self.alloc.set_class_method(
                clazz.method_table,
                Value::String(name),
                Value::Closure(method),
            );
            pop_value!(self);
        }

        Ok(())
    }

    fn init_field(&mut self, field_name: StringHandle, value: Value) -> RuntimeResult<()> {
        if let Value::Class(clazz_handle) = peek_value!(self, 0) {
            let clazz = self.alloc.get_class(clazz_handle);
            self.alloc
                .set_class_method(clazz.value_table, Value::String(field_name), value);
        }

        Ok(())
    }

    fn bind_method(&mut self, clazz_handle: ClassHandle, method_name: Value) -> RuntimeResult<()> {
        let clazz = self.alloc.get_class(clazz_handle);
        if let Some(Value::Closure(closure)) =
            self.alloc.get_class_method(clazz.method_table, method_name)
        {
            let receiver = peek_value!(self, 0);
            let bound = BoundMethodObject::new(receiver, closure);
            let handle = self.with_gc_check(|alloc| alloc.allocate_bound_method(bound));
            pop_value!(self);
            push_value!(self, Value::BoundMethod(handle));
        } else {
            pop_value!(self);
            push_value!(self, Value::Nil);
        }
        Ok(())
    }

    fn bind_intrinsic_method(
        &mut self,
        method_name: StringHandle,
        kind: IntrinsicKind,
        receiver: Value,
    ) -> RuntimeResult<()> {
        let intrinsic = *self.state.intrinsics.get(&kind).ok_or_else(|| {
            QangRuntimeError::new(
                "invalid method call.".to_string(),
                self.state.get_previous_loc(),
            )
        })?;
        let bound = BoundIntrinsicObject::new(receiver, intrinsic, method_name);
        let handle = self.with_gc_check(|alloc| alloc.allocate_bound_intrinsic(bound));
        pop_value!(self);
        push_value!(self, Value::BoundIntrinsic(handle));
        Ok(())
    }

    fn bind_super_method(
        &mut self,
        method_table_handle: HashMapHandle,
        method_name: StringHandle,
    ) -> RuntimeResult<bool> {
        if let Some(Value::Closure(closure)) = self
            .alloc
            .get_class_method(method_table_handle, Value::String(method_name))
        {
            let receiver = peek_value!(self, 0);
            let bound = BoundMethodObject::new(receiver, closure);
            let handle = self.with_gc_check(|alloc| alloc.allocate_bound_method(bound));
            pop_value!(self); // Pop 'this'
            push_value!(self, Value::BoundMethod(handle));
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn close_upvalue(&mut self, last_slot: StackSlot) {
        let mut i = self.state.open_upvalues.len();
        while i > 0 {
            i -= 1;
            let stack_slot = self.state.open_upvalues[i].0;

            if stack_slot >= last_slot {
                let value = self.state.stack[stack_slot];

                let value_handle = self.with_gc_check(|alloc| alloc.allocate_upvalue(value));

                // Extract the closures list by removing and then updating
                let (_, closures_to_update) = self.state.open_upvalues.remove(i);

                // Update all closures that reference this stack slot
                for (closure_handle, upvalue_index) in closures_to_update.iter() {
                    self.alloc.get_closure_mut(*closure_handle).upvalues[*upvalue_index] =
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
                self.alloc.get_closure_mut(closure_handle).upvalues[upvalue_index] =
                    UpvalueReference::Open(stack_slot);
                return;
            }
        }

        // Create a new open upvalue
        self.alloc.get_closure_mut(closure_handle).upvalues[upvalue_index] =
            UpvalueReference::Open(stack_slot);
        self.state
            .open_upvalues
            .push((stack_slot, vec![(closure_handle, upvalue_index)]));
    }

    fn binary_operation<F>(&mut self, op: F) -> RuntimeResult<()>
    where
        F: FnOnce(Value, Value, &mut HeapAllocator) -> Result<Value, BinaryOperationError>,
    {
        #[cfg(feature = "profiler")]
        coz::scope!("binary_operation");

        let b = pop_value!(self);
        let a = pop_value!(self);

        let value = op(a, b, &mut self.alloc)
            .map_err(|e: BinaryOperationError| e.into_qang_error(self.state.get_previous_loc()))?;

        push_value!(self, value);
        Ok(())
    }

    fn call_value(&mut self, value: Value, arg_count: usize) -> RuntimeResult<()> {
        match value {
            Value::Closure(handle) => self.call(handle, arg_count),
            Value::NativeFunction(function) => self.call_native_function(function, arg_count),
            Value::Class(handle) => {
                let constructor_handle = self.alloc.strings.intern(CLASS_INITIALIZER_STRING);
                let clazz = self.alloc.get_class(handle);
                let clazz_method_table = clazz.method_table;
                let value_method_table = clazz.value_table;
                let insance_handle = self.with_gc_check(|alloc| alloc.allocate_instance(handle));
                self.state.stack[self.state.stack_top - arg_count - 1] =
                    Value::Instance(insance_handle);
                let instance_table = self.alloc.get_instance(insance_handle).table;

                self.alloc
                    .tables
                    .copy_into(value_method_table, instance_table);

                if let Some(Value::Closure(constructor)) = self
                    .alloc
                    .get_class_method(clazz_method_table, Value::String(constructor_handle))
                {
                    self.call(constructor, arg_count)?;
                }
                Ok(())
            }
            Value::BoundMethod(handle) => {
                let bound_method = self.alloc.get_bound_method(handle);

                // Replace the method function with the receiver in the stack
                // so that when the method is called, 'this' (slot 0) contains the receiver
                self.state.stack[self.state.stack_top - arg_count - 1] = bound_method.receiver;
                self.call(bound_method.closure, arg_count)
            }
            Value::BoundIntrinsic(handle) => {
                let bound_intrinsic = self.alloc.get_bound_intrinsic(handle);
                self.call_intrinsic_method(
                    bound_intrinsic.receiver,
                    bound_intrinsic.method,
                    arg_count,
                )
            }
            _ => {
                let value_str = value.to_display_string(&self.alloc);
                Err(QangRuntimeError::new(
                    format!("Identifier '{}' not callable.", value_str).to_string(),
                    self.state.get_previous_loc(),
                ))
            }
        }
    }

    fn invoke(&mut self, method_handle: StringHandle, arg_count: usize) -> RuntimeResult<()> {
        let receiver = peek_value!(self, arg_count);

        match receiver {
            Value::Instance(instance_handle) => {
                let instance = self.alloc.get_instance(instance_handle);

                if let Some(value) = self
                    .alloc
                    .get_instance_field(instance.table, Value::String(method_handle))
                {
                    self.state.stack[self.state.stack_top - arg_count - 1] = value;
                    return self.call_value(value, arg_count);
                }

                self.invoke_from_class(instance.clazz, method_handle, arg_count)
            }
            Value::String(_) => {
                let intrinsic = *self
                    .state
                    .intrinsics
                    .get(&IntrinsicKind::String(method_handle))
                    .ok_or_else(|| {
                        QangRuntimeError::new(
                            "invalid method call.".to_string(),
                            self.state.get_previous_loc(),
                        )
                    })?;
                self.call_intrinsic_method(receiver, intrinsic, arg_count)
            }
            Value::Array(_) => {
                let intrinsic = *self
                    .state
                    .intrinsics
                    .get(&IntrinsicKind::Array(method_handle))
                    .ok_or_else(|| {
                        QangRuntimeError::new(
                            "invalid method call.".to_string(),
                            self.state.get_previous_loc(),
                        )
                    })?;
                self.call_intrinsic_method(receiver, intrinsic, arg_count)
            }
            Value::Closure(closure_handle) => {
                if method_handle == self.alloc.strings.intern(CALL_INTRINSIC_STRING) {
                    if let Value::Array(array_handle) = peek_value!(self, 0) {
                        pop_value!(self);
                        let array_length = self.alloc.arrays.length(array_handle);
                        for value in self.alloc.arrays.iter(array_handle) {
                            push_value!(self, value);
                        }
                        self.call(closure_handle, array_length)
                    } else {
                        Err(QangRuntimeError::new(
                            "'call' must take one argument and it must be an array.".to_string(),
                            self.state.get_previous_loc(),
                        ))
                    }
                } else {
                    Err(QangRuntimeError::new(
                        format!(
                            "Cannot invoke {}, no methods exist.",
                            receiver.to_type_string()
                        ),
                        self.state.get_previous_loc(),
                    ))
                }
            }
            _ => Err(QangRuntimeError::new(
                format!(
                    "Cannot invoke {}, no methods exist.",
                    receiver.to_type_string()
                ),
                self.state.get_previous_loc(),
            )),
        }
    }

    pub(crate) fn call(
        &mut self,
        closure_handle: ClosureHandle,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        #[cfg(feature = "profiler")]
        coz::scope!("call_function");

        #[cfg(feature = "profiler")]
        coz::progress!("before_call");

        // Get closure and function, cache function pointer for fast access during execution
        let closure = self.alloc.get_closure(closure_handle);
        let function = self.alloc.get_function(closure.function);

        let final_arg_count = {
            if arg_count < function.arity {
                let arity = function.arity;
                for _ in arg_count..arity {
                    push_value!(self, Value::Nil);
                }
                arity
            } else {
                arg_count
            }
        };

        if self.state.frame_count >= FRAME_MAX {
            let loc = if self.state.frame_count > 0 {
                self.state.get_previous_loc()
            } else {
                SourceLocation::default()
            };
            return Err(QangRuntimeError::new(
                "Maximum call depth exceeded.".to_string(),
                loc,
            ));
        }

        self.state.frame_count += 1;

        let value_slot = self.state.stack_top - final_arg_count - 1;
        let call_frame = &mut self.state.frames[self.state.frame_count - 1];

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

        push_value!(self, Value::Closure(handle));

        for value in &args {
            push_value!(self, *value);
        }

        self.call(handle, args.len())?;

        let result = self.run();

        self.state.stack_top = saved_stack_top;
        self.state.frame_count = saved_frame_count;
        self.state.current_function_ptr = saved_function_ptr;

        result
    }

    fn call_native_function(
        &mut self,
        handle: NativeFunctionHandle,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        let function = self.alloc.get_native_function(handle);
        let mut args = [Value::Nil; 256];

        for i in (0..arg_count).rev() {
            if i < function.arity {
                args[i] = pop_value!(self);
            } else {
                pop_value!(self); // discard values that are passed in but not needed by the function.
            }
        }

        pop_value!(self); // pop function off the stack now that it has been called.

        let value = (function.function)(&args[..function.arity], self)
            .map_err(|e: NativeFunctionError| {
                let loc = self.state.get_previous_loc();
                e.into_qang_error(loc)
            })?
            .unwrap_or_default();

        push_value!(self, value);

        Ok(())
    }

    fn call_intrinsic_method(
        &mut self,
        receiver: Value,
        method: IntrinsicMethod,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        let mut args = [Value::Nil; 256];

        for i in (0..arg_count).rev() {
            if i < method.arity {
                args[i] = pop_value!(self);
            } else {
                pop_value!(self); // discard values that are passed in but not needed by the function.
            }
        }

        pop_value!(self); // pop function off the stack now that it has been called.

        let value = (method.function)(receiver, &args[..method.arity], self)
            .map_err(|e: NativeFunctionError| {
                let loc = self.state.get_previous_loc();
                e.into_qang_error(loc)
            })?
            .unwrap_or_default();

        push_value!(self, value);

        Ok(())
    }

    fn invoke_from_class(
        &mut self,
        clazz_handle: ClassHandle,
        method_handle: StringHandle,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        let clazz = self.alloc.get_class(clazz_handle);
        if let Some(Value::Closure(method)) = self
            .alloc
            .get_class_method(clazz.method_table, Value::String(method_handle))
        {
            self.call(method, arg_count)
        } else {
            // If method is `init` and it does not exist, do nothing (return nil)
            let method_name = self.alloc.strings.get_string(method_handle);
            if method_name == CLASS_INITIALIZER_STRING {
                // Pop arguments but leave receiver on stack
                for _ in 0..arg_count {
                    pop_value!(self);
                }
                // Push nil as return value
                push_value!(self, Value::Nil);
                Ok(())
            } else {
                Err(QangRuntimeError::new(
                    format!(
                        "{} does not exist on class {}.",
                        Value::String(method_handle).to_display_string(&self.alloc),
                        Value::Class(clazz_handle).to_display_string(&self.alloc),
                    ),
                    self.state.get_previous_loc(),
                ))
            }
        }
    }

    pub fn gather_roots(&mut self) -> VecDeque<Value> {
        let capacity = self.state.stack_top
            + self.globals().len()
            + self.state.frame_count
            + self.state.open_upvalues.len();
        let mut closure_roots = VecDeque::with_capacity(capacity);
        closure_roots.extend(&self.state.stack[..self.state.stack_top]);
        closure_roots.extend(self.globals().values());

        for frame in &self.state.frames[..self.state.frame_count] {
            closure_roots.push_back(Value::Closure(frame.closure));
        }

        for (_, closures) in &self.state.open_upvalues {
            for (closure_handle, _) in closures {
                closure_roots.push_back(Value::Closure(*closure_handle));
            }
        }

        closure_roots
    }

    pub fn collect_garbage(&mut self) {
        debug_log!(self.is_debug, "--gc begin");
        let roots = self.gather_roots();
        self.alloc.collect_garbage(roots);
        debug_log!(self.is_debug, "--gc end");
    }

    pub fn globals(&self) -> &FxHashMap<StringHandle, Value> {
        &self.state.globals
    }

    fn get_stack_trace(&self) -> Vec<Trace> {
        self.get_stack_trace_from_frames(0..self.state.frame_count)
    }

    fn get_stack_trace_from_frames(&self, frame_id_range: Range<usize>) -> Vec<Trace> {
        let mut traces = Vec::new();

        for frame_idx in frame_id_range {
            let frame = &self.state.frames[frame_idx];
            let closure = self.alloc.get_closure(frame.closure);
            let function = self.alloc.get_function(closure.function);

            let name = self.alloc.strings.get_string(function.name);

            let loc = if frame.ip > 0 {
                function
                    .chunk
                    .locs
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

    #[cfg(debug_assertions)]
    fn debug_print(&self) {
        print!("          ");
        for i in 0..self.state.stack_top {
            if let Some(value) = self.state.stack.get(i) {
                value.print(&self.alloc);
                print!(" ");
            }
        }
        println!();

        disassemble_instruction(
            &self.state.get_current_function().chunk,
            &self.alloc,
            self.state.frames[self.state.frame_count - 1].ip,
        );
    }
}
