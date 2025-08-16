use std::{cell::RefCell, collections::HashMap, ops::Range, rc::Rc};

#[cfg(feature = "profiler")]
use coz;

use crate::{
    ObjectHeap, QangProgram, QangRuntimeError, Value,
    chunk::{OpCode, SourceLocation},
    compiler::{FRAME_MAX, STACK_MAX},
    debug::disassemble_instruction,
    error::{Trace, ValueConversionError},
    memory::{ClosureHandle, FunctionHandle, StringHandle},
    object::{ClosureObject, FunctionObject, Upvalue},
    qang_std::{
        qang_assert, qang_assert_eq, qang_assert_throws, qang_print, qang_println,
        qang_system_time, qang_to_string, qang_typeof,
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

macro_rules! push_value {
    ($vm:expr, $value:expr) => {
        if $vm.stack_top >= STACK_MAX {
            Err(QangRuntimeError::new(
                format!(
                    "Stack overflow: maximum stack size of {} exceeded",
                    STACK_MAX
                ),
                $vm.get_current_loc(),
            ))
        } else {
            $vm.stack[$vm.stack_top] = $value;
            $vm.stack_top += 1;
            Ok(())
        }
    };
}

macro_rules! pop_value {
    ($vm:expr) => {
        if $vm.stack_top > 0 {
            $vm.stack_top -= 1;
            $vm.stack[$vm.stack_top]
        } else {
            panic!("Stack underflow: unexpected empty stack.")
        }
    };
}

macro_rules! get_current_frame {
    ($vm:expr) => {
        &$vm.frames[$vm.frame_count - 1]
    };
}

macro_rules! get_current_frame_mut {
    ($vm:expr) => {
        &mut $vm.frames[$vm.frame_count - 1]
    };
}

macro_rules! get_current_function {
    ($vm:expr) => {
        &get_current_frame!($vm).closure.function
    };
}

macro_rules! read_byte {
    ($vm:expr) => {{
        let byte = get_current_function!($vm).chunk.code()[get_current_frame!($vm).ip];
        get_current_frame_mut!($vm).ip += 1;
        byte
    }};
}

macro_rules! read_constant {
    ($vm:expr) => {{
        let index = read_byte!($vm) as usize;
        let constants = get_current_function!($vm).chunk.constants();
        constants[index]
    }};
}

macro_rules! read_short {
    ($vm:expr) => {{
        let high_byte = read_byte!($vm) as usize;
        let low_byte = read_byte!($vm) as usize;
        (high_byte << 8) | low_byte
    }};
}

macro_rules! peek {
    ($vm:expr, $distance:expr) => {
        if $vm.stack_top > $distance {
            $vm.stack
                .get($vm.stack_top - 1 - $distance)
                .copied()
                .unwrap_or(Value::Nil)
        } else {
            Value::Nil
        }
    };
}

#[derive(Debug, Clone, Default)]
struct CallFrame {
    closure: Rc<ClosureObject>,
    ip: usize,
    value_slot: usize,
}

#[derive(Clone)]
pub struct Vm {
    is_debug: bool,
    stack_top: usize,
    frame_count: usize,
    stack: Vec<Value>,
    frames: [CallFrame; FRAME_MAX],
    globals: HashMap<StringHandle, Value>,
    heap: ObjectHeap,
    upvalues: Vec<(usize, Rc<RefCell<Upvalue>>)>,
}

impl Vm {
    pub fn new(mut heap: ObjectHeap) -> Self {
        let mut globals = HashMap::new();

        let nil_type_handle = heap.intern_string("NIL".into());
        let nil_type_value_handle = heap.intern_string(NIL_TYPE_STRING.into());
        globals.insert(nil_type_handle, Value::String(nil_type_value_handle));

        let boolean_type_handle = heap.intern_string("BOOLEAN".into());
        let boolean_type_value_handle = heap.intern_string(BOOLEAN_TYPE_STRING.into());
        globals.insert(
            boolean_type_handle,
            Value::String(boolean_type_value_handle),
        );

        let number_type_handle = heap.intern_string("NUMBER".into());
        let number_type_value_handle = heap.intern_string(NUMBER_TYPE_STRING.into());
        globals.insert(number_type_handle, Value::String(number_type_value_handle));

        let string_type_handle = heap.intern_string("STRING".into());
        let string_type_value_handle = heap.intern_string(STRING_TYPE_STRING.into());
        globals.insert(string_type_handle, Value::String(string_type_value_handle));

        let function_type_handle = heap.intern_string("FUNCTION".into());
        let function_type_value_handle = heap.intern_string(FUNCTION_TYPE_STRING.into());
        globals.insert(
            function_type_handle,
            Value::String(function_type_value_handle),
        );

        let vm = Self {
            is_debug: false,
            frame_count: 0,
            stack_top: 1,
            stack: vec![Value::Nil; STACK_MAX],
            frames: std::array::from_fn(|_| CallFrame::default()),
            globals,
            heap,
            upvalues: Vec::with_capacity(8),
        };

        vm.add_native_function("assert", 2, qang_assert)
            .add_native_function("assert_eq", 3, qang_assert_eq)
            .add_native_function("assert_throws", 2, qang_assert_throws)
            .add_native_function("print", 1, qang_print)
            .add_native_function("println", 1, qang_println)
            .add_native_function("system_time", 0, qang_system_time)
            .add_native_function("typeof", 1, qang_typeof)
            .add_native_function("to_string", 1, qang_to_string)
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    pub fn add_native_function(mut self, name: &str, arity: usize, function: NativeFn) -> Self {
        let identifier_handle = self.heap.intern_string(name);
        let native_function = NativeFunction {
            name: identifier_handle,
            arity,
            function,
        };

        self.globals.insert(
            identifier_handle,
            Value::Function(FunctionValueKind::NativeFunction(native_function)),
        );

        self
    }


    fn get_current_function(&self) -> &FunctionObject {
        &get_current_frame!(self).closure.function
    }

    fn get_current_loc(&self) -> SourceLocation {
        self.get_loc_at(get_current_frame!(self).ip)
    }

    fn get_previous_loc(&self) -> SourceLocation {
        if get_current_frame!(self).ip > 0 {
            self.get_loc_at(get_current_frame!(self).ip - 1)
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

    pub fn interpret(&mut self, program: QangProgram) -> RuntimeResult<()> {
        self.call(Rc::new(ClosureObject::new(program.into_function())), 0)?;

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

            let opcode: OpCode = read_byte!(self).into();

            match opcode {
                OpCode::Constant => {
                    let constant = read_constant!(self);
                    push_value!(self, constant)?;
                }
                OpCode::Negate => {
                    if let Value::Number(number) = peek!(self, 0) {
                        if let Some(stack_value) = self.stack.get_mut(self.stack_top - 1) {
                            *stack_value = Value::Number(-number);
                        }
                    } else {
                        return Err(QangRuntimeError::new(
                            "Operand must be a number.".to_string(),
                            self.get_previous_loc(),
                        ));
                    }
                }
                OpCode::Not => {
                    let value = peek!(self, 0);
                    if let Some(stack_value) = self.stack.get_mut(self.stack_top - 1) {
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

                            let result = heap.intern_string(&str1_str2);
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
                        read_constant!(self)
                            .try_into()
                            .map_err(|e: ValueConversionError| {
                                e.into_qang_error(self.get_previous_loc())
                            })?;
                    let value = pop_value!(self);
                    self.globals.insert(identifier_handle, value);
                }
                OpCode::GetGlobal => {
                    let identifier_handle: StringHandle =
                        read_constant!(self)
                            .try_into()
                            .map_err(|e: ValueConversionError| {
                                let loc = self.get_previous_loc();
                                e.into_qang_error(loc)
                            })?;
                    let value = *self.globals.get(&identifier_handle).ok_or_else(|| {
                        let loc = self.get_previous_loc();
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
                        read_constant!(self)
                            .try_into()
                            .map_err(|e: ValueConversionError| {
                                e.into_qang_error(self.get_previous_loc())
                            })?;

                    if !self.globals.contains_key(&identifier_handle) {
                        let identifier_name = self.heap.get_string(identifier_handle);
                        let loc = self.get_previous_loc();
                        return Err(QangRuntimeError::new(
                            format!("Undefined variable: {}.", identifier_name).to_string(),
                            loc,
                        ));
                    }
                    let value = peek!(self, 0);
                    self.globals.insert(identifier_handle, value);
                }
                OpCode::GetLocal => {
                    let slot = read_byte!(self);
                    let absolute_slot = get_current_frame!(self).value_slot + 1 + slot as usize;
                    debug_assert!(
                        absolute_slot < STACK_MAX,
                        "Local slot {} out of bounds",
                        absolute_slot
                    );

                    let value = self.stack[absolute_slot];
                    push_value!(self, value)?;
                }
                OpCode::SetLocal => {
                    let slot = read_byte!(self);
                    let value = peek!(self, 0);
                    let absolute_slot = get_current_frame!(self).value_slot + 1 + slot as usize;

                    self.stack[absolute_slot] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = read_short!(self);
                    if !peek!(self, 0).is_truthy() {
                        get_current_frame_mut!(self).ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = read_short!(self);
                    get_current_frame_mut!(self).ip += offset;
                }
                OpCode::Loop => {
                    let offset = read_short!(self);
                    get_current_frame_mut!(self).ip -= offset;
                }
                OpCode::Call => {
                    let arg_count = read_byte!(self) as usize;
                    let function_value = peek!(self, arg_count);
                    self.call_value(function_value, arg_count)?;
                }
                OpCode::Closure => {
                    let handle: FunctionHandle =
                        read_constant!(self)
                            .try_into()
                            .map_err(|e: ValueConversionError| {
                                QangRuntimeError::new(
                                    e.message().to_string(),
                                    self.get_previous_loc(),
                                )
                            })?;
                    let function = self.heap.clone_function(handle);
                    let mut closure = ClosureObject::new(function);

                    for i in 0..closure.upvalue_count {
                        let is_local = read_byte!(self) != 0;
                        let index = read_byte!(self) as usize;

                        if is_local {
                            let stack_slot = get_current_frame!(self).value_slot + 1 + index;
                            self.capture_upvalue(stack_slot, &mut closure.upvalues[i]);
                        } else {
                            let current_upvalue = get_current_frame!(self).closure.upvalues[index]
                                .borrow()
                                .clone();
                            closure.upvalues[i] = Rc::new(RefCell::new(current_upvalue));
                        }
                    }

                    let closure_handle = self.heap.allocate_closure(closure);
                    push_value!(
                        self,
                        Value::Function(FunctionValueKind::Closure(closure_handle))
                    )?;
                }
                OpCode::GetUpvalue => {
                    let slot = read_byte!(self) as usize;
                    let upvalue = *get_current_frame!(self).closure.upvalues[slot].borrow();

                    match upvalue {
                        Upvalue::Open(stack_slot) => {
                            let value = self.stack[stack_slot];
                            push_value!(self, value)?;
                        }
                        Upvalue::Closed(handle) => {
                            push_value!(self, handle)?;
                        }
                    }
                }
                OpCode::SetUpvalue => {
                    let slot = read_byte!(self) as usize;
                    let value = peek!(self, 0);

                    let stack_slot = {
                        let upvalue_rc = &get_current_frame!(self).closure.upvalues[slot];
                        if let Upvalue::Open(stack_slot) = *upvalue_rc.borrow() {
                            Some(stack_slot)
                        } else {
                            *upvalue_rc.borrow_mut() = Upvalue::Closed(value);
                            None
                        }
                    };

                    if let Some(stack_slot) = stack_slot {
                        self.stack[stack_slot] = value;
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalue(self.stack_top - 1);
                    pop_value!(self);
                }
                OpCode::Return => {
                    let result = pop_value!(self);
                    let value_slot = get_current_frame!(self).value_slot;
                    self.frame_count -= 1;

                    #[cfg(feature = "profiler")]
                    coz::progress!("function_returns");

                    if self.frame_count == 0 {
                        return Ok(result);
                    }

                    self.stack_top = value_slot + 1;
                    self.stack[value_slot] = result;
                }
            };
        }
    }

    fn close_upvalue(&mut self, last_slot: usize) {
        let mut i = self.upvalues.len();
        while i > 0 {
            i -= 1;
            let (slot, upvalue) = &self.upvalues[i];

            if *slot >= last_slot {
                let value = self.stack[*slot];
                *upvalue.borrow_mut() = Upvalue::Closed(value);
                self.upvalues.remove(i);
            }
        }
    }

    fn capture_upvalue(&mut self, stack_slot: usize, dest: &mut Rc<RefCell<Upvalue>>) {
        for (upvalue_slot, upvalue) in &self.upvalues {
            if *upvalue_slot == stack_slot {
                upvalue.clone_into(dest);
                return;
            }
        }

        *dest = Rc::new(RefCell::new(Upvalue::Open(stack_slot)));
        self.upvalues.push((stack_slot, dest.clone()));
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
            .map_err(|e: BinaryOperationError| e.into_qang_error(self.get_previous_loc()))?;

        push_value!(self, value)?;
        Ok(())
    }

    fn call_value(&mut self, value: Value, arg_count: usize) -> RuntimeResult<()> {
        match value {
            Value::Function(FunctionValueKind::Closure(handle)) => {
                let closure = self.heap.clone_closure(handle);
                self.call(closure, arg_count)
            }
            Value::Function(FunctionValueKind::NativeFunction(function)) => {
                self.call_native_function(function, arg_count)
            }
            _ => {
                let value_str = value.to_display_string(&self.heap);
                Err(QangRuntimeError::new(
                    format!("Identifier '{}' not callable.", value_str).to_string(),
                    self.get_previous_loc(),
                ))
            }
        }
    }

    fn call(&mut self, closure: Rc<ClosureObject>, arg_count: usize) -> RuntimeResult<()> {
        #[cfg(feature = "profiler")]
        coz::scope!("call_function");

        #[cfg(feature = "profiler")]
        coz::progress!("before_call");

        let loc = if self.frame_count > 0 {
            self.get_previous_loc()
        } else {
            SourceLocation::default()
        };

        let final_arg_count = if arg_count < closure.function.arity {
            for _ in arg_count..closure.function.arity {
                push_value!(self, Value::Nil)?;
            }
            closure.function.arity
        } else {
            arg_count
        };

        if self.frame_count >= FRAME_MAX {
            return Err(QangRuntimeError::new("Stack overflow.".to_string(), loc));
        }

        self.frame_count += 1;

        let value_slot = self.stack_top - final_arg_count - 1; // This should overwrite the function identifier with its return value.
        let call_frame = get_current_frame_mut!(self);

        call_frame.value_slot = value_slot;
        call_frame.closure.clone_from(&closure);
        call_frame.ip = 0;

        #[cfg(feature = "profiler")]
        coz::progress!("after_call");

        Ok(())
    }

    pub fn call_function(
        &mut self,
        handle: ClosureHandle,
        args: Vec<Value>,
    ) -> RuntimeResult<Value> {
        let saved_stack_top = self.stack_top;
        let saved_frame_count = self.frame_count;

        push_value!(self, Value::Function(FunctionValueKind::Closure(handle)))?;

        for value in &args {
            push_value!(self, *value)?;
        }

        let closure = self.heap.clone_closure(handle);

        self.call(closure, args.len())?;

        match self.run() {
            Ok(return_value) => {
                // Reset the VM state to before the function call
                self.stack_top = saved_stack_top;
                self.frame_count = saved_frame_count;
                Ok(return_value)
            }
            Err(error) => {
                // Reset the VM state to before the function call
                self.stack_top = saved_stack_top;
                self.frame_count = saved_frame_count;
                Err(error)
            }
        }
    }

    fn call_native_function(
        &mut self,
        function: NativeFunction,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        let loc = self.get_previous_loc();
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

    pub fn heap(&self) -> &ObjectHeap {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut ObjectHeap {
        &mut self.heap
    }

    pub fn globals(&self) -> &HashMap<StringHandle, Value> {
        &self.globals
    }

    fn get_stack_trace(&self) -> Vec<Trace> {
        self.get_stack_trace_from_frames(0..self.frame_count)
    }

    fn get_stack_trace_from_frames(&self, frame_id_range: Range<usize>) -> Vec<Trace> {
        let mut traces = Vec::new();

        for frame_idx in frame_id_range {
            let frame = &self.frames[frame_idx];
            let name = self.heap.get_string(frame.closure.function.name);

            let loc = if frame.ip > 0 {
                frame
                    .closure
                    .function
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
        for i in 0..self.stack_top {
            if let Some(value) = self.stack.get(i) {
                value.print(&self.heap);
                print!(" ");
            }
        }
        println!();

        disassemble_instruction(
            &self.get_current_function().chunk,
            &self.heap,
            get_current_frame!(self).ip,
        );
    }
}
