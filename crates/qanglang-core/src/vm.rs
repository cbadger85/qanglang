use std::{collections::HashMap, ops::Range, rc::Rc};

#[cfg(feature = "profiler")]
use coz;

use crate::{
    HeapObject, ObjectHeap, QangProgram, QangRuntimeError, Value,
    chunk::{Chunk, OpCode, SourceLocation},
    compiler::{FRAME_MAX, STACK_MAX},
    debug::disassemble_instruction,
    error::{Trace, ValueConversionError},
    heap::{FunctionObject, ObjectHandle},
    qang_std::{qang_assert, qang_assert_eq, qang_print, qang_println, qang_typeof, system_time},
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

pub type RuntimeResult<T> = Result<T, QangRuntimeError>;

pub type NativeFn = fn(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError>;

macro_rules! push_value {
    ($vm:expr, $value:expr) => {
        if $vm.stack_top >= STACK_MAX {
            panic!(
                "Stack overflow: maximum stack size of {} exceeded",
                STACK_MAX
            );
        }
        $vm.stack[$vm.stack_top] = $value;
        $vm.stack_top += 1;
    };
}

macro_rules! pop_value {
    ($vm:expr) => {
        if $vm.stack_top > 0 {
            $vm.stack_top -= 1;
            Ok($vm.stack[$vm.stack_top])
        } else {
            Err(QangRuntimeError::new(
                "No value found, unexpected empty stack.".to_string(),
                $vm.get_current_loc(),
            ))
        }
    };
}

#[derive(Debug, Clone, Default)]
struct CallFrame {
    current_function: Rc<FunctionObject>,
    ip: usize,
    value_slot: usize,
}

pub struct Vm {
    is_debug: bool,
    stack_top: usize,
    frame_count: usize,
    stack: Vec<Value>,
    frames: [CallFrame; FRAME_MAX],
    globals: HashMap<usize, Value>,
    heap: ObjectHeap,
    program_handle: ObjectHandle,
}

impl Vm {
    pub fn new(mut heap: ObjectHeap) -> Self {
        let mut globals = HashMap::new();

        let nil_type_handle = heap.intern_string("NIL".into());
        let nil_type_value_handle = heap.intern_string(NIL_TYPE_STRING.into());
        globals.insert(
            nil_type_handle.identifier(),
            Value::String(nil_type_value_handle),
        );

        let boolean_type_handle = heap.intern_string("BOOLEAN".into());
        let boolean_type_value_handle = heap.intern_string(BOOLEAN_TYPE_STRING.into());
        globals.insert(
            boolean_type_handle.identifier(),
            Value::String(boolean_type_value_handle),
        );

        let number_type_handle = heap.intern_string("NUMBER".into());
        let number_type_value_handle = heap.intern_string(NUMBER_TYPE_STRING.into());
        globals.insert(
            number_type_handle.identifier(),
            Value::String(number_type_value_handle),
        );

        let string_type_handle = heap.intern_string("STRING".into());
        let string_type_value_handle = heap.intern_string(STRING_TYPE_STRING.into());
        globals.insert(
            string_type_handle.identifier(),
            Value::String(string_type_value_handle),
        );

        let function_type_handle = heap.intern_string("FUNCTION".into());
        let function_type_value_handle = heap.intern_string(FUNCTION_TYPE_STRING.into());
        globals.insert(
            function_type_handle.identifier(),
            Value::String(function_type_value_handle),
        );

        let vm = Self {
            is_debug: false,
            frame_count: 0,
            stack_top: 0,
            stack: vec![Value::Nil; STACK_MAX],
            frames: std::array::from_fn(|_| CallFrame::default()),
            globals,
            heap,
            program_handle: ObjectHandle::default(),
        };

        vm.add_native_function("assert", 2, qang_assert)
            .add_native_function("assert_eq", 3, qang_assert_eq)
            .add_native_function("print", 1, qang_print)
            .add_native_function("println", 1, qang_println)
            .add_native_function("system_time", 0, system_time)
            .add_native_function("typeof", 1, qang_typeof)
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    pub fn add_native_function(mut self, name: &str, arity: usize, function: NativeFn) -> Self {
        let identifier_handle = self.heap.intern_string(name.to_string().into_boxed_str());
        let native_function = NativeFunction {
            name: identifier_handle,
            arity,
            function,
        };

        self.globals.insert(
            identifier_handle.identifier(),
            Value::Function(FunctionValueKind::NativeFunction(native_function)),
        );

        self
    }

    fn get_current_frame(&self) -> &CallFrame {
        debug_assert!(self.frame_count > 0, "No active frame");
        debug_assert!(
            self.frame_count <= FRAME_MAX,
            "Frame count {} exceeds max",
            self.frame_count
        );

        #[cfg(feature = "profiler")]
        coz::scope!("get_current_frame");

        &self.frames[self.frame_count - 1]
    }

    fn get_current_frame_mut(&mut self) -> &mut CallFrame {
        #[cfg(feature = "profiler")]
        coz::scope!("get_current_frame_mut");
        &mut self.frames[self.frame_count - 1]
    }

    fn get_current_function(&self) -> &FunctionObject {
        &self.get_current_frame().current_function
    }

    fn get_current_loc(&self) -> SourceLocation {
        self.get_loc_at(self.get_current_frame().ip)
    }

    fn get_current_chunk(&self) -> &Chunk {
        &self.get_current_function().chunk
    }

    fn get_previous_loc(&self) -> SourceLocation {
        if self.get_current_frame().ip > 0 {
            self.get_loc_at(self.get_current_frame().ip - 1)
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
        let function_handle = program.into();
        self.program_handle = function_handle;

        self.call_function(function_handle, 0)?;

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

            let opcode: OpCode = self.read_byte()?.into();

            match opcode {
                OpCode::Constant => {
                    let constant_index = self.read_byte()? as usize;
                    let constant = self.read_constant(constant_index);
                    push_value!(self, constant);
                }
                OpCode::Negate => {
                    if let Value::Number(number) = self.peek(0) {
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
                    let value = self.peek(0);
                    if let Some(stack_value) = self.stack.get_mut(self.stack_top - 1) {
                        *stack_value = Value::Boolean(!value.is_truthy());
                    }
                }
                OpCode::True => {
                    push_value!(self, Value::Boolean(true));
                }
                OpCode::False => {
                    push_value!(self, Value::Boolean(false));
                }
                OpCode::Nil => {
                    push_value!(self, Value::Nil);
                }
                OpCode::Add => {
                    self.binary_operation(|a, b, heap, loc| match (&a, &b) {
                        (Value::Number(num1), Value::Number(num2)) => {
                            Ok(Value::Number(num1 + num2))
                        }
                        (Value::String(_), Value::String(_)) => {
                            #[cfg(feature = "profiler")]
                            coz::scope!("string_concatenation");

                            let str1: Box<str> = a
                                .into_string(heap)
                                .map_err(|e: ValueConversionError| e.into_qang_error(loc))?;

                            let str2: Box<str> = b
                                .into_string(heap)
                                .map_err(|e: ValueConversionError| e.into_qang_error(loc))?;

                            let mut str1_str2 = String::with_capacity(str1.len() + str2.len());
                            str1_str2.push_str(&str1);
                            str1_str2.push_str(&str2);

                            let result = heap.intern_string(str1_str2.into_boxed_str());
                            Ok(Value::String(result))
                        }
                        (Value::Number(_), _) => Err(QangRuntimeError::new(
                            format!("Cannot add number to {}.", b.to_type_string()),
                            loc,
                        )),
                        (Value::String(_), _) => Err(QangRuntimeError::new(
                            format!("Cannot add string to {}.", b.to_type_string()),
                            loc,
                        )),
                        (_, Value::Number(_)) => Err(QangRuntimeError::new(
                            format!("Cannot add {} to number.", a.to_type_string()),
                            loc,
                        )),
                        (_, Value::String(_)) => Err(QangRuntimeError::new(
                            format!("Cannot add {} to string.", a.to_type_string()),
                            loc,
                        )),
                        _ => Err(QangRuntimeError::new(
                            "Both operands must be a numbers or strings.".to_string(),
                            loc,
                        )),
                    })?;
                }
                OpCode::Subtract => self.binary_operation(|a, b, _heap, loc| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;

                    Ok((a - b).into())
                })?,
                OpCode::Multiply => self.binary_operation(|a, b, _heap, loc| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;

                    Ok((a * b).into())
                })?,
                OpCode::Divide => self.binary_operation(|a, b, _heap, loc| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;

                    Ok((a / b).into())
                })?,
                OpCode::Modulo => self.binary_operation(|a, b, _heap, loc| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;

                    Ok((a % b).into())
                })?,
                OpCode::Equal => {
                    self.binary_operation(|a, b, _heap, _loc| Ok(Value::Boolean(a == b)))?
                }
                OpCode::Greater => self.binary_operation(|a, b, _heap, loc| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    Ok(Value::Boolean(a > b))
                })?,
                OpCode::GreaterEqual => self.binary_operation(|a, b, _heap, loc| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    Ok(Value::Boolean(a >= b))
                })?,
                OpCode::Less => self.binary_operation(|a, b, _heap, loc| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    Ok(Value::Boolean(a < b))
                })?,
                OpCode::LessEqual => self.binary_operation(|a, b, _heap, loc| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangRuntimeError::new("Both operands must be a number.".to_string(), loc)
                    })?;
                    Ok(Value::Boolean(a <= b))
                })?,
                OpCode::Pop => {
                    pop_value!(self)?;
                }
                OpCode::DefineGlobal => {
                    let loc = self.get_previous_loc();
                    let identifier_handle: ObjectHandle =
                        pop_value!(self)?
                            .try_into()
                            .map_err(|e: ValueConversionError| {
                                e.into_qang_error_with_trace(loc, self.get_stack_trace())
                            })?;
                    let value = pop_value!(self)?;
                    self.globals.insert(identifier_handle.identifier(), value);
                }
                OpCode::GetGlobal => {
                    let loc = self.get_previous_loc();
                    let identifier_handle: ObjectHandle =
                        pop_value!(self)?
                            .try_into()
                            .map_err(|e: ValueConversionError| {
                                e.into_qang_error_with_trace(loc, self.get_stack_trace())
                            })?;
                    let value = *self
                        .globals
                        .get(&identifier_handle.identifier())
                        .ok_or_else(|| {
                            let identifier_name = self
                                .heap
                                .get(identifier_handle)
                                .map(|obj| match &obj {
                                    HeapObject::String(string) => string.clone(),
                                    _ => "unknown".to_string().into_boxed_str(),
                                })
                                .unwrap_or("unknown".to_string().into_boxed_str());
                            QangRuntimeError::new(
                                format!("Undefined variable: {}.", identifier_name),
                                loc,
                            )
                        })?;
                    push_value!(self, value);
                }
                OpCode::SetGlobal => {
                    let loc = self.get_previous_loc();
                    let identifier_handle: ObjectHandle =
                        pop_value!(self)?
                            .try_into()
                            .map_err(|e: ValueConversionError| {
                                e.into_qang_error_with_trace(loc, self.get_stack_trace())
                            })?;

                    if !self.globals.contains_key(&identifier_handle.identifier()) {
                        let identifier_name = self
                            .get_identifier_name(identifier_handle)
                            .unwrap_or("<unknown>".to_string().into_boxed_str());

                        return Err(QangRuntimeError::new(
                            format!("Undefined variable: {}.", identifier_name).to_string(),
                            loc,
                        ));
                    }
                    let value = self.peek(0);
                    self.globals.insert(identifier_handle.identifier(), value);
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte()?;
                    let absolute_slot = self.get_current_frame().value_slot + slot as usize;
                    debug_assert!(
                        absolute_slot < STACK_MAX,
                        "Local slot {} out of bounds",
                        absolute_slot
                    );

                    let value = self.stack[absolute_slot];
                    push_value!(self, value);
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte()?;
                    let value = self.peek(0);
                    let absolute_slot = self.get_current_frame().value_slot + slot as usize;

                    self.stack[absolute_slot] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short()?;
                    if !self.peek(0).is_truthy() {
                        self.get_current_frame_mut().ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short()?;
                    self.get_current_frame_mut().ip += offset;
                }
                OpCode::Loop => {
                    let offset = self.read_short()?;
                    self.get_current_frame_mut().ip -= offset;
                }
                OpCode::Call => {
                    let arg_count = self.read_byte()? as usize;
                    let function_value = self.peek(arg_count);
                    self.call_value(function_value, arg_count)?;
                }
                OpCode::Return => {
                    let result = pop_value!(self)?;

                    #[cfg(feature = "profiler")]
                    coz::progress!("function_returns");

                    let value_slot = self.get_current_frame().value_slot;
                    self.frame_count -= 1;

                    if self.frame_count == 0 {
                        debug_assert!(
                            self.stack_top <= 1,
                            "Stack corruption: {} values left",
                            self.stack_top
                        );

                        self.stack_top = 0;
                        return Ok(result);
                    }

                    // Reset stack to before the function call, then push the return value.
                    // This replaces the function and its arguments with the return value.
                    self.stack_top = if value_slot > 0 { value_slot - 1 } else { 0 };
                    push_value!(self, result);
                }
            };
        }
    }

    fn read_byte(&mut self) -> RuntimeResult<u8> {
        let byte = self.get_current_chunk().code()[self.get_current_frame().ip];
        self.get_current_frame_mut().ip += 1;
        Ok(byte)
    }

    fn read_constant(&self, index: usize) -> Value {
        let constants = self.get_current_function().chunk.constants();
        debug_assert!(
            index < constants.len(),
            "Constant index {} out of bounds",
            index
        );
        constants[index]
    }

    fn read_short(&mut self) -> RuntimeResult<usize> {
        let high_byte = self.read_byte()? as usize;
        let low_byte = self.read_byte()? as usize;
        Ok((high_byte << 8) | low_byte)
    }

    fn peek(&self, distance: usize) -> Value {
        if self.stack_top > distance {
            self.stack
                .get(self.stack_top - 1 - distance)
                .copied()
                .unwrap_or(Value::Nil)
        } else {
            Value::Nil
        }
    }

    fn binary_operation<F>(&mut self, op: F) -> RuntimeResult<()>
    where
        F: FnOnce(Value, Value, &mut ObjectHeap, SourceLocation) -> RuntimeResult<Value>,
    {
        #[cfg(feature = "profiler")]
        coz::scope!("binary_operation");

        let op_loc = self.get_previous_loc();
        let b = pop_value!(self)?;
        let a = pop_value!(self)?;

        let value = op(a, b, &mut self.heap, op_loc)?;

        push_value!(self, value);
        Ok(())
    }

    fn call_value(&mut self, value: Value, arg_count: usize) -> RuntimeResult<()> {
        match value {
            Value::Function(FunctionValueKind::QangFunction(handle)) => {
                self.call_function(handle, arg_count)
            }
            Value::Function(FunctionValueKind::NativeFunction(function)) => {
                self.call_native_function(function, arg_count)
            }
            _ => {
                let identifier = value
                    .into_string(&self.heap)
                    .ok()
                    .unwrap_or("<unknown>".into());
                Err(QangRuntimeError::new(
                    format!("Identifier '{}' not callable.", identifier).to_string(),
                    self.get_previous_loc(),
                ))
            }
        }
    }

    fn call_function(&mut self, handle: ObjectHandle, arg_count: usize) -> RuntimeResult<()> {
        #[cfg(feature = "profiler")]
        coz::scope!("call_function");

        #[cfg(feature = "profiler")]
        coz::progress!("before_call");

        // For the initial call from interpret(), there's no previous location
        let loc = if self.frame_count > 0 {
            self.get_previous_loc()
        } else {
            SourceLocation::default()
        };

        let obj = self
            .heap
            .get(handle)
            .ok_or_else(|| QangRuntimeError::new("Orphaned identifier".to_string(), loc))?;

        match obj {
            HeapObject::Function(function) => {
                if arg_count < function.arity {
                    for _ in arg_count..function.arity {
                        push_value!(self, Value::Nil);
                    }
                }

                if self.frame_count >= FRAME_MAX {
                    return Err(QangRuntimeError::new("Stack overflow.".to_string(), loc));
                }

                self.frame_count += 1;

                let value_slot = if self.frame_count == 1 {
                    0
                } else {
                    self.stack_top - arg_count
                };

                let call_frame = &mut self.frames[self.frame_count - 1];
                call_frame.current_function = function.clone();
                call_frame.ip = if handle == self.program_handle { 0 } else { 1 };
                call_frame.value_slot = value_slot;

                Ok(())
            }
            _ => Err(QangRuntimeError::new(
                "Value not callable.".to_string(),
                loc,
            )),
        }?;

        #[cfg(feature = "profiler")]
        coz::progress!("after_call");

        Ok(())
    }

    pub fn call_function_with_args(
        &mut self,
        handle: ObjectHandle,
        args: Vec<Value>,
    ) -> RuntimeResult<Value> {
        push_value!(
            self,
            Value::Function(FunctionValueKind::QangFunction(handle))
        );

        for value in &args {
            push_value!(self, *value);
        }

        self.call_function(handle, args.len())?;

        let return_value = self.run()?;

        Ok(return_value)
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
                args[i] = pop_value!(self)?;
            } else {
                pop_value!(self)?; // discard values that are passed in but not needed by the function.
            }
        }
        pop_value!(self)?; // pop function off the stack now that it has been called.

        let value = (function.function)(args.as_slice(), self)
            .map_err(|e: NativeFunctionError| e.into_qang_error(loc))?
            .unwrap_or_default();

        push_value!(self, value);

        Ok(())
    }

    fn get_identifier_name(&self, handle: ObjectHandle) -> Option<Box<str>> {
        self.heap
            .get(handle)
            .and_then(|obj: &HeapObject| match &obj {
                HeapObject::String(identifier) => Some(identifier.clone()),
                _ => None,
            })
    }

    pub fn heap(&self) -> &ObjectHeap {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut ObjectHeap {
        &mut self.heap
    }

    pub fn globals(&self) -> &HashMap<usize, Value> {
        &self.globals
    }

    fn get_stack_trace(&self) -> Vec<Trace> {
        self.get_stack_trace_from_frames(0..self.frame_count)
    }

    fn get_stack_trace_from_frames(&self, frame_id_range: Range<usize>) -> Vec<Trace> {
        let mut traces = Vec::new();

        for frame_idx in frame_id_range {
            let frame = &self.frames[frame_idx];
            let name = self
                .get_identifier_name(frame.current_function.name)
                .unwrap_or("<unknown>".to_string().into_boxed_str());

            let loc = if frame.ip > 0 {
                frame
                    .current_function
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
            self.get_current_chunk(),
            &self.heap,
            self.get_current_frame().ip,
        );
    }
}
