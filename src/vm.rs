use std::{collections::HashMap, rc::Rc};

use crate::{
    HeapObject, KangProgram, ObjectHeap, QangRuntimeError, Value,
    chunk::{Chunk, OpCode, SourceLocation},
    compiler::{FRAME_MAX, STACK_MAX},
    debug::disassemble_instruction,
    error::{Trace, ValueConversionError},
    heap::{FunctionObject, KangFunction, NativeFunction, ObjectHandle},
    kang_std::{kang_assert, kang_assert_eq, kang_print, kang_println, system_time},
    value::get_value_type,
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

#[derive(Debug, Clone, Default)]
struct CallFrame {
    current_function: Rc<KangFunction>,
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
    pub fn new(heap: ObjectHeap) -> Self {
        let vm = Self {
            is_debug: false,
            frame_count: 0,
            stack_top: 0,
            stack: vec![Value::Nil; STACK_MAX],
            frames: std::array::from_fn(|_| CallFrame::default()),
            globals: HashMap::new(),
            heap,
            program_handle: ObjectHandle::default(),
        };

        vm.add_native_function("assert", 1, kang_assert)
            .add_native_function("assert_eq", 1, kang_assert_eq)
            .add_native_function("print", 1, kang_print)
            .add_native_function("println", 1, kang_println)
            .add_native_function("system_time", 0, system_time)
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

        let handle = self.heap.allocate_object(native_function.into());

        self.globals
            .insert(identifier_handle.identifier(), Value::Function(handle));

        self
    }

    fn get_current_frame(&self) -> &CallFrame {
        &self.frames[self.frame_count - 1]
    }

    fn get_current_frame_mut(&mut self) -> &mut CallFrame {
        &mut self.frames[self.frame_count - 1]
    }

    fn get_current_function(&self) -> &KangFunction {
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

    pub fn interpret(&mut self, program: KangProgram) -> RuntimeResult<()> {
        let function_handle = program.into();
        self.program_handle = function_handle;
        // Use call_function to initialize the first call frame consistently
        self.call_function(function_handle, 0)?;

        self.run()
            .map_err(|e| e.with_stack_trace(self.get_stack_trace()))
    }

    fn run(&mut self) -> RuntimeResult<()> {
        loop {
            if self.is_debug {
                self.debug();
            }
            let opcode: OpCode = self.read_byte()?.into();

            match opcode {
                OpCode::Constant => {
                    let constant_index = self.read_byte()? as usize;
                    let constant = self.read_constant(constant_index);
                    self.push(constant);
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
                    let is_truthy = self.is_truthy(value);
                    if let Some(stack_value) = self.stack.get_mut(self.stack_top - 1) {
                        *stack_value = Value::Boolean(!is_truthy);
                    }
                }
                OpCode::True => {
                    self.push(Value::Boolean(true));
                }
                OpCode::False => {
                    self.push(Value::Boolean(false));
                }
                OpCode::Nil => {
                    self.push(Value::Nil);
                }
                OpCode::Add => {
                    self.binary_operation(|a, b, heap, loc| match (&a, &b) {
                        (Value::Number(num1), Value::Number(num2)) => {
                            Ok(Value::Number(num1 + num2))
                        }
                        (Value::String(_), Value::String(_)) => {
                            let str1: Box<str> = a
                                .into_string(heap)
                                .map_err(|e: ValueConversionError| e.into_qang_error(loc))?;

                            let str2: Box<str> = b
                                .into_string(heap)
                                .map_err(|e: ValueConversionError| e.into_qang_error(loc))?;
                            let result =
                                heap.intern_string(format!("{}{}", str1, str2).into_boxed_str());
                            Ok(Value::String(result))
                        }
                        (Value::Number(_), _) => Err(QangRuntimeError::new(
                            format!("Cannot add number to {}.", get_value_type(&b)),
                            loc,
                        )),
                        (Value::String(_), _) => Err(QangRuntimeError::new(
                            format!("Cannot add string to {}.", get_value_type(&b)),
                            loc,
                        )),
                        (_, Value::Number(_)) => Err(QangRuntimeError::new(
                            format!("Cannot add {} to number.", get_value_type(&a)),
                            loc,
                        )),
                        (_, Value::String(_)) => Err(QangRuntimeError::new(
                            format!("Cannot add {} to string.", get_value_type(&a)),
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
                OpCode::DefineGlobal => {
                    let loc = self.get_previous_loc();
                    let identifier_handle: ObjectHandle =
                        self.pop()?.try_into().map_err(|e: ValueConversionError| {
                            e.into_qang_error_with_trace(loc, self.get_stack_trace())
                        })?;
                    let value = self.pop()?;
                    self.globals.insert(identifier_handle.identifier(), value);
                }
                OpCode::GetGlobal => {
                    let loc = self.get_previous_loc();
                    let identifier_handle: ObjectHandle =
                        self.pop()?.try_into().map_err(|e: ValueConversionError| {
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
                    self.push(value);
                }
                OpCode::SetGlobal => {
                    let loc = self.get_previous_loc();
                    let identifier_handle: ObjectHandle =
                        self.pop()?.try_into().map_err(|e: ValueConversionError| {
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
                    // Local variables are relative to the current frame's value_slot
                    let absolute_slot = self.get_current_frame().value_slot + slot as usize;
                    let value = self.stack.get(absolute_slot).copied().unwrap_or(Value::Nil);
                    self.push(value);
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte()?;
                    let value = self.peek(0);
                    // Local variables are relative to the current frame's value_slot
                    let absolute_slot = self.get_current_frame().value_slot + slot as usize;
                    // Ensure the stack is large enough for this slot
                    while self.stack.len() <= absolute_slot {
                        self.stack.push(Value::default());
                    }
                    self.stack[absolute_slot] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short()?;
                    if !self.is_truthy(self.peek(0)) {
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
                OpCode::Print => {
                    let value = self.pop()?;
                    value.print(&self.heap);
                    println!();
                }
                OpCode::Return => {
                    let result = self.pop()?;

                    // Get the value_slot from the current frame before decrementing frame_count
                    let value_slot = self.get_current_frame().value_slot;
                    self.frame_count -= 1;

                    if self.frame_count == 0 {
                        // For the main function, just pop the final result and exit
                        // Don't call pop() again as there's no active frame for error handling
                        if self.stack_top > 0 {
                            self.stack_top -= 1;
                        }
                        return Ok(());
                    }

                    self.stack_top = value_slot;
                    self.push(result);
                }
                _ => (),
            };
        }
    }

    fn read_byte(&mut self) -> RuntimeResult<u8> {
        if self.get_current_frame().ip >= self.get_current_chunk().count() {
            return Err(QangRuntimeError::new(
                "Instruction pointer out of bounds.".to_string(),
                SourceLocation::default(),
            ));
        }

        let byte = self.get_current_chunk().code()[self.get_current_frame().ip];
        self.get_current_frame_mut().ip += 1;
        Ok(byte)
    }

    fn read_constant(&self, index: usize) -> Value {
        *self
            .get_current_function()
            .chunk
            .constants()
            .get(index)
            .unwrap_or(&Value::Nil)
    }

    fn read_short(&mut self) -> RuntimeResult<usize> {
        let high_byte = self.read_byte()? as usize;
        let low_byte = self.read_byte()? as usize;
        Ok((high_byte << 8) | low_byte)
    }

    fn push(&mut self, value: Value) {
        if self.stack_top >= STACK_MAX {
            panic!(
                "Stack overflow: maximum stack size of {} exceeded",
                STACK_MAX
            );
        }

        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> RuntimeResult<Value> {
        if self.stack_top > 0 {
            self.stack_top -= 1;
            Ok(self.stack[self.stack_top])
        } else {
            Err(QangRuntimeError::new(
                "No value found, unexpected empty stack.".to_string(),
                self.get_current_loc(),
            ))
        }
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
        let op_loc = self.get_previous_loc();
        let b = self.pop()?;
        let a = self.pop()?;

        let value = op(a, b, &mut self.heap, op_loc)?;

        self.push(value);
        Ok(())
    }

    fn call_value(&mut self, value: Value, arg_count: usize) -> RuntimeResult<()> {
        match value {
            Value::Function(handle) => self.call_function(handle, arg_count),
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
        // For the initial call from interpret(), there's no previous location
        let loc = if self.frame_count > 0 {
            self.get_previous_loc()
        } else {
            SourceLocation::default()
        };

        let obj = self.heap.get(handle).ok_or(QangRuntimeError::new(
            "Orphaned identifier".to_string(),
            loc,
        ))?;

        match obj {
            HeapObject::Function(FunctionObject::KangFunction(function)) => {
                if arg_count < function.arity {
                    for _ in arg_count..function.arity {
                        self.stack.push(Value::Nil);
                    }
                }

                if self.frame_count >= FRAME_MAX {
                    return Err(QangRuntimeError::new("Stack overflow.".to_string(), loc));
                }

                // Create new call frame using sliding window approach
                self.frame_count += 1;

                // In the sliding window approach:
                // Before: [other values][function][arg0][arg1]...[argN]
                // After:  [other values][arg0][arg1]...[argN][local vars...]
                // We need to slide arguments to overwrite the function slot

                let value_slot = if self.frame_count == 1 {
                    0 // Initial script call starts with empty stack
                } else {
                    // The function is at position: stack_top - 1 - arg_count
                    // Arguments will be moved to start from this position
                    self.stack_top - 1 - arg_count
                };

                // Slide arguments to overwrite the function slot
                if arg_count > 0 && self.frame_count > 1 {
                    // Move arguments from [value_slot+1, value_slot+2, ..., value_slot+arg_count]
                    // to [value_slot, value_slot+1, ..., value_slot+arg_count-1]
                    for i in 0..arg_count {
                        if let Some(arg_value) = self.stack.get(value_slot + 1 + i).copied() {
                            self.stack[value_slot + i] = arg_value;
                        }
                    }
                    // Adjust stack_top to account for the removed function slot
                    self.stack_top -= 1;
                }

                // Check if this is a script function or user-defined function
                // Script functions start at ip=0, user-defined functions start at ip=1 (to skip parameter count byte)
                let is_script = handle == self.program_handle;

                let call_frame = &mut self.frames[self.frame_count - 1];
                call_frame.current_function = function.clone();
                call_frame.ip = if is_script { 0 } else { 1 };
                call_frame.value_slot = value_slot;

                Ok(())
            }
            HeapObject::Function(FunctionObject::NativeFunction(function)) => {
                self.call_native_function(*function, arg_count)?;
                Ok(())
            }
            _ => Err(QangRuntimeError::new(
                "Value not callable.".to_string(),
                loc,
            )),
        }
    }

    fn call_native_function(
        &mut self,
        function: NativeFunction,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        let loc = self.get_previous_loc();
        let mut args = Vec::<Value>::new();

        // Pop arguments from stack
        for _ in 0..arg_count {
            args.push(self.pop()?);
        }

        // Pop the function value from the stack
        self.pop()?;

        // Fill remaining arguments with nil if needed
        for _ in arg_count..(function.arity) {
            args.push(Value::Nil);
        }

        args.reverse();

        let value = (function.function)(args.as_slice(), self)
            .map_err(|e: NativeFunctionError| {
                e.into_qang_error(loc)
                    .with_stack_trace(self.get_stack_trace())
            })?
            .unwrap_or_default();

        self.push(value);

        Ok(())
    }

    pub fn is_truthy(&self, value: Value) -> bool {
        match value {
            Value::Boolean(boolean) => boolean,
            Value::Nil => false,
            _ => true,
        }
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

    fn get_stack_trace(&self) -> Vec<Trace> {
        let mut traces = Vec::new();

        for frame_idx in 0..self.frame_count {
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
