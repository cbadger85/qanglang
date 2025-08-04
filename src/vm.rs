use std::{collections::HashMap, rc::Rc};

use crate::{
    Chunk, HeapObject, HeapObjectValue, ObjectHeap, OpCode, QangError, SourceMap, Value,
    ast::SourceSpan,
    compiler::{CompilerArtifact, STACK_MAX},
    debug::disassemble_instruction,
    error::ValueConversionError,
    heap::{NativeFunction, ObjectHandle},
    value::get_value_type,
};

pub type RuntimeResult<T> = Result<T, QangError>;

pub type NativeFn = fn(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, QangError>;

pub struct Vm {
    is_debug: bool,
    ip: usize,
    stack_top: usize,
    stack: [Value; STACK_MAX],
    globals: HashMap<usize, Value>,
    source_map: Rc<SourceMap>,
    heap: ObjectHeap,
    chunk: Chunk,
}

impl Vm {
    pub fn new(artifact: CompilerArtifact) -> Self {
        Self {
            is_debug: false,
            ip: 0,
            stack_top: 0,
            stack: std::array::from_fn(|_| Value::default()),
            globals: HashMap::new(),
            source_map: artifact.source_map,
            heap: artifact.heap,
            chunk: artifact.chunk,
        }
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

        let handle = self.heap.allocate_object(HeapObject {
            value: HeapObjectValue::NativeFunction(native_function),
        });

        self.globals
            .insert(identifier_handle.identifier(), Value::Function(handle));

        self
    }

    fn call_native_function(function: NativeFn) -> Result<Value, QangError> {
        Ok(Value::Nil)
    }

    fn get_current_span(&self) -> SourceSpan {
        self.get_span_at(self.ip)
    }

    fn get_previous_span(&self) -> SourceSpan {
        if self.ip > 0 {
            self.get_span_at(self.ip - 1)
        } else {
            SourceSpan::default()
        }
    }

    fn get_span_at(&self, index: usize) -> SourceSpan {
        self.chunk.spans().get(index).copied().unwrap_or_else(|| {
            SourceSpan::new(
                self.source_map.get_source().len(),
                self.source_map.get_source().len(),
            )
        })
    }

    pub fn interpret(&mut self) -> RuntimeResult<()> {
        if self.is_debug {
            self.debug();
        }
        self.run()
    }

    fn run(&mut self) -> RuntimeResult<()> {
        loop {
            let opcode: OpCode = self.read_byte()?.into();

            match opcode {
                OpCode::Constant => {
                    let constant_index = self.read_byte()? as usize;
                    let constant = self.read_constant(constant_index, &self.chunk);
                    self.push(constant);
                }
                OpCode::Negate => {
                    if let Value::Number(number) = self.peek(0) {
                        self.stack[self.stack_top - 1] = Value::Number(-number);
                    } else {
                        return Err(QangError::runtime_error(
                            "Operand must be a number.",
                            self.get_previous_span(),
                        ));
                    }
                }
                OpCode::Not => {
                    let value = self.peek(0);
                    self.stack[self.stack_top - 1] = Value::Boolean(!self.is_truthy(value));
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
                    self.binary_operation(|a, b, heap, span| match (&a, &b) {
                        (Value::Number(num1), Value::Number(num2)) => {
                            Ok(Value::Number(num1 + num2))
                        }
                        (Value::String(_), Value::String(_)) => {
                            let str1: Box<str> = a
                                .into_string(heap)
                                .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                            let str2: Box<str> = b
                                .into_string(heap)
                                .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                            let result =
                                heap.intern_string(format!("{}{}", str1, str2).into_boxed_str());
                            Ok(Value::String(result))
                        }
                        (Value::Number(_), _) => Err(QangError::runtime_error(
                            &format!("Cannot add number to {}.", get_value_type(&b)),
                            span,
                        )),
                        (Value::String(_), _) => Err(QangError::runtime_error(
                            &format!("Cannot add string to {}.", get_value_type(&b)),
                            span,
                        )),
                        (_, Value::Number(_)) => Err(QangError::runtime_error(
                            &format!("Cannot add {} to number.", get_value_type(&a)),
                            span,
                        )),
                        (_, Value::String(_)) => Err(QangError::runtime_error(
                            &format!("Cannot add {} to string.", get_value_type(&a)),
                            span,
                        )),
                        _ => Err(QangError::runtime_error(
                            "Both operands must be a numbers or strings.",
                            span,
                        )),
                    })?;
                }
                OpCode::Subtract => self.binary_operation(|a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;

                    Ok((a - b).into())
                })?,
                OpCode::Multiply => self.binary_operation(|a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;

                    Ok((a * b).into())
                })?,
                OpCode::Divide => self.binary_operation(|a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;

                    Ok((a / b).into())
                })?,
                OpCode::Modulo => self.binary_operation(|a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;

                    Ok((a % b).into())
                })?,
                OpCode::Equal => {
                    self.binary_operation(|a, b, _heap, _span| Ok(Value::Boolean(a == b)))?
                }
                OpCode::Greater => self.binary_operation(|a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    Ok(Value::Boolean(a > b))
                })?,
                OpCode::GreaterEqual => self.binary_operation(|a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    Ok(Value::Boolean(a >= b))
                })?,
                OpCode::Less => self.binary_operation(|a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    Ok(Value::Boolean(a < b))
                })?,
                OpCode::LessEqual => self.binary_operation(|a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    Ok(Value::Boolean(a <= b))
                })?,
                OpCode::DefineGlobal => {
                    let span = self.get_previous_span();
                    let identifier_handle: ObjectHandle = self
                        .pop()?
                        .try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                    let value = self.pop()?;
                    self.globals.insert(identifier_handle.identifier(), value);
                }
                OpCode::GetGlobal => {
                    let span = self.get_previous_span();
                    let identifier_handle: ObjectHandle = self
                        .pop()?
                        .try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                    let value = *self
                        .globals
                        .get(&identifier_handle.identifier())
                        .ok_or_else(|| {
                            let identifier_name = self
                                .heap
                                .get(identifier_handle)
                                .map(|obj| match &obj.value {
                                    HeapObjectValue::String(string) => string.clone(),
                                    _ => "unknown".to_string().into_boxed_str(),
                                })
                                .unwrap_or("unknown".to_string().into_boxed_str());
                            QangError::runtime_error(
                                format!("Undefined variable: {}.", identifier_name).as_str(),
                                span,
                            )
                        })?;
                    self.push(value);
                }
                OpCode::SetGlobal => {
                    let span = self.get_previous_span();
                    let identifier_handle: ObjectHandle = self
                        .pop()?
                        .try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))?;

                    if !self.globals.contains_key(&identifier_handle.identifier()) {
                        let identifier_name = self
                            .get_identifier_name(identifier_handle)
                            .unwrap_or("<unknown>".to_string().into_boxed_str());

                        return Err(QangError::runtime_error(
                            format!("Undefined variable: {}.", identifier_name).as_str(),
                            span,
                        ));
                    }
                    let value = self.peek(0);
                    self.globals.insert(identifier_handle.identifier(), value);
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte()?;
                    self.push(self.stack[slot as usize]);
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte()?;
                    let value = self.peek(0);
                    self.stack[slot as usize] = value;
                }
                OpCode::Print => {
                    let value = self.pop()?;
                    value.print(&self.heap);
                    println!();
                }
                OpCode::Return => {
                    return Ok(());
                }
                _ => (),
            }
        }
    }

    fn read_byte(&mut self) -> RuntimeResult<u8> {
        if self.ip >= self.chunk.count() {
            return Err(QangError::runtime_error(
                "Instruction pointer out of bounds.",
                SourceSpan::new(
                    self.source_map.get_source().len(),
                    self.source_map.get_source().len(),
                ),
            ));
        }

        let byte = self.chunk.code()[self.ip];
        self.ip += 1;
        Ok(byte)
    }

    fn read_constant(&self, index: usize, chunk: &Chunk) -> Value {
        *chunk.constants().get(index).unwrap_or(&Value::Nil)
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> RuntimeResult<Value> {
        if self.stack_top > 0 {
            self.stack_top -= 1;
            let value = std::mem::take(&mut self.stack[self.stack_top]);
            Ok(value)
        } else {
            Err(QangError::runtime_error(
                "No value found, unexpected empty stack.",
                self.get_current_span(),
            ))
        }
    }

    fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack_top - 1 - distance]
    }

    fn binary_operation<F>(&mut self, op: F) -> RuntimeResult<()>
    where
        F: FnOnce(Value, Value, &mut ObjectHeap, SourceSpan) -> RuntimeResult<Value>,
    {
        let op_span = self.get_previous_span();
        let b = self.pop()?;
        let a = self.pop()?;

        let value = op(a, b, &mut self.heap, op_span)?;

        self.push(value);
        Ok(())
    }

    fn is_truthy(&self, value: Value) -> bool {
        match value {
            Value::Boolean(boolean) => boolean,
            Value::Nil => false,
            _ => true,
        }
    }

    fn get_identifier_name(&self, handle: ObjectHandle) -> Option<Box<str>> {
        self.heap
            .get(handle)
            .and_then(|obj: &HeapObject| match &obj.value {
                HeapObjectValue::String(identifier) => Some(identifier.clone()),
                _ => None,
            })
    }

    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    pub fn heap(&self) -> &ObjectHeap {
        &self.heap
    }

    pub fn stack(&self) -> &[Value; STACK_MAX] {
        &self.stack
    }

    #[allow(dead_code)]
    fn debug(&self) {
        print!("          ");
        for i in 0..self.stack_top {
            self.stack[i].print(&self.heap);
            print!(" ");
        }
        println!();

        disassemble_instruction(&self.source_map, &self.chunk, &self.heap, self.ip);
    }
}
