use std::collections::HashMap;

use crate::{
    Chunk, HeapObjectValue, ObjectHeap, OpCode, QangError, Value,
    ast::SourceSpan,
    compiler::{CompilerArtifact, STACK_MAX},
    debug::disassemble_instruction,
    error::ValueConversionError,
    heap::ObjectHandle,
};

pub type RuntimeResult<T> = Result<T, QangError>;

const fn get_value_type(value: &Value) -> &'static str {
    match value {
        Value::Nil => "nil",
        Value::Boolean(_) => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
    }
}

pub struct Vm {
    is_debug: bool,
    ip: usize,
    stack_top: usize,
    stack: [Value; STACK_MAX],
    globals: HashMap<usize, Value>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    fn get_current_span(&self, artifact: &CompilerArtifact) -> SourceSpan {
        self.get_span_at(artifact, self.ip)
    }

    fn get_previous_span(&self, artifact: &CompilerArtifact) -> SourceSpan {
        if self.ip > 0 {
            self.get_span_at(artifact, self.ip - 1)
        } else {
            SourceSpan::default()
        }
    }

    fn get_span_at(&self, artifact: &CompilerArtifact, index: usize) -> SourceSpan {
        artifact
            .chunk
            .spans()
            .get(index)
            .copied()
            .unwrap_or_else(|| {
                SourceSpan::new(
                    artifact.source_map.get_source().len(),
                    artifact.source_map.get_source().len(),
                )
            })
    }

    pub fn interpret(&mut self, artifact: CompilerArtifact) -> RuntimeResult<CompilerArtifact> {
        let result = self.run(artifact)?;
        Ok(result)
    }

    fn run(&mut self, mut artifact: CompilerArtifact) -> RuntimeResult<CompilerArtifact> {
        loop {
            if self.is_debug {
                self.debug(&artifact);
            }
            let opcode: OpCode = self.read_byte(&mut artifact)?.into();

            match opcode {
                OpCode::Constant => {
                    let constant_index = self.read_byte(&mut artifact)? as usize;
                    let constant = self.read_constant(constant_index, &artifact.chunk);
                    self.push(constant);
                }
                OpCode::Negate => {
                    if let Value::Number(number) = self.peek(0) {
                        self.stack[self.stack_top - 1] = Value::Number(-number);
                    } else {
                        return Err(QangError::runtime_error(
                            "Operand must be a number.",
                            self.get_previous_span(&artifact),
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
                    self.binary_operation(&mut artifact, |a, b, heap, span| match (&a, &b) {
                        (Value::Number(num1), Value::Number(num2)) => {
                            Ok(Value::Number(num1 + num2))
                        }
                        (Value::String(_), Value::String(_)) => {
                            let str1: Box<str> = a.into_string(heap, span)?;
                            let str2: Box<str> = b.into_string(heap, span)?;
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
                OpCode::Subtract => self.binary_operation(&mut artifact, |a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;

                    Ok((a - b).into())
                })?,
                OpCode::Multiply => self.binary_operation(&mut artifact, |a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;

                    Ok((a * b).into())
                })?,
                OpCode::Divide => self.binary_operation(&mut artifact, |a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;

                    Ok((a / b).into())
                })?,
                OpCode::Modulo => self.binary_operation(&mut artifact, |a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;

                    Ok((a % b).into())
                })?,
                OpCode::Equal => self.binary_operation(&mut artifact, |a, b, _heap, _span| {
                    Ok(Value::Boolean(a == b))
                })?,
                OpCode::Greater => self.binary_operation(&mut artifact, |a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    Ok(Value::Boolean(a > b))
                })?,
                OpCode::GreaterEqual => {
                    self.binary_operation(&mut artifact, |a, b, _heap, span| {
                        let a: f64 = a.try_into().map_err(|_| {
                            QangError::runtime_error("Both operands must be a number.", span)
                        })?;
                        let b: f64 = b.try_into().map_err(|_| {
                            QangError::runtime_error("Both operands must be a number.", span)
                        })?;
                        Ok(Value::Boolean(a >= b))
                    })?
                }
                OpCode::Less => self.binary_operation(&mut artifact, |a, b, _heap, span| {
                    let a: f64 = a.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    let b: f64 = b.try_into().map_err(|_| {
                        QangError::runtime_error("Both operands must be a number.", span)
                    })?;
                    Ok(Value::Boolean(a < b))
                })?,
                OpCode::LessEqual => {
                    self.binary_operation(&mut artifact, |a, b, _heap, span| {
                        let a: f64 = a.try_into().map_err(|_| {
                            QangError::runtime_error("Both operands must be a number.", span)
                        })?;
                        let b: f64 = b.try_into().map_err(|_| {
                            QangError::runtime_error("Both operands must be a number.", span)
                        })?;
                        Ok(Value::Boolean(a <= b))
                    })?
                }
                OpCode::DefineGlobal => {
                    let span = self.get_previous_span(&artifact);
                    let identifier_handle: ObjectHandle = self
                        .pop(&mut artifact)?
                        .try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                    let value = self.pop(&mut artifact)?;
                    self.globals.insert(identifier_handle.identifier(), value);
                }
                OpCode::GetGlobal => {
                    let span = self.get_previous_span(&artifact);
                    let identifier_handle: ObjectHandle = self
                        .pop(&mut artifact)?
                        .try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                    let value = *self
                        .globals
                        .get(&identifier_handle.identifier())
                        .ok_or_else(|| {
                            let identifier_name = artifact
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
                    let span = self.get_previous_span(&artifact);
                    let identifier_handle: ObjectHandle = self
                        .pop(&mut artifact)?
                        .try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                    if !self.globals.contains_key(&identifier_handle.identifier()) {
                        let identifier_name = artifact
                            .heap
                            .get(identifier_handle)
                            .map(|obj| match &obj.value {
                                HeapObjectValue::String(string) => string.clone(),
                                _ => "unknown".to_string().into_boxed_str(),
                            })
                            .unwrap_or("unknown".to_string().into_boxed_str());
                        return Err(QangError::runtime_error(
                            format!("Undefined variable: {}.", identifier_name).as_str(),
                            span,
                        ));
                    }
                    let value = self.peek(0);
                    self.globals.insert(identifier_handle.identifier(), value);
                }
                OpCode::Print => {
                    let value = self.peek(0);
                    value.print(&artifact.heap);
                    println!();
                }
                OpCode::Return => {
                    return Ok(artifact);
                }
                _ => (),
            }
        }
    }

    fn read_byte(&mut self, artifact: &mut CompilerArtifact) -> RuntimeResult<u8> {
        if self.ip >= artifact.chunk.count() {
            return Err(QangError::runtime_error(
                "Instruction pointer out of bounds.",
                SourceSpan::new(
                    artifact.source_map.get_source().len(),
                    artifact.source_map.get_source().len(),
                ),
            ));
        }

        let byte = artifact.chunk.code()[self.ip];
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

    fn pop(&mut self, artifact: &mut CompilerArtifact) -> RuntimeResult<Value> {
        if self.stack_top > 0 {
            self.stack_top -= 1;
            let value = std::mem::take(&mut self.stack[self.stack_top]);
            Ok(value)
        } else {
            Err(QangError::runtime_error(
                "No value found, unexpected empty stack.",
                self.get_current_span(artifact),
            ))
        }
    }

    fn peek(&self, distance: usize) -> Value {
        self.stack[self.stack_top - 1 - distance]
    }

    fn binary_operation<F>(&mut self, artifact: &mut CompilerArtifact, op: F) -> RuntimeResult<()>
    where
        F: FnOnce(Value, Value, &mut ObjectHeap, SourceSpan) -> RuntimeResult<Value>,
    {
        let op_span = self.get_previous_span(artifact);
        let b = self.pop(artifact)?;
        let a = self.pop(artifact)?;

        let value = op(a, b, &mut artifact.heap, op_span)?;

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

    #[allow(dead_code)]
    fn debug(&self, artifact: &CompilerArtifact) {
        print!("          ");
        for i in 0..self.stack_top {
            self.stack[i].print(&artifact.heap);
            print!(" ");
        }
        println!();

        disassemble_instruction(artifact, self.ip);
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self {
            is_debug: false,
            ip: 0,
            stack_top: 0,
            stack: std::array::from_fn(|_| Value::default()),
            globals: HashMap::new(),
        }
    }
}
