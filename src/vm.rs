use crate::{
    Chunk, HeapObjectValue, ObjectHeap, OpCode, QangError, Value, ast::SourceSpan,
    compiler::CompilerArtifact, debug::disassemble_instruction, error::ValueConversionError,
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

#[derive(Default)]
pub struct Vm {
    is_debug: bool,
}

impl Vm {
    pub fn new() -> Self {
        Self { is_debug: false }
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
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
                    self.push(&mut artifact, constant);
                }
                OpCode::Negate => {
                    if let Value::Number(number) = self.peek(&mut artifact, 0) {
                        artifact.stack[artifact.stack_top - 1] = Value::Number(-number);
                    } else {
                        return Err(QangError::runtime_error(
                            "Operand must be a number.",
                            artifact.get_previous_span(),
                        ));
                    }
                }
                OpCode::Not => {
                    let value = self.peek(&mut artifact, 0);
                    artifact.stack[artifact.stack_top - 1] = Value::Boolean(!self.is_truthy(value));
                }
                OpCode::True => {
                    self.push(&mut artifact, Value::Boolean(true));
                }
                OpCode::False => {
                    self.push(&mut artifact, Value::Boolean(false));
                }
                OpCode::Nil => {
                    self.push(&mut artifact, Value::Nil);
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
                    let span = artifact.get_previous_span();
                    let identifier_handle: ObjectHandle = self
                        .pop(&mut artifact)?
                        .try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                    let value = self.pop(&mut artifact)?;
                    artifact
                        .globals
                        .insert(identifier_handle.identifier(), value);
                }
                OpCode::GetGlobal => {
                    let span = artifact.get_previous_span();
                    let identifier_handle: ObjectHandle = self
                        .pop(&mut artifact)?
                        .try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                    let value = *artifact
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
                    self.push(&mut artifact, value);
                }
                OpCode::SetGlobal => {
                    let span = artifact.get_previous_span();
                    let identifier_handle: ObjectHandle = self
                        .pop(&mut artifact)?
                        .try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))?;
                    if !artifact
                        .globals
                        .contains_key(&identifier_handle.identifier())
                    {
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
                    let value = *self.peek(&mut artifact, 0);
                    artifact
                        .globals
                        .insert(identifier_handle.identifier(), value);
                }
                OpCode::Print => {
                    let value = *self.peek(&mut artifact, 0);
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
        if artifact.ip >= artifact.chunk.count() {
            return Err(QangError::runtime_error(
                "Instruction pointer out of bounds.",
                SourceSpan::new(
                    artifact.source_map.get_source().len(),
                    artifact.source_map.get_source().len(),
                ),
            ));
        }

        let byte = artifact.chunk.code()[artifact.ip];
        artifact.ip += 1;
        Ok(byte)
    }

    fn read_constant(&self, index: usize, chunk: &Chunk) -> Value {
        *chunk.constants().get(index).unwrap_or(&Value::Nil)
    }

    fn push(&mut self, artifact: &mut CompilerArtifact, value: Value) {
        artifact.stack[artifact.stack_top] = value;
        artifact.stack_top += 1;
    }

    fn pop(&mut self, artifact: &mut CompilerArtifact) -> RuntimeResult<Value> {
        if artifact.stack_top > 0 {
            artifact.stack_top -= 1;
            let value = std::mem::take(&mut artifact.stack[artifact.stack_top]);
            Ok(value)
        } else {
            Err(QangError::runtime_error(
                "No value found, unexpected empty stack.",
                artifact.get_current_span(),
            ))
        }
    }

    fn peek<'a>(&self, artifact: &'a mut CompilerArtifact, distance: usize) -> &'a Value {
        &artifact.stack[artifact.stack_top - 1 - distance]
    }

    fn binary_operation<F>(&mut self, artifact: &mut CompilerArtifact, op: F) -> RuntimeResult<()>
    where
        F: FnOnce(Value, Value, &mut ObjectHeap, SourceSpan) -> RuntimeResult<Value>,
    {
        let op_span = artifact.get_previous_span();
        let b = self.pop(artifact)?;
        let a = self.pop(artifact)?;

        let value = op(a, b, &mut artifact.heap, op_span)?;

        self.push(artifact, value);
        Ok(())
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Boolean(boolean) => *boolean,
            Value::Nil => false,
            _ => true,
        }
    }

    #[allow(dead_code)]
    fn debug(&self, artifact: &CompilerArtifact) {
        print!("          ");
        for i in 0..artifact.stack_top {
            artifact.stack[i].print(&artifact.heap);
            print!(" ");
        }
        println!();

        disassemble_instruction(artifact, artifact.ip, &artifact.source_map);
    }
}
