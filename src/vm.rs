use crate::{
    Chunk, HeapObjectValue, ObjectHeap, OpCode, QangError, SourceMap, Value, ast::SourceSpan,
    compiler::CompilerArtifact, debug::disassemble_instruction,
};

pub type RuntimeResult<T> = Result<T, QangError>;

const STACK_MAX: usize = 256;

pub struct Vm<'a> {
    source_map: &'a SourceMap,
    ip: usize,
    stack: [Value; STACK_MAX],
    stack_top: usize,
}

impl<'a> Vm<'a> {
    pub fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            ip: 0,
            stack_top: 0,
            stack: std::array::from_fn(|_| Value::default()),
        }
    }

    pub fn interpret(
        &mut self,
        artifact: CompilerArtifact,
    ) -> RuntimeResult<(Value, CompilerArtifact)> {
        let result = self.run(artifact)?;
        Ok(result)
    }

    fn run(&mut self, mut artifact: CompilerArtifact) -> RuntimeResult<(Value, CompilerArtifact)> {
        loop {
            let opcode: OpCode = self.read_byte(&artifact.chunk)?.into();

            match opcode {
                OpCode::Constant => {
                    let constant_index = self.read_byte(&artifact.chunk)? as usize;
                    let constant = self.read_constant(constant_index, &artifact.chunk);
                    self.push(constant);
                }
                OpCode::Negate => {
                    if let Value::Number(number) = self.peek(0) {
                        self.stack[self.stack_top - 1] = Value::Number(-number);
                    } else {
                        return Err(QangError::runtime_error(
                            "Operand must be a number.",
                            artifact.chunk.spans()[self.ip - 1],
                        ));
                    }
                }
                OpCode::Add => {
                    self.binary_operation(
                        &mut artifact.chunk,
                        &mut artifact.heap,
                        |a, b, heap| match (a, b) {
                            (Value::Number(num1), Value::Number(num2)) => {
                                Ok(Value::Number(num1 + num2))
                            }
                            (Value::String(handle1), Value::String(handle2)) => {
                                let str1 = heap
                                    .get(handle1)
                                    .and_then(|h| match &h.value {
                                        HeapObjectValue::String(str) => Some(str),
                                        _ => None,
                                    })
                                    .ok_or(QangError::runtime_error(
                                        "Expected string.",
                                        SourceSpan::default(),
                                    ))?;
                                let str2 = heap
                                    .get(handle2)
                                    .and_then(|h| match &h.value {
                                        HeapObjectValue::String(str) => Some(str),
                                        _ => None,
                                    })
                                    .ok_or(QangError::runtime_error(
                                        "Expected string.",
                                        SourceSpan::default(),
                                    ))?;
                                let result = heap
                                    .intern_string(format!("{}{}", str1, str2).into_boxed_str());
                                Ok(Value::String(result))
                            }
                            _ => {
                                return Err(QangError::runtime_error(
                                    "Both operands must be numbers or strings.",
                                    SourceSpan::default(),
                                ));
                            }
                        },
                    )?;
                }
                OpCode::Return => {
                    let value = self.pop(&artifact.chunk)?;
                    return Ok((value, artifact));
                }
                _ => (),
            }
        }
    }

    fn get_current_span(&self, chunk: &Chunk) -> SourceSpan {
        chunk.spans()[self.ip]
    }

    fn read_byte(&mut self, chunk: &Chunk) -> RuntimeResult<u8> {
        if self.ip >= chunk.count() {
            return Err(QangError::runtime_error(
                "Instruction pointer out of bounds.",
                SourceSpan::new(
                    self.source_map.get_source().len(),
                    self.source_map.get_source().len(),
                ),
            ));
        }

        let byte = chunk.code()[self.ip];
        self.ip += 1;
        Ok(byte)
    }

    fn read_constant(&self, index: usize, chunk: &Chunk) -> Value {
        chunk.constants().get(index).cloned().unwrap_or(Value::Nil)
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self, chunk: &Chunk) -> RuntimeResult<Value> {
        if self.stack_top > 0 {
            self.stack_top -= 1;
            let value = std::mem::replace(&mut self.stack[self.stack_top], Value::default());
            Ok(value)
        } else {
            Err(QangError::runtime_error(
                "No value found, unexpected empty stack.",
                self.get_current_span(chunk),
            ))
        }
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack_top - 1 - distance]
    }

    fn binary_operation<F>(
        &mut self,
        chunk: &Chunk,
        heap: &mut ObjectHeap,
        op: F,
    ) -> RuntimeResult<()>
    where
        F: FnOnce(Value, Value, &mut ObjectHeap) -> RuntimeResult<Value>,
    {
        let b = self.pop(chunk)?;
        let a = self.pop(chunk)?;

        let value = op(a, b, heap)?;

        self.push(value);
        Ok(())
    }

    fn debug(&self, artifact: &CompilerArtifact) {
        print!("          ");
        for i in 0..self.stack_top {
            self.stack[i].print(&artifact.heap);
            print!(" ");
        }
        println!();

        disassemble_instruction(artifact, self.ip, self.source_map);
    }
}
