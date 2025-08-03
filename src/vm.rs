use crate::{
    Chunk, HeapObjectValue, ObjectHeap, OpCode, QangError, SourceMap, Value, ast::SourceSpan,
    compiler::CompilerArtifact, debug::disassemble_instruction,
};

pub type RuntimeResult<T> = Result<T, QangError>;

const STACK_MAX: usize = 256;

pub struct Vm;

impl Vm {
    pub fn new() -> Self {
        Self
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
                OpCode::Add => {
                    self.binary_operation(&mut artifact, |a, b, heap, span| match (a, b) {
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
                                .ok_or_else(|| QangError::runtime_error(
                                    "Expected string.",
                                    span,
                                ))?;
                            let str2 = heap
                                .get(handle2)
                                .and_then(|h| match &h.value {
                                    HeapObjectValue::String(str) => Some(str),
                                    _ => None,
                                })
                                .ok_or_else(|| QangError::runtime_error(
                                    "Expected string.",
                                    span,
                                ))?;
                            let result =
                                heap.intern_string(format!("{}{}", str1, str2).into_boxed_str());
                            Ok(Value::String(result))
                        }
                        _ => {
                            return Err(QangError::runtime_error(
                                "Both operands must be numbers or strings.",
                                span,
                            ));
                        }
                    })?;
                }
                OpCode::Return => {
                    let value = self.pop(&mut artifact)?;
                    return Ok((value, artifact));
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
        chunk.constants().get(index).cloned().unwrap_or(Value::Nil)
    }

    fn push(&mut self, artifact: &mut CompilerArtifact, value: Value) {
        artifact.stack[artifact.stack_top] = value;
        artifact.stack_top += 1;
    }

    fn pop(&mut self, artifact: &mut CompilerArtifact) -> RuntimeResult<Value> {
        if artifact.stack_top > 0 {
            artifact.stack_top -= 1;
            let value =
                std::mem::replace(&mut artifact.stack[artifact.stack_top], Value::default());
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
        let b = self.pop(artifact)?;
        let a = self.pop(artifact)?;
        let span = artifact.get_previous_span();

        let value = op(a, b, &mut artifact.heap, span)?;

        self.push(artifact, value);
        Ok(())
    }

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
