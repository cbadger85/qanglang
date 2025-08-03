use crate::{Chunk, ObjectHeap, OpCode, QangError, SourceMap, Value, ast::SourceSpan};

pub type RuntimeResult<T> = Result<T, QangError>;

const STACK_MAX: usize = 256;

pub struct Vm<'a> {
    source_map: &'a SourceMap,
    chunk: Chunk,
    ip: usize,
    heap: ObjectHeap,
    stack: [Value; STACK_MAX],
    stack_top: usize,
}

impl<'a> Vm<'a> {
    pub fn new(source_map: &'a SourceMap, chunk: Chunk, heap: ObjectHeap) -> Self {
        Self {
            source_map,
            chunk,
            ip: 0,
            heap,
            stack_top: 0,
            stack: std::array::from_fn(|_| Value::default()),
        }
    }

    pub fn interpret(mut self) -> RuntimeResult<Value> {
        self.run()
    }

    fn run(&mut self) -> RuntimeResult<Value> {
        loop {
            let opcode: OpCode = self.read_byte()?.into();

            match opcode {
                OpCode::Constant => {
                    let constant_index = self.read_byte()? as usize;
                    let constant = self.read_constant(constant_index)?;
                    self.push(constant);
                }
                OpCode::Return => {
                    return Ok(self.pop()?);
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

    fn read_constant(&self, index: usize) -> RuntimeResult<Value> {
        self.chunk
            .constants()
            .get(index)
            .cloned()
            .ok_or(QangError::runtime_error(
                "Missing value.",
                self.chunk.spans()[self.ip],
            ))
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(&mut self) -> RuntimeResult<Value> {
        if self.stack_top > 0 {
            self.stack_top -= 1;
            let value = std::mem::replace(&mut self.stack[self.stack_top], Value::default());
            Ok(value)
        } else {
            Err(QangError::runtime_error(
                "Unexpected empty stack.",
                self.chunk.spans()[self.ip],
            ))
        }
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack_top - 1 - distance]
    }
}
