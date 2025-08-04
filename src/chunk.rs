use crate::{
    HeapObjectValue, ObjectHeap, QangError,
    ast::{self, SourceSpan},
    error::ValueConversionError,
    heap::ObjectHandle,
};

pub const fn get_value_type(value: &Value) -> &'static str {
    match value {
        Value::Nil => "nil",
        Value::Boolean(_) => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(ObjectHandle),
}

impl Value {
    pub fn print(&self, heap: &ObjectHeap) {
        match self {
            Value::Nil => print!("nil"),
            Value::Number(number) => print!("{}", number),
            Value::String(handle) => {
                if let Some(object) = heap.get(*handle) {
                    if let HeapObjectValue::String(str) = &object.value {
                        print!("\"{}\"", str);
                        return;
                    }
                }
                print!("nil")
            }
            Value::Boolean(boolean) => print!("{}", boolean),
        }
    }

    pub fn into_string(self, heap: &ObjectHeap, span: SourceSpan) -> Result<Box<str>, QangError> {
        match self {
            Value::String(handle) => heap
                .get(handle)
                .map(|h| h.value.clone())
                .ok_or(QangError::runtime_error(
                    "Expected string, found nil.",
                    span,
                ))
                .and_then(|v| {
                    v.try_into()
                        .map_err(|e: ValueConversionError| e.into_qang_error(span))
                }),
            _ => Err(QangError::runtime_error(
                format!("Expected string, found {}.", get_value_type(&self)).as_str(),
                span,
            )),
        }
    }
}

impl From<f64> for Value {
    fn from(num: f64) -> Self {
        Value::Number(num)
    }
}

impl TryFrom<Value> for f64 {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(number) => Ok(number),
            _ => Err(ValueConversionError::new(format!(
                "Expected number, found {}.",
                get_value_type(&value)
            ))),
        }
    }
}

impl TryFrom<Value> for ObjectHandle {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(handle) => Ok(handle),
            _ => Err(ValueConversionError::new(format!(
                "Expected referenced value, found {}.",
                get_value_type(&value)
            ))),
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Boolean(boolean) => Ok(boolean),
            _ => Err(ValueConversionError::new(format!(
                "Expected boolean, found {}.",
                get_value_type(&value)
            ))),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum OpCode {
    // Constant16, to support encoding constants in two bytes
    Constant,
    Return,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Modulo,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    Print,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => OpCode::Constant,
            1 => OpCode::Return,
            2 => OpCode::Negate,
            3 => OpCode::Add,
            4 => OpCode::Subtract,
            5 => OpCode::Multiply,
            6 => OpCode::Divide,
            7 => OpCode::Nil,
            8 => OpCode::True,
            9 => OpCode::False,
            10 => OpCode::Not,
            11 => OpCode::Equal,
            12 => OpCode::Greater,
            13 => OpCode::Less,
            14 => OpCode::GreaterEqual,
            15 => OpCode::LessEqual,
            16 => OpCode::Modulo,
            17 => OpCode::Pop,
            18 => OpCode::DefineGlobal,
            19 => OpCode::GetGlobal,
            20 => OpCode::SetGlobal,
            21 => OpCode::GetLocal,
            22 => OpCode::SetLocal,
            23 => OpCode::Print,
            _ => panic!("Unknown opcode: {}", byte),
        }
    }
}

impl From<bool> for OpCode {
    fn from(boolean: bool) -> Self {
        if boolean { OpCode::True } else { OpCode::False }
    }
}

impl From<ast::ComparisonOperator> for OpCode {
    fn from(value: ast::ComparisonOperator) -> Self {
        match value {
            ast::ComparisonOperator::Greater => OpCode::Greater,
            ast::ComparisonOperator::Less => OpCode::Less,
            ast::ComparisonOperator::GreaterEqual => OpCode::GreaterEqual,
            ast::ComparisonOperator::LessEqual => OpCode::LessEqual,
        }
    }
}

impl From<ast::TermOperator> for OpCode {
    fn from(value: ast::TermOperator) -> Self {
        match value {
            ast::TermOperator::Add => OpCode::Add,
            ast::TermOperator::Subtract => OpCode::Subtract,
        }
    }
}

impl From<ast::FactorOperator> for OpCode {
    fn from(value: ast::FactorOperator) -> Self {
        match value {
            ast::FactorOperator::Divide => OpCode::Divide,
            ast::FactorOperator::Multiply => OpCode::Multiply,
            ast::FactorOperator::Modulo => OpCode::Modulo,
        }
    }
}

impl From<ast::UnaryOperator> for OpCode {
    fn from(value: ast::UnaryOperator) -> Self {
        match value {
            ast::UnaryOperator::Minus => OpCode::Negate,
            ast::UnaryOperator::Not => OpCode::Not,
        }
    }
}

impl From<ast::EqualityOperator> for OpCode {
    fn from(_value: ast::EqualityOperator) -> Self {
        OpCode::Equal
    }
}

impl From<OpCode> for u8 {
    fn from(op: OpCode) -> Self {
        op as u8
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Chunk {
    code: Vec<u8>,
    spans: Vec<SourceSpan>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            spans: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn write(&mut self, byte: u8, span: SourceSpan) {
        self.code.push(byte);
        self.spans.push(span);
    }

    pub fn write_opcode(&mut self, opcode: OpCode, span: SourceSpan) {
        self.write(opcode as u8, span);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn count(&self) -> usize {
        self.code.len()
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn spans(&self) -> &[SourceSpan] {
        &self.spans
    }
}
