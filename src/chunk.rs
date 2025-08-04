use crate::{
    Value,
    ast::{self, SourceSpan},
};

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
    JumpIfFalse,
    Jump,
    Loop,
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
            23 => OpCode::JumpIfFalse,
            24 => OpCode::Jump,
            25 => OpCode::Loop,
            26 => OpCode::Print,
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

    pub fn code_mut(&mut self) -> &mut [u8] {
        &mut self.code
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn spans(&self) -> &[SourceSpan] {
        &self.spans
    }
}
