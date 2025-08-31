use crate::{Value, ast};

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Constant16,
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
    Call,
    Closure,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
    Class,
    GetProperty,
    SetProperty,
    Method,
    Invoke,
    Inherit,
    GetSuper,
    SuperInvoke,
    InitField,
    ArrayLiteral,
    GetArrayIndex,
    SetArrayIndex,
    ObjectLiteral,
    Is,
    GetOptionalProperty,
    JumpIfNil,
}

impl From<u8> for OpCode {
    fn from(byte: u8) -> Self {
        match byte {
            0 => OpCode::Constant,
            1 => OpCode::Constant16,
            2 => OpCode::Return,
            3 => OpCode::Negate,
            4 => OpCode::Add,
            5 => OpCode::Subtract,
            6 => OpCode::Multiply,
            7 => OpCode::Divide,
            8 => OpCode::Nil,
            9 => OpCode::True,
            10 => OpCode::False,
            11 => OpCode::Not,
            12 => OpCode::Equal,
            13 => OpCode::Greater,
            14 => OpCode::Less,
            15 => OpCode::GreaterEqual,
            16 => OpCode::LessEqual,
            17 => OpCode::Modulo,
            18 => OpCode::Pop,
            19 => OpCode::DefineGlobal,
            20 => OpCode::GetGlobal,
            21 => OpCode::SetGlobal,
            22 => OpCode::GetLocal,
            23 => OpCode::SetLocal,
            24 => OpCode::JumpIfFalse,
            25 => OpCode::Jump,
            26 => OpCode::Loop,
            27 => OpCode::Call,
            28 => OpCode::Closure,
            29 => OpCode::GetUpvalue,
            30 => OpCode::SetUpvalue,
            31 => OpCode::CloseUpvalue,
            32 => OpCode::Class,
            33 => OpCode::GetProperty,
            34 => OpCode::SetProperty,
            35 => OpCode::Method,
            36 => OpCode::Invoke,
            37 => OpCode::Inherit,
            38 => OpCode::GetSuper,
            39 => OpCode::SuperInvoke,
            40 => OpCode::InitField,
            41 => OpCode::ArrayLiteral,
            42 => OpCode::GetArrayIndex,
            43 => OpCode::SetArrayIndex,
            44 => OpCode::ObjectLiteral,
            45 => OpCode::Is,
            46 => OpCode::GetOptionalProperty,
            47 => OpCode::JumpIfNil,
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
    fn from(value: ast::EqualityOperator) -> Self {
        match value {
            ast::EqualityOperator::Is => OpCode::Is,
            _ => OpCode::Equal,
        }
    }
}

impl From<OpCode> for u8 {
    fn from(op: OpCode) -> Self {
        op as u8
    }
}

#[derive(Debug, Clone, PartialEq, Default, Copy)]
pub struct SourceLocation {
    pub line: u32,
    pub col: u32,
}

impl SourceLocation {
    pub fn new(line: u32, col: u32) -> Self {
        Self { line, col }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub locs: Vec<SourceLocation>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            locs: Vec::new(),
            constants: Vec::with_capacity(256), // Reserve space for strings
        }
    }

    pub fn write(&mut self, byte: u8, loc: SourceLocation) {
        self.code.push(byte);
        self.locs.push(loc);
    }

    pub fn write_opcode(&mut self, opcode: OpCode, loc: SourceLocation) {
        self.write(opcode as u8, loc);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        // Check if constant already exists
        for (i, existing) in self.constants.iter().enumerate() {
            if *existing == value {
                return i;
            }
        }
        
        // Add new constant
        self.constants.push(value);
        self.constants.len() - 1
    }
}
