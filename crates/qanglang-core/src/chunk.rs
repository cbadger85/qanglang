use crate::{Value, ast, memory::StringHandle};

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
    DefineGlobal16,
    GetGlobal16,
    SetGlobal16,
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
    GetProperty16,
    SetProperty16,
    Method16,
    Invoke,
    Invoke16,
    Inherit,
    GetSuper,
    SuperInvoke,
    GetSuper16,
    SuperInvoke16,
    InitField,
    InitField16,
    ArrayLiteral,
    GetArrayIndex,
    SetArrayIndex,
    ObjectLiteral,
    Is,
    GetOptionalProperty,
    GetOptionalProperty16,
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
            22 => OpCode::DefineGlobal16,
            23 => OpCode::GetGlobal16,
            24 => OpCode::SetGlobal16,
            25 => OpCode::GetLocal,
            26 => OpCode::SetLocal,
            27 => OpCode::JumpIfFalse,
            28 => OpCode::Jump,
            29 => OpCode::Loop,
            30 => OpCode::Call,
            31 => OpCode::Closure,
            32 => OpCode::GetUpvalue,
            33 => OpCode::SetUpvalue,
            34 => OpCode::CloseUpvalue,
            35 => OpCode::Class,
            36 => OpCode::GetProperty,
            37 => OpCode::SetProperty,
            38 => OpCode::Method,
            39 => OpCode::GetProperty16,
            40 => OpCode::SetProperty16,
            41 => OpCode::Method16,
            42 => OpCode::Invoke,
            43 => OpCode::Invoke16,
            44 => OpCode::Inherit,
            45 => OpCode::GetSuper,
            46 => OpCode::SuperInvoke,
            47 => OpCode::GetSuper16,
            48 => OpCode::SuperInvoke16,
            49 => OpCode::InitField,
            50 => OpCode::InitField16,
            51 => OpCode::ArrayLiteral,
            52 => OpCode::GetArrayIndex,
            53 => OpCode::SetArrayIndex,
            54 => OpCode::ObjectLiteral,
            55 => OpCode::Is,
            56 => OpCode::GetOptionalProperty,
            57 => OpCode::GetOptionalProperty16,
            58 => OpCode::JumpIfNil,
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
    pub identifier_constants: Vec<StringHandle>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            locs: Vec::new(),
            identifier_constants: Vec::new(),
            constants: Vec::new(),
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

    pub fn add_identifier_constant(&mut self, handle: StringHandle) -> usize {
        // Check if string constant already exists
        for (i, existing) in self.identifier_constants.iter().enumerate() {
            if *existing == handle {
                return i;
            }
        }

        // Add new string constant
        self.identifier_constants.push(handle);
        self.identifier_constants.len() - 1
    }
}
