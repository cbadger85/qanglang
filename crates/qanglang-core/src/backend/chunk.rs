use crate::{
    Value,
    nodes::{ComparisonOperator, EqualityOperator, FactorOperator, TermOperator, UnaryOperator},
};

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
    Closure16,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
    Class,
    Class16,
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
    TailCall,
    Module,
    Module16,
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
            32 => OpCode::Closure16,
            33 => OpCode::GetUpvalue,
            34 => OpCode::SetUpvalue,
            35 => OpCode::CloseUpvalue,
            36 => OpCode::Class,
            37 => OpCode::Class16,
            38 => OpCode::GetProperty,
            39 => OpCode::SetProperty,
            40 => OpCode::Method,
            41 => OpCode::GetProperty16,
            42 => OpCode::SetProperty16,
            43 => OpCode::Method16,
            44 => OpCode::Invoke,
            45 => OpCode::Invoke16,
            46 => OpCode::Inherit,
            47 => OpCode::GetSuper,
            48 => OpCode::SuperInvoke,
            49 => OpCode::GetSuper16,
            50 => OpCode::SuperInvoke16,
            51 => OpCode::InitField,
            52 => OpCode::InitField16,
            53 => OpCode::ArrayLiteral,
            54 => OpCode::GetArrayIndex,
            55 => OpCode::SetArrayIndex,
            56 => OpCode::ObjectLiteral,
            57 => OpCode::Is,
            58 => OpCode::GetOptionalProperty,
            59 => OpCode::GetOptionalProperty16,
            60 => OpCode::JumpIfNil,
            61 => OpCode::TailCall,
            62 => OpCode::Module,
            63 => OpCode::Module16,
            _ => panic!("Unknown opcode: {}", byte),
        }
    }
}

impl From<bool> for OpCode {
    fn from(boolean: bool) -> Self {
        if boolean { OpCode::True } else { OpCode::False }
    }
}

impl From<ComparisonOperator> for OpCode {
    fn from(value: ComparisonOperator) -> Self {
        match value {
            ComparisonOperator::Greater => OpCode::Greater,
            ComparisonOperator::Less => OpCode::Less,
            ComparisonOperator::GreaterEqual => OpCode::GreaterEqual,
            ComparisonOperator::LessEqual => OpCode::LessEqual,
        }
    }
}

impl From<TermOperator> for OpCode {
    fn from(value: TermOperator) -> Self {
        match value {
            TermOperator::Add => OpCode::Add,
            TermOperator::Subtract => OpCode::Subtract,
        }
    }
}

impl From<FactorOperator> for OpCode {
    fn from(value: FactorOperator) -> Self {
        match value {
            FactorOperator::Divide => OpCode::Divide,
            FactorOperator::Multiply => OpCode::Multiply,
            FactorOperator::Modulo => OpCode::Modulo,
        }
    }
}

impl From<UnaryOperator> for OpCode {
    fn from(value: UnaryOperator) -> Self {
        match value {
            UnaryOperator::Minus => OpCode::Negate,
            UnaryOperator::Not => OpCode::Not,
        }
    }
}

impl From<EqualityOperator> for OpCode {
    fn from(value: EqualityOperator) -> Self {
        match value {
            EqualityOperator::Is => OpCode::Is,
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
}
