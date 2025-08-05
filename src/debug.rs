use crate::{
    ObjectHeap,
    chunk::{Chunk, OpCode},
};

#[allow(dead_code)]
pub fn disassemble_chunk(chunk: &Chunk, heap: &ObjectHeap, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.count() {
        offset = disassemble_instruction(chunk, heap, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, heap: &ObjectHeap, offset: usize) -> usize {
    print!("{:04} ", offset);

    if offset > 0 && chunk.locs()[offset].line == chunk.locs()[offset - 1].line {
        print!("   | ");
    } else {
        print!("{:04} ", chunk.locs()[offset].line);
    }

    let instruction = chunk.code()[offset];
    let opcode = OpCode::from(instruction);

    match opcode {
        OpCode::Constant => constant_instruction("OP_CONSTANT", chunk, heap, offset),
        OpCode::Return => simple_instruction("OP_RETURN", offset),
        OpCode::Negate => simple_instruction("OP_NEGATE", offset),
        OpCode::Add => simple_instruction("OP_ADD", offset),
        OpCode::Subtract => simple_instruction("OP_SUBTRACT", offset),
        OpCode::Multiply => simple_instruction("OP_MULTIPLY", offset),
        OpCode::Divide => simple_instruction("OP_DIVIDE", offset),
        OpCode::Nil => simple_instruction("OP_NIL", offset),
        OpCode::True => simple_instruction("OP_TRUE", offset),
        OpCode::False => simple_instruction("OP_FALSE", offset),
        OpCode::Not => simple_instruction("OP_NOT", offset),
        OpCode::Equal => simple_instruction("OP_EQUAL", offset),
        OpCode::Greater => simple_instruction("OP_GREATER", offset),
        OpCode::Less => simple_instruction("OP_LESS", offset),
        OpCode::GreaterEqual => simple_instruction("OP_GREATER_EQUAL", offset),
        OpCode::LessEqual => simple_instruction("OP_LESS_EQUAL", offset),
        OpCode::Modulo => simple_instruction("OP_MODULO", offset),
        OpCode::Pop => simple_instruction("OP_POP", offset),
        OpCode::DefineGlobal => simple_instruction("OP_DEFINE_GLOBAL", offset),
        OpCode::GetGlobal => simple_instruction("OP_GET_GLOBAL", offset),
        OpCode::SetGlobal => simple_instruction("OP_SET_GLOBAL", offset),
        OpCode::GetLocal => simple_instruction("OP_GET_LOCAL", offset),
        OpCode::SetLocal => simple_instruction("OP_SET_LOCAL", offset),
        OpCode::JumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        OpCode::Jump => jump_instruction("OP_JUMP", 1, chunk, offset),
        OpCode::Loop => jump_instruction("OP_LOOP", 1, chunk, offset),
        OpCode::Print => simple_instruction("OP_PRINT", offset),
    }
}

fn constant_instruction(name: &str, chunk: &Chunk, heap: &ObjectHeap, offset: usize) -> usize {
    let constant = chunk.code()[offset + 1] as usize;
    print!("{:<16} {:4} '", name, constant);

    chunk
        .constants()
        .get(constant)
        .cloned()
        .unwrap_or_default()
        .print(heap);

    println!("'");
    offset + 2
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn jump_instruction(name: &str, sign: i32, chunk: &Chunk, offset: usize) -> usize {
    let jump = ((chunk.code()[offset + 1] as u16) << 8) | (chunk.code()[offset + 2] as u16);
    let target = offset as i32 + 3 + sign * (jump as i32);
    println!("{:<16} {:4} -> {}", name, offset, target);
    offset + 3
}
