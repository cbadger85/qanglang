use crate::{Chunk, ObjectHeap, OpCode, SourceMap};

pub fn disassemble_chunk(heap: &ObjectHeap, chunk: &Chunk, source_map: &SourceMap, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.count() {
        offset = disassemble_instruction(heap, chunk, offset, source_map);
    }
}

pub fn disassemble_instruction(
    heap: &ObjectHeap,
    chunk: &Chunk,
    offset: usize,
    source_map: &SourceMap,
) -> usize {
    print!("{:04} ", offset);

    if offset > 0
        && source_map.get_line_number(chunk.spans()[offset].end)
            == source_map.get_line_number(chunk.spans()[offset - 1].start)
    {
        print!("   | ");
    } else {
        chunk.spans()[offset].print(source_map);
    }

    let instruction = chunk.code()[offset];
    let opcode = OpCode::from(instruction);

    match opcode {
        OpCode::Constant => constant_instruction("OP_CONSTANT", heap, chunk, offset),
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
    }
}

fn constant_instruction(name: &str, heap: &ObjectHeap, chunk: &Chunk, offset: usize) -> usize {
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
