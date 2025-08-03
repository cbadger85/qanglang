use crate::{OpCode, SourceMap, compiler::CompilerArtifact};

#[allow(dead_code)]
pub fn disassemble_chunk(artifact: &CompilerArtifact, source_map: &SourceMap, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < artifact.chunk.count() {
        offset = disassemble_instruction(artifact, offset, source_map);
    }
}

pub fn disassemble_instruction(
    artifact: &CompilerArtifact,
    offset: usize,
    source_map: &SourceMap,
) -> usize {
    print!("{:04} ", offset);

    if offset > 0
        && source_map.get_line_number(artifact.chunk.spans()[offset].end)
            == source_map.get_line_number(artifact.chunk.spans()[offset - 1].start)
    {
        print!("   | ");
    } else {
        artifact.chunk.spans()[offset].print(source_map);
    }

    let instruction = artifact.chunk.code()[offset];
    let opcode = OpCode::from(instruction);

    match opcode {
        OpCode::Constant => constant_instruction("OP_CONSTANT", artifact, offset),
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
    }
}

fn constant_instruction(name: &str, artifact: &CompilerArtifact, offset: usize) -> usize {
    let constant = artifact.chunk.code()[offset + 1] as usize;
    print!("{:<16} {:4} '", name, constant);

    artifact
        .chunk
        .constants()
        .get(constant)
        .cloned()
        .unwrap_or_default()
        .print(&artifact.heap);

    println!("'");
    offset + 2
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}
