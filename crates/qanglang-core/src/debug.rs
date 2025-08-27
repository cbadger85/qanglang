use crate::{
    HeapAllocator, Value,
    chunk::{Chunk, OpCode},
};

#[allow(dead_code)]
pub fn disassemble_chunk(chunk: &Chunk, allocator: &HeapAllocator, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;

    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, allocator, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, allocator: &HeapAllocator, offset: usize) -> usize {
    print!("{:04} ", offset);

    if offset > 0 && chunk.locs[offset].line == chunk.locs[offset - 1].line {
        print!("   | ");
    } else {
        print!("{:04} ", chunk.locs[offset].line);
    }

    let instruction = chunk.code[offset];
    let opcode = OpCode::from(instruction);

    match opcode {
        OpCode::Constant => constant_instruction("OP_CONSTANT", chunk, allocator, offset),
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
        OpCode::DefineGlobal => constant_instruction("OP_DEFINE_GLOBAL", chunk, allocator, offset),
        OpCode::GetGlobal => constant_instruction("OP_GET_GLOBAL", chunk, allocator, offset),
        OpCode::SetGlobal => constant_instruction("OP_SET_GLOBAL", chunk, allocator, offset),
        OpCode::GetLocal => byte_instruction("OP_GET_LOCAL", chunk, offset),
        OpCode::SetLocal => byte_instruction("OP_SET_LOCAL", chunk, offset),
        OpCode::JumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        OpCode::Jump => jump_instruction("OP_JUMP", 1, chunk, offset),
        OpCode::Loop => jump_instruction("OP_LOOP", 1, chunk, offset),
        OpCode::Call => byte_instruction("OP_CALL", chunk, offset),
        OpCode::CloseUpvalue => simple_instruction("OP_CLOSE_UPVALUE", offset),
        OpCode::Closure => {
            let mut offset = offset;
            offset += 1; // Skip the opcode itself
            let constant = chunk.code[offset];
            offset += 1;

            print!("{:<16} {:4} '", "OP_CLOSURE", constant);
            let value = chunk.constants[constant as usize];
            println!("{}'", value.to_display_string(allocator));
            let function_obj = match value {
                Value::FunctionDecl(handle) => Some(allocator.get_function(handle)),
                _ => None,
            };

            if let Some(function) = function_obj {
                for _j in 0..function.upvalue_count {
                    let is_local = chunk.code[offset];
                    offset += 1;
                    let index = chunk.code[offset];
                    offset += 1;

                    println!(
                        "{:04}      |                     {} {}",
                        offset - 2,
                        if is_local != 0 { "local" } else { "upvalue" },
                        index
                    );
                }
            }

            offset
        }
        OpCode::GetUpvalue => byte_instruction("OP_GET_UPVALUE", chunk, offset),
        OpCode::SetUpvalue => byte_instruction("OP_GET_UPVALUE", chunk, offset),
        OpCode::Class => constant_instruction("OP_CLASS", chunk, allocator, offset),
        OpCode::GetProperty => constant_instruction("OP_GET_PROPERTY", chunk, allocator, offset),
        OpCode::SetProperty => constant_instruction("OP_SET_PROPERTY", chunk, allocator, offset),
        OpCode::Method => constant_instruction("OP_METHOD", chunk, allocator, offset),
        OpCode::Invoke => invoke_instruction("OP_INVOKE", chunk, allocator, offset),
        OpCode::Inherit => simple_instruction("OP_INHERIT", offset),
        OpCode::GetSuper => constant_instruction("OP_SUPER_INVOKE", chunk, allocator, offset),
        OpCode::SuperInvoke => invoke_instruction("OP_SUPER_INVOKE", chunk, allocator, offset),
        OpCode::InitField => constant_instruction("OP_INIT_FIELD", chunk, allocator, offset),
        OpCode::ArrayLiteral => byte_instruction("OP_ARRAY_LITERAL", chunk, offset),
        OpCode::GetArrayIndex => simple_instruction("OP_GET_ARRAY_INDEX", offset),
        OpCode::SetArrayIndex => simple_instruction("OP_SET_ARRAY_INDEX", offset),
        OpCode::ObjectLiteral => byte_instruction("OP_OBJECT_LITERAL", chunk, offset),
        OpCode::Is => simple_instruction("OP_IS", offset),
    }
}

fn constant_instruction(
    name: &str,
    chunk: &Chunk,
    allocator: &HeapAllocator,
    offset: usize,
) -> usize {
    let constant = chunk.code[offset + 1] as usize;
    print!("{:<16} {:4} '", name, constant);

    chunk
        .constants
        .get(constant)
        .copied()
        .unwrap_or_default()
        .print(allocator);

    println!("'");
    offset + 2
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code[offset + 1];
    println!("{:<16} {:4}", name, slot);
    offset + 2
}

fn jump_instruction(name: &str, sign: i32, chunk: &Chunk, offset: usize) -> usize {
    let jump = ((chunk.code[offset + 1] as u16) << 8) | (chunk.code[offset + 2] as u16);
    let target = offset as i32 + 3 + sign * (jump as i32);
    println!("{:<16} {:4} -> {}", name, offset, target);
    offset + 3
}

fn invoke_instruction(
    name: &str,
    chunk: &Chunk,
    allocator: &HeapAllocator,
    offset: usize,
) -> usize {
    let constant = chunk.code[offset + 1];
    let arg_count = chunk.code[offset + 2];
    print!("{:<16} ({} args) {:4} '", name, arg_count, constant);

    chunk
        .constants
        .get(constant as usize)
        .copied()
        .unwrap_or_default()
        .print(allocator);

    println!("'");
    offset + 3
}

#[allow(dead_code)]
pub fn disassemble_program(allocator: &HeapAllocator) {
    println!("=== PROGRAM DISASSEMBLY ===");
    println!();

    let mut function_count = 0;

    for (index, function) in allocator.iter_functions() {
        function_count += 1;

        let function_name = allocator.strings.get_string(function.name);

        println!(
            "Function #{} (Object #{}) - {}:",
            function_count, index, function_name
        );
        println!("  Arity: {}", function.arity);
        disassemble_chunk(&function.chunk, allocator, function_name);
        println!();
    }

    if function_count == 0 {
        println!("No functions found in the program.");
    } else {
        println!(
            "=== END PROGRAM DISASSEMBLY ({} functions) ===",
            function_count
        );
    }
}

#[macro_export]
macro_rules! debug_log {
    ($is_debug:expr, $($arg:tt)*) => {{
        #[cfg(debug_assertions)]
        {
            if $is_debug {
                println!($($arg)*);
            }
        }
    }};
}
