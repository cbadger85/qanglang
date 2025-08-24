use crate::{
    Value,
    memory::{Arena, Index},
};

const CHUNK_SIZE: usize = 32;

pub type ArrayHandle = Index;
pub type ChunkHandle = Index;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ArrayHeader {
    first_chunk: Option<ChunkHandle>,
    length: usize,
    chunks_count: usize,
    is_marked: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayChunk {
    data: [Option<Value>; CHUNK_SIZE],
    next_chunk: Option<ChunkHandle>,
    used: usize, // how many slots are actually used
    is_marked: bool,
}

pub struct ArrayArena {
    heads: Arena<ArrayHeader>,
    chunks: Arena<ArrayChunk>,
}

impl ArrayArena {
    pub fn new() -> Self {
        Self::with_capacity(64)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            heads: Arena::with_capacity(capacity),
            chunks: Arena::with_capacity(capacity),
        }
    }

    pub fn create_array(&mut self, length: usize) -> ArrayHandle {
        let chunks_needed = if length == 0 {
            0
        } else {
            (length - 1) / CHUNK_SIZE + 1
        };

        let mut first_chunk = None;
        let mut prev_chunk = None;

        // Create all chunks needed
        for _ in 0..chunks_needed {
            let chunk = ArrayChunk {
                data: [None; CHUNK_SIZE],
                next_chunk: None,
                used: 0,
                is_marked: false,
            };
            let chunk_handle = self.chunks.insert(chunk);

            if first_chunk.is_none() {
                first_chunk = Some(chunk_handle);
            }

            if let Some(prev) = prev_chunk {
                self.chunks[prev].next_chunk = Some(chunk_handle);
            }

            prev_chunk = Some(chunk_handle);
        }

        let header = ArrayHeader {
            first_chunk,
            length,
            chunks_count: chunks_needed,
            is_marked: false,
        };

        self.heads.insert(header)
    }

    pub fn insert(&mut self, handle: ArrayHandle, index: usize, value: Value) -> bool {
        let header = &self.heads[handle];

        if index >= header.length {
            return false;
        }

        let chunk_index = index / CHUNK_SIZE;
        let slot_index = index % CHUNK_SIZE;

        let mut current_chunk = header.first_chunk;

        // Navigate to the correct chunk
        for _ in 0..chunk_index {
            if let Some(chunk_handle) = current_chunk {
                current_chunk = self.chunks[chunk_handle].next_chunk;
            } else {
                return false;
            }
        }

        if let Some(chunk_handle) = current_chunk {
            let chunk = &mut self.chunks[chunk_handle];
            let old_value = chunk.data[slot_index].replace(value);

            // Update used count if this slot was previously empty
            if old_value.is_none() {
                chunk.used += 1;
            }

            true
        } else {
            false
        }
    }

    pub fn get(&self, handle: ArrayHandle, index: usize) -> Value {
        let header = &self.heads[handle];

        if index >= header.length {
            return Value::Nil;
        }

        let chunk_index = index / CHUNK_SIZE;
        let slot_index = index % CHUNK_SIZE;

        let mut current_chunk = header.first_chunk;

        // Navigate to the correct chunk
        for _ in 0..chunk_index {
            if let Some(chunk_handle) = current_chunk {
                current_chunk = self.chunks[chunk_handle].next_chunk;
            } else {
                return Value::Nil;
            }
        }

        if let Some(chunk_handle) = current_chunk {
            self.chunks[chunk_handle].data[slot_index]
                .clone()
                .unwrap_or(Value::Nil)
        } else {
            Value::Nil
        }
    }

    pub fn pop(&mut self, handle: ArrayHandle) -> Value {
        let length = self.heads[handle].length;

        if length == 0 {
            return Value::Nil;
        }

        let last_index = length - 1;
        let chunk_index = last_index / CHUNK_SIZE;
        let slot_index = last_index % CHUNK_SIZE;

        // Find the chunk containing the last element
        let mut current_chunk = self.heads[handle].first_chunk;
        for _ in 0..chunk_index {
            if let Some(chunk_handle) = current_chunk {
                current_chunk = self.chunks[chunk_handle].next_chunk;
            } else {
                return Value::Nil;
            }
        }

        if let Some(chunk_handle) = current_chunk {
            // Extract the value
            let value = self.chunks[chunk_handle].data[slot_index]
                .take()
                .unwrap_or(Value::Nil);

            if self.chunks[chunk_handle].used > 0 {
                self.chunks[chunk_handle].used -= 1;
            }

            let should_free_chunk = self.chunks[chunk_handle].used == 0 && chunk_index > 0;
            let next_chunk = self.chunks[chunk_handle].next_chunk;

            // If this chunk is now empty and it's not the first chunk, free it
            if should_free_chunk {
                // Find the previous chunk and unlink this one
                let mut prev_chunk = self.heads[handle].first_chunk;
                for _ in 0..(chunk_index - 1) {
                    if let Some(prev_handle) = prev_chunk {
                        prev_chunk = self.chunks[prev_handle].next_chunk;
                    }
                }

                if let Some(prev_handle) = prev_chunk {
                    self.chunks[prev_handle].next_chunk = next_chunk;
                }

                // Free the chunk
                let _ = self.chunks.remove(chunk_handle);
                self.heads[handle].chunks_count -= 1;
            }

            self.heads[handle].length -= 1;
            value
        } else {
            Value::Nil
        }
    }

    pub fn push(&mut self, handle: ArrayHandle, value: Value) {
        let header = &mut self.heads[handle];
        let new_index = header.length;

        // Check if we need to allocate a new chunk
        let chunks_needed = if new_index == 0 {
            0
        } else {
            (new_index - 1) / CHUNK_SIZE + 1
        };

        if chunks_needed >= header.chunks_count {
            // Need to allocate a new chunk
            let new_chunk = ArrayChunk {
                data: [None; CHUNK_SIZE],
                next_chunk: None,
                used: 0,
                is_marked: false,
            };
            let new_chunk_handle = self.chunks.insert(new_chunk);

            // Find the last chunk and link it
            if let Some(first) = header.first_chunk {
                let mut current = first;
                while let Some(next) = self.chunks[current].next_chunk {
                    current = next;
                }
                self.chunks[current].next_chunk = Some(new_chunk_handle);
            } else {
                header.first_chunk = Some(new_chunk_handle);
            }

            header.chunks_count += 1;
        }

        // Insert the value at the new position
        let chunk_index = new_index / CHUNK_SIZE;
        let slot_index = new_index % CHUNK_SIZE;

        let mut current_chunk = header.first_chunk;
        for _ in 0..chunk_index {
            if let Some(chunk_handle) = current_chunk {
                current_chunk = self.chunks[chunk_handle].next_chunk;
            }
        }

        if let Some(chunk_handle) = current_chunk {
            self.chunks[chunk_handle].data[slot_index] = Some(value);
            self.chunks[chunk_handle].used += 1;
        }

        header.length += 1;
    }

    pub fn reverse(&mut self, handle: ArrayHandle) {
        let header = &self.heads[handle];
        let length = header.length;

        if length <= 1 {
            return;
        }

        // Reverse by swapping elements from start and end
        for i in 0..(length / 2) {
            let start_idx = i;
            let end_idx = length - 1 - i;

            // Get values at both positions
            let start_chunk_idx = start_idx / CHUNK_SIZE;
            let start_slot_idx = start_idx % CHUNK_SIZE;
            let end_chunk_idx = end_idx / CHUNK_SIZE;
            let end_slot_idx = end_idx % CHUNK_SIZE;

            // Navigate to start chunk
            let mut current_chunk = header.first_chunk;
            for _ in 0..start_chunk_idx {
                if let Some(chunk_handle) = current_chunk {
                    current_chunk = self.chunks[chunk_handle].next_chunk;
                }
            }
            let start_chunk_handle = current_chunk;

            // Navigate to end chunk
            let mut current_chunk = header.first_chunk;
            for _ in 0..end_chunk_idx {
                if let Some(chunk_handle) = current_chunk {
                    current_chunk = self.chunks[chunk_handle].next_chunk;
                }
            }
            let end_chunk_handle = current_chunk;

            if let (Some(start_handle), Some(end_handle)) = (start_chunk_handle, end_chunk_handle) {
                if start_handle == end_handle {
                    // Same chunk, simple swap
                    let chunk = &mut self.chunks[start_handle];
                    chunk.data.swap(start_slot_idx, end_slot_idx);
                } else {
                    // Different chunks, need to extract and swap
                    let start_value = self.chunks[start_handle].data[start_slot_idx].clone();
                    let end_value = self.chunks[end_handle].data[end_slot_idx].clone();

                    self.chunks[start_handle].data[start_slot_idx] = end_value;
                    self.chunks[end_handle].data[end_slot_idx] = start_value;
                }
            }
        }
    }

    pub fn length(&self, handle: ArrayHandle) -> usize {
        self.heads[handle].length
    }

    pub fn concat(&mut self, handle1: ArrayHandle, handle2: ArrayHandle) -> ArrayHandle {
        let len1 = self.heads[handle1].length;
        let len2 = self.heads[handle2].length;
        let total_length = len1 + len2;

        // Create new array with combined length
        let new_handle = self.create_array(total_length);

        // Copy elements from first array
        for i in 0..len1 {
            let value = self.get(handle1, i);
            self.insert(new_handle, i, value);
        }

        // Copy elements from second array
        for i in 0..len2 {
            let value = self.get(handle2, i);
            self.insert(new_handle, len1 + i, value);
        }

        new_handle
    }

    pub fn slice(&mut self, handle: ArrayHandle, begin: usize, end: usize) -> ArrayHandle {
        let source_length = self.heads[handle].length;

        // Clamp bounds to valid range
        let start = begin.min(source_length);
        let finish = end.min(source_length).max(start);
        let slice_length = finish - start;

        if slice_length == 0 {
            return self.create_array(0);
        }

        // Create new array with slice length
        let new_handle = self.create_array(slice_length);

        // Copy elements from source array
        for i in 0..slice_length {
            let value = self.get(handle, start + i);
            self.insert(new_handle, i, value);
        }

        new_handle
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_empty_array() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);
        assert_eq!(arena.length(handle), 0);
        assert_eq!(arena.get(handle, 0), Value::Nil);
    }

    #[test]
    fn test_create_small_array() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(5);
        assert_eq!(arena.length(handle), 5);

        // All elements should be Nil initially
        for i in 0..5 {
            assert_eq!(arena.get(handle, i), Value::Nil);
        }
    }

    #[test]
    fn test_create_large_array() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(100);
        assert_eq!(arena.length(handle), 100);
        assert_eq!(arena.get(handle, 99), Value::Nil);
        assert_eq!(arena.get(handle, 100), Value::Nil); // Out of bounds
    }

    #[test]
    fn test_insert_and_get() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(3);

        assert!(arena.insert(handle, 0, Value::Number(1.0)));
        assert!(arena.insert(handle, 1, Value::Number(2.0)));
        assert!(arena.insert(handle, 2, Value::Number(3.0)));

        assert_eq!(arena.get(handle, 0), Value::Number(1.0));
        assert_eq!(arena.get(handle, 1), Value::Number(2.0));
        assert_eq!(arena.get(handle, 2), Value::Number(3.0));
    }

    #[test]
    fn test_insert_out_of_bounds() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(2);

        assert!(arena.insert(handle, 0, Value::Number(1.0)));
        assert!(!arena.insert(handle, 2, Value::Number(3.0))); // Out of bounds
        assert!(!arena.insert(handle, 10, Value::Number(10.0))); // Way out of bounds
    }

    #[test]
    fn test_push_and_pop() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        // Push some values
        arena.push(handle, Value::Number(1.0));
        arena.push(handle, Value::Number(2.0));
        arena.push(handle, Value::Number(3.0));

        assert_eq!(arena.length(handle), 3);
        assert_eq!(arena.get(handle, 0), Value::Number(1.0));
        assert_eq!(arena.get(handle, 1), Value::Number(2.0));
        assert_eq!(arena.get(handle, 2), Value::Number(3.0));

        // Pop values
        assert_eq!(arena.pop(handle), Value::Number(3.0));
        assert_eq!(arena.pop(handle), Value::Number(2.0));
        assert_eq!(arena.length(handle), 1);

        assert_eq!(arena.pop(handle), Value::Number(1.0));
        assert_eq!(arena.length(handle), 0);

        // Pop from empty array
        assert_eq!(arena.pop(handle), Value::Nil);
    }

    #[test]
    fn test_cross_chunk_operations() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        // Push more than one chunk worth of data
        for i in 0..50 {
            arena.push(handle, Value::Number(i as f64));
        }

        assert_eq!(arena.length(handle), 50);

        // Verify values across chunks
        for i in 0..50 {
            assert_eq!(arena.get(handle, i), Value::Number(i as f64));
        }

        // Pop from multiple chunks
        for i in (0..50).rev() {
            assert_eq!(arena.pop(handle), Value::Number(i as f64));
        }

        assert_eq!(arena.length(handle), 0);
    }

    #[test]
    fn test_reverse() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(5);

        // Insert values
        for i in 0..5 {
            arena.insert(handle, i, Value::Number(i as f64));
        }

        arena.reverse(handle);

        // Check reversed values
        for i in 0..5 {
            assert_eq!(arena.get(handle, i), Value::Number((4 - i) as f64));
        }
    }

    #[test]
    fn test_reverse_large_array() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        // Push values across multiple chunks
        for i in 0..70 {
            arena.push(handle, Value::Number(i as f64));
        }

        arena.reverse(handle);

        // Check reversed values
        for i in 0..70 {
            assert_eq!(arena.get(handle, i), Value::Number((69 - i) as f64));
        }
    }

    #[test]
    fn test_concat() {
        let mut arena = ArrayArena::new();

        let handle1 = arena.create_array(3);
        let handle2 = arena.create_array(2);

        // Fill arrays
        arena.insert(handle1, 0, Value::Number(1.0));
        arena.insert(handle1, 1, Value::Number(2.0));
        arena.insert(handle1, 2, Value::Number(3.0));

        arena.insert(handle2, 0, Value::Number(4.0));
        arena.insert(handle2, 1, Value::Number(5.0));

        let concat_handle = arena.concat(handle1, handle2);

        assert_eq!(arena.length(concat_handle), 5);
        assert_eq!(arena.get(concat_handle, 0), Value::Number(1.0));
        assert_eq!(arena.get(concat_handle, 1), Value::Number(2.0));
        assert_eq!(arena.get(concat_handle, 2), Value::Number(3.0));
        assert_eq!(arena.get(concat_handle, 3), Value::Number(4.0));
        assert_eq!(arena.get(concat_handle, 4), Value::Number(5.0));
    }

    #[test]
    fn test_slice() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(10);

        // Fill array
        for i in 0..10 {
            arena.insert(handle, i, Value::Number(i as f64));
        }

        let slice_handle = arena.slice(handle, 2, 7);

        assert_eq!(arena.length(slice_handle), 5);
        for i in 0..5 {
            assert_eq!(arena.get(slice_handle, i), Value::Number((i + 2) as f64));
        }
    }

    #[test]
    fn test_slice_bounds_clamping() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(5);

        for i in 0..5 {
            arena.insert(handle, i, Value::Number(i as f64));
        }

        // Test various boundary conditions
        let slice1 = arena.slice(handle, 10, 20); // Beyond bounds
        assert_eq!(arena.length(slice1), 0);

        let slice2 = arena.slice(handle, 2, 10); // End beyond bounds
        assert_eq!(arena.length(slice2), 3);
        assert_eq!(arena.get(slice2, 0), Value::Number(2.0));

        let slice3 = arena.slice(handle, 3, 2); // Start > end
        assert_eq!(arena.length(slice3), 0);
    }
}
