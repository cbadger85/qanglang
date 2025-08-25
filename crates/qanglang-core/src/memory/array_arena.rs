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

#[derive(Debug, Default, Clone)]
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

    pub fn get(&self, handle: ArrayHandle, index: isize) -> Value {
        let header = &self.heads[handle];
        let length = header.length as isize;

        // Handle negative indices (from the end)
        let actual_index = if index < 0 {
            length + index
        } else {
            index
        };

        // Check bounds - negative index too large or positive index out of bounds
        if actual_index < 0 || actual_index >= length {
            return Value::Nil;
        }

        let actual_index = actual_index as usize;
        let chunk_index = actual_index / CHUNK_SIZE;
        let slot_index = actual_index % CHUNK_SIZE;

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

    pub fn pop(&mut self, handle: ArrayHandle) -> Option<Value> {
        let length = self.heads[handle].length;

        if length == 0 {
            return None;
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
                return None;
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

            // Keep empty chunks attached for reuse - no longer freeing them
            self.heads[handle].length -= 1;
            Some(value)
        } else {
            None
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

        let new_handle = self.create_array(total_length);

        for i in 0..len1 {
            let value = self.get(handle1, i as isize);
            self.insert(new_handle, i, value);
        }

        for i in 0..len2 {
            let value = self.get(handle2, i as isize);
            self.insert(new_handle, len1 + i, value);
        }

        new_handle
    }

    pub fn slice(&mut self, handle: ArrayHandle, begin: isize, end: Option<isize>) -> ArrayHandle {
        let source_length = self.heads[handle].length as isize;

        // Handle negative indices for begin
        let start = if begin < 0 {
            (source_length + begin).max(0) as usize
        } else {
            (begin.min(source_length) as usize).min(source_length as usize)
        };

        // Handle negative indices for end
        let finish = if let Some(end_val) = end {
            if end_val < 0 {
                (source_length + end_val).max(0) as usize
            } else {
                (end_val.min(source_length) as usize).min(source_length as usize)
            }
        } else {
            source_length as usize
        };

        // Safeguard: if begin > end, return empty array
        if start >= finish {
            return self.create_array(0);
        }

        let slice_length = finish - start;

        let new_handle = self.create_array(slice_length);

        for i in 0..slice_length {
            let value = self.get(handle, (start + i) as isize);
            self.insert(new_handle, i, value);
        }

        new_handle
    }

    pub fn iter(&self, handle: ArrayHandle) -> ArrayIterator<'_> {
        ArrayIterator {
            arena: self,
            handle,
            current_index: 0,
        }
    }

    pub fn mark_array(&mut self, handle: ArrayHandle) {
        self.heads[handle].is_marked = true;

        let mut current_chunk = self.heads[handle].first_chunk;

        while let Some(chunk_handle) = current_chunk {
            self.chunks[chunk_handle].is_marked = true;
            current_chunk = self.chunks[chunk_handle].next_chunk;
        }
    }

    pub fn check_is_marked(&self, handle: ArrayHandle) -> bool {
        self.heads[handle].is_marked
    }

    pub fn collect_garbage(&mut self) {
        let estimated_deletions = (self.chunks.len() / 4).max(8);
        let mut handles_to_remove = Vec::with_capacity(estimated_deletions);

        for (handle, head) in self.heads.iter_mut() {
            if head.is_marked {
                head.is_marked = false;
            } else {
                handles_to_remove.push(handle);
            }
        }

        for handle in handles_to_remove.drain(..) {
            let _ = self.heads.remove(handle);
        }

        for (handle, chunk) in self.chunks.iter_mut() {
            if chunk.is_marked {
                chunk.is_marked = false;
            } else {
                handles_to_remove.push(handle);
            }
        }

        for handle in handles_to_remove.drain(..) {
            let _ = self.chunks.remove(handle);
        }
    }

    pub fn get_allocated_bytes(&self) -> usize {
        std::mem::size_of::<ArrayHeader>() * self.heads.len()
            + std::mem::size_of::<ArrayChunk>() * self.chunks.len()
    }
}

pub struct ArrayIterator<'a> {
    arena: &'a ArrayArena,
    handle: ArrayHandle,
    current_index: usize,
}

impl<'a> Iterator for ArrayIterator<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let length = self.arena.length(self.handle);

        if self.current_index >= length {
            return None;
        }

        let value = self.arena.get(self.handle, self.current_index as isize);
        self.current_index += 1;

        // Skip Nil values (empty slots)
        if matches!(value, Value::Nil) {
            self.next()
        } else {
            Some(value)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let length = self.arena.length(self.handle);
        let remaining = length.saturating_sub(self.current_index);
        (0, Some(remaining)) // Lower bound is 0 because we might have Nil values
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
        assert_eq!(arena.pop(handle), Some(Value::Number(3.0)));
        assert_eq!(arena.pop(handle), Some(Value::Number(2.0)));
        assert_eq!(arena.length(handle), 1);

        assert_eq!(arena.pop(handle), Some(Value::Number(1.0)));
        assert_eq!(arena.length(handle), 0);

        // Pop from empty array
        assert_eq!(arena.pop(handle), None);
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
            assert_eq!(arena.pop(handle), Some(Value::Number(i as f64)));
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

        let slice_handle = arena.slice(handle, 2, Some(7));

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
        let slice1 = arena.slice(handle, 10, Some(20)); // Beyond bounds
        assert_eq!(arena.length(slice1), 0);

        let slice2 = arena.slice(handle, 2, Some(10)); // End beyond bounds
        assert_eq!(arena.length(slice2), 3);
        assert_eq!(arena.get(slice2, 0), Value::Number(2.0));

        let slice3 = arena.slice(handle, 3, Some(2)); // Start > end
        assert_eq!(arena.length(slice3), 0);
    }

    #[test]
    fn test_slice_with_optional_end() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(8);

        // Fill array
        for i in 0..8 {
            arena.insert(handle, i, Value::Number(i as f64));
        }

        // Test slice from index 3 to end
        let slice_handle = arena.slice(handle, 3, None);

        assert_eq!(arena.length(slice_handle), 5);
        for i in 0..5 {
            assert_eq!(arena.get(slice_handle, i), Value::Number((i + 3) as f64));
        }

        // Test slice from index 0 to end (full copy)
        let full_slice = arena.slice(handle, 0, None);
        assert_eq!(arena.length(full_slice), 8);
        for i in 0..8 {
            assert_eq!(arena.get(full_slice, i), Value::Number(i as f64));
        }

        // Test slice from beyond bounds to end
        let empty_slice = arena.slice(handle, 10, None);
        assert_eq!(arena.length(empty_slice), 0);
    }

    #[test]
    fn test_negative_indexing() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(5);

        // Fill array with values [0, 1, 2, 3, 4]
        for i in 0..5 {
            arena.insert(handle, i, Value::Number(i as f64));
        }

        // Test negative indices
        assert_eq!(arena.get(handle, -1), Value::Number(4.0)); // Last element
        assert_eq!(arena.get(handle, -2), Value::Number(3.0)); // Second to last
        assert_eq!(arena.get(handle, -3), Value::Number(2.0)); // Third to last
        assert_eq!(arena.get(handle, -4), Value::Number(1.0)); // Fourth to last
        assert_eq!(arena.get(handle, -5), Value::Number(0.0)); // First element

        // Test out of bounds negative indices
        assert_eq!(arena.get(handle, -6), Value::Nil); // Beyond bounds
        assert_eq!(arena.get(handle, -10), Value::Nil); // Way beyond bounds

        // Test positive indices still work
        assert_eq!(arena.get(handle, 0), Value::Number(0.0));
        assert_eq!(arena.get(handle, 4), Value::Number(4.0));
        assert_eq!(arena.get(handle, 5), Value::Nil); // Out of bounds positive

        // Test empty array
        let empty_handle = arena.create_array(0);
        assert_eq!(arena.get(empty_handle, -1), Value::Nil);
        assert_eq!(arena.get(empty_handle, 0), Value::Nil);
    }

    #[test]
    fn test_iterator_empty_array() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        let mut iter = arena.iter(handle);
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_iterator_basic() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(3);

        arena.insert(handle, 0, Value::Number(1.0));
        arena.insert(handle, 1, Value::Number(2.0));
        arena.insert(handle, 2, Value::Number(3.0));

        let values: Vec<Value> = arena.iter(handle).collect();
        assert_eq!(values.len(), 3);
        assert_eq!(values[0], Value::Number(1.0));
        assert_eq!(values[1], Value::Number(2.0));
        assert_eq!(values[2], Value::Number(3.0));
    }

    #[test]
    fn test_iterator_with_empty_slots() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(5);

        // Only fill some slots
        arena.insert(handle, 0, Value::Number(1.0));
        arena.insert(handle, 2, Value::Number(3.0));
        arena.insert(handle, 4, Value::Number(5.0));

        let values: Vec<Value> = arena.iter(handle).collect();
        assert_eq!(values.len(), 3);
        assert_eq!(values[0], Value::Number(1.0));
        assert_eq!(values[1], Value::Number(3.0));
        assert_eq!(values[2], Value::Number(5.0));
    }

    #[test]
    fn test_iterator_cross_chunks() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        // Push values across multiple chunks
        for i in 0..50 {
            arena.push(handle, Value::Number(i as f64));
        }

        let values: Vec<Value> = arena.iter(handle).collect();
        assert_eq!(values.len(), 50);

        for (i, value) in values.iter().enumerate() {
            assert_eq!(*value, Value::Number(i as f64));
        }
    }

    #[test]
    fn test_iterator_for_loop() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        for i in 0..10 {
            arena.push(handle, Value::Number(i as f64));
        }

        let mut expected = 0.0;
        for value in arena.iter(handle) {
            assert_eq!(value, Value::Number(expected));
            expected += 1.0;
        }

        assert_eq!(expected, 10.0);
    }

    #[test]
    fn test_iterator_size_hint() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(5);

        arena.insert(handle, 0, Value::Number(1.0));
        arena.insert(handle, 1, Value::Number(2.0));
        arena.insert(handle, 4, Value::Number(5.0));

        let iter = arena.iter(handle);
        let (lower, upper) = iter.size_hint();

        assert_eq!(lower, 0); // Could have all Nil values
        assert_eq!(upper, Some(5)); // At most 5 elements
    }

    #[test]
    fn test_iterator_after_operations() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        // Push, pop, and push again
        arena.push(handle, Value::Number(1.0));
        arena.push(handle, Value::Number(2.0));
        arena.push(handle, Value::Number(3.0));

        arena.pop(handle);
        arena.push(handle, Value::Number(4.0));

        let values: Vec<Value> = arena.iter(handle).collect();
        assert_eq!(values.len(), 3);
        assert_eq!(values[0], Value::Number(1.0));
        assert_eq!(values[1], Value::Number(2.0));
        assert_eq!(values[2], Value::Number(4.0));
    }

    #[test]
    fn test_mark_array() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        // Push values across multiple chunks
        for i in 0..40 {
            arena.push(handle, Value::Number(i as f64));
        }

        // Initially nothing should be marked
        assert!(!arena.heads[handle].is_marked);

        // Mark the array
        arena.mark_array(handle);

        // Array header should be marked
        assert!(arena.heads[handle].is_marked);

        // All chunks should be marked
        let mut current_chunk = arena.heads[handle].first_chunk;
        while let Some(chunk_handle) = current_chunk {
            assert!(arena.chunks[chunk_handle].is_marked);
            current_chunk = arena.chunks[chunk_handle].next_chunk;
        }
    }

    #[test]
    fn test_collect_garbage_marked_arrays() {
        let mut arena = ArrayArena::new();
        let handle1 = arena.create_array(5);
        let handle2 = arena.create_array(3);

        // Fill arrays
        for i in 0..5 {
            arena.insert(handle1, i, Value::Number(i as f64));
        }
        for i in 0..3 {
            arena.insert(handle2, i, Value::Number((i + 10) as f64));
        }

        // Mark only the first array
        arena.mark_array(handle1);

        // Collect garbage
        arena.collect_garbage();

        // First array should still exist and be accessible
        assert_eq!(arena.length(handle1), 5);
        assert_eq!(arena.get(handle1, 0), Value::Number(0.0));

        // Array should be unmarked after GC
        assert!(!arena.heads[handle1].is_marked);

        // Second array should be gone (this will panic if accessed, but we won't access it)
        // We can't easily test this without exposing Arena internals
    }

    #[test]
    fn test_collect_garbage_unmarked_arrays() {
        let mut arena = ArrayArena::new();
        let handle1 = arena.create_array(3);
        let handle2 = arena.create_array(3);

        // Fill arrays
        arena.insert(handle1, 0, Value::Number(1.0));
        arena.insert(handle2, 0, Value::Number(2.0));

        // Don't mark anything - simulate no roots
        arena.collect_garbage();

        // Both arrays should be collected (we can't easily test this without Arena internals)
        // The main thing is that collect_garbage doesn't panic
    }

    #[test]
    fn test_mark_and_sweep_cycle() {
        let mut arena = ArrayArena::new();
        let handle1 = arena.create_array(0);
        let handle2 = arena.create_array(0);

        // Create arrays with multiple chunks
        for i in 0..35 {
            arena.push(handle1, Value::Number(i as f64));
            arena.push(handle2, Value::Number((i + 100) as f64));
        }

        // Mark only handle1
        arena.mark_array(handle1);

        // Verify marking worked
        assert!(arena.heads[handle1].is_marked);
        assert!(!arena.heads[handle2].is_marked);

        // First GC cycle
        arena.collect_garbage();

        // handle1 should be unmarked now but still exist
        assert!(!arena.heads[handle1].is_marked);
        assert_eq!(arena.length(handle1), 35);

        // Mark handle1 again for next cycle
        arena.mark_array(handle1);

        // Second GC cycle
        arena.collect_garbage();

        // Should still be accessible
        assert_eq!(arena.get(handle1, 0), Value::Number(0.0));
        assert_eq!(arena.get(handle1, 34), Value::Number(34.0));
    }

    #[test]
    fn test_gc_with_empty_arena() {
        let mut arena = ArrayArena::new();

        // Should not panic on empty arena
        arena.collect_garbage();
    }

    #[test]
    fn test_gc_preserves_chunks_of_marked_arrays() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        // Create array spanning multiple chunks
        for i in 0..50 {
            arena.push(handle, Value::Number(i as f64));
        }

        // Mark the array
        arena.mark_array(handle);

        // Collect garbage
        arena.collect_garbage();

        // All data should still be accessible
        for i in 0..50 {
            assert_eq!(arena.get(handle, i), Value::Number(i as f64));
        }

        // Iterator should still work
        let values: Vec<Value> = arena.iter(handle).collect();
        assert_eq!(values.len(), 50);
    }

    #[test]
    fn test_slice_with_negative_indices() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(10);

        // Fill array with values [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        for i in 0..10 {
            arena.insert(handle, i, Value::Number(i as f64));
        }

        // Test negative start index
        let slice1 = arena.slice(handle, -3, None); // Last 3 elements [7, 8, 9]
        assert_eq!(arena.length(slice1), 3);
        assert_eq!(arena.get(slice1, 0), Value::Number(7.0));
        assert_eq!(arena.get(slice1, 1), Value::Number(8.0));
        assert_eq!(arena.get(slice1, 2), Value::Number(9.0));

        // Test negative end index
        let slice2 = arena.slice(handle, 2, Some(-2)); // Elements from index 2 to -2 (exclusive) [2, 3, 4, 5, 6, 7]
        assert_eq!(arena.length(slice2), 6);
        assert_eq!(arena.get(slice2, 0), Value::Number(2.0));
        assert_eq!(arena.get(slice2, 5), Value::Number(7.0));

        // Test both negative indices
        let slice3 = arena.slice(handle, -5, Some(-2)); // Elements from -5 to -2 [5, 6, 7]
        assert_eq!(arena.length(slice3), 3);
        assert_eq!(arena.get(slice3, 0), Value::Number(5.0));
        assert_eq!(arena.get(slice3, 1), Value::Number(6.0));
        assert_eq!(arena.get(slice3, 2), Value::Number(7.0));

        // Test negative index beyond bounds
        let slice4 = arena.slice(handle, -15, Some(5)); // Should clamp to start
        assert_eq!(arena.length(slice4), 5);
        assert_eq!(arena.get(slice4, 0), Value::Number(0.0));
        assert_eq!(arena.get(slice4, 4), Value::Number(4.0));

        // Test negative end index beyond bounds
        let slice5 = arena.slice(handle, 3, Some(-15)); // Should return empty array
        assert_eq!(arena.length(slice5), 0);
    }

    #[test]
    fn test_slice_negative_begin_greater_than_end() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(10);

        // Fill array
        for i in 0..10 {
            arena.insert(handle, i, Value::Number(i as f64));
        }

        // Test where negative begin resolves to index greater than end
        let slice1 = arena.slice(handle, -2, Some(3)); // -2 becomes 8, 8 > 3, should return empty
        assert_eq!(arena.length(slice1), 0);

        // Test where both are negative but begin > end after resolution
        let slice2 = arena.slice(handle, -2, Some(-5)); // -2 becomes 8, -5 becomes 5, 8 > 5, should return empty
        assert_eq!(arena.length(slice2), 0);

        // Test positive begin greater than negative end after resolution
        let slice3 = arena.slice(handle, 7, Some(-4)); // end becomes 6, 7 > 6, should return empty
        assert_eq!(arena.length(slice3), 0);
    }

    #[test]
    fn test_slice_edge_cases_with_negative_indices() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(5);

        // Fill array with [0, 1, 2, 3, 4]
        for i in 0..5 {
            arena.insert(handle, i, Value::Number(i as f64));
        }

        // Test -1 to end (last element)
        let slice1 = arena.slice(handle, -1, None);
        assert_eq!(arena.length(slice1), 1);
        assert_eq!(arena.get(slice1, 0), Value::Number(4.0));

        // Test start to -1 (all but last element)
        let slice2 = arena.slice(handle, 0, Some(-1));
        assert_eq!(arena.length(slice2), 4);
        assert_eq!(arena.get(slice2, 0), Value::Number(0.0));
        assert_eq!(arena.get(slice2, 3), Value::Number(3.0));

        // Test entire array with negative indices
        let slice3 = arena.slice(handle, -5, None); // Should be equivalent to 0 to end
        assert_eq!(arena.length(slice3), 5);
        for i in 0..5 {
            assert_eq!(arena.get(slice3, i), Value::Number(i as f64));
        }

        // Test single element slice with negative indices
        let slice4 = arena.slice(handle, -3, Some(-2)); // Element at index 2
        assert_eq!(arena.length(slice4), 1);
        assert_eq!(arena.get(slice4, 0), Value::Number(2.0));
    }

    #[test]
    fn test_slice_empty_array_with_negative_indices() {
        let mut arena = ArrayArena::new();
        let handle = arena.create_array(0);

        // Test negative indices on empty array
        let slice1 = arena.slice(handle, -1, None);
        assert_eq!(arena.length(slice1), 0);

        let slice2 = arena.slice(handle, -5, Some(-2));
        assert_eq!(arena.length(slice2), 0);

        let slice3 = arena.slice(handle, 0, Some(-1));
        assert_eq!(arena.length(slice3), 0);
    }
}
