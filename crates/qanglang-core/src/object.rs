use crate::{
    chunk::Chunk,
    memory::{FunctionHandle, StringHandle, ValueHandle},
};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ClosureObject {
    pub function: FunctionHandle,
    pub upvalue_count: usize,
    pub upvalues: Vec<UpvalueReference>,
}

impl ClosureObject {
    pub fn new(function: FunctionHandle, upvalue_count: usize) -> Self {
        let upvalues = vec![UpvalueReference::Open(0); upvalue_count];
        Self {
            function,
            upvalues,
            upvalue_count,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum UpvalueReference {
    Open(usize),
    Closed(ValueHandle),
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct FunctionObject {
    pub arity: usize,
    pub name: StringHandle,
    pub chunk: Chunk,
    pub upvalue_count: usize,
}

impl FunctionObject {
    pub fn new(name: StringHandle, arity: usize) -> Self {
        Self {
            name,
            arity,
            chunk: Chunk::new(),
            upvalue_count: 0,
        }
    }
}

#[allow(dead_code)]
mod future_implementations {
    use crate::value::Value;
    use generational_arena::{Arena, Index};

    pub struct HashMapObject {
        buckets: Arena<Bucket>,
        bucket_indices: Vec<generational_arena::Index>,
        len: usize,
        capacity: usize,
    }

    #[derive(Clone, Copy)]
    enum Bucket {
        Empty,
        Occupied { key: Value, value: Value },
        Tombstone,
    }

    impl Default for Bucket {
        fn default() -> Self {
            Bucket::Empty
        }
    }

    impl HashMapObject {
        const LOAD_FACTOR_THRESHOLD: f64 = 0.75;

        pub fn with_capacity(capacity: usize) -> Self {
            let mut buckets = Arena::with_capacity(capacity);
            let mut bucket_indices = Vec::with_capacity(capacity);

            for _ in 0..capacity {
                let index = buckets.insert(Bucket::Empty);
                bucket_indices.push(index);
            }

            Self {
                buckets,
                bucket_indices,
                len: 0,
                capacity,
            }
        }

        pub fn new() -> Self {
            Self::with_capacity(16)
        }

        pub fn len(&self) -> usize {
            self.len
        }

        pub fn is_empty(&self) -> bool {
            self.len == 0
        }

        fn find_slot(&self, key: &Value) -> (usize, bool) {
            let hash = key.hash();
            let mut index = (hash as usize) % self.capacity;

            loop {
                let bucket = &self.buckets[self.bucket_indices[index]];
                match bucket {
                    Bucket::Empty => return (index, false),
                    Bucket::Tombstone => {
                        // Continue probing, but remember this slot for insertion
                    }
                    Bucket::Occupied {
                        key: existing_key, ..
                    } => {
                        if existing_key == key {
                            return (index, true);
                        }
                    }
                }
                index = (index + 1) % self.capacity;
            }
        }

        fn find_insert_slot(&self, key: &Value) -> usize {
            let hash = key.hash();
            let mut index = (hash as usize) % self.capacity;
            let mut tombstone_index = None;

            loop {
                let bucket = &self.buckets[self.bucket_indices[index]];
                match bucket {
                    Bucket::Empty => return tombstone_index.unwrap_or(index),
                    Bucket::Tombstone => {
                        if tombstone_index.is_none() {
                            tombstone_index = Some(index);
                        }
                    }
                    Bucket::Occupied {
                        key: existing_key, ..
                    } => {
                        if existing_key == key {
                            return index;
                        }
                    }
                }
                index = (index + 1) % self.capacity;
            }
        }

        pub fn get(&self, key: &Value) -> Option<&Value> {
            let (index, found) = self.find_slot(key);
            if found {
                if let Bucket::Occupied { value, .. } = &self.buckets[self.bucket_indices[index]] {
                    Some(value)
                } else {
                    None
                }
            } else {
                None
            }
        }

        pub fn insert(&mut self, key: Value, value: Value) -> Option<Value> {
            if self.len as f64 / self.capacity as f64 > Self::LOAD_FACTOR_THRESHOLD {
                self.resize();
            }

            let index = self.find_insert_slot(&key);
            let bucket_index = self.bucket_indices[index];
            let old_bucket = self.buckets[bucket_index];

            let old_value = match old_bucket {
                Bucket::Occupied {
                    key: existing_key,
                    value: old_value,
                } if existing_key == key => Some(old_value),
                _ => {
                    self.len += 1;
                    None
                }
            };

            self.buckets[bucket_index] = Bucket::Occupied { key, value };
            old_value
        }

        pub fn remove(&mut self, key: &Value) -> Option<Value> {
            let (index, found) = self.find_slot(key);
            if found {
                let bucket_index = self.bucket_indices[index];
                if let Bucket::Occupied { value, .. } = self.buckets[bucket_index] {
                    self.buckets[bucket_index] = Bucket::Tombstone;
                    self.len -= 1;
                    Some(value)
                } else {
                    None
                }
            } else {
                None
            }
        }

        fn resize(&mut self) {
            let old_capacity = self.capacity;
            let old_bucket_indices = std::mem::take(&mut self.bucket_indices);

            self.capacity = old_capacity * 2;
            self.bucket_indices = Vec::with_capacity(self.capacity);

            // Create new empty buckets
            for _ in 0..self.capacity {
                let index = self.buckets.insert(Bucket::Empty);
                self.bucket_indices.push(index);
            }

            let old_len = self.len;
            self.len = 0;

            // Rehash all existing entries
            for old_index in old_bucket_indices {
                if let Bucket::Occupied { key, value } = self.buckets[old_index] {
                    self.insert(key, value);
                }
                // Free the old bucket
                self.buckets.remove(old_index);
            }

            debug_assert_eq!(self.len, old_len);
        }
    }

    impl Default for HashMapObject {
        fn default() -> Self {
            Self::new()
        }
    }

    // Idea for array implementation
    const CHUNK_SIZE: usize = 32;

    // This becomes a Value.
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct ArrayHeader {
        first_chunk: Option<Index>,
        length: usize,
        chunks_count: usize,
    }

    impl ArrayHeader {
        pub fn new() -> Self {
            Self {
                first_chunk: None,
                length: 0,
                chunks_count: 0,
            }
        }

        pub fn with_first_chunk(first_chunk: Index) -> Self {
            Self {
                first_chunk: Some(first_chunk),
                length: 0,
                chunks_count: 1,
            }
        }

        pub fn len(&self) -> usize {
            self.length
        }

        pub fn is_empty(&self) -> bool {
            self.length == 0
        }

        pub fn chunks_count(&self) -> usize {
            self.chunks_count
        }

        pub fn first_chunk(&self) -> Option<Index> {
            self.first_chunk
        }

        pub fn find_chunk_for_index(&self, index: usize) -> Option<(Index, usize)> {
            if index >= self.length {
                return None;
            }

            let chunk_index = index / CHUNK_SIZE;

            if chunk_index >= self.chunks_count {
                return None;
            }

            // Return the first chunk and number of jumps needed
            self.first_chunk.map(|first| (first, chunk_index))
        }

        pub fn chunk_and_offset_for_index(&self, index: usize) -> Option<(usize, usize)> {
            if index >= self.length {
                return None;
            }

            let chunk_index = index / CHUNK_SIZE;
            let offset_in_chunk = index % CHUNK_SIZE;

            Some((chunk_index, offset_in_chunk))
        }

        pub fn last_chunk_info(&self) -> Option<(usize, usize)> {
            if self.chunks_count == 0 {
                return None;
            }

            let last_chunk_index = self.chunks_count - 1;
            let elements_in_last_chunk = if self.length == 0 {
                0
            } else {
                ((self.length - 1) % CHUNK_SIZE) + 1
            };

            Some((last_chunk_index, elements_in_last_chunk))
        }

        pub fn needs_new_chunk(&self, elements_to_add: usize) -> bool {
            if self.chunks_count == 0 {
                return elements_to_add > 0;
            }

            let (_, elements_in_last_chunk) = self.last_chunk_info().unwrap_or((0, 0));
            let available_in_last_chunk = CHUNK_SIZE - elements_in_last_chunk;

            elements_to_add > available_in_last_chunk
        }

        pub fn capacity(&self) -> usize {
            self.chunks_count * CHUNK_SIZE
        }

        pub fn available_space(&self) -> usize {
            self.capacity().saturating_sub(self.length)
        }

        pub fn increment_length(&mut self) {
            self.length += 1;
        }

        pub fn decrement_length(&mut self) {
            if self.length > 0 {
                self.length -= 1;
            }
        }

        pub fn add_chunk(&mut self, chunk_index: Index) {
            if self.first_chunk.is_none() {
                self.first_chunk = Some(chunk_index);
            }
            self.chunks_count += 1;
        }

        pub fn remove_chunk(&mut self) {
            if self.chunks_count > 0 {
                self.chunks_count -= 1;
                if self.chunks_count == 0 {
                    self.first_chunk = None;
                }
            }
        }
    }

    // This get's an arena allocator for it.
    #[derive(Debug, Clone, PartialEq)]
    pub struct ArrayChunk {
        data: [Option<Value>; CHUNK_SIZE],
        next_chunk: Option<Index>,
        used: usize, // how many slots are actually used
    }

    impl ArrayChunk {
        pub fn new() -> Self {
            Self {
                data: [None; CHUNK_SIZE],
                next_chunk: None,
                used: 0,
            }
        }

        pub fn with_next_chunk(next_chunk: Index) -> Self {
            Self {
                data: [None; CHUNK_SIZE],
                next_chunk: Some(next_chunk),
                used: 0,
            }
        }

        pub fn get(&self, index: usize) -> Option<&Value> {
            if index < CHUNK_SIZE {
                self.data[index].as_ref()
            } else {
                None
            }
        }

        pub fn get_mut(&mut self, index: usize) -> Option<&mut Value> {
            if index < CHUNK_SIZE {
                self.data[index].as_mut()
            } else {
                None
            }
        }

        pub fn set(&mut self, index: usize, value: Value) -> bool {
            if index < CHUNK_SIZE {
                let was_empty = self.data[index].is_none();
                self.data[index] = Some(value);
                if was_empty {
                    self.used += 1;
                }
                true
            } else {
                false
            }
        }

        pub fn remove(&mut self, index: usize) -> Option<Value> {
            if index < CHUNK_SIZE {
                let old_value = self.data[index].take();
                if old_value.is_some() {
                    self.used -= 1;
                }
                old_value
            } else {
                None
            }
        }

        pub fn push(&mut self, value: Value) -> bool {
            if self.used < CHUNK_SIZE {
                for i in 0..CHUNK_SIZE {
                    if self.data[i].is_none() {
                        self.data[i] = Some(value);
                        self.used += 1;
                        return true;
                    }
                }
            }
            false
        }

        pub fn is_full(&self) -> bool {
            self.used >= CHUNK_SIZE
        }

        pub fn is_empty(&self) -> bool {
            self.used == 0
        }

        pub fn used(&self) -> usize {
            self.used
        }

        pub fn available(&self) -> usize {
            CHUNK_SIZE - self.used
        }

        pub fn next_chunk(&self) -> Option<Index> {
            self.next_chunk
        }

        pub fn set_next_chunk(&mut self, next_chunk: Option<Index>) {
            self.next_chunk = next_chunk;
        }

        pub fn clear(&mut self) {
            self.data = [None; CHUNK_SIZE];
            self.used = 0;
        }
    }
}
