use std::collections::HashMap;

use generational_arena::{Arena, Index};

use crate::{ClosureObject, FunctionObject, FunctionValueKind, Value, ValueConversionError};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Default)]
pub struct StringHandle(usize);

impl TryFrom<Value> for StringHandle {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(handle) => Ok(handle),
            _ => Err(ValueConversionError::new("Expected string.")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub struct ClosureHandle(Index);

impl Default for ClosureHandle {
    fn default() -> Self {
        ClosureHandle(Index::from_raw_parts(0, 0))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub struct ValueHandle(Index);

impl Default for ValueHandle {
    fn default() -> Self {
        ValueHandle(Index::from_raw_parts(0, 0))
    }
}

impl TryFrom<Value> for ClosureHandle {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Function(kind) => match kind {
                FunctionValueKind::Closure(handle) => Ok(handle),
                _ => Err(ValueConversionError::new("Expected function.")),
            },
            _ => Err(ValueConversionError::new("Expected function.")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Default)]
pub struct FunctionHandle(usize);

impl TryFrom<Value> for FunctionHandle {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::FunctionDecl(handle) => Ok(handle),
            _ => Err(ValueConversionError::new("Expected function.")),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ObjectHeap {
    strings: Vec<String>,
    functions: Vec<FunctionObject>,
    closures: Arena<ClosureObject>,
    values: Arena<Value>,
    string_interner: HashMap<String, StringHandle>,
}

impl ObjectHeap {
    pub fn new() -> Self {
        Self::with_capacity(64)
    }

    pub fn with_capacity(initial_capacity: usize) -> Self {
        Self {
            string_interner: HashMap::new(),
            functions: Vec::with_capacity(initial_capacity),
            closures: Arena::with_capacity(initial_capacity),
            strings: Vec::with_capacity(initial_capacity),
            values: Arena::with_capacity(initial_capacity),
        }
    }

    pub fn intern_string_slice(&mut self, s: &str) -> StringHandle {
        if self.string_interner.contains_key(s) {
            self.string_interner[s]
        } else {
            if self.strings.len() == self.strings.capacity() {
                let new_capacity = if self.strings.capacity() == 0 {
                    64
                } else {
                    self.strings.capacity() * 2
                };
                self.strings.reserve(new_capacity - self.strings.capacity());
            }
            self.strings.push(s.to_string());
            let handle = StringHandle(self.strings.len() - 1);

            self.string_interner.insert(s.to_string(), handle);

            handle
        }
    }

    pub fn intern_string(&mut self, s: String) -> StringHandle {
        if self.string_interner.contains_key(&s) {
            self.string_interner[&s]
        } else {
            if self.strings.len() == self.strings.capacity() {
                let new_capacity = if self.strings.capacity() == 0 {
                    64
                } else {
                    self.strings.capacity() * 2
                };
                self.strings.reserve(new_capacity - self.strings.capacity());
            }
            self.strings.push(s.clone());
            let handle = StringHandle(self.strings.len() - 1);

            self.string_interner.insert(s, handle);

            handle
        }
    }

    pub fn get_string(&self, handle: StringHandle) -> &str {
        &self.strings[handle.0]
    }

    pub fn can_allocate_closure(&self) -> bool {
        self.closures.len() < self.closures.capacity()
    }

    pub fn allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        let index = self.closures.insert(closure);
        ClosureHandle(index)
    }

    pub fn force_allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        if self.closures.len() == self.closures.capacity() {
            let new_capacity = if self.closures.capacity() == 0 {
                64
            } else {
                self.closures.capacity() * 2
            };

            self.closures
                .reserve(new_capacity - self.closures.capacity());
        }
        let index = self.closures.insert(closure);
        ClosureHandle(index)
    }

    pub fn get_closure(&self, handle: ClosureHandle) -> &ClosureObject {
        &self.closures[handle.0]
    }

    pub fn get_closure_mut(&mut self, handle: ClosureHandle) -> &mut ClosureObject {
        &mut self.closures[handle.0]
    }

    pub fn free_closure(&mut self, handle: ClosureHandle) {
        self.closures.remove(handle.0);
    }

    pub fn can_allocate_value(&self) -> bool {
        self.values.len() < self.values.capacity()
    }

    pub fn allocate_value(&mut self, value: Value) -> ValueHandle {
        let index = self.values.insert(value);
        ValueHandle(index)
    }

    pub fn force_allocate_value(&mut self, value: Value) -> ValueHandle {
        if self.values.len() == self.values.capacity() {
            let new_capacity = if self.values.capacity() == 0 {
                64
            } else {
                self.values.capacity() * 2
            };

            self.values.reserve(new_capacity - self.values.capacity());
        }
        let index = self.values.insert(value);
        ValueHandle(index)
    }

    pub fn get_value(&self, handle: ValueHandle) -> &Value {
        &self.values[handle.0]
    }

    pub fn get_value_mut(&mut self, handle: ValueHandle) -> &mut Value {
        &mut self.values[handle.0]
    }

    pub fn free_value(&mut self, handle: ValueHandle) {
        self.values.remove(handle.0);
    }

    pub fn allocate_function(&mut self, function: FunctionObject) -> FunctionHandle {
        if self.functions.len() == self.functions.capacity() {
            let new_capacity = if self.functions.capacity() == 0 {
                64
            } else {
                self.functions.capacity() * 2
            };
            self.functions
                .reserve(new_capacity - self.functions.capacity());
        }
        self.functions.push(function);
        FunctionHandle(self.functions.len() - 1)
    }

    pub fn get_function(&self, handle: FunctionHandle) -> &FunctionObject {
        &self.functions[handle.0]
    }

    pub fn iter_functions(&self) -> impl Iterator<Item = (usize, &FunctionObject)> {
        self.functions
            .iter()
            .enumerate()
            .map(|(index, obj)| (index, obj))
    }

    pub fn collect_garbage(&mut self, _roots: &[Value]) {
        // todo!("Implement mark and sweep garbage collection using provided roots");
    }
}

use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};

fn hash_value(value: &Value) -> u64 {
    let mut hasher = FxHasher::default();
    match value {
        Value::Nil => 0u8.hash(&mut hasher),
        Value::Boolean(b) => b.hash(&mut hasher),
        Value::Number(n) => n.to_bits().hash(&mut hasher),
        Value::String(handle) => handle.hash(&mut hasher),
        Value::Function(kind) => kind.hash(&mut hasher),
        Value::FunctionDecl(handle) => handle.hash(&mut hasher),
    }
    hasher.finish()
}

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
        let hash = hash_value(key);
        let mut index = (hash as usize) % self.capacity;
        
        loop {
            let bucket = &self.buckets[self.bucket_indices[index]];
            match bucket {
                Bucket::Empty => return (index, false),
                Bucket::Tombstone => {
                    // Continue probing, but remember this slot for insertion
                },
                Bucket::Occupied { key: existing_key, .. } => {
                    if existing_key == key {
                        return (index, true);
                    }
                }
            }
            index = (index + 1) % self.capacity;
        }
    }

    fn find_insert_slot(&self, key: &Value) -> usize {
        let hash = hash_value(key);
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
                },
                Bucket::Occupied { key: existing_key, .. } => {
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
            Bucket::Occupied { key: existing_key, value: old_value } if existing_key == key => {
                Some(old_value)
            },
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
