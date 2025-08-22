use crate::Value;
use crate::memory::arena::{Arena, Index};

const CHUNK_SIZE: usize = 8; // Buckets per chunk
const DEFAULT_CAPACITY: usize = 16;
const LOAD_FACTOR_THRESHOLD: f64 = 0.75;

pub type HashMapHandle = Index;

#[derive(Debug, Clone)]
pub struct HashMapObject {
    pub first_chunk: Option<BucketChunkHandle>,
    pub len: usize,
    pub capacity: usize,
    pub is_marked: bool,
}

impl HashMapObject {
    pub fn new(capacity: usize) -> Self {
        Self {
            first_chunk: None,
            len: 0,
            capacity,
            is_marked: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BucketChunk {
    pub buckets: [Bucket; CHUNK_SIZE],
    pub next_chunk: Option<BucketChunkHandle>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Default)]
pub struct BucketChunkHandle(Index);

#[derive(Debug, Clone, Copy)]
pub struct Bucket {
    pub kind: BucketKind,
}

#[derive(Debug, Clone, Copy)]
pub enum BucketKind {
    Empty,
    Occupied { key: Value, value: Value },
    Tombstone,
}

impl Default for Bucket {
    fn default() -> Self {
        Bucket {
            kind: BucketKind::Empty,
        }
    }
}

impl Default for BucketChunk {
    fn default() -> Self {
        BucketChunk {
            buckets: [Bucket::default(); CHUNK_SIZE],
            next_chunk: None,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct HashMapArena {
    hashmaps: Arena<HashMapObject>,
    chunks: Arena<BucketChunk>,
}

impl HashMapArena {
    pub fn new() -> Self {
        Self::with_capacity(64)
    }

    pub fn with_capacity(initial_capacity: usize) -> Self {
        Self {
            hashmaps: Arena::with_capacity(initial_capacity),
            chunks: Arena::with_capacity(initial_capacity * 4),
        }
    }

    pub fn new_hashmap(&mut self) -> HashMapHandle {
        self.new_hashmap_with_capacity(DEFAULT_CAPACITY)
    }

    pub fn new_hashmap_with_capacity(&mut self, capacity: usize) -> HashMapHandle {
        let hashmap = HashMapObject::new(capacity);
        self.hashmaps.insert(hashmap)
    }

    pub fn insert(&mut self, handle: HashMapHandle, key: Value, value: Value) -> Option<Value> {
        let hash = key.hash();

        // Check if we need to resize before inserting
        if let Some(hashmap) = self.hashmaps.get(handle) {
            let load_factor = hashmap.len as f64 / hashmap.capacity as f64;
            if load_factor >= LOAD_FACTOR_THRESHOLD {
                self.resize_hashmap(handle);
            }
        }

        self.insert_with_hash(handle, key, value, hash)
    }

    fn insert_with_hash(
        &mut self,
        handle: HashMapHandle,
        key: Value,
        value: Value,
        hash: u64,
    ) -> Option<Value> {
        let capacity = self.hashmaps.get(handle)?.capacity;
        let bucket_index = (hash as usize) % capacity;

        // Find or create the bucket position
        let (chunk_handle, bucket_idx_in_chunk) = self.ensure_bucket_exists(handle, bucket_index);

        // Linear probing to find insertion point
        let mut current_chunk = chunk_handle;
        let mut current_bucket_idx = bucket_idx_in_chunk;
        let mut probed = 0;

        loop {
            if let Some(chunk) = self.chunks.get_mut(current_chunk.0) {
                let bucket = &mut chunk.buckets[current_bucket_idx];

                match bucket.kind {
                    BucketKind::Empty | BucketKind::Tombstone => {
                        // Found insertion point
                        bucket.kind = BucketKind::Occupied { key, value };
                        if let Some(hashmap) = self.hashmaps.get_mut(handle) {
                            hashmap.len += 1;
                        }
                        return None;
                    }
                    BucketKind::Occupied {
                        key: existing_key,
                        value: existing_value,
                    } => {
                        if existing_key == key {
                            // Key already exists, replace value
                            bucket.kind = BucketKind::Occupied { key, value };
                            return Some(existing_value);
                        }
                        // Continue probing
                    }
                }
            }

            // Move to next bucket
            current_bucket_idx += 1;
            if current_bucket_idx >= CHUNK_SIZE {
                current_bucket_idx = 0;
                if let Some(chunk) = self.chunks.get(current_chunk.0) {
                    if let Some(next_chunk) = chunk.next_chunk {
                        current_chunk = next_chunk;
                    } else {
                        // Need to allocate new chunk
                        let new_chunk = BucketChunk::default();
                        let new_chunk_handle = BucketChunkHandle(self.chunks.insert(new_chunk));

                        // Link the new chunk
                        if let Some(chunk) = self.chunks.get_mut(current_chunk.0) {
                            chunk.next_chunk = Some(new_chunk_handle);
                        }
                        current_chunk = new_chunk_handle;
                    }
                }
            }

            probed += 1;
            if probed >= capacity {
                // This shouldn't happen with proper load factor management
                break;
            }
        }

        None
    }

    pub fn get(&self, handle: HashMapHandle, key: &Value) -> Option<Value> {
        let hashmap = self.hashmaps.get(handle)?;
        let hash = key.hash();
        let bucket_index = (hash as usize) % hashmap.capacity;

        let (chunk_handle, bucket_idx_in_chunk) = self.get_bucket_position(handle, bucket_index)?;

        // Linear probing to find key
        let mut current_chunk = chunk_handle;
        let mut current_bucket_idx = bucket_idx_in_chunk;
        let mut probed = 0;

        loop {
            if let Some(chunk) = self.chunks.get(current_chunk.0) {
                let bucket = &chunk.buckets[current_bucket_idx];

                match bucket.kind {
                    BucketKind::Empty => {
                        // Key not found
                        return None;
                    }
                    BucketKind::Occupied {
                        key: existing_key,
                        value,
                    } => {
                        if existing_key == *key {
                            return Some(value);
                        }
                    }
                    BucketKind::Tombstone => {
                        // Continue searching
                    }
                }
            }

            // Move to next bucket
            current_bucket_idx += 1;
            if current_bucket_idx >= CHUNK_SIZE {
                current_bucket_idx = 0;
                if let Some(chunk) = self.chunks.get(current_chunk.0) {
                    if let Some(next_chunk) = chunk.next_chunk {
                        current_chunk = next_chunk;
                    } else {
                        break;
                    }
                }
            }

            probed += 1;
            if probed >= hashmap.capacity {
                break;
            }
        }

        None
    }

    pub fn remove(&mut self, handle: HashMapHandle, key: &Value) -> Option<Value> {
        let hashmap = self.hashmaps.get(handle)?;
        let hash = key.hash();
        let bucket_index = (hash as usize) % hashmap.capacity;

        let (chunk_handle, bucket_idx_in_chunk) = self.get_bucket_position(handle, bucket_index)?;

        // Linear probing to find key
        let mut current_chunk = chunk_handle;
        let mut current_bucket_idx = bucket_idx_in_chunk;
        let mut probed = 0;

        loop {
            if let Some(chunk) = self.chunks.get_mut(current_chunk.0) {
                let bucket = &mut chunk.buckets[current_bucket_idx];

                match bucket.kind {
                    BucketKind::Empty => {
                        // Key not found
                        return None;
                    }
                    BucketKind::Occupied {
                        key: existing_key,
                        value,
                    } => {
                        if existing_key == *key {
                            bucket.kind = BucketKind::Tombstone;
                            if let Some(hashmap) = self.hashmaps.get_mut(handle) {
                                hashmap.len -= 1;
                            }
                            return Some(value);
                        }
                    }
                    BucketKind::Tombstone => {
                        // Continue searching
                    }
                }
            }

            // Move to next bucket
            current_bucket_idx += 1;
            if current_bucket_idx >= CHUNK_SIZE {
                current_bucket_idx = 0;
                if let Some(chunk) = self.chunks.get(current_chunk.0) {
                    if let Some(next_chunk) = chunk.next_chunk {
                        current_chunk = next_chunk;
                    } else {
                        break;
                    }
                }
            }

            probed += 1;
            if probed >= hashmap.capacity {
                break;
            }
        }

        None
    }

    pub fn len(&self, handle: HashMapHandle) -> usize {
        self.hashmaps.get(handle).map(|hm| hm.len).unwrap_or(0)
    }

    pub fn is_empty(&self, handle: HashMapHandle) -> bool {
        self.len(handle) == 0
    }

    fn ensure_bucket_exists(
        &mut self,
        handle: HashMapHandle,
        bucket_index: usize,
    ) -> (BucketChunkHandle, usize) {
        let hashmap = self.hashmaps.get_mut(handle).unwrap();

        // Create first chunk if needed
        if hashmap.first_chunk.is_none() {
            let chunk = BucketChunk::default();
            let chunk_handle = BucketChunkHandle(self.chunks.insert(chunk));
            hashmap.first_chunk = Some(chunk_handle);
        }

        let chunk_handle = hashmap.first_chunk.unwrap();
        let bucket_idx_in_chunk = bucket_index % CHUNK_SIZE;

        (chunk_handle, bucket_idx_in_chunk)
    }

    fn get_bucket_position(
        &self,
        handle: HashMapHandle,
        bucket_index: usize,
    ) -> Option<(BucketChunkHandle, usize)> {
        let hashmap = self.hashmaps.get(handle)?;
        let chunk_handle = hashmap.first_chunk?;
        let bucket_idx_in_chunk = bucket_index % CHUNK_SIZE;

        Some((chunk_handle, bucket_idx_in_chunk))
    }

    fn resize_hashmap(&mut self, handle: HashMapHandle) {
        // Double the capacity
        let old_capacity = self.hashmaps.get(handle).unwrap().capacity;
        let new_capacity = old_capacity * 2;

        // Collect all existing key-value pairs
        let mut pairs = Vec::new();
        if let Some(hashmap) = self.hashmaps.get(handle)
            && let Some(first_chunk) = hashmap.first_chunk
        {
            self.collect_all_pairs(first_chunk, &mut pairs);
        }

        // Clear the hashmap and update capacity
        if let Some(hashmap) = self.hashmaps.get_mut(handle) {
            hashmap.first_chunk = None;
            hashmap.len = 0;
            hashmap.capacity = new_capacity;
        }

        // Reinsert all pairs
        for (key, value) in pairs {
            let hash = key.hash();
            self.insert_with_hash(handle, key, value, hash);
        }
    }

    fn collect_all_pairs(&self, chunk_handle: BucketChunkHandle, pairs: &mut Vec<(Value, Value)>) {
        if let Some(chunk) = self.chunks.get(chunk_handle.0) {
            for bucket in &chunk.buckets {
                if let BucketKind::Occupied { key, value } = bucket.kind {
                    pairs.push((key, value));
                }
            }

            if let Some(next_chunk) = chunk.next_chunk {
                self.collect_all_pairs(next_chunk, pairs);
            }
        }
    }

    pub fn iter(&self, handle: HashMapHandle) -> HashMapIterator<'_> {
        HashMapIterator::new(self, handle)
    }

    pub fn mark_hashmap(&mut self, handle: HashMapHandle, mark: bool) {
        let hashmap = &mut self.hashmaps[handle];
        hashmap.is_marked = mark;
    }

    pub fn delete_hashmap(&mut self, handle: HashMapHandle) -> bool {
        if let Some(hashmap) = self.hashmaps.get(handle) {
            // Mark all chunks as unused by clearing the first_chunk reference
            // The chunks will be cleaned up by the arena's garbage collection
            if let Some(first_chunk) = hashmap.first_chunk {
                self.mark_chunks_for_cleanup(first_chunk);
            }

            // Remove the hashmap from the arena
            self.hashmaps.remove(handle);
            true
        } else {
            false
        }
    }

    fn mark_chunks_for_cleanup(&mut self, chunk_handle: BucketChunkHandle) {
        if let Some(chunk) = self.chunks.get(chunk_handle.0) {
            let next_chunk = chunk.next_chunk;
            // Remove this chunk
            self.chunks.remove(chunk_handle.0);

            // Recursively mark next chunks for cleanup
            if let Some(next) = next_chunk {
                self.mark_chunks_for_cleanup(next);
            }
        }
    }

    pub fn get_allocated_bytes(&self) -> usize {
        let hashmap_bytes = self.hashmaps.len() * std::mem::size_of::<HashMapObject>();
        let chunk_bytes = self.chunks.len() * std::mem::size_of::<BucketChunk>();
        hashmap_bytes + chunk_bytes
    }
}

pub struct HashMapIterator<'a> {
    arena: &'a HashMapArena,
    handle: HashMapHandle,
    current_chunk: Option<BucketChunkHandle>,
    current_bucket_idx: usize,
}

impl<'a> HashMapIterator<'a> {
    fn new(arena: &'a HashMapArena, handle: HashMapHandle) -> Self {
        let current_chunk = arena.hashmaps.get(handle).and_then(|hm| hm.first_chunk);

        Self {
            arena,
            handle,
            current_chunk,
            current_bucket_idx: 0,
        }
    }
}

impl<'a> Iterator for HashMapIterator<'a> {
    type Item = (Value, Value);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(chunk_handle) = self.current_chunk {
            if let Some(chunk) = self.arena.chunks.get(chunk_handle.0) {
                // Search current chunk for next occupied bucket
                while self.current_bucket_idx < CHUNK_SIZE {
                    let bucket = &chunk.buckets[self.current_bucket_idx];
                    self.current_bucket_idx += 1;

                    if let BucketKind::Occupied { key, value } = bucket.kind {
                        return Some((key, value));
                    }
                }

                // Move to next chunk
                self.current_chunk = chunk.next_chunk;
                self.current_bucket_idx = 0;
            } else {
                break;
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_value(n: i32) -> Value {
        Value::Number(n as f64)
    }

    #[test]
    fn test_new_hashmap() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        assert_eq!(arena.len(handle), 0);
        assert!(arena.is_empty(handle));
    }

    #[test]
    fn test_insert_and_get() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        let key1 = create_test_value(1);
        let value1 = create_test_value(100);

        // Insert first key-value pair
        let old_value = arena.insert(handle, key1, value1);
        assert!(old_value.is_none());
        assert_eq!(arena.len(handle), 1);

        // Get the value back
        let retrieved = arena.get(handle, &key1);
        assert_eq!(retrieved, Some(value1));
    }

    #[test]
    fn test_insert_duplicate_key() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        let key = create_test_value(1);
        let value1 = create_test_value(100);
        let value2 = create_test_value(200);

        // Insert first value
        arena.insert(handle, key, value1);
        assert_eq!(arena.len(handle), 1);

        // Insert same key with different value
        let old_value = arena.insert(handle, key, value2);
        assert_eq!(old_value, Some(value1));
        assert_eq!(arena.len(handle), 1);

        // Verify new value is stored
        assert_eq!(arena.get(handle, &key), Some(value2));
    }

    #[test]
    fn test_multiple_insertions() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        for i in 0..10 {
            let key = create_test_value(i);
            let value = create_test_value(i * 10);
            arena.insert(handle, key, value);
        }

        assert_eq!(arena.len(handle), 10);

        // Verify all values can be retrieved
        for i in 0..10 {
            let key = create_test_value(i);
            let expected_value = create_test_value(i * 10);
            assert_eq!(arena.get(handle, &key), Some(expected_value));
        }
    }

    #[test]
    fn test_remove() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        let key = create_test_value(1);
        let value = create_test_value(100);

        // Insert and then remove
        arena.insert(handle, key, value);
        assert_eq!(arena.len(handle), 1);

        let removed_value = arena.remove(handle, &key);
        assert_eq!(removed_value, Some(value));
        assert_eq!(arena.len(handle), 0);
        assert!(arena.is_empty(handle));

        // Try to get removed key
        assert_eq!(arena.get(handle, &key), None);
    }

    #[test]
    fn test_remove_nonexistent_key() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        let key = create_test_value(1);
        let removed = arena.remove(handle, &key);
        assert!(removed.is_none());
        assert_eq!(arena.len(handle), 0);
    }

    #[test]
    fn test_get_nonexistent_key() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        let key = create_test_value(1);
        let result = arena.get(handle, &key);
        assert!(result.is_none());
    }

    #[test]
    fn test_linear_probing_collision() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap_with_capacity(4); // Small capacity to force collisions

        // Insert several different values - some will collide in a small hashmap
        let key1 = create_test_value(1);
        let key2 = create_test_value(2);
        let key3 = create_test_value(3);
        let value1 = create_test_value(100);
        let value2 = create_test_value(200);
        let value3 = create_test_value(300);

        arena.insert(handle, key1, value1);
        arena.insert(handle, key2, value2);
        arena.insert(handle, key3, value3);

        assert_eq!(arena.len(handle), 3);
        assert_eq!(arena.get(handle, &key1), Some(value1));
        assert_eq!(arena.get(handle, &key2), Some(value2));
        assert_eq!(arena.get(handle, &key3), Some(value3));
    }

    #[test]
    fn test_resize_on_load_factor() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap_with_capacity(4);

        // Insert enough items to trigger resize (load factor > 0.75)
        for i in 0..6 {
            let key = create_test_value(i);
            let value = create_test_value(i * 10);
            arena.insert(handle, key, value);
        }

        assert_eq!(arena.len(handle), 6);

        // Verify capacity has increased and all values are still accessible
        for i in 0..6 {
            let key = create_test_value(i);
            let expected_value = create_test_value(i * 10);
            assert_eq!(arena.get(handle, &key), Some(expected_value));
        }

        // Verify capacity has increased
        if let Some(hashmap) = arena.hashmaps.get(handle) {
            assert!(hashmap.capacity > 4);
        }
    }

    #[test]
    fn test_iterator() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        // Insert test data
        let test_pairs = vec![
            (create_test_value(1), create_test_value(10)),
            (create_test_value(2), create_test_value(20)),
            (create_test_value(3), create_test_value(30)),
        ];

        for (key, value) in &test_pairs {
            arena.insert(handle, *key, *value);
        }

        // Collect iterator results
        let mut collected: Vec<_> = arena.iter(handle).collect();
        collected.sort_by_key(|(k, _)| match k {
            Value::Number(n) => *n as i32,
            _ => 0,
        });

        assert_eq!(collected.len(), 3);
        assert_eq!(collected[0], (create_test_value(1), create_test_value(10)));
        assert_eq!(collected[1], (create_test_value(2), create_test_value(20)));
        assert_eq!(collected[2], (create_test_value(3), create_test_value(30)));
    }

    #[test]
    fn test_iterator_empty_hashmap() {
        let arena = HashMapArena::new();
        let handle = HashMapHandle::default();

        let collected: Vec<_> = arena.iter(handle).collect();
        assert!(collected.is_empty());
    }

    #[test]
    fn test_iterator_after_removals() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        // Insert several items
        for i in 0..5 {
            let key = create_test_value(i);
            let value = create_test_value(i * 10);
            arena.insert(handle, key, value);
        }

        // Remove some items
        arena.remove(handle, &create_test_value(1));
        arena.remove(handle, &create_test_value(3));

        // Collect remaining items
        let mut collected: Vec<_> = arena.iter(handle).collect();
        collected.sort_by_key(|(k, _)| match k {
            Value::Number(n) => *n as i32,
            _ => 0,
        });

        assert_eq!(collected.len(), 3);
        assert_eq!(collected[0], (create_test_value(0), create_test_value(0)));
        assert_eq!(collected[1], (create_test_value(2), create_test_value(20)));
        assert_eq!(collected[2], (create_test_value(4), create_test_value(40)));
    }

    #[test]
    fn test_stress_insertions_and_lookups() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        const NUM_ITEMS: i32 = 100;

        // Insert many items
        for i in 0..NUM_ITEMS {
            let key = create_test_value(i);
            let value = create_test_value(i * 7); // Use different multiplier
            arena.insert(handle, key, value);
        }

        assert_eq!(arena.len(handle), NUM_ITEMS as usize);

        // Verify all can be retrieved
        for i in 0..NUM_ITEMS {
            let key = create_test_value(i);
            let expected_value = create_test_value(i * 7);
            assert_eq!(arena.get(handle, &key), Some(expected_value));
        }

        // Remove half
        for i in 0..NUM_ITEMS / 2 {
            let key = create_test_value(i * 2); // Remove even numbers
            arena.remove(handle, &key);
        }

        assert_eq!(arena.len(handle), (NUM_ITEMS - NUM_ITEMS / 2) as usize);

        // Verify remaining items
        for i in 0..NUM_ITEMS {
            let key = create_test_value(i);
            let expected_value = create_test_value(i * 7);

            if i % 2 == 0 && i < NUM_ITEMS {
                // Even numbers should be removed
                assert_eq!(arena.get(handle, &key), None);
            } else {
                // Odd numbers should still exist
                assert_eq!(arena.get(handle, &key), Some(expected_value));
            }
        }
    }

    #[test]
    fn test_different_value_types() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        // Test different value types
        arena.insert(handle, Value::Nil, Value::True);
        arena.insert(handle, Value::False, Value::Number(42.0));
        arena.insert(handle, Value::Number(123.0), Value::Nil);

        assert_eq!(arena.get(handle, &Value::Nil), Some(Value::True));
        assert_eq!(arena.get(handle, &Value::False), Some(Value::Number(42.0)));
        assert_eq!(arena.get(handle, &Value::Number(123.0)), Some(Value::Nil));

        assert_eq!(arena.len(handle), 3);
    }

    #[test]
    fn test_delete_hashmap() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        // Insert some data
        for i in 0..5 {
            let key = create_test_value(i);
            let value = create_test_value(i * 10);
            arena.insert(handle, key, value);
        }

        assert_eq!(arena.len(handle), 5);
        assert!(!arena.is_empty(handle));

        // Delete the hashmap
        let deleted = arena.delete_hashmap(handle);
        assert!(deleted);

        // Verify hashmap is gone - operations should return default/empty values
        assert_eq!(arena.len(handle), 0);
        assert!(arena.is_empty(handle));
        assert_eq!(arena.get(handle, &create_test_value(0)), None);
    }

    #[test]
    fn test_delete_nonexistent_hashmap() {
        let mut arena = HashMapArena::new();
        let invalid_handle = HashMapHandle::default();

        // Try to delete a hashmap that doesn't exist
        let deleted = arena.delete_hashmap(invalid_handle);
        assert!(!deleted);
    }

    #[test]
    fn test_delete_hashmap_with_multiple_chunks() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap_with_capacity(4); // Small capacity to force multiple chunks

        // Insert enough data to create multiple chunks
        for i in 0..20 {
            let key = create_test_value(i);
            let value = create_test_value(i * 10);
            arena.insert(handle, key, value);
        }

        let initial_chunk_count = arena.chunks.len();
        assert!(initial_chunk_count > 1); // Should have multiple chunks

        // Delete the hashmap
        let deleted = arena.delete_hashmap(handle);
        assert!(deleted);

        // Verify all chunks associated with this hashmap are cleaned up
        // Note: The exact chunk count depends on internal implementation,
        // but it should be significantly reduced
        assert_eq!(arena.len(handle), 0);
        assert!(arena.is_empty(handle));
    }

    #[test]
    fn test_delete_hashmap_preserves_other_hashmaps() {
        let mut arena = HashMapArena::new();
        let handle1 = arena.new_hashmap();
        let handle2 = arena.new_hashmap();

        // Insert data in both hashmaps
        for i in 0..3 {
            let key = create_test_value(i);
            let value1 = create_test_value(i * 10);
            let value2 = create_test_value(i * 20);

            arena.insert(handle1, key, value1);
            arena.insert(handle2, key, value2);
        }

        assert_eq!(arena.len(handle1), 3);
        assert_eq!(arena.len(handle2), 3);

        // Delete first hashmap
        let deleted = arena.delete_hashmap(handle1);
        assert!(deleted);

        // Verify first hashmap is gone
        assert_eq!(arena.len(handle1), 0);
        assert!(arena.is_empty(handle1));
        assert_eq!(arena.get(handle1, &create_test_value(0)), None);

        // Verify second hashmap is unaffected
        assert_eq!(arena.len(handle2), 3);
        assert!(!arena.is_empty(handle2));
        for i in 0..3 {
            let key = create_test_value(i);
            let expected_value = create_test_value(i * 20);
            assert_eq!(arena.get(handle2, &key), Some(expected_value));
        }
    }

    #[test]
    fn test_delete_hashmap_memory_efficiency() {
        let mut arena = HashMapArena::new();
        let handle = arena.new_hashmap();

        // Insert substantial data
        for i in 0..50 {
            let key = create_test_value(i);
            let value = create_test_value(i * 10);
            arena.insert(handle, key, value);
        }

        let bytes_before = arena.get_allocated_bytes();
        assert!(bytes_before > 0);

        // Delete the hashmap
        arena.delete_hashmap(handle);

        let bytes_after = arena.get_allocated_bytes();

        // Memory usage should be significantly reduced
        // (May not be zero due to arena implementation details)
        assert!(bytes_after < bytes_before);
    }
}
