use rustc_hash::{FxBuildHasher, FxHashMap, FxHasher};
use std::hash::{Hash, Hasher};
use std::str;

pub type StringHandle = u32;

#[derive(Debug, Clone, Default)]
pub struct StringInterner {
    map: FxHashMap<u64, StringHandle>,
    storage: Vec<u8>,
    offsets: Vec<(u32, u32)>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self::with_capacity(1000, 100000)
    }

    pub fn with_capacity(capacity: usize, bytes: usize) -> Self {
        Self {
            map: FxHashMap::with_capacity_and_hasher(capacity, FxBuildHasher),
            storage: Vec::with_capacity(bytes),
            offsets: Vec::with_capacity(capacity),
        }
    }

    pub fn intern(&mut self, s: &str) -> StringHandle {
        let mut hasher = FxHasher::default();
        s.hash(&mut hasher);
        let mut hash = hasher.finish();

        loop {
            match self.map.get(&hash) {
                None => {
                    let storage_index = self.storage.len() as u32;
                    let offset_index = self.offsets.len() as u32;
                    self.map.insert(hash, offset_index);
                    self.storage.extend_from_slice(s.as_bytes());
                    self.offsets.push((storage_index, s.len() as u32));
                    return offset_index;
                }
                Some(&index) => {
                    if self.get_string(index) == s {
                        return index;
                    }
                    hash = hash.wrapping_add(1);
                }
            }
        }
    }

    pub fn get_string(&self, handle: StringHandle) -> &str {
        let offset = &self.offsets[handle as usize];
        let begin = offset.0 as usize;
        let end = begin + offset.1 as usize;

        str::from_utf8(&self.storage[begin..end]).unwrap()
    }

    pub fn chars(&self, handle: StringHandle) -> impl Iterator<Item = char> + '_ {
        self.get_string(handle).chars()
    }

    pub fn get_allocated_bytes(&self) -> usize {
        self.storage.len() + self.offsets.len() * std::mem::size_of::<(u32, u32)>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_interning() {
        let mut interner = StringInterner::new();

        let handle1 = interner.intern("hello");
        let handle2 = interner.intern("world");
        let handle3 = interner.intern("hello");

        assert_eq!(handle1, handle3);
        assert_ne!(handle1, handle2);
        assert_eq!(interner.get_string(handle1), "hello");
        assert_eq!(interner.get_string(handle2), "world");
    }

    #[test]
    fn test_empty_string() {
        let mut interner = StringInterner::new();

        let handle = interner.intern("");
        assert_eq!(interner.get_string(handle), "");
    }

    #[test]
    fn test_unicode_strings() {
        let mut interner = StringInterner::new();

        let handle1 = interner.intern("🦀");
        let handle2 = interner.intern("Hello, 世界");

        assert_eq!(interner.get_string(handle1), "🦀");
        assert_eq!(interner.get_string(handle2), "Hello, 世界");
    }

    #[test]
    fn test_chars_iterator() {
        let mut interner = StringInterner::new();

        let handle = interner.intern("abc");
        let chars: Vec<char> = interner.chars(handle).collect();

        assert_eq!(chars, vec!['a', 'b', 'c']);
    }

    #[test]
    fn test_hash_collisions() {
        let mut interner = StringInterner::new();

        let mut handles = Vec::new();
        for i in 0..1000 {
            let s = format!("string_{}", i);
            let handle = interner.intern(&s);
            handles.push((handle, s));
        }

        for (handle, expected) in handles {
            assert_eq!(interner.get_string(handle), expected);
        }
    }

    #[test]
    fn test_deduplication() {
        let mut interner = StringInterner::new();

        let handle1 = interner.intern("test");
        let handle2 = interner.intern("test");
        let handle3 = interner.intern("test");

        assert_eq!(handle1, handle2);
        assert_eq!(handle2, handle3);

        assert_eq!(interner.offsets.len(), 1);
    }
}
