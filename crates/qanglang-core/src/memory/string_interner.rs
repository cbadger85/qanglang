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
                    if self.get(index) == s {
                        return index;
                    }
                    hash = hash.wrapping_add(1);
                }
            }
        }
    }

    pub fn get(&self, handle: StringHandle) -> &str {
        let offset = &self.offsets[handle as usize];
        let begin = offset.0 as usize;
        let end = begin + offset.1 as usize;

        unsafe { str::from_utf8_unchecked(&self.storage[begin..end]) }
    }

    pub fn concat(&mut self, handle1: StringHandle, handle2: StringHandle) -> StringHandle {
        let (str1_idx, str1_length) = self.offsets[handle1 as usize];
        let (str2_idx, str2_length) = self.offsets[handle2 as usize];
        let capacity = str1_length + str2_length;
        let str1_begin = str1_idx as usize;
        let str1_end = str1_begin + str1_length as usize;
        let str2_begin = str2_idx as usize;
        let str2_end = str2_begin + str2_length as usize;
        let mut concat_string_bytes = Vec::with_capacity(capacity as usize);
        concat_string_bytes.extend_from_slice(&self.storage[str1_begin..str1_end]);
        concat_string_bytes.extend_from_slice(&self.storage[str2_begin..str2_end]);

        let concat_string = unsafe { str::from_utf8_unchecked(&concat_string_bytes) };

        self.intern(concat_string)
    }

    pub fn starts_with(
        &self,
        string_handle: StringHandle,
        starts_with_handle: StringHandle,
    ) -> bool {
        let (str_idx, str_len) = self.offsets[string_handle as usize];
        let (prefix_idx, prefix_len) = self.offsets[starts_with_handle as usize];

        if prefix_len > str_len {
            return false;
        }

        let str_begin = str_idx as usize;
        let prefix_begin = prefix_idx as usize;
        let prefix_end = prefix_begin + prefix_len as usize;

        &self.storage[str_begin..str_begin + prefix_len as usize]
            == &self.storage[prefix_begin..prefix_end]
    }

    pub fn ends_with(&self, string_handle: StringHandle, suffix_handle: StringHandle) -> bool {
        let (str_idx, str_len) = self.offsets[string_handle as usize];
        let (suffix_idx, suffix_len) = self.offsets[suffix_handle as usize];

        if suffix_len > str_len {
            return false;
        }

        let str_end = str_idx as usize + str_len as usize;
        let str_suffix_start = str_end - suffix_len as usize;
        let suffix_begin = suffix_idx as usize;
        let suffix_end = suffix_begin + suffix_len as usize;

        &self.storage[str_suffix_start..str_end] == &self.storage[suffix_begin..suffix_end]
    }

    pub fn contains(&self, string_handle: StringHandle, needle_handle: StringHandle) -> bool {
        let (str_idx, str_len) = self.offsets[string_handle as usize];
        let (needle_idx, needle_len) = self.offsets[needle_handle as usize];

        if needle_len > str_len {
            return false;
        }

        if needle_len == 0 {
            return true;
        }

        let str_begin = str_idx as usize;
        let str_end = str_begin + str_len as usize;
        let needle_begin = needle_idx as usize;
        let needle_end = needle_begin + needle_len as usize;

        let haystack = &self.storage[str_begin..str_end];
        let needle = &self.storage[needle_begin..needle_end];

        // Use a simple sliding window search without allocation
        if needle_len as usize > haystack.len() {
            return false;
        }

        for i in 0..=(haystack.len() - needle.len()) {
            if &haystack[i..i + needle.len()] == needle {
                return true;
            }
        }

        false
    }

    pub fn to_uppercase(&mut self, handle: StringHandle) -> StringHandle {
        let s = self.get(handle);
        let uppercase = s.to_uppercase();
        self.intern(&uppercase)
    }

    pub fn to_lowercase(&mut self, handle: StringHandle) -> StringHandle {
        let s = self.get(handle);
        let lowercase = s.to_lowercase();
        self.intern(&lowercase)
    }

    pub fn length(&self, handle: StringHandle) -> usize {
        self.get(handle).chars().count()
    }

    pub fn chars(&self, handle: StringHandle) -> impl Iterator<Item = char> + '_ {
        self.get(handle).chars()
    }

    pub fn char_at(&mut self, handle: StringHandle, index: usize) -> Option<StringHandle> {
        let char_opt = self.get(handle).chars().nth(index);
        char_opt.map(|c| {
            let mut buf = [0u8; 4];
            let s = c.encode_utf8(&mut buf);
            self.intern(s)
        })
    }

    pub fn pad_start(
        &mut self,
        handle: StringHandle,
        width: usize,
        pad_char: char,
    ) -> StringHandle {
        let s = self.get(handle);
        let current_len = s.chars().count();

        if current_len >= width {
            return handle;
        }

        let pad_count = width - current_len;
        let mut padded = String::with_capacity(pad_count * pad_char.len_utf8() + s.len());

        for _ in 0..pad_count {
            padded.push(pad_char);
        }
        padded.push_str(s);

        self.intern(&padded)
    }

    pub fn pad_end(&mut self, handle: StringHandle, width: usize, pad_char: char) -> StringHandle {
        let s = self.get(handle);
        let current_len = s.chars().count();

        if current_len >= width {
            return handle;
        }

        let pad_count = width - current_len;
        let mut padded = String::with_capacity(s.len() + pad_count * pad_char.len_utf8());
        padded.push_str(s);

        for _ in 0..pad_count {
            padded.push(pad_char);
        }

        self.intern(&padded)
    }

    pub fn trim(&mut self, handle: StringHandle) -> StringHandle {
        let s = self.get(handle);
        let trimmed = s.trim().to_string();
        self.intern(&trimmed)
    }

    pub fn trim_start(&mut self, handle: StringHandle) -> StringHandle {
        let s = self.get(handle);
        let trimmed = s.trim_start().to_string();
        self.intern(&trimmed)
    }

    pub fn trim_end(&mut self, handle: StringHandle) -> StringHandle {
        let s = self.get(handle);
        let trimmed = s.trim_end().to_string();
        self.intern(&trimmed)
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
        assert_eq!(interner.get(handle1), "hello");
        assert_eq!(interner.get(handle2), "world");
    }

    #[test]
    fn test_empty_string() {
        let mut interner = StringInterner::new();

        let handle = interner.intern("");
        assert_eq!(interner.get(handle), "");
    }

    #[test]
    fn test_unicode_strings() {
        let mut interner = StringInterner::new();

        let handle1 = interner.intern("🦀");
        let handle2 = interner.intern("Hello, 世界");

        assert_eq!(interner.get(handle1), "🦀");
        assert_eq!(interner.get(handle2), "Hello, 世界");
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
            assert_eq!(interner.get(handle), expected);
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

    #[test]
    fn test_starts_with() {
        let mut interner = StringInterner::new();

        let hello = interner.intern("hello world");
        let prefix1 = interner.intern("hello");
        let prefix2 = interner.intern("world");
        let prefix3 = interner.intern("");
        let prefix4 = interner.intern("hello world");
        let prefix5 = interner.intern("hello world!");

        assert!(interner.starts_with(hello, prefix1));
        assert!(!interner.starts_with(hello, prefix2));
        assert!(interner.starts_with(hello, prefix3)); // empty string
        assert!(interner.starts_with(hello, prefix4)); // exact match
        assert!(!interner.starts_with(hello, prefix5)); // longer than string
    }

    #[test]
    fn test_starts_with_unicode() {
        let mut interner = StringInterner::new();

        let text = interner.intern("🦀 Rust");
        let prefix1 = interner.intern("🦀");
        let prefix2 = interner.intern("Rust");

        assert!(interner.starts_with(text, prefix1));
        assert!(!interner.starts_with(text, prefix2));
    }

    #[test]
    fn test_ends_with() {
        let mut interner = StringInterner::new();

        let hello = interner.intern("hello world");
        let suffix1 = interner.intern("world");
        let suffix2 = interner.intern("hello");
        let suffix3 = interner.intern("");
        let suffix4 = interner.intern("hello world");
        let suffix5 = interner.intern("!hello world");

        assert!(interner.ends_with(hello, suffix1));
        assert!(!interner.ends_with(hello, suffix2));
        assert!(interner.ends_with(hello, suffix3)); // empty string
        assert!(interner.ends_with(hello, suffix4)); // exact match
        assert!(!interner.ends_with(hello, suffix5)); // longer than string
    }

    #[test]
    fn test_ends_with_unicode() {
        let mut interner = StringInterner::new();

        let text = interner.intern("Rust 🦀");
        let suffix1 = interner.intern("🦀");
        let suffix2 = interner.intern("Rust");

        assert!(interner.ends_with(text, suffix1));
        assert!(!interner.ends_with(text, suffix2));
    }

    #[test]
    fn test_contains() {
        let mut interner = StringInterner::new();

        let text = interner.intern("hello world");
        let needle1 = interner.intern("lo wo");
        let needle2 = interner.intern("hello");
        let needle3 = interner.intern("world");
        let needle4 = interner.intern("xyz");
        let needle5 = interner.intern("");
        let needle6 = interner.intern("hello world");
        let needle7 = interner.intern("hello world!");

        assert!(interner.contains(text, needle1));
        assert!(interner.contains(text, needle2));
        assert!(interner.contains(text, needle3));
        assert!(!interner.contains(text, needle4));
        assert!(interner.contains(text, needle5)); // empty string
        assert!(interner.contains(text, needle6)); // exact match
        assert!(!interner.contains(text, needle7)); // longer than string
    }

    #[test]
    fn test_contains_unicode() {
        let mut interner = StringInterner::new();

        let text = interner.intern("Hello, 世界!");
        let needle1 = interner.intern("世界");
        let needle2 = interner.intern("世");
        let needle3 = interner.intern("界");
        let needle4 = interner.intern("🦀");

        assert!(interner.contains(text, needle1));
        assert!(interner.contains(text, needle2));
        assert!(interner.contains(text, needle3));
        assert!(!interner.contains(text, needle4));
    }

    #[test]
    fn test_to_uppercase() {
        let mut interner = StringInterner::new();

        let hello = interner.intern("hello");
        let upper = interner.to_uppercase(hello);

        assert_eq!(interner.get(upper), "HELLO");
        assert_eq!(interner.get(hello), "hello"); // original unchanged
    }

    #[test]
    fn test_to_uppercase_unicode() {
        let mut interner = StringInterner::new();

        let text = interner.intern("straße");
        let upper = interner.to_uppercase(text);

        assert_eq!(interner.get(upper), "STRASSE");
    }

    #[test]
    fn test_to_uppercase_already_upper() {
        let mut interner = StringInterner::new();

        let upper = interner.intern("HELLO");
        let result = interner.to_uppercase(upper);

        // Should return the same handle due to interning
        assert_eq!(result, upper);
        assert_eq!(interner.get(result), "HELLO");
    }

    #[test]
    fn test_to_lowercase() {
        let mut interner = StringInterner::new();

        let hello = interner.intern("HELLO");
        let lower = interner.to_lowercase(hello);

        assert_eq!(interner.get(lower), "hello");
        assert_eq!(interner.get(hello), "HELLO"); // original unchanged
    }

    #[test]
    fn test_to_lowercase_unicode() {
        let mut interner = StringInterner::new();

        let text = interner.intern("ΑΒΓΔ");
        let lower = interner.to_lowercase(text);

        assert_eq!(interner.get(lower), "αβγδ");
    }

    #[test]
    fn test_to_lowercase_already_lower() {
        let mut interner = StringInterner::new();

        let lower = interner.intern("hello");
        let result = interner.to_lowercase(lower);

        // Should return the same handle due to interning
        assert_eq!(result, lower);
        assert_eq!(interner.get(result), "hello");
    }

    #[test]
    fn test_length() {
        let mut interner = StringInterner::new();

        let empty = interner.intern("");
        let hello = interner.intern("hello");
        let emoji = interner.intern("🦀");
        let multi = interner.intern("Hello, 世界");

        assert_eq!(interner.length(empty), 0);
        assert_eq!(interner.length(hello), 5);
        assert_eq!(interner.length(emoji), 1); // one character, not bytes
        assert_eq!(interner.length(multi), 9); // counts chars not bytes
    }

    #[test]
    fn test_length_multi_byte_chars() {
        let mut interner = StringInterner::new();

        let text = interner.intern("🦀🦀🦀");
        assert_eq!(interner.length(text), 3);

        let text2 = interner.intern("世界");
        assert_eq!(interner.length(text2), 2);
    }

    #[test]
    fn test_operations_with_empty_strings() {
        let mut interner = StringInterner::new();

        let empty = interner.intern("");
        let text = interner.intern("test");

        assert!(interner.starts_with(empty, empty));
        assert!(interner.ends_with(empty, empty));
        assert!(interner.contains(empty, empty));
        assert!(interner.starts_with(text, empty));
        assert!(interner.ends_with(text, empty));
        assert!(interner.contains(text, empty));

        let upper = interner.to_uppercase(empty);
        let lower = interner.to_lowercase(empty);

        assert_eq!(interner.get(upper), "");
        assert_eq!(interner.get(lower), "");
        assert_eq!(interner.length(empty), 0);
    }

    #[test]
    fn test_char_at() {
        let mut interner = StringInterner::new();

        let hello = interner.intern("hello");

        let h = interner.char_at(hello, 0);
        assert_eq!(h.map(|handle| interner.get(handle)), Some("h"));

        let e = interner.char_at(hello, 1);
        assert_eq!(e.map(|handle| interner.get(handle)), Some("e"));

        let o = interner.char_at(hello, 4);
        assert_eq!(o.map(|handle| interner.get(handle)), Some("o"));

        assert_eq!(interner.char_at(hello, 5), None); // out of bounds
        assert_eq!(interner.char_at(hello, 100), None); // way out of bounds
    }

    #[test]
    fn test_char_at_unicode() {
        let mut interner = StringInterner::new();

        let text = interner.intern("Hello, 世界!");
        assert_eq!(
            interner.char_at(text, 0).map(|h| interner.get(h)),
            Some("H")
        );
        assert_eq!(
            interner.char_at(text, 7).map(|h| interner.get(h)),
            Some("世")
        );
        assert_eq!(
            interner.char_at(text, 8).map(|h| interner.get(h)),
            Some("界")
        );
        assert_eq!(
            interner.char_at(text, 9).map(|h| interner.get(h)),
            Some("!")
        );
        assert_eq!(interner.char_at(text, 10), None);

        let emoji = interner.intern("🦀🦀🦀");
        assert_eq!(
            interner.char_at(emoji, 0).map(|h| interner.get(h)),
            Some("🦀")
        );
        assert_eq!(
            interner.char_at(emoji, 1).map(|h| interner.get(h)),
            Some("🦀")
        );
        assert_eq!(
            interner.char_at(emoji, 2).map(|h| interner.get(h)),
            Some("🦀")
        );
        assert_eq!(interner.char_at(emoji, 3), None);
    }

    #[test]
    fn test_char_at_empty_string() {
        let mut interner = StringInterner::new();

        let empty = interner.intern("");
        assert_eq!(interner.char_at(empty, 0), None);
    }

    #[test]
    fn test_pad_start() {
        let mut interner = StringInterner::new();

        let hello = interner.intern("hello");
        let padded = interner.pad_start(hello, 10, ' ');
        assert_eq!(interner.get(padded), "     hello");

        let padded_stars = interner.pad_start(hello, 8, '*');
        assert_eq!(interner.get(padded_stars), "***hello");

        // Test when string is already at target width
        let no_pad = interner.pad_start(hello, 5, ' ');
        assert_eq!(no_pad, hello);
        assert_eq!(interner.get(no_pad), "hello");

        // Test when string is longer than target width
        let no_pad_long = interner.pad_start(hello, 3, ' ');
        assert_eq!(no_pad_long, hello);
        assert_eq!(interner.get(no_pad_long), "hello");
    }

    #[test]
    fn test_pad_start_unicode() {
        let mut interner = StringInterner::new();

        let emoji = interner.intern("🦀");
        let padded = interner.pad_start(emoji, 5, '-');
        assert_eq!(interner.get(padded), "----🦀");

        let text = interner.intern("世界");
        let padded_text = interner.pad_start(text, 5, '0');
        assert_eq!(interner.get(padded_text), "000世界");
    }

    #[test]
    fn test_pad_end() {
        let mut interner = StringInterner::new();

        let hello = interner.intern("hello");
        let padded = interner.pad_end(hello, 10, ' ');
        assert_eq!(interner.get(padded), "hello     ");

        let padded_stars = interner.pad_end(hello, 8, '*');
        assert_eq!(interner.get(padded_stars), "hello***");

        // Test when string is already at target width
        let no_pad = interner.pad_end(hello, 5, ' ');
        assert_eq!(no_pad, hello);
        assert_eq!(interner.get(no_pad), "hello");

        // Test when string is longer than target width
        let no_pad_long = interner.pad_end(hello, 3, ' ');
        assert_eq!(no_pad_long, hello);
        assert_eq!(interner.get(no_pad_long), "hello");
    }

    #[test]
    fn test_pad_end_unicode() {
        let mut interner = StringInterner::new();

        let emoji = interner.intern("🦀");
        let padded = interner.pad_end(emoji, 5, '-');
        assert_eq!(interner.get(padded), "🦀----");

        let text = interner.intern("世界");
        let padded_text = interner.pad_end(text, 5, '0');
        assert_eq!(interner.get(padded_text), "世界000");
    }

    #[test]
    fn test_trim() {
        let mut interner = StringInterner::new();

        let spaced = interner.intern("  hello  ");
        let trimmed = interner.trim(spaced);
        assert_eq!(interner.get(trimmed), "hello");

        let tab_newline = interner.intern("\t\nhello\n\t");
        let trimmed2 = interner.trim(tab_newline);
        assert_eq!(interner.get(trimmed2), "hello");

        // Test with no whitespace
        let no_space = interner.intern("hello");
        let trimmed3 = interner.trim(no_space);
        assert_eq!(interner.get(trimmed3), "hello");

        // Test with only whitespace
        let only_space = interner.intern("   ");
        let trimmed4 = interner.trim(only_space);
        assert_eq!(interner.get(trimmed4), "");

        // Test empty string
        let empty = interner.intern("");
        let trimmed5 = interner.trim(empty);
        assert_eq!(interner.get(trimmed5), "");
    }

    #[test]
    fn test_trim_unicode() {
        let mut interner = StringInterner::new();

        let text = interner.intern("  Hello, 世界!  ");
        let trimmed = interner.trim(text);
        assert_eq!(interner.get(trimmed), "Hello, 世界!");

        let emoji = interner.intern("\n\t🦀🦀\t\n");
        let trimmed_emoji = interner.trim(emoji);
        assert_eq!(interner.get(trimmed_emoji), "🦀🦀");
    }

    #[test]
    fn test_trim_start() {
        let mut interner = StringInterner::new();

        let spaced = interner.intern("  hello  ");
        let trimmed = interner.trim_start(spaced);
        assert_eq!(interner.get(trimmed), "hello  ");

        let tab_newline = interner.intern("\t\nhello");
        let trimmed2 = interner.trim_start(tab_newline);
        assert_eq!(interner.get(trimmed2), "hello");

        // Test with no leading whitespace
        let no_space = interner.intern("hello  ");
        let trimmed3 = interner.trim_start(no_space);
        assert_eq!(interner.get(trimmed3), "hello  ");

        // Test with only whitespace
        let only_space = interner.intern("   ");
        let trimmed4 = interner.trim_start(only_space);
        assert_eq!(interner.get(trimmed4), "");
    }

    #[test]
    fn test_trim_start_unicode() {
        let mut interner = StringInterner::new();

        let text = interner.intern("  🦀 Rust  ");
        let trimmed = interner.trim_start(text);
        assert_eq!(interner.get(trimmed), "🦀 Rust  ");
    }

    #[test]
    fn test_trim_end() {
        let mut interner = StringInterner::new();

        let spaced = interner.intern("  hello  ");
        let trimmed = interner.trim_end(spaced);
        assert_eq!(interner.get(trimmed), "  hello");

        let tab_newline = interner.intern("hello\n\t");
        let trimmed2 = interner.trim_end(tab_newline);
        assert_eq!(interner.get(trimmed2), "hello");

        // Test with no trailing whitespace
        let no_space = interner.intern("  hello");
        let trimmed3 = interner.trim_end(no_space);
        assert_eq!(interner.get(trimmed3), "  hello");

        // Test with only whitespace
        let only_space = interner.intern("   ");
        let trimmed4 = interner.trim_end(only_space);
        assert_eq!(interner.get(trimmed4), "");
    }

    #[test]
    fn test_trim_end_unicode() {
        let mut interner = StringInterner::new();

        let text = interner.intern("  Rust 🦀  ");
        let trimmed = interner.trim_end(text);
        assert_eq!(interner.get(trimmed), "  Rust 🦀");
    }
}
