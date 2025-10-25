use std::any::{Any, TypeId};

use rustc_hash::{FxBuildHasher, FxHashMap};

/// External state that can be accessed by native functions
#[derive(Debug, Default)]
pub struct ExternalContext {
    data: FxHashMap<TypeId, Box<dyn Any>>,
}

impl ExternalContext {
    pub fn new() -> Self {
        Self {
            data: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    /// Store arbitrary data that native functions can access
    pub fn insert<T: 'static>(&mut self, value: T) {
        self.data.insert(TypeId::of::<T>(), Box::new(value));
    }

    /// Get reference to stored data
    pub fn get<T: 'static>(&self) -> Option<&T> {
        self.data
            .get(&TypeId::of::<T>())
            .and_then(|b| b.downcast_ref::<T>())
    }

    /// Get mutable reference to stored data
    pub fn get_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.data
            .get_mut(&TypeId::of::<T>())
            .and_then(|b| b.downcast_mut::<T>())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_context_is_empty() {
        let ctx = ExternalContext::new();
        assert!(ctx.get::<String>().is_none());
        assert!(ctx.get::<i32>().is_none());
    }

    #[test]
    fn test_default_context_is_empty() {
        let ctx = ExternalContext::default();
        assert!(ctx.get::<String>().is_none());
    }

    #[test]
    fn test_insert_and_get_primitive() {
        let mut ctx = ExternalContext::new();
        ctx.insert(42i32);

        assert_eq!(ctx.get::<i32>(), Some(&42));
        assert!(ctx.get::<i64>().is_none()); // Different type
    }

    #[test]
    fn test_insert_and_get_string() {
        let mut ctx = ExternalContext::new();
        ctx.insert(String::from("hello"));

        assert_eq!(ctx.get::<String>(), Some(&String::from("hello")));
    }

    #[test]
    fn test_insert_and_get_multiple_types() {
        let mut ctx = ExternalContext::new();
        ctx.insert(42i32);
        ctx.insert(String::from("world"));
        ctx.insert(3.14f64);

        assert_eq!(ctx.get::<i32>(), Some(&42));
        assert_eq!(ctx.get::<String>(), Some(&String::from("world")));
        assert_eq!(ctx.get::<f64>(), Some(&3.14));
    }

    #[test]
    fn test_insert_overwrites_existing_value() {
        let mut ctx = ExternalContext::new();
        ctx.insert(42i32);
        ctx.insert(100i32);

        assert_eq!(ctx.get::<i32>(), Some(&100));
    }

    #[test]
    fn test_get_mut_basic() {
        let mut ctx = ExternalContext::new();
        ctx.insert(42i32);

        if let Some(value) = ctx.get_mut::<i32>() {
            *value = 100;
        }

        assert_eq!(ctx.get::<i32>(), Some(&100));
    }

    #[test]
    fn test_get_mut_string() {
        let mut ctx = ExternalContext::new();
        ctx.insert(String::from("hello"));

        if let Some(s) = ctx.get_mut::<String>() {
            s.push_str(" world");
        }

        assert_eq!(ctx.get::<String>(), Some(&String::from("hello world")));
    }

    #[test]
    fn test_get_mut_nonexistent_returns_none() {
        let mut ctx = ExternalContext::new();
        assert!(ctx.get_mut::<i32>().is_none());
    }

    #[test]
    fn test_custom_struct() {
        #[derive(Debug, PartialEq)]
        struct CustomData {
            value: i32,
            name: String,
        }

        let mut ctx = ExternalContext::new();
        ctx.insert(CustomData {
            value: 42,
            name: String::from("test"),
        });

        let data = ctx.get::<CustomData>();
        assert_eq!(
            data,
            Some(&CustomData {
                value: 42,
                name: String::from("test")
            })
        );
    }

    #[test]
    fn test_custom_struct_mutation() {
        struct Counter {
            count: i32,
        }

        let mut ctx = ExternalContext::new();
        ctx.insert(Counter { count: 0 });

        // Increment counter multiple times
        for _ in 0..5 {
            if let Some(counter) = ctx.get_mut::<Counter>() {
                counter.count += 1;
            }
        }

        assert_eq!(ctx.get::<Counter>().map(|c| c.count), Some(5));
    }

    #[test]
    fn test_vec_storage() {
        let mut ctx = ExternalContext::new();
        ctx.insert(vec![1, 2, 3]);

        if let Some(vec) = ctx.get_mut::<Vec<i32>>() {
            vec.push(4);
            vec.push(5);
        }

        assert_eq!(ctx.get::<Vec<i32>>(), Some(&vec![1, 2, 3, 4, 5]));
    }

    #[test]
    fn test_option_storage() {
        let mut ctx = ExternalContext::new();
        ctx.insert(Some(42i32));

        assert_eq!(ctx.get::<Option<i32>>(), Some(&Some(42)));

        ctx.insert(None::<i32>);
        assert_eq!(ctx.get::<Option<i32>>(), Some(&None));
    }

    #[test]
    fn test_nested_types() {
        let mut ctx = ExternalContext::new();
        ctx.insert(vec![vec![1, 2], vec![3, 4]]);

        assert_eq!(
            ctx.get::<Vec<Vec<i32>>>(),
            Some(&vec![vec![1, 2], vec![3, 4]])
        );
    }

    #[test]
    fn test_type_safety() {
        let mut ctx = ExternalContext::new();
        ctx.insert(42u32);

        // Even though both are numeric, they're different types
        assert!(ctx.get::<i32>().is_none());
        assert_eq!(ctx.get::<u32>(), Some(&42));
    }
}
