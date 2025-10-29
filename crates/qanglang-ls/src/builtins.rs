use once_cell::sync::Lazy;
use std::collections::HashMap;

/// Information about a native function
#[derive(Debug, Clone, Copy)]
pub struct NativeFunctionInfo {
    pub name: &'static str,
    #[allow(dead_code)]
    pub parameters: &'static [&'static str],
}

/// Information about an intrinsic method (methods on built-in types)
#[derive(Debug, Clone, Copy)]
pub struct IntrinsicMethodInfo {
    pub name: &'static str,
    #[allow(dead_code)]
    pub parameters: &'static [&'static str],
}

/// Static registry of all native functions
static NATIVE_FUNCTIONS: Lazy<HashMap<&'static str, NativeFunctionInfo>> = Lazy::new(|| {
    let mut map = HashMap::new();

    // Global native functions
    map.insert(
        "print",
        NativeFunctionInfo {
            name: "print",
            parameters: &["value"],
        },
    );
    map.insert(
        "println",
        NativeFunctionInfo {
            name: "println",
            parameters: &["value"],
        },
    );
    map.insert(
        "assert",
        NativeFunctionInfo {
            name: "assert",
            parameters: &["condition", "message"],
        },
    );
    map.insert(
        "assert_eq",
        NativeFunctionInfo {
            name: "assert_eq",
            parameters: &["a", "b", "message"],
        },
    );
    map.insert(
        "assert_throws",
        NativeFunctionInfo {
            name: "assert_throws",
            parameters: &["function", "message"],
        },
    );
    map.insert(
        "typeof",
        NativeFunctionInfo {
            name: "typeof",
            parameters: &["value"],
        },
    );
    map.insert(
        "to_string",
        NativeFunctionInfo {
            name: "to_string",
            parameters: &["value"],
        },
    );
    map.insert(
        "system_time",
        NativeFunctionInfo {
            name: "system_time",
            parameters: &[],
        },
    );
    map.insert(
        "hash",
        NativeFunctionInfo {
            name: "hash",
            parameters: &["value"],
        },
    );
    map.insert(
        "array_of_length",
        NativeFunctionInfo {
            name: "array_of_length",
            parameters: &["length"],
        },
    );

    map
});

/// Static registry of String intrinsic methods
static STRING_METHODS: Lazy<HashMap<&'static str, IntrinsicMethodInfo>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert(
        "to_uppercase",
        IntrinsicMethodInfo {
            name: "to_uppercase",
            parameters: &[],
        },
    );
    map.insert(
        "to_lowercase",
        IntrinsicMethodInfo {
            name: "to_lowercase",
            parameters: &[],
        },
    );
    map.insert(
        "to_number",
        IntrinsicMethodInfo {
            name: "to_number",
            parameters: &[],
        },
    );
    map.insert(
        "concat",
        IntrinsicMethodInfo {
            name: "concat",
            parameters: &["other"],
        },
    );
    map.insert(
        "split",
        IntrinsicMethodInfo {
            name: "split",
            parameters: &["delimiter"],
        },
    );

    map
});

/// Static registry of Array intrinsic methods
static ARRAY_METHODS: Lazy<HashMap<&'static str, IntrinsicMethodInfo>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert(
        "length",
        IntrinsicMethodInfo {
            name: "length",
            parameters: &[],
        },
    );
    map.insert(
        "push",
        IntrinsicMethodInfo {
            name: "push",
            parameters: &["element"],
        },
    );
    map.insert(
        "pop",
        IntrinsicMethodInfo {
            name: "pop",
            parameters: &[],
        },
    );
    map.insert(
        "reverse",
        IntrinsicMethodInfo {
            name: "reverse",
            parameters: &[],
        },
    );
    map.insert(
        "slice",
        IntrinsicMethodInfo {
            name: "slice",
            parameters: &["start", "end"],
        },
    );
    map.insert(
        "get",
        IntrinsicMethodInfo {
            name: "get",
            parameters: &["index"],
        },
    );
    map.insert(
        "concat",
        IntrinsicMethodInfo {
            name: "concat",
            parameters: &["other"],
        },
    );
    map.insert(
        "remove_at",
        IntrinsicMethodInfo {
            name: "remove_at",
            parameters: &["index"],
        },
    );
    map.insert(
        "index_of",
        IntrinsicMethodInfo {
            name: "index_of",
            parameters: &["value"],
        },
    );
    map.insert(
        "contains",
        IntrinsicMethodInfo {
            name: "contains",
            parameters: &["value"],
        },
    );
    map.insert(
        "join",
        IntrinsicMethodInfo {
            name: "join",
            parameters: &["delimiter"],
        },
    );
    map.insert(
        "iter",
        IntrinsicMethodInfo {
            name: "iter",
            parameters: &[],
        },
    );

    map
});

/// Static registry of Number intrinsic methods
static NUMBER_METHODS: Lazy<HashMap<&'static str, IntrinsicMethodInfo>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert(
        "ceil",
        IntrinsicMethodInfo {
            name: "ceil",
            parameters: &[],
        },
    );
    map.insert(
        "floor",
        IntrinsicMethodInfo {
            name: "floor",
            parameters: &[],
        },
    );
    map.insert(
        "trunc",
        IntrinsicMethodInfo {
            name: "trunc",
            parameters: &[],
        },
    );
    map.insert(
        "min",
        IntrinsicMethodInfo {
            name: "min",
            parameters: &["value"],
        },
    );
    map.insert(
        "max",
        IntrinsicMethodInfo {
            name: "max",
            parameters: &["value"],
        },
    );

    map
});

/// Static registry of Function intrinsic methods
static FUNCTION_METHODS: Lazy<HashMap<&'static str, IntrinsicMethodInfo>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert(
        "call",
        IntrinsicMethodInfo {
            name: "call",
            parameters: &[],
        },
    );
    map.insert(
        "apply",
        IntrinsicMethodInfo {
            name: "apply",
            parameters: &["args"],
        },
    );

    map
});

/// Get information about a native function by name
pub fn get_native_function(name: &str) -> Option<&'static NativeFunctionInfo> {
    NATIVE_FUNCTIONS.get(name)
}

/// Get all native function names
pub fn get_all_native_functions() -> impl Iterator<Item = NativeFunctionInfo> {
    NATIVE_FUNCTIONS.values().copied()
}

/// Get information about a String intrinsic method by name
pub fn get_string_method(name: &str) -> Option<&'static IntrinsicMethodInfo> {
    STRING_METHODS.get(name)
}

/// Get all String intrinsic method names
pub fn get_all_string_methods() -> impl Iterator<Item = IntrinsicMethodInfo> {
    STRING_METHODS.values().copied()
}

/// Get information about an Array intrinsic method by name
pub fn get_array_method(name: &str) -> Option<&'static IntrinsicMethodInfo> {
    ARRAY_METHODS.get(name)
}

/// Get all Array intrinsic method names
pub fn get_all_array_methods() -> impl Iterator<Item = IntrinsicMethodInfo> {
    ARRAY_METHODS.values().copied()
}

/// Get information about a Number intrinsic method by name
pub fn get_number_method(name: &str) -> Option<&'static IntrinsicMethodInfo> {
    NUMBER_METHODS.get(name)
}

/// Get all Number intrinsic method names
pub fn get_all_number_methods() -> impl Iterator<Item = IntrinsicMethodInfo> {
    NUMBER_METHODS.values().copied()
}

/// Get information about a Function intrinsic method by name
pub fn get_function_method(name: &str) -> Option<&'static IntrinsicMethodInfo> {
    FUNCTION_METHODS.get(name)
}

/// Get all Function intrinsic method names
pub fn get_all_function_methods() -> impl Iterator<Item = IntrinsicMethodInfo> {
    FUNCTION_METHODS.values().copied()
}
