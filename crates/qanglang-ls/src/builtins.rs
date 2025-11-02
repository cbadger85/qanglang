use once_cell::sync::Lazy;
use std::collections::HashMap;

/// Information about a native function
#[derive(Debug, Clone, Copy)]
pub struct NativeFunctionInfo {
    pub name: &'static str,
}

/// Static registry of all native functions
static NATIVE_FUNCTIONS: Lazy<HashMap<&'static str, NativeFunctionInfo>> = Lazy::new(|| {
    let mut map = HashMap::new();

    // Global native functions
    map.insert("print", NativeFunctionInfo { name: "print" });
    map.insert("println", NativeFunctionInfo { name: "println" });
    map.insert("assert", NativeFunctionInfo { name: "assert" });
    map.insert("assert_eq", NativeFunctionInfo { name: "assert_eq" });
    map.insert(
        "assert_throws",
        NativeFunctionInfo {
            name: "assert_throws",
        },
    );
    map.insert("typeof", NativeFunctionInfo { name: "typeof" });
    map.insert("to_string", NativeFunctionInfo { name: "to_string" });
    map.insert(
        "system_time",
        NativeFunctionInfo {
            name: "system_time",
        },
    );
    map.insert("hash", NativeFunctionInfo { name: "hash" });
    map.insert("env_cwd", NativeFunctionInfo { name: "env_cwd" });
    map.insert("object_get", NativeFunctionInfo { name: "object_get" });
    map.insert(
        "object_assign",
        NativeFunctionInfo {
            name: "object_assign",
        },
    );
    map.insert("object_set", NativeFunctionInfo { name: "object_set" });
    map.insert(
        "object_to_entries",
        NativeFunctionInfo {
            name: "object_to_entries",
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
