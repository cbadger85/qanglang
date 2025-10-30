/// Built-in module registry for the Qang language
///
/// Built-in modules are accessed via the `qang::` namespace, e.g., `import("qang::files")`

/// The source code of the files builtin module
pub const FILES_SOURCE: &str = include_str!("files.ql");

/// The namespace prefix for built-in modules
pub const BUILTIN_NAMESPACE: &str = "qang::";

/// Checks if an import path is a built-in module reference
pub fn is_builtin_import(path: &str) -> bool {
    path.starts_with(BUILTIN_NAMESPACE)
}

/// Extracts the module name from a built-in import path
/// e.g., "qang::files" -> "files"
pub fn extract_builtin_name(path: &str) -> Option<&str> {
    if is_builtin_import(path) {
        Some(&path[BUILTIN_NAMESPACE.len()..])
    } else {
        None
    }
}

/// Resolves a built-in module name to its source code
pub fn get_builtin_source(module_name: &str) -> Option<&'static str> {
    match module_name {
        "files" => Some(FILES_SOURCE),
        _ => None,
    }
}

/// Resolves a built-in import path to its source code
/// e.g., "qang::files" -> Some(FILES_SOURCE)
pub fn resolve_builtin_import(path: &str) -> Option<&'static str> {
    extract_builtin_name(path).and_then(get_builtin_source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_builtin_import() {
        assert!(is_builtin_import("qang::files"));
        assert!(is_builtin_import("qang::other"));
        assert!(!is_builtin_import("./module.ql"));
        assert!(!is_builtin_import("../other.ql"));
    }

    #[test]
    fn test_extract_builtin_name() {
        assert_eq!(extract_builtin_name("qang::files"), Some("files"));
        assert_eq!(extract_builtin_name("qang::other"), Some("other"));
        assert_eq!(extract_builtin_name("./module.ql"), None);
    }

    #[test]
    fn test_resolve_builtin_import() {
        assert!(resolve_builtin_import("qang::files").is_some());
        assert_eq!(resolve_builtin_import("qang::files"), Some(FILES_SOURCE));
        assert!(resolve_builtin_import("qang::nonexistent").is_none());
        assert!(resolve_builtin_import("./module.ql").is_none());
    }
}
