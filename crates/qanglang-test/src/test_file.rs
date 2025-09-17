use std::path::{Path, PathBuf};

/// Represents a source file with both its actual file location and how it should be displayed
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// The actual file path that can be used to read the file
    pub file_path: PathBuf,
    /// The path to display to the user (relative to their context)
    pub display_path: String,
}

impl SourceFile {
    pub fn new(file_path: PathBuf, display_path: String) -> Self {
        Self {
            file_path,
            display_path,
        }
    }
}

pub struct SourceFileResolver {
    user_working_dir: PathBuf,
}

// TODO add support for ignore patterns when resolving a dir of files.

impl SourceFileResolver {
    pub fn new() -> std::io::Result<Self> {
        let user_working_dir = std::env::current_dir()?;
        Ok(Self { user_working_dir })
    }

    /// Resolve a single file or directory path into source files
    pub fn resolve(&self, user_input: &str) -> std::io::Result<Vec<SourceFile>> {
        let input_path = Path::new(user_input);

        // Convert to absolute path for file operations
        let absolute_path = if input_path.is_absolute() {
            input_path.to_path_buf()
        } else {
            self.user_working_dir.join(input_path)
        };

        // Canonicalize to resolve any .. components
        let canonical_path = absolute_path
            .canonicalize()
            .unwrap_or_else(|_| absolute_path.clone());

        if canonical_path.is_file() {
            // Single file case
            self.resolve_single_file(&canonical_path)
        } else if canonical_path.is_dir() {
            // Directory case - find all .ql files
            self.resolve_directory(&canonical_path)
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Path '{}' does not exist", user_input),
            ))
        }
    }

    fn resolve_single_file(&self, absolute_path: &Path) -> std::io::Result<Vec<SourceFile>> {
        // For single files, use a clean display path
        let display_path = self.create_display_path(absolute_path);

        Ok(vec![SourceFile::new(
            absolute_path.to_path_buf(),
            display_path,
        )])
    }

    fn resolve_directory(&self, absolute_path: &Path) -> std::io::Result<Vec<SourceFile>> {
        let mut source_files = Vec::new();
        self.collect_ql_files(absolute_path, &mut source_files)?;

        // Sort for consistent output
        source_files.sort_by(|a, b| a.display_path.cmp(&b.display_path));

        Ok(source_files)
    }

    fn collect_ql_files(&self, dir: &Path, files: &mut Vec<SourceFile>) -> std::io::Result<()> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                self.collect_ql_files(&path, files)?;
            } else if path.extension().and_then(|s| s.to_str()) == Some("ql") {
                let display_path = self.create_display_path(&path);
                files.push(SourceFile::new(path, display_path));
            }
        }
        Ok(())
    }

    fn create_display_path(&self, absolute_path: &Path) -> String {
        // Canonicalize the working directory to match the format of absolute_path
        let canonical_work_dir = self
            .user_working_dir
            .canonicalize()
            .unwrap_or_else(|_| self.user_working_dir.clone());

        // Try to create a nice relative path from the user's working directory
        if let Ok(relative) = absolute_path.strip_prefix(&canonical_work_dir) {
            relative.to_string_lossy().to_string()
        } else {
            // If we can't make it relative, find a sensible common ancestor
            self.find_best_display_path(absolute_path)
        }
    }

    fn find_best_display_path(&self, absolute_path: &Path) -> String {
        // Find common path components between the file and user's working directory
        let file_components: Vec<_> = absolute_path.components().collect();
        let work_components: Vec<_> = self.user_working_dir.components().collect();

        // Find the longest common prefix
        let mut common_len = 0;
        for (file_comp, work_comp) in file_components.iter().zip(work_components.iter()) {
            if file_comp == work_comp {
                common_len += 1;
            } else {
                break;
            }
        }

        if common_len > 0 {
            // Build path from common root (usually the project root)
            let remaining_components = &file_components[common_len..];
            let mut result = PathBuf::new();
            for component in remaining_components {
                result.push(component);
            }
            result.to_string_lossy().to_string()
        } else {
            // Last resort: just use the filename
            absolute_path
                .file_name()
                .unwrap_or(absolute_path.as_os_str())
                .to_string_lossy()
                .to_string()
        }
    }
}
