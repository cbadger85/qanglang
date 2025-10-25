use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct SourceMap {
    source: Vec<char>,
    line_indices: Vec<usize>,
    path: PathBuf,
}

impl SourceMap {
    pub fn new(source: String, path: PathBuf) -> Self {
        let (chars, line_indices) = Self::load_source(source);

        Self {
            path,
            source: chars,
            line_indices,
        }
    }

    pub fn from_source(source: String) -> Self {
        let (chars, line_indices) = Self::load_source(source);

        Self {
            path: PathBuf::new(),
            source: chars,
            line_indices,
        }
    }

    pub fn from_path(path: &Path) -> std::io::Result<Self> {
        let source = std::fs::read_to_string(path)?;
        let (chars, line_indices) = Self::load_source(source);
        Ok(Self {
            path: path.to_path_buf(),
            line_indices,
            source: chars,
        })
    }

    fn load_source(source: String) -> (Vec<char>, Vec<usize>) {
        let chars: Vec<char> = source.chars().collect();
        let mut line_indices = Vec::new();
        let mut in_string = false;
        let mut i = 0;

        while i < chars.len() {
            match chars[i] {
                '"' if !in_string => {
                    in_string = true;
                }
                '"' if in_string => {
                    in_string = false;
                }
                '\\' if in_string => {
                    // Skip escape sequence
                    i += 1; // Skip the backslash
                    if i < chars.len() {
                        i += 1; // Skip the escaped character
                        continue;
                    }
                }
                '\n' if !in_string => {
                    line_indices.push(i);
                }
                _ => {}
            }
            i += 1;
        }

        (chars, line_indices)
    }

    pub fn get_path(&self) -> &Path {
        self.path.as_path()
    }

    pub fn get_source(&self) -> &Vec<char> {
        &self.source
    }

    pub fn get_line(&self, line_number: u32) -> &[char] {
        if line_number == 0 {
            return &[];
        }

        let line_index = (line_number - 1) as usize;

        let start = if line_number == 1 {
            0
        } else {
            // Start after the previous newline
            if let Some(&prev_newline_pos) = self.line_indices.get(line_index - 1) {
                prev_newline_pos + 1
            } else {
                return &[];
            }
        };

        let end = if let Some(&newline_pos) = self.line_indices.get(line_index) {
            newline_pos // Stop at the newline (don't include it)
        } else if line_index == self.line_indices.len() {
            // Last line (no trailing newline)
            self.source.len()
        } else {
            // Line doesn't exist
            return &[];
        };

        if start <= end && end <= self.source.len() {
            &self.source[start..end]
        } else {
            &[]
        }
    }

    pub fn get_line_number(&self, position: usize) -> u32 {
        if position >= self.source.len() {
            return (self.line_indices.len() + 1) as u32;
        }

        // Binary search to find which line this position belongs to
        match self.line_indices.binary_search(&position) {
            Ok(index) => (index + 1) as u32,  // Exact match on newline
            Err(index) => (index + 1) as u32, // Position is between line_indices[index-1] and line_indices[index]
        }
    }

    pub fn get_column_number(&self, position: usize) -> u32 {
        if position >= self.source.len() {
            return 1;
        }
        let line_number = self.get_line_number(position);
        let line_start = if line_number == 1 {
            0
        } else {
            let line_index = (line_number - 2) as usize;
            if let Some(&newline_pos) = self.line_indices.get(line_index) {
                newline_pos + 1
            } else {
                0
            }
        };
        (position - line_start + 1) as u32
    }

    /// Convert LSP position (0-based line/column) to byte offset in source
    pub fn position_to_offset(&self, line: u32, column: u32) -> Option<usize> {
        // LSP uses 0-based indexing
        let target_line = line as usize;
        let target_col = column as usize;

        // Calculate the start of the target line
        let line_start = if target_line == 0 {
            0
        } else {
            // Get the position after the newline of the previous line
            if let Some(&newline_pos) = self.line_indices.get(target_line - 1) {
                newline_pos + 1
            } else {
                // Line doesn't exist
                return None;
            }
        };

        // Calculate the offset by adding the column to the line start
        let offset = line_start + target_col;

        // Ensure offset is within bounds
        if offset <= self.source.len() {
            Some(offset)
        } else {
            None
        }
    }
}
