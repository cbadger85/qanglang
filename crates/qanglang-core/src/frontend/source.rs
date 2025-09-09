use std::{path::PathBuf, sync::Arc};

use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{NodeId, StringHandle};

#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    pub name: String,
    source: Vec<char>,
    line_indices: Vec<usize>,
}

impl SourceMap {
    pub fn new(source: String) -> Self {
        Self::new_with_name("(script)", source)
    }

    pub fn from_path(name: &str, path: PathBuf) -> std::io::Result<Self> {
        let source = std::fs::read_to_string(path)?;
        Ok(Self::new_with_name(&name, source))
    }

    pub fn new_with_name(name: &str, source: String) -> Self {
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

        Self {
            name: name.to_string(),
            source: chars,
            line_indices,
        }
    }

    /// Returns the source code as a vector of characters.
    pub fn get_source(&self) -> &Vec<char> {
        &self.source
    }

    /// Returns a specified line of source code as a vector of characters.
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

    /// Returns the line number (1-based) for a given position in the source
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

    /// Returns the column number (1-based) for a given position in the source
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
}

pub struct ModuleSource {
    pub module_id: NodeId,
    pub source_map: Arc<SourceMap>,
}

pub struct ModuleMap {
    modules: FxHashMap<StringHandle, ModuleSource>,
    main_name: StringHandle,
}

impl ModuleMap {
    pub fn new(main_name: StringHandle, main_id: NodeId, source_map: Arc<SourceMap>) -> Self {
        let mut modules = FxHashMap::with_hasher(FxBuildHasher);
        modules.insert(
            main_name,
            ModuleSource {
                module_id: main_id,
                source_map,
            },
        );

        Self { main_name, modules }
    }

    pub fn get_main(&self) -> &ModuleSource {
        &self.modules[&self.main_name]
    }

    pub fn get(&self, name: StringHandle) -> &ModuleSource {
        &self.modules[&name]
    }

    pub fn insert(&mut self, name: StringHandle, module_id: NodeId, source_map: Arc<SourceMap>) {
        let _ = self.modules.insert(
            name,
            ModuleSource {
                module_id,
                source_map,
            },
        );
    }

    pub fn has(&self, name: StringHandle) -> bool {
        self.modules.contains_key(&name)
    }
}
