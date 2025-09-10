use std::path::{Path, PathBuf};

use serde::Deserialize;

#[derive(Clone, Debug, Default, Deserialize)]
pub struct QangConfig {
    pub root: PathBuf,
    pub entry_points: Vec<PathBuf>,
}

impl QangConfig {
    pub fn resolve(path: &Path) -> Self {
        if let Ok(config_content) = std::fs::read_to_string(path) {
            if let Ok(config) = toml::from_str::<Self>(&config_content) {
                return config;
            }
        }

        return Self::default();
    }
}

pub fn find_config_path() -> Option<PathBuf> {
    let start_path: PathBuf = std::env::current_dir().unwrap_or_default();
    let mut current = if start_path.is_file() {
        start_path.parent().unwrap_or(start_path.as_path())
    } else {
        start_path.as_path()
    };

    loop {
        let config_path = current.join("qang.toml");
        if config_path.exists() {
            return Some(config_path);
        }

        if let Some(parent) = current.parent() {
            current = parent;
        } else {
            return None;
        }
    }
}
