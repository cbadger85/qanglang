use std::path::{Path, PathBuf};

use serde::Deserialize;

#[derive(Clone, Debug, Default, Deserialize)]
pub struct QangConfig {
    pub root: PathBuf,
}

impl QangConfig {
    pub fn new(path: &Path) -> Self {
        let root = if path.is_dir() {
            path.to_path_buf()
        } else {
            path.parent().unwrap_or(path).to_path_buf()
        };

        Self { root }
    }

    pub fn resolve(path: &Path) -> Self {
        if let Ok(config_content) = std::fs::read_to_string(path) {
            if let Ok(config) = toml::from_str::<Self>(&config_content) {
                return config;
            }
        }

        Self::new(path)
    }
}

pub fn find_config_path(path: PathBuf) -> Option<PathBuf> {
    let mut current = if path.is_file() {
        match path.parent() {
            Some(path) => path,
            None => path.as_path(),
        }
    } else {
        path.as_path()
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
