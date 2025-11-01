var OS_PATH_SEPARATOR = _path_get_os_seperator();

class Path {
  init(path) {
    assert(path is STRING, "Expected path to be a string.");
    this._path = path;
  }

  join(other) {
    // should this also allow strings?
    assert(other is Path, "A valid Path must be provided.");

    var other_path = other.to_string();

    // If other is absolute, just return other
    if (other.is_absolute()) {
      return Path(other_path);
    }

    // If this path is empty, return other
    if (this._path.length() == 0) {
      return Path(other_path);
    }

    // If other is empty, return this
    if (other_path.length() == 0) {
      return Path(this._path);
    }

    // Check if this path already ends with separator
    var has_trailing_sep = this._path.ends_with(OS_PATH_SEPARATOR);

    // Check if other path starts with separator
    var other_has_leading_sep = other_path.starts_with(OS_PATH_SEPARATOR);

    // Build the joined path
    var joined = when {
      has_trailing_sep and other_has_leading_sep => this._path + other_path.split("").slice(1).join(""),
      has_trailing_sep or other_has_leading_sep => this._path + other_path,
      else => this._path + OS_PATH_SEPARATOR + other_path
    };

    return Path(joined);
  }

  normalize() {
    // Split path by separators (handle both / and \)
    var parts = this._path.split(OS_PATH_SEPARATOR);
    var normalized_parts = [];

    // Process each part
    for (var i = 0; i < parts.length(); i += 1) {
      var part = parts[i];

      if (part == "." or part == "") {
        // Skip current directory markers and empty parts (except for first if absolute)
        if (i == 0 and part == "") {
          normalized_parts.push(part); // Keep leading slash for absolute paths
        }
        // Skip "." entries
      } else if (part == "..") {
        // Go up one directory if possible
        if (normalized_parts.length() > 0) {
          var last = normalized_parts.get(-1);
          // Don't pop if we're at root or if last is also ".."
          if (last != "" and last != "..") {
            normalized_parts.pop();
          } else {
            normalized_parts.push(part);
          }
        } else {
          normalized_parts.push(part);
        }
      } else {
        normalized_parts.push(part);
      }
    }

    // Join the normalized parts
    var normalized = normalized_parts.join(OS_PATH_SEPARATOR);

    // Handle empty result
    if (normalized == "") {
      normalized = ".";
    }

    return Path(normalized);
  }

  absolute() {
    // If already absolute, normalize and return
    if (this.is_absolute()) {
      return this.normalize();
    }

    // Get current working directory and join with this path, then normalize
    var cwd = env_cwd();
    if (cwd == nil) {
      // If we can't get cwd, just return normalized version
      return this.normalize();
    }

    return Path(cwd).join(this).normalize();
  }

  dirname() {
    // Split by path separator
    var parts = this._path.split(OS_PATH_SEPARATOR);

    // If only one part (no separator), return "."
    if (parts.length() <= 1) {
      return Path(".");
    }

    // Remove the last part (filename)
    parts.pop();

    // Join remaining parts
    var dir = parts.join(OS_PATH_SEPARATOR);

    // Handle empty result (when path was like "/file")
    if (dir == "") {
      // If original path started with separator, return root
      if (this._path.starts_with(OS_PATH_SEPARATOR)) {
        return Path(OS_PATH_SEPARATOR);
      }
      return Path(".");
    }

    return Path(dir);
  }

  basename() {
    // Handle empty path
    if (this._path == "") {
      return nil;
    }

    // Remove trailing separators
    var path = this._path;
    while (path.ends_with(OS_PATH_SEPARATOR) and path.length() > 1) {
      path = path.split("").slice(0, -1).join("");
    }

    // If path is just separator(s), return nil
    if (path == OS_PATH_SEPARATOR or path == "") {
      return nil;
    }

    // Split by path separator and get last part
    var parts = path.split(OS_PATH_SEPARATOR);
    var last_part = parts.get(-1);

    // Return nil if empty or if it's "." or ".."
    if (last_part == "" or last_part == "." or last_part == "..") {
      return nil;
    }

    return last_part;
  }

  extension() {
    return this.basename()?.split(".")?.get(-1);
  }

  exists() {
    return _path_exists(this._path);
  }

  is_absolute() {
    return _path_is_absolute(this._path);
  }

  is_relative() {
    return _path_is_relative(this._path);
  }

  is_file() {
    return _path_is_file(this._path);
  }

  is_dir() {
    return _path_is_dir(this._path);
  }

  to_string() {
    return this._path;
  }
}
