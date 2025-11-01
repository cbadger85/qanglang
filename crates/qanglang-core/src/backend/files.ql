mod path = import("qang::path");
var Path = path.Path;

fn read_file(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }
  var contents = _fs_read_file(path.to_string());
  return Ok(contents);
}

fn read_chars(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return Ok(FileIterator(path));
}

fn write_file(path, content) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  if (!(content is STRING)) {
    return Err("A valid Path must be provided.");
  }

  return _fs_write_file(path.to_string(), content) 
    ? Ok()
    : Err("Unable to save file.");
}

fn append_file(path, content) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }
  
  if (!path.exists()) {
    return Err("File not found.");
  }
  
  if (!(content is STRING)) {
    return Err("A valid Path must be provided.");
  }
  
  return _fs_append_file(path.to_string(), content) 
    ? Ok()
    : Err("Unable to append file.");
}

fn remove_file(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return _fs_remove_file(path.to_string())
    ? Ok()
    : Err("Unable to remove file.");
}

fn list(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return _fs_list(path.to_string())
    ?|entries -> Ok(entries)| 
    or Err("Unable to read directory.");
}

fn create_dir(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return _fs_create_dir(path.to_string()) 
    ? Ok()
    : Err("Unable to create directory.");
}

fn create_dirs(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return _fs_create_dirs(path.to_string()) 
    ? Ok()
    : Err("Unable to create directory(s).");
}

fn remove_dir(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return _fs_remove_dir(path.to_string()) 
    ? Ok()
    : Err("Unable to remove directory.");
}


fn remove_all(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return _fs_remove_all(path.to_string()) 
    ? Ok()
    : Err("Unable to remove directory and contents.");
}

fn get_file_size(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return _fs_get_file_size(path.to_string())
    ?|size -> Ok(size)| 
    or Err("Unable to get file metadata.");
}
  
fn get_file_modified_time(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return _fs_get_file_modified_time(path.to_string())
    ?|size -> Ok(size)| 
    or Err("Unable to get file metadata.");
}

fn copy_file(source, destination) {
  if (!(source is Path)) {
    return Err("`source` must be a valid Path.");
  }

  if (!source.is_file()) {
    return Err("`source` must be a file.");
  }

  if (!(destination is Path)) {
    return Err("`destination` must be a valid Path.");
  }

  return _fs_copy_file(source.to_string(), destination.to_string())
    ? Ok()
    : Err("Unable to copy file.");
}

fn move(source, destination) {
  if (!(source is Path)) {
    return Err("source must be a valid Path.");
  }

  if (!(destination is Path)) {
    return Err("destination must be a valid Path.");
  }

  return _fs_move(source.to_string(), destination.to_string()) 
    ? Ok()
    : Err("Unable to move " + (source.is_file() ? "file" : "directory") + ".");
}

class FileIterator : Iterator {
  init(path) {
    assert(path is Path, "`path` must be an instance of Path.");
    this._path = path;
    this._buffer = "";
    this._buffer_index = 0;
    this._file_offset = 0;
    this._chunk_size = 4096;  // Read 4KB at a time
    this._eof = false;
  }

  has_next() {
    // If we have characters left in the buffer, we have next
    if (this._buffer_index < this._buffer.length()) {
      return true;
    }

    // If we've reached EOF, no more data
    if (this._eof) {
      return false;
    }

    // Try to load next chunk
    return this._load_next_chunk();
  }

  next() {
    if (!this.has_next()) {
      return nil;
    }

    var chars = this._buffer.split("");
    var char = chars[this._buffer_index];
    this._buffer_index += 1;
    return char;
  }

  _load_next_chunk() {
    var result = _fs_read_chunk(this._path.to_string(), this._file_offset, this._chunk_size);

    if (result == nil) {
      this._eof = true;
      return false;
    }

    var chunk = result.content;
    var bytes_read = result.bytes_read;

    if (chunk.length() == 0) {
      this._eof = true;
      return false;
    }

    // Update state - use actual bytes read, not chunk length
    this._buffer = chunk;
    this._buffer_index = 0;
    this._file_offset += bytes_read;

    return true;
  }
}