mod path = import("qang::path");
var Path = path.Path;

fn read_file(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }
  var contents = fs_read_file(path.to_string());
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

  return fs_write_file(path.to_string(), content) 
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
  
  return fs_append_file(path.to_string(), content) 
    ? Ok()
    : Err("Unable to append file.");
}

fn remove_file(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  return fs_remove_file(path.to_string(), content) 
    ? Ok()
    : Err("Unable to remove file.");
}

fn list(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  // TODO return an array of paths inside the current path
  return Ok([]);
}

fn create_dir(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  // TODO create a directory at the given path
  return Ok();
}

fn create_dirs(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }
  // TODO create a directory at the given path, including any required parent directories.
  return Ok();
}

fn remove_dir(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  // TODO removes an empty directory
  return Ok();
}


fn remove_all(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  // TODO removes all files and directories in the path recursively
  return Ok();
}

fn get_file_size(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  // TODO return the file size in bytes.
  return Ok(0);
}

fn get_file_modified_time(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  // TODO returns the epoch time of the last time the file was modified.
  return Ok(0);
}

fn copy_file(source, destination) {
  if (!(source is Path)) {
    return Err("source must be a valid Path.");
  }

  if (!(destination is Path)) {
    return Err("destination must be a valid Path.");
  }

  // TODO copy the file to the new path
  return Ok();
}

fn move(source, destination) {
  if (!(source is Path)) {
    return Err("source must be a valid Path.");
  }

  if (!(destination is Path)) {
    return Err("destination must be a valid Path.");
  }

  // TODO move file/directory from source to destination path
  return Ok();
}

fn current_dir() {
  var cwd = _get_common_working_dir();
  if (cwd == nil) {
    return nil;
  }
  return Path(cwd);
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

    var char = this._buffer.char_at(this._buffer_index);
    this._buffer_index += 1;
    return char;
  }

  _load_next_chunk() {
    var result = fs_read_chunk(this._path.to_string(), this._file_offset, this._chunk_size);

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