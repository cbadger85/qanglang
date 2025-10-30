fn read_file(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }
  
  // TODO return the file as a single string
  return Ok("");
}

fn read_lines(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }
  
  // TODO return the file as an array of strings

  return Ok([]);
}

fn write_file(path, content) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  if (!(content is STRING)) {
    return Err("A valid Path must be provided.");
  }

  // TODO save the file to disk
  return Ok();
}

fn append_file(path, content) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }
  
  // TODO verify file exists
  
  if (!(content is STRING)) {
    return Err("A valid Path must be provided.");
  }
  
  // TODO append data to end of existing file
  return Ok();
}

fn remove_file(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }

  // TODO removes a file at a given directory
  return Ok();
}

fn list_dir(path) {
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
  // TODO return the current working directory as a Path
  return Path("");
}

fn change_dir(path) {
  if (!(path is Path)) {
    return Err("A valid Path must be provided.");
  }
  
  // TODO change the working directory
  return Ok();
}

class Path {
  init(path) {
    assert(path is STRING, "Expected path to be a string.");
    this._path = path;
  }

  join(other) {
    // should this also allow strings?
    assert(other is Path, "A valid Path must be provided.");
    // TODO join the paths and return a new Path
    /*
      - Concatenate the paths with the OS-appropriate separator (/ or \)
      - Handle edge cases like empty paths, trailing slashes, absolute paths being joined
      - You'll need one native function to get the OS path separator
    */
  }

  normalize() {
    // TODO resolve `.` and `..`, fix slashes and return a new Path
    /*
    - Check if already absolute (starts with / or drive letter on Windows)st
      - If not, prepend current_dir() and then call normalize()
    */
  }

  absolute() {
    // TODO convert path to absolute path and return new Path
  }

  dirname() {
    // TODO return the directory portion of the path as a new Path
  }

  basename() {
    // TODO return the filename portion of the path as a string. `nil` if not a filemane.
  }

  extension() {
    // TODO return the extension of the filename as a string. `nil` if no filename or extension.
  }

  exists() {
    // TODO returns true if the path exists, false otherwise.
  }

  is_file() {
    // TODO return true if the path points toa file, false otherwise
  }

  is_dir() {
    // TODO return true if th epath is a dir, false otherwise
  }

  to_string() {
    return this._path;
  }
}

