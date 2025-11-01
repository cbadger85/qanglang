use crate::{NativeFunctionError, Value, ValueKind, Vm};
use std::{
    fs::{self, OpenOptions},
    io::{Read, Seek, SeekFrom, Write},
    path::Path,
    time::UNIX_EPOCH,
};

pub fn qang_path_get_os_seperator(
    _arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let handle = vm.alloc.strings.intern(std::path::MAIN_SEPARATOR_STR);
    Ok(Some(Value::string(handle)))
}

pub fn qang_path_is_file(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(arg_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(path.is_file())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_path_exists(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(arg_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(path.exists())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_path_is_absolute(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(arg_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(path.is_absolute())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_path_is_relative(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(arg_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(path.is_relative())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_path_is_dir(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(arg_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(path.is_dir())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_read_file(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(args_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            let contents =
                fs::read(path).map_err(|_| NativeFunctionError::new("Unable to read file."))?;
            let contents_str = std::str::from_utf8(&contents)
                .map_err(|_| NativeFunctionError::new("Unable to convert file to string"))?;
            let string_handle = vm.alloc.strings.intern(contents_str);
            Ok(Some(Value::string(string_handle)))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

/// Reads a chunk of the file starting at the given byte offset.
/// Args: (path: string, offset: number, chunk_size: number)
/// Returns: object { content: string, bytes_read: number } or nil if EOF
pub fn qang_fs_read_chunk(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(args_count);
    let path = args.first().copied().unwrap_or_else(|| Value::nil()).kind();
    let offset = args.get(1).copied().unwrap_or_else(|| Value::nil()).kind();
    let chunk_size = args.get(2).copied().unwrap_or_else(|| Value::nil()).kind();

    match (path, offset, chunk_size) {
        (
            ValueKind::String(path_handle),
            ValueKind::Number(offset),
            ValueKind::Number(chunk_size),
        ) => {
            let path = vm.alloc.strings.get(path_handle);
            let path = Path::new(path);

            let offset = offset.max(0.0) as u64;
            let chunk_size = chunk_size.max(0.0) as usize;

            // Open file and seek to offset
            let mut file = fs::File::open(path)
                .map_err(|_| NativeFunctionError::new("Unable to open file."))?;

            file.seek(SeekFrom::Start(offset))
                .map_err(|_| NativeFunctionError::new("Unable to seek in file."))?;

            // Read chunk_size bytes from current position
            let mut buffer = vec![0u8; chunk_size];
            let bytes_read = file
                .read(&mut buffer)
                .map_err(|_| NativeFunctionError::new("Unable to read from file."))?;

            // If we read 0 bytes, we're at EOF
            if bytes_read == 0 {
                return Ok(Some(Value::nil()));
            }

            // Use only the bytes we actually read
            let chunk = &buffer[..bytes_read];

            // Convert to string, handling UTF-8 boundaries
            // If we're in the middle of a UTF-8 character, adjust the slice
            let (chunk_str, actual_bytes_read) = match std::str::from_utf8(chunk) {
                Ok(s) => (s, chunk.len()),
                Err(e) => {
                    // Try to recover by finding a valid UTF-8 boundary
                    let valid_up_to = e.valid_up_to();
                    let valid_chunk = &chunk[..valid_up_to];
                    let s = std::str::from_utf8(valid_chunk).map_err(|_| {
                        NativeFunctionError::new("Unable to convert chunk to string")
                    })?;
                    (s, valid_up_to)
                }
            };

            // Create an object literal with both the content and the actual bytes read
            let string_handle = vm.alloc.strings.intern(chunk_str);
            let content_key = vm.alloc.strings.intern("content");
            let bytes_key = vm.alloc.strings.intern("bytes_read");

            let table_handle = vm.alloc.tables.new_hashmap();
            vm.alloc.tables.insert(
                table_handle,
                Value::string(content_key),
                Value::string(string_handle),
            );
            vm.alloc.tables.insert(
                table_handle,
                Value::string(bytes_key),
                Value::number(actual_bytes_read as f64),
            );

            Ok(Some(Value::object_literal(table_handle)))
        }
        (ValueKind::String(_), _, _) => Err(NativeFunctionError::new(
            "offset and chunk_size must be numbers",
        )),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_write_file(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(args_count);
    let path = args.first().copied().unwrap_or_else(|| Value::nil()).kind();
    let data = args.get(1).copied().unwrap_or_else(|| Value::nil()).kind();

    match (path, data) {
        (ValueKind::String(path_handle), ValueKind::String(data_handle)) => {
            let path = vm.alloc.strings.get(path_handle);
            let path = Path::new(path);
            let data: &str = vm.alloc.strings.get(data_handle);

            Ok(Some(Value::boolean(fs::write(path, data).is_ok())))
        }
        (ValueKind::String(_), _) => Err(NativeFunctionError::new(
            "Can only write strings to file system.",
        )),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_remove_file(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(args_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(fs::remove_file(path).is_ok())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_append_file(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(args_count);
    let path = args.first().copied().unwrap_or_else(|| Value::nil()).kind();
    let data = args.get(1).copied().unwrap_or_else(|| Value::nil()).kind();

    match (path, data) {
        (ValueKind::String(path_handle), ValueKind::String(data_handle)) => {
            let path = vm.alloc.strings.get(path_handle);
            let path = Path::new(path);
            let data: &str = vm.alloc.strings.get(data_handle);

            let result = OpenOptions::new()
                .create(true)
                .append(true)
                .open(path)
                .and_then(|mut file| file.write_all(data.as_bytes()));

            Ok(Some(Value::boolean(result.is_ok())))
        }
        (ValueKind::String(_), _) => Err(NativeFunctionError::new(
            "Can only write strings to file system.",
        )),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_list(args_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(args_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);

            match fs::read_dir(path) {
                Ok(entries) => {
                    let array_handle = vm.alloc.arrays.create_array(0);
                    for entry in entries {
                        if let Ok(entry) = entry {
                            if let Some(file_name) = entry.file_name().to_str() {
                                let string_handle = vm.alloc.strings.intern(file_name);
                                vm.alloc
                                    .arrays
                                    .push(array_handle, Value::string(string_handle));
                            }
                        }
                    }

                    Ok(Some(Value::array(array_handle)))
                }
                _ => Ok(Some(Value::nil())),
            }
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_create_dir(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(args_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(fs::create_dir(path).is_ok())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_create_dirs(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(args_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(fs::create_dir_all(path).is_ok())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_remove_dir(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(args_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(fs::remove_dir(path).is_ok())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_remove_all(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(args_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);
            Ok(Some(Value::boolean(fs::remove_dir_all(path).is_ok())))
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_get_file_size(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(args_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);

            let metadata = fs::metadata(path);

            match metadata {
                Ok(metadata) => Ok(Some(Value::number(metadata.len() as f64))),
                Err(_) => Ok(Some(Value::nil())),
            }
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_get_file_modified_time(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match vm
        .get_function_args(args_count)
        .first()
        .copied()
        .map(|v| v.kind())
    {
        Some(ValueKind::String(handle)) => {
            let path = vm.alloc.strings.get(handle);
            let path = Path::new(path);

            let duration = fs::metadata(path)
                .map_err(|_| NativeFunctionError::new("Unable to get file metadata."))
                .and_then(|metadata| {
                    metadata
                        .modified()
                        .map_err(|_| NativeFunctionError::new("Unable to get modified time."))
                })
                .and_then(|modified| {
                    modified
                        .duration_since(UNIX_EPOCH)
                        .map_err(|_| NativeFunctionError::new("Unable to calculate timestamp."))
                });

            match duration {
                Ok(duration) => Ok(Some(Value::number(duration.as_millis() as f64))),
                Err(_) => Ok(Some(Value::nil())),
            }
        }
        None => Err(NativeFunctionError::new("Missing path")),
        _ => Err(NativeFunctionError::new("Path must be a string.")),
    }
}

pub fn qang_fs_copy_file(
    args_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(args_count);
    let from = args.first().copied().unwrap_or_else(|| Value::nil()).kind();
    let to = args.get(1).copied().unwrap_or_else(|| Value::nil()).kind();

    match (from, to) {
        (ValueKind::String(from_handle), ValueKind::String(to_handle)) => {
            let from_path = vm.alloc.strings.get(from_handle);
            let from_path = Path::new(from_path);
            let to_path = vm.alloc.strings.get(to_handle);
            let to_path = Path::new(to_path);

            Ok(Some(Value::boolean(fs::copy(from_path, to_path).is_ok())))
        }
        (ValueKind::String(_), _) => Err(NativeFunctionError::new(
            "Both source and destination must be strings.",
        )),
        _ => Err(NativeFunctionError::new("Source path must be a string.")),
    }
}

pub fn qang_fs_move(args_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(args_count);
    let from = args.first().copied().unwrap_or_else(|| Value::nil()).kind();
    let to = args.get(1).copied().unwrap_or_else(|| Value::nil()).kind();

    match (from, to) {
        (ValueKind::String(from_handle), ValueKind::String(to_handle)) => {
            let from_path = vm.alloc.strings.get(from_handle);
            let from_path = Path::new(from_path);
            let to_path = vm.alloc.strings.get(to_handle);
            let to_path = Path::new(to_path);

            Ok(Some(Value::boolean(fs::rename(from_path, to_path).is_ok())))
        }
        (ValueKind::String(_), _) => Err(NativeFunctionError::new(
            "Both source and destination must be strings.",
        )),
        _ => Err(NativeFunctionError::new("Source path must be a string.")),
    }
}

impl Vm {
    pub(crate) fn with_native_filesystem(&mut self) -> &mut Self {
        self.add_native_function("_path_get_os_seperator", 0, qang_path_get_os_seperator)
            .add_native_function("_path_exists", 1, qang_path_exists)
            .add_native_function("_path_is_file", 1, qang_path_is_file)
            .add_native_function("_path_is_dir", 1, qang_path_is_dir)
            .add_native_function("_path_is_absolute", 1, qang_path_is_absolute)
            .add_native_function("_path_is_relative", 1, qang_path_is_relative)
            .add_native_function("_fs_read_file", 1, qang_fs_read_file)
            .add_native_function("_fs_read_chunk", 3, qang_fs_read_chunk)
            .add_native_function("_fs_write_file", 2, qang_fs_write_file)
            .add_native_function("_fs_append_file", 2, qang_fs_append_file)
            .add_native_function("_fs_remove_file", 1, qang_fs_remove_file)
            .add_native_function("_fs_list", 1, qang_fs_list)
            .add_native_function("_fs_create_dir", 1, qang_fs_create_dir)
            .add_native_function("_fs_create_dirs", 1, qang_fs_create_dirs)
            .add_native_function("_fs_remove_dir", 1, qang_fs_remove_dir)
            .add_native_function("_fs_remove_all", 1, qang_fs_remove_all)
            .add_native_function("_fs_get_file_size", 1, qang_fs_get_file_size)
            .add_native_function(
                "_fs_get_file_modified_time",
                1,
                qang_fs_get_file_modified_time,
            )
            .add_native_function("_fs_move", 2, qang_fs_move)
            .add_native_function("_fs_copy_file", 2, qang_fs_copy_file)
    }
}
