use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{
    GlobalCompilerPipeline, HeapAllocator, NativeFunctionError, QangProgram, QangRuntimeError,
    Value, ValueKind, Vm,
    backend::{
        assembler::STACK_MAX,
        object::{IntrinsicKind, IntrinsicMethod},
        value::{
            ARRAY_TYPE_STRING, BOOLEAN_TYPE_STRING, CLASS_TYPE_STRING, FUNCTION_TYPE_STRING,
            MODULE_TYPE_STRING, NIL_TYPE_STRING, NUMBER_TYPE_STRING, OBJECT_TYPE_STRING,
            STRING_TYPE_STRING,
        },
        vm::RuntimeResult,
    },
    peek_value, pop_value, push_value,
};

pub fn qang_assert(arg_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let assertion = args
        .first()
        .ok_or_else(|| NativeFunctionError::new("No arguments provided."))?;

    if assertion.is_truthy() {
        return Ok(None);
    }
    let message = args
        .get(1)
        .copied()
        .and_then(|v| {
            if v.is_nil() {
                None
            } else {
                Some(v.to_display_string(&vm.alloc))
            }
        })
        .unwrap_or_else(|| "Assertion failed.".to_string());

    Err(NativeFunctionError(message))
}

pub fn qang_assert_eq(arg_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    if args.len() < 2 {
        return Err("Must provide two arguments.".into());
    }

    let a = args[0];
    let b = args[1];

    if a != b {
        let message = args
            .get(2)
            .copied()
            .and_then(|v| {
                if v.is_nil() {
                    None
                } else {
                    Some(v.to_display_string(&vm.alloc))
                }
            })
            .unwrap_or_else(|| "Assertion failed.".to_string());
        Err(NativeFunctionError(message))
    } else {
        Ok(None)
    }
}

pub fn qang_assert_throws(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let assertion = args
        .first()
        .ok_or_else(|| NativeFunctionError::new("No arguments provided."))?;
    let message_handle = args.get(1).copied();

    let function_handle = match assertion.as_closure() {
        Some(function_handle) => function_handle,
        _ => return Err("First argument must be a function.".into()),
    };

    let result = vm.call_function(function_handle, &[] as &[Value; 0]);

    match result {
        Ok(_) => {
            let message = message_handle
                .and_then(|v| {
                    if v.is_nil() {
                        None
                    } else {
                        Some(v.to_display_string(&vm.alloc))
                    }
                })
                .unwrap_or_else(|| "Expected function to throw, but did not.".to_string());

            Err(NativeFunctionError::new(&message))
        }
        Err(_) => Ok(None),
    }
}

pub fn qang_print(arg_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let value = args.first().copied().unwrap_or(Value::nil());
    let value = value.to_display_string(&vm.alloc);
    print!("{}", value);
    Ok(None)
}

pub fn qang_println(arg_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let value = args.first().copied().unwrap_or(Value::nil());
    let value = value.to_display_string(&vm.alloc);
    println!("{}", value);
    Ok(None)
}

pub fn qang_typeof(arg_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let value = args.first().copied().unwrap_or(Value::nil());
    let value_string = value.to_type_string();
    let handle = vm.alloc.strings.intern(value_string);

    Ok(Some(Value::string(handle)))
}

pub fn qang_system_time(
    _arg_count: usize,
    _vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .map_err(|_| NativeFunctionError::new("Unable to get system time."))?;

    let time: f64 = since_the_epoch.as_millis() as f64;

    Ok(Some(time.into()))
}

pub fn qang_to_string(arg_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let value = args.first().copied().unwrap_or(Value::nil());

    let value = value.to_display_string(&vm.alloc);
    let value_handle = vm.alloc.strings.intern(&value);

    Ok(Some(Value::string(value_handle)))
}

pub fn qang_env_cwd(_arg_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    match std::env::current_dir() {
        Ok(path) => {
            let handle = vm.alloc.strings.intern(&path.as_os_str().to_string_lossy());
            Ok(Some(Value::string(handle)))
        }
        _ => Ok(Some(Value::nil())),
    }
}

pub fn qang_string_to_uppercase(
    receiver: Value,
    _arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(handle) = receiver.as_string() {
        let uppercase_handle = vm.alloc.strings.to_uppercase(handle);

        Ok(Some(Value::string(uppercase_handle)))
    } else {
        Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_string_to_lowercase(
    receiver: Value,
    _arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(handle) = receiver.as_string() {
        let lowercase_handle = vm.alloc.strings.to_lowercase(handle);

        Ok(Some(Value::string(lowercase_handle)))
    } else {
        Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_string_split(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(handle) = receiver.as_string() {
        let args = vm.get_function_args(arg_count);
        let delimeter = if let Some(delimeter) = args.first() {
            if let Some(delimeter_handle) = delimeter.as_string() {
                delimeter_handle
            } else {
                return Err(NativeFunctionError::new("Delimeter must be a string."));
            }
        } else {
            vm.alloc.strings.intern("")
        };

        let arr = vm.alloc.arrays.create_array(0);
        let delimeter = vm.alloc.strings.get(delimeter).to_owned();
        let string_to_split = vm.alloc.strings.get(handle).to_owned();

        if delimeter.is_empty() {
            // Split into individual characters when no delimiter
            for ch in string_to_split.chars() {
                let handle = vm.alloc.strings.intern(&ch.to_string());
                vm.alloc.arrays.push(arr, Value::string(handle));
            }
        } else {
            // Normal split with delimiter
            let strings = string_to_split
                .split(&delimeter)
                .map(|s| s.to_owned())
                .collect::<Vec<_>>();
            for string in strings {
                let handle = vm.alloc.strings.intern(string.as_str());
                vm.alloc.arrays.push(arr, Value::string(handle));
            }
        }

        Ok(Some(Value::array(arr)))
    } else {
        Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_string_concat(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    match (receiver.kind(), args.first().copied().map(|a| a.kind())) {
        (ValueKind::String(handle1), Some(ValueKind::String(handle2))) => Ok(Some(Value::string(
            vm.alloc.strings.concat(handle1, handle2),
        ))),
        (ValueKind::String(_), _) => Err(NativeFunctionError::new(
            "A string can only be concatenated with another string.",
        )),
        _ => Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        ))),
    }
}

pub fn qang_string_char_at(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    match (receiver.kind(), args.first().copied().map(|a| a.kind())) {
        (ValueKind::String(handle), Some(ValueKind::Number(index))) => {
            if index < 0.0 {
                return Ok(Some(Value::nil()));
            }

            vm.alloc
                .strings
                .char_at(handle, index.trunc() as usize)
                .map(|char| Ok(Some(Value::string(char))))
                .unwrap_or_else(|| Ok(Some(Value::nil())))
        }
        (ValueKind::String(_), _) => Err(NativeFunctionError::new(
            "A string can only be indexed by a number.",
        )),
        _ => Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        ))),
    }
}

pub fn qang_string_starts_with(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let prefix_string = args.first().copied().unwrap_or(Value::nil());

    match (receiver.kind(), prefix_string.kind()) {
        (ValueKind::String(string_handle), ValueKind::String(prefix_handle)) => {
            Ok(vm.alloc.strings.starts_with(string_handle, prefix_handle))
        }
        (ValueKind::String(_), _) => Ok(false),
        _ => Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        ))),
    }
    .map(|value| Some(Value::boolean(value)))
}

pub fn qang_string_ends_with(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let prefix_string = args.first().copied().unwrap_or(Value::nil());

    match (receiver.kind(), prefix_string.kind()) {
        (ValueKind::String(string_handle), ValueKind::String(prefix_handle)) => {
            Ok(vm.alloc.strings.ends_with(string_handle, prefix_handle))
        }
        (ValueKind::String(_), _) => Ok(false),
        _ => Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        ))),
    }
    .map(|value| Some(Value::boolean(value)))
}

pub fn qang_string_contains(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let prefix_string = args.first().copied().unwrap_or(Value::nil());

    match (receiver.kind(), prefix_string.kind()) {
        (ValueKind::String(string_handle), ValueKind::String(prefix_handle)) => {
            Ok(vm.alloc.strings.contains(string_handle, prefix_handle))
        }
        (ValueKind::String(_), _) => Ok(false),
        _ => Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        ))),
    }
    .map(|value| Some(Value::boolean(value)))
}

pub fn qang_string_length(
    receiver: Value,
    _arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(handle) = receiver.as_string() {
        let length = vm.alloc.strings.length(handle);

        Ok(Some(Value::number(length as f64)))
    } else {
        Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_array_length(
    receiver: Value,
    _arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(handle) = receiver.as_array() {
        let length = vm.alloc.arrays.length(handle);
        Ok(Some(length.into()))
    } else {
        Err(NativeFunctionError(format!(
            "Expected array but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_array_push(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    if let Some(handle) = receiver.as_array() {
        let element = args.first().copied().unwrap_or_default();
        vm.with_gc_check(|alloc| alloc.arrays.push(handle, element));
        Ok(None)
    } else {
        Err(NativeFunctionError(format!(
            "Expected array but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_array_pop(
    receiver: Value,
    _arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(handle) = receiver.as_array() {
        let element = vm.alloc.arrays.pop(handle);
        if element.is_none() {
            return Err(NativeFunctionError::new("Array is empty."));
        }
        Ok(element)
    } else {
        Err(NativeFunctionError(format!(
            "Expected array but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_array_reverse(
    receiver: Value,
    _arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(handle) = receiver.as_array() {
        vm.alloc.arrays.reverse(handle);
        Ok(None)
    } else {
        Err(NativeFunctionError(format!(
            "Expected array but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_array_slice(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    if let Some(handle) = receiver.as_array() {
        let begin = args.first().copied().unwrap_or(Value::nil());
        let end = args.get(1).copied().unwrap_or(Value::nil());

        match (begin.kind(), end.kind()) {
            (ValueKind::Number(begin), ValueKind::Number(end)) => {
                let slice = vm.with_gc_check(|alloc| {
                    Value::array(
                        alloc
                            .arrays
                            .slice(handle, begin as isize, Some(end as isize)),
                    )
                });
                Ok(Some(slice))
            }
            (ValueKind::Number(begin), ValueKind::Nil) => {
                let slice = vm.with_gc_check(|alloc| {
                    Value::array(alloc.arrays.slice(handle, begin as isize, None))
                });
                Ok(Some(slice))
            }
            (ValueKind::Nil, ValueKind::Nil) => {
                let clone =
                    vm.with_gc_check(|alloc| Value::array(alloc.arrays.shallow_copy(handle)));
                Ok(Some(clone))
            }
            _ => Err(NativeFunctionError::new(
                "Expected both values to be a number.",
            )),
        }
    } else {
        Err(NativeFunctionError(format!(
            "Expected array but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_array_get(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    match (receiver.kind(), args.first().map(|a| a.kind())) {
        (ValueKind::Array(handle), Some(ValueKind::Number(index))) => {
            Ok(Some(vm.alloc.arrays.get(handle, index.trunc() as isize)))
        }
        (ValueKind::Array(_), _) => Err(NativeFunctionError::new(
            "An array can only be indexed by a number.",
        )),
        _ => Err(NativeFunctionError(format!(
            "Expected array but recieved {}.",
            receiver.to_type_string()
        ))),
    }
}

pub fn qang_array_concat(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    match (receiver.kind(), args.first().copied().map(|a| a.kind())) {
        (ValueKind::Array(handle1), Some(ValueKind::Array(handle2))) => {
            Ok(Some(Value::array(vm.alloc.arrays.concat(handle1, handle2))))
        }
        (ValueKind::Array(_), _) => Err(NativeFunctionError::new(
            "An array can only be concatenated with another array.",
        )),
        _ => Err(NativeFunctionError(format!(
            "Expected array but recieved {}.",
            receiver.to_type_string()
        ))),
    }
}

pub fn qang_array_remove_at(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    match (receiver.kind(), args.first().map(|a| a.kind())) {
        (ValueKind::Array(handle), Some(ValueKind::Number(index))) => {
            let value = vm.alloc.arrays.remove_at(handle, index.trunc() as isize);

            Ok(value)
        }
        (ValueKind::Array(_), _) => Err(NativeFunctionError::new(
            "An array can only be indexed by a number.",
        )),
        _ => Err(NativeFunctionError(format!(
            "Expected array but recieved {}.",
            receiver.to_type_string()
        ))),
    }
}

pub fn qang_array_index_of(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if arg_count < 1 {
        return Err(NativeFunctionError::new(
            "An argument must be provided for index_of.",
        ));
    }
    let value = vm
        .get_function_args(arg_count)
        .first()
        .copied()
        .unwrap_or(Value::nil());

    if let Some(handle) = receiver.as_array() {
        for (index, item) in vm.alloc.arrays.iter(handle).enumerate() {
            if item == value {
                return Ok(Some(Value::number(index as f64)));
            }
        }
    }

    return Ok(Some(Value::number(-1.0)));
}

pub fn qang_array_contains(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if arg_count < 1 {
        return Err(NativeFunctionError::new(
            "An argument must be provided for contains.",
        ));
    }
    let value = vm
        .get_function_args(arg_count)
        .first()
        .copied()
        .unwrap_or(Value::nil());

    if let Some(handle) = receiver.as_array() {
        for item in vm.alloc.arrays.iter(handle) {
            if item == value {
                return Ok(Some(Value::boolean(true)));
            }
        }
    }

    return Ok(Some(Value::boolean(false)));
}

pub fn qang_array_join(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(handle) = receiver.as_array() {
        let args = vm.get_function_args(arg_count);

        let delimeter = if let Some(delimeter) = args.first() {
            if let Some(delimeter_handle) = delimeter.as_string() {
                delimeter_handle
            } else {
                return Err(NativeFunctionError::new("Delimeter must be a string."));
            }
        } else {
            vm.alloc.strings.intern("")
        };

        let mut combined_string = String::new();
        let delimeter_str = vm.alloc.strings.get(delimeter);

        let mut first = true;
        for value in vm.alloc.arrays.iter(handle) {
            if !first {
                combined_string += delimeter_str;
            }
            first = false;

            let string = value.to_display_string(&vm.alloc);
            combined_string += &string;
        }

        let combined_string_handle = vm.alloc.strings.intern(&combined_string);

        Ok(Some(Value::string(combined_string_handle)))
    } else {
        Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_array_create(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    if let Some(length) = args.first().copied().unwrap_or(Value::nil()).as_number() {
        Ok(Some(Value::array(
            vm.alloc.arrays.create_array(length.trunc() as usize),
        )))
    } else {
        Err(NativeFunctionError::new("Expected length to be a number."))
    }
}

pub fn qang_hash(arg_count: usize, vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let value = args.first().copied();

    match value {
        None => Err("No value provided to hash.".into()),
        Some(value) => Ok(Some(value.hash().into())),
    }
}

pub fn qang_object_to_entries(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let value = args.first().copied().unwrap_or_else(|| Value::nil());

    let entry_handle = vm.alloc.strings.intern("Entry");
    let entry_class = vm
        .globals()
        .get(&entry_handle)
        .copied()
        .ok_or_else(|| NativeFunctionError::new("Expect Entry to exist in stdlib."))
        .and_then(|value| {
            value.as_class().ok_or_else(|| {
                NativeFunctionError(format!(
                    "Expected class but found, {}",
                    value.to_display_string(&vm.alloc)
                ))
            })
        })?;
    let key_handle = vm.alloc.strings.intern("key");
    let value_handle = vm.alloc.strings.intern("value");

    match value.kind() {
        ValueKind::ObjectLiteral(handle) => {
            let table_entries: Vec<(Value, Value)> = vm.alloc.tables.iter(handle).collect();
            let length = table_entries.len();

            let entries = vm.alloc.arrays.create_array(length);

            for (index, (key, value)) in table_entries.into_iter().enumerate() {
                let instance_handle = vm.alloc.allocate_instance(entry_class);
                let table_handle = vm.alloc.get_instance(instance_handle).table;
                vm.alloc
                    .set_instance_field(table_handle, Value::string(key_handle), key);
                vm.alloc
                    .set_instance_field(table_handle, Value::string(value_handle), value);
                vm.alloc
                    .arrays
                    .insert(entries, index, Value::instance(instance_handle));
            }
            Ok(Some(Value::array(entries)))
        }
        ValueKind::Instance(handle) => {
            let table_handle = vm.alloc.get_instance(handle).table;
            let table_entries: Vec<(Value, Value)> = vm.alloc.tables.iter(table_handle).collect();
            let length = table_entries.len();

            let entries = vm.alloc.arrays.create_array(length);

            for (index, (key, value)) in table_entries.into_iter().enumerate() {
                let instance_handle = vm.alloc.allocate_instance(entry_class);
                let table_handle = vm.alloc.get_instance(instance_handle).table;
                vm.alloc
                    .set_instance_field(table_handle, Value::string(key_handle), key);
                vm.alloc
                    .set_instance_field(table_handle, Value::string(value_handle), value);
                vm.alloc
                    .arrays
                    .insert(entries, index, Value::instance(instance_handle));
            }
            Ok(Some(Value::array(entries)))
        }
        _ => Err(NativeFunctionError(format!(
            "Expected object literal or instance, but found {}",
            value.to_display_string(&vm.alloc)
        ))),
    }
}

pub fn qang_object_get(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let object = args.first().copied().unwrap_or_else(|| Value::nil());
    let key = args.get(1).copied().unwrap_or_else(|| Value::nil());

    match (object.kind(), key.kind()) {
        (ValueKind::ObjectLiteral(handle), ValueKind::String(_)) => {
            let value = vm.alloc.tables.get(handle, &key);
            Ok(value)
        }
        (ValueKind::Instance(handle), ValueKind::String(_)) => {
            let table_handle = vm.alloc.get_instance(handle).table;
            let value = vm.alloc.tables.get(table_handle, &key);
            Ok(value)
        }
        (ValueKind::ObjectLiteral(_) | ValueKind::Instance(_), _) => {
            Err(NativeFunctionError(format!(
                "Expected string, but found {}",
                key.to_display_string(&vm.alloc)
            )))
        }
        _ => Err(NativeFunctionError(format!(
            "Expected object literal or instance, but found {}",
            object.to_display_string(&vm.alloc)
        ))),
    }
}

pub fn qang_object_set(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let object = args.first().copied().unwrap_or_else(|| Value::nil());
    let key = args.get(1).copied().unwrap_or_else(|| Value::nil());
    let value = args.get(2).copied().unwrap_or_else(|| Value::nil());

    match (object.kind(), key.kind()) {
        (ValueKind::ObjectLiteral(handle), ValueKind::String(_)) => {
            vm.alloc.tables.insert(handle, key, value);
            Ok(None)
        }
        (ValueKind::Instance(handle), ValueKind::String(_)) => {
            let table_handle = vm.alloc.get_instance(handle).table;
            vm.alloc.tables.insert(table_handle, key, value);
            Ok(None)
        }
        (ValueKind::ObjectLiteral(_) | ValueKind::Instance(_), _) => {
            Err(NativeFunctionError(format!(
                "Expected string, but found {}",
                key.to_display_string(&vm.alloc)
            )))
        }
        _ => Err(NativeFunctionError(format!(
            "Expected object literal or instance, but found {}",
            object.to_display_string(&vm.alloc)
        ))),
    }
}

pub fn qang_object_assign(
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let args = vm.get_function_args(arg_count);
    let object = args.first().copied().unwrap_or_else(|| Value::nil());
    let other = args.get(1).copied().unwrap_or_else(|| Value::nil());

    match (object.kind(), other.kind()) {
        (ValueKind::ObjectLiteral(handle), ValueKind::ObjectLiteral(other_handle)) => {
            vm.alloc.tables.copy_into(other_handle, handle);
            Ok(Some(Value::object_literal(handle)))
        }
        (ValueKind::ObjectLiteral(handle), ValueKind::Instance(other_handle)) => {
            let other_table_handle = vm.alloc.get_instance(other_handle).table;
            vm.alloc.tables.copy_into(other_table_handle, handle);
            Ok(Some(Value::object_literal(handle)))
        }
        (ValueKind::Instance(handle), ValueKind::ObjectLiteral(other_handle)) => {
            let table_handle = vm.alloc.get_instance(handle).table;
            vm.alloc.tables.copy_into(other_handle, table_handle);
            Ok(Some(Value::instance(handle)))
        }
        (ValueKind::Instance(handle), ValueKind::Instance(other_handle)) => {
            let table_handle = vm.alloc.get_instance(handle).table;
            let other_table_handle = vm.alloc.get_instance(other_handle).table;
            vm.alloc.tables.copy_into(other_table_handle, table_handle);
            Ok(Some(Value::instance(handle)))
        }
        (ValueKind::ObjectLiteral(_) | ValueKind::Instance(_), _) => {
            Err(NativeFunctionError(format!(
                "Expected object literal or instance for source, but found {}",
                object.to_display_string(&vm.alloc)
            )))
        }
        _ => Err(NativeFunctionError(format!(
            "Expected object literal or instance for destination, but found {}",
            object.to_display_string(&vm.alloc)
        ))),
    }
}

pub fn qang_number_ceil(
    receiver: Value,
    _arg_count: usize,
    _vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(value) = receiver.as_number() {
        Ok(Some(Value::number(value.ceil())))
    } else {
        Err(NativeFunctionError(format!(
            "Expected number but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_number_floor(
    receiver: Value,
    _arg_count: usize,
    _vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(value) = receiver.as_number() {
        Ok(Some(Value::number(value.floor())))
    } else {
        Err(NativeFunctionError(format!(
            "Expected number but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_number_trunc(
    receiver: Value,
    _arg_count: usize,
    _vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Some(value) = receiver.as_number() {
        Ok(Some(Value::number(value.trunc())))
    } else {
        Err(NativeFunctionError(format!(
            "Expected number but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_number_min(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let min = vm
        .get_function_args(arg_count)
        .first()
        .and_then(|v| v.as_number())
        .ok_or_else(|| NativeFunctionError::new("Min value must be a number."))?;

    if let Some(value) = receiver.as_number() {
        Ok(Some(Value::number(value.min(min))))
    } else {
        Err(NativeFunctionError(format!(
            "Expected number but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_number_max(
    receiver: Value,
    arg_count: usize,
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let max = vm
        .get_function_args(arg_count)
        .first()
        .and_then(|v| v.as_number())
        .ok_or_else(|| NativeFunctionError::new("Max value must be a number."))?;

    if let Some(value) = receiver.as_number() {
        Ok(Some(Value::number(value.max(max))))
    } else {
        Err(NativeFunctionError(format!(
            "Expected number but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

impl Vm {
    pub(crate) fn with_stdlib(&mut self) -> &mut Self {
        if let Err(err) = self.load_stdlib() {
            panic!("Failed to load standard library: {}", err);
        }
        self
    }

    fn load_stdlib(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        self.load_constants();
        let stdlib_source = include_str!("stdlib.ql");
        let program =
            GlobalCompilerPipeline::compile_source(stdlib_source.to_owned(), &mut self.alloc)
                .map_err(|e| format!("Stdlib compilation failed: {:?}", e))?;

        // Execute stdlib to populate globals
        self.execute_stdlib(program)?;

        Ok(())
    }

    fn execute_stdlib(&mut self, program: QangProgram) -> Result<(), Box<dyn std::error::Error>> {
        // Save current VM state
        let saved_stack_top = self.state.stack_top;
        let saved_frame_count = self.state.frame_count;

        // Execute stdlib program
        self.interpret(program)
            .map_err(|e| format!("Stdlib execution failed: {:?}", e))?;

        // Restore VM state (stdlib execution shouldn't affect main program state)
        self.state.stack_top = saved_stack_top;
        self.state.frame_count = saved_frame_count;

        Ok(())
    }

    fn load_constants(&mut self) {
        self.alloc.strings.intern("NIL");
        self.alloc.strings.intern(NIL_TYPE_STRING);
        self.alloc.strings.intern("BOOLEAN");
        self.alloc.strings.intern(BOOLEAN_TYPE_STRING);
        self.alloc.strings.intern("NUMBER");
        self.alloc.strings.intern(NUMBER_TYPE_STRING);
        self.alloc.strings.intern("STRING");
        self.alloc.strings.intern(STRING_TYPE_STRING);
        self.alloc.strings.intern("FUNCTION");
        self.alloc.strings.intern(FUNCTION_TYPE_STRING);
        self.alloc.strings.intern("CLASS");
        self.alloc.strings.intern(CLASS_TYPE_STRING);
        self.alloc.strings.intern("OBJECT");
        self.alloc.strings.intern(OBJECT_TYPE_STRING);
        self.alloc.strings.intern("ARRAY");
        self.alloc.strings.intern(ARRAY_TYPE_STRING);
        self.alloc.strings.intern("MODULE");
        self.alloc.strings.intern(MODULE_TYPE_STRING);
    }

    pub(crate) fn load_intrinsics(
        alloc: &mut HeapAllocator,
    ) -> FxHashMap<IntrinsicKind, IntrinsicMethod> {
        let mut intrinsics = FxHashMap::with_hasher(FxBuildHasher);
        let to_uppercase_handle = alloc.strings.intern("to_uppercase");
        intrinsics.insert(
            IntrinsicKind::String(to_uppercase_handle),
            IntrinsicMethod::Native {
                function: qang_string_to_uppercase,
                arity: 0,
            },
        );
        let to_lowercase_handle = alloc.strings.intern("to_lowercase");
        intrinsics.insert(
            IntrinsicKind::String(to_lowercase_handle),
            IntrinsicMethod::Native {
                function: qang_string_to_lowercase,
                arity: 0,
            },
        );
        let to_number_handle = alloc.strings.intern("to_number");
        intrinsics.insert(
            IntrinsicKind::String(to_number_handle),
            IntrinsicMethod::ToNumber,
        );
        let concat_handle = alloc.strings.intern("concat");

        intrinsics.insert(
            IntrinsicKind::String(concat_handle),
            IntrinsicMethod::Native {
                function: qang_string_concat,
                arity: 1,
            },
        );
        let split_handle = alloc.strings.intern("split");
        intrinsics.insert(
            IntrinsicKind::String(split_handle),
            IntrinsicMethod::Native {
                function: qang_string_split,
                arity: 1,
            },
        );
        let contains_handle = alloc.strings.intern("contains");
        intrinsics.insert(
            IntrinsicKind::String(contains_handle),
            IntrinsicMethod::Native {
                function: qang_string_contains,
                arity: 1,
            },
        );
        let char_at_handle = alloc.strings.intern("char_at");
        intrinsics.insert(
            IntrinsicKind::String(char_at_handle),
            IntrinsicMethod::Native {
                function: qang_string_char_at,
                arity: 1,
            },
        );
        let starts_with_handle = alloc.strings.intern("starts_with");
        intrinsics.insert(
            IntrinsicKind::String(starts_with_handle),
            IntrinsicMethod::Native {
                function: qang_string_starts_with,
                arity: 1,
            },
        );
        let ends_with_handle = alloc.strings.intern("ends_with");
        intrinsics.insert(
            IntrinsicKind::String(ends_with_handle),
            IntrinsicMethod::Native {
                function: qang_string_ends_with,
                arity: 1,
            },
        );
        let length_handle = alloc.strings.intern("length");
        intrinsics.insert(
            IntrinsicKind::String(length_handle),
            IntrinsicMethod::Native {
                function: qang_string_length,
                arity: 0,
            },
        );
        intrinsics.insert(
            IntrinsicKind::Array(length_handle),
            IntrinsicMethod::Native {
                function: qang_array_length,
                arity: 0,
            },
        );
        let array_push_handle = alloc.strings.intern("push");
        intrinsics.insert(
            IntrinsicKind::Array(array_push_handle),
            IntrinsicMethod::Native {
                function: qang_array_push,
                arity: 1,
            },
        );
        let array_pop_handle = alloc.strings.intern("pop");
        intrinsics.insert(
            IntrinsicKind::Array(array_pop_handle),
            IntrinsicMethod::Native {
                function: qang_array_pop,
                arity: 0,
            },
        );
        let array_reverse_handle = alloc.strings.intern("reverse");
        intrinsics.insert(
            IntrinsicKind::Array(array_reverse_handle),
            IntrinsicMethod::Native {
                function: qang_array_reverse,
                arity: 0,
            },
        );
        let array_slice_handle = alloc.strings.intern("slice");
        intrinsics.insert(
            IntrinsicKind::Array(array_slice_handle),
            IntrinsicMethod::Native {
                function: qang_array_slice,
                arity: 2,
            },
        );
        let array_get_handle = alloc.strings.intern("get");
        intrinsics.insert(
            IntrinsicKind::Array(array_get_handle),
            IntrinsicMethod::Native {
                function: qang_array_get,
                arity: 1,
            },
        );
        intrinsics.insert(
            IntrinsicKind::Array(concat_handle),
            IntrinsicMethod::Native {
                function: qang_array_concat,
                arity: 1,
            },
        );
        let array_remove_at_handle = alloc.strings.intern("remove_at");
        intrinsics.insert(
            IntrinsicKind::Array(array_remove_at_handle),
            IntrinsicMethod::Native {
                function: qang_array_remove_at,
                arity: 1,
            },
        );
        let array_join_handle = alloc.strings.intern("join");
        intrinsics.insert(
            IntrinsicKind::Array(array_join_handle),
            IntrinsicMethod::Native {
                function: qang_array_join,
                arity: 1,
            },
        );
        let array_index_of_handle = alloc.strings.intern("index_of");
        intrinsics.insert(
            IntrinsicKind::Array(array_index_of_handle),
            IntrinsicMethod::Native {
                function: qang_array_index_of,
                arity: 1,
            },
        );
        intrinsics.insert(
            IntrinsicKind::Array(contains_handle),
            IntrinsicMethod::Native {
                function: qang_array_contains,
                arity: 1,
            },
        );
        let array_iter_handle = alloc.strings.intern("iter");
        intrinsics.insert(
            IntrinsicKind::Array(array_iter_handle),
            IntrinsicMethod::Iter,
        );
        let number_ceil_handle = alloc.strings.intern("ceil");
        intrinsics.insert(
            IntrinsicKind::Number(number_ceil_handle),
            IntrinsicMethod::Native {
                function: qang_number_ceil,
                arity: 0,
            },
        );
        let number_floor_handle = alloc.strings.intern("floor");
        intrinsics.insert(
            IntrinsicKind::Number(number_floor_handle),
            IntrinsicMethod::Native {
                function: qang_number_floor,
                arity: 0,
            },
        );
        let number_trunc_handle = alloc.strings.intern("trunc");
        intrinsics.insert(
            IntrinsicKind::Number(number_trunc_handle),
            IntrinsicMethod::Native {
                function: qang_number_trunc,
                arity: 0,
            },
        );
        let number_min_handle = alloc.strings.intern("min");
        intrinsics.insert(
            IntrinsicKind::Number(number_min_handle),
            IntrinsicMethod::Native {
                function: qang_number_min,
                arity: 1,
            },
        );
        let number_max_handle = alloc.strings.intern("max");
        intrinsics.insert(
            IntrinsicKind::Number(number_max_handle),
            IntrinsicMethod::Native {
                function: qang_number_max,
                arity: 1,
            },
        );
        let function_call_handle = alloc.strings.intern("call");
        intrinsics.insert(
            IntrinsicKind::Function(function_call_handle),
            IntrinsicMethod::Call,
        );
        let function_apply_handle = alloc.strings.intern("apply");
        intrinsics.insert(
            IntrinsicKind::Function(function_apply_handle),
            IntrinsicMethod::Apply,
        );

        intrinsics
    }

    pub(crate) fn handle_function_intrinsic_call(
        &mut self,
        receiver: Value,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        match receiver.kind() {
            ValueKind::Closure(_)
            | ValueKind::BoundIntrinsic(_)
            | ValueKind::BoundMethod(_)
            | ValueKind::NativeFunction(_) => {
                self.state.stack[self.state.stack_top - arg_count - 1] = receiver;
                self.call_value(receiver, arg_count)
            }
            _ => Err(QangRuntimeError::new(
                "'call' can only be used on functions.".to_string(),
                self.state.get_previous_loc(),
            )),
        }
    }

    pub(crate) fn handle_function_intrinsic_apply(
        &mut self,
        receiver: Value,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        match receiver.kind() {
            ValueKind::Closure(_)
            | ValueKind::BoundIntrinsic(_)
            | ValueKind::BoundMethod(_)
            | ValueKind::NativeFunction(_) => {
                if arg_count != 1 {
                    return Err(QangRuntimeError::new(
                        "'apply' must be called with one argument and it must be an array."
                            .to_string(),
                        self.state.get_previous_loc(),
                    ));
                }

                let array_arg = peek_value!(self, 0);

                if let Some(array_handle) = array_arg.as_array() {
                    // Pop the array argument
                    pop_value!(self);

                    // Replace the bound intrinsic on the stack with the actual function
                    self.state.stack[self.state.stack_top - 1] = receiver;

                    let array_length = self.alloc.arrays.length(array_handle);
                    for value in self.alloc.arrays.iter(array_handle) {
                        push_value!(self, value)?;
                    }

                    self.call_value(receiver, array_length)
                } else {
                    Err(QangRuntimeError::new(
                        "'apply' must be called with one argument and it must be an array."
                            .to_string(),
                        self.state.get_previous_loc(),
                    ))
                }
            }
            _ => Err(QangRuntimeError::new(
                "'apply' can only be used on functions.".to_string(),
                self.state.get_previous_loc(),
            )),
        }
    }

    pub(crate) fn handle_array_intrinsic_iter(
        &mut self,
        receiver: Value,
        _arg_count: usize,
    ) -> RuntimeResult<()> {
        let array_iterator_handle = self.alloc.strings.intern("ArrayIterator");
        let array_iterator = *self
            .globals()
            .get(&array_iterator_handle)
            .expect("Expected ArrayIterator to be loaded from stdlib.");

        pop_value!(self);

        push_value!(self, array_iterator)?;

        push_value!(self, receiver)?;

        self.call_value(array_iterator, 1)
    }

    pub(crate) fn handle_string_intrinsic_to_number(
        &mut self,
        receiver: Value,
        _arg_count: usize,
    ) -> RuntimeResult<()> {
        pop_value!(self);

        let value = receiver.as_string().ok_or_else(|| {
            QangRuntimeError::new(
                "Expected string.".to_string(),
                self.state.get_previous_loc(),
            )
        })?;
        let ok_handle = self.alloc.strings.intern("Ok");
        let err_handle = self.alloc.strings.intern("Err");
        let value = self.alloc.strings.get(value);
        let result = match value.parse::<f64>() {
            Ok(number) => {
                let ok = *self
                    .globals()
                    .get(&ok_handle)
                    .expect("Expected Ok to be loaded from stdlib.");

                push_value!(self, ok)?;
                push_value!(self, Value::number(number))?;
                ok
            }
            Err(_) => {
                let err = *self
                    .globals()
                    .get(&err_handle)
                    .expect("Expected Err to be loaded from stdlib.");

                push_value!(self, err)?;

                let err_msg = self
                    .alloc
                    .strings
                    .intern(format!("Unable to parse value '{:?}' as number.", value).as_str());
                push_value!(self, Value::string(err_msg))?;
                err
            }
        };

        self.call_value(result, 1)
    }
}
