use crate::{NativeFunctionError, Value, Vm, compiler::STACK_MAX, pop_value, push_value};

pub fn qang_assert(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let assertion = args
        .first()
        .ok_or(NativeFunctionError::new("No arguments provided."))?;

    if assertion.is_truthy() {
        return Ok(None);
    }
    let message = args
        .get(1)
        .copied()
        .and_then(|v| match v {
            Value::Nil => None,
            _ => Some(v.to_display_string(&vm.alloc)),
        })
        .unwrap_or_else(|| "Assertion failed.".to_string());

    Err(NativeFunctionError(message))
}

pub fn qang_assert_eq(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    if args.len() < 2 {
        return Err("Must provide two arguments.".into());
    }

    let a = args[0];
    let b = args[1];

    if a != b {
        let message = args
            .get(2)
            .copied()
            .and_then(|v| match v {
                Value::Nil => None,
                _ => Some(v.to_display_string(&vm.alloc)),
            })
            .unwrap_or_else(|| "Assertion failed.".to_string());
        Err(NativeFunctionError(message))
    } else {
        Ok(None)
    }
}

pub fn qang_assert_throws(
    args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let assertion = args
        .first()
        .ok_or(NativeFunctionError::new("No arguments provided."))?;

    let function_handle = match assertion {
        Value::Closure(function_handle) => *function_handle,
        _ => return Err("First argument must be a function.".into()),
    };

    let result = vm.call_function(function_handle, Vec::new());

    match result {
        Ok(_) => {
            let message = args
                .get(1)
                .copied()
                .and_then(|v| match v {
                    Value::Nil => None,
                    _ => Some(v.to_display_string(&vm.alloc)),
                })
                .unwrap_or_else(|| "Expected function to throw, but did not.".to_string());

            Err(NativeFunctionError::new(&message))
        }
        Err(_) => Ok(None),
    }
}

pub fn qang_print(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied().unwrap_or(Value::Nil);
    let value = value.to_display_string(&vm.alloc);
    print!("{}", value);
    Ok(None)
}

pub fn qang_println(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied().unwrap_or(Value::Nil);
    let value = value.to_display_string(&vm.alloc);
    println!("{}", value);
    Ok(None)
}

pub fn qang_typeof(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied().unwrap_or(Value::Nil);
    let value_string = value.to_type_string();
    let handle = vm.alloc.strings.intern(value_string);

    Ok(Some(Value::String(handle)))
}

pub fn qang_system_time(
    _args: &[Value],
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

pub fn qang_to_string(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied().unwrap_or(Value::Nil);

    let value = value.to_display_string(&vm.alloc);
    let value_handle = vm.alloc.strings.intern(&value);

    Ok(Some(Value::String(value_handle)))
}

pub fn qang_string_to_uppercase(
    receiver: Value,
    _args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Value::String(handle) = receiver {
        let uppercase_string = &vm.alloc.strings.get_string(handle).to_uppercase();
        let uppercase_handle = vm.alloc.strings.intern(uppercase_string);

        Ok(Some(Value::String(uppercase_handle)))
    } else {
        Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_string_to_lowercase(
    receiver: Value,
    _args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Value::String(handle) = receiver {
        let lowercase_string = &vm.alloc.strings.get_string(handle).to_lowercase();
        let lowercase_handle = vm.alloc.strings.intern(lowercase_string);

        Ok(Some(Value::String(lowercase_handle)))
    } else {
        Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_array_length(
    receiver: Value,
    _args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Value::Array(handle) = receiver {
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
    args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Value::Array(handle) = receiver {
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
    _args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Value::Array(handle) = receiver {
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
    _args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Value::Array(handle) = receiver {
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
    args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Value::Array(handle) = receiver {
        let begin = args.first().copied().unwrap_or(Value::Nil);
        let end = args.get(1).copied().unwrap_or(Value::Nil);

        match (begin, end) {
            (Value::Number(begin), Value::Number(end)) => {
                let slice = vm.with_gc_check(|alloc| {
                    Value::Array(
                        alloc
                            .arrays
                            .slice(handle, begin as isize, Some(end as isize)),
                    )
                });
                Ok(Some(slice))
            }
            (Value::Number(begin), Value::Nil) => {
                let slice = vm.with_gc_check(|alloc| {
                    Value::Array(alloc.arrays.slice(handle, begin as isize, None))
                });
                Ok(Some(slice))
            }
            (Value::Nil, Value::Nil) => {
                let clone =
                    vm.with_gc_check(|alloc| Value::Array(alloc.arrays.shallow_copy(handle)));
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
    args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match (receiver, args.first()) {
        (Value::Array(handle), Some(Value::Number(index))) => {
            Ok(Some(vm.alloc.arrays.get(handle, index.trunc() as isize)))
        }
        (Value::Array(_), _) => Err(NativeFunctionError::new(
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
    args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match (receiver, args.first()) {
        (Value::Array(handle1), Some(Value::Array(handle2))) => Ok(Some(Value::Array(
            vm.alloc.arrays.concat(handle1, *handle2),
        ))),
        (Value::Array(_), _) => Err(NativeFunctionError::new(
            "An array can only be concatenated with another array.",
        )),
        _ => Err(NativeFunctionError(format!(
            "Expected array but recieved {}.",
            receiver.to_type_string()
        ))),
    }
}

pub fn qang_array_construct(
    args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match args.first().copied().unwrap_or(Value::Nil) {
        Value::Number(length) => {
            // TODO verify length is positive.
            Ok(Some(Value::Array(
                vm.alloc.arrays.create_array(length.trunc() as usize),
            )))
        }
        _ => Err(NativeFunctionError::new("Expected length to be a number.")),
    }
}

pub fn qang_hash(args: &[Value], _vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied();

    match value {
        None => Err("No value provided to hash.".into()),
        Some(value) => Ok(Some(value.hash().into())),
    }
}

pub fn qang_call(
    receiver: Value,
    args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match receiver {
        Value::Closure(handle) | Value::BoundMethod(handle) | Value::BoundIntrinsic(handle) => {
            let arg_count = args.len();
            // - push args
            for value in args.iter().rev() {
                push_value!(vm, *value).map_err(|e| NativeFunctionError(e.message))?;
            }
            let function = Value::Closure(handle);
            // - push function
            push_value!(vm, function).map_err(|e| NativeFunctionError(e.message))?;
            // - call call_value with function and arg_count
            vm.call_value(function, arg_count)
                .map_err(|e| NativeFunctionError(e.message))?;
            // - pop and return value
            Ok(Some(pop_value!(vm)))
        }
        _ => Err(NativeFunctionError::new(
            "'call' can only be used on functions.",
        )),
    }
}

pub fn qang_apply(
    receiver: Value,
    args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    match (receiver, args.first().copied().unwrap_or(Value::Nil)) {
        (
            Value::Closure(handle) | Value::BoundMethod(handle) | Value::BoundIntrinsic(handle),
            Value::Array(array_handle),
        ) => {
            let arg_count = vm.alloc.arrays.length(array_handle);
            for value in vm.alloc.arrays.iter(array_handle).rev() {
                push_value!(vm, value).map_err(|e| NativeFunctionError(e.message))?;
            }
            let function = Value::Closure(handle);
            // - push function
            push_value!(vm, function).map_err(|e| NativeFunctionError(e.message))?;
            // - call call_value with function and arg_count
            vm.call_value(function, arg_count)
                .map_err(|e| NativeFunctionError(e.message))?;
            // - pop and return value
            Ok(Some(pop_value!(vm)))
        }
        (Value::Closure(_) | Value::BoundMethod(_) | Value::BoundIntrinsic(_), _) => {
            Err(NativeFunctionError::new(
                "'apply' must be called with one argument and it must be an array.",
            ))
        }
        _ => Err(NativeFunctionError::new(
            "'apply' can only be used on functions.",
        )),
    }
}
