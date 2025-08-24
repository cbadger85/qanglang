use crate::{NativeFunctionError, Value, Vm};

pub fn qang_assert(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let assertion = args
        .first()
        .ok_or(NativeFunctionError::new("No arguments provided."))?;

    if assertion.is_truthy() {
        return Ok(None);
    }

    let message = args
        .get(1)
        .map(|v| v.to_display_string(vm.allocator()))
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
            .map(|v| v.to_display_string(vm.allocator()))
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
                .map(|v| v.to_display_string(vm.allocator()))
                .unwrap_or_else(|| "Expected function to throw, but did not.".to_string());

            Err(NativeFunctionError::new(&message))
        }
        Err(_) => Ok(None),
    }
}

pub fn qang_print(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied().unwrap_or(Value::Nil);
    let value = value.to_display_string(vm.allocator());
    print!("{}", value);
    Ok(None)
}

pub fn qang_println(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied().unwrap_or(Value::Nil);
    let value = value.to_display_string(vm.allocator());
    println!("{}", value);
    Ok(None)
}

pub fn qang_typeof(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    // TODO delete this function later when the `is` operator is implemented.
    let value = args.first().copied().unwrap_or(Value::Nil);
    let value_string = value.to_type_string();
    let handle = vm.allocator_mut().strings.intern(value_string);

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

    let value = value.to_display_string(vm.allocator());
    let value_handle = vm.allocator_mut().strings.intern(&value);

    Ok(Some(Value::String(value_handle)))
}

pub fn qang_string_to_uppercase(
    receiver: Value,
    _args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    if let Value::String(handle) = receiver {
        let uppercase_string = &vm.allocator().strings.get_string(handle).to_uppercase();
        let uppercase_handle = vm.allocator_mut().strings.intern(uppercase_string);

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
        let lowercase_string = &vm.allocator().strings.get_string(handle).to_lowercase();
        let lowercase_handle = vm.allocator_mut().strings.intern(lowercase_string);

        Ok(Some(Value::String(lowercase_handle)))
    } else {
        Err(NativeFunctionError(format!(
            "Expected string but recieved {}.",
            receiver.to_type_string()
        )))
    }
}

pub fn qang_hash(args: &[Value], _vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied();

    match value {
        None => Err("No value provided to hash.".into()),
        Some(value) => Ok(Some(value.hash().into())),
    }
}
