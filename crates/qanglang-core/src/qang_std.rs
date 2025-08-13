use crate::{FunctionValueKind, NativeFunctionError, Value, Vm};

pub fn qang_assert(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let assertion = args
        .first()
        .ok_or(NativeFunctionError::new("No arguments provided."))?;

    if assertion.is_truthy() {
        return Ok(None);
    }

    let message = args
        .get(1)
        .and_then(|v| v.into_string(vm.heap()).ok())
        .unwrap_or("Assertion failed.".into());

    Err(NativeFunctionError(message.into_string()))
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
            .and_then(|v| v.into_string(vm.heap()).ok())
            .unwrap_or("Assertion failed.".into());

        Err(NativeFunctionError(message.into_string()))
    } else {
        Ok(None)
    }
}

#[allow(dead_code)] // TODO add this to the VM after closures are added.
pub fn qang_assert_throws(
    args: &[Value],
    vm: &mut Vm,
) -> Result<Option<Value>, NativeFunctionError> {
    let assertion = args
        .first()
        .ok_or(NativeFunctionError::new("No arguments provided."))?;

    let function_handle = match assertion {
        Value::Function(FunctionValueKind::Closure(function_handle)) => *function_handle,
        _ => return Err("First argument must be a function.".into()),
    };

    let result = vm.call_function(function_handle, Vec::new());

    match result {
        Ok(_) => {
            let message = args
                .get(1)
                .and_then(|v| v.into_string(vm.heap()).ok())
                .unwrap_or("Expected function to throw, but did not.".into());

            Err(NativeFunctionError::new(&message))
        }
        Err(_) => Ok(None),
    }
}

pub fn qang_print(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied().unwrap_or(Value::Nil);
    let value = value.to_display_string(vm.heap());
    print!("{}", value);
    Ok(None)
}

pub fn qang_println(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied().unwrap_or(Value::Nil);
    let value = value.to_display_string(vm.heap());
    println!("{}", value);
    Ok(None)
}

pub fn qang_typeof(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.first().copied().unwrap_or(Value::Nil);

    let handle = vm.heap_mut().intern_string(value.to_type_string().into());

    Ok(Some(Value::String(handle)))
}

pub fn system_time(_args: &[Value], _vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .map_err(|_| NativeFunctionError::new("Unable to get system time."))?;

    let time: f64 = since_the_epoch.as_millis() as f64;

    // println!("naive time is: {}", time);

    Ok(Some(time.into()))
}

mod tests {
    #[test]
    fn test_qang_assert() {}
}
