use crate::{NativeFunctionError, Value, Vm};

pub fn kang_assert(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let assertion = args
        .get(0)
        .ok_or(NativeFunctionError::new("No arguments provided."))?;
    let is_true = vm.is_truthy(*assertion);

    if is_true {
        return Ok(None);
    }

    let message = args
        .get(1)
        .and_then(|v| v.into_string(vm.heap()).ok())
        .unwrap_or("Assertion failed.".into());

    Err(NativeFunctionError(message.into_string()))
}

pub fn kang_assert_eq(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    if args.len() < 2 {
        return Err("Must provide two arguments.".into());
    }

    let a = args[0];
    let b = args[1];

    if a != b {
        let message = args
            .get(1)
            .and_then(|v| v.into_string(vm.heap()).ok())
            .unwrap_or("Assertion failed.".into());

        Err(NativeFunctionError(message.into_string()))
    } else {
        Ok(None)
    }
}

pub fn kang_print(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.get(0).copied().unwrap_or(Value::Nil);
    let value = value.to_display_string(&vm.heap());
    print!("{}", value);
    Ok(None)
}

pub fn kang_println(args: &[Value], vm: &mut Vm) -> Result<Option<Value>, NativeFunctionError> {
    let value = args.get(0).copied().unwrap_or(Value::Nil);
    let value = value.to_display_string(&vm.heap());
    println!("{}", value);
    Ok(None)
}
