use crate::{
    CompilerPipeline, NativeFunctionError, QangProgram, QangRuntimeError, SourceMap, Value, Vm,
    backend::{
        compiler::STACK_MAX,
        value::{
            ARRAY_TYPE_STRING, BOOLEAN_TYPE_STRING, CLASS_TYPE_STRING, FUNCTION_TYPE_STRING,
            MODULE_TYPE_STRING, NIL_TYPE_STRING, NUMBER_TYPE_STRING, OBJECT_TYPE_STRING,
            STRING_TYPE_STRING,
        },
        vm::RuntimeResult,
    },
    peek_value, pop_value, push_value,
};

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

impl Vm {
    pub fn with_stdlib(mut self) -> Self {
        if self.load_stdlib().is_err() {
            // TODO
            // If stdlib fails to load, continue without it
            // In production, you might want to handle this differently
        }
        self
    }

    fn load_stdlib(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        self.load_constants();
        let stdlib_source = include_str!("stdlib.ql");
        let source_map = SourceMap::from_source(stdlib_source.to_owned());

        let program = CompilerPipeline::new()
            .compile(source_map, &mut self.alloc)
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
        self.alloc.strings.intern(MODULE_TYPE_STRING);
        self.alloc.strings.intern("MODULE");
    }

    pub(crate) fn handle_function_intrinsic_call(
        &mut self,
        receiver: Value,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        match receiver {
            Value::Closure(_)
            | Value::BoundIntrinsic(_)
            | Value::BoundMethod(_)
            | Value::NativeFunction(_) => {
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
        match receiver {
            Value::Closure(_)
            | Value::BoundIntrinsic(_)
            | Value::BoundMethod(_)
            | Value::NativeFunction(_) => {
                if arg_count != 1 {
                    return Err(QangRuntimeError::new(
                        "'apply' must be called with one argument and it must be an array."
                            .to_string(),
                        self.state.get_previous_loc(),
                    ));
                }

                let array_arg = peek_value!(self, 0);
                match array_arg {
                    Value::Array(array_handle) => {
                        // Pop the array argument
                        pop_value!(self);

                        // Replace the bound intrinsic on the stack with the actual function
                        self.state.stack[self.state.stack_top - 1] = receiver;

                        let array_length = self.alloc.arrays.length(array_handle);
                        for value in self.alloc.arrays.iter(array_handle) {
                            push_value!(self, value)?;
                        }

                        self.call_value(receiver, array_length)
                    }
                    _ => Err(QangRuntimeError::new(
                        "'apply' must be called with one argument and it must be an array."
                            .to_string(),
                        self.state.get_previous_loc(),
                    )),
                }
            }
            _ => Err(QangRuntimeError::new(
                "'apply' can only be used on functions.".to_string(),
                self.state.get_previous_loc(),
            )),
        }
    }
}
