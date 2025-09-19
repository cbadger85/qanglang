use std::collections::VecDeque;

use rustc_hash::{FxBuildHasher, FxHashMap};

#[cfg(debug_assertions)]
use crate::debug::disassemble_instruction;
use crate::{
    BoundIntrinsicObject, BoundMethodObject, ClassHandle, ClosureHandle, HashMapHandle,
    HeapAllocator, NativeFn, NativeFunctionError, NativeFunctionHandle, NativeFunctionObject,
    QangProgram, QangRuntimeError, StringHandle, Value, ValueKind,
    backend::{
        chunk::{OpCode, SourceLocation},
        compiler::{FRAME_MAX, STACK_MAX},
        module_resolver::ModuleResolver,
        object::{ClosureObject, FunctionObject, IntrinsicKind, IntrinsicMethod, UpvalueSlot},
        qang_std::{
            qang_array_construct, qang_assert, qang_assert_eq, qang_assert_throws, qang_hash,
            qang_print, qang_println, qang_system_time, qang_to_string, qang_typeof,
        },
        value::{
            ARRAY_TYPE_STRING, BOOLEAN_TYPE_STRING, CLASS_TYPE_STRING, FUNCTION_TYPE_STRING,
            MODULE_TYPE_STRING, NIL_TYPE_STRING, NUMBER_TYPE_STRING, OBJECT_TYPE_STRING,
            STRING_TYPE_STRING,
        },
    },
    debug_log,
};

#[derive(Debug, Clone)]
pub struct BinaryOperationError(pub String);

impl BinaryOperationError {
    pub fn new(message: &str) -> Self {
        Self(message.to_string())
    }

    fn into_qang_error(self, loc: SourceLocation) -> QangRuntimeError {
        QangRuntimeError::new(self.0, loc)
    }
}

impl From<&'_ str> for BinaryOperationError {
    fn from(value: &'_ str) -> Self {
        BinaryOperationError::new(value)
    }
}

pub type RuntimeResult<T> = Result<T, QangRuntimeError>;

type StackSlot = usize;
type UpvalueIndex = usize;
use crate::memory::OpenUpvalueTracker;
type OpenUpvalueEntry = (StackSlot, OpenUpvalueTracker);

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
#[repr(u8)]
pub enum Keyword {
    Number,
    String,
    Nil,
    Boolean,
    Function,
    Class,
    Array,
    Object,
    Init,
    Call,
    Apply,
    Module,
}

#[derive(Debug, Clone, Copy)]
pub struct PropertyCache {
    pub object_type: CachedObjectType,
    pub property_name: StringHandle,
    pub table_handle: HashMapHandle,
    pub generation: u32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CachedObjectType {
    Instance(ClassHandle),
    ObjectLiteral,
    Module,
}

impl Default for PropertyCache {
    fn default() -> Self {
        Self {
            object_type: CachedObjectType::ObjectLiteral,
            property_name: StringHandle::default(),
            table_handle: HashMapHandle::default(),
            generation: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MethodCache {
    pub object_type: CachedObjectType,
    pub method_name: StringHandle,
    pub table_handle: HashMapHandle,
    pub cached_method: Value,
    pub generation: u32,
}

impl Default for MethodCache {
    fn default() -> Self {
        Self {
            object_type: CachedObjectType::ObjectLiteral,
            method_name: StringHandle::default(),
            table_handle: HashMapHandle::default(),
            cached_method: Value::nil(),
            generation: 0,
        }
    }
}

#[macro_export]
macro_rules! push_value {
    ($vm:expr, $value:expr) => {
        if $vm.state.stack_top >= STACK_MAX {
            Err($crate::QangRuntimeError::new(
                format!(
                    "Stack overflow: maximum stack size of {} exceeded",
                    STACK_MAX
                ),
                $vm.state.get_current_loc(),
            ))
        } else {
            $vm.state.stack[$vm.state.stack_top] = $value;
            $vm.state.stack_top += 1;
            Ok(())
        }
    };
}

#[macro_export]
macro_rules! pop_value {
    ($vm:expr) => {{
        debug_assert!(
            $vm.state.stack_top > 0,
            "Stack underflow: unexpected empty stack."
        );
        $vm.state.stack_top -= 1;
        $vm.state.stack[$vm.state.stack_top]
    }};
}

#[macro_export]
macro_rules! peek_value {
    ($vm:expr, $distance:expr) => {
        if $vm.state.stack_top > $distance {
            $vm.state
                .stack
                .get($vm.state.stack_top - 1 - $distance)
                .copied()
                .unwrap_or(Value::nil())
        } else {
            Value::nil()
        }
    };
}

#[macro_export]
macro_rules! read_string {
    ($vm:expr) => {
        match $vm.state.read_constant().as_string() {
            Some(handle) => handle,
            _ => panic!("Expected string constant"),
        }
    };
}

#[macro_export]
macro_rules! read_string_16 {
    ($vm:expr) => {
        match $vm.state.read_constant_16().as_string() {
            Some(handle) => handle,
            _ => panic!("Expected string constant"),
        }
    };
}

#[derive(Debug, Clone, Default)]
pub(crate) struct CallFrame {
    pub closure: ClosureHandle,
    pub ip: usize,
    pub value_slot: usize,
    pub module_export_target: Option<HashMapHandle>,
}

#[derive(Clone)]
pub(crate) struct VmState {
    pub stack_top: usize,
    pub frame_count: usize,
    pub stack: Vec<Value>,
    pub frames: [CallFrame; FRAME_MAX],
    globals: FxHashMap<StringHandle, Value>,
    intrinsics: FxHashMap<IntrinsicKind, IntrinsicMethod>,
    open_upvalues: Vec<OpenUpvalueEntry>,
    current_function_ptr: *const FunctionObject,
    keywords: FxHashMap<Keyword, StringHandle>,
    pub modules: ModuleResolver,
    function_module_context: Option<HashMapHandle>,
    property_cache: [PropertyCache; Self::PROPERTY_CACHE_SIZE],
    method_cache: [MethodCache; Self::METHOD_CACHE_SIZE],
    cache_generation: u32,
    arg_buffer: [Value; 256],
}

impl VmState {
    const PROPERTY_CACHE_SIZE: usize = 256;
    const METHOD_CACHE_SIZE: usize = 256;

    fn new(
        globals: FxHashMap<StringHandle, Value>,
        intrinsics: FxHashMap<IntrinsicKind, IntrinsicMethod>,
        keywords: FxHashMap<Keyword, StringHandle>,
    ) -> Self {
        Self {
            frame_count: 0,
            stack_top: 0,
            stack: vec![Value::default(); STACK_MAX],
            frames: std::array::from_fn(|_| CallFrame::default()),
            globals,
            intrinsics,
            open_upvalues: Vec::with_capacity(8),
            current_function_ptr: std::ptr::null(),
            keywords,
            modules: ModuleResolver::default(),
            function_module_context: None,
            property_cache: [PropertyCache::default(); Self::PROPERTY_CACHE_SIZE],
            method_cache: [MethodCache::default(); Self::METHOD_CACHE_SIZE],
            cache_generation: 0,
            arg_buffer: [Value::default(); 256],
        }
    }

    fn invalidate_caches(&mut self) {
        self.cache_generation = self.cache_generation.wrapping_add(1);
    }

    fn get_property_cache_index(&self) -> usize {
        let ip = self.frames[self.frame_count - 1].ip;
        let closure_handle = self.frames[self.frame_count - 1].closure;

        let cache_key = (ip as u64) ^ closure_handle.as_raw();
        cache_key as usize % Self::PROPERTY_CACHE_SIZE
    }

    fn get_method_cache_index(&self) -> usize {
        let ip = self.frames[self.frame_count - 1].ip;
        let closure_handle = self.frames[self.frame_count - 1].closure;

        let cache_key = (ip as u64) ^ closure_handle.as_raw();
        cache_key as usize % Self::METHOD_CACHE_SIZE
    }

    fn get_current_function(&self) -> &FunctionObject {
        debug_assert!(
            !self.current_function_ptr.is_null(),
            "Function pointer is null"
        );
        unsafe { &*self.current_function_ptr }
    }

    pub fn get_current_loc(&self) -> SourceLocation {
        self.get_loc_at(self.frames[self.frame_count - 1].ip)
    }

    pub fn get_previous_loc(&self) -> SourceLocation {
        if self.frames[self.frame_count - 1].ip > 0 {
            self.get_loc_at(self.frames[self.frame_count - 1].ip - 1)
        } else {
            SourceLocation::default()
        }
    }

    pub fn get_loc_at(&self, index: usize) -> SourceLocation {
        self.get_current_function()
            .chunk
            .locs
            .get(index)
            .copied()
            .unwrap_or_default()
    }

    fn read_byte(&mut self) -> u8 {
        let frame = unsafe { self.frames.get_unchecked_mut(self.frame_count - 1) };
        debug_assert!(
            !self.current_function_ptr.is_null(),
            "Function pointer is null"
        );

        let code = unsafe { &(*self.current_function_ptr).chunk.code };
        debug_assert!(frame.ip < code.len(), "IP out of bounds");
        let byte = unsafe { *code.get_unchecked(frame.ip) };
        frame.ip += 1;
        byte
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        debug_assert!(
            !self.current_function_ptr.is_null(),
            "Function pointer is null"
        );
        let constants = unsafe { &(*self.current_function_ptr).chunk.constants };
        debug_assert!(index < constants.len(), "Constant index out of bounds");
        constants[index]
    }

    fn read_constant_16(&mut self) -> Value {
        let index = self.read_short();
        debug_assert!(
            !self.current_function_ptr.is_null(),
            "Function pointer is null"
        );
        let constants = unsafe { &(*self.current_function_ptr).chunk.constants };
        debug_assert!(index < constants.len(), "Constant index out of bounds");
        constants[index]
    }

    fn read_short(&mut self) -> usize {
        let high_byte = self.read_byte() as usize;
        let low_byte = self.read_byte() as usize;
        (high_byte << 8) | low_byte
    }
}

#[derive(Clone)]
pub struct Vm {
    pub is_debug: bool,
    pub is_gc_enabled: bool,
    pub(crate) state: VmState,
    pub alloc: HeapAllocator,
}

impl Vm {
    pub(crate) fn with_gc_check<T>(&mut self, op: impl FnOnce(&mut HeapAllocator) -> T) -> T {
        if self.is_gc_enabled && self.alloc.should_collect_garbage() {
            self.collect_garbage();
        }
        op(&mut self.alloc)
    }

    pub fn new(mut alloc: HeapAllocator) -> Self {
        let mut keywords = FxHashMap::with_hasher(FxBuildHasher);
        keywords.insert(Keyword::Apply, alloc.strings.intern("apply"));
        keywords.insert(Keyword::Call, alloc.strings.intern("call"));
        let init_handle = alloc.strings.intern("init");
        keywords.insert(Keyword::Init, init_handle);
        let mut globals = FxHashMap::with_capacity_and_hasher(64, FxBuildHasher);

        let nil_type_value_handle = alloc.strings.intern(NIL_TYPE_STRING);
        keywords.insert(Keyword::Nil, nil_type_value_handle);
        globals.insert(
            alloc.strings.intern("NIL"),
            Value::string(nil_type_value_handle),
        );
        let boolean_type_value_handle = alloc.strings.intern(BOOLEAN_TYPE_STRING);
        keywords.insert(Keyword::Boolean, boolean_type_value_handle);
        globals.insert(
            alloc.strings.intern("BOOLEAN"),
            Value::string(boolean_type_value_handle),
        );
        let number_type_value_handle = alloc.strings.intern(NUMBER_TYPE_STRING);
        keywords.insert(Keyword::Number, number_type_value_handle);
        globals.insert(
            alloc.strings.intern("NUMBER"),
            Value::string(number_type_value_handle),
        );
        let string_type_value_handle = alloc.strings.intern(STRING_TYPE_STRING);
        keywords.insert(Keyword::String, string_type_value_handle);
        globals.insert(
            alloc.strings.intern("STRING"),
            Value::string(string_type_value_handle),
        );
        let function_type_value_handle = alloc.strings.intern(FUNCTION_TYPE_STRING);
        keywords.insert(Keyword::Function, function_type_value_handle);
        globals.insert(
            alloc.strings.intern("FUNCTION"),
            Value::string(function_type_value_handle),
        );
        let class_type_value_handle = alloc.strings.intern(CLASS_TYPE_STRING);
        keywords.insert(Keyword::Class, class_type_value_handle);
        globals.insert(
            alloc.strings.intern("CLASS"),
            Value::string(class_type_value_handle),
        );
        let object_type_value_handle = alloc.strings.intern(OBJECT_TYPE_STRING);
        keywords.insert(Keyword::Object, object_type_value_handle);
        globals.insert(
            alloc.strings.intern("OBJECT"),
            Value::string(object_type_value_handle),
        );
        let array_type_value_handle = alloc.strings.intern(ARRAY_TYPE_STRING);
        keywords.insert(Keyword::Array, array_type_value_handle);
        globals.insert(
            alloc.strings.intern("ARRAY"),
            Value::string(array_type_value_handle),
        );
        let module_type_value_handle = alloc.strings.intern(MODULE_TYPE_STRING);
        keywords.insert(Keyword::Module, module_type_value_handle);
        globals.insert(
            alloc.strings.intern("MODULE"),
            Value::string(module_type_value_handle),
        );

        let vm = Self {
            is_debug: false,
            is_gc_enabled: true,
            state: VmState::new(globals, Self::load_intrinsics(&mut alloc), keywords),
            alloc,
        };

        vm.add_native_function("assert", 2, qang_assert)
            .add_native_function("assert_eq", 3, qang_assert_eq)
            .add_native_function("assert_throws", 2, qang_assert_throws)
            .add_native_function("print", 1, qang_print)
            .add_native_function("println", 1, qang_println)
            .add_native_function("system_time", 0, qang_system_time)
            .add_native_function("typeof", 1, qang_typeof)
            .add_native_function("to_string", 1, qang_to_string)
            .add_native_function("hash", 1, qang_hash)
            .add_native_function("array_of_length", 1, qang_array_construct)
            .with_stdlib()
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    pub fn set_gc_status(mut self, is_enabled: bool) -> Self {
        self.is_gc_enabled = is_enabled;
        self
    }

    pub fn add_native_function(mut self, name: &str, arity: usize, function: NativeFn) -> Self {
        let identifier_handle = self.alloc.strings.intern(name);
        let native_function = NativeFunctionObject {
            name_handle: identifier_handle,
            arity,
            function,
        };
        let handle = self.alloc.allocate_native_function(native_function);

        self.state
            .globals
            .insert(identifier_handle, Value::native_function(handle));

        self
    }

    pub fn interpret(&mut self, program: QangProgram) -> RuntimeResult<()> {
        let function_handle = program.get_handle();
        self.state.modules = program.into_modules();
        let upvalue_count = self.alloc.get_function(function_handle).upvalue_count;

        let handle = self.alloc.closures.allocate_closure(ClosureObject::new(
            function_handle,
            upvalue_count,
            None,
        ));
        push_value!(self, Value::closure(handle))?;
        self.call(handle, 0)?;

        #[cfg(feature = "profiler")]
        coz::scope!("vm_interpret");

        let _ = self
            .run()
            .map_err(|e| e.with_stack_trace(self.get_stack_trace()))?;

        #[cfg(feature = "profiler")]
        coz::progress!("execution_complete");

        Ok(())
    }

    fn run(&mut self) -> RuntimeResult<Value> {
        loop {
            #[cfg(feature = "profiler")]
            coz::progress!("vm_instructions");

            #[cfg(debug_assertions)]
            {
                if self.is_debug {
                    self.debug_print();
                }
            }

            let opcode: OpCode = self.state.read_byte().into();

            match opcode {
                OpCode::Constant => {
                    let constant = self.state.read_constant();
                    push_value!(self, constant)?;
                }
                OpCode::Constant16 => {
                    let constant = self.state.read_constant_16();
                    push_value!(self, constant)?;
                }
                OpCode::Negate => {
                    if let Some(number) = peek_value!(self, 0).as_number() {
                        if let Some(stack_value) =
                            self.state.stack.get_mut(self.state.stack_top - 1)
                        {
                            *stack_value = Value::number(-number);
                        }
                    } else {
                        return Err(QangRuntimeError::new(
                            "Operand must be a number.".to_string(),
                            self.state.get_previous_loc(),
                        ));
                    }
                }
                OpCode::Not => {
                    let value = peek_value!(self, 0);
                    if let Some(stack_value) = self.state.stack.get_mut(self.state.stack_top - 1) {
                        *stack_value = (!value.is_truthy()).into();
                    }
                }
                OpCode::True => {
                    push_value!(self, Value::boolean(true))?;
                }
                OpCode::False => {
                    push_value!(self, Value::boolean(false))?;
                }
                OpCode::Nil => {
                    push_value!(self, Value::nil())?;
                }
                OpCode::Add => {
                    self.binary_operation(|a, b, alloc| match (a.kind(), b.kind()) {
                        (ValueKind::Number(num1), ValueKind::Number(num2)) => {
                            Ok(Value::number(num1 + num2))
                        }
                        (ValueKind::String(handle1), ValueKind::String(handle2)) => {
                            #[cfg(feature = "profiler")]
                            coz::scope!("string_concatenation");
                            Ok(Value::string(
                                alloc.strings.concat_strings(handle1, handle2),
                            ))
                        }
                        (ValueKind::Array(handle1), ValueKind::Array(handle2)) => {
                            Ok(Value::array(alloc.arrays.concat(handle1, handle2)))
                        }
                        (ValueKind::Number(_), _) => {
                            Err(format!("Cannot add number to {}.", b.to_type_string())
                                .as_str()
                                .into())
                        }
                        (ValueKind::String(_), _) => {
                            Err(format!("Cannot add string to {}.", b.to_type_string())
                                .as_str()
                                .into())
                        }
                        (ValueKind::Array(_), _) => {
                            Err(format!("Cannot add an array to {}.", b.to_type_string())
                                .as_str()
                                .into())
                        }
                        (_, ValueKind::Number(_)) => {
                            Err(format!("Cannot add {} to number.", a.to_type_string())
                                .as_str()
                                .into())
                        }
                        (_, ValueKind::String(_)) => {
                            Err(format!("Cannot add {} to string.", a.to_type_string())
                                .as_str()
                                .into())
                        }
                        (_, ValueKind::Array(_)) => {
                            Err(format!("Cannot add {} to an array.", a.to_type_string())
                                .as_str()
                                .into())
                        }
                        _ => Err("Both operands must be a numbers, strings or arrays.".into()),
                    })?;
                }
                OpCode::Subtract => self.binary_operation(|a, b, _allocator| {
                    match (a.as_number(), b.as_number()) {
                        (Some(num1), Some(num2)) => Ok((num1 - num2).into()),
                        _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                    }
                })?,
                OpCode::Multiply => self.binary_operation(|a, b, _allocator| {
                    match (a.as_number(), b.as_number()) {
                        (Some(num1), Some(num2)) => Ok((num1 * num2).into()),
                        _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                    }
                })?,
                OpCode::Divide => self.binary_operation(|a, b, _allocator| {
                    match (a.as_number(), b.as_number()) {
                        (Some(num1), Some(num2)) => {
                            if num2 == 0.0 {
                                Err(BinaryOperationError::new("Cannot divide by zero."))
                            } else {
                                Ok((num1 / num2).into())
                            }
                        }
                        _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                    }
                })?,
                OpCode::Modulo => self.binary_operation(|a, b, _allocator| {
                    match (a.as_number(), b.as_number()) {
                        (Some(num1), Some(num2)) => Ok((num1 % num2).into()),
                        _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                    }
                })?,
                OpCode::Equal => self.binary_operation(|a, b, _allocator| Ok((a == b).into()))?,
                OpCode::Greater => self.binary_operation(|a, b, _allocator| {
                    match (a.as_number(), b.as_number()) {
                        (Some(num1), Some(num2)) => Ok((num1 > num2).into()),
                        _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                    }
                })?,
                OpCode::GreaterEqual => self.binary_operation(|a, b, _allocator| {
                    match (a.as_number(), b.as_number()) {
                        (Some(num1), Some(num2)) => Ok((num1 >= num2).into()),
                        _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                    }
                })?,
                OpCode::Less => self.binary_operation(|a, b, _allocator| {
                    match (a.as_number(), b.as_number()) {
                        (Some(num1), Some(num2)) => Ok((num1 < num2).into()),
                        _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                    }
                })?,
                OpCode::LessEqual => self.binary_operation(|a, b, _allocator| {
                    match (a.as_number(), b.as_number()) {
                        (Some(num1), Some(num2)) => Ok((num1 <= num2).into()),
                        _ => Err(BinaryOperationError::new("Both operands must be a number.")),
                    }
                })?,
                OpCode::Is => {
                    let b = pop_value!(self);
                    let a = pop_value!(self);

                    let result = match (a.kind(), b.kind()) {
                        (ValueKind::Array(_), ValueKind::String(string_handle)) => {
                            let keyword_handle = *self
                                .state
                                .keywords
                                .get(&Keyword::Array)
                                .expect("expected keyword");
                            keyword_handle == string_handle
                        }
                        (ValueKind::Number(_), ValueKind::String(string_handle)) => {
                            let keyword_handle = *self
                                .state
                                .keywords
                                .get(&Keyword::Number)
                                .expect("expected keyword");
                            keyword_handle == string_handle
                        }
                        (ValueKind::String(_), ValueKind::String(string_handle)) => {
                            let keyword_handle = *self
                                .state
                                .keywords
                                .get(&Keyword::String)
                                .expect("expected keyword");
                            keyword_handle == string_handle
                        }
                        (ValueKind::Nil, ValueKind::String(string_handle)) => {
                            let keyword_handle = *self
                                .state
                                .keywords
                                .get(&Keyword::Nil)
                                .expect("expected keyword");
                            keyword_handle == string_handle
                        }
                        (ValueKind::True | ValueKind::False, ValueKind::String(string_handle)) => {
                            let keyword_handle = *self
                                .state
                                .keywords
                                .get(&Keyword::Boolean)
                                .expect("expected keyword");
                            keyword_handle == string_handle
                        }
                        (ValueKind::Class(_), ValueKind::String(string_handle)) => {
                            let keyword_handle = *self
                                .state
                                .keywords
                                .get(&Keyword::Class)
                                .expect("expected keyword");
                            keyword_handle == string_handle
                        }
                        (ValueKind::ObjectLiteral(_), ValueKind::String(string_handle)) => {
                            let keyword_handle = *self
                                .state
                                .keywords
                                .get(&Keyword::Object)
                                .expect("expected keyword");
                            keyword_handle == string_handle
                        }
                        (
                            ValueKind::FunctionDecl(_)
                            | ValueKind::NativeFunction(_)
                            | ValueKind::Closure(_)
                            | ValueKind::BoundMethod(_)
                            | ValueKind::BoundIntrinsic(_),
                            ValueKind::String(string_handle),
                        ) => {
                            let keyword_handle = *self
                                .state
                                .keywords
                                .get(&Keyword::Function)
                                .expect("expected keyword");
                            keyword_handle == string_handle
                        }
                        (ValueKind::Instance(_), ValueKind::String(string_handle)) => {
                            let keyword_handle = *self
                                .state
                                .keywords
                                .get(&Keyword::Object)
                                .expect("expected keyword");
                            keyword_handle == string_handle
                        }
                        (ValueKind::Instance(instance_handle), ValueKind::Class(clazz_handle)) => {
                            let instance_of_handle = self.alloc.get_instance(instance_handle).clazz;

                            if instance_of_handle == clazz_handle {
                                true
                            } else {
                                let mut current_handle = instance_of_handle;
                                loop {
                                    let current_class = self.alloc.get_class(current_handle);
                                    if let Some(super_handle) = current_class.super_clazz {
                                        if super_handle == clazz_handle {
                                            break true;
                                        }
                                        current_handle = super_handle;
                                    } else {
                                        break false;
                                    }
                                }
                            }
                        }
                        _ => false,
                    };

                    push_value!(self, result.into())?;
                }
                OpCode::Pop => {
                    pop_value!(self);
                }
                OpCode::DefineGlobal => {
                    let identifier_handle = read_string!(self);
                    let value = pop_value!(self);

                    if let Some(module_target) =
                        self.state.frames[self.state.frame_count - 1].module_export_target
                    {
                        self.alloc.tables.insert(
                            module_target,
                            Value::string(identifier_handle),
                            value,
                        );
                    } else {
                        self.state.globals.insert(identifier_handle, value);
                    }
                }

                OpCode::DefineGlobal16 => {
                    let identifier_handle = read_string_16!(self);
                    let value = pop_value!(self);

                    if let Some(module_target) =
                        self.state.frames[self.state.frame_count - 1].module_export_target
                    {
                        self.alloc.tables.insert(
                            module_target,
                            Value::string(identifier_handle),
                            value,
                        );
                    } else {
                        self.state.globals.insert(identifier_handle, value);
                    }
                }
                OpCode::SetGlobal => {
                    let identifier_handle = read_string!(self);
                    self.set_global(identifier_handle)?;
                }

                OpCode::SetGlobal16 => {
                    let identifier_handle = read_string_16!(self);
                    self.set_global(identifier_handle)?;
                }
                OpCode::GetGlobal => {
                    let identifier_handle = read_string!(self);
                    let value = self.get_global(identifier_handle)?;
                    push_value!(self, value)?;
                }

                OpCode::GetGlobal16 => {
                    let identifier_handle = read_string_16!(self);
                    let value = self.get_global(identifier_handle)?;
                    push_value!(self, value)?;
                }
                OpCode::GetLocal => {
                    let slot = self.state.read_byte();
                    let absolute_slot =
                        self.state.frames[self.state.frame_count - 1].value_slot + slot as usize;
                    debug_assert!(
                        absolute_slot < STACK_MAX,
                        "Local slot {} out of bounds",
                        absolute_slot
                    );

                    let value = self.state.stack[absolute_slot];
                    push_value!(self, value)?;
                }
                OpCode::SetLocal => {
                    let slot = self.state.read_byte();
                    let value = peek_value!(self, 0);
                    let absolute_slot =
                        self.state.frames[self.state.frame_count - 1].value_slot + slot as usize;

                    self.state.stack[absolute_slot] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.state.read_short();
                    if !peek_value!(self, 0).is_truthy() {
                        self.state.frames[self.state.frame_count - 1].ip += offset;
                    }
                }
                OpCode::JumpIfNil => {
                    let offset = self.state.read_short();
                    if peek_value!(self, 0).is_nil() {
                        self.state.frames[self.state.frame_count - 1].ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = self.state.read_short();
                    self.state.frames[self.state.frame_count - 1].ip += offset;
                }
                OpCode::Loop => {
                    let offset = self.state.read_short();
                    self.state.frames[self.state.frame_count - 1].ip -= offset;
                }
                OpCode::Call => {
                    let arg_count = self.state.read_byte() as usize;
                    let function_value = peek_value!(self, arg_count);
                    self.call_value(function_value, arg_count)?;
                }
                OpCode::TailCall => {
                    let arg_count = self.state.read_byte() as usize;
                    let function_value = peek_value!(self, arg_count);
                    self.tail_call_value(function_value, arg_count)?;
                }
                OpCode::Closure => {
                    let constant = self.state.read_constant();
                    self.create_closure(constant)?;
                }
                OpCode::Closure16 => {
                    let constant = self.state.read_constant_16();
                    self.create_closure(constant)?;
                }
                OpCode::GetUpvalue => {
                    let slot = self.state.read_byte() as usize;
                    let current_closure_handle =
                        self.state.frames[self.state.frame_count - 1].closure;
                    let upvalue = self
                        .alloc
                        .closures
                        .get_upvalue(current_closure_handle, slot)
                        .unwrap_or(UpvalueSlot::Open(0));

                    match upvalue {
                        UpvalueSlot::Open(stack_slot) => {
                            let value = self.state.stack[stack_slot];
                            push_value!(self, value)?;
                        }
                        UpvalueSlot::Closed(value_handle) => {
                            let value = *self.alloc.get_upvalue(value_handle);
                            push_value!(self, value)?;
                        }
                    }
                }
                OpCode::SetUpvalue => {
                    let slot = self.state.read_byte() as usize;
                    let value = peek_value!(self, 0);
                    let current_closure_handle =
                        self.state.frames[self.state.frame_count - 1].closure;

                    let upvalue = self
                        .alloc
                        .closures
                        .get_upvalue(current_closure_handle, slot)
                        .unwrap_or(UpvalueSlot::Open(0));
                    match upvalue {
                        UpvalueSlot::Open(stack_slot) => {
                            self.state.stack[stack_slot] = value;
                        }
                        UpvalueSlot::Closed(value_handle) => {
                            *self.alloc.get_upvalue_mut(value_handle) = value;
                        }
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalue(self.state.stack_top - 1);
                    pop_value!(self);
                }
                OpCode::Class => {
                    let identifier_handle = read_string!(self);
                    let class_handle: crate::arena::Index =
                        self.with_gc_check(|alloc| alloc.allocate_class(identifier_handle));
                    push_value!(self, Value::class(class_handle))?
                }
                OpCode::Class16 => {
                    let identifier_handle = read_string_16!(self);
                    let class_handle: crate::arena::Index =
                        self.with_gc_check(|alloc| alloc.allocate_class(identifier_handle));
                    push_value!(self, Value::class(class_handle))?
                }
                OpCode::GetProperty => {
                    let object = peek_value!(self, 0);
                    let identifier = read_string!(self);
                    self.get_property_value(object, false, identifier)?;
                }
                OpCode::GetProperty16 => {
                    let object = peek_value!(self, 0);
                    let identifier = read_string_16!(self);
                    self.get_property_value(object, false, identifier)?;
                }
                OpCode::SetProperty => {
                    let identifier = read_string!(self);
                    self.set_property(identifier)?
                }
                OpCode::SetProperty16 => {
                    let identifier = read_string_16!(self);
                    self.set_property(identifier)?
                }
                OpCode::GetOptionalProperty => {
                    let object = peek_value!(self, 0);
                    let identifier = read_string!(self);
                    self.get_property_value(object, true, identifier)?;
                }
                OpCode::GetOptionalProperty16 => {
                    let object = peek_value!(self, 0);
                    let identifier = read_string_16!(self);
                    self.get_property_value(object, true, identifier)?;
                }
                OpCode::Method => {
                    let identifier_handle = read_string!(self);
                    self.define_method(identifier_handle)?;
                }
                OpCode::Method16 => {
                    let identifier_handle = read_string_16!(self);
                    self.define_method(identifier_handle)?;
                }
                OpCode::InitField => {
                    let identifier_handle = read_string!(self);
                    let value = pop_value!(self);
                    self.init_field(identifier_handle, value)?;
                }
                OpCode::InitField16 => {
                    let identifier_handle = read_string_16!(self);
                    let value = pop_value!(self);
                    self.init_field(identifier_handle, value)?;
                }
                OpCode::Invoke => {
                    let method_handle = read_string!(self);
                    let arg_count = self.state.read_byte();
                    self.invoke(method_handle, arg_count as usize)?;
                }
                OpCode::Invoke16 => {
                    let method_handle = read_string_16!(self);
                    let arg_count = self.state.read_byte();
                    self.invoke(method_handle, arg_count as usize)?;
                }
                OpCode::GetSuper => {
                    let property_handle = read_string!(self);
                    self.get_super(property_handle)?;
                }
                OpCode::GetSuper16 => {
                    let property_handle = read_string_16!(self);
                    self.get_super(property_handle)?;
                }
                OpCode::SuperInvoke => {
                    let method_handle = read_string!(self);
                    self.invoke_super(method_handle)?;
                }
                OpCode::SuperInvoke16 => {
                    let method_handle = read_string_16!(self);
                    self.invoke_super(method_handle)?;
                }
                OpCode::Inherit => {
                    let superclass = peek_value!(self, 0);
                    let subclass = peek_value!(self, 1);

                    match (superclass.as_class(), subclass.as_class()) {
                        (Some(superclass_handle), Some(subclass_handle)) => {
                            let subclass = self.alloc.get_class_mut(subclass_handle);
                            subclass.super_clazz = Some(superclass_handle);
                            let subclass = self.alloc.get_class(subclass_handle);
                            let superclass = self.alloc.get_class(superclass_handle);
                            let subclass_value_table = subclass.value_table;
                            let superclass_value_table = superclass.value_table;
                            let superclass_method_table = superclass.method_table;
                            let subclass_method_table = subclass.method_table;
                            self.alloc
                                .tables
                                .copy_into(superclass_method_table, subclass_method_table);
                            self.alloc
                                .tables
                                .copy_into(superclass_value_table, subclass_value_table);
                            pop_value!(self);
                            Ok(())
                        }
                        (Some(_), _) => Err(QangRuntimeError::new(
                            "Invalid subclass.".to_string(),
                            self.state.get_previous_loc(),
                        )),
                        (_, Some(_)) => Err(QangRuntimeError::new(
                            "Super class must be a class.".to_string(),
                            self.state.get_previous_loc(),
                        )),
                        _ => Err(QangRuntimeError::new(
                            "Invalid class declaration.".to_string(),
                            self.state.get_previous_loc(),
                        )),
                    }?
                }
                OpCode::ArrayLiteral => {
                    let length = self.state.read_byte() as usize;
                    let array = self.with_gc_check(|alloc| alloc.arrays.create_array(length));
                    for i in 0..length {
                        let value = pop_value!(self);
                        self.alloc.arrays.insert(array, i, value);
                    }
                    push_value!(self, Value::array(array))?;
                }
                OpCode::GetArrayIndex => {
                    let index = pop_value!(self);
                    match (index.as_number(), peek_value!(self, 0).as_array()) {
                        (Some(index), Some(handle)) => {
                            let value = self.alloc.arrays.get(handle, index.trunc() as isize);
                            pop_value!(self);
                            push_value!(self, value)?;
                        }
                        (_, Some(_)) => {
                            return Err(QangRuntimeError::new(
                                "An array can only be indexed by a number.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                        (Some(_), _) => {
                            return Err(QangRuntimeError::new(
                                "Only arrays can be indexed.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                        _ => {
                            return Err(QangRuntimeError::new(
                                "Invalid operation.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                    }
                }
                OpCode::SetArrayIndex => {
                    let value = pop_value!(self);
                    let index = pop_value!(self);
                    match (index.as_number(), peek_value!(self, 0).as_array()) {
                        (Some(index), Some(handle)) => {
                            let is_within_bounds =
                                self.alloc
                                    .arrays
                                    .insert(handle, index.trunc() as usize, value);

                            if !is_within_bounds {
                                return Err(QangRuntimeError::new(
                                    "Index out of bounds.".to_string(),
                                    self.state.get_previous_loc(),
                                ));
                            }
                            pop_value!(self);
                            push_value!(self, value)?;
                        }
                        (_, Some(_)) => {
                            return Err(QangRuntimeError::new(
                                "An array can only be indexed by a number.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                        (Some(_), _) => {
                            return Err(QangRuntimeError::new(
                                "Only arrays can be indexed.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                        _ => {
                            return Err(QangRuntimeError::new(
                                "Invalid operation.".to_string(),
                                self.state.get_previous_loc(),
                            ));
                        }
                    }
                }
                OpCode::ObjectLiteral => {
                    let handle = self.alloc.tables.new_hashmap();
                    let length = self.state.read_byte() as usize;
                    for _ in 0..length {
                        let value = pop_value!(self);
                        let key = pop_value!(self);
                        self.alloc.tables.insert(handle, key, value);
                    }
                    push_value!(self, Value::object_literal(handle))?;
                }
                OpCode::Module => {
                    let module_path = read_string!(self);
                    self.load_module(module_path)?;
                }
                OpCode::Module16 => {
                    let module_path = read_string_16!(self);
                    self.load_module(module_path)?;
                }
                OpCode::Return => {
                    let result = pop_value!(self);
                    let value_slot = self.state.frames[self.state.frame_count - 1].value_slot;

                    self.close_upvalue(value_slot);
                    self.state.frame_count -= 1;

                    #[cfg(feature = "profiler")]
                    coz::progress!("function_returns");

                    if self.state.frame_count > 0 {
                        let prev_closure = self
                            .alloc
                            .closures
                            .get_closure(self.state.frames[self.state.frame_count - 1].closure);
                        self.state.function_module_context = prev_closure.module_context;
                    } else {
                        self.state.function_module_context = None;
                    }

                    if self.state.frame_count == 0 {
                        return Ok(result);
                    }

                    let previous_frame = &self.state.frames[self.state.frame_count - 1];
                    let previous_closure = self.alloc.closures.get_closure(previous_frame.closure);
                    let previous_function = self.alloc.get_function(previous_closure.function);
                    self.state.current_function_ptr = previous_function as *const FunctionObject;

                    self.state.stack_top = value_slot + 1;
                    self.state.stack[value_slot] = result;
                }
                OpCode::ModuleReturn => {
                    let result = pop_value!(self);
                    let value_slot = self.state.frames[self.state.frame_count - 1].value_slot;

                    let module_instance =
                        self.state.frames[self.state.frame_count - 1].module_export_target;

                    self.close_upvalue(value_slot);
                    self.state.frame_count -= 1;

                    #[cfg(feature = "profiler")]
                    coz::progress!("function_returns");

                    if let Some(module_instance) = module_instance {
                        self.state.stack[self.state.stack_top - 1] = Value::module(module_instance);
                    } else if self.state.frame_count == 0 {
                        return Ok(result);
                    }

                    if self.state.frame_count == 0 {
                        return Ok(result);
                    }

                    let previous_frame = &self.state.frames[self.state.frame_count - 1];
                    let previous_closure = self.alloc.closures.get_closure(previous_frame.closure);
                    let previous_function = self.alloc.get_function(previous_closure.function);
                    self.state.current_function_ptr = previous_function as *const FunctionObject;
                }
            };
        }
    }

    fn load_module(&mut self, module_path: StringHandle) -> RuntimeResult<()> {
        if let Some(instance_handle) = self.state.modules.get_instance_handle(module_path) {
            push_value!(self, Value::module(instance_handle))?;
            return Ok(());
        }

        let module_function = self
            .state
            .modules
            .get_module_handle(module_path, self.state.get_previous_loc())?;

        let module_instance = self.with_gc_check(|alloc| alloc.tables.new_hashmap());

        self.state
            .modules
            .add_instance(module_path, module_instance);

        let module_closure = self.with_gc_check(|alloc| {
            alloc
                .closures
                .allocate_closure(ClosureObject::new(module_function, 0, None))
        });

        push_value!(self, Value::closure(module_closure))?;
        self.call(module_closure, 0)?;

        self.state.frames[self.state.frame_count - 1].module_export_target = Some(module_instance);

        Ok(())
    }

    fn create_closure(&mut self, constant: Value) -> RuntimeResult<()> {
        let handle = match constant.as_function() {
            Some(handle) => handle,
            _ => {
                return Err(QangRuntimeError::new(
                    "Expected function.".to_string(),
                    self.state.get_previous_loc(),
                ));
            }
        };
        let upvalue_count = self.alloc.get_function(handle).upvalue_count;
        let module_context = if self.state.frame_count > 0 {
            self.state.frames[self.state.frame_count - 1].module_export_target
        } else {
            None
        };
        let closure_handle = self.with_gc_check(|alloc| {
            alloc.closures.allocate_closure(ClosureObject::new(
                handle,
                upvalue_count,
                module_context,
            ))
        });

        push_value!(self, Value::closure(closure_handle))?;

        for i in 0..self
            .alloc
            .closures
            .get_closure(closure_handle)
            .upvalue_count
        {
            let is_local = self.state.read_byte() != 0;
            let index = self.state.read_byte() as usize;

            if is_local {
                let stack_slot = self.state.frames[self.state.frame_count - 1].value_slot + index;
                self.capture_upvalue(stack_slot, closure_handle, i);
            } else {
                let current_closure_handle = self.state.frames[self.state.frame_count - 1].closure;
                if let Some(current_upvalue) = self
                    .alloc
                    .closures
                    .get_upvalue(current_closure_handle, index)
                {
                    self.alloc
                        .closures
                        .set_upvalue(closure_handle, i, current_upvalue);
                }
            }
        }

        Ok(())
    }

    fn define_method(&mut self, name: StringHandle) -> RuntimeResult<()> {
        if let Some(method) = peek_value!(self, 0).as_closure()
            && let Some(clazz_handle) = peek_value!(self, 1).as_class()
        {
            let clazz = self.alloc.get_class(clazz_handle);
            self.alloc.set_class_method(
                clazz.method_table,
                Value::string(name),
                Value::closure(method),
            );
            pop_value!(self);
        }

        Ok(())
    }

    fn init_field(&mut self, field_name: StringHandle, value: Value) -> RuntimeResult<()> {
        if let Some(clazz_handle) = peek_value!(self, 0).as_class() {
            let clazz = self.alloc.get_class(clazz_handle);
            self.alloc
                .set_class_method(clazz.value_table, Value::string(field_name), value);
        }

        Ok(())
    }

    fn bind_method(&mut self, clazz_handle: ClassHandle, method_name: Value) -> RuntimeResult<()> {
        if let Some(method_handle) = method_name.as_string() {
            let init_handle = *self
                .state
                .keywords
                .get(&Keyword::Init)
                .expect("Expected keyword.");
            if method_handle == init_handle {
                pop_value!(self);
                push_value!(self, Value::nil())?;
                return Ok(());
            }
        }

        let clazz = self.alloc.get_class(clazz_handle);
        if let Some(closure) = self
            .alloc
            .get_class_method(clazz.method_table, method_name)
            .and_then(|v| v.as_closure())
        {
            let receiver = peek_value!(self, 0);
            let bound = BoundMethodObject::new(receiver, closure);
            let handle = self.with_gc_check(|alloc| alloc.allocate_bound_method(bound));
            pop_value!(self);
            push_value!(self, Value::bound_method(handle))?;
        } else {
            pop_value!(self);
            push_value!(self, Value::nil())?;
        }
        Ok(())
    }

    fn bind_intrinsic_method(
        &mut self,
        method_name: StringHandle,
        kind: IntrinsicKind,
        receiver: Value,
    ) -> RuntimeResult<()> {
        let intrinsic = *self.state.intrinsics.get(&kind).ok_or_else(|| {
            QangRuntimeError::new(
                "invalid method call.".to_string(),
                self.state.get_previous_loc(),
            )
        })?;

        let final_intrinsic = if receiver.is_nil() {
            match intrinsic {
                IntrinsicMethod::Call => IntrinsicMethod::NilSafeCall,
                IntrinsicMethod::Apply => IntrinsicMethod::NilSafeApply,
                other => other,
            }
        } else {
            intrinsic
        };

        let bound = BoundIntrinsicObject::new(receiver, final_intrinsic, method_name);
        let handle = self.with_gc_check(|alloc| alloc.allocate_bound_intrinsic(bound));
        pop_value!(self);
        push_value!(self, Value::bound_intrinsic(handle))?;
        Ok(())
    }

    fn bind_super_method(
        &mut self,
        method_table_handle: HashMapHandle,
        method_name: StringHandle,
    ) -> RuntimeResult<bool> {
        if let Some(closure) = self
            .alloc
            .get_class_method(method_table_handle, Value::string(method_name))
            .and_then(|v| v.as_closure())
        {
            let receiver = peek_value!(self, 0);
            let bound = BoundMethodObject::new(receiver, closure);
            let handle = self.with_gc_check(|alloc| alloc.allocate_bound_method(bound));
            pop_value!(self); // Pop 'this'
            push_value!(self, Value::bound_method(handle))?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn close_upvalue(&mut self, last_slot: StackSlot) {
        let mut i = self.state.open_upvalues.len();
        while i > 0 {
            i -= 1;
            let (stack_slot, upvalue_ref) = &self.state.open_upvalues[i];

            if *stack_slot >= last_slot {
                let value = self.state.stack[*stack_slot];

                let value_handle = self.alloc.allocate_upvalue(value);

                for (closure_handle, upvalue_index) in
                    upvalue_ref.iter_all_entries(&self.alloc.upvalue_overflow)
                {
                    self.alloc.closures.set_upvalue(
                        closure_handle,
                        upvalue_index,
                        UpvalueSlot::Closed(value_handle),
                    );
                }

                self.state.open_upvalues.remove(i);
            }
        }
    }

    fn capture_upvalue(
        &mut self,
        stack_slot: StackSlot,
        closure_handle: ClosureHandle,
        upvalue_index: UpvalueIndex,
    ) {
        for (open_slot, upvalue_ref) in self.state.open_upvalues.iter_mut() {
            if *open_slot == stack_slot {
                upvalue_ref.add_closure(
                    closure_handle,
                    upvalue_index,
                    &mut self.alloc.upvalue_overflow,
                );

                self.alloc.closures.set_upvalue(
                    closure_handle,
                    upvalue_index,
                    UpvalueSlot::Open(stack_slot),
                );
                return;
            }
        }

        let mut upvalue_ref = OpenUpvalueTracker::new();
        upvalue_ref.add_closure(
            closure_handle,
            upvalue_index,
            &mut self.alloc.upvalue_overflow,
        );

        self.alloc.closures.set_upvalue(
            closure_handle,
            upvalue_index,
            UpvalueSlot::Open(stack_slot),
        );

        self.state.open_upvalues.push((stack_slot, upvalue_ref));
    }

    fn binary_operation<F>(&mut self, op: F) -> RuntimeResult<()>
    where
        F: FnOnce(Value, Value, &mut HeapAllocator) -> Result<Value, BinaryOperationError>,
    {
        #[cfg(feature = "profiler")]
        coz::scope!("binary_operation");

        let b = pop_value!(self);
        let a = pop_value!(self);

        let value = op(a, b, &mut self.alloc)
            .map_err(|e: BinaryOperationError| e.into_qang_error(self.state.get_previous_loc()))?;

        push_value!(self, value)?;
        Ok(())
    }

    pub(crate) fn call_value(&mut self, value: Value, arg_count: usize) -> RuntimeResult<()> {
        match value.kind() {
            ValueKind::Closure(handle) => self.call(handle, arg_count),
            ValueKind::NativeFunction(function) => self.call_native_function(function, arg_count),
            ValueKind::Class(handle) => {
                let clazz = self.alloc.get_class(handle);
                let clazz_method_table = clazz.method_table;
                let clazz_value_table = clazz.value_table;
                let instance_handle = self.with_gc_check(|alloc| alloc.allocate_instance(handle));
                self.state.stack[self.state.stack_top - arg_count - 1] =
                    Value::instance(instance_handle);
                let instance_table = self.alloc.get_instance(instance_handle).table;

                self.alloc
                    .tables
                    .copy_into(clazz_value_table, instance_table);

                let constructor_handle = *self
                    .state
                    .keywords
                    .get(&Keyword::Init)
                    .expect("Expected keyword.");
                if let Some(constructor) = self
                    .alloc
                    .get_class_method(clazz_method_table, Value::string(constructor_handle))
                    .and_then(|v| v.as_closure())
                {
                    self.call(constructor, arg_count)?;
                } else {
                    // No constructor found, pop the arguments from the stack
                    for _ in 0..arg_count {
                        pop_value!(self);
                    }
                }
                Ok(())
            }
            ValueKind::BoundMethod(handle) => {
                let bound_method = self.alloc.get_bound_method(handle);

                self.state.stack[self.state.stack_top - arg_count - 1] = bound_method.receiver;
                self.call(bound_method.closure, arg_count)
            }
            ValueKind::BoundIntrinsic(handle) => {
                let bound_intrinsic = self.alloc.get_bound_intrinsic(handle);
                self.call_intrinsic_method(
                    bound_intrinsic.receiver,
                    bound_intrinsic.method,
                    arg_count,
                )
            }
            _ => {
                let value_str = value.to_display_string(&self.alloc);
                Err(QangRuntimeError::new(
                    format!("Identifier '{}' not callable.", value_str).to_string(),
                    self.state.get_previous_loc(),
                ))
            }
        }
    }

    fn invoke(&mut self, method_handle: StringHandle, arg_count: usize) -> RuntimeResult<()> {
        let receiver = peek_value!(self, arg_count);

        match receiver.kind() {
            ValueKind::Instance(instance_handle) => {
                let instance = self.alloc.get_instance(instance_handle);

                if let Some(value) = self
                    .alloc
                    .get_instance_field(instance.table, Value::string(method_handle))
                {
                    self.state.stack[self.state.stack_top - arg_count - 1] = value;
                    return self.call_value(value, arg_count);
                }

                let init_handle = *self
                    .state
                    .keywords
                    .get(&Keyword::Init)
                    .expect("Expected keyword.");
                if method_handle == init_handle {
                    return Err(QangRuntimeError::new(
                        "Cannot call constructor 'init' on an instance.".to_string(),
                        self.state.get_previous_loc(),
                    ));
                }

                // TODO add instance methods to method cache.
                self.invoke_from_class(instance.clazz, method_handle, arg_count)
            }

            ValueKind::ObjectLiteral(handle) => {
                let object_type = CachedObjectType::ObjectLiteral;

                if let Some(method_value) =
                    self.get_method_cached(object_type, handle, method_handle)
                {
                    self.state.stack[self.state.stack_top - arg_count - 1] = method_value;
                    self.call_value(method_value, arg_count)
                } else {
                    Err(QangRuntimeError::new(
                        format!(
                            "Property '{}' does not exist on object.",
                            self.alloc.strings.get_string(method_handle)
                        ),
                        self.state.get_previous_loc(),
                    ))
                }
            }

            ValueKind::Module(handle) => {
                let object_type = CachedObjectType::Module;

                if let Some(method_value) =
                    self.get_method_cached(object_type, handle, method_handle)
                {
                    self.state.stack[self.state.stack_top - arg_count - 1] = method_value;
                    self.call_value(method_value, arg_count)
                } else {
                    Err(QangRuntimeError::new(
                        format!(
                            "Property '{}' does not exist on module.",
                            self.alloc.strings.get_string(method_handle)
                        ),
                        self.state.get_previous_loc(),
                    ))
                }
            }

            ValueKind::String(_) => {
                let intrinsic = *self
                    .state
                    .intrinsics
                    .get(&IntrinsicKind::String(method_handle))
                    .ok_or_else(|| {
                        QangRuntimeError::new(
                            "invalid method call.".to_string(),
                            self.state.get_previous_loc(),
                        )
                    })?;
                self.call_intrinsic_method(receiver, intrinsic, arg_count)
            }

            ValueKind::Array(_) => {
                let intrinsic = *self
                    .state
                    .intrinsics
                    .get(&IntrinsicKind::Array(method_handle))
                    .ok_or_else(|| {
                        QangRuntimeError::new(
                            "invalid method call.".to_string(),
                            self.state.get_previous_loc(),
                        )
                    })?;
                self.call_intrinsic_method(receiver, intrinsic, arg_count)
            }

            ValueKind::Number(_) => {
                let intrinsic = *self
                    .state
                    .intrinsics
                    .get(&IntrinsicKind::Number(method_handle))
                    .ok_or_else(|| {
                        QangRuntimeError::new(
                            "invalid method call.".to_string(),
                            self.state.get_previous_loc(),
                        )
                    })?;
                self.call_intrinsic_method(receiver, intrinsic, arg_count)
            }

            ValueKind::Closure(_)
            | ValueKind::NativeFunction(_)
            | ValueKind::BoundIntrinsic(_)
            | ValueKind::BoundMethod(_) => {
                let intrinsic = *self
                    .state
                    .intrinsics
                    .get(&IntrinsicKind::Function(method_handle))
                    .ok_or_else(|| {
                        QangRuntimeError::new(
                            "invalid method call.".to_string(),
                            self.state.get_previous_loc(),
                        )
                    })?;
                self.call_intrinsic_method(receiver, intrinsic, arg_count)
            }

            _ => Err(QangRuntimeError::new(
                format!(
                    "Cannot invoke {}, no methods exist.",
                    receiver.to_type_string()
                ),
                self.state.get_previous_loc(),
            )),
        }
    }

    fn call(&mut self, closure_handle: ClosureHandle, arg_count: usize) -> RuntimeResult<()> {
        #[cfg(feature = "profiler")]
        coz::scope!("call_function");

        #[cfg(feature = "profiler")]
        coz::progress!("before_call");

        let closure = self.alloc.closures.get_closure(closure_handle);
        let function = self.alloc.get_function(closure.function);

        let final_arg_count = {
            if arg_count < function.arity {
                let arity = function.arity;
                for _ in arg_count..arity {
                    push_value!(self, Value::nil())?;
                }
                arity
            } else {
                arg_count
            }
        };

        if self.state.frame_count >= FRAME_MAX {
            let loc = if self.state.frame_count > 0 {
                self.state.get_previous_loc()
            } else {
                SourceLocation::default()
            };
            return Err(QangRuntimeError::new(
                "Maximum call depth exceeded.".to_string(),
                loc,
            ));
        }

        self.state.frame_count += 1;

        let value_slot = self.state.stack_top - final_arg_count - 1;
        let call_frame = &mut self.state.frames[self.state.frame_count - 1];

        call_frame.value_slot = value_slot;
        call_frame.closure = closure_handle;
        call_frame.ip = 0;
        call_frame.module_export_target = None;
        self.state.current_function_ptr = function as *const FunctionObject;

        if let Some(module_context) = closure.module_context {
            self.state.function_module_context = Some(module_context);
        }

        #[cfg(feature = "profiler")]
        coz::progress!("after_call");

        Ok(())
    }

    fn tail_call_value(&mut self, callee: Value, arg_count: usize) -> RuntimeResult<()> {
        match callee.kind() {
            ValueKind::Closure(closure_handle) => self.tail_call(closure_handle, arg_count),
            ValueKind::NativeFunction(handle) => self.call_native_function(handle, arg_count),
            ValueKind::Class(_handle) => {
                panic!("Cannot tail call optimize class instancing.")
            }
            ValueKind::BoundMethod(handle) => {
                let bound_method = self.alloc.get_bound_method(handle);
                self.state.stack[self.state.stack_top - arg_count - 1] = bound_method.receiver;
                self.tail_call(bound_method.closure, arg_count)
            }
            ValueKind::BoundIntrinsic(handle) => {
                let bound_intrinsic = self.alloc.get_bound_intrinsic(handle);
                self.call_intrinsic_method(
                    bound_intrinsic.receiver,
                    bound_intrinsic.method,
                    arg_count,
                )
            }
            _ => {
                let value_str = callee.to_display_string(&self.alloc);
                Err(QangRuntimeError::new(
                    format!("Identifier '{}' not callable.", value_str).to_string(),
                    self.state.get_previous_loc(),
                ))
            }
        }
    }

    fn tail_call(&mut self, closure_handle: ClosureHandle, arg_count: usize) -> RuntimeResult<()> {
        let closure = self.alloc.closures.get_closure(closure_handle);
        let function_handle = closure.function;
        let function_arity = self.alloc.get_function(function_handle).arity;

        // Pad arguments if needed
        let final_arg_count = if arg_count < function_arity {
            for _ in arg_count..function_arity {
                push_value!(self, Value::nil())?;
            }
            function_arity
        } else {
            arg_count
        };

        // Get current frame info
        let current_frame = &self.state.frames[self.state.frame_count - 1];
        let current_value_slot = current_frame.value_slot;

        // Close upvalues that might be affected by the tail call
        // This ensures that any upvalues pointing to the current frame's locals
        // are properly closed before we reuse the frame
        self.close_upvalue(current_value_slot + 1);

        // New function and args are at top of stack
        // [... current frame locals] [new_func] [new_arg1] [new_arg2] <- stack_top
        let new_func_pos = self.state.stack_top - final_arg_count - 1;

        // Move new function + args to current frame's position
        for i in 0..=final_arg_count {
            // Include function itself
            self.state.stack[current_value_slot + i] = self.state.stack[new_func_pos + i];
        }

        // Reset stack_top to just after the new arguments
        self.state.stack_top = current_value_slot + final_arg_count + 1;

        // Update current frame (don't increment frame_count!)
        let current_frame = &mut self.state.frames[self.state.frame_count - 1];
        current_frame.closure = closure_handle;
        current_frame.ip = 0;

        // Get function pointer after all mutations are done
        let function = self.alloc.get_function(function_handle);
        self.state.current_function_ptr = function as *const FunctionObject;

        Ok(())
    }

    pub fn call_function(&mut self, handle: ClosureHandle, args: &[Value]) -> RuntimeResult<Value> {
        let saved_stack_top = self.state.stack_top;
        let saved_frame_count = self.state.frame_count;
        let saved_function_ptr = self.state.current_function_ptr;

        push_value!(self, Value::closure(handle))?;

        for value in args {
            push_value!(self, *value)?;
        }

        self.call(handle, args.len())?;

        let result = self.run();

        self.state.stack_top = saved_stack_top;
        self.state.frame_count = saved_frame_count;
        self.state.current_function_ptr = saved_function_ptr;

        result
    }

    pub fn get_function_args(&self, arg_count: usize) -> &[Value] {
        &self.state.arg_buffer[..arg_count]
    }

    fn call_native_function(
        &mut self,
        handle: NativeFunctionHandle,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        let function = self.alloc.get_native_function(handle);

        for i in (0..arg_count).rev() {
            if i < function.arity {
                self.state.arg_buffer[i] = pop_value!(self);
            } else {
                pop_value!(self); // discard values that are passed in but not needed by the function.
            }
        }

        pop_value!(self); // pop function off the stack now that it has been called.

        let value = (function.function)(arg_count, self)
            .map_err(|e: NativeFunctionError| {
                let loc = self.state.get_previous_loc();
                e.into_qang_error(loc)
            })?
            .unwrap_or_default();

        push_value!(self, value)?;
        self.state.arg_buffer.fill(Value::default());

        Ok(())
    }

    fn call_intrinsic_method(
        &mut self,
        receiver: Value,
        method: IntrinsicMethod,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        match method {
            IntrinsicMethod::Native { function, arity } => {
                for i in (0..arg_count).rev() {
                    if i < arity {
                        self.state.arg_buffer[i] = pop_value!(self);
                    } else {
                        pop_value!(self); // discard values that are passed in but not needed by the function.
                    }
                }

                pop_value!(self); // pop function off the stack now that it has been called.

                let value = function(receiver, arg_count, self)
                    .map_err(|e: NativeFunctionError| {
                        let loc = self.state.get_previous_loc();
                        e.into_qang_error(loc)
                    })?
                    .unwrap_or_default();

                push_value!(self, value)?;
                self.state.arg_buffer.fill(Value::default());

                Ok(())
            }
            IntrinsicMethod::Apply => self.handle_function_intrinsic_apply(receiver, arg_count),
            IntrinsicMethod::Call => self.handle_function_intrinsic_call(receiver, arg_count),
            IntrinsicMethod::NilSafeCall | IntrinsicMethod::NilSafeApply => {
                // For nil-safe call, consume arguments and return nil
                for _ in 0..arg_count {
                    pop_value!(self);
                }
                pop_value!(self); // pop the bound method
                push_value!(self, Value::nil())?;
                Ok(())
            }
        }
    }

    fn invoke_from_class(
        &mut self,
        clazz_handle: ClassHandle,
        method_handle: StringHandle,
        arg_count: usize,
    ) -> RuntimeResult<()> {
        let clazz = self.alloc.get_class(clazz_handle);
        if let Some(method) = self
            .alloc
            .get_class_method(clazz.method_table, Value::string(method_handle))
            .and_then(|v| v.as_closure())
        {
            self.call(method, arg_count)
        } else {
            // If method is `init` and it does not exist, do nothing (return nil)
            let init_handle = *self
                .state
                .keywords
                .get(&Keyword::Init)
                .expect("Expected keyword.");
            if method_handle == init_handle {
                // Pop arguments but leave receiver on stack
                for _ in 0..arg_count {
                    pop_value!(self);
                }
                // Push nil as return value
                push_value!(self, Value::nil())?;
                Ok(())
            } else {
                Err(QangRuntimeError::new(
                    format!(
                        "{} does not exist on class {}.",
                        Value::string(method_handle).to_display_string(&self.alloc),
                        Value::class(clazz_handle).to_display_string(&self.alloc),
                    ),
                    self.state.get_previous_loc(),
                ))
            }
        }
    }

    fn get_property_cached(
        &mut self,
        object_type: CachedObjectType,
        table_handle: HashMapHandle,
        property_name: StringHandle,
    ) -> Option<Value> {
        let cache_index = self.state.get_property_cache_index();

        if let Some(cached) = self.state.property_cache.get(cache_index)
            && cached.object_type == object_type
            && cached.property_name == property_name
            && cached.table_handle == table_handle
            && cached.generation == self.state.cache_generation
        {
            let key = Value::string(property_name);
            return self.alloc.tables.get(cached.table_handle, &key);
        }

        let key = Value::string(property_name);
        let value = self.alloc.tables.get(table_handle, &key);

        if let Some(cache_entry) = self.state.property_cache.get_mut(cache_index) {
            *cache_entry = PropertyCache {
                object_type,
                property_name,
                table_handle,
                generation: self.state.cache_generation,
            };
        }

        value
    }

    fn get_method_cached(
        &mut self,
        object_type: CachedObjectType,
        table_handle: HashMapHandle,
        method_name: StringHandle,
    ) -> Option<Value> {
        let cache_index = self.state.get_method_cache_index();

        if let Some(cached) = self.state.method_cache.get(cache_index)
            && cached.object_type == object_type
            && cached.method_name == method_name
            && cached.table_handle == table_handle
            && cached.generation == self.state.cache_generation
        {
            return Some(cached.cached_method);
        }

        let key = Value::string(method_name);
        if let Some(method_value) = self.alloc.tables.get(table_handle, &key) {
            if let Some(cache_entry) = self.state.method_cache.get_mut(cache_index) {
                *cache_entry = MethodCache {
                    object_type,
                    method_name,
                    table_handle,
                    cached_method: method_value,
                    generation: self.state.cache_generation,
                };
            }
            Some(method_value)
        } else {
            None
        }
    }

    fn get_property_value(
        &mut self,
        object: Value,
        optional: bool,
        identifier: StringHandle,
    ) -> RuntimeResult<()> {
        match object.kind() {
            ValueKind::Instance(instance_handle) => {
                let instance = self.alloc.get_instance(instance_handle);
                let clazz = instance.clazz;
                let object_type = CachedObjectType::Instance(clazz);
                let instance_table = instance.table;

                if let Some(value) =
                    self.get_property_cached(object_type, instance_table, identifier)
                {
                    pop_value!(self);
                    push_value!(self, value)?;
                } else {
                    let key = Value::string(identifier);
                    self.bind_method(clazz, key)?;
                }
            }

            ValueKind::ObjectLiteral(handle) => {
                let object_type = CachedObjectType::ObjectLiteral;

                let value = self
                    .get_property_cached(object_type, handle, identifier)
                    .unwrap_or_default();
                pop_value!(self);
                push_value!(self, value)?;
            }

            ValueKind::Module(handle) => {
                let object_type = CachedObjectType::Module;

                let value = self.get_property_cached(object_type, handle, identifier);

                pop_value!(self);
                match value {
                    Some(val) => push_value!(self, val)?,
                    None => {
                        if optional {
                            push_value!(self, Value::nil())?;
                        } else {
                            return Err(QangRuntimeError::new(
                                format!(
                                    "Property '{}' does not exist on module.",
                                    self.alloc.strings.get_string(identifier)
                                ),
                                self.state.get_previous_loc(),
                            ));
                        }
                    }
                }
            }

            ValueKind::Nil => {
                let call_handle = *self
                    .state
                    .keywords
                    .get(&Keyword::Call)
                    .expect("Expected identifier.");
                let apply_handle = *self
                    .state
                    .keywords
                    .get(&Keyword::Apply)
                    .expect("Expected identifier.");

                if identifier == call_handle || identifier == apply_handle {
                    let method = if identifier == call_handle {
                        IntrinsicMethod::NilSafeCall
                    } else {
                        IntrinsicMethod::NilSafeApply
                    };
                    let bound = BoundIntrinsicObject::new(object, method, identifier);
                    let handle = self.with_gc_check(|alloc| alloc.allocate_bound_intrinsic(bound));
                    pop_value!(self);
                    push_value!(self, Value::bound_intrinsic(handle))?;
                } else if optional {
                    pop_value!(self);
                    push_value!(self, Value::nil())?;
                } else {
                    return Err(QangRuntimeError::new(
                        format!("Cannot access property on {}.", object.to_type_string()),
                        self.state.get_previous_loc(),
                    ));
                }
            }

            ValueKind::String(_) => {
                self.bind_intrinsic_method(identifier, IntrinsicKind::String(identifier), object)?;
            }

            ValueKind::Array(_) => {
                self.bind_intrinsic_method(identifier, IntrinsicKind::Array(identifier), object)?;
            }

            ValueKind::Number(_) => {
                self.bind_intrinsic_method(identifier, IntrinsicKind::Number(identifier), object)?;
            }

            ValueKind::Closure(_) | ValueKind::BoundMethod(_) => {
                self.bind_intrinsic_method(
                    identifier,
                    IntrinsicKind::Function(identifier),
                    object,
                )?;
            }

            _ => {
                if optional {
                    pop_value!(self);
                    push_value!(self, Value::nil())?;
                } else {
                    return Err(QangRuntimeError::new(
                        format!("Cannot access property on {}", object.to_type_string()),
                        self.state.get_previous_loc(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn set_property(&mut self, identifier: StringHandle) -> RuntimeResult<()> {
        let instance = peek_value!(self, 1);
        match instance.kind() {
            ValueKind::Instance(instance_handle) => {
                let instance_obj = self.alloc.get_instance(instance_handle);
                let table_handle = instance_obj.table;
                let constant = Value::string(identifier);
                let value = peek_value!(self, 0);

                self.with_gc_check(|alloc| alloc.set_instance_field(table_handle, constant, value));

                let value = pop_value!(self);
                pop_value!(self);
                push_value!(self, value)?;
            }

            ValueKind::ObjectLiteral(obj_handle) => {
                let constant = Value::string(identifier);
                let value = peek_value!(self, 0);

                self.with_gc_check(|alloc| alloc.tables.insert(obj_handle, constant, value));

                let value = pop_value!(self);
                pop_value!(self);
                push_value!(self, value)?;
            }

            ValueKind::Module(module_handle) => {
                let constant = Value::string(identifier);
                let value = peek_value!(self, 0);

                self.with_gc_check(|alloc| alloc.tables.insert(module_handle, constant, value));

                let value = pop_value!(self);
                pop_value!(self);
                push_value!(self, value)?;
            }

            _ => {
                return Err(QangRuntimeError::new(
                    format!("Cannot set property on {}.", instance.to_type_string()),
                    self.state.get_previous_loc(),
                ));
            }
        }
        Ok(())
    }

    fn get_global(&self, identifier_handle: StringHandle) -> RuntimeResult<Value> {
        let key = Value::string(identifier_handle);

        if let Some(function_context) = self.state.function_module_context
            && let Some(value) = self.try_get_from_table(function_context, &key)
        {
            return Ok(value);
        }

        if self.state.frame_count > 0
            && let Some(module_target) =
                self.state.frames[self.state.frame_count - 1].module_export_target
            && let Some(value) = self.try_get_from_table(module_target, &key)
        {
            return Ok(value);
        }

        if self.has_module_context() {
            Ok(self.get_from_globals_or_nil(identifier_handle))
        } else {
            self.try_get_from_globals_strict(identifier_handle)
        }
    }

    fn try_get_from_table(&self, table_handle: HashMapHandle, key: &Value) -> Option<Value> {
        self.alloc.tables.get(table_handle, key)
    }

    fn has_module_context(&self) -> bool {
        let has_frame_module_context = if self.state.frame_count > 0 {
            self.state.frames[self.state.frame_count - 1]
                .module_export_target
                .is_some()
        } else {
            false
        };
        self.state.function_module_context.is_some() || has_frame_module_context
    }

    fn get_from_globals_or_nil(&self, identifier_handle: StringHandle) -> Value {
        self.state
            .globals
            .get(&identifier_handle)
            .copied()
            .unwrap_or(Value::nil())
    }

    fn try_get_from_globals_strict(&self, identifier_handle: StringHandle) -> RuntimeResult<Value> {
        self.state
            .globals
            .get(&identifier_handle)
            .copied()
            .ok_or_else(|| {
                let loc = self.state.get_previous_loc();
                let identifier_name = self.alloc.strings.get_string(identifier_handle);
                QangRuntimeError::new(format!("Undefined variable: {}.", identifier_name), loc)
            })
    }

    fn set_global(&mut self, identifier_handle: StringHandle) -> RuntimeResult<()> {
        let value = peek_value!(self, 0);
        let key = Value::string(identifier_handle);

        if let Some(function_context) = self.state.function_module_context
            && self.try_set_in_table(function_context, &key, value)
        {
            return Ok(());
        }

        if self.state.frame_count > 0
            && let Some(module_target) =
                self.state.frames[self.state.frame_count - 1].module_export_target
            && self.try_set_in_table(module_target, &key, value)
        {
            return Ok(());
        }

        if self.try_set_in_globals(identifier_handle, value) {
            return Ok(());
        }

        self.undefined_variable_error(identifier_handle)
    }

    fn try_set_in_table(&mut self, table_handle: HashMapHandle, key: &Value, value: Value) -> bool {
        if self.alloc.tables.get(table_handle, key).is_some() {
            self.alloc.tables.insert(table_handle, *key, value);
            true
        } else {
            false
        }
    }

    fn try_set_in_globals(&mut self, identifier_handle: StringHandle, value: Value) -> bool {
        if let std::collections::hash_map::Entry::Occupied(mut e) =
            self.state.globals.entry(identifier_handle)
        {
            e.insert(value);
            true
        } else {
            false
        }
    }

    fn undefined_variable_error(&self, identifier_handle: StringHandle) -> RuntimeResult<()> {
        let identifier_name = self.alloc.strings.get_string(identifier_handle);
        let loc = self.state.get_previous_loc();
        Err(QangRuntimeError::new(
            format!("Undefined variable: {}.", identifier_name),
            loc,
        ))
    }

    fn invoke_super(&mut self, method_handle: StringHandle) -> RuntimeResult<()> {
        let arg_count = self.state.read_byte() as usize;
        let superclass = pop_value!(self);

        if let Some(superclass_handle) = superclass.as_class() {
            self.invoke_from_class(superclass_handle, method_handle, arg_count)
        } else {
            Err(QangRuntimeError::new(
                "Super class must be a class.".to_string(),
                self.state.get_previous_loc(),
            ))
        }
    }

    fn get_super(&mut self, property_handle: StringHandle) -> RuntimeResult<()> {
        let superclass = pop_value!(self);

        if let Some(superclass_handle) = superclass.as_class() {
            let superclass_obj = self.alloc.get_class(superclass_handle);

            if let Some(field_value) = self
                .alloc
                .get_class_method(superclass_obj.value_table, Value::string(property_handle))
            {
                self.state.stack[self.state.stack_top - 1] = field_value; // replace 'this' with the value of the field.
            } else if !self.bind_super_method(superclass_obj.method_table, property_handle)? {
                pop_value!(self); // Pop 'this'
                push_value!(self, Value::nil())?;
            }

            Ok(())
        } else {
            Err(QangRuntimeError::new(
                "Super class must be a class.".to_string(),
                self.state.get_previous_loc(),
            ))
        }
    }

    pub fn gather_roots(&mut self) -> VecDeque<Value> {
        let capacity = self.state.stack_top
            + self.globals().len()
            + self.state.frame_count
            + self.state.open_upvalues.len()
            + self.state.modules.count()
            + self.state.arg_buffer.len();
        let mut roots = VecDeque::with_capacity(capacity);
        roots.extend(&self.state.stack[..self.state.stack_top]);
        roots.extend(self.globals().values());

        for frame in &self.state.frames[..self.state.frame_count] {
            roots.push_back(Value::closure(frame.closure));
        }

        for (_, upvalue_ref) in &self.state.open_upvalues {
            for (closure_handle, _) in upvalue_ref.iter() {
                roots.push_back(Value::closure(closure_handle));
            }
        }
        roots.extend(&self.state.arg_buffer);

        self.state.modules.gather_roots(&mut roots);

        roots
    }

    pub fn collect_garbage(&mut self) {
        debug_log!(self.is_debug, "--gc begin");

        self.state.invalidate_caches();

        for (_, upvalue_ref) in &self.state.open_upvalues {
            upvalue_ref.mark_overflow_chunks(&mut self.alloc.upvalue_overflow);
        }

        let roots = self.gather_roots();
        self.alloc.collect_garbage(roots);
        debug_log!(self.is_debug, "--gc end");
    }

    pub fn globals(&self) -> &FxHashMap<StringHandle, Value> {
        &self.state.globals
    }

    #[cfg(debug_assertions)]
    fn debug_print(&self) {
        print!("          ");
        for i in 0..self.state.stack_top {
            if let Some(value) = self.state.stack.get(i) {
                value.print(&self.alloc);
                print!(" ");
            }
        }
        println!();

        disassemble_instruction(
            &self.state.get_current_function().chunk,
            &self.alloc,
            self.state.frames[self.state.frame_count - 1].ip,
        );
    }
}
