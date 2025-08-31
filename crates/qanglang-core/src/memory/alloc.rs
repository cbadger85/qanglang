use crate::StringHandle;
use crate::memory::UpvalueOverflowArena;
use crate::memory::arena::{Arena, Index};
use crate::memory::array_arena::ArrayArena;
use crate::memory::closure_arena::ClosureArena;
use crate::memory::hashmap_arena::HashMapArena;
use crate::memory::object::{
    BoundIntrinsicObject, BoundMethodObject, ClassObject, InstanceObject, NativeFunctionObject,
};
use crate::memory::string_interner::StringInterner;
use crate::{FunctionObject, Upvalue, Value, debug_log};
use std::collections::VecDeque;

pub type UpvalueHandle = Index;

pub type ClassHandle = Index;

pub type InstanceHandle = Index;

pub type BoundMethodHandle = Index;

pub type BoundIntrinsicHandle = Index;

pub type FunctionHandle = u32;

pub type NativeFunctionHandle = u32;

const GC_HEAP_GROW_FACTOR: usize = 2;

#[derive(Debug, Default, Clone)]
pub struct HeapAllocator {
    functions: Vec<FunctionObject>,
    pub closures: ClosureArena,
    upvalues: Arena<Upvalue>,
    pub upvalue_overflow: UpvalueOverflowArena,
    pub strings: StringInterner,
    native_functions: Vec<NativeFunctionObject>,
    classes: Arena<ClassObject>,
    instances: Arena<InstanceObject>,
    bound_methods: Arena<BoundMethodObject>,
    bound_intrinsics: Arena<BoundIntrinsicObject>,
    pub tables: HashMapArena,
    pub arrays: ArrayArena,
    is_debug: bool,
    bytes_until_gc: usize,
}

impl HeapAllocator {
    pub fn new() -> Self {
        Self::with_capacity(1024)
    }

    pub fn with_capacity(initial_capacity: usize) -> Self {
        Self {
            functions: Vec::with_capacity(initial_capacity),
            closures: ClosureArena::with_capacity(initial_capacity),
            strings: StringInterner::new(),
            upvalues: Arena::with_capacity(initial_capacity),
            upvalue_overflow: UpvalueOverflowArena::with_capacity(initial_capacity / 4),
            native_functions: Vec::with_capacity(initial_capacity),
            tables: HashMapArena::with_capacity(initial_capacity),
            classes: Arena::with_capacity(initial_capacity),
            bound_methods: Arena::with_capacity(initial_capacity),
            bound_intrinsics: Arena::with_capacity(initial_capacity),
            instances: Arena::with_capacity(initial_capacity),
            arrays: ArrayArena::with_capacity(initial_capacity),
            is_debug: false,
            bytes_until_gc: 1024 * 1024,
        }
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self.closures = self.closures.set_debug(is_debug);
        self
    }

    pub fn set_bytes_until_gc(mut self, bytes: usize) -> Self {
        self.bytes_until_gc = bytes;
        self
    }

    pub fn allocate_upvalue(&mut self, value: Value) -> UpvalueHandle {
        let handle = self.upvalues.insert(Upvalue {
            value,
            is_marked: false,
        });
        debug_log!(
            self.is_debug,
            "Allocated {} byes for upvalue: {:?}",
            std::mem::size_of::<Upvalue>(),
            handle
        );
        handle
    }

    pub fn get_upvalue(&self, handle: UpvalueHandle) -> &Value {
        &self.upvalues[handle].value
    }

    pub fn get_upvalue_mut(&mut self, handle: UpvalueHandle) -> &mut Value {
        &mut self.upvalues[handle].value
    }

    pub fn free_upvalue(&mut self, handle: UpvalueHandle) {
        debug_log!(self.is_debug, "Freeing upvalue: {:?}", handle);
        self.upvalues.remove(handle);
        debug_log!(
            self.is_debug,
            "Freed {} byes for upvalue: {:?}",
            std::mem::size_of::<Upvalue>(),
            handle
        );
    }

    pub fn allocate_function(&mut self, function: FunctionObject) -> FunctionHandle {
        if self.functions.len() == self.functions.capacity() {
            let new_capacity = if self.functions.capacity() == 0 {
                64
            } else {
                self.functions.capacity() * 2
            };
            self.functions
                .reserve(new_capacity - self.functions.capacity());
        }
        self.functions.push(function);
        let handle = self.functions.len() - 1;

        debug_log!(
            self.is_debug,
            "Allocated {} bytes for function: {:?}",
            std::mem::size_of::<FunctionObject>(),
            handle
        );

        handle as u32
    }

    pub fn get_function(&self, handle: FunctionHandle) -> &FunctionObject {
        &self.functions[handle as usize]
    }

    pub fn iter_functions(&self) -> impl Iterator<Item = (usize, &FunctionObject)> {
        self.functions.iter().enumerate()
    }

    pub fn allocate_native_function(
        &mut self,
        function: NativeFunctionObject,
    ) -> NativeFunctionHandle {
        if self.native_functions.len() == self.native_functions.capacity() {
            let new_capacity = if self.native_functions.capacity() == 0 {
                64
            } else {
                self.native_functions.capacity() * 2
            };
            self.native_functions
                .reserve(new_capacity - self.native_functions.capacity());
        }
        self.native_functions.push(function);
        let handle = self.native_functions.len() - 1;

        debug_log!(
            self.is_debug,
            "Allocated {} bytes for native function: {:?}",
            std::mem::size_of::<NativeFunctionObject>(),
            handle
        );

        handle as u32
    }

    pub fn get_native_function(&self, handle: NativeFunctionHandle) -> &NativeFunctionObject {
        &self.native_functions[handle as usize]
    }

    pub fn allocate_class(&mut self, name: StringHandle) -> ClassHandle {
        let handle = self.classes.insert(ClassObject {
            name,
            method_table: self.tables.new_hashmap(),
            value_table: self.tables.new_hashmap(),
            super_clazz: None,
            is_marked: false,
        });
        debug_log!(
            self.is_debug,
            "Allocated {} byes for class: {:?}",
            std::mem::size_of::<ClassObject>(),
            handle
        );
        handle
    }

    pub fn get_class(&self, handle: ClassHandle) -> &ClassObject {
        &self.classes[handle]
    }

    pub fn get_class_mut(&mut self, handle: ClassHandle) -> &mut ClassObject {
        &mut self.classes[handle]
    }

    pub fn free_class(&mut self, handle: ClassHandle) {
        debug_log!(self.is_debug, "Freeing class: {:?}", handle);
        self.classes.remove(handle);
        debug_log!(
            self.is_debug,
            "Freed {} byes for class: {:?}",
            std::mem::size_of::<ClassObject>(),
            handle
        );
    }

    pub fn get_class_method(&self, handle: ClassHandle, key: Value) -> Option<Value> {
        self.tables.get(handle, &key)
    }

    pub fn set_class_method(&mut self, handle: ClassHandle, key: Value, value: Value) {
        self.tables.insert(handle, key, value);
    }

    pub fn allocate_instance(&mut self, clazz: ClassHandle) -> InstanceHandle {
        let handle = self.instances.insert(InstanceObject {
            clazz,
            table: self.tables.new_hashmap(),
            is_marked: false,
        });
        debug_log!(
            self.is_debug,
            "Allocated {} byes for instance: {:?}",
            std::mem::size_of::<InstanceObject>(),
            handle
        );
        handle
    }

    pub fn get_instance(&self, handle: InstanceHandle) -> &InstanceObject {
        &self.instances[handle]
    }

    pub fn get_instance_mut(&mut self, handle: InstanceHandle) -> &mut InstanceObject {
        &mut self.instances[handle]
    }

    pub fn get_instance_field(&self, handle: InstanceHandle, key: Value) -> Option<Value> {
        self.tables.get(handle, &key)
    }

    pub fn set_instance_field(&mut self, handle: InstanceHandle, key: Value, value: Value) {
        self.tables.insert(handle, key, value);
    }

    pub fn free_instance(&mut self, handle: InstanceHandle) {
        debug_log!(self.is_debug, "Freeing instance: {:?}", handle);
        self.instances.remove(handle);
        debug_log!(
            self.is_debug,
            "Freed {} byes for instance: {:?}",
            std::mem::size_of::<InstanceObject>(),
            handle
        );
    }

    pub fn allocate_bound_method(&mut self, method: BoundMethodObject) -> BoundMethodHandle {
        let handle = self.bound_methods.insert(method);
        debug_log!(
            self.is_debug,
            "Allocated {} byes for method: {:?}",
            std::mem::size_of::<BoundMethodObject>(),
            handle
        );
        handle
    }

    pub fn get_bound_method(&self, handle: BoundMethodHandle) -> &BoundMethodObject {
        &self.bound_methods[handle]
    }

    pub fn get_bound_method_mut(&mut self, handle: BoundMethodHandle) -> &mut BoundMethodObject {
        &mut self.bound_methods[handle]
    }

    pub fn free_bound_method(&mut self, handle: BoundMethodHandle) {
        debug_log!(self.is_debug, "Freeing method: {:?}", handle);
        self.upvalues.remove(handle);
        debug_log!(
            self.is_debug,
            "Freed {} byes for method: {:?}",
            std::mem::size_of::<BoundMethodObject>(),
            handle
        );
    }

    pub fn allocate_bound_intrinsic(
        &mut self,
        method: BoundIntrinsicObject,
    ) -> BoundIntrinsicHandle {
        let handle = self.bound_intrinsics.insert(method);
        debug_log!(
            self.is_debug,
            "Allocated {} byes for intrinsic method: {:?}",
            std::mem::size_of::<BoundIntrinsicObject>(),
            handle
        );
        handle
    }

    pub fn get_bound_intrinsic(&self, handle: BoundIntrinsicHandle) -> &BoundIntrinsicObject {
        &self.bound_intrinsics[handle]
    }

    pub fn get_bound_intrinsic_mut(
        &mut self,
        handle: BoundIntrinsicHandle,
    ) -> &mut BoundIntrinsicObject {
        &mut self.bound_intrinsics[handle]
    }

    pub fn free_bound_intrinsic(&mut self, handle: BoundIntrinsicHandle) {
        debug_log!(self.is_debug, "Freeing intrinsic method: {:?}", handle);
        self.upvalues.remove(handle);
        debug_log!(
            self.is_debug,
            "Freed {} byes for intrinsic method: {:?}",
            std::mem::size_of::<BoundIntrinsicObject>(),
            handle
        );
    }

    #[cfg(debug_assertions)]
    pub fn should_collect_garbage(&mut self) -> bool {
        self.closures.len() < 100 // Simplified check for debug
            || self.upvalues.len() < self.upvalues.capacity()
    }
    #[cfg(not(debug_assertions))]
    pub fn should_collect_garbage(&mut self) -> bool {
        self.total_allocated_bytes() > self.bytes_until_gc
    }

    pub fn total_allocated_bytes(&self) -> usize {
        let string_bytes = self.strings.get_allocated_bytes();
        let closure_bytes = self.closures.get_allocated_bytes();
        let upvalue_bytes = self.upvalues.len() * std::mem::size_of::<Upvalue>();
        let function_bytes = self.functions.len() * std::mem::size_of::<FunctionObject>();
        let native_function_bytes =
            self.native_functions.len() * std::mem::size_of::<NativeFunctionObject>();
        let class_bytes = self.classes.len() * std::mem::size_of::<ClassObject>();
        let instance_bytes = self.instances.len() * std::mem::size_of::<InstanceObject>();
        let table_bytes = self.tables.get_allocated_bytes();
        let method_bytes = self.bound_methods.len() * std::mem::size_of::<BoundMethodObject>();
        let intrinsic_bytes =
            self.bound_intrinsics.len() * std::mem::size_of::<BoundIntrinsicObject>();
        let array_bytes = self.arrays.get_allocated_bytes();
        let upvalue_overflow_bytes = self.upvalue_overflow.get_allocated_bytes();

        string_bytes
            + closure_bytes
            + upvalue_bytes
            + upvalue_overflow_bytes
            + function_bytes
            + native_function_bytes
            + class_bytes
            + table_bytes
            + instance_bytes
            + method_bytes
            + intrinsic_bytes
            + array_bytes
    }

    pub fn collect_garbage(&mut self, roots: VecDeque<Value>) {
        #[cfg(debug_assertions)]
        let total_bytes_before_gc = self.total_allocated_bytes();

        self.trace_references(roots);
        let total_elements = self.classes.len()
            + self.closures.len()
            + self.upvalues.len()
            + self.instances.len()
            + self.bound_methods.len();
        let estimated_deletions = (total_elements / 4).max(8);
        let mut deleted_values: Vec<Index> = Vec::with_capacity(estimated_deletions);

        for (index, upvalue) in self.upvalues.iter_mut() {
            if upvalue.is_marked {
                upvalue.is_marked = false;
            } else {
                deleted_values.push(index);
            }
        }

        for index in deleted_values.drain(..) {
            self.upvalues.remove(index);
        }

        self.closures.collect_garbage();
        self.upvalue_overflow.collect_garbage();

        for (index, clazz) in self.classes.iter_mut() {
            if clazz.is_marked {
                clazz.is_marked = false;
            } else {
                deleted_values.push(index);
            }
        }

        for index in deleted_values.drain(..) {
            self.classes.remove(index);
        }

        for (index, instance) in self.instances.iter_mut() {
            if instance.is_marked {
                instance.is_marked = false;
            } else {
                deleted_values.push(index);
            }
        }

        for index in deleted_values.drain(..) {
            self.instances.remove(index);
        }

        self.tables.collect_garbage();

        for (index, method) in self.bound_methods.iter_mut() {
            if method.is_marked {
                method.is_marked = false;
            } else {
                deleted_values.push(index);
            }
        }

        for index in deleted_values.drain(..) {
            self.bound_methods.remove(index);
        }

        for (index, method) in self.bound_intrinsics.iter_mut() {
            if method.is_marked {
                method.is_marked = false;
            } else {
                deleted_values.push(index);
            }
        }

        for index in deleted_values.drain(..) {
            self.bound_intrinsics.remove(index);
        }

        self.arrays.collect_garbage();

        let new_allocated_bytes = self.total_allocated_bytes();

        self.bytes_until_gc = new_allocated_bytes * GC_HEAP_GROW_FACTOR;

        debug_log!(
            self.is_debug,
            "Collected {} bytes. Next collection at {} bytes.",
            total_bytes_before_gc - new_allocated_bytes,
            self.bytes_until_gc
        );
    }

    fn trace_references(&mut self, mut gray_list: VecDeque<Value>) {
        while let Some(value) = gray_list.pop_front() {
            match value {
                Value::Closure(handle) => {
                    self.closures
                        .trace_closure_references(handle, &mut |upvalue_handle| {
                            let upvalue = &mut self.upvalues[upvalue_handle];
                            if !upvalue.is_marked {
                                debug_log!(self.is_debug, "Marking upvalue: {:?}", upvalue_handle);
                                upvalue.is_marked = true;
                                debug_log!(
                                    self.is_debug,
                                    "Blackening upvalue: {:?}",
                                    upvalue_handle
                                );
                                gray_list.push_back(upvalue.value);
                            }
                        });
                }
                Value::Class(handle) => {
                    let clazz = &mut self.classes[handle];
                    if !clazz.is_marked {
                        debug_log!(self.is_debug, "Marking class: {:?}", handle);
                        clazz.is_marked = true;
                        debug_log!(self.is_debug, "Blackening class: {:?}", handle);
                        debug_log!(
                            self.is_debug,
                            "Marking method table: {:?}",
                            clazz.method_table
                        );
                        self.tables.mark_hashmap(clazz.method_table);
                        debug_log!(
                            self.is_debug,
                            "Blackening method table: {:?}",
                            clazz.method_table
                        );
                        for (key, value) in self.tables.iter(clazz.method_table) {
                            gray_list.push_back(key);
                            gray_list.push_back(value);
                        }
                        debug_log!(
                            self.is_debug,
                            "Blackening method table: {:?}",
                            clazz.method_table
                        );
                        debug_log!(
                            self.is_debug,
                            "Marking value table: {:?}",
                            clazz.method_table
                        );
                        self.tables.mark_hashmap(clazz.value_table);
                        debug_log!(
                            self.is_debug,
                            "Blackening value table: {:?}",
                            clazz.value_table
                        );
                        for (key, value) in self.tables.iter(clazz.value_table) {
                            gray_list.push_back(key);
                            gray_list.push_back(value);
                        }
                        debug_log!(
                            self.is_debug,
                            "Blackening value table: {:?}",
                            clazz.value_table
                        );
                    }
                }
                Value::Instance(handle) => {
                    let instance = &mut self.instances[handle];
                    if !instance.is_marked {
                        debug_log!(self.is_debug, "Marking instance: {:?}", handle);
                        instance.is_marked = true;
                        debug_log!(self.is_debug, "Blackening instance: {:?}", handle);
                        debug_log!(self.is_debug, "Marking table: {:?}", instance.table);
                        self.tables.mark_hashmap(instance.table);
                        debug_log!(self.is_debug, "Blackening table: {:?}", instance.table);
                        gray_list.push_back(Value::Class(instance.clazz));
                        for (key, value) in self.tables.iter(instance.table) {
                            gray_list.push_back(key);
                            gray_list.push_back(value);
                        }
                    }
                }
                Value::BoundMethod(handle) => {
                    let method_binding = &mut self.bound_methods[handle];

                    if !method_binding.is_marked {
                        debug_log!(self.is_debug, "Marking method: {:?}", handle);
                        method_binding.is_marked = true;
                        debug_log!(self.is_debug, "Blackening method: {:?}", handle);
                        gray_list.push_back(method_binding.receiver);
                        gray_list.push_back(Value::Closure(method_binding.closure));
                    }
                }
                Value::BoundIntrinsic(handle) => {
                    let intrinsic_binding = &mut self.bound_intrinsics[handle];

                    if !intrinsic_binding.is_marked {
                        debug_log!(self.is_debug, "Marking intrinsic method: {:?}", handle);
                        intrinsic_binding.is_marked = true;
                        debug_log!(self.is_debug, "Blackening intrinsic method: {:?}", handle);
                        gray_list.push_back(intrinsic_binding.receiver);
                    }
                }
                Value::Array(handle) => {
                    if !self.arrays.check_is_marked(handle) {
                        debug_log!(self.is_debug, "Marking array: {:?}", handle);
                        self.arrays.mark_array(handle);
                        debug_log!(self.is_debug, "Blackening array: {:?}", handle);
                        for value in self.arrays.iter(handle) {
                            gray_list.push_back(value);
                        }
                    }
                }
                Value::ObjectLiteral(handle) => {
                    debug_log!(self.is_debug, "Marking object: {:?}", handle);
                    self.tables.mark_hashmap(handle);
                    debug_log!(self.is_debug, "Blackening object: {:?}", handle);
                    for (key, value) in self.tables.iter(handle) {
                        gray_list.push_back(key);
                        gray_list.push_back(value);
                    }
                }
                _ => (),
            }
        }
    }
}
