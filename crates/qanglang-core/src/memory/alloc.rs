use crate::memory::arena::{Arena, Index};
use crate::memory::hashmap_arena::HashMapArena;
use crate::memory::object::{ClassObject, InstanceObject};
use crate::memory::string_interner::StringInterner;
use crate::{
    ClosureObject, FunctionObject, Upvalue, Value, debug_log, value::NativeFunctionObject,
};
use crate::{StringHandle, UpvalueReference};
use std::collections::VecDeque;

pub type ClosureHandle = Index;

pub type UpvalueHandle = Index;

pub type ClassHandle = Index;

pub type InstanceHandle = Index;

pub type FunctionHandle = u32;

pub type NativeFunctionHandle = u32;

const GC_HEAP_GROW_FACTOR: usize = 2;

#[derive(Debug, Default, Clone)]
pub struct HeapAllocator {
    functions: Vec<FunctionObject>,
    closures: Arena<ClosureObject>,
    upvalues: Arena<Upvalue>,
    pub strings: StringInterner,
    native_functions: Vec<NativeFunctionObject>,
    classes: Arena<ClassObject>,
    instances: Arena<InstanceObject>,
    tables: HashMapArena,
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
            closures: Arena::with_capacity(initial_capacity),
            strings: StringInterner::new(),
            upvalues: Arena::with_capacity(initial_capacity),
            native_functions: Vec::with_capacity(initial_capacity),
            tables: HashMapArena::with_capacity(initial_capacity),
            classes: Arena::with_capacity(initial_capacity),
            instances: Arena::with_capacity(initial_capacity),
            is_debug: false,
            bytes_until_gc: 1024 * 1024,
        }
    }

    pub fn set_debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    pub fn set_bytes_until_gc(mut self, bytes: usize) -> Self {
        self.bytes_until_gc = bytes;
        self
    }

    pub fn allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        let handle = self.closures.insert(closure);
        debug_log!(
            self.is_debug,
            "Allocated {} bytes for closure: {:?}",
            std::mem::size_of::<ClosureObject>(),
            handle
        );
        handle
    }

    pub fn get_closure(&self, handle: ClosureHandle) -> &ClosureObject {
        &self.closures[handle]
    }

    pub fn get_closure_mut(&mut self, handle: ClosureHandle) -> &mut ClosureObject {
        &mut self.closures[handle]
    }

    pub fn free_closure(&mut self, handle: ClosureHandle) {
        self.closures.remove(handle);
        debug_log!(
            self.is_debug,
            "Freed {} byes for closure: {:?}",
            std::mem::size_of::<ClosureObject>(),
            handle
        );
    }

    pub fn mark_closure(&mut self, handle: ClosureHandle) {
        let closure = &mut self.closures[handle];
        closure.is_marked = true;
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

    pub fn mark_upvalue(&mut self, handle: UpvalueHandle) {
        debug_log!(self.is_debug, "Marking upvalue: {:?}", handle);
        let upvalue = &mut self.upvalues[handle];
        upvalue.is_marked = true;
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
            table: self.tables.new_hashmap(),
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

    pub fn mark_class(&mut self, handle: ClassHandle) {
        debug_log!(self.is_debug, "Marking class: {:?}", handle);
        let clazz = &mut self.classes[handle];
        clazz.is_marked = true;
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

    pub fn mark_instance(&mut self, handle: InstanceHandle) {
        debug_log!(self.is_debug, "Marking instance: {:?}", handle);
        let instance = &mut self.instances[handle];
        instance.is_marked = true;
    }

    #[cfg(debug_assertions)]
    pub fn should_collect_garbage(&mut self) -> bool {
        self.closures.len() < self.closures.capacity()
            || self.upvalues.len() < self.upvalues.capacity()
    }
    #[cfg(not(debug_assertions))]
    pub fn should_collect_garbage(&mut self) -> bool {
        self.total_allocated_bytes() > self.bytes_until_gc
    }

    pub fn total_allocated_bytes(&self) -> usize {
        let string_bytes = self.strings.get_allocated_bytes();
        let closure_bytes = self.closures.len() * std::mem::size_of::<ClosureObject>();
        let upvalue_bytes = self.upvalues.len() * std::mem::size_of::<Upvalue>();
        let function_bytes = self.functions.len() * std::mem::size_of::<FunctionObject>();
        let native_function_bytes =
            self.native_functions.len() * std::mem::size_of::<NativeFunctionObject>();
        let class_bytes = self.classes.len() * std::mem::size_of::<ClassObject>();
        let instance_bytes = self.instances.len() * std::mem::size_of::<InstanceObject>();
        let table_bytes = self.tables.get_allocated_bytes();

        string_bytes
            + closure_bytes
            + upvalue_bytes
            + function_bytes
            + native_function_bytes
            + class_bytes
            + table_bytes
            + instance_bytes
    }

    pub fn collect_garbage(&mut self, roots: VecDeque<Value>) {
        #[cfg(debug_assertions)]
        let total_bytes_before_gc = self.total_allocated_bytes();

        self.trace_references(roots);

        let mut deleted_values: Vec<Index> =
            Vec::with_capacity(self.closures.len() + self.upvalues.len());

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

        for (index, closure) in self.closures.iter_mut() {
            if closure.is_marked {
                closure.is_marked = false;
            } else {
                deleted_values.push(index);
            }
        }

        for index in deleted_values.drain(..) {
            self.closures.remove(index);
        }

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

        let new_allocated_bytes = self.total_allocated_bytes();

        self.bytes_until_gc = new_allocated_bytes * GC_HEAP_GROW_FACTOR;

        #[cfg(debug_assertions)]
        {
            println!(
                "Collected {} bytes. Next collection at {} bytes.",
                total_bytes_before_gc - new_allocated_bytes,
                self.bytes_until_gc
            );
        }
    }

    fn trace_references(&mut self, mut gray_list: VecDeque<Value>) {
        while let Some(value) = gray_list.pop_front() {
            match value {
                Value::Closure(handle) => {
                    let closure = &mut self.closures[handle];

                    if !closure.is_marked {
                        debug_log!(self.is_debug, "Blackening closure: {:?}", handle);
                        closure.is_marked = true;

                        for i in 0..closure.upvalue_count {
                            if let UpvalueReference::Closed(handle) = closure.upvalues[i] {
                                let upvalue = &mut self.upvalues[handle];

                                if !upvalue.is_marked {
                                    debug_log!(self.is_debug, "Blackening upvalue: {:?}", handle);
                                    upvalue.is_marked = true;
                                    gray_list.push_back(upvalue.value);
                                }
                            }
                        }
                    }
                }
                Value::Class(handle) => {
                    let clazz = &mut self.classes[handle];
                    if !clazz.is_marked {
                        debug_log!(self.is_debug, "Blackening class: {:?}", handle);
                        clazz.is_marked = true;
                        for (key, value) in self.tables.iter(clazz.table) {
                            gray_list.push_back(key);
                            gray_list.push_back(value);
                        }
                        self.tables.mark_hashmap(clazz.table);
                        debug_log!(self.is_debug, "Blackening table: {:?}", clazz.table);
                    }
                }
                Value::Instance(handle) => {
                    let instance = &mut self.instances[handle];
                    if !instance.is_marked {
                        debug_log!(self.is_debug, "Blackening instance: {:?}", handle);
                        instance.is_marked = true;
                        gray_list.push_back(Value::Class(instance.clazz));
                        for (key, value) in self.tables.iter(instance.table) {
                            gray_list.push_back(key);
                            gray_list.push_back(value);
                        }
                        self.tables.mark_hashmap(instance.table);
                        debug_log!(self.is_debug, "Blackening table: {:?}", instance.table);
                    }
                }
                _ => (),
            }
        }
    }
}
