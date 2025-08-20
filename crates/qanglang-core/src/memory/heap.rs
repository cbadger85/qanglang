use crate::UpvalueReference;
use crate::memory::arena::{Arena, Index};
use crate::{
    ClosureObject, FunctionObject, Upvalue, Value, debug_log, value::NativeFunctionObject,
};
use std::collections::{HashMap, VecDeque};

pub type StringHandle = usize;

pub type ClosureHandle = Index;

pub type UpvalueHandle = Index;

pub type FunctionHandle = usize;

pub type NativeFunctionHandle = usize;

const GC_HEAP_GROW_FACTOR: u64 = 2;

#[derive(Debug, Default, Clone)]
pub struct ObjectHeap {
    strings: Vec<String>,
    functions: Vec<FunctionObject>,
    closures: Arena<ClosureObject>,
    upvalues: Arena<Upvalue>,
    string_interner: HashMap<String, StringHandle>,
    native_functions: Vec<NativeFunctionObject>,
    is_debug: bool,
    bytes_until_gc: u64,
}

impl ObjectHeap {
    pub fn new() -> Self {
        Self::with_capacity(64)
    }

    pub fn with_capacity(initial_capacity: usize) -> Self {
        Self {
            string_interner: HashMap::new(),
            functions: Vec::with_capacity(initial_capacity),
            closures: Arena::with_capacity(initial_capacity),
            strings: Vec::with_capacity(initial_capacity),
            upvalues: Arena::with_capacity(initial_capacity),
            native_functions: Vec::with_capacity(initial_capacity),
            is_debug: false,
            bytes_until_gc: 1024 * 1024,
        }
    }

    pub fn debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    pub fn intern_string_slice(&mut self, s: &str) -> StringHandle {
        if self.string_interner.contains_key(s) {
            self.string_interner[s]
        } else {
            if self.strings.len() == self.strings.capacity() {
                let new_capacity = if self.strings.capacity() == 0 {
                    64
                } else {
                    self.strings.capacity() * 2
                };
                self.strings.reserve(new_capacity - self.strings.capacity());
            }
            self.strings.push(s.to_string());
            let handle = self.strings.len() - 1;

            self.string_interner.insert(s.to_string(), handle);

            handle
        }
    }

    pub fn intern_string(&mut self, s: String) -> StringHandle {
        if self.string_interner.contains_key(&s) {
            self.string_interner[&s]
        } else {
            if self.strings.len() == self.strings.capacity() {
                let new_capacity = if self.strings.capacity() == 0 {
                    64
                } else {
                    self.strings.capacity() * 2
                };
                self.strings.reserve(new_capacity - self.strings.capacity());
            }
            self.strings.push(s.clone());
            let handle = self.strings.len() - 1;

            self.string_interner.insert(s, handle);

            debug_log!(
                self.is_debug,
                "Allocated {} bytes for string: {:?}",
                std::mem::size_of::<String>() * 2 + std::mem::size_of::<StringHandle>(),
                handle
            );

            handle
        }
    }

    pub fn get_string(&self, handle: StringHandle) -> &str {
        &self.strings[handle]
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

        handle
    }

    pub fn get_function(&self, handle: FunctionHandle) -> &FunctionObject {
        &self.functions[handle]
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

        handle
    }

    pub fn get_native_function(&self, handle: NativeFunctionHandle) -> &NativeFunctionObject {
        &self.native_functions[handle]
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

    pub fn total_allocated_bytes(&self) -> u64 {
        let string_bytes = self.strings.len()
            * (std::mem::size_of::<String>() * 2 + std::mem::size_of::<StringHandle>());
        let closure_bytes = self.closures.len() * std::mem::size_of::<ClosureObject>();
        let upvalue_bytes = self.upvalues.len() * std::mem::size_of::<Upvalue>();
        let function_bytes = self.functions.len() * std::mem::size_of::<FunctionObject>();
        let native_function_bytes =
            self.native_functions.len() * std::mem::size_of::<NativeFunctionObject>();

        let total =
            string_bytes + closure_bytes + upvalue_bytes + function_bytes + native_function_bytes;

        total as u64
    }

    pub fn collect_garbage(&mut self, roots: VecDeque<Value>) {
        #[cfg(debug_assertions)]
        let total_bytes_before_gc = self.total_allocated_bytes();

        self.trace_references(roots);

        // TODO figure out a better way to dynamically calculate an initial capacity based on the size of the heap.
        let mut deleted_values: Vec<Index> = Vec::with_capacity(1024);

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
                _ => (),
            }
        }
    }
}
