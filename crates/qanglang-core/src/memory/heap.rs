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

#[derive(Debug, Default, Clone)]
pub struct ObjectHeap {
    strings: Vec<String>,
    functions: Vec<FunctionObject>,
    closures: Arena<ClosureObject>,
    upvalues: Arena<Upvalue>,
    string_interner: HashMap<String, StringHandle>,
    native_functions: Vec<NativeFunctionObject>,
    is_debug: bool,
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
        }
    }

    pub fn debug(mut self, is_debug: bool) -> Self {
        self.is_debug = is_debug;
        self
    }

    pub fn intern_string_slice(&mut self, s: &str) -> StringHandle {
        debug_log!(self.is_debug, "Allocating string...");
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
        debug_log!(self.is_debug, "Allocating string...");
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

            handle
        }
    }

    pub fn get_string(&self, handle: StringHandle) -> &str {
        &self.strings[handle]
    }

    pub fn can_allocate_closure(&self) -> bool {
        self.closures.len() < self.closures.capacity()
    }

    pub fn allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        debug_log!(self.is_debug, "Allocating closure...");
        self.closures.insert(closure)
    }

    pub fn get_closure(&self, handle: ClosureHandle) -> &ClosureObject {
        &self.closures[handle]
    }

    pub fn get_closure_mut(&mut self, handle: ClosureHandle) -> &mut ClosureObject {
        &mut self.closures[handle]
    }

    pub fn free_closure(&mut self, handle: ClosureHandle) {
        debug_log!(self.is_debug, "Freeing function...");
        self.closures.remove(handle);
    }

    pub fn mark_closure(&mut self, handle: ClosureHandle) {
        let closure = &mut self.closures[handle];
        closure.is_marked = true;
    }

    pub fn can_allocate_upvalue(&self) -> bool {
        self.upvalues.len() < self.upvalues.capacity()
    }

    pub fn allocate_upvalue(&mut self, value: Value) -> UpvalueHandle {
        debug_log!(self.is_debug, "Allocating upvalue...");
        self.upvalues.insert(Upvalue {
            value,
            is_marked: false,
        })
    }

    pub fn get_upvalue(&self, handle: UpvalueHandle) -> &Value {
        &self.upvalues[handle].value
    }

    pub fn get_upvalue_mut(&mut self, handle: UpvalueHandle) -> &mut Value {
        &mut self.upvalues[handle].value
    }

    pub fn free_upvalue(&mut self, handle: UpvalueHandle) {
        debug_log!(self.is_debug, "Freeing upvalue...");
        self.upvalues.remove(handle);
    }

    pub fn mark_upvalue(&mut self, handle: UpvalueHandle) {
        let upvalue = &mut self.upvalues[handle];
        upvalue.is_marked = true;
    }

    pub fn allocate_function(&mut self, function: FunctionObject) -> FunctionHandle {
        debug_log!(self.is_debug, "Allocating function...");
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
        self.functions.len() - 1
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
        debug_log!(self.is_debug, "Allocating function...");
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
        self.native_functions.len() - 1
    }

    pub fn get_native_function(&self, handle: NativeFunctionHandle) -> &NativeFunctionObject {
        &self.native_functions[handle]
    }

    pub fn collect_garbage(&mut self, roots: VecDeque<Value>) {
        self.trace_references(roots);

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
    }

    fn trace_references(&mut self, mut gray_list: VecDeque<Value>) {
        while !gray_list.is_empty() {
            let value = gray_list.pop_front().unwrap();

            match value {
                Value::Closure(handle) => {
                    let closure = &mut self.closures[handle];

                    if closure.is_marked {
                        continue;
                    }

                    closure.is_marked = true;

                    for i in 0..closure.upvalue_count {
                        if let UpvalueReference::Closed(handle) = closure.upvalues[i] {
                            let upvalue = &mut self.upvalues[handle];
                            if upvalue.is_marked {
                                continue;
                            }
                            upvalue.is_marked = true;

                            gray_list.push_back(upvalue.value);
                        }
                    }
                }
                _ => (),
            }
        }
    }
}
