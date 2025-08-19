use std::collections::HashMap;

use generational_arena::{Arena, Index};

use crate::{ClosureObject, FunctionObject, Upvalue, Value, ValueConversionError, debug_log};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Default)]
pub struct StringHandle(usize);

impl TryFrom<Value> for StringHandle {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(handle) => Ok(handle),
            _ => Err(ValueConversionError::new("Expected string.")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub struct ClosureHandle(Index);

impl Default for ClosureHandle {
    fn default() -> Self {
        ClosureHandle(Index::from_raw_parts(0, 0))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub struct UpvalueHandle(Index);

impl Default for UpvalueHandle {
    fn default() -> Self {
        UpvalueHandle(Index::from_raw_parts(0, 0))
    }
}

impl TryFrom<Value> for ClosureHandle {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Closure(handle) => Ok(handle),
            _ => Err(ValueConversionError::new("Expected function.")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Default)]
pub struct FunctionHandle(usize);

impl TryFrom<Value> for FunctionHandle {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::FunctionDecl(handle) => Ok(handle),
            _ => Err(ValueConversionError::new("Expected function.")),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ObjectHeap {
    strings: Vec<String>,
    functions: Vec<FunctionObject>,
    closures: Arena<ClosureObject>,
    upvalues: Arena<Upvalue>,
    string_interner: HashMap<String, StringHandle>,
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
            let handle = StringHandle(self.strings.len() - 1);

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
            let handle = StringHandle(self.strings.len() - 1);

            self.string_interner.insert(s, handle);

            handle
        }
    }

    pub fn get_string(&self, handle: StringHandle) -> &str {
        &self.strings[handle.0]
    }

    pub fn can_allocate_closure(&self) -> bool {
        self.closures.len() < self.closures.capacity()
    }

    pub fn allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        debug_log!(self.is_debug, "Allocating closure...");
        let index = self.closures.insert(closure);
        ClosureHandle(index)
    }

    pub fn force_allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        debug_log!(self.is_debug, "Allocating closure...");
        if self.closures.len() == self.closures.capacity() {
            let new_capacity = if self.closures.capacity() == 0 {
                64
            } else {
                self.closures.capacity() * 2
            };

            self.closures
                .reserve(new_capacity - self.closures.capacity());
        }
        let index = self.closures.insert(closure);
        ClosureHandle(index)
    }

    pub fn get_closure(&self, handle: ClosureHandle) -> &ClosureObject {
        &self.closures[handle.0]
    }

    pub fn get_closure_mut(&mut self, handle: ClosureHandle) -> &mut ClosureObject {
        &mut self.closures[handle.0]
    }

    pub fn free_closure(&mut self, handle: ClosureHandle) {
        debug_log!(self.is_debug, "Freeing function...");
        self.closures.remove(handle.0);
    }

    pub fn mark_closure(&mut self, handle: ClosureHandle) {
        let closure = &mut self.closures[handle.0];
        closure.is_marked = true;
    }

    pub fn can_allocate_upvalue(&self) -> bool {
        self.upvalues.len() < self.upvalues.capacity()
    }

    pub fn allocate_upvalue(&mut self, value: Value) -> UpvalueHandle {
        debug_log!(self.is_debug, "Allocating upvalue...");
        let index = self.upvalues.insert(Upvalue {
            value,
            is_marked: false,
        });
        UpvalueHandle(index)
    }

    pub fn force_allocate_upvalue(&mut self, value: Value) -> UpvalueHandle {
        debug_log!(self.is_debug, "Allocating upvalue...");
        if self.upvalues.len() == self.upvalues.capacity() {
            let new_capacity = if self.upvalues.capacity() == 0 {
                64
            } else {
                self.upvalues.capacity() * 2
            };

            self.upvalues
                .reserve(new_capacity - self.upvalues.capacity());
        }
        let index = self.upvalues.insert(Upvalue {
            value,
            is_marked: false,
        });
        UpvalueHandle(index)
    }

    pub fn get_upvalue(&self, handle: UpvalueHandle) -> &Value {
        &self.upvalues[handle.0].value
    }

    pub fn get_upvalue_mut(&mut self, handle: UpvalueHandle) -> &mut Value {
        &mut self.upvalues[handle.0].value
    }

    pub fn free_upvalue(&mut self, handle: UpvalueHandle) {
        debug_log!(self.is_debug, "Freeing upvalue...");
        self.upvalues.remove(handle.0);
    }

    pub fn mark_upvalue(&mut self, handle: UpvalueHandle) {
        let upvalue = &mut self.upvalues[handle.0];
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
        FunctionHandle(self.functions.len() - 1)
    }

    pub fn get_function(&self, handle: FunctionHandle) -> &FunctionObject {
        &self.functions[handle.0]
    }

    pub fn iter_functions(&self) -> impl Iterator<Item = (usize, &FunctionObject)> {
        self.functions
            .iter()
            .enumerate()
            .map(|(index, obj)| (index, obj))
    }

    pub fn collect_garbage(&mut self, _roots: Vec<ClosureHandle>) {
        // todo!("Implement mark and sweep garbage collection using provided roots");
    }
}
