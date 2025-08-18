use std::collections::HashMap;

use generational_arena::{Arena, Index};

use crate::{ClosureObject, FunctionObject, FunctionValueKind, Value, ValueConversionError};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Default)]
pub struct StringHandle {
    index: usize,
    pub is_literal: bool,
}

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
pub struct ClosureHandle {
    index: Index,
    pub name_handle: StringHandle,
}

impl Default for ClosureHandle {
    fn default() -> Self {
        ClosureHandle {
            index: Index::from_raw_parts(0, 0),
            name_handle: StringHandle::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub struct ValueHandle(Index);

impl Default for ValueHandle {
    fn default() -> Self {
        ValueHandle(Index::from_raw_parts(0, 0))
    }
}

impl TryFrom<Value> for ClosureHandle {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Function(kind) => match kind {
                FunctionValueKind::Closure(handle) => Ok(handle),
                _ => Err(ValueConversionError::new("Expected function.")),
            },
            _ => Err(ValueConversionError::new("Expected function.")),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Default)]
pub struct FunctionHandle {
    index: usize,
    pub name_handle: StringHandle,
}

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
    values: Arena<Value>,
    string_interner: HashMap<String, StringHandle>,
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
            values: Arena::with_capacity(initial_capacity),
        }
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
            let handle = StringHandle {
                index: self.strings.len() - 1,
                is_literal: true,
            };

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
            let handle = StringHandle {
                index: self.strings.len() - 1,
                is_literal: true,
            };

            self.string_interner.insert(s, handle);

            handle
        }
    }

    pub fn get_string(&self, handle: StringHandle) -> &str {
        &self.strings[handle.index]
    }

    pub fn can_allocate_closure(&self) -> bool {
        self.closures.len() < self.closures.capacity()
    }

    pub fn allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        let name_handle = closure.function.name_handle;
        let index = self.closures.insert(closure);
        ClosureHandle { index, name_handle }
    }

    pub fn force_allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        let name_handle = closure.function.name_handle;
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
        ClosureHandle { index, name_handle }
    }

    pub fn get_closure(&self, handle: ClosureHandle) -> &ClosureObject {
        &self.closures[handle.index]
    }

    pub fn get_closure_mut(&mut self, handle: ClosureHandle) -> &mut ClosureObject {
        &mut self.closures[handle.index]
    }

    pub fn free_closure(&mut self, handle: ClosureHandle) {
        self.closures.remove(handle.index);
    }

    pub fn can_allocate_value(&self) -> bool {
        self.values.len() < self.values.capacity()
    }

    pub fn allocate_value(&mut self, value: Value) -> ValueHandle {
        let index = self.values.insert(value);
        ValueHandle(index)
    }

    pub fn force_allocate_value(&mut self, value: Value) -> ValueHandle {
        if self.values.len() == self.values.capacity() {
            let new_capacity = if self.values.capacity() == 0 {
                64
            } else {
                self.values.capacity() * 2
            };

            self.values.reserve(new_capacity - self.values.capacity());
        }
        let index = self.values.insert(value);
        ValueHandle(index)
    }

    pub fn get_value(&self, handle: ValueHandle) -> &Value {
        &self.values[handle.0]
    }

    pub fn get_value_mut(&mut self, handle: ValueHandle) -> &mut Value {
        &mut self.values[handle.0]
    }

    pub fn free_value(&mut self, handle: ValueHandle) {
        self.values.remove(handle.0);
    }

    pub fn allocate_function(&mut self, function: FunctionObject) -> FunctionHandle {
        let name_handle = function.name;
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
        FunctionHandle {
            index: self.functions.len() - 1,
            name_handle,
        }
    }

    pub fn get_function(&self, handle: FunctionHandle) -> &FunctionObject {
        &self.functions[handle.index]
    }

    pub fn iter_functions(&self) -> impl Iterator<Item = (usize, &FunctionObject)> {
        self.functions
            .iter()
            .enumerate()
            .map(|(index, obj)| (index, obj))
    }

    pub fn collect_garbage(&mut self, _roots: &[Value]) {
        // todo!("Implement mark and sweep garbage collection using provided roots");
    }
}
