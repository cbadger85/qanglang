use std::collections::HashMap;

use generational_arena::{Arena, Index};

use crate::{ClosureObject, FunctionObject, FunctionValueKind, Value, ValueConversionError};

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
            let handle = StringHandle(self.strings.len() - 1);

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
            let handle = StringHandle(self.strings.len() - 1);

            self.string_interner.insert(s, handle);

            handle
        }
    }

    pub fn allocate_closure(&mut self, closure: ClosureObject) -> ClosureHandle {
        let index = self.closures.insert(closure);
        ClosureHandle(index)
    }

    pub fn allocate_value(&mut self, value: Value) -> ValueHandle {
        let index = self.values.insert(value);
        ValueHandle(index)
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
        FunctionHandle(self.functions.len() - 1)
    }

    pub fn get_string(&self, handle: StringHandle) -> &str {
        &self.strings[handle.0]
    }

    pub fn clone_function(&self, handle: FunctionHandle) -> &FunctionObject {
        &self.functions[handle.0]
    }

    pub fn get_function(&self, handle: FunctionHandle) -> &FunctionObject {
        &self.functions[handle.0]
    }

    pub fn get_closure(&self, handle: ClosureHandle) -> &ClosureObject {
        &self.closures[handle.0]
    }

    pub fn get_closure_mut(&mut self, handle: ClosureHandle) -> &mut ClosureObject {
        &mut self.closures[handle.0]
    }

    pub fn get_value(&self, handle: ValueHandle) -> &Value {
        &self.values[handle.0]
    }

    pub fn get_value_mut(&mut self, handle: ValueHandle) -> &mut Value {
        &mut self.values[handle.0]
    }

    pub fn garbage_collect(&mut self) {
        todo!();
    }

    pub fn iter_functions(&self) -> impl Iterator<Item = (usize, &FunctionObject)> {
        self.functions
            .iter()
            .enumerate()
            .map(|(index, obj)| (index, obj))
    }
}
