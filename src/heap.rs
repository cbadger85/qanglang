use std::{
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::{chunk::Chunk, error::ValueConversionError, vm::NativeFn};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjectHandle(usize);

impl ObjectHandle {
    pub fn identifier(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct NativeFunction {
    pub function: NativeFn,
    pub arity: usize,
    pub name: ObjectHandle,
}

#[derive(Debug, Clone)]
pub struct KangFunction {
    pub arity: usize,
    pub name: ObjectHandle,
    pub chunk: Chunk,
}

impl KangFunction {
    pub fn new(name: ObjectHandle, arity: usize) -> Self {
        Self {
            name,
            arity,
            chunk: Chunk::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum HeapObject {
    String(Box<str>),
    NativeFunction(Rc<NativeFunction>),
    Function(Rc<KangFunction>),
}

pub const fn get_object_value_type(value: &HeapObject) -> &'static str {
    match value {
        HeapObject::String(_) => "string",
        HeapObject::NativeFunction(_) | HeapObject::Function(_) => "function",
    }
}

impl TryFrom<HeapObject> for Box<str> {
    type Error = ValueConversionError;

    fn try_from(value: HeapObject) -> Result<Self, Self::Error> {
        match value {
            HeapObject::String(string) => Ok(string),
            _ => Err(ValueConversionError::new(
                format!("Expected string, found {}.", get_object_value_type(&value)).as_str(),
            )),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ObjectHeap {
    objects: Vec<Option<HeapObject>>,
    free_list: Vec<usize>,
    string_interner: HashMap<Box<str>, usize>,
}

impl ObjectHeap {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            free_list: Vec::new(),
            string_interner: HashMap::new(),
        }
    }

    pub fn intern_string(&mut self, s: Box<str>) -> ObjectHandle {
        match self.string_interner.entry(s) {
            Entry::Occupied(entry) => ObjectHandle(*entry.get()),
            Entry::Vacant(entry) => {
                let s = entry.into_key();
                let handle = self.allocate_object(HeapObject::String(s.clone()));
                self.string_interner.insert(s, handle.identifier());
                handle
            }
        }
    }

    pub fn allocate_object(&mut self, obj: HeapObject) -> ObjectHandle {
        let index = if let Some(free_index) = self.free_list.pop() {
            self.objects[free_index] = Some(obj);
            free_index
        } else {
            self.objects.push(Some(obj));
            self.objects.len() - 1
        };
        ObjectHandle(index)
    }

    pub fn get(&self, handle: ObjectHandle) -> Option<&HeapObject> {
        self.objects[handle.0].as_ref()
    }

    pub fn get_mut(&mut self, handle: ObjectHandle) -> Option<&mut HeapObject> {
        self.objects[handle.0].as_mut()
    }

    pub fn free(&mut self, handle: ObjectHandle) {
        if let Some(obj) = self.objects[handle.0].take() {
            match obj {
                HeapObject::String(_) | HeapObject::NativeFunction(_) => {}
                _ => self.free_list.push(handle.0),
            }
        }
    }

    pub fn garbage_collect(&mut self) {
        for (index, obj) in self.objects.iter().enumerate() {
            match obj {
                Some(_obj) => todo!(
                    "Check if object is in use, if it is not, free it using the index of {}",
                    index
                ),
                _ => {
                    continue;
                }
            }
        }
    }
}
