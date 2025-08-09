use std::{
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::{chunk::Chunk, error::ValueConversionError};

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct ObjectHandle(usize);

impl ObjectHandle {
    pub fn identifier(&self) -> usize {
        self.0
    }
}

impl From<usize> for ObjectHandle {
    fn from(value: usize) -> Self {
        ObjectHandle(value)
    }
}

impl From<ObjectHandle> for usize {
    fn from(value: ObjectHandle) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Default)]
pub struct FunctionObject {
    pub arity: usize,
    pub name: ObjectHandle,
    pub chunk: Chunk,
}

impl FunctionObject {
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
    Function(Rc<FunctionObject>),
}

pub const fn get_object_value_type(value: &HeapObject) -> &'static str {
    match value {
        HeapObject::String(_) => "string",
        HeapObject::Function(_) => "function",
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

impl From<FunctionObject> for HeapObject {
    fn from(value: FunctionObject) -> Self {
        HeapObject::Function(Rc::new(value))
    }
}

impl<'a> TryFrom<&'a HeapObject> for &'a FunctionObject {
    type Error = ValueConversionError;
    fn try_from(value: &'a HeapObject) -> Result<Self, Self::Error> {
        match value {
            HeapObject::Function(function) => Ok(function),
            _ => Err(ValueConversionError::new("Value is not a function")),
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
        #[cfg(feature = "profiler")]
        coz::progress!("heap_allocation");
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
                HeapObject::String(_) => (),
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

    pub fn iter_objects(&self) -> impl Iterator<Item = (usize, &HeapObject)> {
        self.objects
            .iter()
            .enumerate()
            .filter_map(|(index, obj_opt)| obj_opt.as_ref().map(|obj| (index, obj)))
    }
}
