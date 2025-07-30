use std::collections::{HashMap, hash_map::Entry};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjectHandle(usize);

#[derive(Debug)]
pub struct HeapObject {
    pub value: HeapObjectValue,
}

#[derive(Debug)]
pub enum HeapObjectValue {
    String(Box<str>),
}

pub struct ObjectHeap {
    objects: Vec<Option<HeapObject>>,
    free_list: Vec<usize>,
    string_interner: HashMap<Box<str>, ObjectHandle>,
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
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let s = entry.into_key();
                let handle = self.allocate_object(HeapObject {
                    value: HeapObjectValue::String(s.clone()),
                });
                self.string_interner.insert(s, handle);
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
            match obj.value {
                HeapObjectValue::String(_) => {
                    return;
                }
                _ => self.free_list.push(handle.0),
            }
        }
    }

    pub fn garbage_collect(&mut self) {
        for (index, obj) in self.objects.iter().enumerate() {
            match obj {
                Some(obj) => todo!("Check if object is in use, if it is not, free it"),
                _ => {
                    continue;
                }
            }
        }
    }
}
