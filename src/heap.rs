use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ObjectHandle(usize);

#[derive(Debug)]
pub enum HeapObject {
    String(String),
    // TODO - flesh this out below
    Function,
    Class,
    Instance,
}

pub struct ObjectHeap {
    objects: Vec<Option<HeapObject>>,
    free_list: Vec<usize>,
    string_interner: HashMap<String, ObjectHandle>,
}

impl ObjectHeap {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            free_list: Vec::new(),
            string_interner: HashMap::new(),
        }
    }

    pub fn intern_string(&mut self, s: String) -> ObjectHandle {
        if let Some(&existing_handle) = self.string_interner.get(&s) {
            return existing_handle;
        }

        let handle = self.allocate_object(HeapObject::String(s.clone()));

        self.string_interner.insert(s, handle);

        handle
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
            if let HeapObject::String(s) = obj {
                self.string_interner.remove(&s);
            }
            self.free_list.push(handle.0);
        }
    }
}
