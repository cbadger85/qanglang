use std::collections::VecDeque;

use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{
    FunctionHandle, HashMapHandle, QangRuntimeError, SourceLocation, StringHandle, Value,
    backend::vm::RuntimeResult,
};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub struct RuntimeModule {
    function: FunctionHandle,
    instance: Option<HashMapHandle>,
}

impl RuntimeModule {
    pub fn new(function: FunctionHandle) -> Self {
        Self {
            function,
            instance: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ModuleResolver {
    modules: FxHashMap<StringHandle, RuntimeModule>,
    module_names: FxHashMap<HashMapHandle, StringHandle>,
    count: usize,
}

impl ModuleResolver {
    pub fn gather_roots(&self, roots: &mut VecDeque<Value>) {
        for (_, module) in self.modules.iter() {
            if let Some(handle) = module.instance {
                roots.push_front(Value::module(handle));
            }
        }
    }

    pub fn get_instance_handle(&self, module_id: StringHandle) -> Option<HashMapHandle> {
        self.modules.get(&module_id).and_then(|m| m.instance)
    }

    pub fn remove_instance(&mut self, module_id: StringHandle) {
        if let Some(module) = self.modules.get_mut(&module_id) {
            module.instance = None;
            self.count -= 1;
        }
    }

    pub fn get_module_handle(
        &self,
        module_id: StringHandle,
        loc: SourceLocation,
    ) -> RuntimeResult<FunctionHandle> {
        self.modules
            .get(&module_id)
            .map(|m| m.function)
            .ok_or(QangRuntimeError::new(
                "Failed to resolve module.".to_string(),
                loc,
            ))
    }

    pub fn add_instance(&mut self, module_id: StringHandle, handle: HashMapHandle) {
        if let Some(module) = self.modules.get_mut(&module_id) {
            module.instance = Some(handle);
            self.count += 1;
            self.module_names.insert(handle, module_id);
        }
    }

    pub fn get_module_id(&self, handle: HashMapHandle) -> StringHandle {
        self.module_names[&handle]
    }

    pub fn count(&self) -> usize {
        self.count
    }
}

impl From<FxHashMap<StringHandle, RuntimeModule>> for ModuleResolver {
    fn from(val: FxHashMap<StringHandle, RuntimeModule>) -> Self {
        let capacity = val.capacity();
        ModuleResolver {
            modules: val,
            count: 0,
            module_names: FxHashMap::with_capacity_and_hasher(capacity, FxBuildHasher),
        }
    }
}
