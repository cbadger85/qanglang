use rustc_hash::{FxBuildHasher, FxHashMap};

use crate::{
    NodeId, StringHandle,
    frontend::types::{QangType, TypeId},
};

/// Arena for storing types
#[derive(Debug, Default, Clone)]
pub struct TypeArena {
    types: Vec<QangType>,
}

impl TypeArena {
    pub fn new() -> Self {
        let mut arena = Self { types: Vec::new() };

        // Pre-allocate common types
        arena.create_type(QangType::Unknown); // TypeId(0)
        arena.create_type(QangType::Never); // TypeId(1)
        arena.create_type(QangType::Unit); // TypeId(2)
        arena.create_type(QangType::Number); // TypeId(3)
        arena.create_type(QangType::String); // TypeId(4)
        arena.create_type(QangType::Boolean); // TypeId(5)

        arena
    }

    pub fn create_type(&mut self, ty: QangType) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(ty);
        id
    }

    pub fn get_type(&self, id: TypeId) -> &QangType {
        &self.types[id.0]
    }

    pub fn get_type_mut(&mut self, id: TypeId) -> &mut QangType {
        &mut self.types[id.0]
    }

    // Common type constants
    pub const UNKNOWN: TypeId = TypeId(0);
    pub const NEVER: TypeId = TypeId(1);
    pub const UNIT: TypeId = TypeId(2);
    pub const NUMBER: TypeId = TypeId(3);
    pub const STRING: TypeId = TypeId(4);
    pub const BOOLEAN: TypeId = TypeId(5);

    /// Create an optional type
    pub fn make_optional(&mut self, inner: TypeId) -> TypeId {
        self.create_type(QangType::Optional(inner))
    }

    /// Create a function type
    pub fn make_function(&mut self, params: Vec<TypeId>, return_type: TypeId) -> TypeId {
        self.create_type(QangType::Function {
            params,
            return_type,
        })
    }

    /// Create a class type
    pub fn make_class(&mut self, name: StringHandle, class_node: NodeId) -> TypeId {
        self.create_type(QangType::Class { name, class_node })
    }

    /// Create an array type
    pub fn make_array(&mut self, element_type: TypeId) -> TypeId {
        self.create_type(QangType::Array(element_type))
    }

    /// Create an object type
    pub fn make_object(&mut self, fields: Vec<(StringHandle, TypeId)>) -> TypeId {
        self.create_type(QangType::Object { fields })
    }

    /// Create a module type
    pub fn make_module(
        &mut self,
        path: StringHandle,
        exports: Vec<(StringHandle, TypeId)>,
        module_node: NodeId,
    ) -> TypeId {
        self.create_type(QangType::Module {
            path,
            exports,
            module_node,
        })
    }

    /// Check if a type is optional
    pub fn is_optional(&self, ty: TypeId) -> bool {
        matches!(self.get_type(ty), QangType::Optional(_))
    }

    /// Get the inner type of an optional, or return the type itself if not optional
    pub fn unwrap_optional(&self, ty: TypeId) -> TypeId {
        match self.get_type(ty) {
            QangType::Optional(inner) => *inner,
            _ => ty,
        }
    }

    /// Check if a type is a function
    pub fn is_function(&self, ty: TypeId) -> bool {
        matches!(self.get_type(ty), QangType::Function { .. })
    }

    /// Check if a type is a class constructor
    pub fn is_class(&self, ty: TypeId) -> bool {
        matches!(self.get_type(ty), QangType::Class { .. })
    }

    /// Check if a type is a module
    pub fn is_module(&self, ty: TypeId) -> bool {
        matches!(self.get_type(ty), QangType::Module { .. })
    }

    /// Get the exports of a module type
    pub fn get_module_exports(&self, ty: TypeId) -> Option<&Vec<(StringHandle, TypeId)>> {
        match self.get_type(ty) {
            QangType::Module { exports, .. } => Some(exports),
            _ => None,
        }
    }

    /// Look up a specific export in a module
    pub fn lookup_module_export(
        &self,
        module_ty: TypeId,
        export_name: StringHandle,
    ) -> Option<TypeId> {
        self.get_module_exports(module_ty)?
            .iter()
            .find(|(name, _)| *name == export_name)
            .map(|(_, ty)| *ty)
    }
}

/// Type information associated with AST nodes
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeInfo {
    /// The inferred type of this node
    pub inferred_type: TypeId,
    /// Whether this node can be null/nil
    pub nullable: bool,
}

impl TypeInfo {
    pub fn new(inferred_type: TypeId) -> Self {
        Self {
            inferred_type,
            nullable: false,
        }
    }

    pub fn new_nullable(inferred_type: TypeId) -> Self {
        Self {
            inferred_type,
            nullable: true,
        }
    }

    pub fn unknown() -> Self {
        Self::new(TypeArena::UNKNOWN)
    }
}

/// Maps AST nodes to their type information
#[derive(Debug, Clone, Default)]
pub struct TypeTable {
    node_types: FxHashMap<NodeId, TypeInfo>,
}

impl TypeTable {
    pub fn new() -> Self {
        Self {
            node_types: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn set_type(&mut self, node: NodeId, type_info: TypeInfo) {
        self.node_types.insert(node, type_info);
    }

    pub fn get_type(&self, node: NodeId) -> Option<&TypeInfo> {
        self.node_types.get(&node)
    }

    #[allow(dead_code)]
    pub fn get_type_or_unknown(&self, node: NodeId) -> TypeInfo {
        self.node_types
            .get(&node)
            .copied()
            .unwrap_or_else(TypeInfo::unknown)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_arena_basics() {
        let mut arena = TypeArena::new();

        assert_eq!(arena.get_type(TypeArena::NUMBER), &QangType::Number);
        assert_eq!(arena.get_type(TypeArena::STRING), &QangType::String);

        let optional_number = arena.make_optional(TypeArena::NUMBER);
        assert!(arena.is_optional(optional_number));
        assert_eq!(arena.unwrap_optional(optional_number), TypeArena::NUMBER);
    }

    #[test]
    fn test_function_types() {
        let mut arena = TypeArena::new();

        let func_type = arena.make_function(
            vec![TypeArena::NUMBER, TypeArena::STRING],
            TypeArena::BOOLEAN,
        );

        assert!(arena.is_function(func_type));

        if let QangType::Function {
            params,
            return_type,
        } = arena.get_type(func_type)
        {
            assert_eq!(params.len(), 2);
            assert_eq!(*return_type, TypeArena::BOOLEAN);
        } else {
            panic!("Expected function type");
        }
    }
}
