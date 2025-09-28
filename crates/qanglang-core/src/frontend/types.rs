use crate::memory::StringInterner;
use crate::{NodeId, StringHandle};
use rustc_hash::{FxBuildHasher, FxHashMap};
use std::fmt;

/// Unique identifier for types in the type system
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

/// Main type table that maps AST nodes to their resolved types
#[derive(Debug, Clone, Default)]
pub struct TypeTable {
    /// Maps AST node IDs to their resolved types
    node_types: FxHashMap<NodeId, TypeId>,
    /// Storage for all type information
    type_storage: Vec<TypeInfo>,
    /// Next available type ID
    next_type_id: usize,
}

impl TypeTable {
    pub fn new() -> Self {
        Self {
            node_types: FxHashMap::with_hasher(FxBuildHasher),
            type_storage: Vec::new(),
            next_type_id: 0,
        }
    }

    /// Create a new type and return its ID
    pub fn create_type(&mut self, type_info: TypeInfo) -> TypeId {
        let type_id = TypeId(self.next_type_id);
        self.next_type_id += 1;
        self.type_storage.push(type_info);
        type_id
    }

    pub fn replace_type(&mut self, type_id: TypeId, new_type_info: TypeInfo) {
        self.type_storage[type_id.0] = new_type_info;
    }

    /// Get type information by TypeId
    pub fn get_type_info(&self, type_id: TypeId) -> Option<&TypeInfo> {
        self.type_storage.get(type_id.0)
    }

    /// Get the resolved type for an AST node
    pub fn get_node_type(&self, node_id: NodeId) -> Option<TypeId> {
        self.node_types.get(&node_id).copied()
    }

    /// Associate a type with an AST node
    pub fn set_node_type(&mut self, node_id: NodeId, type_id: TypeId) {
        self.node_types.insert(node_id, type_id);
    }

    /// Check if two types are compatible
    pub fn is_assignable(&self, from: TypeId, to: TypeId) -> bool {
        if from == to {
            return true;
        }

        let from_info = self.get_type_info(from);
        let to_info = self.get_type_info(to);

        match (from_info, to_info) {
            (Some(from_type), Some(to_type)) => {
                self.is_type_assignable(&from_type.type_node, &to_type.type_node)
            }
            _ => false,
        }
    }

    fn is_type_assignable(&self, from: &TypeNode, to: &TypeNode) -> bool {
        match (from, to) {
            // Dynamic type hierarchy
            (_, TypeNode::DynamicTop) => true, // Everything assignable to dyn!
            (TypeNode::DynamicTop, _) => true, // dyn! assignable to everything
            (_, TypeNode::DynamicNullable) => true, // Everything assignable to dyn?

            // Primitive types
            (TypeNode::Primitive(a), TypeNode::Primitive(b)) => a == b,

            // Array types (covariant)
            (TypeNode::Array(from_elem_id), TypeNode::Array(to_elem_id)) => {
                if let (Some(from_elem), Some(to_elem)) = (
                    self.get_type_info(*from_elem_id),
                    self.get_type_info(*to_elem_id),
                ) {
                    self.is_type_assignable(&from_elem.type_node, &to_elem.type_node)
                } else {
                    false
                }
            }

            // Function types (contravariant in params, covariant in return)
            (TypeNode::Function(from_fn), TypeNode::Function(to_fn)) => {
                if from_fn.parameters.len() != to_fn.parameters.len() {
                    return false;
                }

                // Parameters are contravariant
                for (from_param_id, to_param_id) in from_fn.parameters.iter().zip(&to_fn.parameters)
                {
                    if let (Some(from_param), Some(to_param)) = (
                        self.get_type_info(*from_param_id),
                        self.get_type_info(*to_param_id),
                    ) {
                        if !self.is_type_assignable(&to_param.type_node, &from_param.type_node) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }

                // Return type is covariant
                if let (Some(from_ret), Some(to_ret)) = (
                    self.get_type_info(from_fn.return_type),
                    self.get_type_info(to_fn.return_type),
                ) {
                    self.is_type_assignable(&from_ret.type_node, &to_ret.type_node)
                } else {
                    false
                }
            }

            // Class inheritance (subclass assignable to superclass)
            (TypeNode::Class(from_class), TypeNode::Class(to_class)) => {
                if from_class.name == to_class.name {
                    return true;
                }

                // Check if from_class inherits from to_class (single level only)
                if let Some(superclass_id) = from_class.superclass
                    && let Some(super_info) = self.get_type_info(superclass_id)
                    && let TypeNode::Class(super_class) = &super_info.type_node {
                        return super_class.name == to_class.name;
                    }

                false
            }

            // Union types
            (from_type, TypeNode::Union(union_type_ids)) => {
                union_type_ids.iter().any(|to_type_id| {
                    if let Some(to_type) = self.get_type_info(*to_type_id) {
                        self.is_type_assignable(from_type, &to_type.type_node)
                    } else {
                        false
                    }
                })
            }

            // Generic types (nominal typing for now)
            (TypeNode::Generic(from_gen), TypeNode::Generic(to_gen)) => {
                from_gen.base_name == to_gen.base_name
                    && from_gen.type_arguments.len() == to_gen.type_arguments.len()
                    && from_gen
                        .type_arguments
                        .iter()
                        .zip(&to_gen.type_arguments)
                        .all(|(from_arg_id, to_arg_id)| {
                            if let (Some(from_arg), Some(to_arg)) = (
                                self.get_type_info(*from_arg_id),
                                self.get_type_info(*to_arg_id),
                            ) {
                                self.is_type_assignable(&from_arg.type_node, &to_arg.type_node)
                            } else {
                                false
                            }
                        })
            }

            (TypeNode::Module(from_mod), TypeNode::Module(to_mod)) => {
                from_mod.name == to_mod.name // Same module
            }

            // Unresolved references cannot be assigned until resolved by semantic analysis
            (TypeNode::UnresolvedReference { .. }, _) => false,
            (_, TypeNode::UnresolvedReference { .. }) => false,

            // Type imports cannot be assigned until resolved by semantic analysis
            (TypeNode::TypeImport { .. }, _) => false,
            (_, TypeNode::TypeImport { .. }) => false,

            _ => false,
        }
    }

    /// Create common built-in types
    pub fn create_builtin_types(&mut self) -> BuiltinTypes {
        let string_type = self.create_type(TypeInfo {
            type_node: TypeNode::Primitive(PrimitiveType::String),
            origin: TypeOrigin::Builtin,
        });

        let number_type = self.create_type(TypeInfo {
            type_node: TypeNode::Primitive(PrimitiveType::Number),
            origin: TypeOrigin::Builtin,
        });

        let boolean_type = self.create_type(TypeInfo {
            type_node: TypeNode::Primitive(PrimitiveType::Boolean),
            origin: TypeOrigin::Builtin,
        });

        let nil_type = self.create_type(TypeInfo {
            type_node: TypeNode::Primitive(PrimitiveType::Nil),
            origin: TypeOrigin::Builtin,
        });

        let dyn_top = self.create_type(TypeInfo {
            type_node: TypeNode::DynamicTop,
            origin: TypeOrigin::Builtin,
        });

        let dyn_nullable = self.create_type(TypeInfo {
            type_node: TypeNode::DynamicNullable,
            origin: TypeOrigin::Builtin,
        });

        let dyn_type = self.create_type(TypeInfo {
            type_node: TypeNode::Dynamic,
            origin: TypeOrigin::Builtin,
        });

        BuiltinTypes {
            string: string_type,
            number: number_type,
            boolean: boolean_type,
            nil: nil_type,
            dyn_top,
            dyn_nullable,
            dyn_type,
        }
    }
}

/// Information about a specific type
#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub type_node: TypeNode,
    pub origin: TypeOrigin,
}

/// Where a type came from (for error reporting and debugging)
#[derive(Debug, Clone)]
pub enum TypeOrigin {
    Builtin,
    Annotation(NodeId), // Explicitly annotated by user
    Inferred(NodeId),   // Inferred from expression
}

/// The actual type representation
#[derive(Debug, Clone, PartialEq)]
pub enum TypeNode {
    /// Primitive types: String, Number, Boolean, Nil
    Primitive(PrimitiveType),

    /// Array type: [T]
    Array(TypeId),

    /// Function type: (T1, T2) -> R
    Function(FunctionType),

    /// Union type: T1 | T2 | T3
    Union(Vec<TypeId>),

    /// Generic type: Array<T>, Map<K, V>
    Generic(GenericType),

    /// Type parameter: T, U, K, V
    TypeParameter(StringHandle),

    /// Constrained type parameter: T : [String]
    ConstrainedTypeParameter {
        name: StringHandle,
        constraint: TypeId,
    },

    /// Unresolved type reference: semantic analysis will resolve to Class, TypeAlias, or TypeParameter
    UnresolvedReference {
        name: StringHandle,
        identifier_node: NodeId, // For error reporting and source location
    },

    /// Object/record type: { name: String, age: Number }
    Object(ObjectType),

    /// Class type: User-defined classes with inheritance
    Class(ClassType),

    /// Dynamic type hierarchy
    DynamicTop, // dyn! - top type
    DynamicNullable, // dyn? - any value including nil
    Dynamic,         // dyn - any value excluding nil

    /// Module type: Imported modules with exported symbols
    Module(ModuleType),

    /// Type imported from another module: import("path").TypeName
    TypeImport {
        module_path: StringHandle,
        type_name: StringHandle,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    String,  // String
    Number,  // Number
    Boolean, // Boolean
    Nil,     // not explicitly declarable.
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub generic_parameters: Option<Vec<StringHandle>>,
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericType {
    pub base_name: StringHandle,     // Array, Map, etc.
    pub type_arguments: Vec<TypeId>, // [T], [K, V], etc.
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectType {
    pub fields: Vec<ObjectField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectField {
    pub name: StringHandle,
    pub field_type: TypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassType {
    pub name: StringHandle,
    pub generic_parameters: Option<Vec<StringHandle>>,
    pub superclass: Option<TypeId>, // Direct parent only
    pub fields: Vec<ClassField>,
    pub methods: Vec<ClassMethod>,
    pub node_id: NodeId, // Reference to the class declaration AST node
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassField {
    pub name: StringHandle,
    pub field_type: TypeId,
    pub inherited: bool, // True if inherited from parent
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassMethod {
    pub name: StringHandle,
    pub function_type: FunctionType,
    pub inherited: bool,      // True if inherited from parent
    pub is_initializer: bool, // True for "init" method
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleType {
    pub name: StringHandle,         // Module name/path
    pub exports: Vec<ModuleExport>, // What this module exports
    pub node_id: NodeId,            // Reference to the module declaration
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleExport {
    pub name: StringHandle,
    pub export_type: TypeId,
}

/// Common built-in types for convenience
#[derive(Debug, Clone)]
pub struct BuiltinTypes {
    pub string: TypeId,
    pub number: TypeId,
    pub boolean: TypeId,
    pub nil: TypeId,
    pub dyn_top: TypeId,
    pub dyn_nullable: TypeId,
    pub dyn_type: TypeId,
}

impl TypeTable {
    /// Helper to create an array type
    pub fn create_array_type(&mut self, element_type: TypeId, node: NodeId) -> TypeId {
        self.create_type(TypeInfo {
            type_node: TypeNode::Array(element_type),
            origin: TypeOrigin::Inferred(node),
        })
    }

    /// Helper to create a function type
    pub fn create_function_type(
        &mut self,
        param_types: Vec<TypeId>,
        return_type: TypeId,
        node: NodeId,
    ) -> TypeId {
        self.create_type(TypeInfo {
            type_node: TypeNode::Function(FunctionType {
                generic_parameters: None,
                parameters: param_types,
                return_type,
            }),
            origin: TypeOrigin::Inferred(node),
        })
    }

    /// Helper to create a union type
    pub fn create_union_type(&mut self, type_ids: Vec<TypeId>, node: NodeId) -> TypeId {
        self.create_type(TypeInfo {
            type_node: TypeNode::Union(type_ids),
            origin: TypeOrigin::Inferred(node),
        })
    }

    /// Create a class type from a class declaration
    pub fn create_class_type(
        &mut self,
        name: StringHandle,
        superclass_type: Option<TypeId>,
        node_id: NodeId,
    ) -> TypeId {
        // Start with empty fields and methods - they'll be populated during type checking
        let class_type = ClassType {
            name,
            generic_parameters: None,
            superclass: superclass_type,
            fields: Vec::new(),
            methods: Vec::new(),
            node_id,
        };

        self.create_type(TypeInfo {
            type_node: TypeNode::Class(class_type),
            origin: TypeOrigin::Annotation(node_id),
        })
    }

    /// Add a field to a class type
    pub fn add_class_field(
        &mut self,
        class_type_id: TypeId,
        field_name: StringHandle,
        field_type: TypeId,
    ) -> Result<(), String> {
        let type_info = self
            .type_storage
            .get_mut(class_type_id.0)
            .ok_or("Invalid class type ID")?;

        if let TypeNode::Class(ref mut class) = type_info.type_node {
            class.fields.push(ClassField {
                name: field_name,
                field_type,
                inherited: false,
            });
            Ok(())
        } else {
            Err("Type is not a class".to_string())
        }
    }

    /// Add a method to a class type
    pub fn add_class_method(
        &mut self,
        class_type_id: TypeId,
        method_name: StringHandle,
        method_type: TypeId,
        is_initializer: bool,
    ) -> Result<(), String> {
        let method_type_node = self
            .get_type_info(method_type)
            .ok_or("Invalid method type")?
            .type_node
            .clone();

        let function_type = match method_type_node {
            TypeNode::Function(func_type) => func_type,
            _ => return Err("Method type must be a function".to_string()),
        };

        let type_info = self
            .type_storage
            .get_mut(class_type_id.0)
            .ok_or("Invalid class type ID")?;

        if let TypeNode::Class(ref mut class) = type_info.type_node {
            class.methods.push(ClassMethod {
                name: method_name,
                function_type,
                inherited: false,
                is_initializer,
            });
            Ok(())
        } else {
            Err("Type is not a class".to_string())
        }
    }

    /// Inherit fields and methods from superclass (single level only)
    pub fn inherit_from_superclass(&mut self, class_type_id: TypeId) -> Result<(), String> {
        // Get the superclass type first
        let superclass_type = {
            let type_info = self
                .get_type_info(class_type_id)
                .ok_or("Invalid class type ID")?;

            if let TypeNode::Class(ref class) = type_info.type_node {
                class.superclass
            } else {
                return Err("Type is not a class".to_string());
            }
        };

        if let Some(superclass_id) = superclass_type {
            let super_class_info = self
                .get_type_info(superclass_id)
                .ok_or("Invalid superclass type ID")?;

            if let TypeNode::Class(super_class) = &super_class_info.type_node {
                // Clone the superclass fields and methods to avoid borrowing issues
                let inherited_fields: Vec<ClassField> = super_class
                    .fields
                    .iter()
                    .map(|field| ClassField {
                        name: field.name,
                        field_type: field.field_type,
                        inherited: true,
                    })
                    .collect();

                let inherited_methods: Vec<ClassMethod> = super_class
                    .methods
                    .iter()
                    .map(|method| ClassMethod {
                        name: method.name,
                        function_type: method.function_type.clone(),
                        inherited: true,
                        is_initializer: false,
                    })
                    .collect();

                // Now add them to the subclass
                let type_info = self
                    .type_storage
                    .get_mut(class_type_id.0)
                    .ok_or("Invalid class type ID")?;

                if let TypeNode::Class(ref mut class) = type_info.type_node {
                    // Add inherited fields (at the beginning)
                    class.fields.splice(0..0, inherited_fields);
                    // Add inherited methods (at the beginning)
                    class.methods.splice(0..0, inherited_methods);
                }
            }
        }

        Ok(())
    }

    /// Look up a field in a class (including inherited fields)
    pub fn get_class_field(
        &self,
        class_type_id: TypeId,
        field_name: StringHandle,
    ) -> Option<&ClassField> {
        let type_info = self.get_type_info(class_type_id)?;

        if let TypeNode::Class(ref class) = type_info.type_node {
            class.fields.iter().find(|field| field.name == field_name)
        } else {
            None
        }
    }

    /// Look up a method in a class (including inherited methods)
    pub fn get_class_method(
        &self,
        class_type_id: TypeId,
        method_name: StringHandle,
    ) -> Option<&ClassMethod> {
        let type_info = self.get_type_info(class_type_id)?;

        if let TypeNode::Class(ref class) = type_info.type_node {
            class
                .methods
                .iter()
                .find(|method| method.name == method_name)
        } else {
            None
        }
    }

    /// Create a module type
    pub fn create_module_type(&mut self, name: StringHandle, node_id: NodeId) -> TypeId {
        let module_type = ModuleType {
            name,
            exports: Vec::new(),
            node_id,
        };

        self.create_type(TypeInfo {
            type_node: TypeNode::Module(module_type),
            origin: TypeOrigin::Annotation(node_id),
        })
    }

    /// Add an export to a module
    pub fn add_module_export(
        &mut self,
        module_type_id: TypeId,
        export_name: StringHandle,
        export_type: TypeId,
    ) -> Result<(), String> {
        let type_info = self
            .type_storage
            .get_mut(module_type_id.0)
            .ok_or("Invalid module type ID")?;

        if let TypeNode::Module(ref mut module) = type_info.type_node {
            module.exports.push(ModuleExport {
                name: export_name,
                export_type,
            });
            Ok(())
        } else {
            Err("Type is not a module".to_string())
        }
    }

    /// Format a type for display purposes
    pub fn format_type(&self, type_id: TypeId, interner: &StringInterner) -> String {
        if let Some(type_info) = self.get_type_info(type_id) {
            self.format_type_node(&type_info.type_node, interner)
        } else {
            "<unknown type>".to_string()
        }
    }

    /// Format a TypeNode for display purposes
    fn format_type_node(&self, type_node: &TypeNode, interner: &StringInterner) -> String {
        match type_node {
            TypeNode::Primitive(prim) => format!("{}", prim),
            TypeNode::Array(elem_id) => {
                format!("[{}]", self.format_type(*elem_id, interner))
            }
            TypeNode::Function(func) => {
                let mut result = String::from("(");
                for (i, param_id) in func.parameters.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&self.format_type(*param_id, interner));
                }
                result.push_str(") -> ");
                result.push_str(&self.format_type(func.return_type, interner));
                result
            }
            TypeNode::Union(type_ids) => {
                // Check if the union contains nil by resolving each TypeId
                let mut nil_found = false;
                let mut non_nil_types = Vec::new();

                for type_id in type_ids {
                    if let Some(type_info) = self.get_type_info(*type_id) {
                        if matches!(type_info.type_node, TypeNode::Primitive(PrimitiveType::Nil)) {
                            nil_found = true;
                        } else {
                            non_nil_types.push(*type_id);
                        }
                    }
                }

                if nil_found && !non_nil_types.is_empty() {
                    // Nullable syntax: show non-nil types with ? suffix
                    let mut result = String::new();
                    for (i, type_id) in non_nil_types.iter().enumerate() {
                        if i > 0 {
                            result.push_str(" | ");
                        }
                        result.push_str(&format!("{}?", self.format_type(*type_id, interner)));
                    }
                    result
                } else {
                    // Regular union syntax
                    let mut result = String::new();
                    for (i, type_id) in type_ids.iter().enumerate() {
                        if i > 0 {
                            result.push_str(" | ");
                        }
                        result.push_str(&self.format_type(*type_id, interner));
                    }
                    result
                }
            }
            TypeNode::Generic(generic) => {
                let mut result = format!("{}<", interner.get_string(generic.base_name));
                for (i, arg_id) in generic.type_arguments.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&self.format_type(*arg_id, interner));
                }
                result.push('>');
                result
            }
            TypeNode::TypeParameter(name) => interner.get_string(*name).to_string(),
            TypeNode::ConstrainedTypeParameter { name, constraint } => {
                format!(
                    "{} : {}",
                    interner.get_string(*name),
                    self.format_type(*constraint, interner)
                )
            }
            TypeNode::UnresolvedReference { name, .. } => interner.get_string(*name).to_string(),
            TypeNode::Object(obj) => {
                let mut result = String::from("{ ");
                for (i, field) in obj.fields.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&format!(
                        "{}: {}",
                        interner.get_string(field.name),
                        self.format_type(field.field_type, interner)
                    ));
                }
                result.push_str(" }");
                result
            }
            TypeNode::Class(class) => {
                format!("class {}", interner.get_string(class.name))
            }
            TypeNode::DynamicTop => "dyn!".to_string(),
            TypeNode::DynamicNullable => "dyn?".to_string(),
            TypeNode::Dynamic => "dyn".to_string(),
            TypeNode::Module(module) => {
                format!("module {}", interner.get_string(module.name))
            }
            TypeNode::TypeImport {
                module_path,
                type_name,
            } => {
                format!(
                    "import(\"{}\").{}",
                    interner.get_string(*module_path),
                    interner.get_string(*type_name)
                )
            }
        }
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::String => write!(f, "String"),
            PrimitiveType::Number => write!(f, "Number"),
            PrimitiveType::Boolean => write!(f, "Boolean"),
            PrimitiveType::Nil => write!(f, "nil"),
        }
    }
}
