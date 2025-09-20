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
            (TypeNode::Array(from_elem), TypeNode::Array(to_elem)) => {
                self.is_type_assignable(from_elem, to_elem)
            }

            // Function types (contravariant in params, covariant in return)
            (TypeNode::Function(from_fn), TypeNode::Function(to_fn)) => {
                if from_fn.parameters.len() != to_fn.parameters.len() {
                    return false;
                }

                // Parameters are contravariant
                for (from_param, to_param) in from_fn.parameters.iter().zip(&to_fn.parameters) {
                    if !self.is_type_assignable(to_param, from_param) {
                        return false;
                    }
                }

                // Return type is covariant
                self.is_type_assignable(&from_fn.return_type, &to_fn.return_type)
            }

            // Class inheritance (subclass assignable to superclass)
            (TypeNode::Class(from_class), TypeNode::Class(to_class)) => {
                if from_class.name == to_class.name {
                    return true;
                }

                // Check if from_class inherits from to_class (single level only)
                if let Some(ref superclass) = from_class.superclass {
                    if let TypeNode::Class(super_class) = superclass.as_ref() {
                        return super_class.name == to_class.name;
                    }
                }

                false
            }

            // Union types
            (from_type, TypeNode::Union(union_types)) => union_types
                .iter()
                .any(|to_type| self.is_type_assignable(from_type, to_type)),

            // Generic types (nominal typing for now)
            (TypeNode::Generic(from_gen), TypeNode::Generic(to_gen)) => {
                from_gen.base_name == to_gen.base_name
                    && from_gen.type_arguments.len() == to_gen.type_arguments.len()
                    && from_gen
                        .type_arguments
                        .iter()
                        .zip(&to_gen.type_arguments)
                        .all(|(from_arg, to_arg)| self.is_type_assignable(from_arg, to_arg))
            }

            (TypeNode::Module(from_mod), TypeNode::Module(to_mod)) => {
                from_mod.name == to_mod.name // Same module
            }

            // Unresolved references cannot be assigned until resolved by semantic analysis
            (TypeNode::UnresolvedReference { .. }, _) => false,
            (_, TypeNode::UnresolvedReference { .. }) => false,

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
    Annotation(NodeId),    // Explicitly annotated by user
    Inferred(NodeId),      // Inferred from expression
    Generic(StringHandle), // Generic type parameter
}

/// The actual type representation
#[derive(Debug, Clone, PartialEq)]
pub enum TypeNode {
    /// Primitive types: String, Number, Boolean, Nil
    Primitive(PrimitiveType),

    /// Array type: [T]
    Array(Box<TypeNode>),

    /// Function type: (T1, T2) -> R
    Function(FunctionType),

    /// Union type: T1 | T2 | T3
    Union(Vec<TypeNode>),

    /// Generic type: Array<T>, Map<K, V>
    Generic(GenericType),

    /// Type parameter: T, U, K, V
    TypeParameter(StringHandle),

    /// Unresolved type reference: semantic analysis will resolve to Class, TypeAlias, or TypeParameter
    UnresolvedReference {
        name: StringHandle,
        identifier_node: NodeId,  // For error reporting and source location
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
    pub parameters: Vec<TypeNode>,
    pub return_type: Box<TypeNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericType {
    pub base_name: StringHandle,       // Array, Map, etc.
    pub type_arguments: Vec<TypeNode>, // [T], [K, V], etc.
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectType {
    pub fields: Vec<ObjectField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectField {
    pub name: StringHandle,
    pub field_type: TypeNode,
    pub optional: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassType {
    pub name: StringHandle,
    pub superclass: Option<Box<TypeNode>>, // Direct parent only
    pub fields: Vec<ClassField>,
    pub methods: Vec<ClassMethod>,
    pub node_id: NodeId, // Reference to the class declaration AST node
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassField {
    pub name: StringHandle,
    pub field_type: TypeNode,
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
    pub export_type: TypeNode,
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
        if let Some(element_info) = self.get_type_info(element_type) {
            self.create_type(TypeInfo {
                type_node: TypeNode::Array(Box::new(element_info.type_node.clone())),
                origin: TypeOrigin::Inferred(node),
            })
        } else {
            // Fallback to dyn! array
            self.create_type(TypeInfo {
                type_node: TypeNode::Array(Box::new(TypeNode::DynamicTop)),
                origin: TypeOrigin::Inferred(node),
            })
        }
    }

    /// Helper to create a function type
    pub fn create_function_type(
        &mut self,
        param_types: Vec<TypeId>,
        return_type: TypeId,
        node: NodeId,
    ) -> TypeId {
        let param_nodes: Vec<TypeNode> = param_types
            .into_iter()
            .filter_map(|id| self.get_type_info(id))
            .map(|info| info.type_node.clone())
            .collect();

        let return_node = self
            .get_type_info(return_type)
            .map(|info| info.type_node.clone())
            .unwrap_or(TypeNode::DynamicTop);

        self.create_type(TypeInfo {
            type_node: TypeNode::Function(FunctionType {
                parameters: param_nodes,
                return_type: Box::new(return_node),
            }),
            origin: TypeOrigin::Inferred(node),
        })
    }

    /// Helper to create a union type
    pub fn create_union_type(&mut self, type_ids: Vec<TypeId>, node: NodeId) -> TypeId {
        let type_nodes: Vec<TypeNode> = type_ids
            .into_iter()
            .filter_map(|id| self.get_type_info(id))
            .map(|info| info.type_node.clone())
            .collect();

        self.create_type(TypeInfo {
            type_node: TypeNode::Union(type_nodes),
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
        let superclass = superclass_type
            .and_then(|id| self.get_type_info(id))
            .map(|info| Box::new(info.type_node.clone()));

        // Start with empty fields and methods - they'll be populated during type checking
        let class_type = ClassType {
            name,
            superclass,
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
        let field_type_node = self
            .get_type_info(field_type)
            .ok_or("Invalid field type")?
            .type_node
            .clone();

        let type_info = self
            .type_storage
            .get_mut(class_type_id.0)
            .ok_or("Invalid class type ID")?;

        if let TypeNode::Class(ref mut class) = type_info.type_node {
            class.fields.push(ClassField {
                name: field_name,
                field_type: field_type_node,
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
                class.superclass.clone()
            } else {
                return Err("Type is not a class".to_string());
            }
        };

        if let Some(superclass) = superclass_type {
            if let TypeNode::Class(super_class) = superclass.as_ref() {
                // Clone the superclass fields and methods to avoid borrowing issues
                let inherited_fields: Vec<ClassField> = super_class
                    .fields
                    .iter()
                    .map(|field| ClassField {
                        name: field.name,
                        field_type: field.field_type.clone(),
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
        let export_type_node = self
            .get_type_info(export_type)
            .ok_or("Invalid export type")?
            .type_node
            .clone();

        let type_info = self
            .type_storage
            .get_mut(module_type_id.0)
            .ok_or("Invalid module type ID")?;

        if let TypeNode::Module(ref mut module) = type_info.type_node {
            module.exports.push(ModuleExport {
                name: export_name,
                export_type: export_type_node,
            });
            Ok(())
        } else {
            Err("Type is not a module".to_string())
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypeEnvironment {
    /// Maps type names to their definitions
    type_names: FxHashMap<StringHandle, TypeId>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            type_names: FxHashMap::with_hasher(FxBuildHasher),
        }
    }

    pub fn declare_type(&mut self, name: StringHandle, type_id: TypeId) {
        self.type_names.insert(name, type_id);
    }

    pub fn lookup_type(&self, name: StringHandle) -> Option<TypeId> {
        self.type_names.get(&name).copied()
    }
}

// Display implementations for nice error messages
impl fmt::Display for TypeNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeNode::Primitive(prim) => write!(f, "{}", prim),
            TypeNode::Array(elem) => write!(f, "[{}]", elem),
            TypeNode::Function(func) => {
                write!(f, "(")?;
                for (i, param) in func.parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", func.return_type)
            }
            TypeNode::Union(types) => {
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", t)?;
                }
                Ok(())
            }
            TypeNode::Generic(genr) => {
                write!(f, "{}<", genr.base_name)?;
                for (i, arg) in genr.type_arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")
            }
            TypeNode::TypeParameter(name) => write!(f, "{}", name),
            TypeNode::UnresolvedReference { name, .. } => write!(f, "{}", name),
            TypeNode::Object(obj) => {
                write!(f, "{{ ")?;
                for (i, field) in obj.fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(
                        f,
                        "{}{}: {}",
                        field.name,
                        if field.optional { "?" } else { "" },
                        field.field_type
                    )?;
                }
                write!(f, " }}")
            }
            TypeNode::Class(class) => {
                write!(f, "class {}", class.name)
            }
            TypeNode::DynamicTop => write!(f, "dyn!"),
            TypeNode::DynamicNullable => write!(f, "dyn?"),
            TypeNode::Dynamic => write!(f, "dyn"),
            TypeNode::Module(module) => {
                write!(f, "module {}", module.name)
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
