use crate::{NodeId, StringHandle, frontend::type_arena::TypeArena};
use std::fmt;

/// Unique identifier for types in the type arena
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

impl Default for TypeId {
    fn default() -> Self {
        TypeArena::UNKNOWN
    }
}

/// Basic types in the Qang language
#[derive(Debug, Clone, PartialEq)]
pub enum QangType {
    /// Unknown type (used during inference)
    Unknown,
    /// Bottom type (never returns)
    Never,
    /// Unit type (no value)
    Unit,
    /// Nullable wrapper around another type
    Optional(TypeId),
    /// Basic primitive types
    Number,
    String,
    Boolean,
    /// Function type with parameters and return type
    Function {
        params: Vec<TypeId>,
        return_type: TypeId,
    },
    /// Class type with name reference
    Class {
        name: StringHandle,
        /// Reference to the class declaration node
        class_node: NodeId,
    },
    /// Array type with element type
    Array(TypeId),
    /// Object type with field types
    Object {
        fields: Vec<(StringHandle, TypeId)>,
    },
    /// Module type - semantically like an object but with module-specific behavior
    Module {
        /// Module name for identification
        path: StringHandle,
        /// Exported members (functions, classes, variables)
        exports: Vec<(StringHandle, TypeId)>,
        /// Reference to the module declaration node
        module_node: NodeId,
    },
}

impl fmt::Display for QangType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            QangType::Unknown => write!(f, "?"),
            QangType::Never => write!(f, "!"),
            QangType::Unit => write!(f, "()"),
            QangType::Optional(inner) => write!(f, "?{}", inner.0),
            QangType::Number => write!(f, "number"),
            QangType::String => write!(f, "string"),
            QangType::Boolean => write!(f, "boolean"),
            QangType::Function {
                params,
                return_type,
            } => {
                write!(f, "(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param.0)?;
                }
                write!(f, ") -> {}", return_type.0)
            }
            QangType::Class { name, .. } => write!(f, "class#{}", name),
            QangType::Array(element) => write!(f, "[{}]", element.0),
            QangType::Object { fields } => {
                write!(f, "{{ ")?;
                for (i, (name, ty)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, ty.0)?;
                }
                write!(f, " }}")
            }
            QangType::Module {
                path: name,
                exports,
                ..
            } => {
                write!(f, "module {} {{ ", name)?;
                for (i, (export_name, ty)) in exports.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", export_name, ty.0)?;
                }
                write!(f, " }}")
            }
        }
    }
}
