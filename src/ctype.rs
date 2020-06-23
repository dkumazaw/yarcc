// Type
use std::slice::Iter;

const INIT: usize = 0;
const VOID: usize = 1;
const CHAR: usize = 1 << 1;
const SHORT: usize = 1 << 2;
const INT: usize = 1 << 3;
const LONG: usize = 1 << 4;

#[derive(Debug, Clone)]
pub struct Type {
    kind: TypeKind,
    pub is_const: bool,
    pub is_volatile: bool,
}

/// Bitflag to configure type
#[derive(Debug, Clone)]
pub struct TypeConfig {
    config: usize,
}

impl TypeConfig {
    pub fn new() -> Self {
        TypeConfig { config: INIT }
    }

    pub fn add(&mut self, s: &str) -> Result<(), &'static str> {
        let to_add = match s {
            "void" => VOID,
            "char" => CHAR,
            "short" => SHORT,
            "int" => INT,
            "long" => LONG,
            _ => {
                return Err("Unsupported type was provided.");
            }
        };

        if (self.config & to_add) != 0 {
            return Err("Identical type names.");
        }
        self.config |= to_add;
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum TypeKind {
    VOID,
    CHAR,
    SHORT,
    INT,
    LONG,
    PTR {
        ptr_to: Box<Type>,
    },
    ARRAY {
        size: usize,
        ptr_to: Box<Type>,
    },
    STRUCT {
        size: usize,
        members: Vec<StructMember>,
    },
    ENUM {
        members: Vec<EnumMember>,
    },
    FUNCTION {
        args: Vec<(String, Type)>,
    },
    INCOMPLETE {
        kind: IncompleteKind,
    },
}

#[derive(Debug, Clone)]
pub struct StructMember {
    pub name: String,
    pub ty: Type,
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub struct EnumMember {
    pub name: String,
    pub val: i32,
}

#[derive(Debug, Clone)]
pub enum IncompleteKind {
    VOID,   // TODO: Rethink this...?
    ARRAY,  // Unknow size
    STRUCT, // Unknown content
    ENUM,   // Unknown content
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Type {
    fn new_from_kind(kind: TypeKind) -> Self {
        Type {
            kind: kind,
            is_const: false,
            is_volatile: false,
        }
    }

    // Constructors:
    pub fn new_base(basekind: &str) -> Self {
        let kind = match basekind {
            "char" => TypeKind::CHAR,
            "short" => TypeKind::SHORT,
            "int" => TypeKind::INT,
            "long" => TypeKind::LONG,
            _ => panic!("Non-base kind was provided."),
        };
        Self::new_from_kind(kind)
    }

    pub fn new_from_config(tc: TypeConfig) -> Result<Self, &'static str> {
        match tc.config {
            VOID => Ok(Self::new_from_kind(TypeKind::VOID)),
            CHAR => Ok(Self::new_from_kind(TypeKind::CHAR)),
            SHORT => Ok(Self::new_from_kind(TypeKind::SHORT)),
            INT => Ok(Self::new_from_kind(TypeKind::INT)),
            LONG => Ok(Self::new_from_kind(TypeKind::LONG)),
            _ => Err("Invalid set of type specifiers."),
        }
    }

    /// Create a new Type who is a pointer to the basety
    pub fn new_ptr(basety: Self) -> Self {
        let kind = TypeKind::PTR {
            ptr_to: Box::new(basety),
        };
        Self::new_from_kind(kind)
    }

    pub fn new_array(basety: Self, array_size: usize) -> Self {
        let kind = TypeKind::ARRAY {
            size: array_size,
            ptr_to: Box::new(basety),
        };
        Self::new_from_kind(kind)
    }

    pub fn new_enum(members: Vec<EnumMember>) -> Self {
        let kind = TypeKind::ENUM { members: members };
        Self::new_from_kind(kind)
    }

    pub fn new_struct(size: usize, members: Vec<StructMember>) -> Self {
        let kind = TypeKind::STRUCT {
            size: size,
            members: members,
        };
        Self::new_from_kind(kind)
    }

    pub fn new_function(args: Vec<(String, Type)>) -> Self {
        let kind = TypeKind::FUNCTION { args: args };
        println!("{:?}", kind);
        Self::new_from_kind(kind)
    }

    pub fn new_incomplete(kind: IncompleteKind) -> Self {
        let tykind = TypeKind::INCOMPLETE { kind: kind };
        Self::new_from_kind(tykind)
    }

    pub fn clone_base(&self) -> Self {
        use TypeKind::*;

        match self.kind {
            PTR { ref ptr_to } | ARRAY { ref ptr_to, .. } => *(ptr_to.clone()),
            _ => panic!("Trying to clone the base of terminal types."),
        }
    }

    pub fn new_ptr_to(&self) -> Self {
        let kind = TypeKind::PTR {
            ptr_to: Box::new(self.clone()),
        };
        Self::new_from_kind(kind)
    }

    pub fn is_void(&self) -> bool {
        use TypeKind::VOID;
        match self.kind {
            VOID => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        use TypeKind::ARRAY;
        match self.kind {
            ARRAY { .. } => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        use TypeKind::{INCOMPLETE, STRUCT};
        match self.kind {
            STRUCT { .. } => true,
            INCOMPLETE { ref kind } => match kind {
                IncompleteKind::STRUCT { .. } => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        use TypeKind::{ENUM, INCOMPLETE};
        match self.kind {
            ENUM { .. } => true,
            INCOMPLETE { ref kind } => match kind {
                IncompleteKind::ENUM { .. } => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        use TypeKind::FUNCTION;
        match self.kind {
            FUNCTION { .. } => true,
            _ => false,
        }
    }

    pub fn is_incomplete(&self) -> bool {
        use TypeKind::INCOMPLETE;
        match self.kind {
            INCOMPLETE { .. } => true,
            _ => false,
        }
    }

    pub fn is_ptr_like(&self) -> bool {
        use TypeKind::*;
        match self.kind {
            PTR { .. } | ARRAY { .. } => true,
            _ => false,
        }
    }

    pub fn is_integral(&self) -> bool {
        use TypeKind::*;
        match self.kind {
            CHAR | SHORT | INT | LONG | ENUM { .. } => true,
            _ => false,
        }
    }

    pub fn is_scalar(&self) -> bool {
        use TypeKind::*;
        if self.is_integral() {
            return true;
        }
        match self.kind {
            PTR { .. } => true,
            _ => false,
        }
    }

    pub fn size(&self) -> usize {
        use TypeKind::*;
        match self.kind {
            VOID => panic!("not implemented"),
            CHAR => 1,
            SHORT => 2,
            INT => 4,
            LONG | PTR { .. } => 8,
            ARRAY { .. } => self.base_size(),
            STRUCT { size, .. } => size.clone(),
            ENUM { .. } => 4,
            FUNCTION { .. } => panic!("not implemented"),
            INCOMPLETE { .. } => panic!("Requesting size of an incomplete type."),
        }
    }

    // TODO: This may be a bit confusing. Maybe name it like "array_size"?
    pub fn total_size(&self) -> usize {
        use TypeKind::*;
        match self.kind {
            ARRAY { size, .. } => size.clone() * self.size(),
            _ => self.size(),
        }
    }

    pub fn base_size(&self) -> usize {
        use TypeKind::*;
        match self.kind {
            PTR { ref ptr_to } | ARRAY { ref ptr_to, .. } => ptr_to.total_size(),
            _ => panic!("Requesting a base size for a terminal type."),
        }
    }

    pub fn set_type_qual(&mut self, is_const: bool, is_volatile: bool) {
        self.is_const = is_const;
        self.is_volatile = is_volatile;
    }

    /// Returns the relative offset and type of a member of struct
    /// None is returned if no such member exists
    pub fn get_member_offset(&self, name: &str) -> Option<(usize, Self)> {
        use TypeKind::{INCOMPLETE, STRUCT};
        if !self.is_struct() {
            panic!("Requesting a member offset from a non-struct type.")
        }
        match self.kind {
            STRUCT { ref members, .. } => {
                let maybe_found = members.iter().find(|m| m.name == name);
                if let Some(found) = maybe_found {
                    Some((found.offset, found.ty.clone()))
                } else {
                    None
                }
            }
            INCOMPLETE { ref kind } => panic!("Not implemented yet."),
            _ => panic!("Unreacheable."),
        }
    }

    // Returns an iterator over arguments of a function type.
    pub fn iter_func_args(&self) -> Iter<(String, Type)> {
        use TypeKind::FUNCTION;
        match self.kind {
            FUNCTION { ref args } => args.iter(),
            _ => panic!("Requesting an argument iterator from non-function type."),
        }
    }
}
