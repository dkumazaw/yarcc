// Type

#[derive(Debug, Clone)]
pub enum Type {
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
    VOID,
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
    pub fn new_base(basekind: &str) -> Self {
        match basekind {
            "char" => Type::CHAR,
            "short" => Type::SHORT,
            "int" => Type::INT,
            "long" => Type::LONG,
            _ => panic!("Non-base kind was provided."),
        }
    }

    // Create a new Type from the provided base type.
    pub fn new_from(basety: Self, ref_depth: usize) -> Self {
        if ref_depth == 0 {
            basety
        } else {
            Type::PTR {
                ptr_to: Box::new(Type::new_from(basety, ref_depth - 1)),
            }
        }
    }

    // Create a new Type based on the kind str str.
    pub fn new(basekind: &str, ref_depth: usize) -> Self {
        if ref_depth == 0 {
            Type::new_base(basekind)
        } else {
            Type::PTR {
                ptr_to: Box::new(Type::new(basekind, ref_depth - 1)),
            }
        }
    }

    pub fn new_array(basekind: &str, ref_depth: usize, array_size: usize) -> Self {
        Type::ARRAY {
            size: array_size,
            ptr_to: Box::new(Type::new(basekind, ref_depth)),
        }
    }

    pub fn new_incomplete(kind: IncompleteKind) -> Self {
        Type::INCOMPLETE { kind: kind }
    }

    pub fn clone_base(&self) -> Self {
        use Type::*;

        match self {
            PTR { ref ptr_to } | ARRAY { ref ptr_to, .. } => *(ptr_to.clone()),
            _ => panic!("Trying to clone the base of terminal types."),
        }
    }

    pub fn as_str(&self) -> &str {
        use Type::*;

        match self {
            CHAR => "char",
            SHORT => "short",
            INT => "int",
            LONG => "long",
            STRUCT { .. } => "struct",
            _ => panic!("This is not a base type."),
        }
    }

    pub fn new_ptr_to(&self) -> Self {
        Type::PTR {
            ptr_to: Box::new(self.clone()),
        }
    }

    pub fn is_struct(&self) -> bool {
        use Type::{INCOMPLETE, STRUCT};
        match self {
            STRUCT { .. } => true,
            INCOMPLETE { ref kind } => match kind {
                IncompleteKind::STRUCT { .. } => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        use Type::{ENUM, INCOMPLETE};
        match self {
            ENUM { .. } => true,
            INCOMPLETE { ref kind } => match kind {
                IncompleteKind::ENUM { .. } => true,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_incomplete(&self) -> bool {
        use Type::INCOMPLETE;
        match self {
            INCOMPLETE { .. } => true,
            _ => false,
        }
    }

    pub fn is_ptr_like(&self) -> bool {
        use Type::*;
        match self {
            PTR { .. } | ARRAY { .. } => true,
            _ => false,
        }
    }

    pub fn size(&self) -> usize {
        use Type::*;
        match self {
            CHAR => 1,
            SHORT => 2,
            INT => 4,
            LONG | PTR { .. } => 8,
            ARRAY { .. } => self.base_size(),
            STRUCT { size, .. } => size.clone(),
            ENUM { .. } => 4,
            INCOMPLETE { .. } => panic!("Requesting size of an incomplete type."),
        }
    }

    // TODO: This may be a bit confusing. Maybe name it like "array_size"?
    pub fn total_size(&self) -> usize {
        use Type::*;
        match self {
            ARRAY { size, .. } => size.clone() * self.size(),
            _ => self.size(),
        }
    }

    pub fn base_size(&self) -> usize {
        use Type::*;
        match self {
            PTR { ptr_to } | ARRAY { ptr_to, .. } => ptr_to.size(),
            _ => panic!("Requesting a base size for a terminal type."),
        }
    }

    /// Returns the relative offset and type of a member of struct
    /// None is returned if no such member exists
    pub fn get_member_offset(&self, name: &str) -> Option<(usize, Self)> {
        use Type::{INCOMPLETE, STRUCT};
        if !self.is_struct() {
            panic!("Requesting a member offset from a non-struct type.")
        }
        match self {
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
}
