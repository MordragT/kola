use derive_more::{Display, FromStr};
use std::fmt;

#[derive(Debug, Display, FromStr, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BuiltinType {
    Unit,
    Bool,
    Num,
    Char,
    Str,
    List,
}

impl BuiltinType {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "Unit" => Some(Self::Unit),
            "Bool" => Some(Self::Bool),
            "Num" => Some(Self::Num),
            "Char" => Some(Self::Char),
            "Str" => Some(Self::Str),
            "List" => Some(Self::List),
            _ => None,
        }
    }
}

#[inline]
pub fn is_builtin_type(s: &str) -> bool {
    matches!(s, "Unit" | "Bool" | "Num" | "Char" | "Str" | "List")
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Builtin {
    pub name: &'static str,
    pub type_: TypeSchemeProtocol,
}

impl Builtin {
    const fn new(name: &'static str, type_: TypeSchemeProtocol) -> Self {
        Self { name, type_ }
    }

    #[inline]
    pub fn from_name(name: &'static str) -> Option<Self> {
        find_builtin(name)
    }

    #[inline]
    pub fn from_id(id: BuiltinId) -> Self {
        BUILTINS[id as usize]
    }
}

#[inline]
pub fn is_builtin(s: &str) -> bool {
    BUILTINS.iter().any(|builtin| builtin.name == s)
}

#[inline]
pub fn find_builtin(s: &str) -> Option<Builtin> {
    BUILTINS.iter().find(|builtin| builtin.name == s).copied()
}

#[inline]
pub fn find_builtin_id(s: &str) -> Option<BuiltinId> {
    BUILTINS
        .iter()
        .position(|builtin| builtin.name == s)
        .map(|id| BuiltinId::from_usize(id))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeProtocol {
    Unit,
    Bool,
    Num,
    Char,
    Str,
    List(&'static Self),
    Record(&'static [(&'static str, Self)]),
    Lambda(&'static Self, &'static Self),
    Var(u32),
}

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct ParseTypeError;

// impl FromStr for Type {
//     type Err = ParseTypeError;

//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         match s {
//             "Bool" => Ok(Type::Bool),
//             "Num" => Ok(Type::Num),
//             "Char" => Ok(Type::Char),
//             "Str" => Ok(Type::Str),
//             _ => Err(ParseTypeError),
//         }
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeSchemeProtocol {
    pub vars_count: u32,
    pub input: TypeProtocol,
    pub output: TypeProtocol,
}

// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub enum BuiltinId {
//     ListLen = 0,
//     ListHead = 1,
// }

// pub const BUILTINS: &[Builtin] = &[
//     Builtin::new(
//         BuiltinId::ListLen,
//         "list_len",
//         TypeScheme {
//             vars: &[0],
//             ty: Type::Lambda(&Type::List(&Type::Var(0)), &Type::Num),
//         },
//     ),
//     Builtin::new(
//         BuiltinId::ListHead,
//         "list_head",
//         TypeScheme {
//             vars: &[0],
//             ty: Type::Lambda(
//                 &Type::List(&Type::Var(0)),
//                 &Type::Record(&[("value", Type::Var(0)), ("next", Type::List(&Type::Var(0)))]),
//             ),
//         },
//     ),
// ];

macro_rules! define_builtins {
    (
        $(
            $name:ident : forall $($var:literal),+ . $input:tt -> $output:tt
        ),* $(,)?
    ) => {
        paste::paste! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub enum BuiltinId {
                $(
                    [<$name:camel>],
                )*
            }

            impl BuiltinId {
                #[inline]
                const fn from_usize(id: usize) -> Self {
                    match id {
                        $(
                            x if x == BuiltinId::[<$name:camel>] as usize => BuiltinId::[<$name:camel>],
                        )*
                        _ => panic!("Invalid BuiltinId"),
                    }
                }
            }

            impl fmt::Display for BuiltinId {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    match self {
                        $(
                            BuiltinId::[<$name:camel>] => write!(f, "__builtin_{}", stringify!($name)),
                        )*
                    }
                }
            }
        }

        #[doc = "Array of all builtin function definitions"]
        const BUILTINS: &[Builtin] = &[
            $(
                Builtin {
                    name: stringify!($name),
                    type_: TypeSchemeProtocol {
                        vars_count: define_builtins!(@count $($var),+),
                        input: define_builtins!(@type $input),
                        output: define_builtins!(@type $output),
                    },
                },
            )*
        ];
    };

    // Simple counting - assumes variables are sequential starting from 0
    (@count 0) => { 1 };
    (@count 0, 1) => { 2 };
    (@count 0, 1, 2) => { 3 };
    (@count 0, 1, 2, 3) => { 4 };

    // For single variable cases
    (@count $var:literal) => { $var + 1 };

    // Type expression parsing
    (@type Unit) => { TypeProtocol::Unit };
    (@type Bool) => { TypeProtocol::Bool };
    (@type Num) => { TypeProtocol::Num };
    (@type Char) => { TypeProtocol::Char };
    (@type Str) => { TypeProtocol::Str };
    (@type $var:literal) => { TypeProtocol::Var($var) };
    (@type (List $inner:tt)) => {
        TypeProtocol::List(&define_builtins!(@type $inner))
    };
    (@type { $($field:literal : $field_type:tt),* $(,)? }) => {
        TypeProtocol::Record(&[
            $(($field, define_builtins!(@type $field_type))),*
        ])
    };
    (@type ($left:tt -> $right:tt)) => {
        TypeProtocol::Lambda(
            &define_builtins!(@type $left),
            &define_builtins!(@type $right)
        )
    };
}

define_builtins! {
    list_len: forall 0 . (List 0) -> Num,
    // list_head: forall 0 . (List 0) -> { "value": 0, "next": (List 0) },
    // list_tail: forall 0 . (List 0) -> (List 0),
    // list_cons: forall 0 . { "value": 0, "next": (List 0) } -> (List 0),
    // list_is_empty: forall 0 . (List 0) -> Bool,
    // list_map: forall 0, 1 . (({ "value": 0 } -> { "value": 1 }) -> (List 0)) -> (List 1),
    // list_filter: forall 0 . (({ "value": 0 } -> Bool) -> (List 0)) -> (List 0),
}
