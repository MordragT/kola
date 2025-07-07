use derive_more::{Display, FromStr};
use std::fmt;

#[derive(Debug, Display, FromStr, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BuiltinEffect {
    Pure,
}

impl BuiltinEffect {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "Pure" => Some(Self::Pure),
            _ => None,
        }
    }
}

#[inline]
pub fn is_builtin_effect(s: &str) -> bool {
    matches!(s, "Pure")
}

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
    Variant(&'static [(&'static str, Self)]),
    Lambda(&'static Self, &'static Self), // restrict to first order functions
    Var(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeSchemeProtocol {
    pub vars_count: u32,
    pub input: TypeProtocol,
    pub output: TypeProtocol,
}

macro_rules! define_builtins {
    (
        $(
            $name:ident : forall $count:literal . $input:tt -> $output:tt
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
                        vars_count: $count,
                        input: define_builtins!(@type $input),
                        output: define_builtins!(@type $output),
                    },
                },
            )*
        ];
    };

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
    (@type [ $($variant:literal : $variant_type:tt),* $(,)? ] ) => {
        TypeProtocol::Variant(&[
            $(($variant, define_builtins!(@type $variant_type))),*
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
    // Builtin List functions
    list_length: forall 1 . (List 0) -> Num,
    list_is_empty: forall 1 . (List 0) -> Bool,

    list_reverse: forall 1 . (List 0) -> (List 0),
    list_sum: forall 0 . (List Num) -> Num,

    list_first: forall 1 . (List 0) -> [ "Some": 0, "None": Unit ],
    list_last: forall 1 . (List 0) -> [ "Some": 0, "None": Unit ],

    list_contains: forall 1 . { "list": (List 0), "value": 0 } -> Bool,
    list_at: forall 1 . { "list": (List 0), "index": Num } -> [ "Some": 0, "None": Unit ],

    list_prepend: forall 1 . { "head": 0, "tail": (List 0) } -> (List 0),
    list_append: forall 1 . { "head": (List 0), "tail": 0 } -> (List 0),
    list_concat: forall 1 . { "head": (List 0), "tail": (List 0) } -> (List 0),

    list_rec: forall 2 . { "list": (List 0), "base": 1, "step": ({ "acc": 1, "head": 0 } -> 1) } -> 1,

    // Builtin Number functions
    num_abs: forall 0 . Num -> Num,
    num_sqrt: forall 0 . Num -> Num,
    num_floor: forall 0 . Num -> Num,
    num_ceil: forall 0 . Num -> Num,
    num_round: forall 0 . Num -> Num,
    num_sin: forall 0 . Num -> Num,
    num_cos: forall 0 . Num -> Num,
    num_tan: forall 0 . Num -> Num,
    num_ln: forall 0 . Num -> Num,
    num_log10: forall 0 . Num -> Num,
    num_exp: forall 0 . Num -> Num,
    num_pow: forall 0 . { "base": Num, "exp": Num } -> Num,

    // Builtin String functions
    str_length: forall 0 . Str -> Num,
    str_is_empty: forall 0 . Str -> Bool,
    str_reverse: forall 0 . Str -> Str,
    str_first: forall 0 . Str -> [ "Some": Char, "None": Unit ],
    str_last: forall 0 . Str -> [ "Some": Char, "None": Unit ],
}
