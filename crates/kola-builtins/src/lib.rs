use derive_more::{Display, FromStr};
use std::{cell::LazyCell, fmt};

use kola_protocol::{KindProtocol, TypeProtocol, TypeSchemeProtocol};

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
    Type,
    Label,
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
            "Type" => Some(Self::Type),
            "Label" => Some(Self::Label),
            _ => None,
        }
    }
}

#[inline]
pub fn is_builtin_type(s: &str) -> bool {
    matches!(
        s,
        "Unit" | "Bool" | "Num" | "Char" | "Str" | "List" | "Type" | "Label"
    )
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Builtin {
    pub name: &'static str,
    pub forall: u32,
    pub input: TypeProtocol,
    pub output: TypeProtocol,
}

impl Builtin {
    #[inline]
    pub fn from_name(name: &'static str) -> Option<Self> {
        find_builtin(name)
    }

    pub fn type_scheme(self) -> TypeSchemeProtocol {
        let Self {
            forall,
            input,
            output,
            ..
        } = self;

        TypeSchemeProtocol::new(forall, TypeProtocol::func(input, output))
    }

    #[inline]
    pub fn from_id(id: BuiltinId) -> Self {
        BUILTINS[id as usize].clone()
    }
}

#[inline]
pub fn is_builtin(s: &str) -> bool {
    BUILTINS.iter().any(|builtin| builtin.name == s)
}

#[inline]
pub fn find_builtin(s: &str) -> Option<Builtin> {
    BUILTINS.iter().find(|builtin| builtin.name == s).cloned()
}

#[inline]
pub fn find_builtin_id(s: &str) -> Option<BuiltinId> {
    BUILTINS
        .iter()
        .position(|builtin| builtin.name == s)
        .map(|id| BuiltinId::from_usize(id))
}

macro_rules! define_builtins {
    (
        $(
            $name:ident : forall $forall:literal . $input:tt -> $output:tt
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
        const BUILTINS: LazyCell<Vec<Builtin>> = LazyCell::new(|| vec![
            $(
                Builtin {
                    name: stringify!($name),
                    forall: $forall,
                    input: define_builtins!(@type $input),
                    output: define_builtins!(@type $output),
                },
            )*
        ]);
    };

    // Type expression parsing
    (@type Any) => { TypeProtocol::Any };
    (@type Unit) => { TypeProtocol::Unit };
    (@type Bool) => { TypeProtocol::Bool };
    (@type Num) => { TypeProtocol::Num };
    (@type Char) => { TypeProtocol::Char };
    (@type Str) => { TypeProtocol::Str };
    (@type $var:literal) => { TypeProtocol::Var($var, KindProtocol::Type) };
    (@type (List $inner:tt)) => {
        TypeProtocol::List(Box::new(define_builtins!(@type $inner)))
    };
    (@type (Type $inner:literal)) => {
        TypeProtocol::Witness(Box::new(TypeProtocol::Var($inner, KindProtocol::Type)))
    };
    (@type (Label $inner:literal)) => {
        TypeProtocol::Witness(Box::new(TypeProtocol::Var($inner, KindProtocol::Label)))
    };
    (@type { $($field:literal : $field_type:tt),* $(,)? }) => {
        TypeProtocol::Record(vec![
            $((String::from($field), define_builtins!(@type $field_type))),*
        ])
    };
    (@type [ $($variant:literal : $variant_type:tt),* $(,)? ] ) => {
        TypeProtocol::Variant(vec![
            $((String::from($variant), define_builtins!(@type $variant_type))),*
        ])
    };
    (@type ($left:tt -> $right:tt)) => {
        TypeProtocol::Func(
            Box::new(define_builtins!(@type $left)),
            Box::new(define_builtins!(@type $right))
        )
    };
}

define_builtins! {
    // Builtin Io functions
    io_debug: forall 1 . 0 -> 0,
    io_read_file: forall 0 . Str -> [ "Ok": Str, "Err": Str ],
    io_write_file: forall 0 . { "path": Str, "contents": Str } -> [ "Ok": Unit, "Err": Str ],

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
    num_rec: forall 1 . { "num": Num, "base": 0, "step": ({ "acc": 0, "head": Num } -> 0) } -> 0,

    // Builtin Record functions
    record_select: forall 3 . { "label": (Label 0), "record": 1 } -> 2,
    record_rec: forall 3 . { "record": 0, "base": 1, "step": ({ "acc": 1, "head": { "key": Str, "value": 2 } } -> 1) } -> 1,

    // Builtin Serde functions
    serde_from_json: forall 1 . { "proto": (Type 0), "json": Str } -> [ "Ok": 0, "Err": Str ],
    serde_to_json: forall 1 . 0 -> [ "Ok": Str, "Err": Str ],

    // Builtin String functions
    str_length: forall 0 . Str -> Num,
    str_is_empty: forall 0 . Str -> Bool,
    str_reverse: forall 0 . Str -> Str,
    str_first: forall 0 . Str -> [ "Some": Char, "None": Unit ],
    str_last: forall 0 . Str -> [ "Some": Char, "None": Unit ],
    str_contains: forall 0 . { "str": Str, "value": Char } -> Bool,
    str_at: forall 0 . { "str": Str, "index": Num } -> [ "Some": Char, "None": Unit ],
    str_prepend: forall 0 . { "head": Char, "tail": Str } -> Str,
    str_append: forall 0 . { "head": Str, "tail": Char } -> Str,
    str_concat: forall 0 . { "head": Str, "tail": Str } -> Str,
    str_rec: forall 1 . { "str": Str, "base": 0, "step": ({ "acc": 0, "head": Char } -> 0) } -> 0,
}
