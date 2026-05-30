use std::{fmt, sync::LazyLock};

use kola_protocol::{TypeProtocol, TypeSchemeProtocol, ty};
use strum::{AsRefStr, EnumCount, EnumIter, EnumString, FromRepr, IntoStaticStr};

/// BuiltinType - the simple, closed set of primitive type constructors
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, strum::Display, EnumString)]
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
    #[inline]
    pub fn from_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }
}

#[inline]
pub fn is_builtin_type(s: &str) -> bool {
    s.parse::<BuiltinType>().is_ok()
}

pub static BUILTINS: LazyLock<[Builtin; BuiltinId::COUNT]> = LazyLock::new(|| {
    std::array::from_fn(|i| {
        let id = BuiltinId::from_repr(i).expect("valid BuiltinId discriminant");
        id.definition()
    })
});

#[inline]
pub fn is_builtin(s: &str) -> bool {
    s.parse::<BuiltinId>().is_ok()
}

#[inline]
pub fn find_builtin(s: &str) -> Option<&'static Builtin> {
    let id = s.parse::<BuiltinId>().ok()?;
    Some(Builtin::from_id(id))
}

#[inline]
pub fn find_builtin_id(s: &str) -> Option<BuiltinId> {
    s.parse().ok()
}

/// Builtin - a single builtin function signature
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Builtin {
    pub name: &'static str,
    pub forall: u32,
    pub input: TypeProtocol,
    pub output: TypeProtocol,
}

impl Builtin {
    #[inline]
    pub fn from_name(name: &'static str) -> Option<&'static Self> {
        find_builtin(name)
    }

    #[inline]
    pub fn from_id(id: BuiltinId) -> &'static Self {
        &BUILTINS[id as usize]
    }

    pub fn type_scheme(&self) -> TypeSchemeProtocol {
        TypeSchemeProtocol::new(
            self.forall,
            TypeProtocol::func(self.input.clone(), self.output.clone()),
        )
    }
}

/// BuiltinId - one discriminant per builtin function
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumCount,
    FromRepr,
    EnumIter,
    AsRefStr,
    IntoStaticStr,
    EnumString,
)]
#[strum(serialize_all = "snake_case")]
#[repr(usize)]
pub enum BuiltinId {
    IoDebug,
    IoReadFile,
    IoWriteFile,
    ListLength,
    ListIsEmpty,
    ListReverse,
    ListSum,
    ListFirst,
    ListLast,
    ListContains,
    ListAt,
    ListPrepend,
    ListAppend,
    ListConcat,
    ListRec,
    NumAbs,
    NumSqrt,
    NumFloor,
    NumCeil,
    NumRound,
    NumSin,
    NumCos,
    NumTan,
    NumLn,
    NumLog10,
    NumExp,
    NumPow,
    NumRec,
    RecordSelect,
    RecordInsert,
    RecordRemove,
    RecordRename,
    RecordContains,
    RecordKeys,
    RecordSize,
    RecordMergeLeft,
    RecordMergeRight,
    RecordRec,
    SerdeFromJson,
    SerdeToJson,
    StrLength,
    StrIsEmpty,
    StrReverse,
    StrFirst,
    StrLast,
    StrContains,
    StrAt,
    StrPrepend,
    StrAppend,
    StrConcat,
    StrRec,
}

impl fmt::Display for BuiltinId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "__builtin_{}", self.as_ref())
    }
}

impl BuiltinId {
    fn definition(&self) -> Builtin {
        let b = |forall, input, output| Builtin {
            name: self.into(),
            forall,
            input,
            output,
        };

        match self {
            // ---- Io ----
            Self::IoDebug => b(1, ty!(0), ty!(0)),
            Self::IoReadFile => b(0, ty!(Str), ty!([ "Ok": Str, "Err": Str ])),
            Self::IoWriteFile => b(
                0,
                ty!({ "path": Str, "contents": Str }),
                ty!([ "Ok": Unit, "Err": Str ]),
            ),

            // ---- List ----
            Self::ListLength => b(1, ty!((List 0)), ty!(Num)),
            Self::ListIsEmpty => b(1, ty!((List 0)), ty!(Bool)),
            Self::ListReverse => b(1, ty!((List 0)), ty!((List 0))),
            Self::ListSum => b(0, ty!((List Num)), ty!(Num)),
            Self::ListFirst => b(1, ty!((List 0)), ty!([ "Some": 0, "None": Unit ])),
            Self::ListLast => b(1, ty!((List 0)), ty!([ "Some": 0, "None": Unit ])),
            Self::ListContains => b(1, ty!({ "list": (List 0), "value": 0 }), ty!(Bool)),
            Self::ListAt => b(
                1,
                ty!({ "list": (List 0), "index": Num }),
                ty!([ "Some": 0, "None": Unit ]),
            ),
            Self::ListPrepend => b(1, ty!({ "head": 0, "tail": (List 0) }), ty!((List 0))),
            Self::ListAppend => b(1, ty!({ "head": (List 0), "tail": 0 }), ty!((List 0))),
            Self::ListConcat => b(
                1,
                ty!({ "head": (List 0), "tail": (List 0) }),
                ty!((List 0)),
            ),
            Self::ListRec => b(
                2,
                ty!({ "list": (List 0), "base": 1, "step": ({ "acc": 1, "head": 0 } -> 1) }),
                ty!(1),
            ),

            // ---- Num ----
            Self::NumAbs => b(0, ty!(Num), ty!(Num)),
            Self::NumSqrt => b(0, ty!(Num), ty!(Num)),
            Self::NumFloor => b(0, ty!(Num), ty!(Num)),
            Self::NumCeil => b(0, ty!(Num), ty!(Num)),
            Self::NumRound => b(0, ty!(Num), ty!(Num)),
            Self::NumSin => b(0, ty!(Num), ty!(Num)),
            Self::NumCos => b(0, ty!(Num), ty!(Num)),
            Self::NumTan => b(0, ty!(Num), ty!(Num)),
            Self::NumLn => b(0, ty!(Num), ty!(Num)),
            Self::NumLog10 => b(0, ty!(Num), ty!(Num)),
            Self::NumExp => b(0, ty!(Num), ty!(Num)),
            Self::NumPow => b(0, ty!({ "base": Num, "exp": Num }), ty!(Num)),
            Self::NumRec => b(
                1,
                ty!({ "num": Num, "base": 0, "step": ({ "acc": 0, "head": Num } -> 0) }),
                ty!(0),
            ),

            // ---- Record ----
            Self::RecordSelect => b(3, ty!({ "label": (LabelWit 0), "record": 1 }), ty!(2)),
            Self::RecordInsert => b(
                4,
                ty!({ "label": (LabelWit 0), "value": 1, "record": 2 }),
                ty!(3),
            ),
            Self::RecordRemove => b(3, ty!({ "label": (LabelWit 0), "record": 1 }), ty!(2)),
            Self::RecordRename => b(
                4,
                ty!({ "from": (LabelWit 0), "to": (LabelWit 1), "record": 2 }),
                ty!(3),
            ),
            Self::RecordContains => b(2, ty!({ "label": (LabelWit 0), "record": 1 }), ty!(Bool)),
            Self::RecordKeys => b(1, ty!(0), ty!((List Str))),
            Self::RecordSize => b(1, ty!(0), ty!(Num)),
            Self::RecordMergeLeft => b(3, ty!({ "left": 0, "right": 1 }), ty!(2)),
            Self::RecordMergeRight => b(3, ty!({ "left": 0, "right": 1 }), ty!(2)),
            Self::RecordRec => b(
                3,
                ty!({ "record": 0, "base": 1, "step": ({ "acc": 1, "head": { "key": Str, "value": 2 } } -> 1) }),
                ty!(1),
            ),

            // ---- Serde ----
            Self::SerdeFromJson => b(
                1,
                ty!({ "proto": (TypeWit 0), "json": Str }),
                ty!([ "Ok": 0, "Err": Str ]),
            ),
            Self::SerdeToJson => b(1, ty!(0), ty!([ "Ok": Str, "Err": Str ])),

            // ---- Str ----
            Self::StrLength => b(0, ty!(Str), ty!(Num)),
            Self::StrIsEmpty => b(0, ty!(Str), ty!(Bool)),
            Self::StrReverse => b(0, ty!(Str), ty!(Str)),
            Self::StrFirst => b(0, ty!(Str), ty!([ "Some": Char, "None": Unit ])),
            Self::StrLast => b(0, ty!(Str), ty!([ "Some": Char, "None": Unit ])),
            Self::StrContains => b(0, ty!({ "str": Str, "value": Char }), ty!(Bool)),
            Self::StrAt => b(
                0,
                ty!({ "str": Str, "index": Num }),
                ty!([ "Some": Char, "None": Unit ]),
            ),
            Self::StrPrepend => b(0, ty!({ "head": Char, "tail": Str }), ty!(Str)),
            Self::StrAppend => b(0, ty!({ "head": Str, "tail": Char }), ty!(Str)),
            Self::StrConcat => b(0, ty!({ "head": Str, "tail": Str }), ty!(Str)),
            Self::StrRec => b(
                1,
                ty!({ "str": Str, "base": 0, "step": ({ "acc": 0, "head": Char } -> 0) }),
                ty!(0),
            ),
        }
    }
}
