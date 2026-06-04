use kola_utils::interner::{StrInterner, StrKey};
use std::fmt;
use strum::{
    AsRefStr, Display, EnumCount, EnumIter, EnumString, FromRepr, IntoStaticStr, VariantNames,
};

/// BuiltinType - the simple, closed set of primitive type constructors
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
    Display,
    VariantNames,
)]
#[strum(serialize_all = "PascalCase")]
#[repr(u8)]
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
    Display,
    VariantNames,
)]
#[strum(serialize_all = "PascalCase")]
#[repr(u8)]
pub enum BuiltinTag {
    Ok,
    Err,
    None,
    Some,
}

impl BuiltinTag {
    #[inline]
    pub fn from_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }
}

#[inline]
pub fn is_builtin_tag(s: &str) -> bool {
    s.parse::<BuiltinTag>().is_ok()
}

#[inline]
pub fn find_builtin_tag(s: &str) -> Option<BuiltinTag> {
    s.parse().ok()
}

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
    Display,
    VariantNames,
)]
#[strum(serialize_all = "snake_case")]
#[repr(u8)]
pub enum BuiltinLabel {
    Path,
    Contents,
    List,
    Value,
    Index,
    Head,
    Tail,
    Base,
    Exp,
    Step,
    Acc,
    Num,
    Label,
    Record,
    Field,
    From,
    To,
    Left,
    Right,
    Key,
    Proto,
    Json,
    Str,
}

impl BuiltinLabel {
    #[inline]
    pub fn from_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }
}

#[inline]
pub fn is_builtin_label(s: &str) -> bool {
    s.parse::<BuiltinLabel>().is_ok()
}

#[inline]
pub fn find_builtin_label(s: &str) -> Option<BuiltinLabel> {
    s.parse().ok()
}

#[derive(Debug, Clone)]
pub struct BuiltinLexicon {
    labels: [StrKey; BuiltinLabel::COUNT],
    tags: [StrKey; BuiltinTag::COUNT],
}

impl BuiltinLexicon {
    pub fn new(interner: &mut StrInterner) -> Self {
        let mut labels_iter = BuiltinLabel::VARIANTS.iter().map(|l| interner.intern(*l));

        let labels = std::array::from_fn(|_| labels_iter.next().unwrap());

        let mut tags_iter = BuiltinTag::VARIANTS.iter().map(|t| interner.intern(*t));

        let tags = std::array::from_fn(|_| tags_iter.next().unwrap());

        Self { labels, tags }
    }

    #[inline]
    pub fn label(&self, label: BuiltinLabel) -> StrKey {
        self.labels[label as usize]
    }

    #[inline]
    pub fn tag(&self, tag: BuiltinTag) -> StrKey {
        self.tags[tag as usize]
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
    VariantNames,
)]
#[strum(serialize_all = "snake_case")]
#[repr(u8)]
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

impl BuiltinId {
    #[inline]
    pub fn from_name(name: &str) -> Option<Self> {
        name.parse().ok()
    }
}

impl fmt::Display for BuiltinId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "__builtin_{}", self.as_ref())
    }
}

#[inline]
pub fn is_builtin(s: &str) -> bool {
    s.parse::<BuiltinId>().is_ok()
}

#[inline]
pub fn find_builtin_id(s: &str) -> Option<BuiltinId> {
    s.parse().ok()
}
