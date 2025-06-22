use kola_utils::interner::StrKey;

use super::{Expr, Symbol};
use crate::{id::Id, ir::IrBuilder};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IsUnit {
    pub source: Symbol,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl IsUnit {
    pub fn new(
        source: Symbol,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IsBool {
    pub source: Symbol,
    pub payload: bool,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl IsBool {
    pub fn new(
        source: Symbol,
        payload: bool,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            payload,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct IsNum {
    pub source: Symbol,
    pub payload: f64,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl IsNum {
    pub fn new(
        source: Symbol,
        payload: f64,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            payload,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IsChar {
    pub source: Symbol,
    pub payload: char,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl IsChar {
    pub fn new(
        source: Symbol,
        payload: char,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            payload,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IsStr {
    pub source: Symbol,
    pub payload: StrKey,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl IsStr {
    pub fn new(
        source: Symbol,
        payload: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            payload,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IsVariant {
    pub source: Symbol,
    pub tag: StrKey,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl IsVariant {
    pub fn new(
        source: Symbol,
        tag: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            tag,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IsTag {
    pub source: Symbol,
    pub payload: StrKey,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl IsTag {
    pub fn new(
        source: Symbol,
        payload: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            payload,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IsList {
    pub source: Symbol,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl IsList {
    pub fn new(
        source: Symbol,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ListIsExact {
    pub source: Symbol,
    pub length: u32,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl ListIsExact {
    pub fn new(
        source: Symbol,
        length: u32,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            length,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ListIsAtLeast {
    pub source: Symbol,
    pub min_length: u32,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl ListIsAtLeast {
    pub fn new(
        source: Symbol,
        min_length: u32,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            min_length,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IsRecord {
    pub source: Symbol,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl IsRecord {
    pub fn new(
        source: Symbol,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            on_success,
            on_failure,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordHasField {
    pub source: Symbol,
    pub field: StrKey,
    pub on_success: Id<PatternMatcher>,
    pub on_failure: Id<PatternMatcher>,
}

impl RecordHasField {
    pub fn new(
        source: Symbol,
        field: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let on_success = builder.add(on_success.into());
        let on_failure = builder.add(on_failure.into());
        Self {
            source,
            field,
            on_success,
            on_failure,
        }
    }
}

// === EXTRACTOR STRUCTS ===

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractIdentity {
    pub bind: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

impl ExtractIdentity {
    pub fn new(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self { bind, source, next }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractListHead {
    pub bind: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

impl ExtractListHead {
    pub fn new(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self { bind, source, next }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractListTail {
    pub bind: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

impl ExtractListTail {
    pub fn new(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self { bind, source, next }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractListAt {
    pub bind: Symbol,
    pub source: Symbol,
    pub index: u32,
    pub next: Id<PatternMatcher>,
}

impl ExtractListAt {
    pub fn new(
        bind: Symbol,
        source: Symbol,
        index: u32,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self {
            bind,
            source,
            index,
            next,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractListSliceFrom {
    pub bind: Symbol,
    pub source: Symbol,
    pub start_index: u32,
    pub next: Id<PatternMatcher>,
}

impl ExtractListSliceFrom {
    pub fn new(
        bind: Symbol,
        source: Symbol,
        start_index: u32,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self {
            bind,
            source,
            start_index,
            next,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractRecordField {
    pub bind: Symbol,
    pub source: Symbol,
    pub field: StrKey,
    pub next: Id<PatternMatcher>,
}

impl ExtractRecordField {
    pub fn new(
        bind: Symbol,
        source: Symbol,
        field: StrKey,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self {
            bind,
            source,
            field,
            next,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractRecordWithoutField {
    pub bind: Symbol,
    pub source: Symbol,
    pub field: StrKey,
    pub next: Id<PatternMatcher>,
}

impl ExtractRecordWithoutField {
    pub fn new(
        bind: Symbol,
        source: Symbol,
        field: StrKey,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self {
            bind,
            source,
            field,
            next,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractVariantTag {
    pub bind: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

impl ExtractVariantTag {
    pub fn new(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self { bind, source, next }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractVariantValue {
    pub bind: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

impl ExtractVariantValue {
    pub fn new(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self { bind, source, next }
    }
}

// === CONTROL FLOW STRUCTS ===

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PatternSuccess {
    pub next: Id<Expr>,
}

impl PatternSuccess {
    pub fn new(next: Id<Expr>) -> Self {
        Self { next }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PatternFailure;

// === MAIN PATTERN MATCHER ENUM ===

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum PatternMatcher {
    // Tests
    IsUnit(IsUnit),
    IsBool(IsBool),
    IsNum(IsNum),
    IsChar(IsChar),
    IsStr(IsStr),
    IsTag(IsTag),
    IsVariant(IsVariant),
    IsList(IsList),
    ListIsExact(ListIsExact),
    ListIsAtLeast(ListIsAtLeast),
    IsRecord(IsRecord),
    RecordHasField(RecordHasField),

    // Extractors
    ExtractIdentity(ExtractIdentity),
    ExtractListHead(ExtractListHead),
    ExtractListTail(ExtractListTail),
    ExtractListAt(ExtractListAt),
    ExtractListSliceFrom(ExtractListSliceFrom),
    ExtractRecordField(ExtractRecordField),
    ExtractRecordWithoutField(ExtractRecordWithoutField),
    ExtractVariantTag(ExtractVariantTag),
    ExtractVariantValue(ExtractVariantValue),

    // Control flow
    Success(PatternSuccess),
    Failure(PatternFailure),
}

impl PatternMatcher {
    // Test constructors
    pub fn is_unit(
        source: Symbol,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = IsUnit::new(source, on_success, on_failure, builder);
        builder.add(Self::IsUnit(matcher))
    }

    pub fn is_bool(
        source: Symbol,
        payload: bool,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = IsBool::new(source, payload, on_success, on_failure, builder);
        builder.add(Self::IsBool(matcher))
    }

    pub fn is_num(
        source: Symbol,
        payload: f64,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = IsNum::new(source, payload, on_success, on_failure, builder);
        builder.add(Self::IsNum(matcher))
    }

    pub fn is_char(
        source: Symbol,
        payload: char,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = IsChar::new(source, payload, on_success, on_failure, builder);
        builder.add(Self::IsChar(matcher))
    }

    pub fn is_str(
        source: Symbol,
        payload: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = IsStr::new(source, payload, on_success, on_failure, builder);
        builder.add(Self::IsStr(matcher))
    }

    pub fn is_tag(
        source: Symbol,
        payload: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = IsTag::new(source, payload, on_success, on_failure, builder);
        builder.add(Self::IsTag(matcher))
    }

    pub fn is_variant(
        source: Symbol,
        tag: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = IsVariant::new(source, tag, on_success, on_failure, builder);
        builder.add(Self::IsVariant(matcher))
    }

    pub fn is_list(
        source: Symbol,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = IsList::new(source, on_success, on_failure, builder);
        builder.add(Self::IsList(matcher))
    }

    pub fn list_is_exact(
        source: Symbol,
        length: u32,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = ListIsExact::new(source, length, on_success, on_failure, builder);
        builder.add(Self::ListIsExact(matcher))
    }

    pub fn list_is_at_least(
        source: Symbol,
        min_length: u32,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = ListIsAtLeast::new(source, min_length, on_success, on_failure, builder);
        builder.add(Self::ListIsAtLeast(matcher))
    }

    pub fn is_record(
        source: Symbol,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = IsRecord::new(source, on_success, on_failure, builder);
        builder.add(Self::IsRecord(matcher))
    }

    pub fn record_has_field(
        source: Symbol,
        field: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let matcher = RecordHasField::new(source, field, on_success, on_failure, builder);
        builder.add(Self::RecordHasField(matcher))
    }

    // Extractor constructors
    pub fn extract_identity(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let extractor = ExtractIdentity::new(bind, source, next, builder);
        builder.add(Self::ExtractIdentity(extractor))
    }

    pub fn extract_list_head(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let extractor = ExtractListHead::new(bind, source, next, builder);
        builder.add(Self::ExtractListHead(extractor))
    }

    pub fn extract_list_tail(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let extractor = ExtractListTail::new(bind, source, next, builder);
        builder.add(Self::ExtractListTail(extractor))
    }

    pub fn extract_list_at(
        bind: Symbol,
        source: Symbol,
        index: u32,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let extractor = ExtractListAt::new(bind, source, index, next, builder);
        builder.add(Self::ExtractListAt(extractor))
    }

    pub fn extract_list_slice_from(
        bind: Symbol,
        source: Symbol,
        start_index: u32,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let extractor = ExtractListSliceFrom::new(bind, source, start_index, next, builder);
        builder.add(Self::ExtractListSliceFrom(extractor))
    }

    pub fn extract_record_field(
        bind: Symbol,
        source: Symbol,
        field: StrKey,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let extractor = ExtractRecordField::new(bind, source, field, next, builder);
        builder.add(Self::ExtractRecordField(extractor))
    }

    pub fn extract_record_without_field(
        bind: Symbol,
        source: Symbol,
        field: StrKey,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let extractor = ExtractRecordWithoutField::new(bind, source, field, next, builder);
        builder.add(Self::ExtractRecordWithoutField(extractor))
    }

    pub fn extract_variant_tag(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let extractor = ExtractVariantTag::new(bind, source, next, builder);
        builder.add(Self::ExtractVariantTag(extractor))
    }

    pub fn extract_variant_value(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Id<Self> {
        let extractor = ExtractVariantValue::new(bind, source, next, builder);
        builder.add(Self::ExtractVariantValue(extractor))
    }

    // Control flow constructors
    pub fn success(next: Id<Expr>, builder: &mut IrBuilder) -> Id<Self> {
        let success = PatternSuccess::new(next);
        builder.add(Self::Success(success))
    }

    pub fn failure(builder: &mut IrBuilder) -> Id<Self> {
        builder.add(Self::Failure(PatternFailure))
    }
}
