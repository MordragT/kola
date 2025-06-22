use kola_print::prelude::*;
use kola_utils::interner::StrKey;

use super::{Expr, Symbol};
use crate::{
    id::Id,
    ir::{IrBuilder, IrView},
    print::IrPrinter,
};

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

// Unit -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, IsUnit> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IsUnit {
            on_success,
            on_failure,
            ..
        } = self.node;

        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [arena.notate("Unit -> "), success, failure].concat_in(arena)
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

// Bool <payload> -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, IsBool> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IsBool {
            payload,
            on_success,
            on_failure,
            ..
        } = self.node;

        let payload = payload.display_in(arena);
        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [
            arena.notate("Bool "),
            payload,
            arena.notate(" -> "),
            success,
            failure,
        ]
        .concat_in(arena)
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

// Num <payload> -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, IsNum> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IsNum {
            payload,
            on_success,
            on_failure,
            ..
        } = self.node;

        let payload = payload.display_in(arena);
        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [
            arena.notate("Num "),
            payload,
            arena.notate(" -> "),
            success,
            failure,
        ]
        .concat_in(arena)
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

// Char <payload> -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, IsChar> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IsChar {
            payload,
            on_success,
            on_failure,
            ..
        } = self.node;

        let payload = payload.display_in(arena);
        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [
            arena.notate("Char "),
            payload,
            arena.notate(" -> "),
            success,
            failure,
        ]
        .concat_in(arena)
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

// Str <payload> -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, IsStr> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IsStr {
            payload,
            on_success,
            on_failure,
            ..
        } = self.node;

        let payload = payload.display_in(arena);
        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [
            arena.notate("Str "),
            payload,
            arena.notate(" -> "),
            success,
            failure,
        ]
        .concat_in(arena)
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

// Variant <tag> -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, IsVariant> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IsVariant {
            tag,
            on_success,
            on_failure,
            ..
        } = self.node;

        let tag = tag.display_in(arena);
        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [
            arena.notate("Variant "),
            tag,
            arena.notate(" -> "),
            success,
            failure,
        ]
        .concat_in(arena)
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

// Tag <payload> -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, IsTag> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IsTag {
            payload,
            on_success,
            on_failure,
            ..
        } = self.node;

        let payload = payload.display_in(arena);
        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [
            arena.notate("Tag "),
            payload,
            arena.notate(" -> "),
            success,
            failure,
        ]
        .concat_in(arena)
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

// List -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, IsList> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IsList {
            on_success,
            on_failure,
            ..
        } = self.node;

        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [arena.notate("List -> "), success, failure].concat_in(arena)
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

// ListExact <length> -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, ListIsExact> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ListIsExact {
            length,
            on_success,
            on_failure,
            ..
        } = self.node;

        let length = length.display_in(arena);
        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [
            arena.notate("ListExact "),
            length,
            arena.notate(" -> "),
            success,
            failure,
        ]
        .concat_in(arena)
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

// ListAtLeast <min_length> -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, ListIsAtLeast> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ListIsAtLeast {
            min_length,
            on_success,
            on_failure,
            ..
        } = self.node;

        let min_length = min_length.display_in(arena);
        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [
            arena.notate("ListAtLeast "),
            min_length,
            arena.notate(" -> "),
            success,
            failure,
        ]
        .concat_in(arena)
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

// Record -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, IsRecord> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let IsRecord {
            on_success,
            on_failure,
            ..
        } = self.node;

        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [arena.notate("Record -> "), success, failure].concat_in(arena)
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

// RecordField <field> -> <on_success>
impl<'a> Notate<'a> for IrPrinter<'a, RecordHasField> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let RecordHasField {
            field,
            on_success,
            on_failure,
            ..
        } = self.node;

        let field = field.display_in(arena);
        let success = self.to(on_success).notate(arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.to(on_failure).notate(arena),
        ]
        .concat_in(arena);

        [
            arena.notate("RecordField "),
            field,
            arena.notate(" -> "),
            success,
            failure,
        ]
        .concat_in(arena)
    }
}

// === EXTRACTOR STRUCTS ===

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtractIdentity {
    pub bind: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

// <bind> = extract_identity <source> => <next>
// <bind> =
//    extract_identity <source>
//    => <next>
impl<'a> Notate<'a> for IrPrinter<'a, ExtractIdentity> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ExtractIdentity { bind, source, next } = self.node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let next = self.to(next).notate(arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" extract_identity "),
            source.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("extract_identity "),
            source,
            arena.newline(),
            "=> ".display_in(arena),
            next,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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

// <bind> = extract_list_head <source> => <next>
// <bind> =
//    extract_list_head <source>
//    => <next>
impl<'a> Notate<'a> for IrPrinter<'a, ExtractListHead> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ExtractListHead { bind, source, next } = self.node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let next = self.to(next).notate(arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" extract_list_head "),
            source.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("extract_list_head "),
            source,
            arena.newline(),
            "=> ".display_in(arena),
            next,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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

// <bind> = extract_list_tail <source> => <next>
// <bind> =
//    extract_list_tail <source>
//    => <next>
impl<'a> Notate<'a> for IrPrinter<'a, ExtractListTail> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ExtractListTail { bind, source, next } = self.node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let next = self.to(next).notate(arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" extract_list_tail "),
            source.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("extract_list_tail "),
            source,
            arena.newline(),
            "=> ".display_in(arena),
            next,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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

// <bind> = extract_list_at <source> <index> => <next>
// <bind> =
//    extract_list_at <source> <index>
//    => <next>
impl<'a> Notate<'a> for IrPrinter<'a, ExtractListAt> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ExtractListAt {
            bind,
            source,
            index,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let index = index.display_in(arena);
        let next = self.to(next).notate(arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" extract_list_at "),
            source.clone(),
            arena.notate(" "),
            index.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("extract_list_at "),
            source,
            arena.notate(" "),
            index,
            arena.newline(),
            "=> ".display_in(arena),
            next,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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

// <bind> = extract_list_slice_from <source> <start_index> => <next>
// <bind> =
//    extract_list_slice_from <source> <start_index>
//    => <next>
impl<'a> Notate<'a> for IrPrinter<'a, ExtractListSliceFrom> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ExtractListSliceFrom {
            bind,
            source,
            start_index,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let start_index = start_index.display_in(arena);
        let next = self.to(next).notate(arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" extract_list_slice_from "),
            source.clone(),
            arena.notate(" "),
            start_index.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("extract_list_slice_from "),
            source,
            arena.notate(" "),
            start_index,
            arena.newline(),
            "=> ".display_in(arena),
            next,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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

// <bind> = extract_record_field <source> <field> => <next>
// <bind> =
//    extract_record_field <source> <field>
//    => <next>
impl<'a> Notate<'a> for IrPrinter<'a, ExtractRecordField> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ExtractRecordField {
            bind,
            source,
            field,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let field = field.display_in(arena);
        let next = self.to(next).notate(arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" extract_record_field "),
            source.clone(),
            arena.notate(" "),
            field.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("extract_record_field "),
            source,
            arena.notate(" "),
            field,
            arena.newline(),
            "=> ".display_in(arena),
            next,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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

// <bind> = extract_record_without_field <source> <field> => <next>
// <bind> =
//    extract_record_without_field <source> <field>
//    => <next>
impl<'a> Notate<'a> for IrPrinter<'a, ExtractRecordWithoutField> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ExtractRecordWithoutField {
            bind,
            source,
            field,
            next,
        } = self.node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let field = field.display_in(arena);
        let next = self.to(next).notate(arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" extract_record_without_field "),
            source.clone(),
            arena.notate(" "),
            field.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("extract_record_without_field "),
            source,
            arena.notate(" "),
            field,
            arena.newline(),
            "=> ".display_in(arena),
            next,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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

// <bind> = extract_variant_tag <source> => <next>
// <bind> =
//    extract_variant_tag <source>
//    => <next>
impl<'a> Notate<'a> for IrPrinter<'a, ExtractVariantTag> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ExtractVariantTag { bind, source, next } = self.node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let next = self.to(next).notate(arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" extract_variant_tag "),
            source.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("extract_variant_tag "),
            source,
            arena.newline(),
            "=> ".display_in(arena),
            next,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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

// <bind> = extract_variant_value <source> => <next>
// <bind> =
//    extract_variant_value <source>
//    => <next>
impl<'a> Notate<'a> for IrPrinter<'a, ExtractVariantValue> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        let ExtractVariantValue { bind, source, next } = self.node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let next = self.to(next).notate(arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" extract_variant_value "),
            source.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("extract_variant_value "),
            source,
            arena.newline(),
            "=> ".display_in(arena),
            next,
        ]
        .concat_in(arena)
        .indent(arena);

        head.then(single.or(multi, arena), arena)
    }
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

impl<'a> Notate<'a> for IrPrinter<'a, PatternSuccess> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        arena.just('@').then(
            self.labels[self.node.next.as_usize()].display_in(arena),
            arena,
        )
    }
}

impl PatternSuccess {
    pub fn new(next: Id<Expr>) -> Self {
        Self { next }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PatternFailure;

impl<'a> Notate<'a> for IrPrinter<'a, PatternFailure> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        arena.notate("failure")
    }
}

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

impl<'a> Notate<'a> for IrPrinter<'a, Id<PatternMatcher>> {
    fn notate(&self, arena: &'a Bump) -> Notation<'a> {
        if self.is_node_shared() {
            return self.node_label().yellow().display_in(arena);
        }

        match self.node.get(self.ir) {
            PatternMatcher::IsUnit(tester) => self.to(tester).notate(arena),
            PatternMatcher::IsBool(tester) => self.to(tester).notate(arena),
            PatternMatcher::IsNum(tester) => self.to(tester).notate(arena),
            PatternMatcher::IsChar(tester) => self.to(tester).notate(arena),
            PatternMatcher::IsStr(tester) => self.to(tester).notate(arena),
            PatternMatcher::IsTag(tester) => self.to(tester).notate(arena),
            PatternMatcher::IsVariant(tester) => self.to(tester).notate(arena),
            PatternMatcher::IsList(tester) => self.to(tester).notate(arena),
            PatternMatcher::ListIsExact(tester) => self.to(tester).notate(arena),
            PatternMatcher::ListIsAtLeast(tester) => self.to(tester).notate(arena),
            PatternMatcher::IsRecord(tester) => self.to(tester).notate(arena),
            PatternMatcher::RecordHasField(tester) => self.to(tester).notate(arena),
            PatternMatcher::ExtractIdentity(extractor) => self.to(extractor).notate(arena),
            PatternMatcher::ExtractListHead(extractor) => self.to(extractor).notate(arena),
            PatternMatcher::ExtractListTail(extractor) => self.to(extractor).notate(arena),
            PatternMatcher::ExtractListAt(extractor) => self.to(extractor).notate(arena),
            PatternMatcher::ExtractListSliceFrom(extractor) => self.to(extractor).notate(arena),
            PatternMatcher::ExtractRecordField(extractor) => self.to(extractor).notate(arena),
            PatternMatcher::ExtractRecordWithoutField(extractor) => {
                self.to(extractor).notate(arena)
            }
            PatternMatcher::ExtractVariantTag(extractor) => self.to(extractor).notate(arena),
            PatternMatcher::ExtractVariantValue(extractor) => self.to(extractor).notate(arena),
            PatternMatcher::Success(success) => self.to(success).notate(arena),
            PatternMatcher::Failure(failure) => self.to(failure).notate(arena),
        }
    }
}
impl PatternMatcher {
    // Test constructors
    pub fn is_unit(
        source: Symbol,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = IsUnit::new(source, on_success, on_failure, builder);
        Self::IsUnit(matcher)
    }

    pub fn is_bool(
        source: Symbol,
        payload: bool,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = IsBool::new(source, payload, on_success, on_failure, builder);
        Self::IsBool(matcher)
    }

    pub fn is_num(
        source: Symbol,
        payload: f64,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = IsNum::new(source, payload, on_success, on_failure, builder);
        Self::IsNum(matcher)
    }

    pub fn is_char(
        source: Symbol,
        payload: char,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = IsChar::new(source, payload, on_success, on_failure, builder);
        Self::IsChar(matcher)
    }

    pub fn is_str(
        source: Symbol,
        payload: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = IsStr::new(source, payload, on_success, on_failure, builder);
        Self::IsStr(matcher)
    }

    pub fn is_tag(
        source: Symbol,
        payload: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = IsTag::new(source, payload, on_success, on_failure, builder);
        Self::IsTag(matcher)
    }

    pub fn is_variant(
        source: Symbol,
        tag: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = IsVariant::new(source, tag, on_success, on_failure, builder);
        Self::IsVariant(matcher)
    }

    pub fn is_list(
        source: Symbol,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = IsList::new(source, on_success, on_failure, builder);
        Self::IsList(matcher)
    }

    pub fn list_is_exact(
        source: Symbol,
        length: u32,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = ListIsExact::new(source, length, on_success, on_failure, builder);
        Self::ListIsExact(matcher)
    }

    pub fn list_is_at_least(
        source: Symbol,
        min_length: u32,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = ListIsAtLeast::new(source, min_length, on_success, on_failure, builder);
        Self::ListIsAtLeast(matcher)
    }

    pub fn is_record(
        source: Symbol,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = IsRecord::new(source, on_success, on_failure, builder);
        Self::IsRecord(matcher)
    }

    pub fn record_has_field(
        source: Symbol,
        field: StrKey,
        on_success: impl Into<PatternMatcher>,
        on_failure: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let matcher = RecordHasField::new(source, field, on_success, on_failure, builder);
        Self::RecordHasField(matcher)
    }

    // Extractor constructors
    pub fn extract_identity(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ExtractIdentity::new(bind, source, next, builder);
        Self::ExtractIdentity(extractor)
    }

    pub fn extract_list_head(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ExtractListHead::new(bind, source, next, builder);
        Self::ExtractListHead(extractor)
    }

    pub fn extract_list_tail(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ExtractListTail::new(bind, source, next, builder);
        Self::ExtractListTail(extractor)
    }

    pub fn extract_list_at(
        bind: Symbol,
        source: Symbol,
        index: u32,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ExtractListAt::new(bind, source, index, next, builder);
        Self::ExtractListAt(extractor)
    }

    pub fn extract_list_slice_from(
        bind: Symbol,
        source: Symbol,
        start_index: u32,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ExtractListSliceFrom::new(bind, source, start_index, next, builder);
        Self::ExtractListSliceFrom(extractor)
    }

    pub fn extract_record_field(
        bind: Symbol,
        source: Symbol,
        field: StrKey,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ExtractRecordField::new(bind, source, field, next, builder);
        Self::ExtractRecordField(extractor)
    }

    pub fn extract_record_without_field(
        bind: Symbol,
        source: Symbol,
        field: StrKey,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ExtractRecordWithoutField::new(bind, source, field, next, builder);
        Self::ExtractRecordWithoutField(extractor)
    }

    pub fn extract_variant_tag(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ExtractVariantTag::new(bind, source, next, builder);
        Self::ExtractVariantTag(extractor)
    }

    pub fn extract_variant_value(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ExtractVariantValue::new(bind, source, next, builder);
        Self::ExtractVariantValue(extractor)
    }
}
