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
impl<'a> Notate<'a, IsUnit> for IrPrinter<'a> {
    fn notate(&self, node: &IsUnit, arena: &'a Bump) -> Notation<'a> {
        let IsUnit {
            on_success,
            on_failure,
            ..
        } = node;

        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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
impl<'a> Notate<'a, IsBool> for IrPrinter<'a> {
    fn notate(&self, node: &IsBool, arena: &'a Bump) -> Notation<'a> {
        let IsBool {
            payload,
            on_success,
            on_failure,
            ..
        } = node;

        let payload = payload.display_in(arena);
        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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
impl<'a> Notate<'a, IsNum> for IrPrinter<'a> {
    fn notate(&self, node: &IsNum, arena: &'a Bump) -> Notation<'a> {
        let IsNum {
            payload,
            on_success,
            on_failure,
            ..
        } = node;

        let payload = payload.display_in(arena);
        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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
impl<'a> Notate<'a, IsChar> for IrPrinter<'a> {
    fn notate(&self, node: &IsChar, arena: &'a Bump) -> Notation<'a> {
        let IsChar {
            payload,
            on_success,
            on_failure,
            ..
        } = node;

        let payload = payload.display_in(arena);
        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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
impl<'a> Notate<'a, IsStr> for IrPrinter<'a> {
    fn notate(&self, node: &IsStr, arena: &'a Bump) -> Notation<'a> {
        let IsStr {
            payload,
            on_success,
            on_failure,
            ..
        } = node;

        let payload = payload.display_in(arena);
        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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

// Tag <payload> -> <on_success>
impl<'a> Notate<'a, IsVariant> for IrPrinter<'a> {
    fn notate(&self, node: &IsVariant, arena: &'a Bump) -> Notation<'a> {
        let IsVariant {
            tag,
            on_success,
            on_failure,
            ..
        } = node;

        let tag = self.interner[*tag].display_in(arena);
        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
        ]
        .concat_in(arena);

        [
            arena.notate("Tag "),
            tag,
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
impl<'a> Notate<'a, IsList> for IrPrinter<'a> {
    fn notate(&self, node: &IsList, arena: &'a Bump) -> Notation<'a> {
        let IsList {
            on_success,
            on_failure,
            ..
        } = node;

        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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
impl<'a> Notate<'a, ListIsExact> for IrPrinter<'a> {
    fn notate(&self, node: &ListIsExact, arena: &'a Bump) -> Notation<'a> {
        let ListIsExact {
            length,
            on_success,
            on_failure,
            ..
        } = node;

        let length = length.display_in(arena);
        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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
impl<'a> Notate<'a, ListIsAtLeast> for IrPrinter<'a> {
    fn notate(&self, node: &ListIsAtLeast, arena: &'a Bump) -> Notation<'a> {
        let ListIsAtLeast {
            min_length,
            on_success,
            on_failure,
            ..
        } = node;

        let min_length = min_length.display_in(arena);
        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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
impl<'a> Notate<'a, IsRecord> for IrPrinter<'a> {
    fn notate(&self, node: &IsRecord, arena: &'a Bump) -> Notation<'a> {
        let IsRecord {
            on_success,
            on_failure,
            ..
        } = node;

        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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
impl<'a> Notate<'a, RecordHasField> for IrPrinter<'a> {
    fn notate(&self, node: &RecordHasField, arena: &'a Bump) -> Notation<'a> {
        let RecordHasField {
            field,
            on_success,
            on_failure,
            ..
        } = node;

        let field = field.display_in(arena);
        let success = self.notate(on_success, arena);
        let failure = [
            arena.newline(),
            arena.notate("| "),
            self.notate(on_failure, arena),
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
pub struct Identity {
    pub bind: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

// <bind> = id <source> => <next>
// <bind> =
//    id <source>
//    => <next>
impl<'a> Notate<'a, Identity> for IrPrinter<'a> {
    fn notate(&self, node: &Identity, arena: &'a Bump) -> Notation<'a> {
        let Identity { bind, source, next } = node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let next = self.notate(next, arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" id "),
            source.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("id "),
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

impl Identity {
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
pub struct ListSplitHead {
    pub head: Symbol,
    pub tail_list: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

// <bind> = extract_list_head <source> => <next>
// <bind> =
//    extract_list_head <source>
//    => <next>
impl<'a> Notate<'a, ListSplitHead> for IrPrinter<'a> {
    fn notate(&self, node: &ListSplitHead, arena: &'a Bump) -> Notation<'a> {
        let ListSplitHead {
            head,
            tail_list,
            source,
            next,
        } = node;

        let head = head.display_in(arena);
        let tail_list = tail_list.display_in(arena);
        let source = source.display_in(arena);
        let next = self.notate(next, arena);

        let head = [head, arena.notate(", "), tail_list, arena.notate(" =")].concat_in(arena);

        let single = [
            arena.notate(" split_head "),
            source.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("split_head "),
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

impl ListSplitHead {
    pub fn new(
        head: Symbol,
        tail_list: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self {
            head,
            tail_list,
            source,
            next,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ListSplitTail {
    pub head_list: Symbol,
    pub tail: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

// <bind> = extract_list_tail <source> => <next>
// <bind> =
//    extract_list_tail <source>
//    => <next>
impl<'a> Notate<'a, ListSplitTail> for IrPrinter<'a> {
    fn notate(&self, node: &ListSplitTail, arena: &'a Bump) -> Notation<'a> {
        let ListSplitTail {
            head_list,
            tail,
            source,
            next,
        } = node;

        let head_list = head_list.display_in(arena);
        let tail = tail.display_in(arena);
        let source = source.display_in(arena);
        let next = self.notate(next, arena);

        let head = [head_list, arena.notate(", "), tail, arena.notate(" =")].concat_in(arena);

        let single = [
            arena.notate(" split_tail "),
            source.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("split_tail "),
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

impl ListSplitTail {
    pub fn new(
        head_list: Symbol,
        tail: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self {
            head_list,
            tail,
            source,
            next,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ListGetAt {
    pub bind: Symbol,
    pub source: Symbol,
    pub index: u32,
    pub next: Id<PatternMatcher>,
}

// <bind> = extract_list_at <source> <index> => <next>
// <bind> =
//    extract_list_at <source> <index>
//    => <next>
impl<'a> Notate<'a, ListGetAt> for IrPrinter<'a> {
    fn notate(&self, node: &ListGetAt, arena: &'a Bump) -> Notation<'a> {
        let ListGetAt {
            bind,
            source,
            index,
            next,
        } = node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let index = index.display_in(arena);
        let next = self.notate(next, arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" get_at "),
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
            arena.notate("get_at "),
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

impl ListGetAt {
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
pub struct ListSplitAt {
    pub head: Symbol,
    pub tail: Symbol,
    pub source: Symbol,
    pub index: u32,
    pub next: Id<PatternMatcher>,
}

// <bind> = extract_list_slice_from <source> <start_index> => <next>
// <bind> =
//    extract_list_slice_from <source> <start_index>
//    => <next>
impl<'a> Notate<'a, ListSplitAt> for IrPrinter<'a> {
    fn notate(&self, node: &ListSplitAt, arena: &'a Bump) -> Notation<'a> {
        let ListSplitAt {
            head,
            tail,
            source,
            index,
            next,
        } = node;

        let head = head.display_in(arena);
        let tail = tail.display_in(arena);
        let source = source.display_in(arena);
        let start_index = index.display_in(arena);
        let next = self.notate(next, arena);

        let head = [head, arena.notate(", "), tail, arena.notate(" =")].concat_in(arena);

        let single = [
            arena.notate(" split_at "),
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
            arena.notate("split_at "),
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

impl ListSplitAt {
    pub fn new(
        head: Symbol,
        tail: Symbol,
        source: Symbol,
        index: u32,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let next = builder.add(next.into());
        Self {
            head,
            tail,
            source,
            index,
            next,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RecordGetAt {
    pub bind: Symbol,
    pub source: Symbol,
    pub field: StrKey,
    pub next: Id<PatternMatcher>,
}

// <bind> = extract_record_field <source> <field> => <next>
// <bind> =
//    extract_record_field <source> <field>
//    => <next>
impl<'a> Notate<'a, RecordGetAt> for IrPrinter<'a> {
    fn notate(&self, node: &RecordGetAt, arena: &'a Bump) -> Notation<'a> {
        let RecordGetAt {
            bind,
            source,
            field,
            next,
        } = node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let field = field.display_in(arena);
        let next = self.notate(next, arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" get_at "),
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
            arena.notate("get_at "),
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

impl RecordGetAt {
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
pub struct VariantGet {
    pub bind: Symbol,
    pub source: Symbol,
    pub next: Id<PatternMatcher>,
}

// <bind> = get <source> => <next>
// <bind> =
//    get <source>
//    => <next>
impl<'a> Notate<'a, VariantGet> for IrPrinter<'a> {
    fn notate(&self, node: &VariantGet, arena: &'a Bump) -> Notation<'a> {
        let VariantGet { bind, source, next } = node;

        let bind = bind.display_in(arena);
        let source = source.display_in(arena);
        let next = self.notate(next, arena);

        let head = bind.then(arena.notate(" ="), arena);

        let single = [
            arena.notate(" get "),
            source.clone(),
            arena.notate(" => "),
            next.clone(),
        ]
        .concat_in(arena)
        .flatten(arena);

        let multi = [
            arena.newline(),
            arena.notate("get "),
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

impl VariantGet {
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

impl<'a> Notate<'a, PatternSuccess> for IrPrinter<'a> {
    fn notate(&self, node: &PatternSuccess, arena: &'a Bump) -> Notation<'a> {
        arena
            .just('!')
            .then(self.labels[node.next.as_usize()].display_in(arena), arena)
    }
}

impl PatternSuccess {
    pub fn new(next: Id<Expr>) -> Self {
        Self { next }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PatternFailure;

impl<'a> Notate<'a, PatternFailure> for IrPrinter<'a> {
    fn notate(&self, _node: &PatternFailure, arena: &'a Bump) -> Notation<'a> {
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
    IsVariant(IsVariant),
    IsList(IsList),
    ListIsExact(ListIsExact),
    ListIsAtLeast(ListIsAtLeast),
    IsRecord(IsRecord),
    RecordHasField(RecordHasField),

    // Extractors
    Identity(Identity),
    ListSplitHead(ListSplitHead),
    ListSplitTail(ListSplitTail),
    ListGetAt(ListGetAt),
    ListSplitAt(ListSplitAt),
    RecordGetAt(RecordGetAt),
    VariantGet(VariantGet),

    // Control flow
    Success(PatternSuccess),
    Failure(PatternFailure),
}

impl<'a> Notate<'a, Id<PatternMatcher>> for IrPrinter<'a> {
    fn notate(&self, node: &Id<PatternMatcher>, arena: &'a Bump) -> Notation<'a> {
        if self.shared[node.as_usize()] {
            return arena
                .just('@')
                .then(self.labels[node.as_usize()].display_in(arena), arena);
        }

        match self.ir.instr(*node) {
            PatternMatcher::IsUnit(tester) => self.notate(&tester, arena),
            PatternMatcher::IsBool(tester) => self.notate(&tester, arena),
            PatternMatcher::IsNum(tester) => self.notate(&tester, arena),
            PatternMatcher::IsChar(tester) => self.notate(&tester, arena),
            PatternMatcher::IsStr(tester) => self.notate(&tester, arena),
            PatternMatcher::IsVariant(tester) => self.notate(&tester, arena),
            PatternMatcher::IsList(tester) => self.notate(&tester, arena),
            PatternMatcher::ListIsExact(tester) => self.notate(&tester, arena),
            PatternMatcher::ListIsAtLeast(tester) => self.notate(&tester, arena),
            PatternMatcher::IsRecord(tester) => self.notate(&tester, arena),
            PatternMatcher::RecordHasField(tester) => self.notate(&tester, arena),
            PatternMatcher::Identity(extractor) => self.notate(&extractor, arena),
            PatternMatcher::ListSplitHead(extractor) => self.notate(&extractor, arena),
            PatternMatcher::ListSplitTail(extractor) => self.notate(&extractor, arena),
            PatternMatcher::ListGetAt(extractor) => self.notate(&extractor, arena),
            PatternMatcher::ListSplitAt(extractor) => self.notate(&extractor, arena),
            PatternMatcher::RecordGetAt(extractor) => self.notate(&extractor, arena),
            PatternMatcher::VariantGet(extractor) => self.notate(&extractor, arena),
            PatternMatcher::Success(success) => self.notate(&success, arena),
            PatternMatcher::Failure(failure) => self.notate(&failure, arena),
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
    pub fn identity(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = Identity::new(bind, source, next, builder);
        Self::Identity(extractor)
    }

    pub fn list_split_head(
        head: Symbol,
        tail_list: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ListSplitHead::new(head, tail_list, source, next, builder);
        Self::ListSplitHead(extractor)
    }

    pub fn list_split_tail(
        head_list: Symbol,
        tail: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ListSplitTail::new(head_list, tail, source, next, builder);
        Self::ListSplitTail(extractor)
    }

    pub fn list_get_at(
        bind: Symbol,
        source: Symbol,
        index: u32,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ListGetAt::new(bind, source, index, next, builder);
        Self::ListGetAt(extractor)
    }

    pub fn list_split_at(
        head: Symbol,
        tail: Symbol,
        source: Symbol,
        index: u32,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = ListSplitAt::new(head, tail, source, index, next, builder);
        Self::ListSplitAt(extractor)
    }

    pub fn record_get_at(
        bind: Symbol,
        source: Symbol,
        field: StrKey,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = RecordGetAt::new(bind, source, field, next, builder);
        Self::RecordGetAt(extractor)
    }

    pub fn variant_get(
        bind: Symbol,
        source: Symbol,
        next: impl Into<PatternMatcher>,
        builder: &mut IrBuilder,
    ) -> Self {
        let extractor = VariantGet::new(bind, source, next, builder);
        Self::VariantGet(extractor)
    }
}
