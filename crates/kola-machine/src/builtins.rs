use std::fs;

use kola_builtins::{BuiltinLabel, BuiltinLexicon, BuiltinTag};
use kola_ir::{
    id::Id,
    instr::{Expr, Func, Label, Symbol, Tag},
};
use kola_runtime::{
    closure::Closure,
    env::EnvIdx,
    heap::Heap,
    string::StringIdx,
    value::{Value, to_usize_exact},
};

use crate::{
    config::{MachineState, StandardConfig},
    cont::{Cont, ContFrame},
    machine::Ctx,
};

// ── helper variant constructors ──────────────────────────────────

#[inline]
fn ok_variant(value: Value, heap: &mut Heap, lexicon: &BuiltinLexicon) -> Value {
    Value::Variant(heap.variants.alloc(Tag(lexicon.tag(BuiltinTag::Ok)), value))
}

#[inline]
fn err_variant_msg(msg: &str, heap: &mut Heap, lexicon: &BuiltinLexicon) -> Value {
    let err = heap.strings.alloc(msg);
    Value::Variant(
        heap.variants
            .alloc(Tag(lexicon.tag(BuiltinTag::Err)), Value::Str(err)),
    )
}

#[inline]
fn some_variant(value: Value, heap: &mut Heap, lexicon: &BuiltinLexicon) -> Value {
    Value::Variant(
        heap.variants
            .alloc(Tag(lexicon.tag(BuiltinTag::Some)), value),
    )
}

#[inline]
fn none_variant(heap: &mut Heap, lexicon: &BuiltinLexicon) -> Value {
    Value::Variant(
        heap.variants
            .alloc(Tag(lexicon.tag(BuiltinTag::None)), Value::None),
    )
}

// ── IO builtins ──────────────────────────────────────────────────

pub fn io_debug(arg: Value, _heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    println!("Debug: {:?}", arg);
    Ok(arg)
}

pub fn io_read_file(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Str(path) = arg else {
        return Err("io_read_file requires a string argument".to_owned());
    };
    let path = heap.strings.get_or_default(&path);
    match fs::read_to_string(ctx.join_path(path)) {
        Err(err) => Ok(err_variant_msg(&err.to_string(), heap, &ctx.lexicon)),
        Ok(contents) => {
            let contents = heap.strings.alloc(&contents);
            Ok(ok_variant(Value::Str(contents), heap, &ctx.lexicon))
        }
    }
}

pub fn io_write_file(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("io_write_file requires a record argument".to_owned());
    };

    let Some(Value::Str(path)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Path))
    else {
        return Err("io_write_file requires 'path' field with a string".to_owned());
    };
    let Some(Value::Str(contents)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Contents))
    else {
        return Err("io_write_file requires 'contents' field with a string".to_owned());
    };

    let [path, contents] = heap.strings.resolve_many([&path, &contents]);
    match fs::write(ctx.join_path(path.get()), contents.get()) {
        Err(err) => Ok(err_variant_msg(&err.to_string(), heap, &ctx.lexicon)),
        Ok(_) => Ok(ok_variant(Value::None, heap, &ctx.lexicon)),
    }
}

// ── List builtins ────────────────────────────────────────────────

pub fn list_length(arg: Value, heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    let Value::List(list) = arg else {
        return Err("list_length requires a list argument".to_owned());
    };
    Ok(Value::Num(heap.lists.len(list) as f64))
}

pub fn list_is_empty(arg: Value, heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    let Value::List(list) = arg else {
        return Err("list_is_empty requires a list argument".to_owned());
    };
    Ok(Value::Bool(heap.lists.is_empty(list)))
}

pub fn list_reverse(arg: Value, heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    let Value::List(list) = arg else {
        return Err("list_reverse requires a list argument".to_owned());
    };
    Ok(Value::List(heap.lists.reverse(list)))
}

pub fn list_sum(arg: Value, heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    let Value::List(list) = arg else {
        return Err("list_sum requires a list argument".to_owned());
    };
    heap.lists
        .try_fold(list, 0.0, |acc, v| {
            if let Value::Num(n) = v {
                Ok(acc + n)
            } else {
                Err(format!("Cannot sum non-numeric value: {:?}", v))
            }
        })
        .map(Value::Num)
}

pub fn list_contains(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("list_contains requires a record argument".to_owned());
    };

    let Some(Value::List(list)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::List))
    else {
        return Err("list_contains requires 'list' field with a list".to_owned());
    };
    let Some(value) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Value))
    else {
        return Err("list_contains requires 'value' field".to_owned());
    };
    Ok(Value::Bool(heap.lists.contains(list, value)))
}

pub fn list_at(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("list_at requires a record argument".to_owned());
    };

    let Some(Value::List(list)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::List))
    else {
        return Err("list_at requires 'list' field with a list".to_owned());
    };
    let Some(Value::Num(index)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Index))
    else {
        return Err("list_at requires 'index' field".to_owned());
    };
    match to_usize_exact(index).and_then(|idx| heap.lists.get_element(list, idx)) {
        Some(value) => Ok(some_variant(value, heap, &ctx.lexicon)),
        None => Ok(none_variant(heap, &ctx.lexicon)),
    }
}

pub fn list_first(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::List(list) = arg else {
        return Err("list_first requires a list argument".to_owned());
    };

    match heap.lists.first(list) {
        Some(head) => Ok(some_variant(head, heap, &ctx.lexicon)),
        None => Ok(none_variant(heap, &ctx.lexicon)),
    }
}

pub fn list_last(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::List(list) = arg else {
        return Err("list_last requires a list argument".to_owned());
    };

    match heap.lists.last(list) {
        Some(tail) => Ok(some_variant(tail, heap, &ctx.lexicon)),
        None => Ok(none_variant(heap, &ctx.lexicon)),
    }
}

pub fn list_prepend(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("list_prepend requires a record argument".to_owned());
    };

    let Some(head_value) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Head))
    else {
        return Err("list_prepend requires 'head' field".to_owned());
    };
    let Some(Value::List(tail_list)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Tail))
    else {
        return Err("list_prepend requires 'tail' field with a list".to_owned());
    };
    let result = heap.lists.push_front(tail_list, head_value);
    Ok(Value::List(Some(result)))
}

pub fn list_append(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("list_append requires a record argument".to_owned());
    };

    let Some(Value::List(head_list)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Head))
    else {
        return Err("list_append requires 'head' field with a list".to_owned());
    };
    let Some(tail_value) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Tail))
    else {
        return Err("list_append requires 'tail' field".to_owned());
    };
    let result = heap.lists.push_back(head_list, tail_value);
    Ok(Value::List(Some(result)))
}

pub fn list_concat(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("list_concat requires a record argument".to_owned());
    };

    let Some(Value::List(head_list)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Head))
    else {
        return Err("list_concat requires 'head' field with a list".to_owned());
    };
    let Some(Value::List(tail_list)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Tail))
    else {
        return Err("list_concat requires 'tail' field with a list".to_owned());
    };
    let result = heap.lists.concat(head_list, tail_list);
    Ok(Value::List(result))
}

// ── Numerical builtins ───────────────────────────────────────────

macro_rules! num_unary {
    ($name:ident, $method:ident) => {
        pub fn $name(arg: Value, _heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
            let Value::Num(n) = arg else {
                return Err(concat!(stringify!($name), " requires a numeric argument").to_owned());
            };
            Ok(Value::Num(n.$method()))
        }
    };
}

num_unary!(num_abs, abs);
num_unary!(num_sqrt, sqrt);
num_unary!(num_floor, floor);
num_unary!(num_ceil, ceil);
num_unary!(num_round, round);
num_unary!(num_sin, sin);
num_unary!(num_cos, cos);
num_unary!(num_tan, tan);
num_unary!(num_ln, ln);
num_unary!(num_log10, log10);
num_unary!(num_exp, exp);

pub fn num_pow(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("num_pow requires a record argument".to_owned());
    };

    let Some(Value::Num(base)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Base))
    else {
        return Err("num_pow requires 'base' field".to_owned());
    };
    let Some(Value::Num(exp)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Exp))
    else {
        return Err("num_pow requires 'exp' field".to_owned());
    };
    Ok(Value::Num(base.powf(exp)))
}

// ── Record builtins ──────────────────────────────────────────────

pub fn record_select(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(outer) = arg else {
        return Err("record_select requires a record argument".to_owned());
    };

    let Some(Value::Label(Label(label_key))) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Label))
    else {
        return Err("record_select requires 'label' field with a label".to_owned());
    };

    let Some(Value::Record(record)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Record))
    else {
        return Err("record_select requires 'record' field with a record".to_owned());
    };

    match heap.records.get_value(record, label_key) {
        Some(value) => Ok(value),
        None => Err(format!(
            "Field '{}' not found in record",
            heap.strings.interner[label_key]
        )),
    }
}

pub fn record_insert(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(outer) = arg else {
        return Err("record_insert requires a record argument".to_owned());
    };

    let Some(Value::Label(Label(label_key))) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Label))
    else {
        return Err("record_insert requires 'label' field with a label".to_owned());
    };

    let Some(value) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Value))
    else {
        return Err("record_insert requires 'value' field".to_owned());
    };

    let Some(Value::Record(record)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Record))
    else {
        return Err("record_insert requires 'record' field with a record".to_owned());
    };

    heap.records.insert(record, label_key, value);
    Ok(Value::Record(record))
}

pub fn record_remove(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(outer) = arg else {
        return Err("record_remove requires a record argument".to_owned());
    };

    let Some(Value::Label(Label(label_key))) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Label))
    else {
        return Err("record_remove requires 'label' field with a label".to_owned());
    };

    let Some(Value::Record(record)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Record))
    else {
        return Err("record_remove requires 'record' field with a record".to_owned());
    };

    heap.records.remove(record, label_key);
    Ok(Value::Record(record))
}

pub fn record_rename(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(outer) = arg else {
        return Err("record_rename requires a record argument".to_owned());
    };

    let Some(Value::Label(Label(from_key))) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::From))
    else {
        return Err("record_rename requires 'from' field with a label".to_owned());
    };

    let Some(Value::Label(Label(to_key))) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::To))
    else {
        return Err("record_rename requires 'to' field with a label".to_owned());
    };

    let Some(Value::Record(record)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Record))
    else {
        return Err("record_rename requires 'record' field with a record".to_owned());
    };

    if let Some((value, record)) = heap.records.remove(record, from_key) {
        heap.records.insert(record, to_key, value);
        Ok(Value::Record(record))
    } else {
        Err(format!(
            "Field '{}' not found in record",
            heap.strings.interner[from_key]
        ))
    }
}

pub fn record_contains(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(outer) = arg else {
        return Err("record_contains requires a record argument".to_owned());
    };

    let Some(Value::Label(Label(label_key))) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Label))
    else {
        return Err("record_contains requires 'label' field with a label".to_owned());
    };

    let Some(Value::Record(record)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Record))
    else {
        return Err("record_contains requires 'record' field with a record".to_owned());
    };

    Ok(Value::Bool(heap.records.contains_key(record, label_key)))
}

pub fn record_keys(arg: Value, heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("record_keys requires a record argument".to_owned());
    };

    let keys = heap
        .records
        .iter(record)
        .map(|(k, _)| Value::Str(Some(StringIdx::Static(k))));

    let list = heap.lists.alloc_from_iter(keys);

    Ok(Value::List(list))
}

pub fn record_size(arg: Value, heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("record_size requires a record argument".to_owned());
    };
    Ok(Value::Num(heap.records.len(record) as f64))
}

pub fn record_merge_left(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(outer) = arg else {
        return Err("record_merge_left requires a record argument".to_owned());
    };

    let Some(Value::Record(left)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Left))
    else {
        return Err("record_merge_left requires 'left' field with a record".to_owned());
    };
    let Some(Value::Record(right)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Right))
    else {
        return Err("record_merge_left requires 'right' field with a record".to_owned());
    };
    let merged = heap.records.merge_left(left, right);
    Ok(Value::Record(merged))
}

pub fn record_merge_right(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(outer) = arg else {
        return Err("record_merge_right requires a record argument".to_owned());
    };

    let Some(Value::Record(left)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Left))
    else {
        return Err("record_merge_right requires 'left' field with a record".to_owned());
    };
    let Some(Value::Record(right)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Right))
    else {
        return Err("record_merge_right requires 'right' field with a record".to_owned());
    };
    let merged = heap.records.merge_right(left, right);
    Ok(Value::Record(merged))
}

// ── Serde builtins ───────────────────────────────────────────────

pub fn serde_from_json(_arg: Value, _heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    todo!("serde_from_json not yet migrated to new label/value system")
}

pub fn serde_to_json(_arg: Value, _heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    todo!("serde_to_json not yet migrated to new label/value system")
}

// ── String builtins ──────────────────────────────────────────────

pub fn str_length(arg: Value, heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    let Value::Str(s) = arg else {
        return Err("str_length requires a string argument".to_owned());
    };
    Ok(Value::Num(
        s.map(|s| heap.strings.len(s) as f64).unwrap_or(0.0),
    ))
}

pub fn str_is_empty(arg: Value, _heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    let Value::Str(s) = arg else {
        return Err("str_is_empty requires a string argument".to_owned());
    };
    Ok(Value::Bool(s.is_none()))
}

pub fn str_reverse(arg: Value, heap: &mut Heap, _ctx: &Ctx) -> Result<Value, String> {
    let Value::Str(s) = arg else {
        return Err("str_reverse requires a string argument".to_owned());
    };
    Ok(Value::Str(s.map(|s| heap.strings.reverse(s))))
}

pub fn str_first(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Str(s) = arg else {
        return Err("str_first requires a string argument".to_owned());
    };

    if let Some(s) = s {
        let first = heap.strings.first(s);
        Ok(some_variant(Value::Char(first), heap, &ctx.lexicon))
    } else {
        Ok(none_variant(heap, &ctx.lexicon))
    }
}

pub fn str_last(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Str(s) = arg else {
        return Err("str_last requires a string argument".to_owned());
    };

    if let Some(s) = s {
        let last = heap.strings.last(s);
        Ok(some_variant(Value::Char(last), heap, &ctx.lexicon))
    } else {
        Ok(none_variant(heap, &ctx.lexicon))
    }
}

pub fn str_contains(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("str_contains requires a record argument".to_owned());
    };

    let Some(Value::Str(s)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Str))
    else {
        return Err("str_contains requires 'str' field with a string".to_owned());
    };
    let Some(Value::Char(c)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Value))
    else {
        return Err("str_contains requires 'value' field with a char".to_owned());
    };

    let result = if let Some(s) = s {
        let mut buf = [0; 4];
        let needle: &str = c.encode_utf8(&mut buf);
        heap.strings.contains(s, needle)
    } else {
        false
    };
    Ok(Value::Bool(result))
}

pub fn str_at(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("str_at requires a record argument".to_owned());
    };

    let Some(Value::Str(s)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Str))
    else {
        return Err("str_at requires 'str' field with a string".to_owned());
    };
    let Some(Value::Num(index)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Index))
    else {
        return Err("str_at requires 'index' field with a number".to_owned());
    };

    let Some(s) = s else {
        return Ok(none_variant(heap, &ctx.lexicon));
    };

    let Some(idx) = to_usize_exact(index) else {
        return Err("str_at requires 'index' field to be a non-negative integer".to_owned());
    };

    match heap.strings.at(s, idx) {
        Some(c) => Ok(some_variant(Value::Char(c), heap, &ctx.lexicon)),
        None => Ok(none_variant(heap, &ctx.lexicon)),
    }
}

pub fn str_prepend(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("str_prepend requires a record argument".to_owned());
    };

    let Some(Value::Char(c)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Head))
    else {
        return Err("str_prepend requires 'head' field with a char".to_owned());
    };
    let Some(Value::Str(s)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Tail))
    else {
        return Err("str_prepend requires 'tail' field with a string".to_owned());
    };

    if let Some(s) = s {
        let mut buf = [0; 4];
        let c: &str = c.encode_utf8(&mut buf);
        Ok(Value::Str(Some(heap.strings.push_front(s, c))))
    } else {
        Ok(Value::Str(Some(StringIdx::unit(c))))
    }
}

pub fn str_append(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("str_append requires a record argument".to_owned());
    };

    let Some(Value::Str(s)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Head))
    else {
        return Err("str_append requires 'head' field with a string".to_owned());
    };
    let Some(Value::Char(c)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Tail))
    else {
        return Err("str_append requires 'tail' field with a char".to_owned());
    };

    if let Some(s) = s {
        let mut buf = [0; 4];
        let c: &str = c.encode_utf8(&mut buf);
        Ok(Value::Str(Some(heap.strings.push_back(s, c))))
    } else {
        Ok(Value::Str(Some(StringIdx::unit(c))))
    }
}

pub fn str_concat(arg: Value, heap: &mut Heap, ctx: &Ctx) -> Result<Value, String> {
    let Value::Record(record) = arg else {
        return Err("str_concat requires a record argument".to_owned());
    };

    let Some(Value::Str(s1)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Head))
    else {
        return Err("str_concat requires 'head' field with a string".to_owned());
    };
    let Some(Value::Str(s2)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Tail))
    else {
        return Err("str_concat requires 'tail' field with a string".to_owned());
    };

    let result = match (s1, s2) {
        (Some(s1), Some(s2)) => Some(heap.strings.concat(s1, s2)),
        (Some(s), None) | (None, Some(s)) => Some(s),
        _ => None,
    };
    Ok(Value::Str(result))
}

// ── Complex (recursion) builtins ──────────────────────────────────
//
// These return MachineState directly because they interact with the
// continuation stack and environment.

// list_rec([], base, step) = base
// list_rec([head, ...tail], base, step) = step(head, list_rec(tail, base, step))
pub fn list_rec(
    bind: Symbol,
    arg: Value,
    env: EnvIdx,
    mut cont: Cont,
    next: Id<Expr>,
    ctx: &mut Ctx,
    heap: &mut Heap,
) -> MachineState {
    let Value::Record(record) = arg else {
        return MachineState::Error("list_rec requires a record argument".to_owned());
    };

    let Some(Value::List(list)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::List))
    else {
        return MachineState::Error("list_rec requires 'list' field with a list".to_owned());
    };

    let Some(base) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Base))
    else {
        return MachineState::Error("list_rec requires 'base' field".to_owned());
    };

    let Some(Value::Closure(step)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Step))
    else {
        return MachineState::Error("list_rec requires 'step' field with a func".to_owned());
    };

    let Some(list) = list else {
        // Empty list — bind base directly, go to next
        let env = heap.envs.insert(env, bind, base);
        return MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        });
    };

    let (head, tail) = heap.lists.split_front(list);

    // Create a pure continuation frame for the next expression
    let pure_frame = ContFrame::pure(bind, next, env);
    cont.push(pure_frame);

    // Create a primitive recursion frame for the list_rec operation
    if let Some(tail) = tail {
        let rec_frame = ContFrame::list_rec(tail, step);
        cont.push(rec_frame);
    }

    let Closure {
        env,
        func: Func { param, body },
    } = step;

    // Create argument for the step function
    let acc = (ctx.lexicon.label(BuiltinLabel::Acc), base);
    let head = (ctx.lexicon.label(BuiltinLabel::Head), head);
    let record = heap.records.alloc_fixed([acc, head]);

    let env = heap.envs.insert(env, param, Value::Record(record));

    MachineState::Standard(StandardConfig {
        control: body.get(&ctx.ir),
        env,
        cont,
    })
}

pub fn num_rec(
    bind: Symbol,
    arg: Value,
    env: EnvIdx,
    mut cont: Cont,
    next: Id<Expr>,
    ctx: &mut Ctx,
    heap: &mut Heap,
) -> MachineState {
    let Value::Record(record) = arg else {
        return MachineState::Error("num_rec requires a record argument".to_owned());
    };

    let Some(Value::Num(n)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Num))
    else {
        return MachineState::Error("num_rec requires 'num' field with a number".to_owned());
    };

    let Some(base) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Base))
    else {
        return MachineState::Error("num_rec requires 'base' field".to_owned());
    };

    let Some(Value::Closure(step)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Step))
    else {
        return MachineState::Error("num_rec requires 'step' field with a func".to_owned());
    };

    if n <= 0.0 {
        // Empty number — bind base directly, go to next
        let env = heap.envs.insert(env, bind, base);
        return MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        });
    }

    // Create a pure continuation frame for the next expression
    let pure_frame = ContFrame::pure(bind, next, env);
    cont.push(pure_frame);

    // Create a primitive recursion frame
    if n >= 2.0 {
        let rec_frame = ContFrame::num_rec(n - 2.0, step);
        cont.push(rec_frame);
    }

    let Closure {
        env,
        func: Func { param, body },
    } = step;

    // Create argument for the step function
    let acc = (ctx.lexicon.label(BuiltinLabel::Acc), base);
    let head = (ctx.lexicon.label(BuiltinLabel::Head), Value::Num(n - 1.0));
    let record = heap.records.alloc_fixed([acc, head]);

    let env = heap.envs.insert(env, param, Value::Record(record));

    MachineState::Standard(StandardConfig {
        control: body.get(&ctx.ir),
        env,
        cont,
    })
}

pub fn record_rec(
    bind: Symbol,
    arg: Value,
    env: EnvIdx,
    mut cont: Cont,
    next: Id<Expr>,
    ctx: &mut Ctx,
    heap: &mut Heap,
) -> MachineState {
    let Value::Record(outer) = arg else {
        return MachineState::Error("record_rec requires a record argument".to_owned());
    };

    let Some(Value::Record(record)) = heap
        .records
        .get_value(outer, ctx.lexicon.label(BuiltinLabel::Record))
    else {
        return MachineState::Error("record_rec requires 'record' field with a record".to_owned());
    };

    let Some(base) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Base))
    else {
        return MachineState::Error("record_rec requires 'base' field".to_owned());
    };

    let Some(Value::Closure(step)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Step))
    else {
        return MachineState::Error("record_rec requires 'step' field with a func".to_owned());
    };

    let Some(record) = record else {
        // Empty record — bind base directly, go to next
        let env = heap.envs.insert(env, bind, base);
        return MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        });
    };

    let (first, tail) = heap.records.split_front(record);

    let key = (
        ctx.lexicon.label(BuiltinLabel::Key),
        Value::Str(Some(StringIdx::Static(first.0))),
    );
    let val = (ctx.lexicon.label(BuiltinLabel::Value), first.1);
    let head = heap.records.alloc_fixed([key, val]);

    // Create a pure continuation frame for the next expression
    let pure_frame = ContFrame::pure(bind, next, env);
    cont.push(pure_frame);

    // Create a primitive recursion frame for the record_rec operation
    if let Some(tail) = tail {
        let rec_frame = ContFrame::record_rec(tail, step);
        cont.push(rec_frame);
    }

    let Closure {
        env,
        func: Func { param, body },
    } = step;

    // Create argument for the step function
    let acc = (ctx.lexicon.label(BuiltinLabel::Acc), base);
    let head = (ctx.lexicon.label(BuiltinLabel::Head), Value::Record(head));
    let record = heap.records.alloc_fixed([acc, head]);

    let env = heap.envs.insert(env, param, Value::Record(record));

    MachineState::Standard(StandardConfig {
        control: body.get(&ctx.ir),
        env,
        cont,
    })
}

pub fn str_rec(
    bind: Symbol,
    arg: Value,
    env: EnvIdx,
    mut cont: Cont,
    next: Id<Expr>,
    ctx: &mut Ctx,
    heap: &mut Heap,
) -> MachineState {
    let Value::Record(record) = arg else {
        return MachineState::Error("str_rec requires a record argument".to_owned());
    };

    let Some(Value::Str(s)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Str))
    else {
        return MachineState::Error("str_rec requires 's' field with a string".to_owned());
    };

    let Some(base) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Base))
    else {
        return MachineState::Error("str_rec requires 'base' field".to_owned());
    };

    let Some(Value::Closure(step)) = heap
        .records
        .get_value(record, ctx.lexicon.label(BuiltinLabel::Step))
    else {
        return MachineState::Error("str_rec requires 'step' field with a func".to_owned());
    };

    let Some(s) = s else {
        // Empty string — bind base directly, go to next
        let env = heap.envs.insert(env, bind, base);
        return MachineState::Standard(StandardConfig {
            control: next.get(&ctx.ir),
            env,
            cont,
        });
    };

    let (head, tail) = heap.strings.pop_front(s);

    // Create a pure continuation frame for the next expression
    let pure_frame = ContFrame::pure(bind, next, env);
    cont.push(pure_frame);

    // Create a primitive recursion frame
    if let Some(tail) = tail {
        let rec_frame = ContFrame::str_rec(tail, step);
        cont.push(rec_frame);
    }

    let Closure {
        env,
        func: Func { param, body },
    } = step;

    // Create argument for the step function
    let acc = (ctx.lexicon.label(BuiltinLabel::Acc), base);
    let head = (ctx.lexicon.label(BuiltinLabel::Head), Value::Char(head));
    let record = heap.records.alloc_fixed([acc, head]);

    let env = heap.envs.insert(env, param, Value::Record(record));

    MachineState::Standard(StandardConfig {
        control: body.get(&ctx.ir),
        env,
        cont,
    })
}
