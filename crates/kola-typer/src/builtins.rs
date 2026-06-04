use kola_builtins::{BuiltinId, BuiltinLabel, BuiltinLexicon, BuiltinTag};
use kola_types::{
    kind::Kind,
    types::{Label, LabelOrVar, LabeledType, MonoType, PolyType, Row, TypeVar},
};

pub fn builtin_type(id: BuiltinId, lexicon: &BuiltinLexicon) -> PolyType {
    let func = MonoType::func;
    let list = MonoType::list;
    let rec = MonoType::record;
    let variant = MonoType::variant;
    let var = MonoType::Var;
    let wit_t = |v: TypeVar| MonoType::wit(MonoType::Var(v));
    let wit_l = |v: TypeVar| MonoType::wit(MonoType::Label(LabelOrVar::Var(v)));

    const CHAR: MonoType = MonoType::CHAR;
    const STR: MonoType = MonoType::STR;
    const NUM: MonoType = MonoType::NUM;
    const BOOL: MonoType = MonoType::BOOL;
    const UNIT: MonoType = MonoType::UNIT;

    // Updated label-based row extension helper
    let ext_l = |lbl: BuiltinLabel, ty: MonoType, tail: Row| {
        Row::ext(
            LabeledType::new(LabelOrVar::Label(Label(lexicon.label(lbl))), ty),
            tail,
        )
    };

    //  Updated tag-based row extension helper
    let ext_t = |tag: BuiltinTag, ty: MonoType, tail: Row| {
        Row::ext(
            LabeledType::new(LabelOrVar::Label(Label(lexicon.tag(tag))), ty),
            tail,
        )
    };

    let mono = match id {
        // ---- IO Operations ----
        BuiltinId::IoDebug => {
            let a = TypeVar::new(Kind::Type);
            func(var(a), var(a))
        }
        BuiltinId::IoReadFile => func(
            STR,
            variant(ext_t(
                BuiltinTag::Ok,
                STR,
                ext_t(BuiltinTag::Err, STR, Row::Empty),
            )),
        ),
        BuiltinId::IoWriteFile => func(
            rec(ext_l(
                BuiltinLabel::Path,
                STR,
                ext_l(BuiltinLabel::Contents, STR, Row::Empty),
            )),
            variant(ext_t(
                BuiltinTag::Ok,
                UNIT,
                ext_t(BuiltinTag::Err, STR, Row::Empty),
            )),
        ),

        // ---- List Manipulation ----
        BuiltinId::ListLength => {
            let a = TypeVar::new(Kind::Type);
            func(list(var(a)), NUM)
        }
        BuiltinId::ListIsEmpty => {
            let a = TypeVar::new(Kind::Type);
            func(list(var(a)), BOOL)
        }
        BuiltinId::ListReverse => {
            let a = TypeVar::new(Kind::Type);
            func(list(var(a)), list(var(a)))
        }
        BuiltinId::ListSum => func(list(NUM), NUM),
        BuiltinId::ListFirst => {
            let a = TypeVar::new(Kind::Type);
            func(
                list(var(a)),
                variant(ext_t(
                    BuiltinTag::Some,
                    var(a),
                    ext_t(BuiltinTag::None, UNIT, Row::Empty),
                )),
            )
        }
        BuiltinId::ListLast => {
            let a = TypeVar::new(Kind::Type);
            func(
                list(var(a)),
                variant(ext_t(
                    BuiltinTag::Some,
                    var(a),
                    ext_t(BuiltinTag::None, UNIT, Row::Empty),
                )),
            )
        }
        BuiltinId::ListContains => {
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::List,
                    list(var(a)),
                    ext_l(BuiltinLabel::Value, var(a), Row::Empty),
                )),
                BOOL,
            )
        }
        BuiltinId::ListAt => {
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::List,
                    list(var(a)),
                    ext_l(BuiltinLabel::Index, NUM, Row::Empty),
                )),
                variant(ext_t(
                    BuiltinTag::Some,
                    var(a),
                    ext_t(BuiltinTag::None, UNIT, Row::Empty),
                )),
            )
        }
        BuiltinId::ListPrepend => {
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::Head,
                    var(a),
                    ext_l(BuiltinLabel::Tail, list(var(a)), Row::Empty),
                )),
                list(var(a)),
            )
        }
        BuiltinId::ListAppend => {
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::Head,
                    list(var(a)),
                    ext_l(BuiltinLabel::Tail, var(a), Row::Empty),
                )),
                list(var(a)),
            )
        }
        BuiltinId::ListConcat => {
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::Head,
                    list(var(a)),
                    ext_l(BuiltinLabel::Tail, list(var(a)), Row::Empty),
                )),
                list(var(a)),
            )
        }
        BuiltinId::ListRec => {
            let a = TypeVar::new(Kind::Type);
            let b = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::List,
                    list(var(a)),
                    ext_l(
                        BuiltinLabel::Base,
                        var(b),
                        ext_l(
                            BuiltinLabel::Step,
                            func(
                                rec(ext_l(
                                    BuiltinLabel::Acc,
                                    var(b),
                                    ext_l(BuiltinLabel::Head, var(a), Row::Empty),
                                )),
                                var(b),
                            ),
                            Row::Empty,
                        ),
                    ),
                )),
                var(b),
            )
        }

        // ---- Numeric Computations ----
        BuiltinId::NumAbs => func(NUM, NUM),
        BuiltinId::NumSqrt => func(NUM, NUM),
        BuiltinId::NumFloor => func(NUM, NUM),
        BuiltinId::NumCeil => func(NUM, NUM),
        BuiltinId::NumRound => func(NUM, NUM),
        BuiltinId::NumSin => func(NUM, NUM),
        BuiltinId::NumCos => func(NUM, NUM),
        BuiltinId::NumTan => func(NUM, NUM),
        BuiltinId::NumLn => func(NUM, NUM),
        BuiltinId::NumLog10 => func(NUM, NUM),
        BuiltinId::NumExp => func(NUM, NUM),
        BuiltinId::NumPow => func(
            rec(ext_l(
                BuiltinLabel::Base,
                NUM,
                ext_l(BuiltinLabel::Exp, NUM, Row::Empty),
            )),
            NUM,
        ),
        BuiltinId::NumRec => {
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::Num,
                    NUM,
                    ext_l(
                        BuiltinLabel::Base,
                        var(a),
                        ext_l(
                            BuiltinLabel::Step,
                            func(
                                rec(ext_l(
                                    BuiltinLabel::Acc,
                                    var(a),
                                    ext_l(BuiltinLabel::Head, NUM, Row::Empty),
                                )),
                                var(a),
                            ),
                            Row::Empty,
                        ),
                    ),
                )),
                var(a),
            )
        }

        // ---- Row-Polymorphic Records ----
        BuiltinId::RecordSelect => {
            let l = TypeVar::new(Kind::Label);
            let r = TypeVar::new(Kind::Row);
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::Label,
                    wit_l(l),
                    ext_l(
                        BuiltinLabel::Record,
                        rec(ext_l(BuiltinLabel::Field, var(a), Row::Var(r))),
                        Row::Empty,
                    ),
                )),
                var(a),
            )
        }
        BuiltinId::RecordInsert => {
            let l = TypeVar::new(Kind::Label);
            let r1 = TypeVar::new(Kind::Row);
            let r2 = TypeVar::new(Kind::Row);
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::Label,
                    wit_l(l),
                    ext_l(
                        BuiltinLabel::Value,
                        var(a),
                        ext_l(BuiltinLabel::Record, rec(Row::Var(r1)), Row::Empty),
                    ),
                )),
                rec(Row::Var(r2)),
            )
        }
        BuiltinId::RecordRemove => {
            let l = TypeVar::new(Kind::Label);
            let r1 = TypeVar::new(Kind::Row);
            let r2 = TypeVar::new(Kind::Row);
            func(
                rec(ext_l(
                    BuiltinLabel::Label,
                    wit_l(l),
                    ext_l(BuiltinLabel::Record, rec(Row::Var(r1)), Row::Empty),
                )),
                rec(Row::Var(r2)),
            )
        }
        BuiltinId::RecordRename => {
            let l1 = TypeVar::new(Kind::Label);
            let l2 = TypeVar::new(Kind::Label);
            let r1 = TypeVar::new(Kind::Row);
            let r2 = TypeVar::new(Kind::Row);
            func(
                rec(ext_l(
                    BuiltinLabel::From,
                    wit_l(l1),
                    ext_l(
                        BuiltinLabel::To,
                        wit_l(l2),
                        ext_l(BuiltinLabel::Record, rec(Row::Var(r1)), Row::Empty),
                    ),
                )),
                rec(Row::Var(r2)),
            )
        }
        BuiltinId::RecordContains => {
            let l = TypeVar::new(Kind::Label);
            let r = TypeVar::new(Kind::Row);
            func(
                rec(ext_l(
                    BuiltinLabel::Label,
                    wit_l(l),
                    ext_l(BuiltinLabel::Record, rec(Row::Var(r)), Row::Empty),
                )),
                BOOL,
            )
        }
        BuiltinId::RecordKeys => {
            let r = TypeVar::new(Kind::Row);
            func(rec(Row::Var(r)), list(STR))
        }
        BuiltinId::RecordSize => {
            let r = TypeVar::new(Kind::Row);
            func(rec(Row::Var(r)), NUM)
        }
        BuiltinId::RecordMergeLeft => {
            let r1 = TypeVar::new(Kind::Row);
            let r2 = TypeVar::new(Kind::Row);
            let r3 = TypeVar::new(Kind::Row);
            func(
                rec(ext_l(
                    BuiltinLabel::Left,
                    rec(Row::Var(r1)),
                    ext_l(BuiltinLabel::Right, rec(Row::Var(r2)), Row::Empty),
                )),
                rec(Row::Var(r3)),
            )
        }
        BuiltinId::RecordMergeRight => {
            let r1 = TypeVar::new(Kind::Row);
            let r2 = TypeVar::new(Kind::Row);
            let r3 = TypeVar::new(Kind::Row);
            func(
                rec(ext_l(
                    BuiltinLabel::Left,
                    rec(Row::Var(r1)),
                    ext_l(BuiltinLabel::Right, rec(Row::Var(r2)), Row::Empty),
                )),
                rec(Row::Var(r3)),
            )
        }
        BuiltinId::RecordRec => {
            let r = TypeVar::new(Kind::Row);
            let a = TypeVar::new(Kind::Type);
            let b = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::Record,
                    rec(Row::Var(r)),
                    ext_l(
                        BuiltinLabel::Base,
                        var(a),
                        ext_l(
                            BuiltinLabel::Step,
                            func(
                                rec(ext_l(
                                    BuiltinLabel::Acc,
                                    var(a),
                                    ext_l(
                                        BuiltinLabel::Head,
                                        rec(ext_l(
                                            BuiltinLabel::Key,
                                            STR,
                                            ext_l(BuiltinLabel::Value, var(b), Row::Empty),
                                        )),
                                        Row::Empty,
                                    ),
                                )),
                                var(a),
                            ),
                            Row::Empty,
                        ),
                    ),
                )),
                var(a),
            )
        }

        // ---- Serialization / Deserialization ----
        BuiltinId::SerdeFromJson => {
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::Proto,
                    wit_t(a),
                    ext_l(BuiltinLabel::Json, STR, Row::Empty),
                )),
                variant(ext_t(
                    BuiltinTag::Ok,
                    var(a),
                    ext_t(BuiltinTag::Err, STR, Row::Empty),
                )),
            )
        }
        BuiltinId::SerdeToJson => {
            let a = TypeVar::new(Kind::Type);
            func(
                var(a),
                variant(ext_t(
                    BuiltinTag::Ok,
                    STR,
                    ext_t(BuiltinTag::Err, STR, Row::Empty),
                )),
            )
        }

        // ==========================================
        // ---- String Management ----
        // ==========================================
        BuiltinId::StrLength => func(STR, NUM),
        BuiltinId::StrIsEmpty => func(STR, BOOL),
        BuiltinId::StrReverse => func(STR, STR),
        BuiltinId::StrFirst => func(
            STR,
            variant(ext_t(
                BuiltinTag::Some,
                CHAR,
                ext_t(BuiltinTag::None, UNIT, Row::Empty),
            )),
        ),
        BuiltinId::StrLast => func(
            STR,
            variant(ext_t(
                BuiltinTag::Some,
                CHAR,
                ext_t(BuiltinTag::None, UNIT, Row::Empty),
            )),
        ),
        BuiltinId::StrContains => func(
            rec(ext_l(
                BuiltinLabel::Str,
                STR,
                ext_l(BuiltinLabel::Value, CHAR, Row::Empty),
            )),
            BOOL,
        ),
        BuiltinId::StrAt => func(
            rec(ext_l(
                BuiltinLabel::Str,
                STR,
                ext_l(BuiltinLabel::Index, NUM, Row::Empty),
            )),
            variant(ext_t(
                BuiltinTag::Some,
                CHAR,
                ext_t(BuiltinTag::None, UNIT, Row::Empty),
            )),
        ),
        BuiltinId::StrPrepend => func(
            rec(ext_l(
                BuiltinLabel::Head,
                CHAR,
                ext_l(BuiltinLabel::Tail, STR, Row::Empty),
            )),
            STR,
        ),
        BuiltinId::StrAppend => func(
            rec(ext_l(
                BuiltinLabel::Head,
                STR,
                ext_l(BuiltinLabel::Tail, CHAR, Row::Empty),
            )),
            STR,
        ),
        BuiltinId::StrConcat => func(
            rec(ext_l(
                BuiltinLabel::Head,
                STR,
                ext_l(BuiltinLabel::Tail, STR, Row::Empty),
            )),
            STR,
        ),
        BuiltinId::StrRec => {
            let a = TypeVar::new(Kind::Type);
            func(
                rec(ext_l(
                    BuiltinLabel::Str,
                    STR,
                    ext_l(
                        BuiltinLabel::Base,
                        var(a),
                        ext_l(
                            BuiltinLabel::Step,
                            func(
                                rec(ext_l(
                                    BuiltinLabel::Acc,
                                    var(a),
                                    ext_l(BuiltinLabel::Head, CHAR, Row::Empty),
                                )),
                                var(a),
                            ),
                            Row::Empty,
                        ),
                    ),
                )),
                var(a),
            )
        }
    };

    mono.generalize(&[])
}
