use crate::{id::Id, instr::*, ir::IrView};

pub trait IrVisitor<Ir>
where
    Ir: IrView,
{
    type Error;

    // Symbol visitors
    fn visit_symbol(&mut self, _symbol: Symbol) -> Result<(), Self::Error> {
        Ok(())
    }

    // Atom visitors
    fn visit_atom(&mut self, _atom: Id<Atom>, _ir: &Ir) -> Result<(), Self::Error> {
        Ok(())
    }

    // Expression visitors
    fn walk_expr(&mut self, expr: Id<Expr>, ir: &Ir) -> Result<(), Self::Error> {
        match ir.instr(expr) {
            Expr::Ret(ret_expr) => self.visit_ret_expr(ret_expr, ir),
            Expr::Call(call_expr) => self.visit_call_expr(call_expr, ir),
            Expr::If(if_expr) => self.visit_if_expr(if_expr, ir),
            Expr::Let(let_expr) => self.visit_let_expr(let_expr, ir),
            Expr::Unary(unary_expr) => self.visit_unary_expr(unary_expr, ir),
            Expr::Binary(binary_expr) => self.visit_binary_expr(binary_expr, ir),
            Expr::List(list_expr) => self.visit_list_expr(list_expr, ir),
            Expr::Record(record_expr) => self.visit_record_expr(record_expr, ir),
            Expr::RecordExtend(record_extend_expr) => {
                self.visit_record_extend_expr(record_extend_expr, ir)
            }
            Expr::RecordRestrict(record_restrict_expr) => {
                self.visit_record_restrict_expr(record_restrict_expr, ir)
            }
            Expr::RecordUpdate(record_update_expr) => {
                self.visit_record_update_expr(record_update_expr, ir)
            }
            Expr::RecordAccess(record_access_expr) => {
                self.visit_record_access_expr(record_access_expr, ir)
            }
            Expr::PatternMatch(pattern_match_expr) => {
                self.visit_pattern_match_expr(pattern_match_expr, ir)
            }
        }
    }

    fn visit_expr(&mut self, expr: Id<Expr>, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_expr(expr, ir)
    }

    fn walk_ret_expr(&mut self, ret_expr: RetExpr, ir: &Ir) -> Result<(), Self::Error> {
        let RetExpr { arg } = ret_expr;
        self.visit_atom(arg, ir)
    }

    fn visit_ret_expr(&mut self, ret_expr: RetExpr, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_ret_expr(ret_expr, ir)
    }

    fn walk_call_expr(&mut self, call_expr: CallExpr, ir: &Ir) -> Result<(), Self::Error> {
        let CallExpr {
            bind,
            func,
            arg,
            next,
        } = call_expr;
        self.visit_symbol(bind)?;
        self.visit_atom(func, ir)?;
        self.visit_atom(arg, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_call_expr(&mut self, call_expr: CallExpr, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_call_expr(call_expr, ir)
    }

    fn walk_if_expr(&mut self, if_expr: IfExpr, ir: &Ir) -> Result<(), Self::Error> {
        let IfExpr {
            bind,
            predicate,
            then,
            or,
            next,
        } = if_expr;
        self.visit_symbol(bind)?;
        self.visit_atom(predicate, ir)?;
        self.visit_expr(then, ir)?;
        self.visit_expr(or, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_if_expr(&mut self, if_expr: IfExpr, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_if_expr(if_expr, ir)
    }

    fn walk_let_expr(&mut self, let_expr: LetExpr, ir: &Ir) -> Result<(), Self::Error> {
        let LetExpr { bind, value, next } = let_expr;
        self.visit_symbol(bind)?;
        self.visit_atom(value, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_let_expr(&mut self, let_expr: LetExpr, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_let_expr(let_expr, ir)
    }

    fn visit_unary_op(&mut self, _op: UnaryOp) -> Result<(), Self::Error> {
        Ok(())
    }

    fn walk_unary_expr(&mut self, unary_expr: UnaryExpr, ir: &Ir) -> Result<(), Self::Error> {
        let UnaryExpr {
            bind,
            op,
            arg,
            next,
        } = unary_expr;
        self.visit_symbol(bind)?;
        self.visit_unary_op(op)?;
        self.visit_atom(arg, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_unary_expr(&mut self, unary_expr: UnaryExpr, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_unary_expr(unary_expr, ir)
    }

    fn visit_binary_op(&mut self, _op: BinaryOp) -> Result<(), Self::Error> {
        Ok(())
    }

    fn walk_binary_expr(&mut self, binary_expr: BinaryExpr, ir: &Ir) -> Result<(), Self::Error> {
        let BinaryExpr {
            bind,
            op,
            lhs,
            rhs,
            next,
        } = binary_expr;
        self.visit_symbol(bind)?;
        self.visit_binary_op(op)?;
        self.visit_atom(lhs, ir)?;
        self.visit_atom(rhs, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_binary_expr(&mut self, binary_expr: BinaryExpr, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_binary_expr(binary_expr, ir)
    }

    fn walk_list_item(&mut self, list_item: Id<ListItem>, ir: &Ir) -> Result<(), Self::Error> {
        let ListItem { value, next } = ir.instr(list_item);
        self.visit_atom(value, ir)?;
        if let Some(next) = next {
            self.visit_list_item(next, ir)?;
        }
        Ok(())
    }

    fn visit_list_item(&mut self, list_item: Id<ListItem>, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_list_item(list_item, ir)
    }

    fn walk_list_expr(&mut self, list_expr: ListExpr, ir: &Ir) -> Result<(), Self::Error> {
        let ListExpr {
            bind,
            head,
            tail: _,
            next,
        } = list_expr;
        self.visit_symbol(bind)?;
        if let Some(head) = head {
            self.visit_list_item(head, ir)?;
        }
        self.visit_expr(next, ir)
    }

    fn visit_list_expr(&mut self, list_expr: ListExpr, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_list_expr(list_expr, ir)
    }

    fn walk_record_field(
        &mut self,
        record_field: Id<RecordField>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let RecordField { value, next, .. } = ir.instr(record_field);
        self.visit_atom(value, ir)?;
        if let Some(next) = next {
            self.visit_record_field(next, ir)?;
        }
        Ok(())
    }

    fn visit_record_field(
        &mut self,
        record_field: Id<RecordField>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_record_field(record_field, ir)
    }

    fn walk_record_expr(&mut self, record_expr: RecordExpr, ir: &Ir) -> Result<(), Self::Error> {
        let RecordExpr { bind, head, next } = record_expr;
        self.visit_symbol(bind)?;
        if let Some(head) = head {
            self.visit_record_field(head, ir)?;
        }
        self.visit_expr(next, ir)
    }

    fn visit_record_expr(&mut self, record_expr: RecordExpr, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_record_expr(record_expr, ir)
    }

    fn walk_field_path(&mut self, field_path: Id<FieldPath>, ir: &Ir) -> Result<(), Self::Error> {
        let FieldPath { next, .. } = ir.instr(field_path);
        if let Some(next) = next {
            self.visit_field_path(next, ir)?;
        }
        Ok(())
    }

    fn visit_field_path(&mut self, field_path: Id<FieldPath>, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_field_path(field_path, ir)
    }

    fn walk_record_extend_expr(
        &mut self,
        record_extend_expr: RecordExtendExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let RecordExtendExpr {
            bind,
            base,
            path,
            value,
            next,
        } = record_extend_expr;
        self.visit_symbol(bind)?;
        self.visit_atom(base, ir)?;
        self.visit_field_path(path, ir)?;
        self.visit_atom(value, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_record_extend_expr(
        &mut self,
        record_extend_expr: RecordExtendExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_record_extend_expr(record_extend_expr, ir)
    }

    fn walk_record_restrict_expr(
        &mut self,
        record_restrict_expr: RecordRestrictExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let RecordRestrictExpr {
            bind,
            base,
            path,
            next,
        } = record_restrict_expr;
        self.visit_symbol(bind)?;
        self.visit_atom(base, ir)?;
        self.visit_field_path(path, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_record_restrict_expr(
        &mut self,
        record_restrict_expr: RecordRestrictExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_record_restrict_expr(record_restrict_expr, ir)
    }

    fn visit_record_update_op(&mut self, _op: RecordUpdateOp) -> Result<(), Self::Error> {
        Ok(())
    }

    fn walk_record_update_expr(
        &mut self,
        record_update_expr: RecordUpdateExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let RecordUpdateExpr {
            bind,
            base,
            path,
            op,
            value,
            next,
        } = record_update_expr;
        self.visit_symbol(bind)?;
        self.visit_atom(base, ir)?;
        self.visit_field_path(path, ir)?;
        self.visit_record_update_op(op)?;
        self.visit_atom(value, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_record_update_expr(
        &mut self,
        record_update_expr: RecordUpdateExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_record_update_expr(record_update_expr, ir)
    }

    fn walk_record_access_expr(
        &mut self,
        record_access_expr: RecordAccessExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let RecordAccessExpr {
            bind, base, next, ..
        } = record_access_expr;
        self.visit_symbol(bind)?;
        self.visit_atom(base, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_record_access_expr(
        &mut self,
        record_access_expr: RecordAccessExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_record_access_expr(record_access_expr, ir)
    }

    fn walk_pattern_match_expr(
        &mut self,
        pattern_match_expr: PatternMatchExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let PatternMatchExpr {
            bind,
            source,
            matcher,
            next,
        } = pattern_match_expr;
        self.visit_symbol(bind)?;
        self.visit_atom(source, ir)?;
        self.visit_pattern_matcher(matcher, ir)?;
        self.visit_expr(next, ir)
    }

    fn visit_pattern_match_expr(
        &mut self,
        pattern_match_expr: PatternMatchExpr,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_pattern_match_expr(pattern_match_expr, ir)
    }

    fn walk_pattern_matcher(
        &mut self,
        matcher: Id<PatternMatcher>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        match ir.instr(matcher) {
            PatternMatcher::IsUnit(is_unit) => self.visit_is_unit(is_unit, ir),
            PatternMatcher::IsBool(is_bool) => self.visit_is_bool(is_bool, ir),
            PatternMatcher::IsNum(is_num) => self.visit_is_num(is_num, ir),
            PatternMatcher::IsChar(is_char) => self.visit_is_char(is_char, ir),
            PatternMatcher::IsStr(is_str) => self.visit_is_str(is_str, ir),
            PatternMatcher::IsTag(is_tag) => self.visit_is_tag(is_tag, ir),
            PatternMatcher::IsVariant(is_variant) => self.visit_is_variant(is_variant, ir),
            PatternMatcher::IsList(is_list) => self.visit_is_list(is_list, ir),
            PatternMatcher::ListIsExact(is_exact) => self.visit_list_is_exact(is_exact, ir),
            PatternMatcher::ListIsAtLeast(is_at_least) => {
                self.visit_list_is_at_least(is_at_least, ir)
            }
            PatternMatcher::IsRecord(is_record) => self.visit_is_record(is_record, ir),
            PatternMatcher::RecordHasField(has_field) => self.visit_record_has_field(has_field, ir),
            PatternMatcher::Identity(identity) => self.visit_identity(identity, ir),
            PatternMatcher::ListSplitHead(split_head) => self.visit_list_split_head(split_head, ir),
            PatternMatcher::ListSplitTail(split_tail) => self.visit_list_split_tail(split_tail, ir),
            PatternMatcher::ListGetAt(get_at) => self.visit_list_get_at(get_at, ir),
            PatternMatcher::ListSplitAt(split_at) => self.visit_list_split_at(split_at, ir),
            PatternMatcher::RecordGetAt(get_at) => self.visit_record_get_at(get_at, ir),
            // PatternMatcher::ExtractRecordWithoutField(extract_record_without_field) => {
            //     self.visit_extract_record_without_field(extract_record_without_field, ir)
            // }
            // PatternMatcher::ExtractVariantTag(extract_variant_tag) => {
            //     self.visit_extract_variant_tag(extract_variant_tag, ir)
            // }
            // PatternMatcher::ExtractVariantValue(extract_variant_value) => {
            //     self.visit_extract_variant_value(extract_variant_value, ir)
            // }
            PatternMatcher::Success(success) => self.visit_pattern_success(success, ir),
            PatternMatcher::Failure(failure) => self.visit_pattern_failure(failure, ir),
        }
    }

    fn visit_pattern_matcher(
        &mut self,
        matcher: Id<PatternMatcher>,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_pattern_matcher(matcher, ir)
    }

    fn walk_is_unit(&mut self, is_unit: IsUnit, ir: &Ir) -> Result<(), Self::Error> {
        let IsUnit {
            source,
            on_success,
            on_failure,
        } = is_unit;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_is_unit(&mut self, is_unit: IsUnit, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_is_unit(is_unit, ir)
    }

    fn walk_is_bool(&mut self, is_bool: IsBool, ir: &Ir) -> Result<(), Self::Error> {
        let IsBool {
            source,
            on_success,
            on_failure,
            ..
        } = is_bool;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_is_bool(&mut self, is_bool: IsBool, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_is_bool(is_bool, ir)
    }

    fn walk_is_num(&mut self, is_num: IsNum, ir: &Ir) -> Result<(), Self::Error> {
        let IsNum {
            source,
            on_success,
            on_failure,
            ..
        } = is_num;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_is_num(&mut self, is_num: IsNum, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_is_num(is_num, ir)
    }

    fn walk_is_char(&mut self, is_char: IsChar, ir: &Ir) -> Result<(), Self::Error> {
        let IsChar {
            source,
            on_success,
            on_failure,
            ..
        } = is_char;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_is_char(&mut self, is_char: IsChar, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_is_char(is_char, ir)
    }

    fn walk_is_str(&mut self, is_str: IsStr, ir: &Ir) -> Result<(), Self::Error> {
        let IsStr {
            source,
            on_success,
            on_failure,
            ..
        } = is_str;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_is_str(&mut self, is_str: IsStr, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_is_str(is_str, ir)
    }

    fn walk_is_tag(&mut self, is_tag: IsTag, ir: &Ir) -> Result<(), Self::Error> {
        let IsTag {
            source,
            on_success,
            on_failure,
            ..
        } = is_tag;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_is_tag(&mut self, is_tag: IsTag, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_is_tag(is_tag, ir)
    }

    fn walk_is_variant(&mut self, is_variant: IsVariant, ir: &Ir) -> Result<(), Self::Error> {
        let IsVariant {
            source,
            on_success,
            on_failure,
            ..
        } = is_variant;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_is_variant(&mut self, is_variant: IsVariant, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_is_variant(is_variant, ir)
    }

    fn walk_is_list(&mut self, is_list: IsList, ir: &Ir) -> Result<(), Self::Error> {
        let IsList {
            source,
            on_success,
            on_failure,
        } = is_list;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_is_list(&mut self, is_list: IsList, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_is_list(is_list, ir)
    }

    fn walk_list_is_exact(
        &mut self,
        list_is_exact: ListIsExact,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let ListIsExact {
            source,
            on_success,
            on_failure,
            ..
        } = list_is_exact;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_list_is_exact(
        &mut self,
        list_is_exact: ListIsExact,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_list_is_exact(list_is_exact, ir)
    }

    fn walk_list_is_at_least(
        &mut self,
        list_is_at_least: ListIsAtLeast,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let ListIsAtLeast {
            source,
            on_success,
            on_failure,
            ..
        } = list_is_at_least;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_list_is_at_least(
        &mut self,
        list_is_at_least: ListIsAtLeast,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_list_is_at_least(list_is_at_least, ir)
    }

    fn walk_is_record(&mut self, is_record: IsRecord, ir: &Ir) -> Result<(), Self::Error> {
        let IsRecord {
            source,
            on_success,
            on_failure,
        } = is_record;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_is_record(&mut self, is_record: IsRecord, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_is_record(is_record, ir)
    }

    fn walk_record_has_field(
        &mut self,
        record_has_field: RecordHasField,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let RecordHasField {
            source,
            on_success,
            on_failure,
            ..
        } = record_has_field;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(on_success, ir)?;
        self.visit_pattern_matcher(on_failure, ir)
    }

    fn visit_record_has_field(
        &mut self,
        record_has_field: RecordHasField,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_record_has_field(record_has_field, ir)
    }

    fn walk_identity(&mut self, identity: Identity, ir: &Ir) -> Result<(), Self::Error> {
        let Identity { bind, source, next } = identity;
        self.visit_symbol(bind)?;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(next, ir)
    }

    fn visit_identity(&mut self, identity: Identity, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_identity(identity, ir)
    }

    fn walk_list_split_head(
        &mut self,
        list_split_head: ListSplitHead,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let ListSplitHead {
            head,
            tail_list,
            source,
            next,
        } = list_split_head;
        self.visit_symbol(head)?;
        self.visit_symbol(tail_list)?;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(next, ir)
    }

    fn visit_list_split_head(
        &mut self,
        list_split_head: ListSplitHead,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_list_split_head(list_split_head, ir)
    }

    fn walk_list_split_tail(
        &mut self,
        list_split_tail: ListSplitTail,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let ListSplitTail {
            head_list,
            tail,
            source,
            next,
        } = list_split_tail;
        self.visit_symbol(head_list)?;
        self.visit_symbol(tail)?;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(next, ir)
    }

    fn visit_list_split_tail(
        &mut self,
        list_split_tail: ListSplitTail,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_list_split_tail(list_split_tail, ir)
    }

    fn walk_list_get_at(&mut self, list_get_at: ListGetAt, ir: &Ir) -> Result<(), Self::Error> {
        let ListGetAt {
            bind, source, next, ..
        } = list_get_at;
        self.visit_symbol(bind)?;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(next, ir)
    }

    fn visit_list_get_at(&mut self, list_get_at: ListGetAt, ir: &Ir) -> Result<(), Self::Error> {
        self.walk_list_get_at(list_get_at, ir)
    }

    fn walk_list_split_at(
        &mut self,
        list_split_at: ListSplitAt,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let ListSplitAt {
            head,
            tail,
            source,
            next,
            ..
        } = list_split_at;
        self.visit_symbol(head)?;
        self.visit_symbol(tail)?;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(next, ir)
    }

    fn visit_list_split_at(
        &mut self,
        list_split_at: ListSplitAt,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_list_split_at(list_split_at, ir)
    }

    fn walk_record_get_at(
        &mut self,
        record_get_at: RecordGetAt,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let RecordGetAt {
            bind, source, next, ..
        } = record_get_at;
        self.visit_symbol(bind)?;
        self.visit_symbol(source)?;
        self.visit_pattern_matcher(next, ir)
    }

    fn visit_record_get_at(
        &mut self,
        record_get_at: RecordGetAt,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_record_get_at(record_get_at, ir)
    }

    // fn walk_extract_record_without_field(
    //     &mut self,
    //     extract_record_without_field: ExtractRecordWithoutField,
    //     ir: &Ir,
    // ) -> Result<(), Self::Error> {
    //     let ExtractRecordWithoutField {
    //         bind, source, next, ..
    //     } = extract_record_without_field;
    //     self.visit_symbol(bind)?;
    //     self.visit_symbol(source)?;
    //     self.visit_pattern_matcher(next, ir)
    // }

    // fn visit_extract_record_without_field(
    //     &mut self,
    //     extract_record_without_field: ExtractRecordWithoutField,
    //     ir: &Ir,
    // ) -> Result<(), Self::Error> {
    //     self.walk_extract_record_without_field(extract_record_without_field, ir)
    // }

    // fn walk_extract_variant_tag(
    //     &mut self,
    //     extract_variant_tag: ExtractVariantTag,
    //     ir: &Ir,
    // ) -> Result<(), Self::Error> {
    //     let ExtractVariantTag {
    //         bind, source, next, ..
    //     } = extract_variant_tag;
    //     self.visit_symbol(bind)?;
    //     self.visit_symbol(source)?;
    //     self.visit_pattern_matcher(next, ir)
    // }

    // fn visit_extract_variant_tag(
    //     &mut self,
    //     extract_variant_tag: ExtractVariantTag,
    //     ir: &Ir,
    // ) -> Result<(), Self::Error> {
    //     self.walk_extract_variant_tag(extract_variant_tag, ir)
    // }

    // fn walk_extract_variant_value(
    //     &mut self,
    //     extract_variant_value: ExtractVariantValue,
    //     ir: &Ir,
    // ) -> Result<(), Self::Error> {
    //     let ExtractVariantValue {
    //         bind, source, next, ..
    //     } = extract_variant_value;
    //     self.visit_symbol(bind)?;
    //     self.visit_symbol(source)?;
    //     self.visit_pattern_matcher(next, ir)
    // }

    // fn visit_extract_variant_value(
    //     &mut self,
    //     extract_variant_value: ExtractVariantValue,
    //     ir: &Ir,
    // ) -> Result<(), Self::Error> {
    //     self.walk_extract_variant_value(extract_variant_value, ir)
    // }

    fn walk_pattern_success(
        &mut self,
        pattern_success: PatternSuccess,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        let PatternSuccess { next } = pattern_success;
        self.visit_expr(next, ir)
    }

    fn visit_pattern_success(
        &mut self,
        pattern_success: PatternSuccess,
        ir: &Ir,
    ) -> Result<(), Self::Error> {
        self.walk_pattern_success(pattern_success, ir)
    }

    fn visit_pattern_failure(
        &mut self,
        _pattern_failure: PatternFailure,
        _ir: &Ir,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}
