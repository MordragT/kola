use crate::types;
use std::ops::ControlFlow;

pub trait TypeVisitor: Sized {
    type BreakValue;

    fn visit_primitive(
        &mut self,
        _primitive: &types::PrimitiveType,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_func(&mut self, func: &types::FuncType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&func.input)?;
        self.visit_comp(&func.output)?;
        ControlFlow::Continue(())
    }

    fn visit_comp(&mut self, comp: &types::CompType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&comp.ty)?;
        self.visit_row(&comp.effect)?;

        ControlFlow::Continue(())
    }

    fn visit_list(&mut self, list: &types::ListType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&list.0)?;
        ControlFlow::Continue(())
    }

    fn visit_record(&mut self, record: &types::RecordType) -> ControlFlow<Self::BreakValue> {
        self.visit_row(&record.0)?;
        ControlFlow::Continue(())
    }

    fn visit_variant(&mut self, variant: &types::VariantType) -> ControlFlow<Self::BreakValue> {
        self.visit_row(&variant.0)?;
        ControlFlow::Continue(())
    }

    fn visit_labeled(&mut self, property: &types::LabeledType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&property.ty)?;
        ControlFlow::Continue(())
    }

    fn visit_row(&mut self, row: &types::Row) -> ControlFlow<Self::BreakValue> {
        match row {
            types::Row::Empty => ControlFlow::Continue(()),
            types::Row::Var(var) => self.visit_var(var),
            types::Row::Extension { head, tail } => {
                self.visit_labeled(head)?;
                self.visit_row(tail)?;
                ControlFlow::Continue(())
            }
        }
    }

    fn visit_var(&mut self, _var: &types::TypeVar) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_label_or_var(
        &mut self,
        label_or_var: &types::LabelOrVar,
    ) -> ControlFlow<Self::BreakValue> {
        match label_or_var {
            types::LabelOrVar::Label(label) => self.visit_label(label),
            types::LabelOrVar::Var(var) => self.visit_var(var),
        }
    }

    fn visit_label(&mut self, _label: &types::Label) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_wit(&mut self, wit: &types::WitType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&wit.0)
    }

    fn visit_mono(&mut self, mono: &types::MonoType) -> ControlFlow<Self::BreakValue> {
        match mono {
            types::MonoType::Primitive(b) => self.visit_primitive(b),
            types::MonoType::Func(f) => self.visit_func(f),
            types::MonoType::List(l) => self.visit_list(l),
            types::MonoType::Record(r) => self.visit_record(r),
            types::MonoType::Variant(v) => self.visit_variant(v),
            types::MonoType::Var(v) => self.visit_var(v),
            types::MonoType::Row(r) => self.visit_row(r),
            types::MonoType::Label(l) => self.visit_label_or_var(l),
            types::MonoType::Wit(w) => self.visit_wit(w),
        }
    }
}

pub trait TypeVisitorMut: Sized {
    type BreakValue;

    fn visit_primitive_mut(
        &mut self,
        _primitive: &mut types::PrimitiveType,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_func_mut(&mut self, func: &mut types::FuncType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut func.input)?;
        self.visit_comp_mut(&mut func.output)?;
        ControlFlow::Continue(())
    }

    fn visit_comp_mut(&mut self, comp: &mut types::CompType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut comp.ty)?;
        self.visit_row_mut(&mut comp.effect)?;

        ControlFlow::Continue(())
    }

    fn visit_list_mut(&mut self, list: &mut types::ListType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut list.0)?;
        ControlFlow::Continue(())
    }

    fn visit_record_mut(
        &mut self,
        record: &mut types::RecordType,
    ) -> ControlFlow<Self::BreakValue> {
        self.visit_row_mut(&mut record.0)?;
        ControlFlow::Continue(())
    }

    fn visit_variant_mut(
        &mut self,
        variant: &mut types::VariantType,
    ) -> ControlFlow<Self::BreakValue> {
        self.visit_row_mut(&mut variant.0)?;
        ControlFlow::Continue(())
    }

    fn visit_labeled_mut(
        &mut self,
        property: &mut types::LabeledType,
    ) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut property.ty)?;
        ControlFlow::Continue(())
    }

    fn visit_row_mut(&mut self, row: &mut types::Row) -> ControlFlow<Self::BreakValue> {
        match row {
            types::Row::Empty => ControlFlow::Continue(()),
            types::Row::Var(var) => self.visit_var_mut(var),
            types::Row::Extension { head, tail } => {
                self.visit_labeled_mut(head)?;
                self.visit_row_mut(tail)?;
                ControlFlow::Continue(())
            }
        }
    }

    fn visit_var_mut(&mut self, _var: &mut types::TypeVar) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_label_or_var_mut(
        &mut self,
        label_or_var: &mut types::LabelOrVar,
    ) -> ControlFlow<Self::BreakValue> {
        match label_or_var {
            types::LabelOrVar::Label(label) => self.visit_label_mut(label),
            types::LabelOrVar::Var(var) => self.visit_var_mut(var),
        }
    }

    fn visit_label_mut(&mut self, _label: &mut types::Label) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_wit_mut(&mut self, wit: &mut types::WitType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut wit.0)
    }

    fn visit_mono_mut(&mut self, mono: &mut types::MonoType) -> ControlFlow<Self::BreakValue> {
        match mono {
            types::MonoType::Primitive(b) => self.visit_primitive_mut(b),
            types::MonoType::Func(f) => self.visit_func_mut(f),
            types::MonoType::List(l) => self.visit_list_mut(l),
            types::MonoType::Record(r) => self.visit_record_mut(r),
            types::MonoType::Variant(v) => self.visit_variant_mut(v),
            types::MonoType::Row(r) => self.visit_row_mut(r),
            types::MonoType::Var(v) => self.visit_var_mut(v),
            types::MonoType::Label(l) => self.visit_label_or_var_mut(l),
            types::MonoType::Wit(w) => self.visit_wit_mut(w),
        }
    }
}

pub trait TypeVisitable {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor;
    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut;
}

impl TypeVisitable for types::PrimitiveType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_primitive(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_primitive_mut(self)
    }
}

impl TypeVisitable for types::FuncType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_func(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_func_mut(self)
    }
}

impl TypeVisitable for types::CompType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_comp(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_comp_mut(self)
    }
}

impl TypeVisitable for types::ListType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_list(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_list_mut(self)
    }
}

impl TypeVisitable for types::RecordType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_record(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_record_mut(self)
    }
}

impl TypeVisitable for types::VariantType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_variant(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_variant_mut(self)
    }
}

impl TypeVisitable for types::MonoType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_mono(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_mono_mut(self)
    }
}

impl TypeVisitable for types::LabeledType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_labeled(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_labeled_mut(self)
    }
}

impl TypeVisitable for types::Row {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_row(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_row_mut(self)
    }
}

impl TypeVisitable for types::TypeVar {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_var(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_var_mut(self)
    }
}

impl TypeVisitable for types::LabelOrVar {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_label_or_var(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_label_or_var_mut(self)
    }
}

impl TypeVisitable for types::Label {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_label(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_label_mut(self)
    }
}

impl TypeVisitable for types::WitType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_wit(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_wit_mut(self)
    }
}
