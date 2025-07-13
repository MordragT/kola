use std::ops::ControlFlow;

pub trait TypeVisitor: Sized {
    type BreakValue;

    fn visit_primitive(
        &mut self,
        _primitive: &super::PrimitiveType,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_func(&mut self, func: &super::FuncType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&func.input)?;
        self.visit_comp(&func.output)?;
        ControlFlow::Continue(())
    }

    fn visit_comp(&mut self, comp: &super::CompType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&comp.ty)?;
        self.visit_row(&comp.effect)?;

        ControlFlow::Continue(())
    }

    fn visit_list(&mut self, list: &super::ListType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&list.0)?;
        ControlFlow::Continue(())
    }

    fn visit_record(&mut self, record: &super::RecordType) -> ControlFlow<Self::BreakValue> {
        self.visit_row(&record.0)?;
        ControlFlow::Continue(())
    }

    fn visit_variant(&mut self, variant: &super::VariantType) -> ControlFlow<Self::BreakValue> {
        self.visit_row(&variant.0)?;
        ControlFlow::Continue(())
    }

    fn visit_labeled(&mut self, property: &super::LabeledType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&property.ty)?;
        ControlFlow::Continue(())
    }

    fn visit_row(&mut self, row: &super::Row) -> ControlFlow<Self::BreakValue> {
        match row {
            super::Row::Empty => ControlFlow::Continue(()),
            super::Row::Var(var) => self.visit_var(var),
            super::Row::Extension { head, tail } => {
                self.visit_labeled(head)?;
                self.visit_row(tail)?;
                ControlFlow::Continue(())
            }
        }
    }

    fn visit_var(&mut self, _var: &super::TypeVar) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_label_or_var(
        &mut self,
        label_or_var: &super::LabelOrVar,
    ) -> ControlFlow<Self::BreakValue> {
        match label_or_var {
            super::LabelOrVar::Label(label) => self.visit_label(label),
            super::LabelOrVar::Var(var) => self.visit_var(var),
        }
    }

    fn visit_label(&mut self, _label: &super::Label) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_wit(&mut self, wit: &super::WitType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&wit.0)
    }

    fn visit_mono(&mut self, mono: &super::MonoType) -> ControlFlow<Self::BreakValue> {
        match mono {
            super::MonoType::Primitive(b) => self.visit_primitive(b),
            super::MonoType::Func(f) => self.visit_func(f),
            super::MonoType::List(l) => self.visit_list(l),
            super::MonoType::Record(r) => self.visit_record(r),
            super::MonoType::Variant(v) => self.visit_variant(v),
            super::MonoType::Var(v) => self.visit_var(v),
            super::MonoType::Row(r) => self.visit_row(r),
            super::MonoType::Label(l) => self.visit_label_or_var(l),
            super::MonoType::Wit(w) => self.visit_wit(w),
        }
    }
}

pub trait TypeVisitorMut: Sized {
    type BreakValue;

    fn visit_primitive_mut(
        &mut self,
        _primitive: &mut super::PrimitiveType,
    ) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_func_mut(&mut self, func: &mut super::FuncType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut func.input)?;
        self.visit_comp_mut(&mut func.output)?;
        ControlFlow::Continue(())
    }

    fn visit_comp_mut(&mut self, comp: &mut super::CompType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut comp.ty)?;
        self.visit_row_mut(&mut comp.effect)?;

        ControlFlow::Continue(())
    }

    fn visit_list_mut(&mut self, list: &mut super::ListType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut list.0)?;
        ControlFlow::Continue(())
    }

    fn visit_record_mut(
        &mut self,
        record: &mut super::RecordType,
    ) -> ControlFlow<Self::BreakValue> {
        self.visit_row_mut(&mut record.0)?;
        ControlFlow::Continue(())
    }

    fn visit_variant_mut(
        &mut self,
        variant: &mut super::VariantType,
    ) -> ControlFlow<Self::BreakValue> {
        self.visit_row_mut(&mut variant.0)?;
        ControlFlow::Continue(())
    }

    fn visit_labeled_mut(
        &mut self,
        property: &mut super::LabeledType,
    ) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut property.ty)?;
        ControlFlow::Continue(())
    }

    fn visit_row_mut(&mut self, row: &mut super::Row) -> ControlFlow<Self::BreakValue> {
        match row {
            super::Row::Empty => ControlFlow::Continue(()),
            super::Row::Var(var) => self.visit_var_mut(var),
            super::Row::Extension { head, tail } => {
                self.visit_labeled_mut(head)?;
                self.visit_row_mut(tail)?;
                ControlFlow::Continue(())
            }
        }
    }

    fn visit_var_mut(&mut self, _var: &mut super::TypeVar) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_label_or_var_mut(
        &mut self,
        label_or_var: &mut super::LabelOrVar,
    ) -> ControlFlow<Self::BreakValue> {
        match label_or_var {
            super::LabelOrVar::Label(label) => self.visit_label_mut(label),
            super::LabelOrVar::Var(var) => self.visit_var_mut(var),
        }
    }

    fn visit_label_mut(&mut self, _label: &mut super::Label) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_wit_mut(&mut self, wit: &mut super::WitType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut wit.0)
    }

    fn visit_mono_mut(&mut self, mono: &mut super::MonoType) -> ControlFlow<Self::BreakValue> {
        match mono {
            super::MonoType::Primitive(b) => self.visit_primitive_mut(b),
            super::MonoType::Func(f) => self.visit_func_mut(f),
            super::MonoType::List(l) => self.visit_list_mut(l),
            super::MonoType::Record(r) => self.visit_record_mut(r),
            super::MonoType::Variant(v) => self.visit_variant_mut(v),
            super::MonoType::Row(r) => self.visit_row_mut(r),
            super::MonoType::Var(v) => self.visit_var_mut(v),
            super::MonoType::Label(l) => self.visit_label_or_var_mut(l),
            super::MonoType::Wit(w) => self.visit_wit_mut(w),
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

impl TypeVisitable for super::PrimitiveType {
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

impl TypeVisitable for super::FuncType {
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

impl TypeVisitable for super::CompType {
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

impl TypeVisitable for super::ListType {
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

impl TypeVisitable for super::RecordType {
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

impl TypeVisitable for super::VariantType {
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

impl TypeVisitable for super::MonoType {
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

impl TypeVisitable for super::LabeledType {
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

impl TypeVisitable for super::Row {
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

impl TypeVisitable for super::TypeVar {
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

impl TypeVisitable for super::LabelOrVar {
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

impl TypeVisitable for super::Label {
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

impl TypeVisitable for super::WitType {
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
