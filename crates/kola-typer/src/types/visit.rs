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
        self.visit_mono(&list.el)?;
        ControlFlow::Continue(())
    }

    fn visit_property(&mut self, property: &super::LabeledType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&property.ty)?;
        ControlFlow::Continue(())
    }

    fn visit_row(&mut self, record: &super::RowType) -> ControlFlow<Self::BreakValue> {
        match record {
            super::RowType::Empty => ControlFlow::Continue(()),
            super::RowType::Extension { head, tail } => {
                self.visit_property(head)?;
                self.visit_mono(tail)?;
                ControlFlow::Continue(())
            }
        }
    }

    fn visit_type_rep(&mut self, rep: &super::TypeRep) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&rep.ty)
    }

    fn visit_var(&mut self, _var: &super::TypeVar) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_mono(&mut self, mono: &super::MonoType) -> ControlFlow<Self::BreakValue> {
        match mono {
            super::MonoType::Primitive(b) => self.visit_primitive(b),
            super::MonoType::Func(f) => self.visit_func(f),
            super::MonoType::List(l) => self.visit_list(l),
            super::MonoType::Row(r) => self.visit_row(r),
            super::MonoType::TypeRep(rep) => self.visit_type_rep(rep),
            super::MonoType::Var(v) => self.visit_var(v),
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
        self.visit_mono_mut(&mut list.el)?;
        ControlFlow::Continue(())
    }

    fn visit_property_mut(
        &mut self,
        property: &mut super::LabeledType,
    ) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut property.ty)?;
        ControlFlow::Continue(())
    }

    fn visit_row_mut(&mut self, record: &mut super::RowType) -> ControlFlow<Self::BreakValue> {
        match record {
            super::RowType::Empty => ControlFlow::Continue(()),
            super::RowType::Extension { head, tail } => {
                self.visit_property_mut(head)?;
                self.visit_mono_mut(tail)?;
                ControlFlow::Continue(())
            }
        }
    }

    fn visit_type_rep_mut(&mut self, rep: &mut super::TypeRep) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut rep.ty)
    }

    fn visit_var_mut(&mut self, _var: &mut super::TypeVar) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
    }

    fn visit_mono_mut(&mut self, mono: &mut super::MonoType) -> ControlFlow<Self::BreakValue> {
        match mono {
            super::MonoType::Primitive(b) => self.visit_primitive_mut(b),
            super::MonoType::Func(f) => self.visit_func_mut(f),
            super::MonoType::List(l) => self.visit_list_mut(l),
            super::MonoType::Row(r) => self.visit_row_mut(r),
            super::MonoType::TypeRep(rep) => self.visit_type_rep_mut(rep),
            super::MonoType::Var(v) => self.visit_var_mut(v),
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
        visitor.visit_property(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_property_mut(self)
    }
}

impl TypeVisitable for super::RowType {
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

impl TypeVisitable for super::TypeRep {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_type_rep(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_type_rep_mut(self)
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
