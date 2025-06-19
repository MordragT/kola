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
        self.visit_mono(&func.output)?;
        ControlFlow::Continue(())
    }

    fn visit_list(&mut self, list: &super::ListType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&list.el)?;
        ControlFlow::Continue(())
    }

    fn visit_mono(&mut self, mono: &super::MonoType) -> ControlFlow<Self::BreakValue> {
        match mono {
            super::MonoType::Builtin(b) => self.visit_primitive(b),
            super::MonoType::Func(f) => self.visit_func(f),
            super::MonoType::List(l) => self.visit_list(l),
            super::MonoType::Row(r) => self.visit_record(r),
            super::MonoType::Var(v) => self.visit_var(v),
        }
    }

    fn visit_property(&mut self, property: &super::LabeledType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono(&property.ty)?;
        ControlFlow::Continue(())
    }

    fn visit_record(&mut self, record: &super::RowType) -> ControlFlow<Self::BreakValue> {
        match record {
            super::RowType::Empty => ControlFlow::Continue(()),
            super::RowType::Extension { head, tail } => {
                self.visit_property(head)?;
                self.visit_mono(tail)?;
                ControlFlow::Continue(())
            }
        }
    }

    fn visit_var(&mut self, _var: &super::TypeVar) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
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
        self.visit_mono_mut(&mut func.output)?;
        ControlFlow::Continue(())
    }

    fn visit_list_mut(&mut self, list: &mut super::ListType) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut list.el)?;
        ControlFlow::Continue(())
    }

    fn visit_mono_mut(&mut self, mono: &mut super::MonoType) -> ControlFlow<Self::BreakValue> {
        match mono {
            super::MonoType::Builtin(b) => self.visit_primitive_mut(b),
            super::MonoType::Func(f) => self.visit_func_mut(f),
            super::MonoType::List(l) => self.visit_list_mut(l),
            super::MonoType::Row(r) => self.visit_record_mut(r),
            super::MonoType::Var(v) => self.visit_var_mut(v),
        }
    }

    fn visit_property_mut(
        &mut self,
        property: &mut super::LabeledType,
    ) -> ControlFlow<Self::BreakValue> {
        self.visit_mono_mut(&mut property.ty)?;
        ControlFlow::Continue(())
    }

    fn visit_record_mut(&mut self, record: &mut super::RowType) -> ControlFlow<Self::BreakValue> {
        match record {
            super::RowType::Empty => ControlFlow::Continue(()),
            super::RowType::Extension { head, tail } => {
                self.visit_property_mut(head)?;
                self.visit_mono_mut(tail)?;
                ControlFlow::Continue(())
            }
        }
    }

    fn visit_var_mut(&mut self, _var: &mut super::TypeVar) -> ControlFlow<Self::BreakValue> {
        ControlFlow::Continue(())
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
        visitor.visit_record(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_record_mut(self)
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
