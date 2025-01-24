use std::ops::ControlFlow;

pub trait TypeVisitor: Sized {
    type BreakValue;

    fn visit_builtin(&mut self, builtin: &super::BuiltinType) -> ControlFlow<Self::BreakValue> {
        walk_builtin(self, builtin)
    }

    fn visit_func(&mut self, func: &super::FuncType) -> ControlFlow<Self::BreakValue> {
        walk_func(self, func)
    }

    fn visit_property(&mut self, property: &super::Property) -> ControlFlow<Self::BreakValue> {
        walk_property(self, property)
    }

    fn visit_mono(&mut self, mono: &super::MonoType) -> ControlFlow<Self::BreakValue> {
        walk_mono(self, mono)
    }

    fn visit_record(&mut self, record: &super::RowType) -> ControlFlow<Self::BreakValue> {
        walk_record(self, record)
    }

    fn visit_var(&mut self, var: &super::TypeVar) -> ControlFlow<Self::BreakValue> {
        walk_var(self, var)
    }
}

pub trait TypeVisitorMut: Sized {
    type BreakValue;

    fn visit_builtin_mut(
        &mut self,
        builtin: &mut super::BuiltinType,
    ) -> ControlFlow<Self::BreakValue> {
        walk_builtin_mut(self, builtin)
    }

    fn visit_func_mut(&mut self, func: &mut super::FuncType) -> ControlFlow<Self::BreakValue> {
        walk_func_mut(self, func)
    }

    fn visit_property_mut(
        &mut self,
        property: &mut super::Property,
    ) -> ControlFlow<Self::BreakValue> {
        walk_property_mut(self, property)
    }

    fn visit_mono_mut(&mut self, mono: &mut super::MonoType) -> ControlFlow<Self::BreakValue> {
        walk_mono_mut(self, mono)
    }

    fn visit_record_mut(&mut self, record: &mut super::RowType) -> ControlFlow<Self::BreakValue> {
        walk_record_mut(self, record)
    }

    fn visit_var_mut(&mut self, var: &mut super::TypeVar) -> ControlFlow<Self::BreakValue> {
        walk_var_mut(self, var)
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

// Builtin Type

impl TypeVisitable for super::BuiltinType {
    fn visit_type_by<V>(&self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitor,
    {
        visitor.visit_builtin(self)
    }

    fn visit_type_mut_by<V>(&mut self, visitor: &mut V) -> ControlFlow<V::BreakValue>
    where
        V: TypeVisitorMut,
    {
        visitor.visit_builtin_mut(self)
    }
}

pub fn walk_builtin<V>(
    _visitor: &mut V,
    _builtin: &super::BuiltinType,
) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitor,
{
    ControlFlow::Continue(())
}

pub fn walk_builtin_mut<V>(
    _visitor: &mut V,
    _builtin: &mut super::BuiltinType,
) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitorMut,
{
    ControlFlow::Continue(())
}

// Func Type

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

pub fn walk_func<V>(visitor: &mut V, func: &super::FuncType) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitor,
{
    visitor.visit_mono(&func.arg)?;
    visitor.visit_mono(&func.ret)?;
    ControlFlow::Continue(())
}

pub fn walk_func_mut<V>(visitor: &mut V, func: &mut super::FuncType) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitorMut,
{
    visitor.visit_mono_mut(&mut func.arg)?;
    visitor.visit_mono_mut(&mut func.ret)?;
    ControlFlow::Continue(())
}

// Property Type

impl TypeVisitable for super::Property {
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

pub fn walk_property<V>(visitor: &mut V, property: &super::Property) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitor,
{
    visitor.visit_mono(&property.v)?;
    ControlFlow::Continue(())
}

pub fn walk_property_mut<V>(
    visitor: &mut V,
    property: &mut super::Property,
) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitorMut,
{
    visitor.visit_mono_mut(&mut property.v)?;
    ControlFlow::Continue(())
}

// Mono Type

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

pub fn walk_mono<V>(visitor: &mut V, mono: &super::MonoType) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitor,
{
    match mono {
        super::MonoType::Builtin(b) => visitor.visit_builtin(b),
        super::MonoType::Func(f) => visitor.visit_func(f),
        super::MonoType::Row(r) => visitor.visit_record(r),
        super::MonoType::Var(v) => visitor.visit_var(v),
    }
}

pub fn walk_mono_mut<V>(visitor: &mut V, mono: &mut super::MonoType) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitorMut,
{
    match mono {
        super::MonoType::Builtin(b) => visitor.visit_builtin_mut(b),
        super::MonoType::Func(f) => visitor.visit_func_mut(f),
        super::MonoType::Row(r) => visitor.visit_record_mut(r),
        super::MonoType::Var(v) => visitor.visit_var_mut(v),
    }
}

// Record Type

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

pub fn walk_record<V>(visitor: &mut V, record: &super::RowType) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitor,
{
    match record {
        super::RowType::Empty => ControlFlow::Continue(()),
        super::RowType::Extension { head, tail } => {
            visitor.visit_property(head)?;
            visitor.visit_mono(tail)?;
            ControlFlow::Continue(())
        }
    }
}

pub fn walk_record_mut<V>(
    visitor: &mut V,
    record: &mut super::RowType,
) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitorMut,
{
    match record {
        super::RowType::Empty => ControlFlow::Continue(()),
        super::RowType::Extension { head, tail } => {
            visitor.visit_property_mut(head)?;
            visitor.visit_mono_mut(tail)?;
            ControlFlow::Continue(())
        }
    }
}

// TypeVar

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

pub fn walk_var<V>(_visitor: &mut V, _var: &super::TypeVar) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitor,
{
    ControlFlow::Continue(())
}

pub fn walk_var_mut<V>(_visitor: &mut V, _var: &mut super::TypeVar) -> ControlFlow<V::BreakValue>
where
    V: TypeVisitorMut,
{
    ControlFlow::Continue(())
}
