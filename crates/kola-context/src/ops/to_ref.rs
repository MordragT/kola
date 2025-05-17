use crate::{Ctx, Empty};

pub trait ToRef {
    type Ref<'a>
    where
        Self: 'a;
    type Mut<'a>
    where
        Self: 'a;

    fn to_ref(&self) -> Self::Ref<'_>;
    fn to_mut(&mut self) -> Self::Mut<'_>;
}

impl ToRef for Empty {
    type Ref<'a> = Self;
    type Mut<'a> = Self;

    fn to_ref(&self) -> Self::Ref<'_> {
        *self
    }

    fn to_mut(&mut self) -> Self::Mut<'_> {
        *self
    }
}

impl<Head, Tail> ToRef for Ctx<Head, Tail>
where
    Tail: ToRef,
{
    type Ref<'a>
        = Ctx<&'a Head, Tail::Ref<'a>>
    where
        Self: 'a;
    type Mut<'a>
        = Ctx<&'a mut Head, Tail::Mut<'a>>
    where
        Self: 'a;

    fn to_ref(&self) -> Self::Ref<'_> {
        let Ctx(head, tail) = self;
        Ctx(head, tail.to_ref())
    }

    fn to_mut(&mut self) -> Self::Mut<'_> {
        let Ctx(head, tail) = self;
        Ctx(head, tail.to_mut())
    }
}
