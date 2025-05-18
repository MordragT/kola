use crate::cons::{Cons, Nil};

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

impl ToRef for Nil {
    type Ref<'a> = Self;
    type Mut<'a> = Self;

    fn to_ref(&self) -> Self::Ref<'_> {
        *self
    }

    fn to_mut(&mut self) -> Self::Mut<'_> {
        *self
    }
}

impl<Head, Tail> ToRef for Cons<Head, Tail>
where
    Tail: ToRef,
{
    type Ref<'a>
        = Cons<&'a Head, Tail::Ref<'a>>
    where
        Self: 'a;
    type Mut<'a>
        = Cons<&'a mut Head, Tail::Mut<'a>>
    where
        Self: 'a;

    fn to_ref(&self) -> Self::Ref<'_> {
        let Cons(head, tail) = self;
        Cons(head, tail.to_ref())
    }

    fn to_mut(&mut self) -> Self::Mut<'_> {
        let Cons(head, tail) = self;
        Cons(head, tail.to_mut())
    }
}
