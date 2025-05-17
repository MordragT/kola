use crate::{
    Ctx,
    index::{Here, Index, There},
};

pub trait Get<T, I>
where
    I: Index,
{
    fn get(&self) -> &T;

    fn get_mut(&mut self) -> &mut T;
}

impl<Head, Tail> Get<Head, Here> for Ctx<Head, Tail> {
    fn get(&self) -> &Head {
        &self.0
    }

    fn get_mut(&mut self) -> &mut Head {
        &mut self.0
    }
}

impl<Head, Tail, FromTail, TailIndex> Get<FromTail, There<TailIndex>> for Ctx<Head, Tail>
where
    Tail: Get<FromTail, TailIndex> + ?Sized,
    TailIndex: Index,
{
    fn get(&self) -> &FromTail {
        self.1.get()
    }

    fn get_mut(&mut self) -> &mut FromTail {
        self.1.get_mut()
    }
}
