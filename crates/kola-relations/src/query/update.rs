use crate::Id;
use crate::presence::Presence;
use crate::query::{FilterQuery, Plan, Query, ScanOneQuery, Traversal};
use crate::relation::{SliceRelation, TotalRelationMut};
use crate::slice::SliceId;

pub trait RowFilter: Plan<Src = <Self as Plan>::Target> {
    fn matches(&self, value: &<Self as Plan>::Target) -> bool;
}

impl<T: 'static> RowFilter for ScanOneQuery<T> {
    #[inline]
    fn matches(&self, _: &T) -> bool {
        true
    }
}

impl<P: RowFilter, F: Fn(&P::Target) -> bool> RowFilter for FilterQuery<P, F> {
    #[inline]
    fn matches(&self, value: &P::Target) -> bool {
        self.plan.matches(value) && (self.predicate)(value)
    }
}

impl<P: RowFilter> Query<P> {
    #[inline]
    pub fn update_id<Ctx, F>(self, ctx: &mut Ctx, id: Id<P::Target>, mut f: F) -> usize
    where
        Ctx: TotalRelationMut<P::Target, P::Target>,
        F: FnMut(&mut P::Target),
    {
        let row = ctx.get_mut(id);
        if self.plan.matches(row) {
            f(row);
            1
        } else {
            0
        }
    }

    #[inline]
    pub fn update_all<Ctx, F>(self, ctx: &mut Ctx, mut f: F) -> usize
    where
        Ctx: TotalRelationMut<P::Target, P::Target>,
        F: FnMut(&mut P::Target),
    {
        let mut updated = 0;
        for (_, row) in ctx.rows_mut() {
            if self.plan.matches(row) {
                f(row);
                updated += 1;
            }
        }
        updated
    }

    #[inline]
    // TODO: remove once callers use `SliceRelationMut::get_slice_mut` directly.
    pub fn update_with_scratch<Ctx, F>(
        self,
        ctx: &mut Ctx,
        slice: SliceId<P::Target>,
        scratch: &mut Vec<Id<P::Target>>,
        mut f: F,
    ) -> usize
    where
        Ctx: SliceRelation<P::Target> + TotalRelationMut<P::Target, P::Target>,
        F: FnMut(&mut P::Target),
    {
        scratch.clear();
        scratch.extend(ctx.get_slice(slice).iter().copied());

        let mut updated = 0;
        for &id in scratch.iter() {
            let row = ctx.get_mut(id);
            if self.plan.matches(row) {
                f(row);
                updated += 1;
            }
        }
        updated
    }
}

impl<P: Plan> Query<P> {
    #[inline]
    pub fn select_ids<'ctx, Ctx, I>(
        self,
        ctx: &'ctx Ctx,
        ids: I,
        out: &mut Vec<Id<P::Target>>,
    ) -> usize
    where
        I: IntoIterator<Item = Id<P::Src>>,
        I::IntoIter: 'ctx,
        P: Traversal<'ctx, Ctx> + 'ctx,
        Ctx: 'ctx,
    {
        out.clear();
        for id in ids {
            if let Some((target_id, _)) =
                <P::Presence as Presence>::into_option(self.plan.execute(ctx, id))
            {
                out.push(target_id);
            }
        }
        out.len()
    }
}

#[inline]
pub fn update_ids<Ctx, T: 'static, F>(ctx: &mut Ctx, ids: &[Id<T>], mut f: F) -> usize
where
    Ctx: TotalRelationMut<T, T>,
    F: FnMut(&mut T),
{
    for &id in ids {
        f(ctx.get_mut(id));
    }
    ids.len()
}

#[inline]
pub fn update_split<'read, Read, Write, P, I, F>(
    query: Query<P>,
    read: &'read Read,
    write: &mut Write,
    ids: I,
    mut f: F,
) -> usize
where
    I: IntoIterator<Item = Id<P::Src>>,
    I::IntoIter: 'read,
    P: Traversal<'read, Read> + 'read,
    Write: TotalRelationMut<P::Target, P::Target>,
    F: FnMut(&mut P::Target),
{
    let mut updated = 0;
    for id in ids {
        if let Some((target_id, _)) =
            <P::Presence as Presence>::into_option(query.plan.execute(read, id))
        {
            f(write.get_mut(target_id));
            updated += 1;
        }
    }
    updated
}
