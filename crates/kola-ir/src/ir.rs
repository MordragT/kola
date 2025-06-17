use kola_utils::{
    convert::{TryAsMut, TryAsRef},
    interner::StrKey,
};

use crate::{
    id::Id,
    instr::{Atom, Expr, FieldPath, Instr, ListItem, RecordField},
};

pub trait IrView {
    fn instr<T>(&self, id: Id<T>) -> T
    where
        T: Copy,
        Instr: TryAsRef<T>;

    fn iter_fields(&self, head: Option<Id<RecordField>>) -> FieldIter<'_, Self> {
        FieldIter::new(self, head)
    }

    fn iter_items(&self, head: Option<Id<ListItem>>) -> ItemIter<'_, Self> {
        ItemIter::new(self, head)
    }

    fn iter_path(&self, head: Option<Id<FieldPath>>) -> PathIter<'_, Self> {
        PathIter::new(self, head)
    }
}

pub struct FieldIter<'a, T: IrView + ?Sized> {
    ir: &'a T,
    current: Option<Id<RecordField>>,
}

impl<'a, T: IrView + ?Sized> FieldIter<'a, T> {
    pub fn new(ir: &'a T, head: Option<Id<RecordField>>) -> Self {
        Self { ir, current: head }
    }
}

impl<'a, T: IrView + ?Sized> Iterator for FieldIter<'a, T> {
    type Item = RecordField;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(field_id) = self.current {
            let field = self.ir.instr(field_id);
            self.current = field.next;
            Some(field)
        } else {
            None
        }
    }
}

pub struct ItemIter<'a, T: IrView + ?Sized> {
    ir: &'a T,
    current: Option<Id<ListItem>>,
}

impl<'a, T: IrView + ?Sized> ItemIter<'a, T> {
    pub fn new(ir: &'a T, head: Option<Id<ListItem>>) -> Self {
        Self { ir, current: head }
    }
}

impl<'a, T: IrView + ?Sized> Iterator for ItemIter<'a, T> {
    type Item = ListItem;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item_id) = self.current {
            let item = self.ir.instr(item_id);
            self.current = item.next;
            Some(item)
        } else {
            None
        }
    }
}

pub struct PathIter<'a, T: IrView + ?Sized> {
    ir: &'a T,
    current: Option<Id<FieldPath>>,
}

impl<'a, T: IrView + ?Sized> PathIter<'a, T> {
    pub fn new(ir: &'a T, head: Option<Id<FieldPath>>) -> Self {
        Self { ir, current: head }
    }
}

impl<'a, T: IrView + ?Sized> Iterator for PathIter<'a, T> {
    type Item = FieldPath;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(path_id) = self.current {
            let path = self.ir.instr(path_id);
            self.current = path.next;
            Some(path)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct IrBuilder {
    instructions: Vec<Instr>,
}

impl IrBuilder {
    #[inline]
    pub fn new() -> Self {
        // Reserve slot 0 with a dummy instruction.
        let instructions = vec![Instr::Noop];

        Self { instructions }
    }

    #[inline]
    pub fn add<T>(&mut self, instr: T) -> Id<T>
    where
        T: Into<Instr>,
    {
        let id = self.instructions.len() as u32;

        let instr = instr.into();
        self.instructions.push(instr);

        Id::new(id)
    }

    #[inline]
    pub fn instr_mut<T>(&mut self, id: Id<T>) -> &mut T
    where
        Instr: TryAsMut<T>,
    {
        let instr = &mut self.instructions[id.as_usize()];
        instr.try_as_mut().unwrap()
    }

    /// Prepends a field to the head of the record fields.

    #[inline]
    pub fn add_field(
        &mut self,
        field: (StrKey, Atom),
        head: Option<Id<RecordField>>,
    ) -> Id<RecordField> {
        let (label, atom) = field;
        let value = self.add(atom);
        let field = self.add(RecordField {
            label,
            value,
            next: head,
        });
        field
    }

    #[inline]
    pub fn extend_fields(
        &mut self,
        fields: impl IntoIterator<Item = (StrKey, Atom)>,
        mut head: Option<Id<RecordField>>,
    ) -> Option<Id<RecordField>> {
        for field in fields {
            head = Some(self.add_field(field, head));
        }

        head
    }

    /// Prepends a field path component to the head of the field path.
    #[inline]
    pub fn add_path(&mut self, label: StrKey, head: Option<Id<FieldPath>>) -> Id<FieldPath> {
        let path = self.add(FieldPath { label, next: head });
        path
    }

    #[inline]
    pub fn extend_path(
        &mut self,
        labels: impl IntoIterator<Item = StrKey>,
        mut head: Option<Id<FieldPath>>,
    ) -> Option<Id<FieldPath>> {
        for label in labels {
            head = Some(self.add_path(label, head));
        }

        head
    }

    #[inline]
    pub fn prepend_item(
        &mut self,
        atom: impl Into<Atom>,
        head: Option<Id<ListItem>>,
    ) -> Id<ListItem> {
        let value = self.add(atom.into());
        let item = self.add(ListItem { value, next: head });
        item
    }

    #[inline]
    pub fn append_item(
        &mut self,
        atom: impl Into<Atom>,
        tail: Option<Id<ListItem>>,
    ) -> Id<ListItem> {
        let value = self.add(atom.into());
        let item = self.add(ListItem { value, next: None });
        if let Some(tail) = tail {
            let tail_item = self.instr_mut(tail);
            // For true appending, the next item of the tail must be None.
            debug_assert!(tail_item.next.is_none());
            tail_item.next = Some(item);
        }
        item
    }

    #[inline]
    pub fn prepend_all_items(
        &mut self,
        items: impl IntoIterator<Item = Atom>,
        mut head: Option<Id<ListItem>>,
    ) -> Option<Id<ListItem>> {
        for item in items {
            head = Some(self.prepend_item(item, head));
        }

        head
    }

    #[inline]
    pub fn finish(self, root: Id<Expr>) -> Ir {
        let Self { instructions } = self;
        Ir { instructions, root }
    }
}

impl IrView for IrBuilder {
    fn instr<T>(&self, id: Id<T>) -> T
    where
        T: Copy,
        Instr: TryAsRef<T>,
    {
        let instr = &self.instructions[id.as_usize()];
        *instr.try_as_ref().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct Ir {
    instructions: Vec<Instr>,
    root: Id<Expr>,
}

impl Ir {
    pub fn root(&self) -> Id<Expr> {
        self.root
    }
}

impl IrView for Ir {
    fn instr<T>(&self, id: Id<T>) -> T
    where
        T: Copy,
        Instr: TryAsRef<T>,
    {
        let instr = &self.instructions[id.as_usize()];
        *instr.try_as_ref().unwrap()
    }
}
