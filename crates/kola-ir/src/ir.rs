use kola_utils::convert::{TryAsMut, TryAsRef};

use crate::{
    id::Id,
    instr::{Atom, Expr, Instr, ListItem, RecordField, Symbol},
};

pub trait IrView {
    fn instr<T>(&self, id: Id<T>) -> T
    where
        T: Copy,
        Instr: TryAsRef<T>;

    fn iter_fields(&self, head: Option<Id<RecordField>>) -> FieldIter<'_, Self> {
        FieldIter::new(self, head)
    }

    fn iter_items_forward(&self, head: Option<Id<ListItem>>) -> ForwardItemIter<'_, Self> {
        ForwardItemIter::new(self, head)
    }

    fn iter_items_backward(&self, tail: Option<Id<ListItem>>) -> BackwardItemIter<'_, Self> {
        BackwardItemIter::new(self, tail)
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

pub struct ForwardItemIter<'a, T: IrView + ?Sized> {
    ir: &'a T,
    current: Option<Id<ListItem>>,
}

impl<'a, T: IrView + ?Sized> ForwardItemIter<'a, T> {
    pub fn new(ir: &'a T, head: Option<Id<ListItem>>) -> Self {
        Self { ir, current: head }
    }
}

impl<'a, T: IrView + ?Sized> Iterator for ForwardItemIter<'a, T> {
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

pub struct BackwardItemIter<'a, T: IrView + ?Sized> {
    ir: &'a T,
    current: Option<Id<ListItem>>,
}

impl<'a, T: IrView + ?Sized> BackwardItemIter<'a, T> {
    pub fn new(ir: &'a T, head: Option<Id<ListItem>>) -> Self {
        Self { ir, current: head }
    }
}

impl<'a, T: IrView + ?Sized> Iterator for BackwardItemIter<'a, T> {
    type Item = ListItem;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item_id) = self.current {
            let item = self.ir.instr(item_id);
            self.current = item.prev;
            Some(item)
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
    pub fn new() -> Self {
        // Reserve slot 0 with a dummy instruction.
        let instructions = vec![Instr::Noop];

        Self { instructions }
    }

    pub fn add<T>(&mut self, instr: T) -> Id<T>
    where
        T: Into<Instr>,
    {
        let id = self.instructions.len() as u32;

        let instr = instr.into();
        self.instructions.push(instr);

        Id::new(id)
    }

    pub fn instr_mut<T>(&mut self, id: Id<T>) -> &mut T
    where
        Instr: TryAsMut<T>,
    {
        let instr = &mut self.instructions[id.as_usize()];
        instr.try_as_mut().unwrap()
    }

    /// Prepends a field to the head of the record fields.
    pub fn add_field(
        &mut self,
        field: (Symbol, Atom),
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

    pub fn prepend_item(&mut self, item: Atom, head: Option<Id<ListItem>>) -> Id<ListItem> {
        let value = self.add(item);
        let item = self.add(ListItem {
            value,
            next: head,
            prev: None,
        });
        if let Some(head_id) = head {
            let head_item = self.instr_mut(head_id);
            head_item.prev = Some(item);
        }
        item
    }

    pub fn append_item(&mut self, item: Atom, tail: Option<Id<ListItem>>) -> Id<ListItem> {
        let value = self.add(item);
        let item = self.add(ListItem {
            value,
            next: None,
            prev: tail,
        });
        if let Some(tail_id) = tail {
            let tail_item = self.instr_mut(tail_id);
            tail_item.next = Some(item);
        }
        item
    }

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
