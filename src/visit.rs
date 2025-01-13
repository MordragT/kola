pub trait Visitor {} // TODO only node visitor necessary ?

pub trait Visit<T> {
    fn visit(&self, visitor: T) {
        self.walk()
    }

    fn visit_mut(&mut self, visitor: T) {
        self.walk()
    }

    fn walk(&self);
}
