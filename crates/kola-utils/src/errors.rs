use std::{
    fmt::{self, Display},
    ops::{Index, IndexMut},
    slice, vec,
};

/// An error type which can represent multiple errors.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Errors<T> {
    errors: Vec<T>,
}

impl<T> Default for Errors<T> {
    fn default() -> Self {
        Errors::new()
    }
}

impl<T> Errors<T> {
    /// Creates a new, empty `Errors` instance.
    pub fn new() -> Errors<T> {
        Errors::from(Vec::new())
    }

    /// Returns true if `self` contains any errors
    pub fn has_errors(&self) -> bool {
        !self.is_empty()
    }

    pub fn take(&mut self) -> Self {
        let errors = std::mem::take(&mut self.errors);
        Self { errors }
    }

    pub fn replace(&mut self, with: Self) -> Self {
        let errors = std::mem::replace(&mut self.errors, with.errors);
        Self { errors }
    }

    /// The number of errors in the error list
    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    /// Adds an error to `self`
    pub fn push(&mut self, t: T) {
        self.errors.push(t);
    }

    /// Pops and error off the error list
    pub fn pop(&mut self) -> Option<T> {
        self.errors.pop()
    }

    pub fn append(&mut self, other: &mut Self) {
        self.errors.append(&mut other.errors);
    }

    pub fn iter(&self) -> slice::Iter<T> {
        self.errors.iter()
    }

    pub fn iter_mut(&mut self) -> slice::IterMut<T> {
        self.errors.iter_mut()
    }

    pub fn drain(
        &mut self,
        range: impl std::ops::RangeBounds<usize>,
    ) -> impl Iterator<Item = T> + '_ {
        self.errors.drain(range)
    }
}

impl<T> fmt::Display for Errors<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for err in &self.errors {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}

impl<T> Index<usize> for Errors<T> {
    type Output = T;
    fn index(&self, index: usize) -> &T {
        &self.errors[index]
    }
}

impl<T> IndexMut<usize> for Errors<T> {
    fn index_mut(&mut self, index: usize) -> &mut T {
        &mut self.errors[index]
    }
}

impl<T> Extend<T> for Errors<T> {
    fn extend<Iter: IntoIterator<Item = T>>(&mut self, iter: Iter) {
        self.errors.extend(iter);
    }
}

impl<T> From<T> for Errors<T> {
    fn from(err: T) -> Errors<T> {
        Errors { errors: vec![err] }
    }
}

impl<T> From<Vec<T>> for Errors<T> {
    fn from(errors: Vec<T>) -> Errors<T> {
        Errors { errors }
    }
}

impl<T> FromIterator<T> for Errors<T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Errors<T> {
        Errors {
            errors: iter.into_iter().collect(),
        }
    }
}

impl<T> From<Errors<T>> for Vec<T> {
    fn from(errors: Errors<T>) -> Vec<T> {
        errors.errors
    }
}

impl<T> IntoIterator for Errors<T> {
    type Item = T;

    type IntoIter = vec::IntoIter<T>;

    fn into_iter(self) -> vec::IntoIter<T> {
        self.errors.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Errors<T> {
    type Item = &'a T;

    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> slice::Iter<'a, T> {
        self.errors.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Errors<T> {
    type Item = &'a mut T;

    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> slice::IterMut<'a, T> {
        self.errors.iter_mut()
    }
}
