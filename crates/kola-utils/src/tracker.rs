/// A generic task tracking structure that manages items in "todo" and "done" states.
///
/// This structure allows for tracking items that need to be processed (`todo`)
/// and items that have been completed (`done`), with operations to move items
/// between these states.
pub struct Tracker<T> {
    /// Items that are pending completion
    todo: Vec<T>,
    /// Items that have been completed
    done: Vec<T>,
}

impl<T> Tracker<T> {
    /// Creates a new, empty `Tracker`.
    ///
    /// # Examples
    ///
    /// ```
    /// let tracker: Tracker<i32> = Tracker::new();
    /// assert!(tracker.is_empty());
    /// ```
    pub fn new() -> Self {
        Tracker {
            todo: Vec::new(),
            done: Vec::new(),
        }
    }

    /// Creates a new `Tracker` with the specified capacity for both todo and done lists.
    ///
    /// # Examples
    ///
    /// ```
    /// let tracker: Tracker<String> = Tracker::with_capacity(10);
    /// ```
    pub fn with_capacity(capacity: usize) -> Self {
        Tracker {
            todo: Vec::with_capacity(capacity),
            done: Vec::with_capacity(capacity),
        }
    }

    /// Adds a new item to the todo list.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// assert_eq!(tracker.len(), 1);
    /// ```
    pub fn start(&mut self, item: T) {
        self.todo.push(item);
    }

    /// Moves the most recently added todo item to the done list.
    /// Has no effect if the todo list is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// tracker.finish();
    /// assert!(tracker.is_empty());
    /// ```
    pub fn finish(&mut self) {
        if let Some(current) = self.todo.pop() {
            self.done.push(current);
        }
    }

    /// Removes and returns the most recently added item from the todo list.
    ///
    /// # Returns
    ///
    /// * `Some(T)` - The removed item
    /// * `None` - If the todo list is empty
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// let removed = tracker.discard();
    /// assert_eq!(removed, Some("task 1"));
    /// assert!(tracker.is_empty());
    /// ```
    pub fn discard(&mut self) -> Option<T> {
        self.todo.pop()
    }

    /// Moves the most recently finished item back to the todo list.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// tracker.finish();
    /// tracker.restore();
    /// assert_eq!(tracker.len(), 1);
    /// ```
    pub fn restore(&mut self) {
        if let Some(previous) = self.done.pop() {
            self.todo.push(previous);
        }
    }

    /// Returns a reference to the most recently added todo item, if any.
    ///
    /// # Returns
    ///
    /// * `Some(&T)` - Reference to the most recent todo item
    /// * `None` - If the todo list is empty
    pub fn current(&self) -> Option<&T> {
        self.todo.last()
    }

    /// Returns a mutable reference to the most recently added todo item, if any.
    ///
    /// # Returns
    ///
    /// * `Some(&mut T)` - Mutable reference to the most recent todo item
    /// * `None` - If the todo list is empty
    pub fn current_mut(&mut self) -> Option<&mut T> {
        self.todo.last_mut()
    }

    /// Clears both todo and done lists, removing all items.
    pub fn clear(&mut self) {
        self.todo.clear();
        self.done.clear();
    }

    /// Returns the number of items in the todo list.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// tracker.start("task 2");
    /// assert_eq!(tracker.pending_count(), 2);
    /// ```
    pub fn pending_count(&self) -> usize {
        self.todo.len()
    }

    /// Returns the number of items in the done list.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// tracker.finish();
    /// assert_eq!(tracker.completed_count(), 1);
    /// ```
    pub fn completed_count(&self) -> usize {
        self.done.len()
    }

    /// Returns the total number of items in both todo and done lists.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// tracker.start("task 2");
    /// tracker.finish();
    /// assert_eq!(tracker.total_count(), 2);
    /// ```
    pub fn total_count(&self) -> usize {
        self.todo.len() + self.done.len()
    }

    /// Returns an iterator over references to the items in the todo list.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start(1);
    /// tracker.start(2);
    ///
    /// let sum: i32 = tracker.pending_tasks().sum();
    /// assert_eq!(sum, 3);
    /// ```
    pub fn pending(&self) -> impl Iterator<Item = &T> {
        self.todo.iter()
    }

    /// Returns an iterator over references to the items in the done list.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start(1);
    /// tracker.start(2);
    /// tracker.finish();
    ///
    /// let sum: i32 = tracker.completed_tasks().sum();
    /// assert_eq!(sum, 2);
    /// ```
    pub fn completed(&self) -> impl Iterator<Item = &T> {
        self.done.iter()
    }

    /// Returns a mutable iterator over items in the todo list.
    pub fn pending_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.todo.iter_mut()
    }

    /// Returns a mutable iterator over items in the done list.
    pub fn completed_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.done.iter_mut()
    }

    /// Consumes the tracker and returns only the completed items.
    ///
    /// This method destroys the `Tracker` and returns ownership of the completed items
    /// as a vector, discarding any pending items.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// tracker.finish();
    ///
    /// let completed = tracker.into_completed();
    /// assert_eq!(completed.len(), 1);
    /// ```
    pub fn into_completed(self) -> Vec<T> {
        self.done
    }

    /// Consumes the tracker and returns only the pending items.
    ///
    /// This method destroys the `Tracker` and returns ownership of the pending items
    /// as a vector, discarding any completed items.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// tracker.start("task 2");
    ///
    /// let pending = tracker.into_pending();
    /// assert_eq!(pending.len(), 2);
    /// ```
    pub fn into_pending(self) -> Vec<T> {
        self.todo
    }

    /// Consumes the tracker and returns a tuple of vectors containing the todo and done items.
    ///
    /// This method destroys the `Tracker` and returns ownership of its internal storage as
    /// a tuple of vectors `(todo, done)`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut tracker = Tracker::new();
    /// tracker.start("task 1");
    /// tracker.start("task 2");
    /// tracker.finish();
    ///
    /// let (todo, done) = tracker.into_vec();
    /// assert_eq!(todo.len(), 1);
    /// assert_eq!(done.len(), 1);
    /// ```
    pub fn into_vec(self) -> (Vec<T>, Vec<T>) {
        (self.todo, self.done)
    }
}
