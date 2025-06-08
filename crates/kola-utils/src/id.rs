// Id { u32 }
#[macro_export]
macro_rules! define_id {
    ($name:ident) => {
        #[derive(
            Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
        )]
        pub struct $name {
            id: u32,
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({})", stringify!($name), self.id)
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.id)
            }
        }

        impl $name {
            pub fn as_usize(&self) -> usize {
                self.id as usize
            }

            pub fn id(&self) -> u32 {
                self.id
            }
        }
    };
}

// Id { u32, level: u32}
#[macro_export]
macro_rules! define_leveled_id {
    ($name:ident) => {
        #[derive(
            Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
        )]
        pub struct $name {
            id: u32,
            level: u32,
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}({}, level: {})",
                    stringify!($name),
                    self.id,
                    self.level
                )
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.id)
            }
        }

        impl $name {
            pub fn as_usize(&self) -> usize {
                self.id as usize
            }

            pub fn id(&self) -> u32 {
                self.id
            }

            pub fn level(&self) -> u32 {
                self.level
            }
        }
    };
}

// Id<T> { u32, t: PhantomData<T> }
#[macro_export]
macro_rules! define_unique_id {
    ($name:ident) => {
        #[derive(serde::Serialize, serde::Deserialize)]
        pub struct $name<T: ?Sized> {
            id: u32,
            t: std::marker::PhantomData<T>,
        }

        impl<T: ?Sized> Clone for $name<T> {
            fn clone(&self) -> Self {
                Self {
                    id: self.id,
                    t: std::marker::PhantomData,
                }
            }
        }

        impl<T: ?Sized> Copy for $name<T> {}

        impl<T: ?Sized> PartialEq for $name<T> {
            fn eq(&self, other: &Self) -> bool {
                self.id == other.id
            }
        }

        impl<T: ?Sized> Eq for $name<T> {}

        impl<T: ?Sized> PartialOrd for $name<T> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl<T: ?Sized> Ord for $name<T> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.id.cmp(&other.id)
            }
        }

        impl<T: ?Sized> std::hash::Hash for $name<T> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.id.hash(state);
            }
        }

        impl<T: ?Sized> std::fmt::Debug for $name<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({})", stringify!($name), self.id)
            }
        }

        impl<T: ?Sized> std::fmt::Display for $name<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.id)
            }
        }

        impl<T: ?Sized> $name<T> {
            pub fn as_usize(&self) -> usize {
                self.id as usize
            }

            pub fn id(&self) -> u32 {
                self.id
            }
        }
    };
}

// Id<T> { u32, level: u32, t: PhantomData<T> }
#[macro_export]
macro_rules! define_unique_leveled_id {
    ($name:ident) => {
        #[derive(serde::Serialize, serde::Deserialize)]
        pub struct $name<T: ?Sized> {
            id: u32,
            level: u32,
            t: std::marker::PhantomData<T>,
        }

        impl<T: ?Sized> Clone for $name<T> {
            fn clone(&self) -> Self {
                Self {
                    id: self.id,
                    level: self.level,
                    t: std::marker::PhantomData,
                }
            }
        }

        impl<T: ?Sized> Copy for $name<T> {}

        impl<T: ?Sized> PartialEq for $name<T> {
            fn eq(&self, other: &Self) -> bool {
                self.id == other.id && self.level == other.level
            }
        }

        impl<T: ?Sized> Eq for $name<T> {}

        impl<T: ?Sized> PartialOrd for $name<T> {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl<T: ?Sized> Ord for $name<T> {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.id.cmp(&other.id).then(self.level.cmp(&other.level))
            }
        }

        impl<T: ?Sized> std::hash::Hash for $name<T> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.id.hash(state);
                self.level.hash(state);
            }
        }

        impl<T: ?Sized> std::fmt::Debug for $name<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}({}, level: {})",
                    stringify!($name),
                    self.id,
                    self.level
                )
            }
        }

        impl<T: ?Sized> std::fmt::Display for $name<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.id)
            }
        }

        impl<T: ?Sized> $name<T> {
            pub fn as_usize(&self) -> usize {
                self.id as usize
            }

            pub fn id(&self) -> u32 {
                self.id
            }

            pub fn level(&self) -> u32 {
                self.level
            }
        }
    };
}
