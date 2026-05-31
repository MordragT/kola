pub trait SerializeWith<T> {
    fn serialize<S>(&self, serializer: S, t: &T) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer;
}
