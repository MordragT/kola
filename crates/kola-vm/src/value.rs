use derive_more::From;
use kola_collections::shadow_map::ShadowMap;
use kola_ir::instr::{Func, Symbol};
use kola_utils::as_variant;
use std::{fmt, rc::Rc};

use crate::{cont::Cont, env::Env};

/// Values produced by evaluating expressions
#[derive(Debug, From, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Char(char),
    Num(f64),
    Str(String),
    /// A function closure (environment, function definition)
    Func(Env, Func),
    /// A captured continuation
    Cont(Cont),
    /// A variant (label, value)
    Variant(Symbol, Box<Self>),
    /// A record (map of labels to values)
    Record(Record),
}

impl Value {
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_char(&self) -> bool {
        matches!(self, Value::Char(_))
    }

    pub fn is_num(&self) -> bool {
        matches!(self, Value::Num(_))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Value::Str(_))
    }

    pub fn is_func(&self) -> bool {
        matches!(self, Value::Func(_, _))
    }

    pub fn is_cont(&self) -> bool {
        matches!(self, Value::Cont(_))
    }

    pub fn is_variant(&self) -> bool {
        matches!(self, Value::Variant(_, _))
    }

    pub fn is_record(&self) -> bool {
        matches!(self, Value::Record(_))
    }

    pub fn as_bool(&self) -> Option<bool> {
        as_variant!(self, Self::Bool).copied()
    }

    pub fn as_char(&self) -> Option<char> {
        as_variant!(self, Self::Char).copied()
    }

    pub fn as_num(&self) -> Option<f64> {
        as_variant!(self, Self::Num).copied()
    }

    pub fn as_str(&self) -> Option<&str> {
        as_variant!(self, Self::Str).map(|s| s.as_str())
    }

    pub fn as_func(&self) -> Option<&Func> {
        todo!()
    }

    pub fn as_cont(&self) -> Option<&Cont> {
        as_variant!(self, Self::Cont)
    }

    pub fn as_variant(&self) -> Option<(&Symbol, &Value)> {
        todo!()
    }

    pub fn as_record(&self) -> Option<&Record> {
        as_variant!(self, Self::Record)
    }

    pub fn into_bool(self) -> Option<bool> {
        as_variant!(self, Self::Bool)
    }

    pub fn into_char(self) -> Option<char> {
        as_variant!(self, Self::Char)
    }

    pub fn into_num(self) -> Option<f64> {
        as_variant!(self, Self::Num)
    }

    pub fn into_str(self) -> Option<String> {
        as_variant!(self, Self::Str)
    }

    pub fn into_func(self) -> Option<(Env, Func)> {
        // as_variant!(self, Self::Func)
        todo!()
    }

    pub fn into_cont(self) -> Option<Cont> {
        as_variant!(self, Self::Cont)
    }

    pub fn into_variant(self) -> Option<(Symbol, Value)> {
        // as_variant!(self, Self::Variant)
        todo!()
    }

    pub fn into_record(self) -> Option<Record> {
        as_variant!(self, Self::Record)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Char(c) => write!(f, "{}", c),
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::Func(_, _) => write!(f, "<function>"),
            Value::Cont(_) => write!(f, "<continuation>"),
            Value::Variant(label, value) => write!(f, "{}({})", label, value),
            Value::Record(r) => write!(f, "{}", r),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record(Rc<ShadowMap<Symbol, Value>>);

impl Record {
    pub fn empty() -> Self {
        Self(Rc::new(ShadowMap::new()))
    }

    pub fn new(map: ShadowMap<Symbol, Value>) -> Self {
        Self(Rc::new(map))
    }

    pub fn insert(&mut self, key: Symbol, value: Value) {
        let map = Rc::make_mut(&mut self.0);
        map.insert(key, value);
    }

    pub fn get(&self, key: &Symbol) -> Option<&Value> {
        self.0.get(key)
    }

    pub fn remove(&mut self, key: &Symbol) -> Option<Value> {
        let map = Rc::make_mut(&mut self.0);
        map.remove(key)
    }

    pub fn contains_key(&self, key: &Symbol) -> bool {
        self.0.contains_key(key)
    }

    pub fn keys(&self) -> impl Iterator<Item = &Symbol> {
        self.0.keys()
    }

    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.0.values()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<ShadowMap<Symbol, Value>> for Record {
    fn from(map: ShadowMap<Symbol, Value>) -> Self {
        Self(Rc::new(map))
    }
}

impl fmt::Display for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for (key, value) in self.0.iter() {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{} = {}", key, value)?;
            first = false;
        }
        write!(f, "}}")
    }
}
