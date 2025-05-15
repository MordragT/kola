use crate::{
    config::{MachineState, StandardConfig},
    value::Value,
};
use kola_ir::{
    id::InstrId,
    instr::{Atom, RetExpr},
    ir::Ir,
};

pub trait Eval {
    fn eval(&self, config: &StandardConfig, ir: &Ir) -> Result<MachineState, String>;
}

impl Eval for InstrId<Atom> {
    fn eval(&self, config: &StandardConfig, ir: &Ir) -> Result<MachineState, String> {
        match *self.get(ir) {
            Atom::Bool(b) => Ok(Value::Bool(b).into()),
            Atom::Char(c) => Ok(Value::Char(c).into()),
            Atom::Num(n) => Ok(Value::Num(n).into()),
            Atom::Str(s) => Ok(Value::Str(s.to_string()).into()), // You may need to convert StrKey to String
            Atom::Func(_) => {
                // Create a closure by capturing the current environment
                Ok(Value::Func(config.env.clone(), *self).into())
            }
            Atom::Symbol(s) => {
                // Look up the symbol in the environment
                if let Some(value_id) = config.env.lookup(&s) {
                    value_id.eval(config, ir)
                } else {
                    Err(format!("Unbound variable: {}", s))
                }
            }
        }
    }
}
