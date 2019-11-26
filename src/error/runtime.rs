use crate::ast::operator::EvalError;
#[derive(Debug)]
pub enum RuntimeError {
    Eval(EvalError),
    RangeStepCannotBeZero,
    InvalidRangeBound
}

impl From<EvalError> for RuntimeError {
    fn from(err: EvalError) -> Self {
        Self::Eval(err)
    }
}
