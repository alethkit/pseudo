#[derive(Debug)]
pub enum RuntimeError {
    DivisionByZero,
    OutOfRange,
    UndefinedVariable,
    MustBeCalled,
    RangeStepCannotBeZero,
    InvalidRangeBound
}

