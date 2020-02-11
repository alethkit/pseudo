/*
Location is used to keep track of where errors take place,
so that errors shown to users can be more easily tracked.
*/
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Location {
    pub column: u64,
    pub line: u64,
}

impl Location {
    pub fn new(line: u64, column: u64) -> Self {
        Location { line, column }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Line {}, Column {}", self.line, self.column)
    }
}

#[derive(Debug)]
pub struct Locatable<T> {
    t: T,
    loc: Location,
}

impl<T> Locatable<T> {
    pub fn new(t: T, loc: Location) -> Self {
        Locatable { t, loc }
    }

    pub fn deloc(self) -> T {
        self.t
    }
    pub fn deloc_ref(&self) -> &T {
        &self.t
    }

    pub fn get_loc(self) -> Location {
        self.loc
    }

    pub fn loc_ref(&self) -> &Location {
        &self.loc
    }
    pub fn from_res<E>((res, loc): (Result<T, E>, Location)) -> Result<Locatable<T>, Locatable<E>> {
        match res {
            Ok(val) => Ok(Locatable::new(val, loc)),
            Err(e) => Err(Locatable::new(e, loc)),
        }
    }

    pub fn deconstruct(self) -> (T, Location) {
        (self.t, self.loc)
    }

    pub fn deconstruct_ref(&self) -> (&T, &Location) {
        (&self.t, &self.loc)
    }
}

impl<T: Display> Display for Locatable<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.loc, self.t)
    }
}

impl<T> From<(T, Location)> for Locatable<T> {
    fn from((t, loc): (T, Location)) -> Self {
        Self { t, loc }
    }
}
