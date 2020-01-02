use crate::internal::RuntimeError;

pub trait IOProvider {
    // IOProvider is implemented as an interface to allow for either GUI or console access to the
    // interpreter.
    fn get_line(&self) -> Result<String, RuntimeError>;
    fn show_line(&self, line_to_be_shown: &str);
    fn show_err(&self, err_to_be_shown: &str);
}

#[derive(Copy, Clone)]
pub struct ConsoleIOProvider {}

impl IOProvider for ConsoleIOProvider {
    fn get_line(&self) -> Result<String, RuntimeError> {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        Ok(line)
    }

    fn show_line(&self, line_to_be_shown: &str) {
        println!("{}", line_to_be_shown);
    }

    fn show_err(&self, err_to_be_shown: &str) {
        println!("Error: {}", err_to_be_shown)
    }
}

