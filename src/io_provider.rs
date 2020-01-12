use crate::IOError;

pub trait IOProvider {
    // IOProvider is implemented as an interface to allow for either GUI or console access to the
    // interpreter.
    fn get_line(&mut self) -> Result<String, IOError>;
    fn show_line(&mut self, line_to_be_shown: &str);
    fn show_err(&mut self, err_to_be_shown: &str);
    fn compare_output_stream(&self, out_stream: Vec<String>) -> bool {false}
}

#[derive(Copy, Clone)]
pub struct ConsoleIOProvider {}

impl IOProvider for ConsoleIOProvider {
    fn get_line(&mut self) -> Result<String, IOError> {
        let mut line = String::new();
        std::io::stdin().read_line(&mut line)?;
        Ok(line)
    }

    fn show_line(&mut self, line_to_be_shown: &str) {
        println!("{}", line_to_be_shown);
    }

    fn show_err(&mut self, err_to_be_shown: &str) {
        println!("Error: {}", err_to_be_shown)
    }
}
