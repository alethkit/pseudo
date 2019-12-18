use crate::error::runtime::RuntimeError;
use glib::markup_escape_text;
use gtk::prelude::{ContainerExt, DialogExt, EntryExt, TextBufferExt, WidgetExt};
use gtk::{TextBuffer, Window};

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

#[derive(Clone)]
pub struct GUIIOProvider {
    output_buf: TextBuffer,
    window: Window,
}

impl GUIIOProvider {
    pub fn new(output_buf: TextBuffer, window: Window) -> Self {
        Self { output_buf, window }
    }
}

impl IOProvider for GUIIOProvider {
    fn get_line(&self) -> Result<String, RuntimeError> {
        let dialog = gtk::Dialog::new_with_buttons(
            Some("Input requested"),
            Some(&self.window),
            gtk::DialogFlags::MODAL | gtk::DialogFlags::DESTROY_WITH_PARENT,
            &[("Send", gtk::ResponseType::Accept)],
        );
        let area = dialog.get_content_area();
        let user_entry = gtk::Entry::new();
        area.add(&user_entry);
        dialog.show_all();
        dialog.run();
        let contents = user_entry.get_text().unwrap();
        dialog.destroy();

        Ok(contents.to_string())
    }
    fn show_line(&self, line_to_be_shown: &str) {
        self.output_buf
            .insert(&mut self.output_buf.get_end_iter(), line_to_be_shown);
        self.output_buf
            .insert(&mut self.output_buf.get_end_iter(), "\n");
    }

    fn show_err(&self, err_to_be_shown: &str) {
        let erred_string = "<span foreground=\"red\">".to_owned()
            + markup_escape_text(err_to_be_shown).as_str()
            + "\n</span>";
        self.output_buf
            .insert_markup(&mut self.output_buf.get_end_iter(), &erred_string);
    }
}
