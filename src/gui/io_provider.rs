/*
An implementation of IOProvider for the program's GUI
*/
use glib::markup_escape_text;
use gtk::prelude::{ContainerExt, DialogExt, EntryExt, TextBufferExt, WidgetExt};
use gtk::{TextBuffer, Window};
use pseudocode::{IOError, IOProvider};

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
    fn get_line(&mut self) -> Result<String, IOError> {
        let dialog = gtk::Dialog::new_with_buttons(
            Some("Input requested"),
            Some(&self.window),
            // Modal dialogue ensures that users cannot skip input phase.
            gtk::DialogFlags::MODAL | gtk::DialogFlags::DESTROY_WITH_PARENT,
            &[("Send", gtk::ResponseType::Accept)],
        );
        let area = dialog.get_content_area();
        let user_entry = gtk::Entry::new();
        area.add(&user_entry);
        dialog.show_all();
        dialog.run();
        let contents = user_entry
            .get_text()
            .ok_or(IOError("Error getting user input from GUI"))?;
        dialog.destroy();

        Ok(contents.to_string())
    }
    fn show_line(&mut self, line_to_be_shown: &str) {
        // Newline has to be inserted as insert method does not automatically insert a newline
        // character
        self.output_buf
            .insert(&mut self.output_buf.get_end_iter(), line_to_be_shown);
        self.output_buf
            .insert(&mut self.output_buf.get_end_iter(), "\n");
    }

    fn show_err(&mut self, err_to_be_shown: &str) {
        // Escaping used to prevent arbritary output to be treated as markup.
        let erred_string = "<span foreground=\"red\">".to_owned()
            + markup_escape_text(err_to_be_shown).as_str()
            + "\n</span>";
        self.output_buf
            .insert_markup(&mut self.output_buf.get_end_iter(), &erred_string);
    }
}
