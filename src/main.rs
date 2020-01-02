mod gui;
mod utils;
use std::env::args;
use gio::prelude::{ApplicationExt,ApplicationExtManual};



fn main() {
    let application = gtk::Application::new(None, Default::default()).unwrap();
    application.connect_activate(|app| {
        gui::build_ui(app);
    });
    application.run(&args().collect::<Vec<_>>());
}
