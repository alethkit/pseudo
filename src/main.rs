#![windows_subsystem = "windows"] // Removes terminal window
mod gui;
mod utils;
use gio::prelude::{ApplicationExt, ApplicationExtManual};
use std::env::args;

fn main() {
    // Boilerplate code to start application GUI
    let application = gtk::Application::new(None, Default::default()).unwrap();
    application.connect_activate(|app| {
        gui::build_ui(app);
    });
    application.run(&args().collect::<Vec<_>>());
}
