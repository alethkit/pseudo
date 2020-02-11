/*
Module provides interactivity for the program's GUI.
gui.glade provides the actual design file for the GUI.
*/
mod io_provider;
use super::utils::run_program;
use io_provider::GUIIOProvider;

use gtk::prelude::*;
use std::fs::{read_to_string, File};
use std::io::Write;

fn create_message_dialog(win: gtk::Window, msg: &str) {
    //Used for unexpected errors
    let diag = gtk::MessageDialog::new(
        Some(&win),
        gtk::DialogFlags::empty(),
        gtk::MessageType::Error,
        gtk::ButtonsType::Close,
        msg,
    );
    diag.run();
    diag.destroy();
}

pub fn build_ui(app: &gtk::Application) {
    // Used to register SourceView for the builder.
    let _dummy = sourceview::View::new();
    let builder = gtk::Builder::new_from_string(include_str!("gui.glade"));
    let window: gtk::Window = builder.get_object("main_window").unwrap();
    let open_file_button: gtk::Button = builder.get_object("open_file_button").unwrap();
    let save_file_button: gtk::Button = builder.get_object("save_file_button").unwrap();
    let run_file_button: gtk::Button = builder.get_object("run_file_button").unwrap();
    let input_view: sourceview::View = builder.get_object("source_view").unwrap();
    let window_clone = window.clone();
    open_file_button.connect_clicked(move |_| {
        // Adds functionality to the open file button
        let dialog = gtk::FileChooserNative::new(
            Some("Select a file"),
            Some(&window_clone),
            gtk::FileChooserAction::Open,
            Some("Open"),
            Some("Cancel"),
        );
        dialog.run();
        let filename = match dialog.get_filename() {
            Some(f) => f,
            None => return,
        };
        dialog.destroy();
        let file = match read_to_string(filename) {
            Ok(f) => f,
            Err(e) => {
                create_message_dialog(window_clone.clone(), &e.to_string());
                return;
            }
        };
        // Inserts file contents in input view
        let buf = input_view.get_buffer().unwrap();
        buf.insert(&mut buf.get_start_iter(), &file)
    });
    let window_clone = window.clone();
    let input_view: sourceview::View = builder.get_object("source_view").unwrap();
    let input_clone = input_view.clone();
    save_file_button.connect_clicked(move |_| {
        // Adds functionality to the save file button
        let dialog = gtk::FileChooserNative::new(
            Some("Select a file"),
            Some(&window_clone),
            gtk::FileChooserAction::Save,
            Some("Save"),
            Some("Cancel"),
        );
        dialog.run();
        let filename = match dialog.get_filename() {
            Some(f) => f,
            None => return,
        };
        dialog.destroy();
        let buf = input_view.get_buffer().unwrap();
        let contents = buf
            .get_text(&buf.get_start_iter(), &buf.get_end_iter(), false)
            .unwrap();
        let mut file = match File::create(filename) {
            Ok(f) => f,
            Err(e) => {
                create_message_dialog(window_clone.clone(), &e.to_string());
                return;
            }
        };
        match file.write_all(contents.as_str().as_bytes()) {
            // Inserts input view contents in file
            Ok(_) => (),
            Err(e) => create_message_dialog(window_clone.clone(), &e.to_string()),
        };
    });
    let output_view: gtk::TextView = builder.get_object("output_view").unwrap();
    let window_clone = window.clone();
    run_file_button.connect_clicked(move |_| {
        let buf = input_clone.get_buffer().unwrap();
        let output_buf = output_view.get_buffer().unwrap();
        output_buf.delete(
            &mut output_buf.get_start_iter(),
            &mut output_buf.get_end_iter(),
        );
        let gui_io = GUIIOProvider::new(output_buf, window_clone.clone());
        let contents = buf
            .get_text(&buf.get_start_iter(), &buf.get_end_iter(), false)
            .unwrap();
        run_program(&contents, gui_io)
    });
    window.set_application(Some(app));
    window.show_all();
}
