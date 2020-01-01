use gio::prelude::*;
use gtk::prelude::*;
use pseudocode::interpreter::Interpreter;
use pseudocode::io_provider::{GUIIOProvider, IOProvider};
use pseudocode::lexer::{Lexer, LocatableChars};
use pseudocode::parser::Parser;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::Write;
use std::path::Path;

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

fn build_ui(app: &gtk::Application) {
    // Used to register SourceView for the builder.
    let _dummy = sourceview::View::new();
    let builder = gtk::Builder::new_from_file(&Path::new("./src/main.glade"));
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
        let gui_io = GUIIOProvider::new(output_buf.clone(), window_clone.clone());
        let contents = buf
            .get_text(&buf.get_start_iter(), &buf.get_end_iter(), false)
            .unwrap();
        run_program(&contents, gui_io)
    });
    window.set_application(Some(app));
    window.show_all();
}

fn run_program(contents: &str, provider: impl IOProvider + Clone + 'static) {
    //Runs the program given and interacts with the user via IOProvider.
    let chars = LocatableChars::from(contents);
    let lex = Lexer::from(chars);
    let (tokens, errors): (Vec<_>, Vec<_>) = lex.partition(|(r, _l)| r.is_ok());
    if errors.is_empty() {
        let pars = Parser::from(tokens.into_iter().map(|(r, l)| (r.unwrap(), l)));
        let program: Result<Vec<_>, _> = pars.collect();
        match program {
            Ok(stmts) => {
                let mut inter = Interpreter::new(Box::new(provider.clone()));
                if let Err(e) = inter.execute(&stmts.into_iter().map(|c| c.0).collect()) {
                    provider.show_err(&format!("Runtime error: {:#?}", e));
                }
            }
            Err(e) => provider.show_err(&format!("Error: {:#?}", e)),
        }
    } else {
        let err_string = format!("Errors: {:#?}", errors);
        provider.show_err(&err_string);
    }
}

fn main() {
    let application = gtk::Application::new(None, Default::default()).unwrap();
    application.connect_activate(|app| {
        build_ui(app);
    });
    application.run(&args().collect::<Vec<_>>());
}
