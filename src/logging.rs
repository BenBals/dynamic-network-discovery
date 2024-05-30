//! helper functions for the logging backend
// Adapted from https://gitlab.hpi.de/wasletztepreis_bp/db_scripts/-/blob/master/rust-db/common/src/logging.rs
use crate::util::find_project_root;
use flexi_logger::{self, writers::FileLogWriter, Duplicate, Logger, FileSpec};
use log::Level::Warn;
use std::fs;

/// Creates a logging backend
/// By default all logs with Info or higher are written to a logfile in folder logs.
/// All logs with level at least Info are also written to stdout.
/// Logs with level at least Error are also written to stderr.
///
/// logs can be written via log::{error!, warn!, info!, debug!, trace!}
pub fn init_logging() {
    let mut output_dir = find_project_root().unwrap();
    output_dir.push("logs");
    fs::create_dir(&output_dir).unwrap_or_else(|_| {});
    Logger::try_with_env_or_str("info")
        .expect("Could not construct info logger")
        .format(flexi_logger::colored_opt_format)
        .log_to_file(FileSpec::default()
            .directory(output_dir))
        .print_message()
        .duplicate_to_stderr(Duplicate::Warn)
        .start()
        .unwrap_or_else(|error| panic!("Logging initialization failed: {}", error));
    log_panics::init();
}

/// Creates a logging backend for use in testing
/// By default all logs with Warn or higher are printed to stdout.
pub fn init_test_logging() {
    if !log::log_enabled!(Warn) {
        Logger::try_with_env_or_str("warn")
            .expect("Could not construct warn logger")
            .format(flexi_logger::colored_opt_format)
            .start()
            .unwrap_or_else(|error| panic!("Logging initialization failed: {}", error));
    }
}