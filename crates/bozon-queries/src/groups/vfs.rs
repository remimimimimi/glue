use std::path::PathBuf;

/// Virtual file system for file reading (and watching in future).
#[salsa::query_group(VfsStorage)]
pub trait Vfs: salsa::Database {
    fn read_file(&self, path: PathBuf) -> String;
}

fn read_file(_: &dyn Vfs, path: PathBuf) -> String {
    // TODO: Add proper io error handling
    std::fs::read_to_string(&path).expect(&format!(
        "Cannot read {}",
        path.as_os_str().to_str().unwrap()
    ))
}
