use std::{path::PathBuf, sync::Arc};

use bozon_ast::Ast;

/// Parser query group.
#[salsa::query_group(ParserStorage)]
pub trait Parser: salsa::Database + crate::groups::Vfs {
    // NOTE: Maybe there's should be some kind of wrapper over strings (actual source) and paths.
    // So user can choose which one he's using. AFAIK currently impossible to do generics in
    // query group traits.
    /// Returns ast of given file.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use std::path::PathBuf;
    /// use bozon_queries::*;
    ///
    /// let db = Database::default();
    /// let parse_result = db.ast(PathBuf::from("/path/to/file"));
    /// ```
    fn ast(&self, filepath: PathBuf) -> Result<Arc<Ast>, Vec<chumsky::error::Simple<char>>>;
}

/// This is an implementation of a trait function.
pub fn ast(
    db: &dyn Parser,
    filepath: PathBuf,
) -> Result<Arc<Ast>, Vec<chumsky::error::Simple<char>>> {
    use chumsky::Parser;

    let source = db.read_file(filepath);
    bozon_parser::program().parse(source.as_str()).map(Arc::new)
}
