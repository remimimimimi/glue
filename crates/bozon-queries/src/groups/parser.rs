use std::path::PathBuf;

use bozon_ast::sexp::Ast;

/// Parser query group.
#[salsa::query_group(ParserStorage)]
pub trait Parser: salsa::Database + crate::groups::Vfs {
    // NOTE: Maybe there's should be some kind of wrapper over strings (actual source) and paths.
    // So user can choose which one he's using. AFAIK currently impossible to do generics in
    // query group traits.
    /// Returns s-expression ast of given file.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use std::path::PathBuf;
    /// use bozon_queries::*;
    ///
    /// let db = Database::default();
    /// let parse_result = db.sexp_ast(PathBuf::from("/path/to/file"));
    /// ```
    fn sexp_ast(&self, filepath: PathBuf) -> Result<Ast, Vec<chumsky::error::Simple<char>>>;
}

/// This is an implementation of a trait function.
pub fn sexp_ast(
    db: &dyn Parser,
    filepath: PathBuf,
) -> Result<Ast, Vec<chumsky::error::Simple<char>>> {
    use chumsky::Parser;

    let source = db.read_file(filepath);
    bozon_parser::sexp::program().parse(source.as_str())
}
