use std::ops;

use bozon_span::Span;

pub type Ast = Vec<Atom>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Atom {
    pub prefix: Option<PrefixKind>,
    pub kind: AtomKind,
    pub span: Span,
}

impl Atom {
    pub fn new(prefix: Option<PrefixKind>, kind: AtomKind, span: ops::Range<usize>) -> Self {
        Atom {
            prefix,
            kind,
            span: Span::new(span.start, span.end),
        }
    }
}

#[non_exhaustive]
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum PrefixKind {
    /// '
    Quote,
    /// `
    QuasiQuote,
    /// ,
    Unquote,
    /// ,@
    UnquoteSplicing,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum AtomKind {
    /// May be function name, smile or even number
    Ident(String),
    String(String),
    List(Vec<Atom>, BracketKind),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum BracketKind {
    /// ()
    Round,
    /// {}
    Curly,
    /// []
    Square,
}
