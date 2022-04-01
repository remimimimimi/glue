use std::ops;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct File {
    source: String,
    ast: Vec<Atom>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Atom {
    /// May be function name, smile or even number
    Ident(String),
    String(String),
    List(Vec<Atom>),
}

// TODO: Add to parser and atom
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum DelimiterKind {
    /// ()
    Parens,
    /// {}
    Brackets,
    /// []
    Braces,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Node<T> {
    pub item: T,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Span {
    start: usize,
    /// Hope that this'll be enough for
    end: u16,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end: (end - start)
                .try_into()
                .expect("oops, span out of range of u16 number :^("),
        }
    }
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.start + usize::from(self.end)
    }
}

impl ops::Add for Span {
    type Output = Span;
    fn add(self, other: Self) -> Self::Output {
        let start = self.start().min(other.start());
        let end = if self.end() > other.end() {
            self.end()
        } else {
            other.end()
        } - start;
        Self {
            start,
            end: end
                .try_into()
                .expect("oops, span out of range of u16 number :^("),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_add() {
        let span1 = Span::new(10, 10000);
        let span2 = Span::new(100, 1000);
        assert_eq!(span1 + span2, Span::new(10, 10000));
    }
}
