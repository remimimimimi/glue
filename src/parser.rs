use chumsky::{error, prelude::*};

use crate::ast::*;

fn prefix() -> impl Parser<char, PrefixKind, Error = Simple<char>> {
    just('\'')
        .map(|_| PrefixKind::Quote)
        .or(just('`').map(|_| PrefixKind::QuasiQuote))
        .or(just(',').map(|_| PrefixKind::Unquote))
        .or(just(",@").map(|_| PrefixKind::UnquoteSplicing))
        .or(just("#'").map(|_| PrefixKind::Syntax))
        .or(just("#`").map(|_| PrefixKind::QuasiSyntax))
        .or(just("#,").map(|_| PrefixKind::Unsyntax))
        .or(just("#,@").map(|_| PrefixKind::UnsyntaxSplicing))
        .labelled("prefix")
}

fn ident() -> impl Parser<char, AtomKind, Error = Simple<char>> {
    // XXX: Maybe include more whitespace symbols?
    none_of::<_, _, Simple<char>>("()[]{}\t\n\r ")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(AtomKind::Ident)
        .labelled("ident")
}

fn string() -> impl Parser<char, AtomKind, Error = Simple<char>> {
    none_of::<_, _, Simple<char>>("\"")
        .repeated()
        .delimited_by(just('"'), just('"'))
        .collect::<String>()
        .map(AtomKind::String)
        .labelled("string")
}

fn program() -> impl Parser<char, Vec<Atom>, Error = Simple<char>> {
    let sexp = recursive(|l| {
        prefix()
            .or_not()
            .then(
                l.clone()
                    .separated_by(text::whitespace())
                    .delimited_by(just('(').padded(), just(')').padded())
                    .map(|lst| AtomKind::List(lst, DelimiterKind::Parens))
                    .or(l
                        .clone()
                        .separated_by(text::whitespace())
                        .delimited_by(just('{').padded(), just('}').padded())
                        .map(|lst| AtomKind::List(lst, DelimiterKind::Brackets)))
                    .or(l
                        .separated_by(text::whitespace())
                        .delimited_by(just('[').padded(), just(']').padded())
                        .map(|lst| AtomKind::List(lst, DelimiterKind::Braces)))
                    .or(string())
                    .or(ident()),
            )
            .map_with_span(|(prefix, kind), span| Atom {
                prefix,
                kind,
                span: span.into(),
            })
            .padded()
    });

    sexp.separated_by(text::whitespace()).collect::<Vec<Atom>>()
}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use chumsky::Parser;

    #[test]
    fn simple() {
        use super::program;
        assert_eq!(
            program().parse("()"),
            Ok(vec![Atom {
                prefix: None,
                kind: AtomKind::List(vec![], DelimiterKind::Parens),
                span: Span::new(0, 2)
            }])
        );

        assert_eq!(
            program().parse("( )"),
            Ok(vec![Atom {
                prefix: None,
                kind: AtomKind::List(vec![], DelimiterKind::Parens),
                span: Span::new(0, 3)
            }])
        );

        assert_eq!(
            super::program().parse("\t\r\n (\t\r\n )"),
            Ok(vec![Atom {
                prefix: None,
                kind: AtomKind::List(vec![], DelimiterKind::Parens),
                span: Span::new(4, 10)
            }])
        );
    }

    #[quickcheck]
    fn prefix(input: String) -> bool {
        (input.starts_with('\'')
            || input.starts_with('`')
            || input.starts_with(',')
            || input.starts_with(",@")
            || input.starts_with("#'")
            || input.starts_with("#`")
            || input.starts_with("#,")
            || input.starts_with("#,@")) as u8
            <= super::prefix().parse(input).is_ok() as u8
    }

    #[test]
    fn string() {
        assert_eq!(
            super::string().parse(r#""hellow""#),
            Ok(AtomKind::String(r#"hellow"#.into())),
        );

        assert_eq!(
            super::string().parse(r#""Uwu" "#),
            Ok(AtomKind::String(r#"Uwu"#.into())),
        );

        assert_eq!(
            super::string().parse(r#""Hello\nWorld!""#),
            Ok(AtomKind::String(r#"Hello\nWorld!"#.into())),
        );

        assert_eq!(
            super::string()
                .map_with_span(|kind, span| Atom::new(None, kind, span))
                .parse(r#""""#),
            Ok(Atom {
                prefix: None,
                kind: AtomKind::String(r#""#.into()),
                span: Span::new(0, 2)
            }),
        );
    }

    #[test]
    fn ident() {
        assert_eq!(
            super::ident().parse("hellow"),
            Ok(AtomKind::Ident("hellow".into())),
        );
        assert_eq!(super::ident().parse("1"), Ok(AtomKind::Ident("1".into())),);
        assert_eq!(
            super::ident().parse("42 "),
            Ok(AtomKind::Ident("42".into())),
        );
    }

    #[test]
    fn list_whitespaces() {
        use super::program;

        assert_eq!(
            program().parse(r#"( "Hello")"#),
            Ok(vec![Atom {
                prefix: None,
                kind: AtomKind::List(
                    vec![Atom {
                        prefix: None,
                        kind: AtomKind::String("Hello".into()),
                        span: Span::new(2, 9)
                    }],
                    DelimiterKind::Parens
                ),
                span: Span::new(0, 10)
            }])
        );

        assert_eq!(
            program().parse("(1)"),
            Ok(vec![Atom {
                prefix: None,
                kind: AtomKind::List(
                    vec![Atom {
                        prefix: None,
                        kind: AtomKind::Ident("1".into()),
                        span: Span::new(1, 2)
                    }],
                    DelimiterKind::Parens
                ),
                span: Span::new(0, 3)
            }])
        );

        assert_eq!(
            program().parse("( 1)"),
            Ok(vec![Atom {
                prefix: None,
                kind: AtomKind::List(
                    vec![Atom {
                        prefix: None,
                        kind: AtomKind::Ident("1".into()),
                        span: Span::new(2, 3)
                    }],
                    DelimiterKind::Parens
                ),
                span: Span::new(0, 4)
            }])
        );

        assert_eq!(
            program().parse("(1 )"),
            Ok(vec![Atom {
                prefix: None,
                kind: AtomKind::List(
                    vec![Atom {
                        prefix: None,
                        kind: AtomKind::Ident("1".into()),
                        span: Span::new(1, 2)
                    }],
                    DelimiterKind::Parens
                ),
                span: Span::new(0, 4)
            }])
        );

        assert_eq!(
            program().parse("( 1 )"),
            Ok(vec![Atom {
                prefix: None,
                kind: AtomKind::List(
                    vec![Atom {
                        prefix: None,
                        kind: AtomKind::Ident("1".into()),
                        span: Span::new(2, 3)
                    }],
                    DelimiterKind::Parens
                ),
                span: Span::new(0, 5)
            }])
        );
    }
}
