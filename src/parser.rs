use chumsky::{error, prelude::*};

use crate::ast::*;

fn prefix() -> impl Parser<char, Atom, Error = Simple<char>> {
    just('\'')
        .map(|_| Atom::Ident("quote".into()))
        .or(just('`').map(|_| Atom::Ident("quasiquote".into())))
        .or(just(',').map(|_| Atom::Ident("unquote".into())))
        .or(just(",@").map(|_| Atom::Ident("unquote-splicing".into())))
        .or(just("#'").map(|_| Atom::Ident("syntax".into())))
        .or(just("#`").map(|_| Atom::Ident("quasi-syntax".into())))
        .or(just("#,").map(|_| Atom::Ident("unsyntax".into())))
        .or(just("#,@").map(|_| Atom::Ident("unsyntax-splicing".into())))
        .labelled("prefix")
}

fn ident() -> impl Parser<char, Atom, Error = Simple<char>> {
    // XXX: Maybe include more whitespace symbols?
    none_of::<_, _, Simple<char>>("()[]{}\t\n\r ")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Atom::Ident)
        .labelled("ident")
}

fn string() -> impl Parser<char, Atom, Error = Simple<char>> {
    none_of::<_, _, Simple<char>>("\"")
        .repeated()
        .delimited_by(just('"'), just('"'))
        .collect::<String>()
        .map(Atom::String)
        .labelled("string")
}

fn inline_prefix((p, a): (Option<Atom>, Atom)) -> Atom {
    match (p, a) {
        (Some(a1), a2 @ Atom::Ident(_) | a2 @ Atom::String(_)) => Atom::List(vec![a1, a2]),
        (Some(a), Atom::List(l)) => {
            Atom::List(vec![vec![a], l].iter().cloned().flatten().collect())
        }
        (_, a) => a,
    }
}

fn program() -> impl Parser<char, Vec<Atom>, Error = Simple<char>> {
    let sexp = recursive(|l| {
        prefix()
            .or_not()
            .then(
                l.separated_by(text::whitespace())
                    .delimited_by(just('(').padded(), just(')').padded())
                    .map(Atom::List)
                    .or(string())
                    .or(ident()),
            )
            .padded()
            .map(|a| inline_prefix(a))
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
        assert_eq!(program().parse("()"), Ok(vec![Atom::List(vec![])]));
        assert_eq!(program().parse("( )"), Ok(vec![Atom::List(vec![])]));
        assert_eq!(
            super::program().parse("\t\r\n (\t\r\n )"),
            Ok(vec![Atom::List(vec![])])
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
            Ok(Atom::String(r#"hellow"#.into())),
        );
        assert_eq!(
            super::string().parse(r#""Uwu" "#),
            Ok(Atom::String(r#"Uwu"#.into())),
        );
        assert_eq!(
            super::string().parse(r#""Hello\nWorld!""#),
            Ok(Atom::String(r#"Hello\nWorld!"#.into())),
        );
    }

    #[test]
    fn ident() {
        assert_eq!(
            super::ident().parse("hellow"),
            Ok(Atom::Ident("hellow".into())),
        );
        assert_eq!(super::ident().parse("1"), Ok(Atom::Ident("1".into())),);
        assert_eq!(super::ident().parse("42 "), Ok(Atom::Ident("42".into())),);
    }

    #[test]
    fn list_whitespaces() {
        use super::program;

        assert_eq!(
            program().parse(r#"( "Hello")"#),
            Ok(vec![Atom::List(vec![Atom::String("Hello".into())])])
        );

        assert_eq!(
            program().parse("( 1)"),
            Ok(vec![Atom::List(vec![Atom::Ident("1".into()),])])
        );

        assert_eq!(
            program().parse("(1 )"),
            Ok(vec![Atom::List(vec![Atom::Ident("1".into()),])])
        );

        assert_eq!(
            program().parse("( 1 )"),
            Ok(vec![Atom::List(vec![Atom::Ident("1".into()),])])
        );
    }
}
