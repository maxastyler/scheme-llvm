use std::str::Bytes;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'a> {
    Number(f64),
    Identifier(&'a str),
    Nil,
}

impl<'a> From<f64> for Atom<'a> {
    fn from(value: f64) -> Self {
        Atom::Number(value)
    }
}

impl<'a> From<f64> for Expression<'a> {
    fn from(value: f64) -> Self {
        Expression::Atom(Atom::Number(value))
    }
}

impl<'a> From<&'a str> for Atom<'a> {
    fn from(value: &'a str) -> Self {
        Atom::Identifier(value)
    }
}

impl<'a> From<&'a str> for Expression<'a> {
    fn from(value: &'a str) -> Self {
        Expression::Atom(Atom::Identifier(value))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    Define {
        identifier: &'a str,
        value: Box<Expression<'a>>,
    },
    Let {
        pairs: Vec<(&'a str, Expression<'a>)>,
        body: Box<Expression<'a>>,
    },
    Lambda {
        arguments: Vec<&'a str>,
        body: Box<Expression<'a>>,
    },
    Begin(Vec<Expression<'a>>),
    List(Vec<Expression<'a>>),
    Atom(Atom<'a>),
}

fn parse_identifier<'a>(input: &'a str) -> Option<(&'a str, &'a str)> {
    let (id, input) = split_to_first_whitespace(input)?;
    if id.bytes().next()?.is_ascii_alphabetic() {
        Some((id, input))
    } else {
        None
    }
}

fn split_to_first_whitespace<'a>(input: &'a str) -> Option<(&'a str, &'a str)> {
    let mut split_index = input
        .bytes()
        .take_while(|b| !b.is_ascii_whitespace() && b != &(')' as u8))
        .count();
    if split_index == 0 {
        None
    } else {
        Some(input.split_at(split_index))
    }
}

fn parse_expression<'a>(input: &'a str) -> Option<(Expression<'a>, &'a str)> {
    parse_bracketed(input).or_else(|| parse_atom(input).map(|(e, i)| (Expression::Atom(e), i)))
}

fn parse_atom<'a>(input: &'a str) -> Option<(Atom<'a>, &'a str)> {
    let (value, input) = split_to_first_whitespace(input)?;
    let b = value.bytes().next()?;
    if b.is_ascii_digit() {
        Some((value.parse().ok().map(Atom::Number)?, input))
    } else {
        Some((Atom::Identifier(value), input))
    }
}

fn parse_define<'a>(input: &'a str) -> Option<(Expression<'a>, &'a str)> {
    let (start, input) = split_to_first_whitespace(input)?;
    if start != "define" {
        return None;
    }
    let ((ident, value), input) = parse_binding_pair(input.trim_start())?;
    Some((
        Expression::Define {
            identifier: ident,
            value: Box::new(value),
        },
        input,
    ))
}

fn parse_binding_pair<'a>(input: &'a str) -> Option<((&'a str, Expression<'a>), &'a str)> {
    let (ident, input) = parse_identifier(input)?;
    let (value, input) = parse_expression(input.trim_start())?;
    Some(((ident, value), input))
}

fn parse_let<'a>(input: &'a str) -> Option<(Expression<'a>, &'a str)> {
    let (start, input) = split_to_first_whitespace(input)?;
    if start != "let" {
        return None;
    }
    let bindings_fun = |mut i: &'a str| {
        let mut bindings = vec![];
        loop {
            match bracketed(parse_binding_pair, i.trim_start()) {
                Some(((ident, value), new_i)) => {
                    bindings.push((ident, value));
                    i = new_i;
                }
                None => break,
            }
        }
        Some((bindings, i))
    };
    let (bindings, input) = bracketed(bindings_fun, input.trim_start())?;
    let (body, input) = parse_expression(input.trim_start())?;
    Some((
        Expression::Let {
            pairs: bindings,
            body: Box::new(body),
        },
        input,
    ))
}
fn parse_lambda<'a>(input: &'a str) -> Option<(Expression<'a>, &'a str)> {
    let (start, input) = split_to_first_whitespace(input)?;
    if start != "lambda" {
        return None;
    }
    let args_fun = |mut i: &'a str| {
        let mut args = vec![];
        loop {
            match parse_identifier(i.trim_start()) {
                Some((ident, new_i)) => {
                    args.push(ident);
                    i = new_i;
                }
                None => break,
            }
        }
        Some((args, i))
    };
    let (args, input) = bracketed(args_fun, input.trim_start())?;
    let (body, input) = parse_expression(input.trim_start())?;
    Some((
        Expression::Lambda {
            arguments: args,
            body: Box::new(body),
        },
        input,
    ))
}
fn parse_begin<'a>(input: &'a str) -> Option<(Expression<'a>, &'a str)> {
    let (start, input) = split_to_first_whitespace(input)?;
    if start != "begin" {
        return None;
    }
    let (exprs, input) = parse_list(input.trim_start())?;
    if exprs.is_empty() {
        None
    } else {
        Some((Expression::Begin(exprs), input))
    }
}

pub fn parse_list<'a>(mut i: &'a str) -> Option<(Vec<Expression<'a>>, &'a str)> {
    let mut exprs = vec![];
    loop {
        match parse_expression(i.trim_start()) {
            Some((expr, new_i)) => {
                exprs.push(expr);
                i = new_i;
            }
            None => break,
        }
    }
    Some((exprs, i))
}

fn bracketed<'a, A, F>(f: F, input: &'a str) -> Option<(A, &'a str)>
where
    F: Fn(&'a str) -> Option<(A, &'a str)>,
{
    let input = input.strip_prefix("(")?.trim_start();
    let (res, input) = f(input)?;
    let input = input.trim_start().strip_prefix(")")?;
    Some((res, input))
}

fn parse_bracketed<'a>(input: &'a str) -> Option<(Expression<'a>, &'a str)> {
    bracketed(
        |input| {
            parse_define(input)
                .or_else(|| parse_let(input))
                .or_else(|| parse_lambda(input))
                .or_else(|| parse_begin(input))
                .or_else(|| parse_list(input).map(|(es, i)| (Expression::List(es), i)))
        },
        input,
    )
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_atom() {
        assert_eq!(parse_atom("3 3240 "), Some((Atom::Number(3.0), " 3240 ")));
        assert_eq!(
            parse_atom("+ 3240 "),
            Some((Atom::Identifier("+"), " 3240 "))
        );
        assert_eq!(parse_atom("32aa"), None);
    }

    #[test]
    fn test_bracketed() {
        assert_eq!(bracketed(|i| Some(("hi", i)), "()  "), Some(("hi", "  ")));
        assert_eq!(
            bracketed(parse_atom, "(3.0)  "),
            Some((Atom::Number(3f64), "  "))
        );
        assert_eq!(
            bracketed(parse_atom, "(ooh3)  "),
            Some((Atom::Identifier("ooh3"), "  "))
        );
    }

    #[test]
    fn test_begin() {
        assert_eq!(
            parse_begin("begin 3 4 )"),
            Some((Expression::Begin(vec![3.0.into(), 4.0.into()]), " )"))
        );
        assert_eq!(parse_begin("begin )"), None);
    }

    #[test]
    fn test_let() {
        assert_eq!(
            parse_let("let ((x 3) (y 4)) 10) =="),
            Some((
                Expression::Let {
                    pairs: vec![("x", 3.0.into()), ("y", 4.0.into())],
                    body: Box::new(10.0.into())
                },
                ") =="
            ))
        );
    }

    #[test]
    fn test_parse_list() {
        assert_eq!(
            parse_list(
                "(define x 3) (let ((y 3)) 4)(let () 4) (begin 3 2) 1 (lambda (x y) (* x y))"
            ),
            Some((
                vec![
                    Expression::Define {
                        identifier: "x",
                        value: Box::new(3.0.into())
                    },
                    Expression::Let {
                        pairs: vec![("y", 3.0.into())],
                        body: Box::new(4.0.into())
                    },
                    Expression::Let {
                        pairs: vec![],
                        body: Box::new(4.0.into())
                    },
                    Expression::Begin(vec![3.0.into(), 2.0.into()]),
                    1.0.into(),
                    Expression::Lambda {
                        arguments: vec!["x", "y"],
                        body: Box::new(Expression::List(vec!["*".into(), "x".into(), "y".into()]))
                    }
                ],
                ""
            ))
        );
        assert_eq!(
            parse_list(
                "(define x 3) (let ((y 3)) 4)(let () 4) (begin 3 2) 1 (lambda (x y) (* x y))"
            ),
            Some((
                vec![
                    Expression::Define {
                        identifier: "x",
                        value: Box::new(3.0.into())
                    },
                    Expression::Let {
                        pairs: vec![("y", 3.0.into())],
                        body: Box::new(4.0.into())
                    },
                    Expression::Let {
                        pairs: vec![],
                        body: Box::new(4.0.into())
                    },
                    Expression::Begin(vec![3.0.into(), 2.0.into()]),
                    1.0.into(),
                    Expression::Lambda {
                        arguments: vec!["x", "y"],
                        body: Box::new(Expression::List(vec!["*".into(), "x".into(), "y".into()]))
                    }
                ],
                ""
            ))
        );
    }
}
