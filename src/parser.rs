use std::str::Bytes;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'a> {
    Number(f64),
    Operator(&'a str),
    Identifier(&'a str),
    Nil,
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
    let mut b_iter = input.bytes().enumerate();
    if b_iter.next()?.1.is_ascii_alphabetic() {
        let index = b_iter
            .skip_while(|(_, c)| !c.is_ascii_whitespace())
            .next()?
            .0;
        Some(input.split_at(index))
    } else {
        None
    }
}

fn parse_expression<'a>(input: &'a str) -> Option<(Expression<'a>, &'a str)> {
    None
}

fn parse_atom<'a>(input: &'a str) -> Option<(Atom<'a>, &'a str)> {
    todo!()
}

fn parse_define<'a>(input: &'a str) -> Option<(Expression<'a>, &'a str)> {
    let input = input.strip_prefix("define")?.trim_start();
    let ((ident, value), input) = parse_binding_pair(input)?;
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
    let input = input.strip_prefix("define")?.trim_start();
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
        if bindings.is_empty() {
            None
        } else {
            Some((bindings, i))
        }
    };
    let (bindings, input) = bracketed(bindings_fun, input)?;
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
    let input = input.strip_prefix("lambda")?.trim_start();
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
    let (args, input) = bracketed(args_fun, input)?;
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
    todo!()
}
fn parse_list<'a>(input: &'a str) -> Option<(Expression<'a>, &'a str)> {
    todo!()
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
                .or_else(|| parse_list(input))
        },
        input,
    )
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_parse_bracketed() {
        assert_eq!(parse_bracketed("(define x 3)"), None);
    }
}
