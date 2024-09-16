use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
    IResult, Parser,
};

#[derive(Debug, PartialEq, Clone)]
enum Token<'src> {
    Ident(&'src str),
    Number(f64),
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Value(Token<'src>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    FnInvoke(&'src str, Vec<Expression<'src>>),
}

fn main() {
    println!("Hello, world!");
}

fn eval(expr: Expression) -> f64 {
    match expr {
        Expression::Value(token) => match token {
            Token::Ident(ident) => panic!("Unknown name {:?}", ident),
            Token::Number(f) => f,
        },
        Expression::Add(lhs, rhs) => eval(*lhs) + eval(*rhs),
        Expression::Sub(lhs, rhs) => eval(*lhs) - eval(*rhs),
        Expression::Mul(lhs, rhs) => eval(*lhs) * eval(*rhs),
        Expression::Div(lhs, rhs) => eval(*lhs) / eval(*rhs),
        Expression::FnInvoke("sqrt", args) => unary_fn(f64::sqrt)(args),
        Expression::FnInvoke("sin", args) => unary_fn(f64::sin)(args),
        Expression::FnInvoke("cos", args) => unary_fn(f64::cos)(args),
        Expression::FnInvoke("tan", args) => unary_fn(f64::tan)(args),
        Expression::FnInvoke("asin", args) => unary_fn(f64::asin)(args),
        Expression::FnInvoke("acos", args) => unary_fn(f64::acos)(args),
        Expression::FnInvoke("atan", args) => unary_fn(f64::atan)(args),
        Expression::FnInvoke("atan2", args) => binary_fn(f64::atan2)(args),
        Expression::FnInvoke("pow", args) => binary_fn(f64::powf)(args),
        Expression::FnInvoke("exp", args) => unary_fn(f64::exp)(args),
        Expression::FnInvoke("log", args) => binary_fn(f64::log)(args),
        Expression::FnInvoke("log10", args) => unary_fn(f64::log10)(args),
        Expression::FnInvoke(name, _) => {
            panic!("Unknown function {name:?}")
        }
    }
}

fn expr(i: &str) -> IResult<&str, Expression> {
    let (i, init) = term(i)?;

    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => panic!("Additice expression shoud have '+' or '-' operator"),
        },
    )(i)
}

fn term(i: &str) -> IResult<&str, Expression> {
    let (i, init) = factor(i)?;

    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || init.clone(),
        |acc, (op, val): (char, Expression)| match op {
            '*' => Expression::Mul(Box::new(acc), Box::new(val)),
            '/' => Expression::Div(Box::new(acc), Box::new(val)),
            _ => panic!("Muitiplicative expression should have '*' or '/' operator"),
        },
    )(i)
}

fn unary_fn(f: fn(f64) -> f64) -> impl Fn(Vec<Expression>) -> f64 {
    move |args| {
        f(eval(
            args.into_iter().next().expect("function missing argument"),
        ))
    }
}

fn binary_fn(f: fn(f64, f64) -> f64) -> impl Fn(Vec<Expression>) -> f64 {
    move |args| {
        let mut itr = args.into_iter();
        let lhs = eval(itr.next().expect("function missing the first argument"));
        let rhs = eval(itr.next().expect("function missing the second argument"));
        f(lhs, rhs)
    }
}

fn func_call(i: &str) -> IResult<&str, Expression> {
    let (r, ident) = space_delimited(identifier)(i)?;
    let (r, args) = space_delimited(delimited(
        tag("("),
        many0(delimited(multispace0, expr, space_delimited(opt(tag(","))))),
        tag(")"),
    ))(r)?;
    Ok((r, Expression::FnInvoke(ident, args)))
}

fn factor(i: &str) -> IResult<&str, Expression> {
    alt((number, func_call, ident, parens))(i)
}

fn ident(i: &str) -> IResult<&str, Expression> {
    let (r, res) = space_delimited(identifier)(i)?;
    Ok((r, Expression::Value(Token::Ident(res))))
}

fn identifier(i: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(i)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = space_delimited(recognize_float)(input)?;
    Ok((
        r,
        Expression::Value(Token::Number(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?)),
    ))
}

fn parens(i: &str) -> IResult<&str, Expression> {
    space_delimited(delimited(tag("("), expr, tag(")")))(i)
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_number() {
        assert_eq!(
            number("-123.4 "),
            Ok(("", Expression::Value(Token::Number(-123.4))))
        );
    }

    #[test]
    fn test_ident() {
        assert_eq!(
            ident("hoge5 123"),
            Ok(("123", Expression::Value(Token::Ident("hoge5"))))
        );
    }

    #[test]
    fn test_add() {
        assert_eq!(
            expr("1 + (2.3 + ident) + 3"),
            Ok((
                "",
                Expression::Add(
                    Box::new(Expression::Add(
                        Box::new(Expression::Value(Token::Number(1.))),
                        Box::new(Expression::Add(
                            Box::new(Expression::Value(Token::Number(2.3))),
                            Box::new(Expression::Value(Token::Ident("ident")))
                        ))
                    )),
                    Box::new(Expression::Value(Token::Number(3.))),
                )
            ))
        )
    }

    #[test]
    fn test_eval() {
        assert_eq!(expr(" 2 + 3 * 2").map(|(_, e)| eval(e)), Ok(8.));
        assert_eq!(
            expr("10 / 5 - 5 * 2 * (1 + 2)").map(|(_, e)| eval(e)),
            Ok(-28.)
        );
        assert_eq!(expr("pow(2, 3) + 3").map(|(_, e)| eval(e)), Ok(11.))
    }
}
