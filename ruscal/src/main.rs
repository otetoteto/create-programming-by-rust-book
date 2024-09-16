use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::recognize,
    multi::{fold_many0, many0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
    IResult,
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
}

fn main() {
    println!("Hello, world!");
}

fn term(i: &str) -> IResult<&str, Expression> {
    alt((number, ident, parens))(i)
}

fn ident(i: &str) -> IResult<&str, Expression> {
    let (r, res) = delimited(multispace0, identifier, multispace0)(i)?;
    Ok((r, Expression::Value(Token::Ident(res))))
}

fn identifier(i: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(i)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = delimited(multispace0, recognize_float, multispace0)(input)?;
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
    delimited(
        multispace0,
        delimited(tag("("), expr, tag(")")),
        multispace0,
    )(i)
}

fn expr(i: &str) -> IResult<&str, Expression> {
    let (i, init) = term(i)?;

    fold_many0(
        pair(delimited(multispace0, char('+'), multispace0), term),
        move || init.clone(),
        |acc, (_op, val): (char, Expression)| Expression::Add(Box::new(acc), Box::new(val)),
    )(i)
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
}
