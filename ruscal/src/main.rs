#[derive(Debug, PartialEq)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
}

fn main() {
    println!("Hello, world!");
}

fn expr(input: &str) -> Option<(&str, Expression)> {
    if let Some(res) = add(input) {
        return Some(res);
    }
    if let Some(res) = term(input) {
        return Some(res);
    }
    None
}

fn add(mut input: &str) -> Option<(&str, Expression)> {
    let mut left = None;
    while let Some((next_input, expr)) = add_term(input) {
        if let Some(prev_left) = left {
            left = Some(Expression::Add(Box::new(prev_left), Box::new(expr)));
        } else {
            left = Some(expr);
        }
        input = next_input
    }

    let left = left?;
    let (next_input, rhs) = expr(input)?;

    Some((next_input, Expression::Add(Box::new(left), Box::new(rhs))))
}

fn term(input: &str) -> Option<(&str, Expression)> {
    if let Some(res) = paren(input) {
        return Some(res);
    }
    if let Some(res) = token(input) {
        return Some(res);
    }
    None
}

fn add_term(input: &str) -> Option<(&str, Expression)> {
    let (next_input, lhs) = term(input)?;
    let next_input = plus(whitespace(next_input))?;
    Some((next_input, lhs))
}

fn paren(input: &str) -> Option<(&str, Expression)> {
    let next_input = lparen(whitespace(input))?;
    let (next_input, expr) = expr(next_input)?;
    let next_input = rparen(whitespace(next_input))?;
    Some((next_input, expr))
}

fn token(input: &str) -> Option<(&str, Expression)> {
    if let Some(res) = ident(whitespace(input)) {
        return Some(res);
    }
    if let Some(res) = number(whitespace(input)) {
        return Some(res);
    }
    None
}

fn lparen(mut input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some('(')) {
        input = advance_char(input);
        return Some(input);
    }
    None
}

fn rparen(mut input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some(')')) {
        input = advance_char(input);
        return Some(input);
    }
    None
}

fn whitespace(mut input: &str) -> &str {
    while matches!(peek_char(input), Some(' ')) {
        input = advance_char(input)
    }
    input
}

fn number(mut input: &str) -> Option<(&str, Expression)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '0'..='9'))) {
        input = advance_char(input);
        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
        if let Ok(num) = start[..(start.len() - input.len())].parse::<f64>() {
            return Some((input, Expression::NumLiteral(num)));
        }
    }
    None
}

fn ident(mut input: &str) -> Option<(&str, Expression)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            input = advance_char(input);
        }
        return Some((
            input,
            Expression::Ident(&start[..(start.len() - input.len())]),
        ));
    }
    None
}

fn plus(input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some('+')) {
        return Some(advance_char(input));
    }
    None
}

fn advance_char(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}

fn peek_char(input: &str) -> Option<char> {
    input.chars().next()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace("    "), "");
    }

    #[test]
    fn test_number() {
        assert_eq!(
            number("-123.4 "),
            Some((" ", Expression::NumLiteral(-123.4)))
        );
    }

    #[test]
    fn test_ident() {
        assert_eq!(
            ident("hoge5 123"),
            Some((" 123", Expression::Ident("hoge5")))
        );
    }

    #[test]
    fn test_add() {
        assert_eq!(
            expr("1 + (2.3 + ident) + 3"),
            Some((
                "",
                Expression::Add(
                    Box::new(Expression::Add(
                        Box::new(Expression::NumLiteral(1.)),
                        Box::new(Expression::Add(
                            Box::new(Expression::NumLiteral(2.3)),
                            Box::new(Expression::Ident("ident"))
                        ))
                    )),
                    Box::new(Expression::NumLiteral(3.)),
                )
            ))
        )
    }
}
