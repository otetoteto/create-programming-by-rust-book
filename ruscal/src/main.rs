#[derive(Debug, PartialEq, Eq)]
enum Token {
    Ident,
    Number,
    LParen,
    RParen,
}

fn main() {
    println!("Hello, world!");
}

fn source(mut input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    while !input.is_empty() {
        input = if let (next_input, Some(token)) = token(input) {
            tokens.push(token);
            next_input
        } else {
            break;
        }
    }
    tokens
}

fn token(i: &str) -> (&str, Option<Token>) {
    if let (i, Some(ident_res)) = ident(whitespace(i)) {
        return (i, Some(ident_res));
    }
    if let (i, Some(number_res)) = number(whitespace(i)) {
        return (i, Some(number_res));
    }
    if let (i, Some(lparen_res)) = lparen(whitespace(i)) {
        return (i, Some(lparen_res));
    }
    if let (i, Some(rparen_res)) = rparen(whitespace(i)) {
        return (i, Some(rparen_res));
    }
    (i, None)
}

fn lparen(mut input: &str) -> (&str, Option<Token>) {
    if matches!(peek_char(input), Some('(')) {
        input = advance_char(input);
        return (input, Some(Token::LParen));
    }
    (input, None)
}

fn rparen(mut input: &str) -> (&str, Option<Token>) {
    if matches!(peek_char(input), Some(')')) {
        input = advance_char(input);
        return (input, Some(Token::RParen));
    }
    (input, None)
}

fn whitespace(mut input: &str) -> &str {
    while matches!(peek_char(input), Some(' ')) {
        input = advance_char(input)
    }
    input
}

fn number(mut input: &str) -> (&str, Option<Token>) {
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '0'..='9'))) {
        input = advance_char(input);
        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
        (input, Some(Token::Number))
    } else {
        (input, None)
    }
}

fn ident(mut input: &str) -> (&str, Option<Token>) {
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            input = advance_char(input);
        }
        (input, Some(Token::Ident))
    } else {
        (input, None)
    }
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
        assert_eq!(number("-123.4 "), (" ", Some(Token::Number)));
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("hoge5 123"), (" 123", Some(Token::Ident)));
    }

    #[test]
    fn test_source() {
        assert_eq!(
            source("    ident  12.4 -3 token (( +0.9)((  a ))        )"),
            vec![
                Token::Ident,
                Token::Number,
                Token::Number,
                Token::Ident,
                Token::LParen,
                Token::LParen,
                Token::Number,
                Token::RParen,
                Token::LParen,
                Token::LParen,
                Token::Ident,
                Token::RParen,
                Token::RParen,
                Token::RParen
            ]
        )
    }
}
