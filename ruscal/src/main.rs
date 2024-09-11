#[derive(Debug, PartialEq)]
enum TokenTree<'src> {
    Token(Token<'src>),
    Tree(Vec<TokenTree<'src>>),
}

#[derive(Debug, PartialEq)]
enum Token<'src> {
    Ident(&'src str),
    Number(f64),
    LParen,
    RParen,
}

fn main() {
    println!("Hello, world!");
}

fn source(mut input: &str) -> (&str, TokenTree) {
    let mut trees = vec![];
    while !input.is_empty() {
        input = if let (next_input, Some(token)) = token(input) {
            match token {
                Token::LParen => {
                    let (i, tree) = source(next_input);
                    trees.push(tree);
                    i
                }
                Token::RParen => {
                    return (next_input, TokenTree::Tree(trees));
                }
                _ => {
                    trees.push(TokenTree::Token(token));
                    next_input
                }
            }
        } else {
            break;
        }
    }
    (whitespace(input), TokenTree::Tree(trees))
}

fn token(i: &str) -> (&str, Option<Token>) {
    if let Some((i, ident_res)) = ident(whitespace(i)) {
        return (i, Some(ident_res));
    }
    if let Some((i, number_res)) = number(whitespace(i)) {
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

fn number(mut input: &str) -> Option<(&str, Token)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '0'..='9'))) {
        input = advance_char(input);
        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
        if let Ok(num) = start[..(start.len() - input.len())].parse::<f64>() {
            return Some((input, Token::Number(num)));
        }
    }
    None
}

fn ident(mut input: &str) -> Option<(&str, Token)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            input = advance_char(input);
        }
        return Some((input, Token::Ident(&start[..(start.len() - input.len())])));
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
        assert_eq!(number("-123.4 "), Some((" ", Token::Number(-123.4))));
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("hoge5 123"), Some((" 123", Token::Ident("hoge5"))));
    }

    #[test]
    fn test_source() {
        assert_eq!(
            source("    ident  12.4 -3 token (( +0.9)((  a ))        )   "),
            (
                "",
                TokenTree::Tree(vec![
                    TokenTree::Token(Token::Ident("ident")),
                    TokenTree::Token(Token::Number(12.4)),
                    TokenTree::Token(Token::Number(-3 as f64)),
                    TokenTree::Token(Token::Ident("token")),
                    TokenTree::Tree(vec![
                        TokenTree::Tree(vec![TokenTree::Token(Token::Number(0.9)),]),
                        TokenTree::Tree(vec![TokenTree::Tree(vec![TokenTree::Token(
                            Token::Ident("a")
                        ),])])
                    ]),
                ])
            )
        )
    }
}
