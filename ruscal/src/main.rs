use std::{collections::HashMap, io::Read};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
    Finish, IResult, Parser,
};

type Statements<'a> = Vec<Statement<'a>>;

#[derive(Debug, Clone, PartialEq)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
}

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
    let mut buf = String::new();
    if std::io::stdin().read_to_string(&mut buf).is_ok() {
        let parsed_statements = match statements(&buf) {
            Ok(parsed_statements) => parsed_statements,
            Err(e) => {
                eprintln!("Parse error {e:?}");
                return;
            }
        };

        let mut variables = HashMap::new();

        for statement in parsed_statements {
            match statement {
                Statement::Expression(expr) => {
                    println!("eval: {:?}", eval(expr, &variables));
                }
                Statement::VarDef(name, expr) => {
                    let value = eval(expr, &variables);
                    variables.insert(name, value);
                }
                Statement::VarAssign(name, expr) => {
                    if !variables.contains_key(name) {
                        panic!("Variables is not difined");
                    }
                    let value = eval(expr, &variables);
                    variables.insert(name, value);
                }
            }
        }
    }
}

fn eval(expr: Expression, vars: &HashMap<&str, f64>) -> f64 {
    use Expression::*;
    match expr {
        Value(token) => match token {
            Token::Ident(ident) => *vars.get(ident).expect("Variable not found"),
            Token::Number(f) => f,
        },
        Add(lhs, rhs) => eval(*lhs, vars) + eval(*rhs, vars),
        Sub(lhs, rhs) => eval(*lhs, vars) - eval(*rhs, vars),
        Mul(lhs, rhs) => eval(*lhs, vars) * eval(*rhs, vars),
        Div(lhs, rhs) => eval(*lhs, vars) / eval(*rhs, vars),
        FnInvoke("sqrt", args) => unary_fn(f64::sqrt)(args, vars),
        FnInvoke("sin", args) => unary_fn(f64::sin)(args, vars),
        FnInvoke("cos", args) => unary_fn(f64::cos)(args, vars),
        FnInvoke("tan", args) => unary_fn(f64::tan)(args, vars),
        FnInvoke("asin", args) => unary_fn(f64::asin)(args, vars),
        FnInvoke("acos", args) => unary_fn(f64::acos)(args, vars),
        FnInvoke("atan", args) => unary_fn(f64::atan)(args, vars),
        FnInvoke("atan2", args) => binary_fn(f64::atan2)(args, vars),
        FnInvoke("pow", args) => binary_fn(f64::powf)(args, vars),
        FnInvoke("exp", args) => unary_fn(f64::exp)(args, vars),
        FnInvoke("log", args) => binary_fn(f64::log)(args, vars),
        FnInvoke("log10", args) => unary_fn(f64::log10)(args, vars),
        FnInvoke(name, _) => {
            panic!("Unknown function {name:?}")
        }
    }
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((var_def, var_assign, expr_statement))(i)
}

fn var_def(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("var"))(i)?;
    let (i, ident) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    Ok((i, Statement::VarDef(ident, expr)))
}

fn var_assign(i: &str) -> IResult<&str, Statement> {
    let (i, ident) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(char('='))(i)?;
    let (i, expr) = space_delimited(expr)(i)?;
    Ok((i, Statement::VarAssign(ident, expr)))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
    let (i, res) = expr(i)?;
    Ok((i, Statement::Expression(res)))
}

fn statements(i: &str) -> Result<Statements, nom::error::Error<&str>> {
    let (_, res) = separated_list0(tag(";"), statement)(i).finish()?;
    Ok(res)
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

fn unary_fn(f: fn(f64) -> f64) -> impl Fn(Vec<Expression>, &HashMap<&str, f64>) -> f64 {
    move |args, vars| {
        f(eval(
            args.into_iter().next().expect("function missing argument"),
            vars,
        ))
    }
}

fn binary_fn(f: fn(f64, f64) -> f64) -> impl Fn(Vec<Expression>, &HashMap<&str, f64>) -> f64 {
    move |args, vars| {
        let mut itr = args.into_iter();
        let lhs = eval(
            itr.next().expect("function missing the first argument"),
            vars,
        );
        let rhs = eval(
            itr.next().expect("function missing the second argument"),
            vars,
        );
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
