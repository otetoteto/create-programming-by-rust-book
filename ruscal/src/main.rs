use std::{collections::HashMap, io::Read};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0},
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, terminated},
    Finish, IResult, Parser,
};

type Statements<'a> = Vec<Statement<'a>>;

type Variables = HashMap<String, f64>;

#[derive(Debug, Clone, PartialEq)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    For {
        loop_var: &'src str,
        start: Expression<'src>,
        end: Expression<'src>,
        stmts: Statements<'src>,
    },
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
    If(
        Box<Expression<'src>>,
        Box<Expression<'src>>,
        Option<Box<Expression<'src>>>,
    ),
}

fn main() {
    let mut buf = String::new();
    if std::io::stdin().read_to_string(&mut buf).is_ok() {
        let parsed_statements = match statements_finish(&buf) {
            Ok(parsed_statements) => parsed_statements,
            Err(e) => {
                eprintln!("Parse error {e:?}");
                return;
            }
        };

        let mut variables = HashMap::new();

        eval_stmts(&parsed_statements, &mut variables);
    }
}

fn eval_stmts(stmts: &[Statement], vars: &mut Variables) {
    for statement in stmts {
        match statement {
            Statement::Expression(expr) => {
                println!("eval: {:?}", eval(expr, vars));
            }
            Statement::VarDef(name, expr) => {
                let value = eval(expr, vars);
                vars.insert(name.to_string(), value);
            }
            Statement::VarAssign(name, expr) => {
                if !vars.contains_key(*name) {
                    panic!("Variables is not difined");
                }
                let value = eval(expr, vars);
                vars.insert(name.to_string(), value);
            }
            Statement::For {
                loop_var,
                start,
                end,
                stmts,
            } => {
                let start = eval(start, vars) as isize;
                let end = eval(end, vars) as isize;
                for i in start..end {
                    vars.insert(loop_var.to_string(), i as f64);
                    eval_stmts(stmts, vars);
                }
            }
        }
    }
}

fn statements_finish(i: &str) -> Result<Statements, nom::error::Error<&str>> {
    let (_, res) = statements(i).finish()?;
    Ok(res)
}

fn eval(expr: &Expression, vars: &Variables) -> f64 {
    use Expression::*;
    match expr {
        Value(token) => match token {
            Token::Ident(ident) => *vars.get(*ident).expect("Variable not found"),
            Token::Number(f) => *f,
        },
        Add(lhs, rhs) => eval(lhs, vars) + eval(rhs, vars),
        Sub(lhs, rhs) => eval(lhs, vars) - eval(rhs, vars),
        Mul(lhs, rhs) => eval(lhs, vars) * eval(rhs, vars),
        Div(lhs, rhs) => eval(lhs, vars) / eval(rhs, vars),
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
        If(cond, t_case, f_case) => {
            if eval(cond, vars) != 0. {
                eval(t_case, vars)
            } else if let Some(f_case) = f_case {
                eval(f_case, vars)
            } else {
                0.
            }
        }
    }
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((
        for_statement,
        terminated(alt((var_def, var_assign, expr_statement)), char(';')),
    ))(i)
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

fn for_statement(i: &str) -> IResult<&str, Statement> {
    let (i, _) = space_delimited(tag("for"))(i)?;
    let (i, loop_var) = space_delimited(identifier)(i)?;
    let (i, _) = space_delimited(tag("in"))(i)?;
    let (i, start) = space_delimited(expr)(i)?;
    let (i, _) = space_delimited(tag("to"))(i)?;
    let (i, end) = space_delimited(expr)(i)?;
    let (i, stmts) = delimited(open_brace, statements, close_brace)(i)?;
    Ok((
        i,
        Statement::For {
            loop_var,
            start,
            end,
            stmts,
        },
    ))
}

fn expr_statement(i: &str) -> IResult<&str, Statement> {
    let (i, res) = expr(i)?;
    Ok((i, Statement::Expression(res)))
}

fn statements(i: &str) -> IResult<&str, Statements> {
    let (i, stmt) = many0(statement)(i)?;
    let (i, _) = opt(char(';'))(i)?;
    Ok((i, stmt))
}

fn expr(i: &str) -> IResult<&str, Expression> {
    alt((if_expr, num_expr))(i)
}

fn if_expr(i: &str) -> IResult<&str, Expression> {
    let (i, _) = space_delimited(tag("if"))(i)?;
    let (i, cond) = expr(i)?;
    let (i, t_case) = delimited(open_brace, expr, close_brace)(i)?;
    let (i, f_case) = opt(preceded(
        space_delimited(tag("else")),
        delimited(open_brace, expr, close_brace),
    ))(i)?;

    Ok((
        i,
        Expression::If(Box::new(cond), Box::new(t_case), f_case.map(Box::new)),
    ))
}

fn num_expr(i: &str) -> IResult<&str, Expression> {
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

fn unary_fn(f: fn(f64) -> f64) -> impl Fn(&[Expression], &Variables) -> f64 {
    move |args, vars| {
        f(eval(
            args.into_iter().next().expect("function missing argument"),
            vars,
        ))
    }
}

fn binary_fn(f: fn(f64, f64) -> f64) -> impl Fn(&[Expression], &Variables) -> f64 {
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

fn open_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = space_delimited(char('{'))(i)?;
    Ok((i, ()))
}

fn close_brace(i: &str) -> IResult<&str, ()> {
    let (i, _) = space_delimited(char('}'))(i)?;
    Ok((i, ()))
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}
