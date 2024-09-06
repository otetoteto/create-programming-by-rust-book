use std::{
    collections::HashMap,
    io::{BufRead, BufReader},
};

struct Vm {
    stack: Vec<Value>,
    vars: HashMap<String, Value>,
    blocks: Vec<Vec<Value>>,
}

impl Vm {
    fn new() -> Self {
        Self {
            stack: vec![],
            vars: HashMap::new(),
            blocks: vec![],
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Value {
    Num(i32),
    Op(String),
    Block(Vec<Value>),
    Sym(String),
}

impl Value {
    fn as_num(&self) -> i32 {
        match self {
            Self::Num(val) => *val,
            _ => panic!("Value is not a number"),
        }
    }

    fn to_block(self) -> Vec<Value> {
        match self {
            Self::Block(val) => val,
            _ => panic!("Value is not a block"),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Value::Num(i) => i.to_string(),
            Value::Op(s) | Value::Sym(s) => s.clone(),
            Value::Block(_) => "<Block>".to_string(),
        }
    }
}

fn main() {
    if let Some(f) = std::env::args()
        .nth(1)
        .and_then(|f| std::fs::File::open(f).ok())
    {
        parse_batch(BufReader::new(f));
    } else {
        parse_interactive();
    }
}

fn parse_batch(source: impl BufRead) -> Vec<Value> {
    let mut vm = Vm::new();
    for line in source.lines().flatten() {
        for word in line.split(" ") {
            parse_word(word, &mut vm);
        }
    }
    vm.stack
}

fn parse_interactive() {
    let mut vm = Vm::new();
    for line in std::io::stdin().lines().flatten() {
        for word in line.split(" ") {
            parse_word(word, &mut vm);
        }
        println!("stack: {:?}", vm.stack);
    }
}

fn parse_word(word: &str, vm: &mut Vm) {
    if word.is_empty() {
        return;
    }
    if word == "{" {
        vm.blocks.push(vec![]);
    } else if word == "}" {
        let top_block = vm.blocks.pop().expect("Block stack underrun!");
        eval(Value::Block(top_block), vm);
    } else {
        let code = if let Ok(i) = word.parse::<i32>() {
            Value::Num(i)
        } else if word.starts_with("/") && word != "/" {
            Value::Sym(word[1..].to_string())
        } else {
            Value::Op(word.to_string())
        };
        eval(code, vm);
    }
}

fn eval(code: Value, vm: &mut Vm) {
    // block が続く間はその block stack に値を積み上げていく
    // block の中身が評価されるのは現状 if オペレーションの時のみ (遅延実行される)
    if let Some(top_block) = vm.blocks.last_mut() {
        top_block.push(code);
        return;
    }
    match code {
        Value::Op(ref op) => match op as &str {
            "+" => add(&mut vm.stack),
            "-" => sub(&mut vm.stack),
            "*" => mul(&mut vm.stack),
            "/" => div(&mut vm.stack),
            "<" => lt(&mut vm.stack),
            "if" => op_if(vm),
            "def" => op_def(vm),
            "puts" => puts(vm),
            _ => {
                let val = vm
                    .vars
                    .get(op)
                    .expect(&format!("{op:?} is not a defined operation"));
                vm.stack.push(val.clone());
            }
        },
        _ => vm.stack.push(code.clone()),
    }
}

fn op_if(vm: &mut Vm) {
    let false_branch = vm.stack.pop().unwrap().to_block();
    let true_branch = vm.stack.pop().unwrap().to_block();
    let cond = vm.stack.pop().unwrap().to_block();

    for code in cond {
        eval(code, vm);
    }

    let cond_result = vm.stack.pop().unwrap().as_num();

    if cond_result != 0 {
        for code in true_branch {
            eval(code, vm);
        }
    } else {
        for code in false_branch {
            eval(code, vm);
        }
    }
}

fn op_def(vm: &mut Vm) {
    let val = vm.stack.pop().unwrap();
    let sym = vm.stack.pop().unwrap().to_string();

    vm.vars.insert(sym, val);
}

fn puts(vm: &mut Vm) {
    let val = vm.stack.pop().unwrap();
    println!("{}", val.to_string());
}

macro_rules! impl_op {
    {$name :ident, $op:tt} => {
        fn $name(stack: &mut Vec<Value>) {
            let r = stack.pop().unwrap().as_num();
            let l = stack.pop().unwrap().as_num();
            stack.push(Value::Num((l $op r) as i32));
        }
    };
}

impl_op!(add, +);
impl_op!(sub, -);
impl_op!(mul, *);
impl_op!(div, /);
impl_op!(lt, <);

#[cfg(test)]
mod test {
    use super::{Value::*, *};
    use std::io::Cursor;

    fn parse(input: &str) -> Vec<Value> {
        parse_batch(Cursor::new(input))
    }

    #[test]
    fn test_group() {
        assert_eq!(
            parse("1 2 + { 3 4 }"),
            vec![Num(3), Block(vec![Num(3), Num(4)])]
        );
    }

    #[test]
    fn test_if_false() {
        assert_eq!(parse("{ 1 -1 + } { 100 } { -100 } if"), vec![Num(-100)]);
    }

    #[test]
    fn test_if_true() {
        assert_eq!(parse("{ 1 1 + } { 100 } { -100 } if"), vec![Num(100)]);
    }

    #[test]
    fn test_var() {
        assert_eq!(parse("/x 10 def /y 20 def x y *"), vec![Num(200)]);
    }

    #[test]
    fn test_var_if() {
        assert_eq!(
            parse("/x 10 def /y 20 def { x y < } { x } { y } if"),
            vec![Num(10)]
        );
    }

    #[test]
    fn test_multiline() {
        assert_eq!(
            parse(
                r#"
/x 10 def
/y 20 def

{
    x
    y
    <
}
{ x }
{ y }
if
"#
            ),
            vec![Num(10)]
        );
    }
}
