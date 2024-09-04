fn main() {
    for line in std::io::stdin().lines() {
        let mut stack = vec![];

        if let Ok(line) = line {
            let words: Vec<_> = line.split(" ").collect();

            for word in words {
                if let Ok(parsed) = word.parse::<i32>() {
                    stack.push(parsed);
                } else {
                    match word {
                        "+" => add(&mut stack),
                        "-" => sub(&mut stack),
                        "*" => mul(&mut stack),
                        "/" => div(&mut stack),
                        _ => panic!("{word:?} could not be parsed"),
                    }
                }
            }
        }

        println!("stack: {stack:?}");
    }
}

fn add(stack: &mut Vec<i32>) {
    let r = stack.pop().unwrap();
    let l = stack.pop().unwrap();
    stack.push(l + r);
}

fn sub(stack: &mut Vec<i32>) {
    let r = stack.pop().unwrap();
    let l = stack.pop().unwrap();
    stack.push(l - r);
}

fn mul(stack: &mut Vec<i32>) {
    let r = stack.pop().unwrap();
    let l = stack.pop().unwrap();
    stack.push(l * r);
}

fn div(stack: &mut Vec<i32>) {
    let r = stack.pop().unwrap();
    let l = stack.pop().unwrap();
    stack.push(l / r);
}
