fn main() {
    for line in std::io::stdin().lines() {
        if let Ok(line) = line {
            let words: Vec<_> = line.split(" ").collect();
            println!("Line: {words:?}");
        }
    }
}

fn add(stack: &mut Vec<i32>) {
    let l = stack.pop().unwrap();
    let r = stack.pop().unwrap();
    stack.push(l + r);
}
