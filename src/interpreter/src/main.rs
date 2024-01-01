use crate::evaluator::eval;
use libonion::object::environment::Env;
use libonion::parser::parse;
use std::cell::RefCell;
use std::io::stdin;
use std::rc::Rc;

mod evaluator;
mod tests;

fn main() {
    println!("Onion interpreter");
    let env: Env = Rc::new(RefCell::new(Default::default()));

    // let input = "component MyComponent {
        
    // }";

    let input = "
    component Position {

    }
    ";

    match parse(&input) {
        Ok(node) => match eval(node, &env) {
            Ok(evaluated) => println!("{}", evaluated),
            Err(e) => eprintln!("{}", e),
        },
        Err(e) => eprintln!("parse error: {}", e[0]),
    }

    // loop {
    //     let mut input = String::new();
    //     stdin().read_line(&mut input).unwrap();

    //     if input.trim_end().is_empty() {
    //         println!("bye");
    //         std::process::exit(0)
    //     }

    //     match parse(&input) {
    //         Ok(node) => match eval(node, &env) {
    //             Ok(evaluated) => println!("{}", evaluated),
    //             Err(e) => eprintln!("{}", e),
    //         },
    //         Err(e) => eprintln!("parse error: {}", e[0]),
    //     }
    // }
}
