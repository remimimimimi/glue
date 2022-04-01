#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod parser;
pub mod ast;
pub mod queries;
pub mod ctx;

fn main() {
    println!("Hello, world!");
}

