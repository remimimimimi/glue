#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod ast;
pub mod ctx;
pub mod parser;
pub mod queries;

fn main() {
    println!("Hello, world!");
}
