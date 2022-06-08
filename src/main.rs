use repl::Repl;

pub mod lexer;
pub mod repl;
pub mod token;

fn main() {
    Repl::start();
}
