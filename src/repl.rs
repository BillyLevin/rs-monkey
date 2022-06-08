use crate::{lexer::Lexer, token::Token};
use rustyline::Editor;

pub struct Repl;

const PROMPT: &str = ">> ";

impl Repl {
    pub fn start() {
        let mut rl = Editor::<()>::new();

        loop {
            match rl.readline(PROMPT) {
                Ok(line) => {
                    let mut lexer = Lexer::new(&line);

                    loop {
                        match lexer.next_token() {
                            Token::Eof => break,
                            token => println!("{:?}", token),
                        }
                    }
                }
                _ => break,
            }
        }
    }
}
