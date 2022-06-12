use crate::{evaluator::Evaluator, lexer::Lexer, parser::Parser};
use rustyline::Editor;

pub struct Repl;

const PROMPT: &str = ">> ";

impl Repl {
    pub fn start() {
        let mut rl = Editor::<()>::new();

        loop {
            match rl.readline(PROMPT) {
                Ok(line) => {
                    let lexer = Lexer::new(&line);
                    let mut parser = Parser::new(lexer);

                    let program = parser.parse_program();
                    let errors = parser.errors();

                    if errors.len() != 0 {
                        for error in errors {
                            println!("\t{}", error);
                        }

                        continue;
                    }

                    let mut evaluator = Evaluator::new();

                    if let Some(evaluated) = evaluator.eval(program) {
                        println!("{}", evaluated);
                    }
                }
                _ => break,
            }
        }
    }
}
