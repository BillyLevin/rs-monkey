use crate::{evaluator::Evaluator, lexer::Lexer, parser::Parser};
use rustyline::Editor;

pub struct Repl;

const PROMPT: &str = ">> ";

impl Repl {
    pub fn start() {
        let mut rl = Editor::<()>::new();
        let mut evaluator = Evaluator::new();

        while let Ok(line) = rl.readline(PROMPT) {
            let lexer = Lexer::new(&line);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            let errors = parser.errors();

            if !errors.is_empty() {
                for error in errors {
                    println!("\t{}", error);
                }

                continue;
            }

            if let Some(evaluated) = evaluator.eval(program) {
                println!("{}", evaluated);
            }
        }
    }
}
