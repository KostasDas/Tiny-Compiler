use std::fs;

pub mod lexer;
pub mod parser;

fn main() {
    // read the file
    // break it into words
    // tokenize it
    let code = fs::read_to_string("tiny.txt").expect("Could not read input file");
    let _tokens = lexer::Tokenizer::tokenize(code);

    // create the parse tree
    for _token in _tokens {
        println!("{:?}", _token);
    }
}
