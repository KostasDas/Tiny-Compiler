use std::fs;

pub mod lexer;
pub mod parser;

fn main() {
    // read the file
    // break it into words
    // tokenize it
    let code = fs::read_to_string("tiny.txt").expect("Could not read input file");
    let tokens = lexer::Tokenizer::tokenize(code);
    // create the parse tree
    let mut parser = parser::Parser {
        tokens: tokens.into_iter().peekable(),
    };
    let _ast = parser.parse().expect("Failed");
    println!("{:#?}", _ast);
}
