pub mod lexer;
pub mod parser;


pub fn compile(code: String) {
    let tokens = lexer::Tokenizer::tokenize(code);
    // create the parse tree
    let mut parser = parser::Parser {
        tokens: tokens.into_iter().peekable(),
    };
    let _ast = parser.parse().expect("Failed");
    println!("{:#?}", _ast);
}