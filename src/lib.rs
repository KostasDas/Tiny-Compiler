use crate::tac::{ThreeAddressCodeVisitor, Visitable};

pub mod lexer;
pub mod parser;
pub mod tac;


pub fn compile(code: String) {
    let tokens = lexer::Tokenizer::tokenize(code);
    // create the parse tree
    let mut parser = parser::Parser {
        tokens: tokens.into_iter().peekable(),
    };
    let syntax_tree = parser.parse().expect("Failed");
    println!("{:#?}", syntax_tree);

    let mut tac_visitor = ThreeAddressCodeVisitor{};
    let tac = syntax_tree.accept(&mut tac_visitor);
    println!("{:#?}", tac)
}