use std::fs;



fn main() {
    let code = fs::read_to_string("tiny.txt").expect("Could not read input file");
    tiny_compiler::compile(code);
}
