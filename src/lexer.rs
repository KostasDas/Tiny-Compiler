
pub struct Tokenizer {
}

impl Tokenizer {
    //todo some meaningful expect messages
    pub fn tokenize(input: String)-> Vec<TokenKind> {
        let mut tokens: Vec<TokenKind> = vec![];
        let mut line_iter = input.lines().into_iter();
        while let Some(line) = line_iter.next() {
            let mut chars = line.chars().peekable();
            let mut token = String::from("");
            while let Some(c) = chars.next() {
                let mut kind = TokenKind::None;
                match c {
                    '/' => {
                        let peek_next = chars.peek().expect("Could not end in \"/\"");
                        // ignore comments
                        if *peek_next == '*' {
                            let peek_next = chars.peek().expect("Could not end in \"/\"");
                            if *peek_next == '*' {
                                let mut last_two = vec![];
                                let mut found_end = false;
                                // skip until we reach end of multiline comment '/'
                                while !found_end {
                                    for ch in chars.by_ref() {
                                        // if last two + '/' are **/
                                        if ch == '/' && last_two.clone().into_iter().all(|l| l == '*') {
                                            found_end = true;
                                        }
                                        last_two.push(ch);
                                        if last_two.len() == 3 {
                                            last_two.remove(0);
                                        }
                                    }
                                    if let Some(line) = line_iter.next() {
                                        chars = line.chars().peekable();
                                    }
                                }
                                continue
                            }
                        }
                        kind= TokenKind::Div
                    }
                    '*' => kind = TokenKind::Mul,
                    ';' => kind = TokenKind::Semi,
                    '(' => kind = TokenKind::Lparen,
                    ')' => kind = TokenKind::Rparen,
                    ',' => kind = TokenKind::Comma,
                    '+' => kind = TokenKind::Plus,
                    '-' => kind = TokenKind::Minus,
                    ' ' => {}
                    ':' => {
                        let next = chars.next().expect("Program cannot end in :");
                        if next == '=' {
                            kind = TokenKind::Assign
                        }

                    }
                    '=' => {
                        let next = chars.next().expect("Program cannot end in =");
                        if next == '=' {
                            kind = TokenKind::Equal
                        }
                    }
                    '!' => {
                        let next = chars.next().expect("Program cannot end in !");
                        if next == '=' {
                            kind = TokenKind::NotEqual
                        }
                    }
                    _ => {
                        token.push(c);
                        let peek_one = chars.peek();
                        // if we've reached the end of the line, emit the current token
                        match peek_one {
                            None => {
                                tokens.push(TokenKind::from(&token[..]));
                                token = String::new()
                            }
                            Some(_) => {
                                continue
                            }
                        }
                    }
                }
                // emit the tokens, if we've built some word/integer/real number
                // or an operator/separator
                if !token.is_empty() {
                    let mut token_kind = TokenKind::from(&token[..]);
                    let real_value = token.parse::<f64>();
                    token_kind = match real_value {
                        Ok(v) => {
                            TokenKind::from(v)
                        }
                        Err(_) => { token_kind }
                    };
                    let int_value = token.parse::<usize>();
                    token_kind = match int_value {
                        Ok(v) => {
                            TokenKind::from(v)
                        }
                        Err(_) => { token_kind }
                    };
                    tokens.push(token_kind);
                    token = String::new()
                }
                if kind != TokenKind::None {
                    tokens.push(kind);
                }

            }
        }
        tokens
    }

}



#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Integer(usize),
    Real(f64),
    Identifier(String),
    Semi,
    Comma,
    Lparen,
    Rparen,
    Plus,
    Minus,
    Mul,
    Div,
    Assign,
    Equal,
    NotEqual,
    None

}

impl From<String> for TokenKind {
    fn from(value: String) -> TokenKind {
        TokenKind::Identifier(value)
    }
}

impl<'a> From<&'a str> for TokenKind {
    fn from(value: &'a str) -> TokenKind {
        TokenKind::Identifier(value.to_string())
    }
}

impl From<usize> for TokenKind {
    fn from(value: usize) -> TokenKind {
        TokenKind::Integer(value)
    }
}

impl From<f64> for TokenKind {
    fn from(value: f64) -> TokenKind {
        TokenKind::Real(value)
    }
}
// pub enum Keyword {
//     WRITE,
//     READ,
//     IF,
//     ELSE,
//     RETURN,
//     BEGIN,
//     END,
//     MAIN,
//     STRING,
//     INT,
//     REAL
// }