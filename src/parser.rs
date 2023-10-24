use std::iter::Peekable;
use std::vec::IntoIter;
use crate::lexer::TokenKind;

pub(crate) enum ParseError {
    SyntaxError(String)
}

pub(crate) struct Parser {
    pub(crate) tokens: Peekable<IntoIter<TokenKind>>,
}

impl Parser {
    pub(crate) fn parse(&mut self) -> Result<Node, ParseError> {
        self.program()
    }

    /// Program -> MethodDeclaration+
    fn program(&mut self) -> Result<Node, ParseError> {
        let mut methods: Vec<Box<Node>> = vec![];
        while self.next() {
            match self.method_declaration() {
                Ok(n) => {}
                Err(b) => {}
            }
            methods.push(Box::new(self.method_declaration()?));
        }

        Ok(
            Node::Program {
                body: methods
            }
        )
    }

    //todo add meaningful messages
    /// Type [MAIN] Identifier '(' FormalParams ')' Block
    fn method_declaration(&mut self) -> Result<Node, ParseError> {
        let ttype = self.ttype(self.next())?;
        let is_main = self.matches(self.peek(), "MAIN");
        if is_main {
            // we got what we wanted, discard the MAIN token
            self.next();
        }
        let method_name = self.identifier(self.next())?;
        if self.next() != TokenKind::Lparen {
            return Err(ParseError::SyntaxError(String::from("Expected left parenthesis got some shit instead")))
        }
        let parameters = self.formal_parameters()?;

        if self.next() != TokenKind::Rparen {
            return Err(ParseError::SyntaxError(String::from("Expected right parenthesis got some shit instead")))
        }
        let method_block = self.block()?;
        Ok(Node::MethodDeclaration{
            name: method_name,
            is_main,
            return_type: Box::new(ttype),
            parameters: Box::new(parameters),
            body: Box::new(method_block)
        })
    }
    ///BEGIN Statement+ END
    fn block(&mut self) -> Result<Node, ParseError> {
        let curr_token = self.next();
        if !self.matches(&curr_token, "BEGIN") {
            return Err(ParseError::SyntaxError(String::from("Block can only start with 'BEGIN' keyword")));
        }
        let mut statements:Vec<Box<Node>> = vec![];
        /// 1 or more statements
        /// BEGIN
        /// INT x;
        /// READ(x, 1);
        /// INT Y;
        /// END

        while match self.statement() {
            Ok(s) => {
                statements.push(Box::new(s))
            }
            Err(_) => {}
        } {

        }

        if !self.matches(&self.next(), "END") {
            return Err(ParseError::SyntaxError(String::from("Block can only end with 'END' keyword")));
        }
        Ok(Node::Block{body: statements})
    }

    fn statement(&mut self) -> Result<Node, ParseError> {

    }

    /// FormalParams -> [FormalParam ( ',' FormalParam)*]
    fn formal_parameters(&mut self) -> Result<Node, ParseError> {

    }
    // INT | REAL | STRING
    fn ttype(&self, token: TokenKind) -> Result<Node, ParseError> {
        match token {
            TokenKind::Identifier(x) if (x=="INT" || x=="REAL" || x=="STRING") => Ok(Node::Type(x)),
            _ => Err(ParseError::SyntaxError(String::from("Type can only be INT | REAL | STRING")))
        }
    }
    fn identifier(&self, token: TokenKind) -> Result<String, ParseError> {
        match token {
            TokenKind::Identifier(x) => { Ok(x)}
            _ => Err(ParseError::SyntaxError(String::from("Method without name found")))
        }
    }
    fn matches(&self, token: &TokenKind, target: &str) -> bool {
        match token {
            TokenKind::Identifier(x) if x == target => { true },
            _ => { false }
        }
    }

    fn peek(&mut self) -> &TokenKind {
        self.tokens.peek().expect("Could not peek; no tokens left")
    }

    fn next(&mut self) -> TokenKind {
        self.tokens.next().expect("Could not get next; no tokens left")
    }
}
#[derive(Clone)]
pub(crate) enum Node {
    Program{
        body: Vec<Box<Node>>
    },
    MethodDeclaration{
        name: String,
        is_main: bool,
        parameters: Box<Node>,
        body: Box<Node>,
        return_type: Box<Node>
    },
    Block{
        body: Vec<Box<Node>>
    },
    Statement{body: Box<Node>},
    LocalVarDecl,
    AssignStmt,
    ReturnStmt,
    IfStmt,
    WriteStmt,
    ReadStmt,
    Type(String),
    FormalParameters,
}



