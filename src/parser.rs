use std::iter::Peekable;
use std::vec::IntoIter;
use crate::lexer::TokenKind;
pub(crate) enum ParseError {
    SyntaxError(String),
    Fail(String)
}

pub(crate) struct Parser {
    pub(crate) tokens: Peekable<IntoIter<TokenKind>>,
}

impl Parser {
    pub(crate) fn parse(&mut self) -> Result<Node, ParseError> {
        match self.program() {
            None => Err(ParseError::Fail(String::from("Failed to parse"))),
            Some(tree) => Ok(tree)
        }
    }

    /// Program -> MethodDeclaration+
    fn program(&mut self) -> Option<Node> {
        let mut methods: Vec<Box<Node>> = vec![];
        match self.method_declaration() {
            None => { return None } //todo parse error here
            Some(method) => methods.push(Box::new(method))
        }
        while let Some(method) = self.method_declaration() {
            methods.push(Box::new(method))
        }
        Some(Node::Program{
            method_declarations: methods
        })
    }

    //todo add meaningful messages
    /// Type [MAIN] Identifier '(' FormalParams ')' Block
    fn method_declaration(&mut self) -> Option<Node> {
        let next = self.next();
        let ttype = ttype(&next)?;
        let is_main = matches(self.peek(), "MAIN");
        if is_main {
            // we got what we wanted, discard the MAIN token
            self.next();
        }
        let next = self.next();
        let method_name = identifier(&next)?;
        if self.next() != TokenKind::Lparen {
            return None //todo syntax error
        }
        let parameters = self.formal_parameters()?;

        if self.next() != TokenKind::Rparen {
            return  None // todo syntax error
        }
        let body = self.block()?;
        Some(
            Node::MethodDeclaration {
                return_type: Box::new(Node::Type(ttype)),
                is_main,
                name: method_name,
                formal_parameters: Box::new(parameters),
                body: Box::new(body)
            }
        )
    }
    ///BEGIN Statement+ END
    fn block(&mut self) -> Option<Node> {
        if !matches(&self.next(), "BEGIN") {
            return None
        }
        let mut statements:Vec<Box<Node>> = vec![];

        match self.statement() {
            None => { return None } //todo parse error here
            Some(s) => statements.push(Box::new(s))
        }
        while let Some(statement) = self.statement() {
            statements.push(Box::new(statement))
        }

        if !matches(&self.next(), "END") {
            return None //todo parse error
        }
        Some(Node::Block {statements})
    }
    /// Statement -> Block | LocalVarDecl | AssignStmt | ReturnStmt | IfStmt | WriteStmt | ReadStmt
    fn statement(&mut self) -> Option<Node> {
        let lookahead = self.peek();
        return match lookahead {
            TokenKind::Identifier(id) => {
                match &id[..] {
                    "BEGIN" => self.block(),
                    "WRITE" => self.write_statement(),
                    "READ" => self.read_statement(),
                    "IF" => self.if_statement(),
                    "RETURN" => self.return_statement(),
                    _ =>  {
                        if ttype(lookahead).is_some() {
                            return self.variable_declaration()
                        }
                        return self.assignment_statement()
                    }
                }
            }
            _ => None //todo syntax error
        }
    }

    /// LocalVarDecl -> Type Id ';' | Type AssignmentStmt
    fn variable_declaration(&mut self) -> Option<Node> {
        let ttype = match ttype(&self.next()) {
            None => return None, //todo syntax error
            Some(s) => s
        };
        let ttype = Box::new(Node::Type(ttype));
        //do not move the iterator yet to the current token
        let var_name = match identifier(self.peek()) {
            None => return None, //todo syntax error
            Some(x) => x
        };
        // we perform a lookahead of 1 here to check if we have an assignment statement or just
        // a variable declaration
        return if *self.peek() == TokenKind::Semi {
            self.next(); //consume the variable name (Id)
            self.next(); //consume the semi colon
            Some(
                Node::LocalVariableDeclaration {
                    ttype,
                    name: var_name,
                    value: None
                }
            )
        } else {
            let assignment = self.assignment_statement()?;
            Some(
                Node::LocalVariableDeclaration {
                    ttype,
                    name: var_name,
                    value: Option::from(Box::new(assignment))
                }
            )

        }
    }
    /// Id := Expression ';'
    fn assignment_statement(&mut self) -> Option<Node> {
        let id = match identifier(&self.next()) {
            None => return None, //todo syntax error
            Some(x) => x
        };
        if self.next() != TokenKind::Assign {
            return None //todo syntax error
        }
        let value = self.expression()?;

        if self.next() != TokenKind::Semi {
            return None //todo syntax error
        }

        Some(
            Node::AssignmentStatement {
                id,
                value
            }
        )
    }
    ///ReturnStmt -> RETURN expression ';'
    fn return_statement(&mut self) -> Option<Node> {
        if !matches(&self.next(), "RETURN") {
            return None
        }
        let expression = self.expression()?;

        if self.next() != TokenKind::Semi {
            return None //todo syntax error
        }

        Some(Node::ReturnStatement {
            expression
        })
    }
    /// IfStmt  -> If '(' BoolExpression ')' Statement (ELSE Statement)?
    fn if_statement(&mut self) -> Option<Node> {
        if !matches(&self.next(), "IF") {
            return None
        }
        if self.next() != TokenKind::Lparen {
            return None //todo syntax error
        }
        let condition = self.bool_expression()?;

        if self.next() != TokenKind::Rparen {
            return None //todo syntax error
        }
        let mut statements: Vec<Box<Node>> = vec![];
        statements.push(Box::new(self.statement()?));
        if matches(&self.next(), "ELSE") {
            statements.push(Box::new(self.statement()?));
        }
        Some(Node::Statement {
            stmt: Box::new(Node::IfStatement {
                condition,
                body: Box::new(
                    Node::Block {
                        statements
                    }
                )
            })
        })
    }

    /// ReadStmt -> READ '(' Id ',' QString ')' ';'
    fn read_statement(&mut self) -> Option<Node> {
        if !matches(&self.next(), "READ") {
            return None
        }
        if self.next() != TokenKind::Lparen {
            return None //todo syntax error
        }
        let id = identifier(&self.next())?;

        if self.next() != TokenKind::Comma {
            return None //todo syntax error
        }

        let value = self.q_string()?;

        if self.next() != TokenKind::Rparen {
            return None //todo syntax error
        }
        if self.next() != TokenKind::Semi {
            return None //todo syntax error
        }

        Some(Node::Statement {
            stmt: Box::new(Node::ReadStatement {
                id,
                q_string: value
            })
        })
    }
    ///WriteStmt -> '(' Expression ',' QString ')' ';'
    fn write_statement(&mut self) -> Option<Node> {
        if !matches(&self.next(), "WRITE") {
            return None
        }
        if self.next() != TokenKind::Lparen {
            return None //todo syntax error
        }
        let expression = match self.expression() {
            None => { return None}//todo syntax error
            Some(e) => {e}
        };

        if self.next() != TokenKind::Comma {
            return None //todo syntax error
        }
        let value = self.q_string()?;

        if self.next() != TokenKind::Rparen {
            return None
        }
        if self.next() != TokenKind::Semi {
            return None
        }
        Some(Node::Statement {
            stmt: Box::new(Node::WriteStatement {
                expression,
                q_string: value
            })
        })

    }

    /// any value enclosed in quotation marks
    fn q_string(&mut self) -> Option<Literal> {
        match self.next() {
            TokenKind::Identifier(x) => Some(Literal::STRING(x)),
            _ => None //todo syntax error
        }
    }
    fn expression(&mut self) -> Option<Expression> {
        None
    }

    fn bool_expression(&mut self) -> Option<Expression> {
        let lhs = self.expression()?;
        let operator = match self.next() {
            TokenKind::Equal => { BoolOperator::Equal }
            TokenKind::NotEqual => { BoolOperator::NotEqual }
            _=> { return None } //todo parse error
        };
        let rhs = self.expression()?;

        Some(Expression::Boolean {
            lhs: Box::new(lhs),
            op: operator,
            rhs: Box::new(rhs)
        })

    }
    /// FormalParams -> [FormalParam ( ',' FormalParam)*]
    fn formal_parameters(&mut self) -> Option<Node> {
        let mut params: Vec<Box<Node>> = vec![];
        if let Some(param) = self.formal_parameter() {
            params.push(Box::new(param))
        }

        while *self.peek() == TokenKind::Comma {
            self.next(); // discard the comma
            match self.formal_parameter() {
                None => { return None } //todo parse error
                Some(param) => params.push(Box::new(param))
            }
        }

        Some(Node::FormalParameters {parameters: params})
    }

    /// FormalParam -> Type Id
    fn formal_parameter(&mut self) -> Option<Node> {
        let ttype = ttype(&self.next())?;
        let identifier = identifier(&self.next())?;
        Some(Node::FormalParameter {
            ttype: Box::new(Node::Type(ttype)),
            name: identifier
        })
    }

    

    fn peek(&mut self) -> &TokenKind {
        self.tokens.peek().expect("Could not peek; no tokens left")
    }

    fn next(&mut self) -> TokenKind {
        self.tokens.next().expect("Could not get next; no tokens left")
    }

}

fn identifier(token: &TokenKind) -> Option<Literal> {
    match token {
        TokenKind::Identifier(x) => { Some(Literal::STRING(x.clone()))}
        _ => None
    }
}

/// INT | REAL | STRING
fn ttype(token: &TokenKind) -> Option<String>{
    match token {
        TokenKind::Identifier(x) if (x=="INT" || x=="REAL" || x=="STRING") => Some(x.clone()),
        _ => None
    }
}
fn matches(token: &TokenKind, target: &str) ->bool {
    match token {
        TokenKind::Identifier(x) if x == target => { true },
        _ => { false }
    }
}
pub(crate) enum Node {
    Program{
        method_declarations: Vec<Box<Node>>
    },
    MethodDeclaration {
        name: Literal,
        return_type: Box<Node>,
        body: Box<Node>,
        is_main: bool,
        formal_parameters: Box<Node>
    },
    Block {
      statements: Vec<Box<Node>>
    },
    Statement {
      stmt: Box<Node>
    },
    LocalVariableDeclaration {
        ttype: Box<Node>,
        name: Literal,
        value: Option<Box<Node>>
    },
    AssignmentStatement {
        id: Literal,
        value: Expression
    },
    ReturnStatement {
        expression: Expression,
    },
    IfStatement {
        condition: Expression,
        body: Box<Node>
    },
    ReadStatement {
        id: Literal,
        q_string: Literal
    },
    WriteStatement {
        expression: Expression,
        q_string: Literal
    },
    Type(String),
    FormalParameters{
        parameters: Vec<Box<Node>>
    },
    FormalParameter {
        ttype: Box<Node>,
        name: Literal
    }
}

pub(crate) enum Expression {
    Multiplicative,
    Primary,
    Boolean{
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BoolOperator
    },
    ActualParameters,
}

pub(crate) enum Literal {
    STRING(String),
    INT(usize),
    REAL(f64)
}

pub(crate) enum BoolOperator {
    Equal,
    NotEqual
}



