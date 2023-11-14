use std::iter::Peekable;
use std::vec::IntoIter;
use crate::lexer::TokenKind;

//todo create error reporter with lines and columns
#[derive(Debug)]
pub(crate) enum ParseError {
    SyntaxError(String),
    Fail(String),
}

#[derive(Debug)]
pub(crate) struct Parser {
    pub(crate) tokens: Peekable<IntoIter<TokenKind>>,
}

impl Parser {
    pub(crate) fn parse(&mut self) -> Result<Node, ParseError> {
        match self.program() {
            None => Err(ParseError::Fail(String::from("Failed to parse file, see individual syntax errors"))),
            Some(tree) => Ok(tree)
        }
    }

    /// Program -> MethodDeclaration+
    fn program(&mut self) -> Option<Node> {
        let mut methods: Vec<Box<Node>> = vec![];
        while let Some(method) = self.method_declaration() {
            methods.push(Box::new(method))
        }
        Some(Node::Program {
            method_declarations: methods
        })
    }

    /// Type [MAIN] Identifier '(' FormalParams ')' Block
    fn method_declaration(&mut self) -> Option<Node> {
        let next = self.next();
        let return_type = to_type(&next)?;
        let is_main = matches(self.peek(), "MAIN");
        if is_main {
            // we got what we wanted, discard the MAIN token
            self.next();
        }

        let method_name = identifier(&self.next())?;
        let next = self.next();
        if next != TokenKind::Lparen {
            println!("Expected left parenthesis got {:?} instead", next);
            return None;
        }
        let parameters = match self.formal_parameters() {
            None => None,
            Some(params) => Option::from(Box::new(params))
        };

        let next = self.next();
        if next != TokenKind::Rparen {
            println!("Expected right parenthesis got {:?} instead", next);
            return None;
        }
        let body = self.block()?;
        Some(
            Node::FunctionDeclaration {
                return_type: Box::new(Node::Type(return_type)),
                is_main,
                name: method_name,
                formal_parameters: parameters,
                body: Box::new(body),
            }
        )
    }
    ///BEGIN Statement+ END
    fn block(&mut self) -> Option<Node> {
        let next = self.next();
        if !matches(&next, "BEGIN") {
            return None;
        }
        let mut statements: Vec<Box<Node>> = vec![];

        while let Some(statement) = self.statement() {
            statements.push(Box::new(statement))
        }

        let next = self.next();
        if !matches(&next, "END") {
            println!("Expected END got {:?} instead", next);
            return None;
        }
        Some(Node::Block { statements })
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
                    "END" => None,
                    _ => {
                        if to_type(lookahead).is_some() {
                            return self.variable_declaration();
                        }
                        let next = self.next();
                        return self.assignment_statement(next);
                    }
                }
            }
            _ => {
                None
            }
        };
    }

    /// LocalVarDecl -> Type Id ';' | Type AssignmentStmt
    fn variable_declaration(&mut self) -> Option<Node> {
        let next = self.next();
        let var_type = match to_type(&next) {
            None => {
                println!("Expected type token got {:?} instead", next);
                return None;
            }
            Some(s) => s
        };
        let var_type = Box::new(Node::Type(var_type));
        //do not move the iterator yet to the current token as we will need
        //the identifier for the assignment too
        let peek = self.peek();
        let var_name = match identifier(peek) {
            None => {
                println!("Expected identifier got {:?} instead", peek);
                return None;
            }
            Some(x) => x
        };
        let next = self.next();
        // we perform a lookahead of 1 here to check if we have an assignment statement or just
        // a variable declaration. we parse assignments into a declaration and and an assignment
        return if *self.peek() == TokenKind::Semi {
            self.next(); //consume the semi colon
            Some(
                Node::LocalVariableDeclaration {
                    var_type: var_type,
                    name: var_name,
                    value: None,
                }
            )
        } else {
            let assignment = self.assignment_statement(next)?;
            Some(
                Node::LocalVariableDeclaration {
                    var_type: var_type,
                    name: var_name,
                    value: Option::from(Box::new(assignment)),
                }
            )
        };
    }
    /// Id := Expression ';'
    fn assignment_statement(&mut self, next: TokenKind) -> Option<Node> {
        let id = match identifier(&next) {
            None => {
                println!("Expected Identifier got: {:?} instead", next);
                return None;
            }
            Some(x) => x
        };
        let next = self.next();
        if next != TokenKind::Assign {
            println!("Expected ':=' got: {:?} instead", next);
            return None;
        }
        let value = self.expression()?;

        let next = self.next();
        if next != TokenKind::Semi {
            println!("Expected ';' got: {:?} instead", next);
            return None;
        }

        Some(
            Node::AssignmentStatement {
                id,
                value,
            }
        )
    }
    ///ReturnStmt -> RETURN expression ';'
    fn return_statement(&mut self) -> Option<Node> {
        if !matches(&self.next(), "RETURN") {
            return None;
        }
        let expression = self.expression()?;
        let next = self.next();
        if next != TokenKind::Semi {
            println!("Expected ';' got {:?} instead", next);
            return None;
        }
        Some(Node::ReturnStatement {
            expression
        })
    }
    /// IfStmt  -> If '(' BoolExpression ')' Statement (ELSE Statement)?
    fn if_statement(&mut self) -> Option<Node> {
        if !matches(&self.next(), "IF") {
            return None;
        }
        let next = self.next();
        if next != TokenKind::Lparen {
            println!("Expected '(' got {:?} instead", next);
            return None;
        }
        let condition = self.bool_expression()?;

        let next = self.next();
        if next != TokenKind::Rparen {
            println!("Expected ')' got {:?} instead", next);
            return None;
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
                ),
            })
        })
    }

    /// ReadStmt -> READ '(' Id ',' QString ')' ';'
    fn read_statement(&mut self) -> Option<Node> {
        if !matches(&self.next(), "READ") {
            return None;
        }
        let next = self.next();
        if next != TokenKind::Lparen {
            println!("Expected left parenthesis got {:?} instead", next);
            return None;
        }
        let id = identifier(&self.next())?;

        let next = self.next();
        if next != TokenKind::Comma {
            println!("Expected comma {:?} instead", next);
            return None;
        }

        let value = self.next();
        let value = self.q_string(value)?;

        let next = self.next();
        if next != TokenKind::Rparen {
            println!("Expected ')' got {:?} instead", next);
            return None;
        }
        let next = self.next();
        if next != TokenKind::Semi {
            println!("Expected ';' got {:?} instead", next);
            return None;
        }

        Some(Node::Statement {
            stmt: Box::new(Node::ReadStatement {
                id,
                q_string: value,
            })
        })
    }
    ///WriteStmt -> '(' Expression ',' QString ')' ';'
    fn write_statement(&mut self) -> Option<Node> {
        if !matches(&self.next(), "WRITE") {
            return None;
        }
        let next = self.next();
        if next != TokenKind::Lparen {
            println!("Expected left parenthesis got {:?} instead", next);
            return None;
        }
        let expression = self.expression()?;

        let next = self.next();
        if next != TokenKind::Comma {
            println!("Expected comma {:?} instead", next);
            return None;
        }

        let value = self.next();
        let value = self.q_string(value)?;

        let next = self.next();
        if next != TokenKind::Rparen {
            println!("Expected ')' got {:?} instead", next);
            return None;
        }
        let next = self.next();
        if next != TokenKind::Semi {
            println!("Expected ';' got {:?} instead", next);
            return None;
        }
        Some(Node::Statement {
            stmt: Box::new(Node::WriteStatement {
                expression,
                q_string: value,
            })
        })
    }
    /// String value
    /// q_string -> "([^"]|\\")*"
    fn q_string(&mut self, next: TokenKind) -> Option<Literal> {
        match next {
            TokenKind::Identifier(x) => Some(Literal::STRING(x)),
            _ => {
                println!("Expected Identifier got {:?} instead", next);
                None
            }
        }
    }
    /// Expression -> MultiplicativeExpr  (( '+' | '-' ) MultiplicativeExpr)*
    fn expression(&mut self) -> Option<Expression> {
        let exp = self.multiplicative_expr()?;
        let mut peek = self.peek();
        let mut rhs: Vec<Box<Expression>> = vec![];

        while *peek == TokenKind::Plus || *peek == TokenKind::Minus {
            let op = if self.next() == TokenKind::Plus {
                MathOperator::Plus
            } else {
                MathOperator::Minus
            };
            let rhs_exp = self.multiplicative_expr()?;
            rhs.push(
                Box::from(Expression::PlusMinus {
                    op,
                    rhs: Box::new(rhs_exp),
                })
            );
            peek = self.peek();
        }

        Some(
            Expression::Multiplicative {
                lhs: Box::new(exp),
                rhs,
            }
        )
    }
    /// MultiplicativeExpr -> PrimaryExpr (( '*' | '/' ) PrimaryExpr)*
    fn multiplicative_expr(&mut self) -> Option<Expression> {
        let exp = self.primary_expr()?;
        let mut peek = self.peek();
        let mut rhs: Vec<Box<Expression>> = vec![];

        while *peek == TokenKind::Mul || *peek == TokenKind::Div {
            let op = if self.next() == TokenKind::Mul {
                MathOperator::Mul
            } else {
                MathOperator::Div
            };
            let rhs_exp = self.primary_expr()?;
            rhs.push(
                Box::from(Expression::MulDiv {
                    op,
                    rhs: Box::new(rhs_exp),
                })
            );
            peek = self.peek();
        }

        Some(
            Expression::Multiplicative {
                lhs: Box::new(exp),
                rhs,
            }
        )
    }
    ///PrimaryExpr -> Num  // Integer or Real numbers
    ///              | Id
    ///              | '(' Expression ')'
    ///              | Id '(' ActualParams ')'
    fn primary_expr(&mut self) -> Option<Expression> {
        let next = self.next();
        if next == TokenKind::Lparen {
            let expr = self.expression()?;
            let next = self.next();
            if next != TokenKind::Rparen {
                println!("Expected ')' got {:?} instead", next);
                return None;
            }
            return Some(Expression::Expression { value: Box::new(expr) });
        }
        return match next {
            TokenKind::Integer(i) => {
                Some(Expression::Primitive { value: Literal::INT(i) })
            }
            TokenKind::Real(r) => Some(Expression::Primitive { value: Literal::REAL(r) }),
            TokenKind::Identifier(s) => {
                let peek = self.peek();
                return if *peek == TokenKind::Lparen {
                    self.next(); // consume the lparen
                    let params = match self.actual_params() {
                        None => None,
                        Some(params) => Option::from(Box::new(params))
                    };
                    let next = self.next();
                    if next != TokenKind::Rparen {
                        println!("Expected ')' got {:?} instead", next);
                        return None;
                    }
                    Some(
                        Expression::MethodCall {
                            id: Box::from(Expression::Id(s)),
                            params,
                        }
                    )
                } else {
                    Some(
                        Expression::Id(s)
                    )
                };
            }
            _ => {
                println!("Could not recognise expression to parse at token: {:?}", next);
                None
            }
        };
    }
    /// ActualParams -> [Expression ( ',' Expression)*]
    fn actual_params(&mut self) -> Option<Expression> {
        let mut params: Vec<Box<Expression>> = vec![];
        if let Some(param) = self.expression() {
            params.push(Box::new(param))
        }

        while *self.peek() == TokenKind::Comma {
            self.next(); // discard the comma
            params.push(Box::new(self.expression()?));
        }

        Some(Expression::ActualParameters { params })
    }

    fn bool_expression(&mut self) -> Option<Expression> {
        let lhs = self.expression()?;
        let next = self.next();
        let operator = match next {
            TokenKind::Equal => { BoolOperator::Equal }
            TokenKind::NotEqual => { BoolOperator::NotEqual }
            _ => {
                println!("Expected boolean operator got {:?} instead", next);
                return None;
            }
        };
        let rhs = self.expression()?;

        Some(Expression::Boolean {
            lhs: Box::new(lhs),
            op: operator,
            rhs: Box::new(rhs),
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
            params.push(Box::from(self.formal_parameter()?));
        }

        Some(Node::FormalParameters { parameters: params })
    }

    /// FormalParam -> Type Id
    fn formal_parameter(&mut self) -> Option<Node> {
        let peek = self.peek();
        let param_type = to_type(peek)?;
        self.next(); // consume the peeked token since we found a parameter
        let next = self.next();
        let identifier = identifier(&next)?;
        Some(Node::FormalParameter {
            param_type: Box::new(Node::Type(param_type)),
            name: identifier,
        })
    }
    ///We don't want to fail if we've reached the end so return a None reference
    fn peek(&mut self) -> &TokenKind {
        self.tokens.peek().unwrap_or(&TokenKind::None)
    }
    fn next(&mut self) -> TokenKind {
        self.tokens.next().unwrap_or(TokenKind::None)
    }
}

fn identifier(token: &TokenKind) -> Option<Expression> {
    match token {
        TokenKind::Identifier(x) =>
            Some(
                Expression::Id(x.clone())
            ),
        _ => None
    }
}

fn to_type(token: &TokenKind) -> Option<String> {
    match token {
        TokenKind::Identifier(x) if (x == "INT" || x == "REAL" || x == "STRING") => Some(x.clone()),
        _ => None
    }
}

fn matches(token: &TokenKind, target: &str) -> bool {
    match token {
        TokenKind::Identifier(x) if x == target => true,
        _ => false
    }
}

#[derive(Debug)]
pub enum Node {
    Program {
        method_declarations: Vec<Box<Node>>
    },
    FunctionDeclaration {
        name: Expression,
        return_type: Box<Node>,
        body: Box<Node>,
        is_main: bool,
        formal_parameters: Option<Box<Node>>,
    },
    Block {
        statements: Vec<Box<Node>>
    },
    Statement {
        stmt: Box<Node>
    },
    LocalVariableDeclaration {
        var_type: Box<Node>,
        name: Expression,
        value: Option<Box<Node>>,
    },
    AssignmentStatement {
        id: Expression,
        value: Expression,
    },
    ReturnStatement {
        expression: Expression,
    },
    IfStatement {
        condition: Expression,
        body: Box<Node>,
    },
    ReadStatement {
        id: Expression,
        q_string: Literal,
    },
    WriteStatement {
        expression: Expression,
        q_string: Literal,
    },
    Type(String),
    FormalParameters {
        parameters: Vec<Box<Node>>
    },
    FormalParameter {
        param_type: Box<Node>,
        name: Expression,
    },
}



#[derive(Debug)]
pub enum Expression {
    Expression {
        value: Box<Expression>
    },
    Multiplicative {
        lhs: Box<Expression>,
        rhs: Vec<Box<Expression>>,
    },
    PlusMinus {
        op: MathOperator,
        rhs: Box<Expression>,
    },
    MulDiv {
        op: MathOperator,
        rhs: Box<Expression>,
    },
    Primitive {
        value: Literal
    },
    Id(String),
    MethodCall {
        id: Box<Expression>,
        params: Option<Box<Expression>>,
    },
    Boolean {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BoolOperator,
    },
    ActualParameters {
        params: Vec<Box<Expression>>
    },
}

#[derive(Debug)]
pub enum Literal {
    STRING(String),
    INT(usize),
    REAL(f64),
}


#[derive(Debug)]
pub enum BoolOperator {
    Equal,
    NotEqual,
}

#[derive(Debug)]
pub enum MathOperator {
    Plus,
    Minus,
    Mul,
    Div,
}



