use std::vec;
use crate::parser::{Expression, Literal, Node};

pub trait Visitor<T> {
    fn visit_expression(&self, e: &Expression, ctx: &Context) -> Vec<T>;
    fn visit_node(&self, n: &Node, ctx: &Context) -> Vec<T>;
    fn visit_literal(&self, l: &Literal, ctx: &Context) -> Vec<T>;
}
pub trait Visitable<T> {
    fn accept(&self, visitor: &dyn Visitor<T>, ctx: &Context) -> Vec<T>;
}

impl Visitable<Quadruple> for Node {
    fn accept(&self, visitor: &dyn Visitor<Quadruple>, ctx: &Context) -> Vec<Quadruple> {
        visitor.visit_node(&self, ctx)
    }
}

impl Visitable<Quadruple> for Expression {
    fn accept<>(&self, visitor: &dyn Visitor<Quadruple>, ctx: &Context) -> Vec<Quadruple> {
        visitor.visit_expression(&self, ctx)
    }
}

impl Visitable<Quadruple> for Literal {
    fn accept<>(&self, visitor: &dyn Visitor<Quadruple>, ctx: &Context) -> Vec<Quadruple> {
        visitor.visit_literal(&self, ctx)
    }
}




pub struct Context {
    pub(crate) in_function: bool
}

impl Context {
    pub fn new() -> Context {
        Context {
            in_function: false
        }
    }
}
pub struct ThreeAddressCodeVisitor {
}

impl Visitor<Quadruple> for ThreeAddressCodeVisitor {

    fn visit_expression(&self, e: &Expression, ctx: &Context) -> Vec<Quadruple> {
        let mut quadruples = vec![];
        match e {
            Expression::Expression { .. } => {}
            Expression::Multiplicative { .. } => {}
            Expression::PlusMinus { .. } => {}
            Expression::MulDiv { .. } => {}
            Expression::Primitive { .. } => {}
            Expression::Id(value) => {
                quadruples.push(Quadruple::Label(LabelType::BEGIN, value.to_string()))
            }
            Expression::MethodCall { .. } => {}
            Expression::Boolean { .. } => {}
            Expression::ActualParameters { .. } => {}
        }

        return quadruples;
    }

    fn visit_node(&self, n: &Node, ctx: &Context) -> Vec<Quadruple> {
        let mut quadruples = vec![];
        match n {
            Node::Program{method_declarations}=> {
                quadruples.push(Quadruple::Label(LabelType::BEGIN, String::from("Program")));
                for method in method_declarations {
                    quadruples.append(&mut method.accept(self as &dyn Visitor<Quadruple>, ctx))
                }
                quadruples.push(Quadruple::Label(LabelType::END, String::from("Program")))
            }
            Node::FunctionDeclaration {
                name,
                return_type,
                is_main,
                formal_parameters,
                body
            } => {
                let mut function_name = name.accept(self as &dyn Visitor<Quadruple>, ctx);
                quadruples.append(&mut function_name);

            }
            Node::Block { .. } => {}
            Node::Statement { .. } => {}
            Node::LocalVariableDeclaration { .. } => {}
            Node::AssignmentStatement { .. } => {}
            Node::ReturnStatement { .. } => {}
            Node::IfStatement { .. } => {}
            Node::ReadStatement { .. } => {}
            Node::WriteStatement { .. } => {}
            Node::Type(_) => {}
            Node::FormalParameters { .. } => {}
            Node::FormalParameter { .. } => {}
        }

        quadruples
    }

    fn visit_literal(&self, l: &Literal, ctx: &Context) -> Vec<Quadruple> {
        todo!()
    }
}

#[derive(Debug)]
pub enum Quadruple {
    Label(LabelType, String),
    Value(String)
}
#[derive(Debug)]
pub enum Function {
    Main,
    Secondary
}
#[derive(Debug)]
pub enum LabelType {
    BEGIN,
    END
}
