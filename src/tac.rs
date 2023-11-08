use crate::parser::{Expression, Literal, Node};

pub trait Visitor<T> {
    fn visit_expression(&self, e: &Expression) -> Vec<T>;
    fn visit_node(&self, n: &Node) -> Vec<T>;
    fn visit_literal(&self, l: &Literal) -> Vec<T>;
}
pub trait Visitable<T> {
    fn accept(&self, visitor: &dyn Visitor<T>) -> Vec<T>;
}

impl Visitable<Quadruple> for Node {
    fn accept(&self, visitor: &dyn Visitor<Quadruple>) -> Vec<Quadruple> {
        visitor.visit_node(&self)
    }
}

impl Visitable<Quadruple> for Expression {
    fn accept<>(&self, visitor: &dyn Visitor<Quadruple>) -> Vec<Quadruple> {
        visitor.visit_expression(&self)
    }
}

impl Visitable<Quadruple> for Literal {
    fn accept<>(&self, visitor: &dyn Visitor<Quadruple>) -> Vec<Quadruple> {
        visitor.visit_literal(&self)
    }
}

pub struct ThreeAddressCodeVisitor {
}

impl Visitor<Quadruple> for ThreeAddressCodeVisitor {
    fn visit_expression(&self, e: &Expression) -> Vec<Quadruple> {
        todo!()
    }

    fn visit_node(&self, n: &Node) -> Vec<Quadruple> {
        let mut quadruples = vec![];
        match n {
            Node::Program{method_declarations}=> {
                quadruples.push(Quadruple::Label(String::from("Start_Program")));
                for method in method_declarations {
                    quadruples.append(&mut method.accept(self as &dyn Visitor<Quadruple>))
                }
                quadruples.push(Quadruple::Label(String::from("End_Program")))
            }
            Node::MethodDeclaration { .. } => {}
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

    fn visit_literal(&self, l: &Literal) -> Vec<Quadruple> {
        todo!()
    }
}

#[derive(Debug)]
pub enum Quadruple {
    Label(String),
}
