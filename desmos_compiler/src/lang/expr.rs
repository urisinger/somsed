use std::collections::{HashMap, HashSet};

use anyhow::Result;
use pest::Parser;

use super::parser::{parse_expr, ExprParser, Rule};

#[derive(Debug)]
pub struct ResolvedExpr {
    pub expr: Expr,
    pub used_idents: HashSet<String>,
}

impl ResolvedExpr {
    pub fn parse(s: &str) -> Result<Self> {
        let pairs = ExprParser::parse(Rule::comparison, s)?
            .next()
            .unwrap()
            .into_inner();

        let mut expr = parse_expr(pairs)?;

        let used_idents = expr.used_idents();
        expr.replace_scope();

        Ok(Self { expr, used_idents })
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Implicit {
        lhs: Node,
        op: ComparisonOp,
        rhs: Node,
    },
    Explicit {
        lhs: Node,
    },
    VarDef {
        ident: String,
        rhs: Node,
    },
    FnDef {
        ident: String,
        args: Vec<String>,
        rhs: Node,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Lit(Literal),
    Ident(String),
    BinOp {
        lhs: Box<Node>,
        op: BinaryOp,
        rhs: Box<Node>,
    },
    UnaryOp {
        val: Box<Node>,
        op: UnaryOp,
    },
    Extract {
        val: Box<Node>,
        index: usize,
    },
    FnCall {
        ident: String,
        args: Vec<Node>,
    },
    FnArg {
        index: usize,
    },
}

impl Expr {
    pub fn parse(s: &str) -> Result<Self> {
        let pairs = ExprParser::parse(Rule::comparison, s)?
            .next()
            .unwrap()
            .into_inner();
        let mut expr = parse_expr(pairs)?;
        expr.replace_scope();
        Ok(expr)
    }

    fn used_idents(&self) -> HashSet<String> {
        let mut idents = HashSet::new();

        match self {
            Expr::Explicit { lhs: expr } => expr.used_idents(&mut idents),
            Expr::Implicit { lhs, rhs, .. } => {
                lhs.used_idents(&mut idents);
                rhs.used_idents(&mut idents);
            }
            Expr::VarDef { rhs, .. } => rhs.used_idents(&mut idents),
            Expr::FnDef { rhs, .. } => rhs.used_idents(&mut idents),
        }
        idents
    }

    fn replace_scope(&mut self) {
        match self {
            Expr::Explicit { lhs: expr } => {
                let mut scope = HashMap::with_capacity(1);

                scope.insert("x", 0);
                expr.replace_scope(&scope)
            }
            Expr::Implicit { lhs, rhs, .. } => {
                let mut scope = HashMap::with_capacity(2);
                scope.insert("x", 0);

                scope.insert("y", 1);

                lhs.replace_scope(&scope);
                rhs.replace_scope(&scope);
            }
            Expr::VarDef { rhs, .. } => {
                let scope = HashMap::with_capacity(0);
                rhs.replace_scope(&scope)
            }
            Expr::FnDef { args, rhs, .. } => rhs.replace_scope(
                &args
                    .iter()
                    .enumerate()
                    .map(|(i, s)| (s.as_str(), i))
                    .collect(),
            ),
        }
    }
}

impl Node {
    pub fn replace_scope(&mut self, scope: &HashMap<&str, usize>) {
        match self {
            Node::Ident(ident) => {
                if let Some(id) = scope.get(ident.as_str()) {
                    *self = Node::FnArg { index: *id };
                }
            }
            Node::FnArg { .. } => {}
            Node::Lit(Literal::Float(_)) => {}
            Node::Lit(Literal::List(nodes)) => {
                nodes.iter_mut().for_each(|node| node.replace_scope(scope))
            }
            Node::Lit(Literal::Point(x, y)) => {
                x.replace_scope(scope);
                y.replace_scope(scope);
            }
            Node::Extract { val, .. } => {
                val.replace_scope(scope);
            }

            Node::BinOp { lhs, rhs, .. } => {
                lhs.replace_scope(scope);
                rhs.replace_scope(scope);
            }
            Node::UnaryOp { val, .. } => val.replace_scope(scope),
            Node::FnCall { args, .. } => args.iter_mut().for_each(|node| node.replace_scope(scope)),
        }
    }

    fn used_idents(&self, idents: &mut HashSet<String>) {
        match self {
            Node::Ident(ident) => {
                idents.insert(ident.as_str().to_owned());
            }
            Node::FnArg { .. } => {}
            Node::Lit(Literal::Float(_)) => {}
            Node::Lit(Literal::List(nodes)) => {
                nodes.iter().for_each(|node| node.used_idents(idents))
            }
            Node::Lit(Literal::Point(x, y)) => {
                x.used_idents(idents);
                y.used_idents(idents);
            }
            Node::Extract { val, .. } => {
                val.used_idents(idents);
            }

            Node::BinOp { lhs, rhs, .. } => {
                lhs.used_idents(idents);
                rhs.used_idents(idents);
            }
            Node::UnaryOp { val, .. } => val.used_idents(idents),
            Node::FnCall { args, .. } => args.iter().for_each(|node| node.used_idents(idents)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComparisonOp {
    Eq,
    Gt,
    Lt,
    Gte,
    Lte,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Float(f64),
    List(Vec<Node>),
    Point(Box<Node>, Box<Node>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Dot,
    Paran,
    Div,
    Pow,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Neg,
    Sqrt,
    Sin,
    Cos,
    Tan,
    Csc,
    Sec,
    Cot,
    InvSin,
    InvCos,
    InvTan,
    InvCsc,
    InvSec,
    InvCot,
    Fac,
}
