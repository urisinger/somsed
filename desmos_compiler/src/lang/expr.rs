use std::collections::{HashMap, HashSet};

use anyhow::{bail, Context, Result};
use pest::Parser;

use crate::expressions::Expressions;

use super::{
    generic_value::{ListType, ScalerType, ValueType},
    parser::{parse_expr, ExprParser, Rule},
};

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

            Node::BinOp { lhs, rhs, .. } => {
                lhs.used_idents(idents);
                rhs.used_idents(idents);
            }
            Node::UnaryOp { val, .. } => val.used_idents(idents),
            Node::FnCall { args, .. } => args.iter().for_each(|node| node.used_idents(idents)),
        }
    }

    pub fn return_type(&self, exprs: &Expressions, call_types: &[ValueType]) -> Result<ValueType> {
        Ok(match self {
            Node::Lit(Literal::Float(_)) => ValueType::Number(()),
            Node::Lit(Literal::List(elements)) => ValueType::List(
                match elements.iter().try_fold(None, |acc, expr| {
                    expr.return_type(exprs, call_types)
                        .map(|current| match acc {
                            None => Some(current),
                            Some(a) if a == current => Some(current),
                            _ => None,
                        })
                })? {
                    Some(ValueType::Number(_)) => ListType::Number(()),
                    Some(ValueType::Point(_)) => ListType::PointList(()),
                    Some(ValueType::List(_)) => bail!("Lists in lists are not allowed"),
                    None => ListType::Number(()),
                },
            ),
            Node::Lit(Literal::Point(..)) => ValueType::Point(()),
            Node::Ident(ident) => {
                match exprs
                    .get_expr(ident)
                    .context(format!("failed to get expr for ident, {}", ident))?
                {
                    Expr::VarDef { rhs, .. } => rhs.return_type(exprs, call_types)?,
                    expr => bail!("expr has wrong type, this should not happen: {expr:?}"),
                }
            }
            Node::BinOp { op, lhs, rhs } => {
                let lhs_type = lhs.return_type(exprs, call_types)?;
                let rhs_type = rhs.return_type(exprs, call_types)?;
                binop_return_type(lhs_type, *op, rhs_type)?
            }
            Node::UnaryOp { val, op } => match val.return_type(exprs, call_types)? {
                ValueType::Number(_) => match op {
                    UnaryOp::Neg | UnaryOp::Sqrt | UnaryOp::Sin | UnaryOp::Cos | UnaryOp::Tan => {
                        ValueType::Number(())
                    }
                    _ => bail!("Unary operation `{op:?}` is not implemented for number"),
                },
                ValueType::Point(_) => match op {
                    UnaryOp::Neg => ValueType::Point(()),
                    UnaryOp::GetX => ValueType::Number(()),
                    UnaryOp::GetY => ValueType::Number(()),
                    _ => bail!("Unary op {:?} cant be applied to point", op),
                },
                ValueType::List(_) => bail!("Unary op {:?} cant be applied to list", op),
            },
            Node::FnCall { ident, args } => match exprs.get_expr(ident) {
                Some(Expr::FnDef { rhs, .. }) => {
                    let call_types = args
                        .iter()
                        .map(|arg| arg.return_type(exprs, call_types))
                        .collect::<Result<Vec<_>>>()?;
                    rhs.return_type(exprs, &call_types)?
                }
                Some(Expr::VarDef { rhs, .. }) => rhs.return_type(exprs, call_types)?,
                None => bail!("unknown ident {ident}"),
                _ => bail!("expr has the wrong type"),
            },
            Node::FnArg { index } => call_types[*index].clone(),
        })
    }
}

fn binop_return_type(lhs: ValueType, op: BinaryOp, rhs: ValueType) -> Result<ValueType> {
    match (lhs, rhs) {
        (
            lhs @ (ValueType::Number(_) | ValueType::Point(_)),
            rhs @ (ValueType::Number(_) | ValueType::Point(_)),
        ) => {
            let lhs_scaler = lhs.lift_scaler().expect("expected scaler");
            let rhs_scaler = rhs.lift_scaler().expect("expected scaler");
            Ok(
                match simple_binop_return_type(lhs_scaler, op, rhs_scaler)? {
                    ScalerType::Number(_) => ValueType::Number(()),
                    ScalerType::PointList(_) => ValueType::Point(()),
                },
            )
        }
        (ValueType::List(lhs), rhs @ (ValueType::Number(_) | ValueType::Point(_))) => {
            let rhs_scaler = rhs.lift_scaler().expect("expected scaler");
            let list_type = simple_binop_return_type(lhs, op, rhs_scaler)?;
            Ok(ValueType::List(list_type))
        }

        (lhs @ (ValueType::Number(_) | ValueType::Point(_)), ValueType::List(rhs)) => {
            let lhs_scaler = lhs.lift_scaler().expect("expected scaler");
            let list_type = simple_binop_return_type(lhs_scaler, op, rhs)?;
            Ok(ValueType::List(list_type))
        }
        (ValueType::List(_), ValueType::List(_)) => {
            bail!("Binary operations on lists not implemented yet")
        }
    }
}

fn simple_binop_return_type(lhs: ScalerType, op: BinaryOp, rhs: ScalerType) -> Result<ScalerType> {
    Ok(match (lhs, rhs) {
        (ScalerType::Number(_), ScalerType::Number(_)) => ScalerType::Number(()),
        (ScalerType::Number(_), ScalerType::PointList(_)) => match op {
            BinaryOp::Dot | BinaryOp::Paran => ScalerType::PointList(()),
            _ => bail!("Type error: Cant {op:?} number and point"),
        },
        (ScalerType::PointList(_), ScalerType::Number(_)) => match op {
            BinaryOp::Dot | BinaryOp::Paran | BinaryOp::Div => ScalerType::PointList(()),
            _ => bail!("Type error: Cant {op:?} point and Number"),
        },
        (ScalerType::PointList(_), ScalerType::PointList(_)) => match op {
            BinaryOp::Add | BinaryOp::Sub => ScalerType::PointList(()),
            _ => bail!("Type error:Cant {op:?} point and point"),
        },
    })
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
    GetX,
    GetY,
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
