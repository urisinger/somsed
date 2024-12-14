use std::collections::HashMap;
use std::iter;

use anyhow::Result;
use lazy_static::lazy_static;
use pest::iterators::Pairs;

use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "lang/desmos.pest"] // Point to the grammar file
struct ExprParser;

lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        PrattParser::new()
            .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
            .op(Op::infix(Rule::mul, Assoc::Left)
                | Op::infix(Rule::div, Assoc::Left)
                | Op::infix(Rule::paren, Assoc::Left))
            .op(Op::infix(Rule::relop, Assoc::Left))
            .op(Op::postfix(Rule::fac) | Op::postfix(Rule::pow))
            .op(Op::prefix(Rule::neg)
                | Op::prefix(Rule::sqrt)
                | Op::prefix(Rule::sin)
                | Op::prefix(Rule::cos)
                | Op::prefix(Rule::tan)
                | Op::prefix(Rule::csc)
                | Op::prefix(Rule::sec)
                | Op::prefix(Rule::cot)
                | Op::prefix(Rule::invsin)
                | Op::prefix(Rule::invcos)
                | Op::prefix(Rule::invtan)
                | Op::prefix(Rule::invcsc)
                | Op::prefix(Rule::invsec)
                | Op::prefix(Rule::invcot))
    };
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Implicit {
        lhs: Node,
        op: ComparisonOp,
        rhs: Node,
    },
    Explicit {
        expr: Node,
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

impl Expr {
    pub fn parse(s: &str) -> Result<Self> {
        let pairs = ExprParser::parse(Rule::program, s)?
            .next()
            .unwrap()
            .into_inner();
        let mut expr = Self::decide_expr_type(parse_expr(pairs)?);
        expr.replace_scope();
        Ok(expr)
    }

    fn replace_scope(&mut self) {
        match self {
            Expr::Explicit { expr } => {
                let mut scope = HashMap::with_capacity(1);
                scope.insert("x", 0);
                expr.replace_scope(&scope);
            }
            Expr::Implicit { lhs, rhs, .. } => {
                let mut scope = HashMap::with_capacity(1);
                scope.insert("x", 0);

                scope.insert("y", 1);

                lhs.replace_scope(&scope);
                rhs.replace_scope(&scope);
            }
            Expr::VarDef { rhs, .. } => rhs.replace_scope(&HashMap::new()),
            Expr::FnDef { args, rhs, .. } => rhs.replace_scope(
                &args
                    .iter()
                    .enumerate()
                    .map(|(i, s)| (s.as_str(), i))
                    .collect(),
            ),
        }
    }

    fn decide_expr_type(node: Node) -> Self {
        if let Node::Comp { lhs, op, rhs } = node {
            match *lhs {
                Node::Ident(ident) => {
                    if ident == "x" || ident == "y" {
                        Self::Implicit {
                            lhs: Node::Ident(ident),
                            op,
                            rhs: *rhs,
                        }
                    } else {
                        Expr::VarDef { ident, rhs: *rhs }
                    }
                }
                Node::FnCall { ident, args }
                    if args.iter().all(|node| matches!(node, Node::Ident(_))) =>
                {
                    let args = args
                        .into_iter()
                        .map(|node| {
                            if let Node::Ident(ident) = node {
                                ident
                            } else {
                                unreachable!("should have been verified before")
                            }
                        })
                        .collect();

                    Self::FnDef {
                        ident,
                        args,
                        rhs: *rhs,
                    }
                }
                _ => Self::Implicit {
                    lhs: *lhs,
                    op,
                    rhs: *rhs,
                },
            }
        } else {
            Self::Explicit { expr: node }
        }
    }
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
    Tuple {
        args: Vec<Node>,
    },
    Comp {
        lhs: Box<Node>,
        op: ComparisonOp,
        rhs: Box<Node>,
    },
    FnArg {
        index: usize,
    },
}
impl Node {
    pub fn replace_scope(&mut self, scope: &HashMap<&str, usize>) {
        match self {
            Node::FnArg { .. } => {}
            Node::Lit(Literal::Float(_)) => {}
            Node::Lit(Literal::List(nodes)) => {
                for node in nodes {
                    node.replace_scope(scope);
                }
            }
            Node::Ident(ident) => {
                if let Some(id) = scope.get(ident.as_str()) {
                    *self = Node::FnArg { index: *id }
                }
            }
            Node::BinOp { lhs, rhs, .. } => {
                lhs.replace_scope(scope);
                rhs.replace_scope(scope);
            }
            Node::UnaryOp { val, .. } => {
                val.replace_scope(scope);
            }
            Node::FnCall { args, .. } => {
                for arg in args {
                    arg.replace_scope(scope);
                }
            }
            Node::Tuple { args } => {
                for arg in args {
                    arg.replace_scope(scope);
                }
            }
            Node::Comp { lhs, rhs, .. } => {
                lhs.replace_scope(scope);
                rhs.replace_scope(scope);
            }
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
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
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

fn parse_expr(pairs: Pairs<Rule>) -> Result<Node> {
    PRATT_PARSER
        .map_primary(|primary| {
            Ok(match primary.as_rule() {
                Rule::int => Node::Lit(Literal::Float(primary.as_str().parse::<u64>()? as f64)),
                Rule::ident => {
                    let name = primary.as_str().to_string();

                    Node::Ident(name)
                }
                Rule::expr => parse_expr(primary.into_inner())?,
                Rule::comparison => parse_expr(primary.into_inner())?,
                Rule::fn_call => {
                    let mut pairs = primary.into_inner();
                    let func_name = pairs.next().unwrap();
                    match func_name.as_rule() {
                        Rule::ident => {
                            let args = pairs
                                .map(|expr| parse_expr(expr.into_inner()))
                                .collect::<Result<Vec<_>>>()?;
                            let ident = func_name.as_str().to_string();

                            Node::FnCall { args, ident }
                        }
                        _ => {
                            if pairs.len() == 0 {
                                parse_expr(func_name.into_inner())?
                            } else {
                                let args = iter::once(func_name)
                                    .chain(pairs)
                                    .map(|expr| parse_expr(expr.into_inner()))
                                    .collect::<Result<Vec<_>>>()?;

                                Node::Tuple { args }
                            }
                        }
                    }
                }
                Rule::list => Node::Lit(Literal::List(
                    primary
                        .into_inner()
                        .map(|pair| parse_expr(pair.into_inner()))
                        .collect::<Result<Vec<_>>>()?,
                )),
                Rule::float => Node::Lit(Literal::Float(primary.as_str().parse::<f64>()?)),
                Rule::frac => {
                    let mut pairs = primary.into_inner();
                    let numerator = pairs.next().unwrap();
                    let denominator = pairs.next().unwrap();
                    Node::BinOp {
                        lhs: Box::new(parse_expr(numerator.into_inner())?),
                        op: BinaryOp::Div,
                        rhs: Box::new(parse_expr(denominator.into_inner())?),
                    }
                }
                rule => unreachable!(
                    "Expr::parse expected fn_call, int, expr, ident, found {:?}",
                    rule
                ),
            })
        })
        .map_prefix(|op, rhs| {
            Ok(match op.as_rule() {
                Rule::neg => Node::UnaryOp {
                    val: Box::new(rhs?),
                    op: UnaryOp::Neg,
                },

                rule => Node::UnaryOp {
                    val: Box::new(rhs?),
                    op: match rule {
                        Rule::neg => UnaryOp::Neg,
                        Rule::sqrt => UnaryOp::Sqrt,
                        Rule::sin => UnaryOp::Sin,
                        Rule::cos => UnaryOp::Cos,
                        Rule::tan => UnaryOp::Tan,
                        Rule::csc => UnaryOp::Csc,
                        Rule::sec => UnaryOp::Sec,
                        Rule::cot => UnaryOp::Cot,
                        Rule::invsin => UnaryOp::InvSin,
                        Rule::invcos => UnaryOp::InvCos,
                        Rule::invtan => UnaryOp::InvTan,
                        Rule::invcsc => UnaryOp::InvCsc,
                        Rule::invsec => UnaryOp::InvSec,
                        Rule::invcot => UnaryOp::InvCot,
                        _ => unreachable!(),
                    },
                },
            })
        })
        .map_postfix(|lhs, op| {
            Ok(match op.as_rule() {
                Rule::pow => {
                    let mut pairs = op.into_inner();
                    let exponent = pairs.next().unwrap();
                    Node::BinOp {
                        lhs: Box::new(lhs?),
                        op: BinaryOp::Pow,
                        rhs: Box::new(parse_expr(exponent.into_inner())?),
                    }
                }

                _ => unreachable!(),
            })
        })
        .map_infix(|lhs, op, rhs| {
            Ok(match op.as_rule() {
                Rule::relop => {
                    let op = match op.as_str() {
                        "=" => ComparisonOp::Eq,
                        ">" => ComparisonOp::Gt,
                        "<" => ComparisonOp::Lt,
                        ">=" => ComparisonOp::Gte,
                        "<=" => ComparisonOp::Lte,
                        rule => {
                            unreachable!("expected eq, gt, lt, gte, lte found {:?}", rule)
                        }
                    };
                    Node::Comp {
                        lhs: Box::new(lhs?),
                        rhs: Box::new(rhs?),
                        op,
                    }
                }
                _ => Node::BinOp {
                    lhs: Box::new(lhs?),
                    op: match op.as_rule() {
                        Rule::add => BinaryOp::Add,
                        Rule::sub => BinaryOp::Sub,
                        Rule::mul => BinaryOp::Mul,
                        Rule::div => BinaryOp::Div,
                        Rule::pow => BinaryOp::Pow,
                        Rule::paren => BinaryOp::Mul,

                        _ => unreachable!(),
                    },
                    rhs: Box::new(rhs?),
                },
            })
        })
        .parse(pairs)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! generate_tests {
    ( $( $name:ident: $input:expr => $expected:expr ),* $(,)? ) => {
        #[cfg(test)]
        mod tests {
            use super::*;

            $(
                #[test]
                fn $name() {
                    let input = $input;
                    let parsed = Expr::parse(input).expect("Parsing failed");
                    assert_eq!(parsed, $expected);
                }
            )*
        }
    };
}

    generate_tests! {
        test_literal_float: "42.0" => Expr::Explicit {
            expr: Node::Lit(Literal::Float(42.0))
        },
        test_literal_integer_as_float: "42" => Expr::Explicit {
            expr: Node::Lit(Literal::Float(42.0))
        },
        test_variable_definition: "a = 10" => Expr::VarDef {
            ident: "a".to_string(),
            rhs: Node::Lit(Literal::Float(10.0)),
        },
        test_simple_addition: "3 + 4" => Expr::Explicit {
            expr: Node::BinOp {
                lhs: Box::new(Node::Lit(Literal::Float(3.0))),
                op: BinaryOp::Add,
                rhs: Box::new(Node::Lit(Literal::Float(4.0))),
            }
        },
        test_nested_expression: "(1 + 2) * 3" => Expr::Explicit {
            expr: Node::BinOp {
                lhs: Box::new(Node::BinOp {
                    lhs: Box::new(Node::Lit(Literal::Float(1.0))),
                    op: BinaryOp::Add,
                    rhs: Box::new(Node::Lit(Literal::Float(2.0))),
                }),
                op: BinaryOp::Mul,
                rhs: Box::new(Node::Lit(Literal::Float(3.0))),
            }
        },
        test_function_call: "\\sin(0)" => Expr::Explicit {
            expr: Node::UnaryOp {
                op: UnaryOp::Sin,
                val: Box::new(Node::Lit(Literal::Float(0.0)))
            }
        },
        test_implicit_relation: "x > 5" => Expr::Implicit {
            lhs: Node::FnArg{index: 0},
            op: ComparisonOp::Gt,
            rhs: Node::Lit(Literal::Float(5.0)),
        },
        test_complex_implicit_relation: "x + y = 10" => Expr::Implicit {
            lhs: Node::BinOp {
                lhs: Box::new(Node::FnArg{index: 0}),
                op: BinaryOp::Add,
                rhs: Box::new(Node::FnArg{index: 1}),
            },
            op: ComparisonOp::Eq,
            rhs: Node::Lit(Literal::Float(10.0)),
        },
        test_function_definition: "g(a, b) = a * b" => Expr::FnDef {
            ident: "g".to_string(),
            args: vec!["a".to_string(), "b".to_string()],
            rhs: Node::BinOp {
                lhs: Box::new(Node::FnArg { index: 0 }),
                op: BinaryOp::Mul,
                rhs: Box::new(Node::FnArg { index: 1 }),
            }
        },
        test_tuple: "(1, 2, 3)" => Expr::Explicit {
            expr: Node::Tuple {
                args: vec![
                    Node::Lit(Literal::Float(1.0)),
                    Node::Lit(Literal::Float(2.0)),
                    Node::Lit(Literal::Float(3.0)),
                ]
            }
        },
        test_fraction: "1 / 2" => Expr::Explicit {
            expr: Node::BinOp {
                lhs: Box::new(Node::Lit(Literal::Float(1.0))),
                op: BinaryOp::Div,
                rhs: Box::new(Node::Lit(Literal::Float(2.0))),
            }
        },
        test_exponentiation: "2 ^ {3}" => Expr::Explicit {
            expr: Node::BinOp {
                lhs: Box::new(Node::Lit(Literal::Float(2.0))),
                op: BinaryOp::Pow,
                rhs: Box::new(Node::Lit(Literal::Float(3.0))),
            }
        },
        test_unary_negation: "-5" => Expr::Explicit {
            expr: Node::UnaryOp {
                val: Box::new(Node::Lit(Literal::Float(5.0))),
                op: UnaryOp::Neg,
            }
        },
        test_unary_function: "\\sqrt(9)" => Expr::Explicit {
            expr: Node::UnaryOp  {
                op: UnaryOp::Sqrt,
                val: Box::new(Node::Lit(Literal::Float(9.0)))
            }
        },
        test_complex_nested_expression: "\\sqrt((3 + 4) * 2)" => Expr::Explicit {
            expr: Node::UnaryOp {
                op: UnaryOp::Sqrt,
                val: Box::new(Node::BinOp {
                    lhs: Box::new(Node::BinOp {
                        lhs: Box::new(Node::Lit(Literal::Float(3.0))),
                        op: BinaryOp::Add,
                        rhs: Box::new(Node::Lit(Literal::Float(4.0))),
                    }),
                    op: BinaryOp::Mul,
                    rhs: Box::new(Node::Lit(Literal::Float(2.0))),
                })
            }
        }
    }
}
