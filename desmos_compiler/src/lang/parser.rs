use anyhow::Result;
use lazy_static::lazy_static;
use pest::iterators::Pairs;

use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest_derive::Parser;

use super::expr::{BinaryOp, ComparisonOp, Expr, Literal, Node, UnaryOp};

#[derive(Parser)]
#[grammar = "lang/desmos.pest"] // Point to the grammar file
pub struct ExprParser;

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

pub fn parse_expr(mut pairs: Pairs<Rule>) -> Result<Expr> {
    let lhs = pairs.next();
    let op = pairs.next();
    let rhs = pairs.next();
    match op {
        Some(op) => {
            let rhs = parse_node(rhs.expect("If op is provided rhs is expected").into_inner())?;
            let lhs = lhs.expect("expected atleast one expr").into_inner();
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

            if lhs.len() == 1 {
                let lhs_first = lhs.peek().unwrap();
                match lhs_first.as_rule() {
                    Rule::ident => {
                        let name = lhs_first.as_str().to_string();
                        if name == "x" || name == "y" {
                            Ok(Expr::Implicit {
                                lhs: Node::Ident(name),
                                op,
                                rhs,
                            })
                        } else {
                            Ok(Expr::VarDef { ident: name, rhs })
                        }
                    }
                    Rule::fn_call => {
                        let mut pairs = lhs_first.into_inner();
                        let func_name = pairs.next().unwrap();
                        let args = pairs
                            .map(|expr| parse_node(expr.into_inner()))
                            .collect::<Result<Vec<_>>>()?;
                        let ident = func_name.as_str().to_string();
                        if args.iter().all(|node| matches!(node, Node::Ident(_))) {
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

                            Ok(Expr::FnDef { ident, args, rhs })
                        } else {
                            Ok(Expr::Implicit {
                                lhs: Node::FnCall { ident, args },
                                op,
                                rhs,
                            })
                        }
                    }

                    _ => Ok(Expr::Implicit {
                        lhs: parse_node(lhs)?,
                        op,
                        rhs,
                    }),
                }
            } else {
                Ok(Expr::Implicit {
                    lhs: parse_node(lhs)?,
                    op,
                    rhs,
                })
            }
        }
        None => Ok(Expr::Explicit {
            lhs: parse_node(lhs.expect("expected atleast one expr").into_inner())?,
        }),
    }
}

fn parse_node(pairs: Pairs<Rule>) -> Result<Node> {
    PRATT_PARSER
        .map_primary(|primary| {
            Ok(match primary.as_rule() {
                Rule::int => Node::Lit(Literal::Float(primary.as_str().parse::<u64>()? as f64)),
                Rule::ident => {
                    let name = primary.as_str().to_string();

                    Node::Ident(name)
                }
                Rule::expr => parse_node(primary.into_inner())?,
                Rule::comparison => parse_node(primary.into_inner())?,
                Rule::fn_call => {
                    let mut pairs = primary.into_inner();
                    let func_name = pairs.next().unwrap();
                    let args = pairs
                        .map(|expr| parse_node(expr.into_inner()))
                        .collect::<Result<Vec<_>>>()?;
                    let ident = func_name.as_str().to_string();

                    Node::FnCall { args, ident }
                }
                Rule::point => {
                    let mut pairs = primary.into_inner();
                    let x = Box::new(parse_node(pairs.next().unwrap().into_inner())?);
                    let y = Box::new(parse_node(pairs.next().unwrap().into_inner())?);

                    Node::Lit(Literal::Point(x, y))
                }
                Rule::list => Node::Lit(Literal::List(
                    primary
                        .into_inner()
                        .map(|pair| parse_node(pair.into_inner()))
                        .collect::<Result<Vec<_>>>()?,
                )),
                Rule::float => Node::Lit(Literal::Float(primary.as_str().parse::<f64>()?)),
                Rule::frac => {
                    let mut pairs = primary.into_inner();
                    let numerator = pairs.next().unwrap();
                    let denominator = pairs.next().unwrap();
                    Node::BinOp {
                        lhs: Box::new(parse_node(numerator.into_inner())?),
                        op: BinaryOp::Div,
                        rhs: Box::new(parse_node(denominator.into_inner())?),
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
                        rhs: Box::new(parse_node(exponent.into_inner())?),
                    }
                }

                _ => unreachable!(),
            })
        })
        .map_infix(|lhs, op, rhs| {
            Ok(Node::BinOp {
                lhs: Box::new(lhs?),
                op: match op.as_rule() {
                    Rule::add => BinaryOp::Add,
                    Rule::sub => BinaryOp::Sub,
                    Rule::mul => BinaryOp::Dot,
                    Rule::div => BinaryOp::Div,
                    Rule::pow => BinaryOp::Pow,
                    Rule::paren => BinaryOp::Paran,

                    r => unreachable!("{r:?}"),
                },
                rhs: Box::new(rhs?),
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
            lhs: Node::Lit(Literal::Float(42.0))
        },
        test_literal_integer_as_float: "42" => Expr::Explicit {
            lhs: Node::Lit(Literal::Float(42.0))
        },
        test_variable_definition: "a = 10" => Expr::VarDef {
            ident: "a".to_string(),
            rhs: Node::Lit(Literal::Float(10.0)),
        },
        test_simple_addition: "3 + 4" => Expr::Explicit {
            lhs: Node::BinOp {
                lhs: Box::new(Node::Lit(Literal::Float(3.0))),
                op: BinaryOp::Add,
                rhs: Box::new(Node::Lit(Literal::Float(4.0))),
            }
        },
        test_nested_expression: "(1 + 2) * 3" => Expr::Explicit {
            lhs: Node::BinOp {
                lhs: Box::new(Node::BinOp {
                    lhs: Box::new(Node::Lit(Literal::Float(1.0))),
                    op: BinaryOp::Add,
                    rhs: Box::new(Node::Lit(Literal::Float(2.0))),
                }),
                op: BinaryOp::Dot,
                rhs: Box::new(Node::Lit(Literal::Float(3.0))),
            }
        },
        test_function_call: "\\sin(0)" => Expr::Explicit {
            lhs: Node::UnaryOp {
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
                op: BinaryOp::Dot,
                rhs: Box::new(Node::FnArg { index: 1 }),
            }
        },
        test_tuple: "(1, 2)" => Expr::Explicit {
            lhs: Node::Lit(Literal::Point(
                Box::new(Node::Lit(Literal::Float(1.0))),
                Box::new(Node::Lit(Literal::Float(2.0))),
            ))

        },
        test_fraction: "1 / 2" => Expr::Explicit {
            lhs: Node::BinOp {
                lhs: Box::new(Node::Lit(Literal::Float(1.0))),
                op: BinaryOp::Div,
                rhs: Box::new(Node::Lit(Literal::Float(2.0))),
            }
        },
        test_exponentiation: "2 ^ {3}" => Expr::Explicit {
            lhs: Node::BinOp {
                lhs: Box::new(Node::Lit(Literal::Float(2.0))),
                op: BinaryOp::Pow,
                rhs: Box::new(Node::Lit(Literal::Float(3.0))),
            }
        },
        test_unary_negation: "-5" => Expr::Explicit {
            lhs: Node::UnaryOp {
                val: Box::new(Node::Lit(Literal::Float(5.0))),
                op: UnaryOp::Neg,
            }
        },
        test_unary_function: "\\sqrt(9)" => Expr::Explicit {
            lhs: Node::UnaryOp  {
                op: UnaryOp::Sqrt,
                val: Box::new(Node::Lit(Literal::Float(9.0)))
            }
        },
        test_complex_nested_expression: "\\sqrt((3 + 4) * 2)" => Expr::Explicit {
            lhs: Node::UnaryOp {
                op: UnaryOp::Sqrt,
                val: Box::new(Node::BinOp {
                    lhs: Box::new(Node::BinOp {
                        lhs: Box::new(Node::Lit(Literal::Float(3.0))),
                        op: BinaryOp::Add,
                        rhs: Box::new(Node::Lit(Literal::Float(4.0))),
                    }),
                    op: BinaryOp::Dot,
                    rhs: Box::new(Node::Lit(Literal::Float(2.0))),
                })
            }
        }
    }
}
