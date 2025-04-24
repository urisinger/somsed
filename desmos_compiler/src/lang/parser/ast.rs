use std::collections::{HashMap, HashSet};

use anyhow::{bail, Result};

use crate::{expressions::Expressions, lang::codegen::ir::IRType};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOperator {
    Neg,
    Fac,
    Sqrt,
    Norm,
    PointX,
    PointY,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Dot,
    Cross,
    Index,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ComparisonOperator {
    Equal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, PartialEq)]
pub struct ChainedComparison {
    pub operands: Vec<Expression>,
    pub operators: Vec<ComparisonOperator>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SumProdKind {
    Sum,
    Prod,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Number(f64),
    Point(Box<Expression>, Box<Expression>),
    Identifier(String),
    List(Vec<Expression>),
    ListRange {
        before_ellipsis: Vec<Expression>,
        after_ellipsis: Vec<Expression>,
    },
    UnaryOperation {
        operation: UnaryOperator,
        arg: Box<Expression>,
    },
    BinaryOperation {
        operation: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    CallOrMultiply {
        callee: String,
        args: Vec<Expression>,
    },
    Call {
        callee: String,
        args: Vec<Expression>,
    },
    ChainedComparison(ChainedComparison),
    Piecewise {
        test: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Option<Box<Expression>>,
    },
    SumProd {
        kind: SumProdKind,
        variable: String,
        lower_bound: Box<Expression>,
        upper_bound: Box<Expression>,
        body: Box<Expression>,
    },
    With {
        body: Box<Expression>,
        substitutions: Vec<(String, Expression)>,
    },
    For {
        body: Box<Expression>,
        lists: Vec<(String, Expression)>,
    },
}

impl Expression {
    pub fn used_functions(
        &self,
        env: &Expressions,
        param_types: &HashMap<String, IRType>,
    ) -> Result<HashSet<(String, Vec<IRType>)>> {
        let mut fns = HashSet::new();
        self.collect_functions(env, param_types, &mut fns)?;
        Ok(fns)
    }

    fn collect_functions(
        &self,
        env: &Expressions,
        param_types: &HashMap<String, IRType>,
        fns: &mut HashSet<(String, Vec<IRType>)>,
    ) -> Result<()> {
        match self {
            /*Node::FnCall { ident, args } => {
                match env.get_expr(ident.as_str()) {
                    Some(Expr::FnDef { .. }) => {
                        fns.insert((
                            ident.clone(),
                            args.iter()
                                .map(|arg| arg.ty(env, param_types))
                                .collect::<Result<Vec<_>, _>>()?,
                        ));
                    }
                    Some(Expr::VarDef { .. }) => {
                        if args.len() != 1 {
                            bail!(
                                "{ident} is a variable and cannot be called with {} arguments",
                                args.len()
                            );
                        }
                    }
                    Some(_) => {
                        bail!("{ident} is not a function or variable");
                    }
                    None => {
                        bail!("Unknown identifier: {ident}");
                    }
                }

                for arg in args {
                    arg.collect_functions(env, param_types, fns)?;
                }
            }
            }

            Node::UnaryOp { val, .. } => val.collect_functions(env, param_types, fns)?,
            Node::BinOp { lhs, rhs, .. } => {
                        lhs.collect_functions(env, param_types, fns)?;
                        rhs.collect_functions(env, param_types, fns)?;
                    }
            Node::Extract { val, .. } => val.collect_functions(env, param_types, fns)?,
            Node::Index { list, index } => {
                        list.collect_functions(env, param_types, fns)?;
                        index.collect_functions(env, param_types, fns)?;
                    }
            Node::Lit(Literal::List(nodes)) => {
                        for n in nodes {
                            n.collect_functions(env, param_types, fns)?;
                        }
                    }
            Node::Lit(Literal::Point(x, y)) => {
                        x.collect_functions(env, param_types, fns)?;
                        y.collect_functions(env, param_types, fns)?;
                    }
            Node::Ident(_) | Node::FnArg { .. } | Node::Lit(Literal::Float(_)) => {}
            */
            Expression::Point(x, y) => {
                x.collect_functions(env, param_types, fns)?;
                y.collect_functions(env, param_types, fns)?;
            }
            Expression::List(expressions) => {
                for n in expressions {
                    n.collect_functions(env, param_types, fns)?;
                }
            }
            Expression::UnaryOperation { operation, arg } => {
                arg.collect_functions(env, param_types, fns)?;
            }
            Expression::BinaryOperation {
                operation,
                left,
                right,
            } => {
                left.collect_functions(env, param_types, fns)?;
                right.collect_functions(env, param_types, fns)?;
            }
            Expression::CallOrMultiply { callee, args } => {
                match env.get_expr(callee.as_str()) {
                    Some(ExpressionListEntry::FunctionDeclaration { .. }) => {
                        fns.insert((
                            callee.clone(),
                            args.iter()
                                .map(|arg| arg.ty(env, param_types))
                                .collect::<Result<Vec<_>, _>>()?,
                        ));
                    }
                    Some(ExpressionListEntry::Assignment { .. }) => {
                        if args.len() != 1 {
                            bail!(
                                "{callee} is a variable and cannot be called with {} arguments",
                                args.len()
                            );
                        }
                    }
                    Some(_) => {
                        bail!("{callee} is not a function or variable");
                    }
                    None => {
                        bail!("Unknown identifier: {callee}");
                    }
                }

                for arg in args {
                    arg.collect_functions(env, param_types, fns)?;
                }
            }
            Expression::ChainedComparison(ChainedComparison { operands, .. }) => {
                for expr in operands {
                    expr.collect_functions(env, param_types, fns)?;
                }
            }
            Expression::Piecewise {
                test,
                consequent,
                alternate,
            } => todo!(),
            Expression::SumProd {
                kind,
                variable,
                lower_bound,
                upper_bound,
                body,
            } => todo!(),
            Expression::With {
                body,
                substitutions,
            } => todo!(),
            Expression::For { body, lists } => todo!(),
            Expression::Number(_) | Expression::Identifier(_) => {}

            Expression::ListRange {
                before_ellipsis,
                after_ellipsis,
            } => todo!(),

            Expression::Call { callee, args } => todo!(),
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionListEntry {
    Assignment {
        name: String,
        value: Expression,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<String>,
        body: Expression,
    },
    Relation(ChainedComparison),
    Expression(Expression),
}
