use std::collections::HashMap;

use op::{eval_binop, eval_unary_op};
use thiserror::Error;

use crate::{
    expressions::ExpressionId,
    lang::{
        parser::{BinaryOp, Expr, Literal, Node, ResolvedExpr},
        value::{ListValue, Value, ValueType},
    },
};

mod op;

use super::llvm::jit::PointLayout;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Unknown identifier: {0}")]
    UnknownIdent(String),
    #[error("Wrong identifier type")]
    WrongIdentType,
    #[error("Inconsistent list types")]
    InconsistentListTypes,
    #[error("Nested lists are not allowed")]
    NestedListsNotAllowed,
    #[error("Unexpected type encountered during evaluation")]
    UnexpectedType,
    #[error("Argument count mismatch")]
    ArgumentCountMismatch,
    #[error("Invalid argument index")]
    InvalidArgumentIndex,
    #[error("Operation not implemented")]
    NotImplemented,
    #[error("Comparrison should not apear here")]
    UnexpectedComparison,
}

impl Node {
    /// Evaluate a node.
    ///
    /// - `args` is the list of argument nodes (for function bodies referring to FnArg).
    /// - `exprs` is the mapping from ExpressionId to ResolvedExpr (for looking up function/variable definitions).
    /// - `idents` maps identifier names to ExpressionId.
    pub fn eval(
        &self,
        args: &[Value],
        exprs: &HashMap<ExpressionId, ResolvedExpr>,
        idents: &HashMap<String, ExpressionId>,
    ) -> Result<Value, EvalError> {
        match self {
            Node::Lit(literal) => literal.eval(args, exprs, idents),

            Node::Ident(ident) => {
                let expr_id = idents
                    .get(ident)
                    .ok_or_else(|| EvalError::UnknownIdent(ident.clone()))?;
                let resolved_expr = exprs
                    .get(expr_id)
                    .ok_or_else(|| EvalError::UnknownIdent(ident.clone()))?;
                match &resolved_expr.expr {
                    Expr::VarDef { rhs, .. } => rhs.eval(args, exprs, idents),
                    _ => Err(EvalError::WrongIdentType),
                }
            }

            Node::BinOp { lhs, op, rhs } => {
                let left = lhs.eval(args, exprs, idents)?;
                let right = rhs.eval(args, exprs, idents)?;
                eval_binop(left, *op, right)
            }

            Node::UnaryOp { val, op } => {
                let value = val.eval(args, exprs, idents)?;
                eval_unary_op(value, *op)
            }

            Node::FnCall {
                ident,
                args: call_args,
            } => {
                let expr_id = idents
                    .get(ident)
                    .ok_or_else(|| EvalError::UnknownIdent(ident.clone()))?;
                let resolved_expr = exprs
                    .get(expr_id)
                    .ok_or_else(|| EvalError::UnknownIdent(ident.clone()))?;
                match &resolved_expr.expr {
                    Expr::FnDef {
                        args: param_names,
                        rhs,
                        ..
                    } => {
                        if call_args.len() != param_names.len() {
                            return Err(EvalError::ArgumentCountMismatch);
                        }
                        let evaluated_args: Vec<Value> = call_args
                            .iter()
                            .map(|arg| arg.eval(args, exprs, idents))
                            .collect::<Result<Vec<Value>, _>>()?;
                        rhs.eval(&evaluated_args, exprs, idents)
                    }
                    Expr::VarDef { rhs, .. } => {
                        if call_args.len() == 1 {
                            let lhs_value = rhs.eval(args, exprs, idents)?;
                            let rhs_value = call_args[0].eval(args, exprs, idents)?;
                            eval_binop(lhs_value, BinaryOp::Mul, rhs_value)
                        } else {
                            Err(EvalError::WrongIdentType)
                        }
                    }
                    _ => Err(EvalError::WrongIdentType),
                }
            }

            Node::FnArg { index } => {
                let arg_node = args.get(*index).ok_or(EvalError::InvalidArgumentIndex)?;
                Ok(arg_node.clone())
            }
        }
    }
}

impl Literal {
    pub fn eval(
        &self,
        args: &[Value],
        exprs: &HashMap<ExpressionId, ResolvedExpr>,
        idents: &HashMap<String, ExpressionId>,
    ) -> Result<Value, EvalError> {
        match self {
            Literal::Float(n) => Ok(Value::Number(*n)),

            Literal::Point(x_literal, y_literal) => {
                let x_value = x_literal.eval(args, exprs, idents)?;
                let y_value = y_literal.eval(args, exprs, idents)?;

                match (x_value, y_value) {
                    (Value::Number(x_num), Value::Number(y_num)) => {
                        Ok(Value::Point { x: x_num, y: y_num })
                    }
                    _ => Err(EvalError::UnexpectedType),
                }
            }

            Literal::List(nodes) => {
                let mut iter = nodes.iter();

                if let Some(first_node) = iter.next() {
                    let first_value = first_node.eval(args, exprs, idents)?;
                    let expected_type = first_value.get_type();
                    let mut values = Vec::with_capacity(nodes.len());
                    values.push(first_value);

                    for node in iter {
                        let value = node.eval(args, exprs, idents)?;
                        if value.get_type() == expected_type {
                            values.push(value);
                        } else {
                            return Err(EvalError::InconsistentListTypes);
                        }
                    }

                    match expected_type {
                        ValueType::Number => {
                            let nums = values
                                .into_iter()
                                .map(|v| {
                                    if let Value::Number(n) = v {
                                        n
                                    } else {
                                        unreachable!(
                                            "Type was already checked; this should never happen"
                                        )
                                    }
                                })
                                .collect();
                            Ok(Value::List(ListValue::NumberList(nums)))
                        }
                        ValueType::Point => {
                            let points = values
                                .into_iter()
                                .map(|v| {
                                    if let Value::Point { x, y } = v {
                                        PointLayout { x, y }
                                    } else {
                                        unreachable!(
                                            "Type was already checked; this should never happen"
                                        )
                                    }
                                })
                                .collect();
                            Ok(Value::List(ListValue::PointList(points)))
                        }
                        ValueType::List(_) => Err(EvalError::NestedListsNotAllowed),
                    }
                } else {
                    Ok(Value::List(ListValue::NumberList(vec![])))
                }
            }
        }
    }
}
