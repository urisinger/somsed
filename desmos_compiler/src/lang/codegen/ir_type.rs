use std::collections::HashMap;

use anyhow::{anyhow, bail, Result};

use crate::{
    expressions::Expressions,
    lang::parser::ast::{BinaryOperator, Expression, ExpressionListEntry, UnaryOperator},
};

use super::ir::{IRScalerType, IRType};

impl Expression {
    /// Return the static IRType of this node.
    pub fn ty(
        &self,
        env: &Expressions,
        params: &HashMap<String, IRType>,
    ) -> anyhow::Result<IRType> {
        use IRScalerType::*;
        use IRType::*;

        match self {
            Expression::Number(_) => Ok(Scaler(Number)),
            Expression::Point(_, _) => Ok(Scaler(Point)),
            Expression::List(elements) => {
                // empty  →  default to Number list
                let first = elements
                    .first()
                    .map(|n| n.ty(env, params))
                    .transpose()?
                    .unwrap_or(Scaler(Number));

                // assert homogeneity
                for n in elements {
                    anyhow::ensure!(n.ty(env, params)? == first, "mixed element types in list");
                }
                match first {
                    Scaler(st) => Ok(List(st)),
                    List(_) => bail!("nested lists are not supported"),
                }
            }

            Expression::Identifier(name) => ty_ident(env, params, name),

            Expression::UnaryOperation { operation, arg } => {
                ty_unary(*operation, arg.ty(env, params)?)
            }
            Expression::BinaryOperation {
                operation,
                left,
                right,
            } => {
                let lt = left.ty(env, params)?;
                let rt = right.ty(env, params)?;
                ty_binary(lt, *operation, rt)
            }
            Expression::CallOrMultiply { callee, args } => ty_call(env, params, callee, args),

            Expression::For { body, lists } => {
                let mut scoped_params = params.clone();

                for (name, expr) in lists {
                    let ty = expr.ty(env, &scoped_params)?;
                    match ty {
                        IRType::List(scaler_type) => {
                            scoped_params.insert(name.clone(), IRType::Scaler(scaler_type));
                        }
                        IRType::Scaler(_) => bail!("expected List, found Scaler"),
                    }
                }

                // Type of the body inside the loop (with new bindings)
                let body_ty = body.ty(env, &scoped_params)?;

                match body_ty {
                    Scaler(s) => Ok(List(s)),
                    List(_) => bail!("Expected scaler, found list"),
                }
            }

            Expression::Call { callee, args } => todo!(),
            Expression::ChainedComparison(chained_comparison) => todo!(),
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

            Expression::ListRange {
                before_ellipsis,
                after_ellipsis,
            } => todo!(),
        }
    }
}

fn ty_binary(lhs: IRType, op: BinaryOperator, rhs: IRType) -> Result<IRType> {
    use IRType::*;

    match (lhs, op, rhs) {
        (Scaler(lhs), op, Scaler(rhs)) => Ok(Scaler(ty_scaler_binary(lhs, op, rhs)?)),

        (List(lhs), BinaryOperator::Index, Scaler(IRScalerType::Number)) => Ok(Scaler(lhs)),
        (List(lhs), op, Scaler(rhs))
        | (Scaler(lhs), op, List(rhs))
        | (List(lhs), op, List(rhs)) => Ok(List(ty_scaler_binary(lhs, op, rhs)?)),
    }
}

fn ty_scaler_binary(
    lhs: IRScalerType,
    op: BinaryOperator,
    rhs: IRScalerType,
) -> Result<IRScalerType> {
    use BinaryOperator::*;
    use IRScalerType::*;
    match (lhs, op, rhs) {
        /* ───────────────────  number  ⨯  number  ─────────────────── */
        (Number, Add | Sub | Dot | Mul | Div | Pow, Number) => Ok(Number),

        /* ───────────────────  point   ⨯  point   ─────────────────── */
        (Point, Add | Sub, Point) => Ok(Point),

        /* ────────────────  number ⨯ point / point ⨯ number  ──────────────── */
        (Number, Dot | Mul, Point) | (Point, Dot | Mul, Number) => Ok(Point),

        /* ─────────────────────  point  ÷  number  ───────────────────── */
        (Point, Div, Number) => Ok(Point),

        _ => bail!("type error: {op:?} is not defined for {lhs:?} and {rhs:?}"),
    }
}

fn ty_unary(op: UnaryOperator, inner: IRType) -> Result<IRType> {
    use IRScalerType::*;
    use IRType::*;
    use UnaryOperator::*;

    match (op, inner) {
        // number → number   (all unary ops on scalars allowed)
        (_, Scaler(Number)) => Ok(Scaler(Number)),

        // point → point  (only Neg is defined in code‑gen)
        (Neg, Scaler(Point)) => Ok(Scaler(Point)),
        (PointX, Scaler(Point)) => Ok(Scaler(Number)),
        (PointY, Scaler(Point)) => Ok(Scaler(Number)),

        // everything else is invalid
        _ => bail!("unary op {op:?} is not defined for {inner:?}"),
    }
}

fn ty_ident(
    env: &Expressions,
    params: &HashMap<String, IRType>,
    name: &str,
) -> anyhow::Result<IRType> {
    if let Some(ty) = params.get(name).cloned() {
        Ok(ty)
    } else {
        match env
            .get_expr(name)
            .ok_or_else(|| anyhow!("unknown ident {name}"))?
        {
            ExpressionListEntry::Assignment { value, .. } => value.ty(env, params),
            ExpressionListEntry::FunctionDeclaration { .. } => {
                bail!("{name} is a function, not a value")
            }
            _ => bail!("{name} is not an ident, this indicates a bug"),
        }
    }
}

fn ty_call(
    env: &Expressions,
    params: &HashMap<String, IRType>,
    ident: &str,
    args: &[Expression],
) -> anyhow::Result<IRType> {
    let ExpressionListEntry::FunctionDeclaration {
        body, parameters, ..
    } = env
        .get_expr(ident)
        .ok_or_else(|| anyhow!("unknown function {ident}"))?
    else {
        bail!("{ident} is not a function")
    };

    anyhow::ensure!(
        args.len() == parameters.len(),
        "parity mismatch calling {ident}"
    );

    // compute argument types
    let arg_tys: HashMap<String, IRType> = args
        .iter()
        .zip(parameters.iter())
        .map(|(expr, key)| Ok((key.to_string(), expr.ty(env, params)?)))
        .collect::<anyhow::Result<_>>()?;

    // Use the function body to get its return type,            <-- recursion!
    body.ty(env, &arg_tys)
}
