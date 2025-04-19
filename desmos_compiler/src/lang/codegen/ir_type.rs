use anyhow::{anyhow, bail, Result};

use crate::{
    expressions::Expressions,
    lang::expr::{BinaryOp, Expr, Literal, Node, UnaryOp},
};

use super::ir::{IRScalerType, IRType};

impl Node {
    /// Return the static IRType of this node.
    pub fn ty(&self, env: &Expressions, params: &[IRType]) -> anyhow::Result<IRType> {
        use IRScalerType::*;
        use IRType::*;
        use Node::*;

        match self {
            Lit(Literal::Float(_)) => Ok(Scaler(Number)),
            Lit(Literal::Point(_, _)) => Ok(Scaler(Point)),
            Lit(Literal::List(elems)) => {
                // empty  →  default to Number list
                let first = elems
                    .first()
                    .map(|n| n.ty(env, params))
                    .transpose()?
                    .unwrap_or(Scaler(Number));

                // assert homogeneity
                for n in elems {
                    anyhow::ensure!(n.ty(env, params)? == first, "mixed element types in list");
                }
                match first {
                    Scaler(st) => Ok(List(st)),
                    List(_) => bail!("nested lists are not supported"),
                }
            }

            FnArg { index } => params
                .get(*index)
                .cloned()
                .ok_or_else(|| anyhow!("function has no arg #{index}")),

            Ident(name) => ty_ident(env, params, name),
            UnaryOp { val, op } => ty_unary(*op, val.ty(env, params)?),
            BinOp { lhs, rhs, op } => {
                let lt = lhs.ty(env, params)?;
                let rt = rhs.ty(env, params)?;
                ty_binary(lt, *op, rt)
            }
            Extract { val, .. } => val.ty(env, params),
            FnCall { ident, args } => ty_call(env, params, ident, args), // step 3 below
        }
    }
}

fn ty_binary(lhs: IRType, op: BinaryOp, rhs: IRType) -> Result<IRType> {
    use IRType::*;

    match (lhs, op, rhs) {
        (Scaler(lhs), op, Scaler(rhs)) => Ok(Scaler(ty_scaler_binary(lhs, op, rhs)?)),

        (List(lhs), op, Scaler(rhs)) => Ok(List(ty_scaler_binary(lhs, op, rhs)?)),

        (Scaler(lhs), op, List(rhs)) => Ok(List(ty_scaler_binary(lhs, op, rhs)?)),

        /* ── Everything else (lists, mixed scalars, unsupported op) ── */
        _ => bail!("type error: {op:?} is not defined for {lhs:?} and {rhs:?}"),
    }
}

fn ty_scaler_binary(lhs: IRScalerType, op: BinaryOp, rhs: IRScalerType) -> Result<IRScalerType> {
    use BinaryOp::*;
    use IRScalerType::*;
    match (lhs, op, rhs) {
        /* ───────────────────  number  ⨯  number  ─────────────────── */
        (Number, Add | Sub | Dot | Paran | Div | Pow, Number) => Ok(Number),

        /* ───────────────────  point   ⨯  point   ─────────────────── */
        (Point, Add | Sub, Point) => Ok(Point),

        /* ────────────────  number ⨯ point / point ⨯ number  ──────────────── */
        (Number, Dot | Paran, Point) | (Point, Dot | Paran, Number) => Ok(Point),

        /* ─────────────────────  point  ÷  number  ───────────────────── */
        (Point, Div, Number) => Ok(Point),

        _ => bail!("type error: {op:?} is not defined for {lhs:?} and {rhs:?}"),
    }
}

fn ty_unary(op: UnaryOp, inner: IRType) -> Result<IRType> {
    use IRScalerType::*;
    use IRType::*;
    use UnaryOp::*;

    match (op, inner) {
        // number → number   (all unary ops on scalars allowed)
        (_, Scaler(Number)) => Ok(Scaler(Number)),

        // point → point  (only Neg is defined in code‑gen)
        (Neg, Scaler(Point)) => Ok(Scaler(Point)),

        // everything else is invalid
        _ => bail!("unary op {op:?} is not defined for {inner:?}"),
    }
}

fn ty_ident(env: &Expressions, params: &[IRType], name: &str) -> anyhow::Result<IRType> {
    match env
        .get_expr(name)
        .ok_or_else(|| anyhow!("unknown ident {name}"))?
    {
        Expr::VarDef { rhs, .. } => rhs.ty(env, params),
        Expr::FnDef { .. } => bail!("{name} is a function, not a value"),
        _ => bail!("{name} is not an ident, this indicates a bug"),
    }
}

fn ty_call(
    env: &Expressions,
    params: &[IRType],
    ident: &str,
    args: &[Node],
) -> anyhow::Result<IRType> {
    let Expr::FnDef {
        rhs,
        args: fn_params,
        ..
    } = env
        .get_expr(ident)
        .ok_or_else(|| anyhow!("unknown function {ident}"))?
    else {
        bail!("{ident} is not a function")
    };

    println!("{args:?}, {fn_params:?}");

    anyhow::ensure!(
        args.len() == fn_params.len(),
        "parity mismatch calling {ident}"
    );

    // compute argument types
    let arg_tys: Vec<_> = args
        .iter()
        .map(|n| n.ty(env, params))
        .collect::<anyhow::Result<_>>()?;

    // Use the function body to get its return type,            <-- recursion!
    rhs.ty(env, &arg_tys)
}
