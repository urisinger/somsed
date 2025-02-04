use anyhow::{bail, Context, Result};

use crate::lang::{
    backends::llvm::{
        types::{CompilerListType, CompilerType},
        value::CompilerValue,
    },
    parser::{BinaryOp, Expr, Literal, Node},
};

use super::{CodeGen, FnContext};

impl<'ctx> CodeGen<'ctx, '_> {
    pub fn return_type(
        &self,
        expr: &Node,
        call_types: &[CompilerType<'ctx>],
    ) -> Result<CompilerType<'ctx>> {
        Ok(match expr {
            Node::Lit(Literal::Float(_)) => CompilerType::Number(self.float_type),
            Node::Lit(Literal::List(elements)) => CompilerType::List(
                match elements
                    .iter()
                    .map(|expr| self.return_type(expr, call_types).map(Some))
                    .reduce(|acc, res| match (acc, res) {
                        (Ok(Some(a)), Ok(Some(b))) if a == b => Ok(Some(a)),

                        (Ok(_), Ok(_)) => Ok(None),

                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                    })
                    .transpose()
                    .map(|opt_opt| opt_opt.flatten())?
                {
                    Some(CompilerType::Number(_)) => CompilerListType::Number(self.list_type),
                    Some(CompilerType::Point(_)) => CompilerListType::Point(self.list_type),
                    Some(CompilerType::List(_)) => bail!("Lists in lists are not allowed"),
                    None => {
                        if elements.len() == 0 {
                            CompilerListType::Number(self.list_type)
                        } else {
                            bail!("Lists can only contain one type")
                        }
                    }
                },
            ),
            Node::Lit(Literal::Point(..)) => CompilerType::Point(self.point_type),
            Node::Ident(ident) => {
                match self
                    .exprs
                    .get_expr(ident)
                    .context(format!("failed to get expr for ident, {}", ident))?
                {
                    Expr::VarDef { rhs, .. } => self.return_type(rhs, call_types)?,
                    expr => bail!("expr has wrong type, this should not happend, {expr:?}"),
                }
            }
            Node::BinOp { .. } => CompilerType::Number(self.float_type),
            Node::UnaryOp { val, .. } => self.return_type(val, call_types)?,
            Node::FnCall { ident, args } => match self.exprs.get_expr(ident) {
                Some(Expr::FnDef { rhs, .. }) => {
                    let call_types = args
                        .iter()
                        .map(|arg| {
                            let value = self.return_type(arg, call_types)?;
                            Ok(value)
                        })
                        .collect::<Result<Vec<_>>>()?;

                    self.return_type(rhs, &call_types)?
                }
                Some(Expr::VarDef { rhs, .. }) => self.return_type(rhs, call_types)?,
                None => bail!("unknown ident {ident}"),
                _ => bail!("expr has the wrong type"),
            },
            Node::FnArg { index } => call_types[*index],
        })
    }

    pub fn codegen_expr(
        &mut self,
        expr: &Node,
        fn_context: &FnContext<'ctx, '_>,
    ) -> Result<CompilerValue<'ctx>> {
        Ok(match expr {
            Node::Lit(Literal::Float(value)) => {
                let float_type = self.context.f64_type();
                float_type.const_float(*value).into()
            }
            Node::Lit(Literal::List(elements)) => {
                let elements = elements
                    .iter()
                    .map(|node| self.codegen_expr(node, fn_context))
                    .collect::<Result<Vec<_>>>()?;
                self.codegen_list_new(elements)?
            }
            Node::Lit(Literal::Point(x, y)) => {
                let x = self.codegen_expr(x, fn_context)?;
                let y = self.codegen_expr(y, fn_context)?;

                let (x, y) = match (x, y) {
                    (CompilerValue::Number(x), CompilerValue::Number(y)) => (x, y),
                    _ => bail!("Point must have number types only"),
                };
                let mut point_value = self.point_type.get_undef();
                point_value = self
                    .builder
                    .build_insert_value(point_value, x, 0, "x")?
                    .into_struct_value();
                point_value = self
                    .builder
                    .build_insert_value(point_value, y, 1, "y")?
                    .into_struct_value();
                CompilerValue::Point(point_value)
            }
            Node::Ident(ident) => self.get_var(ident, fn_context)?,
            Node::BinOp { lhs, op, rhs } => {
                let lhs = self.codegen_expr(lhs, fn_context)?;
                let rhs = self.codegen_expr(rhs, fn_context)?;
                self.codegen_binary_op(lhs, *op, rhs)?
            }
            Node::UnaryOp { val, op } => {
                let lhs = self.codegen_expr(val, fn_context)?;
                self.codegen_unary_op(lhs, *op)?
            }
            Node::FnCall { ident, args } => match self.exprs.get_expr(ident) {
                Some(Expr::FnDef { .. }) => {
                    let args = args
                        .iter()
                        .map(|arg| {
                            let value = self.codegen_expr(arg, fn_context)?;
                            Ok(value)
                        })
                        .collect::<Result<Vec<_>>>()?;

                    self.codegen_fn_call(ident, &args)?
                }
                Some(Expr::VarDef { .. }) => {
                    if args.len() == 1 {
                        let lhs = self.get_var(ident, fn_context)?;
                        let rhs = self.codegen_expr(&args[0], fn_context)?;
                        self.codegen_binary_op(lhs, BinaryOp::Mul, rhs)?
                    } else {
                        bail!("{ident} is not a function")
                    }
                }
                None => bail!("unknown ident {ident}"),
                _ => unreachable!("idents should be VarDef or FnDef only"),
            },
            Node::FnArg { index } => fn_context.args[*index],
        })
    }
}
