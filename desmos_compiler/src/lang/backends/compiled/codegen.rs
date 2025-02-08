use std::collections::HashMap;

use anyhow::{anyhow, bail, Context, Result};

use crate::{
    expressions::Expressions,
    lang::parser::{BinaryOp, Expr, Literal, Node},
};

use super::{
    backend::{self, BackendValue, CodeBuilder},
    generic_value::{GenericValue, ValueType},
};

pub struct CodeGen<'a, Backend: backend::Backend> {
    backend: &'a Backend,
    exprs: &'a Expressions,
    pub return_types: HashMap<String, ValueType>,
}

impl<'a, Backend: backend::Backend> CodeGen<'a, Backend> {
    pub fn new(backend: &'a Backend, exprs: &'a Expressions) -> Self {
        Self {
            backend,
            exprs,
            return_types: HashMap::new(),
        }
    }

    pub fn codegen_expr<'ctx>(
        &mut self,
        builder: &Backend::Builder<'ctx>,
        expr: &Node,
    ) -> Result<BackendValue<'ctx, Backend>> {
        Ok(match expr {
            Node::Lit(Literal::Float(value)) => GenericValue::Number(builder.const_number(*value)),
            Node::Lit(Literal::List(_)) => {
                bail!("nrn")
            }
            Node::Lit(Literal::Point(x, y)) => {
                let x = self.codegen_expr(builder, x)?;
                let y = self.codegen_expr(builder, y)?;

                let (x, y) = match (x, y) {
                    (GenericValue::Number(x), GenericValue::Number(y)) => (x, y),
                    _ => bail!("Point must have number types only"),
                };
                GenericValue::Point(builder.point(x, y))
            }
            Node::Ident(ident) => self.get_var(builder, ident)?,
            Node::BinOp { lhs, op, rhs } => {
                let lhs = self.codegen_expr(builder, lhs)?;
                let rhs = self.codegen_expr(builder, rhs)?;
                self.codegen_binary_op(builder, lhs, *op, rhs)?
            }
            Node::UnaryOp { val, op } => {
                let lhs = self.codegen_expr(builder, val)?;
                self.codegen_unary_op(builder, lhs, *op)?
            }
            Node::FnCall { ident, args } => match self.exprs.get_expr(ident) {
                Some(Expr::FnDef { .. }) => {
                    let args = args
                        .iter()
                        .map(|arg| {
                            let value = self.codegen_expr(builder, arg)?;
                            Ok(value)
                        })
                        .collect::<Result<Vec<_>>>()?;

                    self.codegen_fn_call(builder, ident, &args)?
                }
                Some(Expr::VarDef { .. }) => {
                    if args.len() == 1 {
                        let lhs = self.get_var(builder, ident)?;
                        let rhs = self.codegen_expr(builder, &args[0])?;
                        self.codegen_binary_op(builder, lhs, BinaryOp::Paran, rhs)?
                    } else {
                        bail!("{ident} is not a function")
                    }
                }
                None => bail!("unknown ident {ident}"),
                _ => unreachable!("idents should be VarDef or FnDef only"),
            },
            Node::FnArg { index } => builder.get_arg(*index).unwrap().clone(),
        })
    }

    pub fn get_var<'ctx>(
        &mut self,
        builder: &Backend::Builder<'ctx>,
        ident: &str,
    ) -> Result<BackendValue<'ctx, Backend>> {
        match self
            .exprs
            .get_expr(ident)
            .context(anyhow!("Cannot find expr {ident}"))?
        {
            Expr::VarDef { rhs, .. } => self.codegen_expr(builder, rhs),
            _ => bail!("Expr is not of type VarDef"),
        }
    }

    pub fn codegen_fn_call<'ctx>(
        &mut self,
        builder: &Backend::Builder<'ctx>,
        name: &str,
        args: &[BackendValue<'ctx, Backend>],
    ) -> Result<BackendValue<'ctx, Backend>> {
        let types: Vec<ValueType> = args.iter().map(|arg| arg.get_type()).collect();

        let len = types.iter().map(|t| t.name().len() + 1).sum::<usize>() + name.len();
        let mut specialized_name = String::with_capacity(len);

        for t in &types {
            specialized_name.push('_');
            specialized_name.push_str(t.name());
        }

        if let Some(function) = self.backend.get_fn(&specialized_name) {
            let return_type = self
                .return_types
                .get(&specialized_name)
                .expect("compile_fn should have inserted return type");

            Ok(builder.call_fn(function, args, return_type))
        } else {
            match self.exprs.get_expr(name) {
                Some(Expr::FnDef { rhs, .. }) => {
                    let function = self.compile_fn(specialized_name.clone(), rhs, &types)?;

                    let return_type = self
                        .return_types
                        .get(&specialized_name)
                        .expect("compile_fn should have inserted return type");

                    Ok(builder.call_fn(function, args, return_type))
                }
                None => bail!("no exprssion found for function {name}"),
                _ => unreachable!("this indicates a bug"),
            }
        }
    }

    pub fn compile_fn(
        &mut self,
        specialized_name: impl Into<String>,
        node: &Node,
        types: &[ValueType],
    ) -> Result<Backend::FnValue> {
        let specialized_name = specialized_name.into();
        let ret = node.return_type(self.exprs, types)?;
        let builder = self.backend.get_builder(&specialized_name, types, &ret);

        self.return_types.insert(specialized_name, ret);

        let value = self.codegen_expr(&builder, node)?;

        Ok(builder.build_return(value))
    }
}
