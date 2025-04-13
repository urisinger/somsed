pub mod backend;
mod bin_op;
mod unary_op;

use anyhow::{anyhow, bail, Context, Result};

use crate::{
    expressions::Expressions,
    lang::{
        expr::{BinaryOp, Expr, Literal, Node},
        generic_value::{GenericList, GenericValue, ValueType},
    },
};

use backend::CodeBuilder;

pub struct CodeGen<'a> {
    exprs: &'a Expressions,
}

impl<'a> CodeGen<'a> {
    pub fn new(exprs: &'a Expressions) -> Self {
        Self { exprs }
    }

    pub fn codegen_expr<'ctx, Builder: CodeBuilder>(
        &mut self,
        builder: &mut Builder,
        expr: &Node,
    ) -> Result<
        GenericValue<
            Builder::NumberValue,
            Builder::PointValue,
            Builder::NumberListValue,
            Builder::PointListValue,
        >,
    > {
        Ok(match expr {
            Node::Lit(Literal::Float(value)) => GenericValue::Number(builder.const_number(*value)),
            Node::Lit(Literal::List(elements)) => {
                match &elements
                    .iter()
                    .map(|element| self.codegen_expr(builder, element))
                    .try_fold(None, |acc: Option<GenericList<Vec<_>, Vec<_>>>, current| {
                        let current = current?;
                        Ok(match (acc, current) {
                            (None, GenericValue::Number(val)) => {
                                Some(GenericList::Number(vec![val.clone()]))
                            }
                            (None, GenericValue::Point(val)) => {
                                Some(GenericList::PointList(vec![val.clone()]))
                            }
                            (None, GenericValue::List(_)) => {
                                bail!("List elements must not be Lists")
                            }

                            (Some(GenericList::Number(mut list)), GenericValue::Number(val)) => {
                                list.push(val.clone());
                                Some(GenericList::Number(list))
                            }
                            (Some(GenericList::PointList(mut list)), GenericValue::Point(val)) => {
                                list.push(val.clone());
                                Some(GenericList::PointList(list))
                            }

                            (Some(GenericList::Number(_)), GenericValue::Point(_))
                            | (Some(GenericList::PointList(_)), GenericValue::Number(_))
                            | (_, GenericValue::List(_)) => {
                                bail!("List elements must be of the same type")
                            }
                        })
                    })? {
                    Some(GenericList::Number(elements)) => {
                        GenericValue::List(GenericList::Number(builder.number_list(elements)?))
                    }
                    Some(GenericList::PointList(elements)) => {
                        GenericValue::List(GenericList::PointList(builder.point_list(elements)?))
                    }
                    None => GenericValue::List(GenericList::Number(builder.number_list(&[])?)),
                }
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

    pub fn get_var<'ctx, Builder: CodeBuilder>(
        &mut self,
        builder: &mut Builder,
        ident: &str,
    ) -> Result<
        GenericValue<
            Builder::NumberValue,
            Builder::PointValue,
            Builder::NumberListValue,
            Builder::PointListValue,
        >,
    > {
        match self
            .exprs
            .get_expr(ident)
            .context(anyhow!("Cannot find expr {ident}"))?
        {
            Expr::VarDef { rhs, .. } => self.codegen_expr(builder, rhs),
            _ => bail!("Expr is not of type VarDef"),
        }
    }

    pub fn codegen_fn_call<'ctx, Builder: CodeBuilder>(
        &mut self,
        builder: &mut Builder,
        name: &str,
        args: &[GenericValue<
            Builder::NumberValue,
            Builder::PointValue,
            Builder::NumberListValue,
            Builder::PointListValue,
        >],
    ) -> Result<
        GenericValue<
            Builder::NumberValue,
            Builder::PointValue,
            Builder::NumberListValue,
            Builder::PointListValue,
        >,
    > {
        let types: Vec<ValueType> = args.iter().map(|arg| arg.get_type()).collect();

        let len = types.iter().map(|t| t.name().len() + 1).sum::<usize>() + name.len();
        let mut specialized_name = String::with_capacity(len);

        specialized_name.push_str(name);

        for t in &types {
            specialized_name.push('_');
            specialized_name.push_str(t.name());
        }

        builder
            .call_fn(&specialized_name, args, self)
            .ok_or_else(|| anyhow!("function does not exist"))
    }

    pub fn compile_fn<'ctx, Builder: CodeBuilder>(
        &mut self,
        mut builder: Builder,
        node: &Node,
    ) -> Result<()> {
        let value = self.codegen_expr(&mut builder, node)?;

        builder.build_return(value);
        Ok(())
    }
}
