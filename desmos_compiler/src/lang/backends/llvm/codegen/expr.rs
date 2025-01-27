use std::ops::Deref;

use anyhow::{bail, Context, Result};
use inkwell::values::BasicMetadataValueEnum;

use crate::lang::{
    backends::llvm::{
        types::{ListType, ValueType},
        value::{List, Value},
    },
    parser::{BinaryOp, Expr, Literal, Node},
};

use super::{CodeGen, FnContext};

impl<'ctx> CodeGen<'ctx, '_> {
    pub fn return_type(
        &self,
        expr: &Node,
        call_types: &[ValueType<'ctx>],
    ) -> Result<ValueType<'ctx>> {
        Ok(match expr {
            Node::Lit(Literal::Float(_)) => ValueType::Number(self.float_type),
            Node::Lit(Literal::List(_)) => ValueType::List(ListType::Number(self.list_type)),
            Node::Lit(Literal::Point(..)) => ValueType::Point(self.point_type),
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
            Node::BinOp { .. } => ValueType::Number(self.float_type),
            Node::UnaryOp { val, .. } => {
                let val_value = self.return_type(val, call_types)?;

                val_value
            }
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

            Node::Comp { .. } => unreachable!("Comp node should not appear here"),
        })
    }

    pub fn codegen_expr(
        &mut self,
        expr: &Node,
        fn_context: &FnContext<'ctx, '_>,
    ) -> Result<Value<'ctx>> {
        Ok(match expr {
            Node::Lit(Literal::Float(value)) => {
                let float_type = self.context.f64_type();
                float_type.const_float(*value).into()
            }
            Node::Lit(Literal::List(elements)) => {
                let int_type = self.context.i64_type();
                let float_type = self.context.f64_type();

                let size = int_type.const_int(elements.len() as u64, false);
                let malloc_fn = self
                    .module
                    .get_function("malloc")
                    .expect("malloc should exist");

                let total_size =
                    self.builder
                        .build_int_mul(size, int_type.const_int(8, false), "total_size")?;
                let pointer = self
                    .builder
                    .build_call(malloc_fn, &[total_size.into()], "malloc_call")?
                    .try_as_basic_value()
                    .left()
                    .expect("malloc should return a pointer")
                    .into_pointer_value();

                for (i, element) in elements.iter().enumerate() {
                    let value = self.codegen_expr(element, fn_context)?;

                    let float_value = match value {
                        Value::Number(v) => v,
                        _ => bail!("List elements must be numbers"),
                    };

                    let element_ptr = unsafe {
                        self.builder.build_in_bounds_gep(
                            float_type,
                            pointer,
                            &[int_type.const_int(i as u64, false)],
                            &format!("element_ptr_{}", i),
                        )
                    };
                    self.builder.build_store(element_ptr?, float_value)?;
                }

                let mut list_value = self.list_type.get_undef();
                list_value = self
                    .builder
                    .build_insert_value(list_value, size, 0, "list_size")?
                    .into_struct_value();
                list_value = self
                    .builder
                    .build_insert_value(list_value, pointer, 1, "list_ptr")?
                    .into_struct_value();

                Value::List(List::Number(list_value))
            }
            Node::Lit(Literal::Point(x, y)) => {
                let x = self.codegen_expr(x, fn_context)?;
                let y = self.codegen_expr(y, fn_context)?;

                let (x, y) = match (x, y) {
                    (Value::Number(x), Value::Number(y)) => (x, y),
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
                Value::Point(point_value)
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
                Some(Expr::FnDef { args: fn_args, .. }) => {
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
            Node::Comp { .. } => unreachable!("comp should not apear here"),
        })
    }
}
