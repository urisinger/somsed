use anyhow::{bail, Context, Result};
use inkwell::values::BasicMetadataValueEnum;

use crate::lang::{
    backends::llvm::{
        types::{ListType, ValueType},
        value::{List, Value},
    },
    parser::{BinaryOp, Expr, Literal, Node, UnaryOp},
};

use super::CodeGen;

impl<'ctx, 'expr> CodeGen<'ctx, 'expr> {
    pub fn return_type(&self, expr: &Node, call_types: &[ValueType]) -> Result<ValueType> {
        Ok(match expr {
            Node::Lit(Literal::Float(_)) => ValueType::Number,
            Node::Lit(Literal::List(_)) => ValueType::List(ListType::Number),
            Node::Ident(ident) => {
                // Handling identifiers is more complex and often involves looking up values in a symbol table.
                // Here we assume you have some mechanism to resolve identifiers to LLVM values.
                //
                match self
                    .exprs
                    .get_expr(ident)
                    .context(format!("failed to get expr for ident, {}", ident))?
                {
                    Expr::VarDef { rhs, .. } => self.return_type(rhs, call_types)?,
                    expr => bail!("expr has wrong type, this should not happend, {expr:?}"),
                }
            }
            Node::BinOp { .. } => ValueType::Number,
            Node::UnaryOp { val, op } => {
                let val_value = self.return_type(val, call_types)?;

                match op {
                    UnaryOp::Neg => val_value,
                    _ => unimplemented!(),
                }
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
            _ => unimplemented!(),
        })
    }

    pub fn codegen_expr(&mut self, expr: &Node, call_args: &[Value<'ctx>]) -> Result<Value<'ctx>> {
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
                    let value = self.codegen_expr(element, call_args)?;

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

            Node::Ident(ident) => {
                // Handling identifiers is more complex and often involves looking up values in a symbol table.
                // Here we assume you have some mechanism to resolve identifiers to LLVM values.
                self.get_var(ident)?
            }
            Node::BinOp { lhs, op, rhs } => {
                let lhs = self.codegen_expr(lhs, call_args)?;
                let rhs = self.codegen_expr(rhs, call_args)?;
                self.codegen_binary_op(lhs, *op, rhs)?
            }
            Node::UnaryOp { val, op } => {
                let val_value = self.codegen_expr(val, call_args)?;

                match op {
                    UnaryOp::Neg => match val_value {
                        Value::Number(v) => self.builder.build_float_neg(v, "neg")?.into(),
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                }
            }
            Node::FnCall { ident, args } => match self.exprs.get_expr(ident) {
                Some(Expr::FnDef { .. }) => {
                    let (types, args) = args
                        .iter()
                        .map(|arg| {
                            let value = self.codegen_expr(arg, call_args)?;
                            Ok((
                                value.get_type(),
                                BasicMetadataValueEnum::from(value.as_basic_value_enum()),
                            ))
                        })
                        .collect::<Result<(Vec<_>, Vec<_>)>>()?;
                    let (function, ret_type) = self.get_fn(ident, &types)?;

                    Value::from_basic_value_enum(
                        self.builder
                            .build_call(function, &args, ident)?
                            .try_as_basic_value()
                            .expect_left("return type should not be void"),
                        ret_type,
                    )
                    .expect("ret type does not match expected ret type")
                }
                Some(Expr::VarDef { .. }) => {
                    if args.len() == 1 {
                        let lhs = self.get_var(ident)?;
                        let rhs = self.codegen_expr(&args[0], call_args)?;
                        self.codegen_binary_op(lhs, BinaryOp::Mul, rhs)?
                    } else {
                        bail!("{ident} is not a function")
                    }
                }
                None => bail!("unknown ident {ident}"),
                _ => unreachable!("idents should be VarDef or FnDef only"),
            },
            Node::FnArg { index } => call_args[*index],
            _ => unimplemented!(),
        })
    }
}
