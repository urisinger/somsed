use anyhow::{anyhow, Result};
use inkwell::{
    intrinsics::Intrinsic,
    values::{AnyValue, FloatValue},
};

use crate::lang::{backends::llvm::value::Value, parser::BinaryOp};

use super::CodeGen;

impl<'ctx, 'expr> CodeGen<'ctx, 'expr> {
    pub fn codegen_binary_op(
        &self,
        lhs: Value<'ctx>,
        op: BinaryOp,
        rhs: Value<'ctx>,
    ) -> Result<Value<'ctx>> {
        match (lhs, rhs) {
            (Value::List(lhs), Value::Number(rhs)) => {
                self.codegen_list_loop(lhs, |lhs| self.codegen_binary_number_op(lhs, op, rhs))
            }
            (Value::Number(lhs), Value::Number(rhs)) => {
                Ok(Value::Number(self.codegen_binary_number_op(lhs, op, rhs)?))
            }
            (_, _) => Err(anyhow!(
                "typeerror, expected (List, Number) or (Number, Number)"
            )),
        }
    }

    pub fn codegen_binary_number_op(
        &self,
        lhs: FloatValue<'ctx>,
        op: BinaryOp,
        rhs: FloatValue<'ctx>,
    ) -> Result<FloatValue<'ctx>> {
        Ok(match op {
            BinaryOp::Add => self.builder.build_float_add(lhs, rhs, "add")?,
            BinaryOp::Sub => self.builder.build_float_sub(lhs, rhs, "sub")?,
            BinaryOp::Mul => self.builder.build_float_mul(lhs, rhs, "mul")?,
            BinaryOp::Div => self.builder.build_float_div(lhs, rhs, "div")?,
            BinaryOp::Pow => {
                let intrinsic = Intrinsic::find("llvm.pow").unwrap();

                self.builder
                    .build_call(
                        intrinsic
                            .get_declaration(
                                &self.module,
                                &[
                                    self.context.f64_type().into(),
                                    self.context.f64_type().into(),
                                ],
                            )
                            .unwrap(),
                        &[lhs.into(), rhs.into()],
                        "pow",
                    )?
                    .as_any_value_enum()
                    .try_into()
                    .expect("pow intrinsic did not return f64")
            }
        })
    }
}
