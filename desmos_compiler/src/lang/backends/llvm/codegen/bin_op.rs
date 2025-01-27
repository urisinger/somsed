use anyhow::{anyhow, Result};
use inkwell::{
    intrinsics::Intrinsic,
    values::{AnyValue, FloatValue},
};

use crate::lang::{backends::llvm::value::CompilerValue, parser::BinaryOp};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx, '_> {
    pub fn codegen_binary_op(
        &self,
        lhs: CompilerValue<'ctx>,
        op: BinaryOp,
        rhs: CompilerValue<'ctx>,
    ) -> Result<CompilerValue<'ctx>> {
        match (lhs, rhs) {
            (CompilerValue::List(lhs), CompilerValue::Number(rhs)) => {
                self.codegen_list_loop(lhs, |lhs| self.codegen_binary_number_op(lhs, op, rhs))
            }
            (CompilerValue::Number(lhs), CompilerValue::Number(rhs)) => Ok(CompilerValue::Number(
                self.codegen_binary_number_op(lhs, op, rhs)?,
            )),
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
