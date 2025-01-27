use anyhow::{bail, Result};
use inkwell::{
    intrinsics::Intrinsic,
    values::{AnyValue, FloatValue},
};

use crate::lang::{backends::llvm::value::CompilerValue, parser::UnaryOp};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx, '_> {
    pub(crate) fn codegen_unary_op(
        &self,
        lhs: CompilerValue<'ctx>,
        op: UnaryOp,
    ) -> Result<CompilerValue<'ctx>> {
        Ok(match lhs {
            CompilerValue::Number(lhs) => {
                CompilerValue::Number(self.codegen_unary_number_op(lhs, op)?)
            }
            CompilerValue::Point(_) => bail!("Unary ops are not implemented for Points"),
            CompilerValue::List(_) => bail!("Unary operations are not defined for lists"),
        })
    }

    pub(crate) fn codegen_unary_number_op(
        &self,
        lhs: FloatValue<'ctx>,
        op: UnaryOp,
    ) -> Result<FloatValue<'ctx>> {
        Ok(match op {
            UnaryOp::Neg => self.builder.build_float_neg(lhs, "neg")?,
            UnaryOp::Sqrt => {
                let intrinsic = Intrinsic::find("llvm.sqrt").unwrap();
                self.builder
                    .build_call(
                        intrinsic
                            .get_declaration(&self.module, &[self.context.f64_type().into()])
                            .unwrap(),
                        &[lhs.into()],
                        "sqrt",
                    )?
                    .as_any_value_enum()
                    .try_into()
                    .expect("sqrt intrinsic did not return f64")
            }
            UnaryOp::Sin => {
                let intrinsic = Intrinsic::find("llvm.sin").unwrap();
                self.builder
                    .build_call(
                        intrinsic
                            .get_declaration(&self.module, &[self.context.f64_type().into()])
                            .unwrap(),
                        &[lhs.into()],
                        "sin",
                    )?
                    .as_any_value_enum()
                    .try_into()
                    .expect("sin intrinsic did not return f64")
            }
            UnaryOp::Cos => {
                let intrinsic = Intrinsic::find("llvm.cos").unwrap();
                self.builder
                    .build_call(
                        intrinsic
                            .get_declaration(&self.module, &[self.context.f64_type().into()])
                            .unwrap(),
                        &[lhs.into()],
                        "cos",
                    )?
                    .as_any_value_enum()
                    .try_into()
                    .expect("cos intrinsic did not return f64")
            }
            UnaryOp::Tan => {
                let sin_value = self.codegen_unary_number_op(lhs, UnaryOp::Sin)?;
                let cos_value = self.codegen_unary_number_op(lhs, UnaryOp::Cos)?;

                self.builder
                    .build_float_div(sin_value, cos_value, "tan")
                    .expect("tan division failed")
            }
            _ => bail!("Unary operation `{op:?}` is not implemented"),
        })
    }
}
