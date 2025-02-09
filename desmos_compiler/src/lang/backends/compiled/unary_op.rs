use anyhow::{bail, Result};

use crate::lang::expr::UnaryOp;

use super::{
    backend::{self, BackendValue, CodeBuilder},
    codegen::CodeGen,
    generic_value::GenericValue,
};

impl<'a, Backend: backend::Backend> CodeGen<'a, Backend> {
    pub(crate) fn codegen_unary_op<'ctx>(
        &self,
        builder: &Backend::Builder<'ctx>,
        lhs: BackendValue<'ctx, Backend>,
        op: UnaryOp,
    ) -> Result<BackendValue<'ctx, Backend>> {
        Ok(match lhs {
            GenericValue::Number(lhs) => {
                GenericValue::Number(self.codegen_unary_number_op(builder, lhs, op)?)
            }
            GenericValue::Point(_) => bail!("Unary ops are not implemented for Points"),
            GenericValue::List(_) => bail!("Unary operations are not defined for lists"),
        })
    }

    pub(crate) fn codegen_unary_number_op<'ctx>(
        &self,
        builder: &Backend::Builder<'ctx>,
        lhs: <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue,
        op: UnaryOp,
    ) -> Result<<Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue> {
        Ok(match op {
            UnaryOp::Neg => builder.neg(lhs),
            UnaryOp::Sqrt => builder.sqrt(lhs),
            UnaryOp::Sin => builder.sin(lhs),
            UnaryOp::Cos => builder.cos(lhs),
            UnaryOp::Tan => builder.tan(lhs),
            _ => bail!("Unary operation `{op:?}` is not implemented"),
        })
    }
}
