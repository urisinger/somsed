use anyhow::{bail, Result};

use crate::lang::{expr::UnaryOp, generic_value::GenericValue};

use super::{
    backend::{self, BackendValue, CodeBuilder},
    CodeGen,
};

impl<Backend: backend::Backend> CodeGen<'_, Backend> {
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
            GenericValue::Point(lhs) => match op {
                UnaryOp::Neg => {
                    let x = builder.neg(builder.get_x(lhs.clone()));
                    let y = builder.neg(builder.get_y(lhs));
                    GenericValue::Point(builder.point(x, y))
                }
                UnaryOp::GetX => GenericValue::Number(builder.get_x(lhs)),
                UnaryOp::GetY => GenericValue::Number(builder.get_y(lhs)),
                _ => bail!("Unary op {:?} cant be applied to point", op),
            },
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
            _ => bail!("Unary operation `{op:?}` is not implemented for number"),
        })
    }
}
