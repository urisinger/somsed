use anyhow::{bail, Result};

use crate::lang::{expr::UnaryOp, generic_value::GenericValue};

use super::{
    backend::{BuilderValue, CodeBuilder},
    CodeGen,
};

impl CodeGen<'_> {
    pub(crate) fn codegen_unary_op<Builder: CodeBuilder>(
        &self,
        builder: &mut Builder,
        lhs: BuilderValue<Builder>,
        op: UnaryOp,
    ) -> Result<BuilderValue<Builder>> {
        Ok(match lhs {
            GenericValue::Number(lhs) => {
                GenericValue::Number(self.codegen_unary_number_op(builder, lhs, op)?)
            }
            GenericValue::Point(lhs) => match op {
                UnaryOp::Neg => {
                    let x = builder.get_x(lhs.clone());
                    let x = builder.neg(x);
                    let y = builder.get_y(lhs);
                    let y = builder.neg(y);
                    GenericValue::Point(builder.point(x, y))
                }
                UnaryOp::GetX => GenericValue::Number(builder.get_x(lhs)),
                UnaryOp::GetY => GenericValue::Number(builder.get_y(lhs)),
                _ => bail!("Unary op {:?} cant be applied to point", op),
            },
            GenericValue::List(_) => bail!("Unary operations are not defined for lists"),
        })
    }

    pub(crate) fn codegen_unary_number_op<Builder: CodeBuilder>(
        &self,
        builder: &mut Builder,
        lhs: Builder::NumberValue,
        op: UnaryOp,
    ) -> Result<Builder::NumberValue> {
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
