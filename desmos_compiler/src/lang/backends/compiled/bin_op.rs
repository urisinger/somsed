use anyhow::{anyhow, Result};

use crate::lang::expr::BinaryOp;

use super::{
    backend::{self, BackendValue, CodeBuilder},
    codegen::CodeGen,
    generic_value::GenericValue,
};

impl<'a, Backend: backend::Backend> CodeGen<'a, Backend> {
    pub fn codegen_binary_op<'ctx>(
        &self,
        builder: &Backend::Builder<'ctx>,
        lhs: BackendValue<'ctx, Backend>,
        op: BinaryOp,
        rhs: BackendValue<'ctx, Backend>,
    ) -> Result<BackendValue<'ctx, Backend>> {
        match (lhs, rhs) {
            (GenericValue::Number(lhs_val), GenericValue::Number(rhs_val)) => {
                let result = self.codegen_binary_number_op(builder, lhs_val, op, rhs_val)?;
                Ok(GenericValue::Number(result))
            }
            (_, _) => Err(anyhow!(
            "typeerror, expected (List<Number>, Number), (List<Point>, Number) or (Number, Number)"
        )),
        }
    }

    pub fn codegen_binary_number_op<'ctx>(
        &self,
        builder: &Backend::Builder<'ctx>,
        lhs: <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue,
        op: BinaryOp,
        rhs: <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue,
    ) -> Result<<Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue> {
        Ok(match op {
            BinaryOp::Add => builder.add(lhs, rhs),
            BinaryOp::Sub => builder.sub(lhs, rhs),
            BinaryOp::Mul => builder.mul(lhs, rhs),
            BinaryOp::Paran => builder.mul(lhs, rhs),
            BinaryOp::Div => builder.div(lhs, rhs),
            BinaryOp::Pow => builder.pow(lhs, rhs),
        })
    }
}
