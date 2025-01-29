use anyhow::{anyhow, Result};
use inkwell::{
    intrinsics::Intrinsic,
    values::{AnyValue, BasicValue, FloatValue},
};

use crate::lang::{
    backends::llvm::value::{CompilerList, CompilerValue},
    parser::BinaryOp,
};

use super::CodeGen;

impl<'ctx> CodeGen<'ctx, '_> {
    pub fn codegen_binary_op(
        &self,
        lhs: CompilerValue<'ctx>,
        op: BinaryOp,
        rhs: CompilerValue<'ctx>,
    ) -> Result<CompilerValue<'ctx>> {
        match (lhs, rhs) {
            (
                CompilerValue::List(CompilerList::Number(lhs_struct)),
                CompilerValue::Number(rhs_val),
            ) => {
                // We treat each element as f64
                let element_type = self.context.f64_type().into();

                // Use the generic map-in-place to transform each element
                let updated_struct =
                    self.codegen_list_map(lhs_struct, element_type, |loaded_enum| {
                        // Convert the loaded `BasicValueEnum` to `FloatValue`
                        let old_value = loaded_enum.into_float_value();

                        // Use your existing codegen_binary_number_op to combine old_value + rhs_val
                        let new_value = self.codegen_binary_number_op(old_value, op, rhs_val)?;

                        // Convert back to BasicValueEnum
                        Ok(new_value.as_basic_value_enum())
                    })?;

                // Return the updated list as List(Number)
                Ok(CompilerValue::List(CompilerList::Number(updated_struct)))
            }

            (CompilerValue::Number(lhs_val), CompilerValue::Number(rhs_val)) => {
                let result = self.codegen_binary_number_op(lhs_val, op, rhs_val)?;
                Ok(CompilerValue::Number(result))
            }

            // ───────────────────────────────────────────────────────────────
            // (4) Fallback Error
            // ───────────────────────────────────────────────────────────────
            (_, _) => Err(anyhow!(
            "typeerror, expected (List<Number>, Number), (List<Point>, Number) or (Number, Number)"
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
