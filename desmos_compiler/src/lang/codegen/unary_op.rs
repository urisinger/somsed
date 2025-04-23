use crate::lang::codegen::IRSegment;
use anyhow::{bail, Result};

use crate::lang::expr::UnaryOp;

use super::{
    ir::{BlockID, IRScalerType, IRType, InstID, Instruction},
    IRGen,
};

impl IRGen<'_> {
    pub(crate) fn codegen_unary_op(
        segment: &mut IRSegment,
        _: &[IRType],
        current_block: BlockID,
        lhs: InstID,
        op: UnaryOp,
    ) -> Result<InstID> {
        Ok(match lhs.ty() {
            IRType::Scaler(IRScalerType::Number) => {
                Self::codegen_unary_number_op(segment, current_block, lhs, op)?
            }
            IRType::Scaler(IRScalerType::Point) => match op {
                UnaryOp::Neg => {
                    let x =
                        segment.push(current_block, Instruction::Extract(lhs, 0), IRType::NUMBER);
                    let x = segment.push(current_block, Instruction::Neg(x), IRType::NUMBER);
                    let y =
                        segment.push(current_block, Instruction::Extract(lhs, 1), IRType::NUMBER);
                    let y = segment.push(current_block, Instruction::Neg(y), IRType::NUMBER);

                    segment.push(current_block, Instruction::Point(x, y), IRType::POINT)
                }

                _ => bail!("Unary op {:?} cant be applied to point", op),
            },
            IRType::List(_) => bail!("Unary operations are not defined for lists"),
        })
    }

    pub(crate) fn codegen_unary_number_op(
        segment: &mut IRSegment,
        current_block: BlockID,
        lhs: InstID,
        op: UnaryOp,
    ) -> Result<InstID> {
        Ok(segment.push(
            current_block,
            match op {
                UnaryOp::Neg => Instruction::Neg,
                UnaryOp::Sqrt => Instruction::Sqrt,
                UnaryOp::Sin => Instruction::Sin,
                UnaryOp::Cos => Instruction::Cos,
                UnaryOp::Tan => Instruction::Tan,
                _ => todo!(),
            }(lhs),
            IRType::NUMBER,
        ))
    }
}
