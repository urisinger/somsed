use std::thread::current;

use anyhow::{bail, Result};

use crate::lang::{
    codegen::ir::{IRScalerType, IRType},
    expr::BinaryOp,
};

use super::{
    ir::{BlockID, IRSegment, InstID, Instruction},
    IRGen,
};

impl IRGen<'_> {
    pub(crate) fn codegen_binary_op(
        segment: &mut IRSegment,
        current_block: BlockID,
        lhs: InstID,
        op: BinaryOp,
        rhs: InstID,
    ) -> Result<InstID> {
        use IRScalerType::*;
        use IRType::*;

        match (lhs.ty(), rhs.ty()) {
            (Scaler(Number), Scaler(Number)) => {
                Self::codegen_binary_number_op(segment, current_block, lhs, op, rhs)
            }

            (Scaler(Point), Scaler(Point)) => {
                Self::codegen_binary_point_op(segment, current_block, lhs, op, rhs)
            }

            (Scaler(Number), Scaler(Point)) => {
                Self::codegen_number_point_op(segment, current_block, lhs, op, rhs)
            }

            (Scaler(Point), Scaler(Number)) => {
                Self::codegen_point_number_op(segment, current_block, lhs, op, rhs)
            }

            // Scalar-List or List-Scalar
            (List(_), Scaler(_)) | (Scaler(_), List(_)) => {
                Self::codegen_distributed_op(segment, current_block, lhs, op, rhs)
            }

            _ => bail!(
                "type error: unsupported combination {:?} and {:?}",
                lhs.ty(),
                rhs.ty()
            ),
        }
    }

    fn codegen_distributed_op(
        segment: &mut IRSegment,
        current_block: BlockID,
        lhs: InstID,
        op: BinaryOp,
        rhs: InstID,
    ) -> Result<InstID> {
        use IRType::*;
        let inner_block = segment.create_block();

        Ok(match (lhs.ty(), rhs.ty()) {
            (List(lhs_ty), Scaler(rhs_ty)) => {
                let lhs_inst = segment.push(
                    inner_block,
                    Instruction::BlockArg { index: 0 },
                    Scaler(lhs_ty),
                );
                let rhs_inst = segment.push(
                    inner_block,
                    Instruction::BlockArg { index: 1 },
                    Scaler(rhs_ty),
                );
                let ty =
                    Self::codegen_binary_op(segment, inner_block, lhs_inst, op, rhs_inst)?.ty();

                let ty = match ty {
                    Scaler(scaler) => List(scaler),
                    List(_) => bail!("Block incorrectly returns a list, this indicates a bug"),
                };
                segment.push(
                    current_block,
                    Instruction::Map {
                        lists: vec![lhs],
                        args: vec![rhs],
                        block_id: inner_block,
                    },
                    ty,
                )
            }
            (Scaler(_), Scaler(_)) => todo!(),
            (Scaler(lhs_ty), List(rhs_ty)) => {
                let lhs_inst = segment.push(
                    inner_block,
                    Instruction::BlockArg { index: 1 },
                    Scaler(lhs_ty),
                );
                let rhs_inst = segment.push(
                    inner_block,
                    Instruction::BlockArg { index: 0 },
                    Scaler(rhs_ty),
                );
                let ty =
                    Self::codegen_binary_op(segment, inner_block, lhs_inst, op, rhs_inst)?.ty();
                let ty = match ty {
                    Scaler(scaler) => List(scaler),
                    List(_) => bail!("Block incorrectly returns a list, this indicates a bug"),
                };
                segment.push(
                    current_block,
                    Instruction::Map {
                        lists: vec![rhs],
                        args: vec![lhs],
                        block_id: inner_block,
                    },
                    ty,
                )
            }
            (List(_), List(_)) => todo!(),
        })
    }

    fn codegen_binary_number_op(
        segment: &mut IRSegment,
        current_block: BlockID,
        lhs: InstID,
        op: BinaryOp,
        rhs: InstID,
    ) -> Result<InstID> {
        Ok(segment.push(
            current_block,
            Instruction::BinaryOp { lhs, op, rhs },
            IRType::NUMBER,
        ))
    }

    fn codegen_binary_point_op(
        segment: &mut IRSegment,
        current_block: BlockID,
        lhs: InstID,
        op: BinaryOp,
        rhs: InstID,
    ) -> Result<InstID> {
        use BinaryOp::*;
        match op {
            Add | Sub => {
                let mut extract = |inst: InstID, index: usize| {
                    segment.push(
                        current_block,
                        Instruction::Extract(inst, index),
                        IRType::NUMBER,
                    )
                };

                let (lx, ly) = (extract(lhs, 0), extract(lhs, 1));
                let (rx, ry) = (extract(rhs, 0), extract(rhs, 1));

                let op_x = Self::codegen_binary_number_op(segment, current_block, lx, op, rx)?;
                let op_y = Self::codegen_binary_number_op(segment, current_block, ly, op, ry)?;

                Ok(segment.push(current_block, Instruction::Point(op_x, op_y), IRType::POINT))
            }
            _ => bail!("BinaryOp {:?} is not supported for Point <-> Point", op),
        }
    }

    fn codegen_number_point_op(
        segment: &mut IRSegment,
        current_block: BlockID,
        number: InstID,
        op: BinaryOp,
        point: InstID,
    ) -> Result<InstID> {
        use BinaryOp::*;
        match op {
            Dot | Paran => {
                let mut extract = |inst: InstID, index: usize| {
                    segment.push(
                        current_block,
                        Instruction::Extract(inst, index),
                        IRType::NUMBER,
                    )
                };

                let px = extract(point, 0);
                let py = extract(point, 1);

                let op_x = Self::codegen_binary_number_op(segment, current_block, number, op, px)?;
                let op_y = Self::codegen_binary_number_op(segment, current_block, number, op, py)?;

                Ok(segment.push(current_block, Instruction::Point(op_x, op_y), IRType::POINT))
            }
            _ => bail!("BinaryOp {:?} is not supported for Number <-> Point", op),
        }
    }

    fn codegen_point_number_op(
        segment: &mut IRSegment,
        current_block: BlockID,
        point: InstID,
        op: BinaryOp,
        number: InstID,
    ) -> Result<InstID> {
        use BinaryOp::*;
        match op {
            Dot | Paran | Div => {
                let mut extract = |inst: InstID, index: usize| {
                    segment.push(
                        current_block,
                        Instruction::Extract(inst, index),
                        IRType::NUMBER,
                    )
                };

                let px = extract(point, 0);
                let py = extract(point, 1);

                let op_x = Self::codegen_binary_number_op(segment, current_block, px, op, number)?;
                let op_y = Self::codegen_binary_number_op(segment, current_block, py, op, number)?;

                Ok(segment.push(current_block, Instruction::Point(op_x, op_y), IRType::POINT))
            }
            _ => bail!("BinaryOp {:?} is not supported for Point <-> Number", op),
        }
    }
}
