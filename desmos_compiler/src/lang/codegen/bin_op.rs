use anyhow::{anyhow, bail, Result};

use crate::lang::{
    expr::BinaryOp,
    generic_value::{GenericList, GenericValue, ListType},
};

use super::{
    backend::{self, BackendValue, CodeBuilder},
    CodeGen,
};

enum ScalarPosition {
    Left,
    Right,
}

// All of this is a bit weird, but doing it the normal match way would be even more annoying
impl<Backend: backend::Backend> CodeGen<'_, Backend> {
    pub fn codegen_binary_op<'ctx>(
        &self,
        builder: &Backend::Builder<'ctx>,
        lhs: BackendValue<'ctx, Backend>,
        op: BinaryOp,
        rhs: BackendValue<'ctx, Backend>,
    ) -> Result<BackendValue<'ctx, Backend>> {
        match (lhs, rhs) {
            // (Number, Number)
            (lhs @ (GenericValue::Number(_) | GenericValue::Point(_)), rhs @ (GenericValue::Number(_) | GenericValue::Point(_))) => {
                let rhs_val = rhs.lift_scaler().expect("expected scaler");
                let lhs_val =lhs.lift_scaler().expect("expected scaler");

                match self.codegen_simple_binary_op(
                    builder,
                   lhs_val,
                    op,
                    rhs_val,
                )? {
                    GenericList::Number(n) => Ok(GenericValue::Number(n)),
                    GenericList::PointList(p) => Ok(GenericValue::Point(p)),
                }
            }

            // For all cases where one operand is a list and the other is a scalar,
            // we can reduce four cases to just two.
            // (List, Scalar)
            (GenericValue::List(list_val), scalar @ (GenericValue::Number(_) | GenericValue::Point(_))) =>
            {
                // Determine the output list type based on the kind of list.
                let output_ty = match &list_val {
                    GenericList::PointList(_) => match op {
                        BinaryOp::Dot | BinaryOp::Div | BinaryOp::Paran => ListType::PointList(()),
                        BinaryOp::Add | BinaryOp::Sub => ListType::PointList(()),
                        _ => bail!("Operation {op:?} is not defined for a Point list and scalar"),
                    },
                    GenericList::Number(_) => ListType::Number(()),
                };
                self.distribute_mixed_op(builder, list_val, scalar, ScalarPosition::Right, op, output_ty)
            }

            // (Scalar, List)
            (scalar @ (GenericValue::Number(_) | GenericValue::Point(_)), GenericValue::List(list_val)) =>
            {
                let output_ty = match &list_val {
                    GenericList::PointList(_) => match op {
                        BinaryOp::Dot | BinaryOp::Div | BinaryOp::Paran => ListType::PointList(()),
                        BinaryOp::Add | BinaryOp::Sub => ListType::PointList(()),
                        _ => bail!("Operation {op:?} is not defined for a scalar and Point list"),
                    },
                    GenericList::Number(_) => ListType::Number(()),
                };
                self.distribute_mixed_op(builder, list_val, scalar, ScalarPosition::Left, op, output_ty)
            }
            _ => Err(anyhow!(
                "type error, expected (List<...>, Scalar), (Scalar, List<...>), (Number, Number), (Point, Point) or (Number, Point)"
            )),
        }
    }

    fn distribute_mixed_op<'ctx>(
        &self,
        builder: &Backend::Builder<'ctx>,
        list: GenericList<
            <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberListValue,
            <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::PointListValue,
        >,
        scalar: BackendValue<'ctx, Backend>,
        pos: ScalarPosition,
        op: BinaryOp,
        output_ty: ListType,
    ) -> Result<BackendValue<'ctx, Backend>> {
        // Use the builder's map_list helper to iterate over list elements.
        let result_list = builder.map_list(list, output_ty, |elem| {
            // For each list element, lift the scalar into a list element
            // and call the simple binary op with arguments swapped if needed.
            let lifted = match &scalar {
                GenericValue::Number(n) => GenericList::Number(n.clone()),
                GenericValue::Point(p) => GenericList::PointList(p.clone()),
                _ => unreachable!("Scalar must be Number or Point"),
            };

            let result_elem = match pos {
                ScalarPosition::Left => {
                    // Scalar is on the left: op(lifted, elem)
                    self.codegen_simple_binary_op(builder, lifted, op, elem)
                }
                ScalarPosition::Right => {
                    // Scalar is on the right: op(elem, lifted)
                    self.codegen_simple_binary_op(builder, elem, op, lifted)
                }
            };
            result_elem.expect("Type should have been checked earlier")
        });
        Ok(GenericValue::List(result_list))
    }

    fn codegen_simple_binary_op<'ctx>(
        &self,
        builder: &Backend::Builder<'ctx>,
        lhs: GenericList<
            <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue,
            <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::PointValue,
        >,
        op: BinaryOp,
        rhs: GenericList<
            <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue,
            <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::PointValue,
        >,
    ) -> Result<
        GenericList<
            <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue,
            <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::PointValue,
        >,
    > {
        Ok(match (lhs, rhs) {
            (GenericList::Number(lhs), GenericList::Number(rhs)) => {
                GenericList::Number(self.codegen_binary_number_op(builder, lhs, op, rhs))
            }
            (GenericList::Number(lhs), GenericList::PointList(rhs)) => match op {
                BinaryOp::Dot | BinaryOp::Paran => {
                    let x = builder.mul(lhs.clone(), builder.get_x(rhs.clone()));
                    let y = builder.mul(lhs, builder.get_y(rhs));

                    GenericList::PointList(builder.point(x, y))
                }
                _ => bail!("Cant {op:?} number and point"),
            },
            (GenericList::PointList(lhs), GenericList::Number(rhs)) => match op {
                BinaryOp::Dot | BinaryOp::Paran => {
                    let x = builder.mul(builder.get_x(lhs.clone()), rhs.clone());
                    let y = builder.mul(builder.get_y(lhs), rhs);

                    GenericList::PointList(builder.point(x, y))
                }
                BinaryOp::Div => {
                    let x = builder.div(builder.get_x(lhs.clone()), rhs.clone());
                    let y = builder.div(builder.get_y(lhs), rhs);

                    GenericList::PointList(builder.point(x, y))
                }
                _ => bail!("Cant {op:?} point and Number"),
            },
            (GenericList::PointList(lhs), GenericList::PointList(rhs)) => match op {
                BinaryOp::Add => {
                    let x = builder.add(builder.get_x(lhs.clone()), builder.get_x(rhs.clone()));

                    let y = builder.add(builder.get_y(lhs), builder.get_y(rhs));

                    GenericList::PointList(builder.point(x, y))
                }
                BinaryOp::Sub => {
                    let x = builder.sub(builder.get_x(lhs.clone()), builder.get_x(rhs.clone()));

                    let y = builder.sub(builder.get_y(lhs), builder.get_y(rhs));

                    GenericList::PointList(builder.point(x, y))
                }
                _ => bail!("Cant {op:?} point and point"),
            },
        })
    }

    fn codegen_binary_number_op<'ctx>(
        &self,
        builder: &Backend::Builder<'ctx>,
        lhs: <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue,
        op: BinaryOp,
        rhs: <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue,
    ) -> <Backend::Builder<'ctx> as CodeBuilder<Backend::FnValue>>::NumberValue {
        match op {
            BinaryOp::Add => builder.add(lhs, rhs),
            BinaryOp::Sub => builder.sub(lhs, rhs),
            BinaryOp::Dot => builder.mul(lhs, rhs),
            BinaryOp::Paran => builder.mul(lhs, rhs),
            BinaryOp::Div => builder.div(lhs, rhs),
            BinaryOp::Pow => builder.pow(lhs, rhs),
        }
    }
}
