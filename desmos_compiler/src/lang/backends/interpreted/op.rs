use crate::lang::{
    parser::{BinaryOp, UnaryOp},
    value::{ListValue, Value},
};

use super::EvalError;

pub fn eval_binop(left: Value, op: BinaryOp, right: Value) -> Result<Value, EvalError> {
    match (left, op, right) {
        (Value::Number(l), BinaryOp::Add, Value::Number(r)) => Ok(Value::Number(l + r)),
        (Value::Number(l), BinaryOp::Sub, Value::Number(r)) => Ok(Value::Number(l - r)),
        (Value::Number(l), BinaryOp::Mul, Value::Number(r)) => Ok(Value::Number(l * r)),
        (Value::Number(l), BinaryOp::Div, Value::Number(r)) => Ok(Value::Number(l / r)),

        (Value::List(ListValue::NumberList(lhs_list)), op, Value::Number(r)) => {
            let mapped = lhs_list
                .into_iter()
                .map(|elem| match op {
                    BinaryOp::Add => Ok(elem + r),
                    BinaryOp::Sub => Ok(elem - r),
                    BinaryOp::Mul => Ok(elem * r),
                    BinaryOp::Div => Ok(elem / r),
                    _ => Err(EvalError::UnexpectedType),
                })
                .collect::<Result<Vec<f64>, EvalError>>()?;
            Ok(Value::List(ListValue::NumberList(mapped)))
        }

        (Value::Number(l), op, Value::List(ListValue::NumberList(rhs_list))) => {
            let mapped = rhs_list
                .into_iter()
                .map(|elem| match op {
                    BinaryOp::Add => Ok(l + elem),
                    BinaryOp::Sub => Ok(l - elem),
                    BinaryOp::Mul => Ok(l * elem),
                    BinaryOp::Div => Ok(l / elem),
                    _ => Err(EvalError::UnexpectedType),
                })
                .collect::<Result<Vec<f64>, EvalError>>()?;
            Ok(Value::List(ListValue::NumberList(mapped)))
        }

        _ => Err(EvalError::UnexpectedType),
    }
}

pub fn eval_unary_op(value: Value, op: UnaryOp) -> Result<Value, EvalError> {
    match (op, value) {
        (UnaryOp::Neg, Value::Number(n)) => Ok(Value::Number(-n)),
        _ => Err(EvalError::UnexpectedType),
    }
}
