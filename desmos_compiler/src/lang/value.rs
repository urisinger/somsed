use super::backends::llvm::jit::{JitListValue, JitValue, PointLayout};
use anyhow::{bail, Error};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Point { x: f64, y: f64 },
    List(ListValue),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ListValue {
    NumberList(Vec<f64>),
}

impl TryFrom<JitValue> for Value {
    type Error = Error;

    fn try_from(jit_value: JitValue) -> Result<Self, Self::Error> {
        match jit_value {
            JitValue::Number(n) => Ok(Value::Number(n)),
            JitValue::Point(PointLayout { x, y }) => Ok(Value::Point { x, y }),
            JitValue::List(JitListValue::Number(list_layout)) => {
                // Convert the `ListLayout` into a slice of f64
                unsafe {
                    if list_layout.ptr.is_null() {
                        bail!("List pointer is null");
                    }

                    // Compute the number of elements in the list
                    let element_count = list_layout.size as usize / std::mem::size_of::<f64>();

                    // Convert the raw pointer into a slice
                    let slice =
                        std::slice::from_raw_parts(list_layout.ptr as *const f64, element_count);

                    // Clone the slice into a Vec to create a safe wrapper
                    let safe_vec = slice.to_vec();

                    // Wrap the Vec<f64> in a `ListValue`
                    Ok(Value::List(ListValue::NumberList(safe_vec)))
                }
            }
        }
    }
}
