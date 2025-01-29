use super::backends::llvm::jit::{JitListValue, JitValue, ListLayout, PointLayout};
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
    PointList(Vec<PointLayout>),
}

impl TryFrom<&JitValue> for Value {
    type Error = Error;

    fn try_from(jit_value: &JitValue) -> Result<Self, Self::Error> {
        match jit_value {
            JitValue::Number(n) => Ok(Value::Number(*n)),
            JitValue::Point(PointLayout { x, y }) => Ok(Value::Point { x: *x, y: *y }),
            JitValue::List(JitListValue::Number(list_layout)) => {
                convert_list::<f64>(list_layout).map(|l| Value::List(ListValue::NumberList(l)))
            }
            JitValue::List(JitListValue::Point(list_layout)) => {
                convert_list::<PointLayout>(list_layout)
                    .map(|l| Value::List(ListValue::PointList(l)))
            }
        }
    }
}

/// Generic function to convert `ListLayout` into a `Vec<T>`.
/// It ensures that the pointer is valid before performing unsafe operations.
fn convert_list<T: Clone>(list_layout: &ListLayout) -> Result<Vec<T>, Error> {
    unsafe {
        if list_layout.ptr.is_null() {
            bail!("List pointer is null");
        }

        // Compute the number of elements in the list
        let element_count = list_layout.size as usize;

        // Convert the raw pointer into a slice
        let slice = std::slice::from_raw_parts(list_layout.ptr as *const T, element_count);

        // Clone the slice into a Vec to create a safe wrapper
        Ok(slice.to_vec())
    }
}
