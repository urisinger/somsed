use crate::lang::generic_value::{GenericList, GenericValue, ValueType};
use cranelift::prelude::*;

pub type CraneliftValue = GenericValue<[Value; 1], [Value; 2], [Value; 2], [Value; 2]>;

impl CraneliftValue {
    pub fn from_values(values: &[Value], ty: &ValueType) -> Option<Self> {
        Some(match ty {
            GenericValue::Number(_) => CraneliftValue::Number(*as_array(values)?),
            GenericValue::Point(_) => CraneliftValue::Point(*as_array(values)?),
            GenericValue::List(GenericList::Number(_)) => {
                CraneliftValue::List(GenericList::Number(*as_array(values)?))
            }
            GenericValue::List(GenericList::PointList(_)) => {
                CraneliftValue::List(GenericList::PointList(*as_array(values)?))
            }
        })
    }
    pub fn as_struct(&self) -> &[Value] {
        match self {
            CraneliftValue::Number(v) => v,
            CraneliftValue::Point(p) => p,
            CraneliftValue::List(GenericList::Number(l))
            | CraneliftValue::List(GenericList::PointList(l)) => l,
        }
    }
}

pub fn value_count(ty: ValueType) -> usize {
    match ty {
        GenericValue::Number(_) => 1,
        GenericValue::Point(_) => 2,
        GenericValue::List(_) => 2,
    }
}

const fn as_array<T, const N: usize>(arr: &[T]) -> Option<&[T; N]> {
    if arr.len() == N {
        let ptr = arr.as_ptr() as *const [T; N];

        // SAFETY: The underlying array of a slice can be reinterpreted as an actual array `[T; N]` if `N` is not greater than the slice's length.
        let me = unsafe { &*ptr };
        Some(me)
    } else {
        None
    }
}
