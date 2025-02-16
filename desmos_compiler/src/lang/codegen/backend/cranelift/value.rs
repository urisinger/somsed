use crate::lang::generic_value::{GenericList, GenericValue};
use cranelift::prelude::*;

pub type CraneliftValue = GenericValue<[Value; 1], [Value; 2], [Value; 2], [Value; 2]>;

impl CraneliftValue {
    pub fn as_struct(&self) -> &[Value] {
        match self {
            CraneliftValue::Number(v) => v,
            CraneliftValue::Point(p) => p,
            CraneliftValue::List(GenericList::Number(l))
            | CraneliftValue::List(GenericList::PointList(l)) => l,
        }
    }
}
