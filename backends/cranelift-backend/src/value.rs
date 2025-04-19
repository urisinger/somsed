use cranelift::prelude::*;
use desmos_compiler::lang::codegen::ir::{IRScalerType, IRType};

#[derive(Debug, Clone, Copy)]
pub enum CraneliftValue {
    Scaler(CraneliftScaler),
    List(CraneliftList),
}

impl CraneliftValue {
    pub fn number(value: Value) -> Self {
        Self::Scaler(CraneliftScaler::Number([value]))
    }

    pub fn point(value: [Value; 2]) -> Self {
        Self::Scaler(CraneliftScaler::Point(value))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CraneliftScaler {
    Number([Value; 1]),
    Point([Value; 2]),
}

#[derive(Debug, Clone, Copy)]
pub enum CraneliftList {
    Number([Value; 2]),
    Point([Value; 2]),
}

impl CraneliftValue {
    /// Construct from flat Cranelift values and expected IRType
    pub fn from_values(values: &[Value], ty: IRType) -> Option<Self> {
        Some(match ty {
            IRType::Scaler(IRScalerType::Number) => {
                CraneliftValue::Scaler(CraneliftScaler::Number(*as_array(values)?))
            }
            IRType::Scaler(IRScalerType::Point) => {
                CraneliftValue::Scaler(CraneliftScaler::Point(*as_array(values)?))
            }
            IRType::List(IRScalerType::Number) => {
                CraneliftValue::List(CraneliftList::Number(*as_array(values)?))
            }
            IRType::List(IRScalerType::Point) => {
                CraneliftValue::List(CraneliftList::Point(*as_array(values)?))
            }
        })
    }

    /// Get flat Cranelift values for passing to other instructions
    pub fn as_struct(&self) -> &[Value] {
        match self {
            CraneliftValue::Scaler(CraneliftScaler::Number(v)) => v,
            CraneliftValue::Scaler(CraneliftScaler::Point(v)) => v,
            CraneliftValue::List(CraneliftList::Number(v)) => v,
            CraneliftValue::List(CraneliftList::Point(v)) => v,
        }
    }

    pub fn ty(&self) -> IRType {
        match self {
            CraneliftValue::Scaler(CraneliftScaler::Number(_)) => {
                IRType::Scaler(IRScalerType::Number)
            }
            CraneliftValue::Scaler(CraneliftScaler::Point(_)) => {
                IRType::Scaler(IRScalerType::Point)
            }
            CraneliftValue::List(CraneliftList::Number(_)) => IRType::List(IRScalerType::Number),
            CraneliftValue::List(CraneliftList::Point(_)) => IRType::List(IRScalerType::Point),
        }
    }
}

/// Number of Cranelift `Value`s required to represent an `IRType`
pub fn value_count(ty: IRType) -> usize {
    match ty {
        IRType::Scaler(IRScalerType::Number) => 1,
        IRType::Scaler(IRScalerType::Point) => 2,
        IRType::List(_) => 2,
    }
}

const fn as_array<T, const N: usize>(arr: &[T]) -> Option<&[T; N]> {
    if arr.len() == N {
        let ptr = arr.as_ptr() as *const [T; N];
        Some(unsafe { &*ptr })
    } else {
        None
    }
}
