use inkwell::values::{
    AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FloatValue, StructValue,
};

use super::types::{CompilerListType, CompilerType};

#[derive(Debug, Clone, Copy)]
pub enum CompilerList<'ctx> {
    Number(StructValue<'ctx>),
    Point(StructValue<'ctx>),
}

impl<'ctx> CompilerList<'ctx> {
    pub fn as_basic_value_enum(self) -> BasicValueEnum<'ctx> {
        match self {
            Self::Number(value) => value.as_basic_value_enum(),
            Self::Point(value) => value.as_basic_value_enum(),
        }
    }

    pub fn as_any_value_enum(self) -> AnyValueEnum<'ctx> {
        match self {
            Self::Number(value) => value.as_any_value_enum(),
            Self::Point(value) => value.as_any_value_enum(),
        }
    }

    pub fn from_basic_value_enum(
        value: BasicValueEnum<'ctx>,
        ty: CompilerListType,
    ) -> Option<Self> {
        match ty {
            CompilerListType::Number(_) => Some(CompilerList::Number(value.try_into().ok()?)),
            CompilerListType::Point(_) => Some(CompilerList::Point(value.try_into().ok()?)),
        }
    }

    pub fn get_type(&self) -> CompilerListType<'ctx> {
        match self {
            Self::Number(list) => CompilerListType::Number(list.get_type()),
            Self::Point(list) => CompilerListType::Point(list.get_type()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CompilerValue<'ctx> {
    Number(FloatValue<'ctx>),
    Point(StructValue<'ctx>),
    List(CompilerList<'ctx>),
}

impl<'ctx> CompilerValue<'ctx> {
    pub fn as_basic_value_enum(self) -> BasicValueEnum<'ctx> {
        match self {
            CompilerValue::Number(number) => number.as_basic_value_enum(),
            CompilerValue::Point(p) => p.as_basic_value_enum(),
            CompilerValue::List(arr) => arr.as_basic_value_enum(),
        }
    }

    pub fn as_any_value_enum(self) -> AnyValueEnum<'ctx> {
        match self {
            CompilerValue::Number(number) => number.as_any_value_enum(),
            CompilerValue::Point(p) => p.as_any_value_enum(),
            CompilerValue::List(arr) => arr.as_any_value_enum(),
        }
    }

    pub fn get_type(&self) -> CompilerType<'ctx> {
        match self {
            CompilerValue::Number(num) => CompilerType::Number(num.get_type()),
            CompilerValue::Point(p) => CompilerType::Point(p.get_type()),
            CompilerValue::List(arr) => CompilerType::List(arr.get_type()),
        }
    }

    pub fn from_basic_value_enum(value: BasicValueEnum<'ctx>, ty: CompilerType) -> Option<Self> {
        match ty {
            CompilerType::Number(_) => match value {
                BasicValueEnum::FloatValue(float_value) => Some(CompilerValue::Number(float_value)),
                _ => None,
            },
            CompilerType::Point(_) => match value {
                BasicValueEnum::StructValue(p) => Some(CompilerValue::Point(p)),
                _ => None,
            },
            CompilerType::List(arr_type) => {
                CompilerList::from_basic_value_enum(value, arr_type).map(CompilerValue::List)
            }
        }
    }
}

impl<'ctx> From<FloatValue<'ctx>> for CompilerValue<'ctx> {
    fn from(value: FloatValue<'ctx>) -> Self {
        Self::Number(value)
    }
}
