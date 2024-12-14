use inkwell::values::{
    AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FloatValue, StructValue,
};

use super::types::{ListType, ValueType};

#[derive(Debug, Clone, Copy)]
pub enum List<'ctx> {
    Number(StructValue<'ctx>),
}

impl<'ctx> List<'ctx> {
    pub fn as_basic_value_enum(self) -> BasicValueEnum<'ctx> {
        match self {
            Self::Number(value) => value.as_basic_value_enum(),
        }
    }

    pub fn as_any_value_enum(self) -> AnyValueEnum<'ctx> {
        match self {
            Self::Number(value) => value.as_any_value_enum(),
        }
    }

    pub fn from_basic_value_enum(value: BasicValueEnum<'ctx>, ty: ListType) -> Option<Self> {
        match ty {
            ListType::Number => Some(List::Number(value.try_into().ok()?)),
        }
    }

    pub fn get_type(&self) -> ListType {
        match self {
            Self::Number(_) => ListType::Number,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value<'ctx> {
    Number(FloatValue<'ctx>),
    List(List<'ctx>),
}

impl<'ctx> Value<'ctx> {
    pub fn as_basic_value_enum(self) -> BasicValueEnum<'ctx> {
        match self {
            Value::Number(number) => number.as_basic_value_enum(),

            Value::List(arr) => arr.as_basic_value_enum(),
        }
    }

    pub fn as_any_value_enum(self) -> AnyValueEnum<'ctx> {
        match self {
            Value::Number(number) => number.as_any_value_enum(),
            Value::List(arr) => arr.as_any_value_enum(),
        }
    }

    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Number(_) => ValueType::Number,

            Value::List(arr) => ValueType::List(arr.get_type()),
        }
    }

    pub fn from_basic_value_enum(value: BasicValueEnum<'ctx>, ty: ValueType) -> Option<Self> {
        match ty {
            ValueType::Number => match value {
                BasicValueEnum::FloatValue(float_value) => Some(Value::Number(float_value)),
                _ => None,
            },
            ValueType::List(arr_type) => {
                List::from_basic_value_enum(value, arr_type).map(Value::List)
            }
        }
    }
}

impl<'ctx> From<FloatValue<'ctx>> for Value<'ctx> {
    fn from(value: FloatValue<'ctx>) -> Self {
        Self::Number(value)
    }
}
