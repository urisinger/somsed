use inkwell::{
    llvm_sys::prelude::LLVMTypeRef,
    types::{AnyType, AnyTypeEnum, AsTypeRef, BasicType, BasicTypeEnum, FloatType, StructType},
    values::{
        AnyValue, AnyValueEnum, AsValueRef, BasicValue, BasicValueEnum, FloatValue, StructValue,
    },
};

use crate::lang::generic_value::{GenericList, GenericValue, ValueType};

pub type LLVMValue<'ctx> =
    GenericValue<FloatValue<'ctx>, StructValue<'ctx>, StructValue<'ctx>, StructValue<'ctx>>;

impl<'ctx> LLVMValue<'ctx> {
    pub fn from_basic_value_enum(value: BasicValueEnum<'ctx>, ty: &ValueType) -> Option<Self> {
        match ty {
            GenericValue::Number(_) => match value {
                BasicValueEnum::FloatValue(float_value) => Some(Self::Number(float_value)),
                _ => None,
            },
            GenericValue::Point(_) => match value {
                BasicValueEnum::StructValue(p) => Some(Self::Point(p)),
                _ => None,
            },
            GenericValue::List(arr_type) => match arr_type {
                GenericList::NumberList(_) => match value {
                    BasicValueEnum::StructValue(l) => Some(Self::List(GenericList::NumberList(l))),
                    _ => None,
                },

                GenericList::PointList(_) => match value {
                    BasicValueEnum::StructValue(l) => Some(Self::List(GenericList::PointList(l))),
                    _ => None,
                },
            },
        }
    }
}

unsafe impl<'ctx> AsValueRef for LLVMValue<'ctx> {
    fn as_value_ref(&self) -> inkwell::llvm_sys::prelude::LLVMValueRef {
        match self {
            GenericValue::Number(t) => t.as_value_ref(),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::NumberList(s))
            | GenericValue::List(GenericList::PointList(s)) => s.as_value_ref(),
        }
    }
}

unsafe impl<'ctx> AnyValue<'ctx> for LLVMValue<'ctx> {
    fn as_any_value_enum(&self) -> AnyValueEnum<'ctx> {
        match self {
            GenericValue::Number(t) => AnyValueEnum::FloatValue(*t),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::NumberList(s))
            | GenericValue::List(GenericList::PointList(s)) => AnyValueEnum::StructValue(*s),
        }
    }
}

unsafe impl<'ctx> BasicValue<'ctx> for LLVMValue<'ctx> {
    fn as_basic_value_enum(&self) -> BasicValueEnum<'ctx> {
        match self {
            GenericValue::Number(t) => BasicValueEnum::FloatValue(*t),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::NumberList(s))
            | GenericValue::List(GenericList::PointList(s)) => BasicValueEnum::StructValue(*s),
        }
    }
}

pub type LLVMType<'ctx> =
    GenericValue<FloatType<'ctx>, StructType<'ctx>, StructType<'ctx>, StructType<'ctx>>;

impl<'ctx> LLVMType<'ctx> {
    pub fn from_type(
        ty: &ValueType,
        number_type: FloatType<'ctx>,
        point_type: StructType<'ctx>,
        list_type: StructType<'ctx>,
    ) -> Self {
        match ty {
            GenericValue::Number(()) => GenericValue::Number(number_type),
            GenericValue::Point(()) => GenericValue::Point(point_type),
            GenericValue::List(GenericList::NumberList(())) => {
                GenericValue::List(GenericList::NumberList(list_type))
            }
            GenericValue::List(GenericList::PointList(())) => {
                GenericValue::List(GenericList::PointList(list_type))
            }
        }
    }
}

unsafe impl<'ctx> AsTypeRef for LLVMType<'ctx> {
    fn as_type_ref(&self) -> LLVMTypeRef {
        match self {
            GenericValue::Number(t) => t.as_type_ref(),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::NumberList(s))
            | GenericValue::List(GenericList::PointList(s)) => s.as_type_ref(),
        }
    }
}

unsafe impl<'ctx> AnyType<'ctx> for LLVMType<'ctx> {
    fn as_any_type_enum(&self) -> AnyTypeEnum<'ctx> {
        match self {
            GenericValue::Number(t) => AnyTypeEnum::FloatType(*t),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::NumberList(s))
            | GenericValue::List(GenericList::PointList(s)) => AnyTypeEnum::StructType(*s),
        }
    }
}

unsafe impl<'ctx> BasicType<'ctx> for LLVMType<'ctx> {
    fn as_basic_type_enum(&self) -> BasicTypeEnum<'ctx> {
        match self {
            GenericValue::Number(t) => BasicTypeEnum::FloatType(*t),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::NumberList(s))
            | GenericValue::List(GenericList::PointList(s)) => BasicTypeEnum::StructType(*s),
        }
    }
}
