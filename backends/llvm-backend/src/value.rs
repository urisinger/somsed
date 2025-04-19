use inkwell::{
    llvm_sys::prelude::LLVMTypeRef,
    types::{AnyType, AnyTypeEnum, AsTypeRef, BasicType, BasicTypeEnum, FloatType, StructType},
    values::{
        AnyValue, AnyValueEnum, AsValueRef, BasicValue, BasicValueEnum, FloatValue, StructValue,
    },
};

use desmos_compiler::lang::generic_value::{GenericList, GenericValue, ValueType};

#[derive(Debug, Clone)]
pub struct LLVMValue<'ctx>(
    pub GenericValue<FloatValue<'ctx>, StructValue<'ctx>, StructValue<'ctx>, StructValue<'ctx>>,
);

impl<'ctx> LLVMValue<'ctx> {
    pub fn from_basic_value_enum(value: BasicValueEnum<'ctx>, ty: &ValueType) -> Option<Self> {
        match ty {
            GenericValue::Number(_) => match value {
                BasicValueEnum::FloatValue(f) => Some(Self(GenericValue::Number(f))),
                _ => None,
            },
            GenericValue::Point(_) => match value {
                BasicValueEnum::StructValue(s) => Some(Self(GenericValue::Point(s))),
                _ => None,
            },
            GenericValue::List(GenericList::Number(_)) => match value {
                BasicValueEnum::StructValue(s) => {
                    Some(Self(GenericValue::List(GenericList::Number(s))))
                }
                _ => None,
            },
            GenericValue::List(GenericList::Point(_)) => match value {
                BasicValueEnum::StructValue(s) => {
                    Some(Self(GenericValue::List(GenericList::Point(s))))
                }
                _ => None,
            },
        }
    }
}

unsafe impl AsValueRef for LLVMValue<'_> {
    fn as_value_ref(&self) -> LLVMValueRef {
        match &self.0 {
            GenericValue::Number(n) => n.as_value_ref(),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::Number(s))
            | GenericValue::List(GenericList::Point(s)) => s.as_value_ref(),
        }
    }
}

unsafe impl<'ctx> AnyValue<'ctx> for LLVMValue<'ctx> {
    fn as_any_value_enum(&self) -> AnyValueEnum<'ctx> {
        match &self.0 {
            GenericValue::Number(n) => AnyValueEnum::FloatValue(*n),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::Number(s))
            | GenericValue::List(GenericList::Point(s)) => AnyValueEnum::StructValue(*s),
        }
    }
}

unsafe impl<'ctx> BasicValue<'ctx> for LLVMValue<'ctx> {
    fn as_basic_value_enum(&self) -> BasicValueEnum<'ctx> {
        match &self.0 {
            GenericValue::Number(n) => BasicValueEnum::FloatValue(*n),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::Number(s))
            | GenericValue::List(GenericList::Point(s)) => BasicValueEnum::StructValue(*s),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LLVMType<'ctx>(
    pub GenericValue<FloatType<'ctx>, StructType<'ctx>, StructType<'ctx>, StructType<'ctx>>,
);

impl<'ctx> LLVMType<'ctx> {
    pub fn from_type(
        ty: &ValueType,
        number_type: FloatType<'ctx>,
        point_type: StructType<'ctx>,
        list_type: StructType<'ctx>,
    ) -> Self {
        let inner = match ty {
            GenericValue::Number(()) => GenericValue::Number(number_type),
            GenericValue::Point(()) => GenericValue::Point(point_type),
            GenericValue::List(GenericList::Number(())) => {
                GenericValue::List(GenericList::Number(list_type))
            }
            GenericValue::List(GenericList::Point(())) => {
                GenericValue::List(GenericList::Point(list_type))
            }
        };

        Self(inner)
    }
}

unsafe impl AsTypeRef for LLVMType<'_> {
    fn as_type_ref(&self) -> LLVMTypeRef {
        match &self.0 {
            GenericValue::Number(t) => t.as_type_ref(),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::Number(s))
            | GenericValue::List(GenericList::Point(s)) => s.as_type_ref(),
        }
    }
}

unsafe impl<'ctx> AnyType<'ctx> for LLVMType<'ctx> {
    fn as_any_type_enum(&self) -> AnyTypeEnum<'ctx> {
        match &self.0 {
            GenericValue::Number(t) => AnyTypeEnum::FloatType(*t),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::Number(s))
            | GenericValue::List(GenericList::Point(s)) => AnyTypeEnum::StructType(*s),
        }
    }
}

unsafe impl<'ctx> BasicType<'ctx> for LLVMType<'ctx> {
    fn as_basic_type_enum(&self) -> BasicTypeEnum<'ctx> {
        match &self.0 {
            GenericValue::Number(t) => BasicTypeEnum::FloatType(*t),
            GenericValue::Point(s)
            | GenericValue::List(GenericList::Number(s))
            | GenericValue::List(GenericList::Point(s)) => BasicTypeEnum::StructType(*s),
        }
    }
}
