use inkwell::{
    builder::Builder,
    intrinsics::Intrinsic,
    module::Module,
    types::{BasicTypeEnum, FloatType, StructType},
    values::{
        AnyValue, BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, StructValue,
    },
};

use crate::lang::backends::compiled::{
    backend::CodeBuilder,
    generic_value::{GenericValue, ValueType},
};

use super::value::LLVMValue;

pub struct LLVMBuilder<'module, 'ctx> {
    pub(crate) module: &'module Module<'ctx>,
    pub(crate) builder: Builder<'ctx>,

    pub(crate) function: FunctionValue<'ctx>,
    pub(crate) args: Vec<LLVMValue<'ctx>>,
    pub(crate) number_type: FloatType<'ctx>,
    pub(crate) point_type: StructType<'ctx>,
    pub(crate) list_type: StructType<'ctx>,
}

impl<'module, 'ctx> LLVMBuilder<'module, 'ctx> {
    fn build_intrinsic(
        &self,
        intrinsic_name: &str,
        args: &[BasicMetadataValueEnum<'ctx>],
        label: &str,
    ) -> FloatValue<'ctx> {
        let intrinsic = Intrinsic::find(intrinsic_name)
            .expect(&format!("Intrinsic {} must exist", intrinsic_name));

        let arg_types: Vec<BasicTypeEnum<'ctx>> = vec![self.number_type.into(); args.len()];

        let declaration = intrinsic
            .get_declaration(&self.module, &arg_types)
            .expect("Failed to get intrinsic declaration");

        self.builder
            .build_call(declaration, args, label)
            .expect("Intrinsic call failed")
            .as_any_value_enum()
            .try_into()
            .expect(&format!("Intrinsic {} did not return f64", intrinsic_name))
    }
}

impl<'module, 'ctx> CodeBuilder<FunctionValue<'ctx>> for LLVMBuilder<'module, 'ctx> {
    type NumberValue = FloatValue<'ctx>;
    type PointValue = StructValue<'ctx>;

    type NumberListValue = StructValue<'ctx>;
    type PointListValue = StructValue<'ctx>;

    fn build_return(self, value: LLVMValue) -> FunctionValue<'ctx> {
        self.builder
            .build_return(Some(&value))
            .expect("invalid builder state");
        self.function
    }

    fn get_arg(
        &self,
        index: usize,
    ) -> Option<
        &GenericValue<
            Self::NumberValue,
            Self::PointValue,
            Self::NumberListValue,
            Self::PointListValue,
        >,
    > {
        self.args.get(index)
    }

    fn call_fn(
        &self,
        function: FunctionValue<'ctx>,
        values: &[GenericValue<
            Self::NumberValue,
            Self::PointValue,
            Self::NumberListValue,
            Self::PointListValue,
        >],
        ret: &ValueType,
    ) -> GenericValue<
        Self::NumberValue,
        Self::PointValue,
        Self::NumberListValue,
        Self::PointListValue,
    > {
        let args: Vec<_> = values
            .iter()
            .map(|value| value.as_basic_value_enum().into())
            .collect();

        let call = self
            .builder
            .build_call(function, &args, "call_fn")
            .expect("invalid builder state");

        LLVMValue::from_basic_value_enum(
            call.try_as_basic_value()
                .expect_left("no function can return void"),
            ret,
        )
        .expect("function must return a valid type")
    }

    fn const_number(&self, number: f64) -> Self::NumberValue {
        self.number_type.const_float(number)
    }

    fn point(&self, x: Self::NumberValue, y: Self::NumberValue) -> Self::PointValue {
        let mut point_value = self.point_type.get_undef();
        point_value = self
            .builder
            .build_insert_value(point_value, x, 0, "x")
            .expect("index is less then amount of fields")
            .into_struct_value();
        point_value = self
            .builder
            .build_insert_value(point_value, y, 1, "y")
            .expect("index is less then amount of fields")
            .into_struct_value();

        point_value
    }

    fn add(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        self.builder
            .build_float_add(lhs, rhs, "add_float")
            .expect("invalid builder state")
    }

    fn sub(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        self.builder
            .build_float_sub(lhs, rhs, "sub_float")
            .expect("invalid builder state")
    }

    fn mul(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        self.builder
            .build_float_mul(lhs, rhs, "mul_float")
            .expect("invalid builder state")
    }

    fn div(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        self.builder
            .build_float_div(lhs, rhs, "div_float")
            .expect("invalid builder state")
    }

    fn pow(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        self.build_intrinsic("llvm.pow", &[lhs.into(), rhs.into()], "pow")
    }

    fn neg(&self, lhs: Self::NumberValue) -> Self::NumberValue {
        self.builder
            .build_float_neg(lhs, "neg")
            .expect("invalid builder state")
    }

    fn sqrt(&self, lhs: Self::NumberValue) -> Self::NumberValue {
        self.build_intrinsic("llvm.sqrt", &[lhs.into()], "sqrt")
    }

    fn sin(&self, lhs: Self::NumberValue) -> Self::NumberValue {
        self.build_intrinsic("llvm.sin", &[lhs.into()], "sin")
    }

    fn cos(&self, lhs: Self::NumberValue) -> Self::NumberValue {
        self.build_intrinsic("llvm.cos", &[lhs.into()], "cos")
    }

    fn tan(&self, lhs: Self::NumberValue) -> Self::NumberValue {
        self.build_intrinsic("llvm.tan", &[lhs.into()], "tan")
    }
}
