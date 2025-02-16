use cranelift::{
    codegen::ir::{Function, UserExternalName, UserExternalNameRef},
    prelude::*,
};
use cranelift_module::FuncId;

use crate::lang::{
    codegen::backend::CodeBuilder,
    generic_value::{GenericValue, ValueType},
};

use super::value::CraneliftValue;

struct BackendBuilder<'ctx> {
    pub builder: FunctionBuilder<'ctx>,
    pub function: Function,
    pub func_id: FuncId,

    // Vector to the start of each variable, the number of values is decided by the type
    args: Vec<CraneliftValue>,
}

impl<'ctx> CodeBuilder<FuncId> for BackendBuilder<'ctx> {
    type NumberValue = [Value; 1];
    type PointValue = [Value; 2];

    type NumberListValue = [Value; 2];
    type PointListValue = [Value; 2];

    fn build_return(mut self, value: CraneliftValue) -> FuncId {
        self.builder.ins().return_(value.as_struct());
        self.func_id
    }

    fn get_arg(&mut self, index: usize) -> Option<&CraneliftValue> {
        self.args.get(index)
    }

    fn call_fn(
        &mut self,
        function: FuncId,
        values: &[CraneliftValue],
        ret: &ValueType,
    ) -> CraneliftValue {
        self.function.import_function(ExtFuncData {
            name: ExternalName::user(UserExternalNameRef::from_u32(self.func_id.as_u32())),
        })
    }

    fn const_number(&mut self, number: f64) -> Self::NumberValue {
        todo!()
    }

    fn point(&mut self, x: Self::NumberValue, y: Self::NumberValue) -> Self::PointValue {
        todo!()
    }

    fn number_list(
        &mut self,
        elements: &[Self::NumberValue],
    ) -> anyhow::Result<Self::NumberListValue> {
        todo!()
    }

    fn point_list(
        &mut self,
        elements: &[Self::PointValue],
    ) -> anyhow::Result<Self::PointListValue> {
        todo!()
    }

    fn map_list(
        &mut self,
        list: crate::lang::generic_value::GenericList<Self::NumberListValue, Self::PointListValue>,
        output_ty: crate::lang::generic_value::ListType,
        f: impl Fn(
            crate::lang::generic_value::GenericList<Self::NumberValue, Self::PointValue>,
        )
            -> crate::lang::generic_value::GenericList<Self::NumberValue, Self::PointValue>,
    ) -> crate::lang::generic_value::GenericList<Self::NumberListValue, Self::PointListValue> {
        todo!()
    }

    fn get_x(&mut self, point: Self::PointValue) -> Self::NumberValue {
        todo!()
    }

    fn get_y(&mut self, point: Self::PointValue) -> Self::NumberValue {
        todo!()
    }

    fn add(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn sub(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn mul(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn div(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn pow(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn neg(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn sqrt(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn sin(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn cos(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }

    fn tan(&mut self, lhs: Self::NumberValue) -> Self::NumberValue {
        todo!()
    }
}
