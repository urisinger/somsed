pub mod compiled_exprs;
pub mod jit;
#[cfg(feature = "llvm")]
pub mod llvm;

#[cfg(feature = "cranelift")]
pub mod cranelift;

use jit::{ExplicitFn, ExplicitJitFn, ImplicitFn, ImplicitJitFn, JitValue, PointValue};

use crate::lang::generic_value::{GenericList, GenericValue, ListType, ValueType};

use super::CodeGen;

pub trait ExecutionEngine {
    type ExplicitNumberFn: ExplicitFn<f64>;
    type ExplicitPointFn: ExplicitFn<PointValue>;
    type ExplicitNumberListFn: ExplicitFn<Vec<f64>>;
    type ExplicitPointListFn: ExplicitFn<Vec<PointValue>>;

    type ImplicitNumberFn: ImplicitFn<f64>;
    type ImplicitPointFn: ImplicitFn<PointValue>;
    type ImplicitNumberListFn: ImplicitFn<Vec<f64>>;
    type ImplicitPointListFn: ImplicitFn<Vec<PointValue>>;

    fn eval(&self, name: &str, ty: &ValueType) -> Option<JitValue>;

    fn get_explicit_fn(&self, name: &str, ty: &ValueType) -> Option<ExplicitJitFn<Self>>;

    fn get_implicit_fn(&self, name: &str, ty: &ValueType) -> Option<ImplicitJitFn<Self>>;
}

pub type BuilderValue<Builder: CodeBuilder> = GenericValue<
    Builder::NumberValue,
    Builder::PointValue,
    Builder::NumberListValue,
    Builder::PointListValue,
>;

pub trait CodeBuilder {
    type NumberValue: Clone;
    type PointValue: Clone;
    type NumberListValue: Clone;
    type PointListValue: Clone;

    fn build_return(
        self,
        value: GenericValue<
            Self::NumberValue,
            Self::PointValue,
            Self::NumberListValue,
            Self::PointListValue,
        >,
    );

    fn call_fn<'a>(
        &mut self,
        name: &str,
        values: &[GenericValue<
            Self::NumberValue,
            Self::PointValue,
            Self::NumberListValue,
            Self::PointListValue,
        >],
        codegen: &mut CodeGen<'a>,
    ) -> Option<
        GenericValue<
            Self::NumberValue,
            Self::PointValue,
            Self::NumberListValue,
            Self::PointListValue,
        >,
    >;

    fn get_arg(
        &mut self,
        index: usize,
    ) -> Option<
        &GenericValue<
            Self::NumberValue,
            Self::PointValue,
            Self::NumberListValue,
            Self::PointListValue,
        >,
    >;

    fn const_number(&mut self, number: f64) -> Self::NumberValue;
    fn point(&mut self, x: Self::NumberValue, y: Self::NumberValue) -> Self::PointValue;

    fn number_list(
        &mut self,
        elements: &[Self::NumberValue],
    ) -> anyhow::Result<Self::NumberListValue>;

    fn point_list(&mut self, elements: &[Self::PointValue])
        -> anyhow::Result<Self::PointListValue>;

    fn map_list(
        &mut self,
        list: GenericList<Self::NumberListValue, Self::PointListValue>,
        output_ty: ListType,
        f: impl Fn(
            &mut Self,
            GenericList<Self::NumberValue, Self::PointValue>,
        ) -> GenericList<Self::NumberValue, Self::PointValue>,
    ) -> GenericList<Self::NumberListValue, Self::PointListValue>;

    fn get_x(&mut self, point: Self::PointValue) -> Self::NumberValue;
    fn get_y(&mut self, point: Self::PointValue) -> Self::NumberValue;

    fn add(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;
    fn sub(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;
    fn mul(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;
    fn div(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;
    fn pow(&mut self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;

    fn neg(&mut self, lhs: Self::NumberValue) -> Self::NumberValue;
    fn sqrt(&mut self, lhs: Self::NumberValue) -> Self::NumberValue;

    fn sin(&mut self, lhs: Self::NumberValue) -> Self::NumberValue;
    fn cos(&mut self, lhs: Self::NumberValue) -> Self::NumberValue;
    fn tan(&mut self, lhs: Self::NumberValue) -> Self::NumberValue;
}
