use crate::lang::generic_value::{GenericList, GenericValue};

use super::ExecutionEngine;

pub type JitValue = GenericValue<f64, PointValue, Vec<f64>, Vec<PointValue>>;

pub type JitListValue = GenericList<Vec<f64>, Vec<PointValue>>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PointValue {
    pub x: f64,
    pub y: f64,
}

pub trait ExplicitFn<T> {
    fn call(&self, x: f64) -> T;
}

pub type ExplicitJitFn<BackendT: ExecutionEngine> = GenericValue<
    BackendT::ExplicitNumberFn,
    BackendT::ExplicitPointFn,
    BackendT::ExplicitNumberListFn,
    BackendT::ExplicitPointListFn,
>;

pub type ExplicitListJitFn<BackendT: ExecutionEngine> =
    GenericList<BackendT::ExplicitNumberListFn, BackendT::ExplicitPointListFn>;

impl<
        NumberFn: ExplicitFn<f64>,
        PointFn: ExplicitFn<PointValue>,
        NumberListFn: ExplicitFn<Vec<f64>>,
        PointListFn: ExplicitFn<Vec<PointValue>>,
    > GenericValue<NumberFn, PointFn, NumberListFn, PointListFn>
{
    pub fn call(&self, x: f64) -> JitValue {
        match self {
            GenericValue::Number(f) => JitValue::Number(f.call(x)),
            GenericValue::Point(f) => JitValue::Point(f.call(x)),
            GenericValue::List(generic_list) => JitValue::List(generic_list.call(x)),
        }
    }
}

impl<NumberListFn: ExplicitFn<Vec<f64>>, PointListFn: ExplicitFn<Vec<PointValue>>>
    GenericList<NumberListFn, PointListFn>
{
    pub fn call(&self, x: f64) -> JitListValue {
        match self {
            GenericList::Number(f) => JitListValue::Number(f.call(x)),
            GenericList::PointList(f) => JitListValue::PointList(f.call(x)),
        }
    }
}
pub trait ImplicitFn<T> {
    fn call_implicit(&self, x: f64, y: f64) -> T;
}

pub type ImplicitJitFn<BackendT: ExecutionEngine> = GenericValue<
    BackendT::ImplicitNumberFn,
    BackendT::ImplicitPointFn,
    BackendT::ImplicitNumberListFn,
    BackendT::ImplicitPointListFn,
>;

pub type ImplicitListJitFn<BackendT: ExecutionEngine> =
    GenericList<BackendT::ImplicitNumberListFn, BackendT::ImplicitPointListFn>;

impl<
        NumberFn: ImplicitFn<f64>,
        PointFn: ImplicitFn<PointValue>,
        NumberListFn: ImplicitFn<Vec<f64>>,
        PointListFn: ImplicitFn<Vec<PointValue>>,
    > GenericValue<NumberFn, PointFn, NumberListFn, PointListFn>
{
    pub fn call_implicit(&self, x: f64, y: f64) -> JitValue {
        match self {
            GenericValue::Number(f) => JitValue::Number(f.call_implicit(x, y)),
            GenericValue::Point(f) => JitValue::Point(f.call_implicit(x, y)),
            GenericValue::List(generic_list) => JitValue::List(generic_list.call_implicit(x, y)),
        }
    }
}

impl<NumberListFn: ImplicitFn<Vec<f64>>, PointListFn: ImplicitFn<Vec<PointValue>>>
    GenericList<NumberListFn, PointListFn>
{
    pub fn call_implicit(&self, x: f64, y: f64) -> JitListValue {
        match self {
            GenericList::Number(f) => JitListValue::Number(f.call_implicit(x, y)),
            GenericList::PointList(f) => JitListValue::PointList(f.call_implicit(x, y)),
        }
    }
}
