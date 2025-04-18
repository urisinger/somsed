use super::ExecutionEngine;

#[derive(Debug, Clone)]
pub enum JitValue {
    Number(f64),
    Point(PointValue),
    NumberList(Vec<f64>),
    PointList(Vec<PointValue>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PointValue {
    pub x: f64,
    pub y: f64,
}

pub trait ExplicitFn<T> {
    fn call(&self, x: f64) -> T;
}

pub enum ExplicitJitFn<BackendT: ExecutionEngine + ?Sized> {
    Number(BackendT::ExplicitNumberFn),
    Point(BackendT::ExplicitPointFn),
    NumberList(BackendT::ExplicitNumberListFn),
    PointList(BackendT::ExplicitPointListFn),
}

impl<BackendT: ExecutionEngine> ExplicitJitFn<BackendT> {
    pub fn call(&self, x: f64) -> JitValue {
        match self {
            ExplicitJitFn::Number(f) => JitValue::Number(f.call(x)),
            ExplicitJitFn::Point(f) => JitValue::Point(f.call(x)),
            ExplicitJitFn::NumberList(f) => JitValue::NumberList(f.call(x)),
            ExplicitJitFn::PointList(f) => JitValue::PointList(f.call(x)),
        }
    }
}

pub trait ImplicitFn<T> {
    fn call_implicit(&self, x: f64, y: f64) -> T;
}

pub enum ImplicitJitFn<BackendT: ExecutionEngine + ?Sized> {
    Number(BackendT::ImplicitNumberFn),
    Point(BackendT::ImplicitPointFn),
    NumberList(BackendT::ImplicitNumberListFn),
    PointList(BackendT::ImplicitPointListFn),
}

impl<BackendT: ExecutionEngine> ImplicitJitFn<BackendT> {
    pub fn call_implicit(&self, x: f64, y: f64) -> JitValue {
        match self {
            ImplicitJitFn::Number(f) => JitValue::Number(f.call_implicit(x, y)),
            ImplicitJitFn::Point(f) => JitValue::Point(f.call_implicit(x, y)),
            ImplicitJitFn::NumberList(f) => JitValue::NumberList(f.call_implicit(x, y)),
            ImplicitJitFn::PointList(f) => JitValue::PointList(f.call_implicit(x, y)),
        }
    }
}
