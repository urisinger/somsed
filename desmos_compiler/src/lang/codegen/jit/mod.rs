pub mod function;

use function::{ExplicitFn, ExplicitJitFn, ImplicitFn, ImplicitJitFn, JitValue, PointValue};

use super::ir::IRType;

pub trait ExecutionEngine {
    type ExplicitNumberFn: ExplicitFn<f64>;
    type ExplicitPointFn: ExplicitFn<PointValue>;
    type ExplicitNumberListFn: ExplicitFn<Vec<f64>>;
    type ExplicitPointListFn: ExplicitFn<Vec<PointValue>>;

    type ImplicitNumberFn: ImplicitFn<f64>;
    type ImplicitPointFn: ImplicitFn<PointValue>;
    type ImplicitNumberListFn: ImplicitFn<Vec<f64>>;
    type ImplicitPointListFn: ImplicitFn<Vec<PointValue>>;

    fn eval(&self, name: &str, ty: &IRType) -> Option<JitValue>;

    fn get_explicit_fn(&self, name: &str, ty: &IRType) -> Option<ExplicitJitFn<Self>>;

    fn get_implicit_fn(&self, name: &str, ty: &IRType) -> Option<ImplicitJitFn<Self>>;
}
