use anyhow::anyhow;
use compiled_exprs::{CompiledExpr, CompiledExprs};
use jit::{ExplicitFn, ExplicitJitFn, ImplicitFn, ImplicitJitFn, JitValue, PointValue};

use crate::{expressions::Expressions, lang::expr::Expr};

use super::{
    codegen::CodeGen,
    generic_value::{GenericValue, ValueType},
};

pub mod compiled_exprs;
pub mod jit;
pub mod llvm;

pub fn compile_expressions<BackendT: CompiledBackend>(
    backend: &BackendT,
    exprs: &Expressions,
) -> CompiledExprs<BackendT::Engine> {
    let execution_engine = backend.get_execution_engine();

    let mut codegen = CodeGen::new(backend, exprs);

    let mut compiled_exprs: CompiledExprs<BackendT::Engine> = CompiledExprs::new();

    //We first have to compile everything
    for (id, expr) in &exprs.exprs {
        match &expr.expr {
            Expr::Implicit { lhs, rhs, .. } => {
                let args_t = [ValueType::Number(()), ValueType::Number(())];

                let lhs_name = format!("implicit_{}_lhs", id.0);
                _ = codegen
                    .compile_fn(&lhs_name, &lhs, &args_t)
                    .inspect_err(|e| {
                        compiled_exprs.errors.insert(*id, e.to_string());
                    });

                let rhs_name = format!("implicit_{}_rhs", id.0);
                _ = codegen
                    .compile_fn(&rhs_name, &rhs, &args_t)
                    .inspect_err(|e| {
                        compiled_exprs.errors.insert(*id, e.to_string());
                    });
            }
            Expr::Explicit { lhs } => {
                let name = format!("explicit_{}", id.0);

                let args = if expr.used_idents.contains("x") {
                    vec![ValueType::Number(())]
                } else {
                    vec![]
                };

                _ = codegen.compile_fn(&name, &lhs, &args).inspect_err(|e| {
                    compiled_exprs.errors.insert(*id, e.to_string());
                });
            }
            Expr::VarDef { rhs, ident } => {
                if !expr.used_idents.is_empty() {
                    continue;
                }
                let name = format!("{}_number_number", ident);

                let args = vec![];
                _ = codegen.compile_fn(&name, &rhs, &args).inspect_err(|e| {
                    compiled_exprs.errors.insert(*id, e.to_string());
                });
            }
            _ => {}
        }
    }

    // Only then we can retrive the functions
    for (id, expr) in &exprs.exprs {
        if compiled_exprs.errors.contains_key(id) {
            continue;
        }
        match &expr.expr {
            Expr::Implicit { op, .. } => {
                let lhs_name = format!("implicit_{}_lhs", id.0);
                let lhs_result = execution_engine
                    .get_implicit_fn(
                        &lhs_name,
                        codegen.return_types.get(&lhs_name).unwrap_or_else(|| {
                            panic!("Return type not found for rhs: {}", lhs_name);
                        }),
                    )
                    .ok_or_else(|| anyhow!("fn {} does not exist", lhs_name));

                let rhs_name = format!("implicit_{}_rhs", id.0);
                let rhs_result = execution_engine
                    .get_implicit_fn(
                        &rhs_name,
                        codegen.return_types.get(&rhs_name).unwrap_or_else(|| {
                            panic!("Return type not found for rhs: {}", rhs_name);
                        }),
                    )
                    .ok_or_else(|| anyhow!("fn {} does not exist", rhs_name));

                let result = match (lhs_result, rhs_result) {
                    (Ok(lhs), Ok(rhs)) => Ok(CompiledExpr::Implicit { lhs, op: *op, rhs }),
                    (Err(e), _) => Err(e),
                    (_, Err(e)) => Err(e),
                };

                compiled_exprs.insert(*id, result);
            }
            Expr::Explicit { .. } => {
                let name = format!("explicit_{}", id.0);

                let value = if expr.used_idents.contains("x") {
                    execution_engine
                        .get_explicit_fn(
                            &name,
                            codegen.return_types.get(&name).unwrap_or_else(|| {
                                panic!("Return type not found for rhs: {}", name);
                            }),
                        )
                        .ok_or_else(|| anyhow!("fn {} does not exist", name))
                        .map(|lhs| CompiledExpr::Explicit { lhs })
                } else {
                    let return_type = codegen.return_types.get(&name).unwrap_or_else(|| {
                        panic!("Return type not found for explicit function: {}", name);
                    });

                    execution_engine
                        .eval(&name, &return_type)
                        .ok_or_else(|| anyhow!("Could not find function {name}"))
                        .map(|value| CompiledExpr::Constant { value })
                };

                compiled_exprs.insert(*id, value);
            }
            Expr::VarDef { ident, .. } => {
                if !expr.used_idents.is_empty() {
                    continue;
                }
                let name = format!("{}_number_number", ident);

                let return_type = codegen.return_types.get(&name).unwrap_or_else(|| {
                    panic!("Return type not found for explicit function: {}", name);
                });

                let value = execution_engine.eval(&name, &return_type);

                compiled_exprs.insert(
                    *id,
                    value
                        .ok_or_else(|| anyhow!("Could not find function {name}"))
                        .map(|value| CompiledExpr::Constant { value }),
                );
            }
            _ => {}
        };
    }

    compiled_exprs
}

pub type BackendValue<'a, BackendT> = GenericValue<
    <<BackendT as Backend>::Builder<'a>as CodeBuilder< <BackendT as Backend>::FnValue>>::NumberValue,
    <<BackendT as Backend>::Builder<'a> as CodeBuilder< <BackendT as Backend>::FnValue>>::PointValue,
    <<BackendT as Backend>::Builder<'a> as CodeBuilder< <BackendT as Backend>::FnValue>>::NumberListValue,
    <<BackendT as Backend>::Builder<'a> as CodeBuilder< <BackendT as Backend>::FnValue>>::PointListValue,
>;

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

    fn get_explicit_fn(
        &self,
        name: &str,
        ty: &ValueType,
    ) -> Option<
        ExplicitJitFn<
            Self::ExplicitNumberFn,
            Self::ExplicitPointFn,
            Self::ExplicitNumberListFn,
            Self::ExplicitPointListFn,
        >,
    >;

    fn get_implicit_fn(
        &self,
        name: &str,
        ty: &ValueType,
    ) -> Option<
        ImplicitJitFn<
            Self::ImplicitNumberFn,
            Self::ImplicitPointFn,
            Self::ImplicitNumberListFn,
            Self::ImplicitPointListFn,
        >,
    >;
}

pub trait CompiledBackend: Backend {
    type Engine: ExecutionEngine;
    fn get_execution_engine(&self) -> Self::Engine;
}

pub trait Backend {
    type FnValue;
    type Builder<'a>: CodeBuilder<Self::FnValue>
    where
        Self: 'a;

    fn get_builder<'a>(
        &'a self,
        name: &str,
        types: &[ValueType],
        return_type: &ValueType,
    ) -> Self::Builder<'a>;

    fn get_fn(&self, name: &str) -> Option<Self::FnValue>;
}

pub trait CodeBuilder<FnValue> {
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
    ) -> FnValue;

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
    >;

    fn call_fn(
        &self,
        function: FnValue,
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
    >;

    fn const_number(&self, number: f64) -> Self::NumberValue;
    fn point(&self, x: Self::NumberValue, y: Self::NumberValue) -> Self::PointValue;

    fn add(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;
    fn sub(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;
    fn mul(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;
    fn div(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;
    fn pow(&self, lhs: Self::NumberValue, rhs: Self::NumberValue) -> Self::NumberValue;

    fn neg(&self, lhs: Self::NumberValue) -> Self::NumberValue;
    fn sqrt(&self, lhs: Self::NumberValue) -> Self::NumberValue;

    fn sin(&self, lhs: Self::NumberValue) -> Self::NumberValue;
    fn cos(&self, lhs: Self::NumberValue) -> Self::NumberValue;
    fn tan(&self, lhs: Self::NumberValue) -> Self::NumberValue;
}
