use anyhow::anyhow;

use crate::{
    expressions::Expressions,
    lang::{codegen::CodeGen, expr::Expr, generic_value::ValueType},
};

use super::{
    compiled_exprs::{CompiledExpr, CompiledExprs},
    jit::{ExplicitFn, ExplicitJitFn, ImplicitFn, ImplicitJitFn, JitValue, PointValue},
    Backend,
};

pub trait CompiledBackend: Backend {
    type Engine: ExecutionEngine;
    fn get_execution_engine(&self) -> Self::Engine;
}

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

pub fn compile_expressions<BackendT: CompiledBackend>(
    backend: &BackendT,
    exprs: &Expressions,
) -> CompiledExprs<BackendT::Engine> {
    let mut codegen = CodeGen::new(backend, exprs);

    let mut compiled_exprs: CompiledExprs<BackendT::Engine> = CompiledExprs::new();

    //We first have to compile everything
    for (id, expr) in &exprs.exprs {
        match &expr.expr {
            Expr::Implicit { lhs, rhs, .. } => {
                let args_t = [ValueType::Number(()), ValueType::Number(())];

                let lhs_name = format!("implicit_{}_lhs", id.0);
                _ = codegen
                    .compile_fn(&lhs_name, lhs, &args_t)
                    .inspect_err(|e| {
                        compiled_exprs.errors.insert(*id, e.to_string());
                    });

                let rhs_name = format!("implicit_{}_rhs", id.0);
                _ = codegen
                    .compile_fn(&rhs_name, rhs, &args_t)
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

                _ = codegen.compile_fn(&name, lhs, &args).inspect_err(|e| {
                    compiled_exprs.errors.insert(*id, e.to_string());
                });
            }
            Expr::VarDef { rhs, ident } => {
                if !expr.used_idents.is_empty() {
                    continue;
                }
                let name = ident.to_string();
                println!("{:?}", expr.used_idents);

                let args = [];
                _ = codegen.compile_fn(&name, rhs, &args).inspect_err(|e| {
                    compiled_exprs.errors.insert(*id, e.to_string());
                });
            }
            _ => {}
        }
    }

    let execution_engine = backend.get_execution_engine();

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
                        .eval(&name, return_type)
                        .ok_or_else(|| anyhow!("Could not find function {name}"))
                        .map(|value| CompiledExpr::Constant { value })
                };

                compiled_exprs.insert(*id, value);
            }
            Expr::VarDef { ident, .. } => {
                if !expr.used_idents.is_empty() {
                    continue;
                }

                let name = ident.to_string();

                let return_type = codegen.return_types.get(&name).unwrap_or_else(|| {
                    panic!("Return type not found for explicit function: {}", name);
                });

                let value = execution_engine.eval(&name, return_type);

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
