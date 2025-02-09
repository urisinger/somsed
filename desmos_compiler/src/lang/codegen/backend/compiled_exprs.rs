use std::collections::HashMap;

use crate::{expressions::ExpressionId, lang::expr::ComparisonOp};

use super::{
    jit::{ExplicitJitFn, ImplicitJitFn, JitValue},
    ExecutionEngine,
};

#[derive(Default)]
pub struct CompiledExprs<BackendT: ExecutionEngine> {
    pub compiled: HashMap<ExpressionId, CompiledExpr<BackendT>>,
    pub errors: HashMap<ExpressionId, String>,
}

impl<BackendT: ExecutionEngine> CompiledExprs<BackendT> {
    pub fn new() -> Self {
        Self {
            compiled: HashMap::new(),
            errors: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: ExpressionId, result: anyhow::Result<CompiledExpr<BackendT>>) {
        match result {
            Ok(expr) => {
                self.compiled.insert(id, expr);
            }
            Err(err) => {
                self.errors.insert(id, err.to_string());
            }
        }
    }
}

pub enum CompiledExpr<BackendT: ExecutionEngine> {
    Implicit {
        lhs: ImplicitJitFn<
            BackendT::ImplicitNumberFn,
            BackendT::ImplicitPointFn,
            BackendT::ImplicitNumberListFn,
            BackendT::ImplicitPointListFn,
        >,
        op: ComparisonOp,
        rhs: ImplicitJitFn<
            BackendT::ImplicitNumberFn,
            BackendT::ImplicitPointFn,
            BackendT::ImplicitNumberListFn,
            BackendT::ImplicitPointListFn,
        >,
    },
    Explicit {
        lhs: ExplicitJitFn<
            BackendT::ExplicitNumberFn,
            BackendT::ExplicitPointFn,
            BackendT::ExplicitNumberListFn,
            BackendT::ExplicitPointListFn,
        >,
    },
    Constant {
        value: JitValue,
    },
}
