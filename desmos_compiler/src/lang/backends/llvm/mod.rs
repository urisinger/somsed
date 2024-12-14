use std::collections::HashMap;

use crate::{expressions::ExpressionId, lang::parser::ComparisonOp};

use self::jit::{ExplicitJitFn, ImplicitJitFn};

pub mod codegen;
pub mod jit;
pub mod types;
pub mod value;

#[derive(Default)]
pub struct CompiledExprs<'ctx> {
    pub compiled: HashMap<ExpressionId, CompiledExpr<'ctx>>,
    pub errors: HashMap<ExpressionId, String>,
}

impl<'ctx> CompiledExprs<'ctx> {
    pub fn new() -> Self {
        Self {
            compiled: HashMap::new(),
            errors: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: ExpressionId, result: anyhow::Result<CompiledExpr<'ctx>>) {
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

pub enum CompiledExpr<'ctx> {
    Implicit {
        lhs: ImplicitJitFn<'ctx>,
        op: ComparisonOp,
        rhs: ImplicitJitFn<'ctx>,
    },
    Explicit {
        lhs: ExplicitJitFn<'ctx>,
    },
}
