use anyhow::Result;

use std::collections::{HashMap, HashSet};

use crate::lang::expr::{Expr, ResolvedExpr};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExpressionId(pub u32);

impl From<u32> for ExpressionId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

#[derive(Debug, Default)]
pub struct Expressions {
    pub exprs: HashMap<ExpressionId, ResolvedExpr>,
    idents: HashMap<String, ExpressionId>,
}

impl Expressions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn remove_expr(&mut self, id: impl Into<ExpressionId>) {
        let k = id.into();
        if let Some(ResolvedExpr {
            expr: Expr::VarDef { ident, .. } | Expr::FnDef { ident, .. },
            ..
        }) = self.exprs.remove(&k)
        {
            self.idents.remove(&ident);
        }
    }

    pub fn insert_expr(&mut self, id: impl Into<ExpressionId>, s: &str) -> Result<()> {
        let k = id.into();
        self.parse_expr(s, k).map(|expr| {
            self.exprs.insert(k, expr);
        })
    }

    pub fn get_used_idents(&self, name: &str) -> Option<&HashSet<String>> {
        if let Some(id) = self.idents.get(name) {
            self.exprs.get(id).map(|e| &e.used_idents)
        } else {
            None
        }
    }

    pub fn get_expr(&self, name: &str) -> Option<&Expr> {
        if let Some(id) = self.idents.get(name) {
            self.exprs.get(id).map(|e| &e.expr)
        } else {
            None
        }
    }

    pub fn get_resolved_expr(&self, name: &str) -> Option<&ResolvedExpr> {
        if let Some(id) = self.idents.get(name) {
            self.exprs.get(id)
        } else {
            None
        }
    }

    fn parse_expr(&mut self, s: &str, k: ExpressionId) -> Result<ResolvedExpr> {
        let expr = ResolvedExpr::parse(s)?;
        match &expr.expr {
            Expr::VarDef { ident, .. } | Expr::FnDef { ident, .. } => {
                self.idents.insert(ident.clone(), k);
            }
            _ => (),
        };

        Ok(expr)
    }
}
