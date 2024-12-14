use anyhow::Result;

use crate::lang::parser::Expr;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct ExpressionId(pub u32);

impl From<u32> for ExpressionId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

#[derive(Debug)]
pub struct Expressions {
    pub exprs: HashMap<ExpressionId, Expr>,
    idents: HashMap<String, ExpressionId>,

    pub max_id: u32,
}

impl Default for Expressions {
    fn default() -> Self {
        Self {
            exprs: Default::default(),
            idents: Default::default(),
            max_id: 0,
        }
    }
}

impl Expressions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn remove_expr(&mut self, id: impl Into<ExpressionId>) {
        let k = id.into();
        if let Some(Expr::VarDef { ident, .. } | Expr::FnDef { ident, .. }) = self.exprs.remove(&k)
        {
            self.idents.remove(&ident);
        }
    }

    #[allow(unused_must_use)]
    pub fn add_expr(&mut self, s: &str) -> Result<ExpressionId> {
        self.set_expr(self.max_id, s)?;
        self.max_id += 1;
        Ok((self.max_id - 1).into())
    }

    pub fn set_expr(&mut self, id: impl Into<ExpressionId>, s: &str) -> Result<()> {
        let k = id.into();
        self.parse_expr(s, k).map(|expr| {
            self.exprs.insert(k, expr);
        })
    }

    pub fn get_expr(&self, name: &str) -> Option<&Expr> {
        if let Some(id) = self.idents.get(name) {
            self.exprs.get(id)
        } else {
            None
        }
    }

    fn parse_expr(&mut self, s: &str, k: ExpressionId) -> Result<Expr> {
        let expr = Expr::parse(s)?;
        match &expr {
            Expr::VarDef { ident, .. } | Expr::FnDef { ident, .. } => {
                self.idents.insert(ident.clone(), k);
            }
            _ => (),
        };

        Ok(expr)
    }
}
